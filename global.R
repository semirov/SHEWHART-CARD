library(shiny)
library(DBI)
library(pool)



host <- "192.168.26.43"
username <- "root"
password <- "!QAZxsw2"
###########################пулл соединения
pool_select <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "scadabr_test",
  host = host,
  username = username,
  password = password,
  minSize = 1,
  maxSize = Inf,    
  idleTimeout = 60*1000 

)
pool_insert <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "stat_test",
  host = host,
  username = username,
  password = password,
  minSize = 1,
  maxSize = Inf,    
  idleTimeout = 60*1000 
  
)

#########################################

#Запрашиваем данные точки id в колличестве n
get_last_pointValue <- function(id){
  sql <- "SELECT pointValue, FROM_UNIXTIME(ts/1000, '%e.%m.%y %T')  as 'date', ts
  FROM
  pointvalues 
  WHERE
  dataPointId = ?datapointid
  ORDER BY ID DESC
  LIMIT 1;"
  query <- sqlInterpolate(pool_select, sql, datapointid = id) 
  return(query)
}


#Запрос для проверки изменений
get_maxts_pointValue <- function(id){
  sql <- "SELECT MAX(ts)
  FROM
  pointvalues 
  WHERE
  dataPointId = ?datapointid"
  query <- sqlInterpolate(pool_select, sql,datapointid = id) 
  return(query)
  
}

insert_stat_param<- function(id, value, ts){
  sql <- "INSERT INTO `value` (`value`, `idparam`, `ts`) VALUES (?val, ?idparam, ?timestamp);"
  query <- sqlInterpolate(pool_insert, sql,val = value, idparam = id, timestamp = ts)

  return(query)
}

get_fromto_pointValue <- function(id, start, end){
  sql <- "SELECT pointValue, ts FROM pointvalues where dataPointId = ?datapointid and ts > ?starTS and ts < ?endTS"
  query <- sqlInterpolate(pool_select, sql,datapointid = id, starTS = start, endTS = end)
  
  return(query)
}


#Фукнция опроса базы
get_stat_data <- function(USL, LSL, n_clear, n_group, id_param){

  myReact <- reactiveValues( clear_data = data.frame(), # фрейм чистых данных
                             grup_data = data.frame(), # фрем группированных данных
                             mean_data = data.frame(),  # фрейм усредненных выборок
                             range_data = data.frame(), # фрейм размаха
                             stdDev_data = data.frame(), #фрейм стандартных отклонений
                             disp_data = data.frame(), #фрейм значений дисперсии
                             Cp = 0, Cr = 0, Mean = 0, Avg = 0, Sigma = 0, StdDev = 0, Skew = 0, Kurtosis = 0, Range = 0,
                             realtime_data = data.frame())
  
  #Расчет параметров
  myReact$Cp <- reactive((USL-LSL)/(6*myReact$Sigma()))
  myReact$Cr <- reactive(100/myReact$Cp())
  myReact$Mean <- reactive(mean(myReact$mean_data$mean))
  myReact$Avg  <- reactive(mean(myReact$mean_data$mean))
  myReact$Sigma  <- reactive(sd(myReact$clear_data$pointValue))
  myReact$StdDev <- reactive(sd(myReact$mean_data$mean))
  myReact$Skew <- reactive(skewness(myReact$mean_data$mean))
  myReact$Kurtosis <- reactive(kurtosis(myReact$mean_data$mean))
  myReact$Range <- reactive(max(myReact$grup_data$pointValue) - min(myReact$grup_data$pointValue))
  
  
  # Реактивноый опрос базы данных
  myReact$realtime_data <- reactivePoll(700, 
                                        NULL, 
                                        checkFunc <- function(){
                                          
                                          reactive(dbGetQuery(pool_select,get_maxts_pointValue(id_param)))
                                        }, 
                                        valueFunc <- function(){
                                          dbGetQuery(pool_select,get_last_pointValue(id_param))}
                                        )

  data_load_to_base <- reactivePoll(3000, 
                                    NULL, 
                                    checkFunc <- function(){
                                      
                                        myReact$StdDev()
                                       
                                    }, 
                                    valueFunc <- function(){
                                     
                                     
                                      ts =as.character(as.POSIXct(isolate(myReact$grup_data$ts[1]/1000),origin = "1970-01-01"))
                                      
                                      dbGetQuery(pool_insert,insert_stat_param(1,isolate(myReact$Cp()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(2,isolate(myReact$Cr()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(3,isolate(myReact$Mean()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(4,isolate(myReact$Avg()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(5,isolate(myReact$StdDev()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(6,isolate(myReact$Skew()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(7,isolate(myReact$Kurtosis()),ts))
                                      dbGetQuery(pool_insert,insert_stat_param(8,isolate(myReact$Sigma()),ts))
                                     
                                      
                                      
                                      
                                      
                                    })
  #Наблюдатель для запроса проверки изменеия данных и записи в базу
  observe({
    invalidateLater(2000)
    if(!is.na(isolate(myReact$StdDev()))){
      data_load_to_base()
    }
    
    
  })
  
  # слушатель изменений и добавление
  observe({
    
    myReact$clear_data <- tail(rbind(isolate(myReact$clear_data), myReact$realtime_data()), n_clear)
    
    if(length(isolate(myReact$grup_data$ts)) >= n_group){ 
      
      #Усредним в темповую таблицу значение и метки времени и помести в mean_data
      temp_dp_mean <- data.frame(mean = mean(isolate(myReact$grup_data$pointValue)), ts = isolate(myReact$grup_data$ts[1]))
      myReact$mean_data <- tail(rbind(isolate(myReact$mean_data), temp_dp_mean),n_clear/n_group)
      
      #посчитаем размах
      temp_dp_range <- data.frame(range = max(isolate(myReact$grup_data$pointValue)) - min(isolate(myReact$grup_data$pointValue)), ts = isolate(myReact$grup_data$ts[1]))
      myReact$range_data <- tail(rbind(isolate(myReact$range_data), temp_dp_range),n_clear/n_group)
      
      #посчитаем СКО выборок
      temp_dp_stdDev <- data.frame(stdDev = sd(isolate(myReact$grup_data$pointValue)), ts = isolate(myReact$grup_data$ts[1]))
      myReact$stdDev_data <- tail(rbind(isolate(myReact$stdDev_data), temp_dp_stdDev),n_clear/n_group)
      
      temp_dp_disp <- data.frame(disp = sqrt(sd(isolate(myReact$grup_data$pointValue))), ts = isolate(myReact$grup_data$ts[1]))
      myReact$disp_data <- tail(rbind(isolate(myReact$disp_data), temp_dp_disp),n_clear/n_group)
      
      #отчистка группового фрейма
      myReact$grup_data <- isolate(myReact$grup_data[0,])
      
      #формирование первой после отичстки строки
      myReact$grup_data <- rbind(isolate(myReact$grup_data), isolate(myReact$realtime_data()))
    }
    else{
      #промеждуточное добавление строк
      myReact$grup_data <- rbind(isolate(myReact$grup_data), isolate(myReact$realtime_data()))
    }
    
  })
  

return(myReact)
}



#Запрос таблицы от дата_время до Дата_время
get_pointValue_data <-function(id, start, end, n_group, USL, LSL){
  start_linux_ts <- as.numeric(start) * 1000
  end_linux_ts <- as.numeric(end) * 1000
  
  if(end_linux_ts <= start_linux_ts) {
    showNotification(ui = "Время в графе 'Конец' не может быть раньше времени, чем графе 'Начало' - данные не будут загружены", type = "warning", duration = 60)
     return(NULL)
    }else{
  
  dataFrame <- reactiveValues(
    grup_data = data.frame(), # фрейм усредненных данных
    mean_data = data.frame(),  # фрейм усредненных выборок
    range_data = data.frame(), # фрейм размаха
    stdDev_data = data.frame(), # фрейм СКО выборок
    clear_data = data.frame(), # фрейм чистых данных
    disp_data = data.frame(), # фрейм значений дисперсии
    Cp = 0,
    Cr = 0,
    Mean = 0,
    Avg = 0,
    Sigma = 0,
    StdDev = 0,
    Skew = 0,
    Kurtosis = 0
  )  
  

  dataFrame$clear_data <- dbGetQuery(pool_select,get_fromto_pointValue(id,start_linux_ts,end_linux_ts))
  
  withProgress(message = 'Вычисляем значения..', value = 0, {
  
  
  for (i in 1:nrow(isolate(dataFrame$clear_data))) {
    row <- isolate(dataFrame$clear_data)[i,] #присвоим всю выбранную строку
    
    if (length(dataFrame$grup_data$ts) >= n_group){
      
      
      ts = isolate(dataFrame$grup_data$ts)[1] # таймстемп группы
      
      #фрейм средний значений подгрупп
      
      
      temp_dp_mean <- data.frame(mean = mean(isolate(dataFrame$grup_data$pointValue)), ts)
      dataFrame$mean_data <- rbind(isolate(dataFrame$mean_data), temp_dp_mean)
      
      #фрейм размахов
      temp_dp_range <- data.frame(range = max(isolate(dataFrame$grup_data$pointValue)) - min(isolate(dataFrame$grup_data$pointValue)), ts = isolate(dataFrame$grup_data$ts)[1])
      dataFrame$range_data <- rbind(isolate(dataFrame$range_data), temp_dp_range)
      
      #посчитаем СКО выборок
      temp_dp_stdDev <- data.frame(stdDev = sd(isolate(dataFrame$grup_data$pointValue)), ts)
      dataFrame$stdDev_data <- rbind(isolate(dataFrame$stdDev_data), temp_dp_stdDev)
      
      temp_dp_disp <- data.frame(disp = sqrt(sd(isolate(dataFrame$grup_data$pointValue))), ts)
      dataFrame$disp_data <- rbind(isolate(dataFrame$disp_data), temp_dp_disp)
      
      
      
      #отчистка группового фрейма
      dataFrame$grup_data <- isolate(dataFrame$grup_data[0,])
      
      #формирование первой после отичстки строки
      dataFrame$grup_data <- rbind(isolate(dataFrame$grup_data), row)
    }
    else{
      dataFrame$grup_data <- rbind(isolate(dataFrame$grup_data), row)
    }
    
    incProgress(1/nrow(isolate(dataFrame$clear_data)), detail = paste("Посчитано значений:", i))
    
  }
  
  })
  

  
  dataFrame$Mean <- mean(dataFrame$mean_data$mean)
  dataFrame$Avg  <- mean(dataFrame$mean_data$mean)
  dataFrame$Sigma  <- sd(dataFrame$clear_data$pointValue)
  dataFrame$Cp <- (USL-LSL)/(6*dataFrame$Sigma)
  dataFrame$StdDev <- sd(dataFrame$mean_data$mean)
  dataFrame$Cr <- 100/dataFrame$Cp
  dataFrame$Skew <- skewness(dataFrame$mean_data$mean)
  dataFrame$Kurtosis <- kurtosis(dataFrame$mean_data$mean)
  dataFrame$Range <- max(dataFrame$grup_data$pointValue) - min(dataFrame$grup_data$pointValue)
  
  
   
  return(dataFrame)
    }
}


