library(shiny)
library(DBI)
library(pool)
library(moments)
library(ggplot2)
library(scales)


display.mode="showcase"
options(shiny.reactlog=TRUE) 


#id <<- 110 # CAV
#id <<- 109 #Hm
#id <<- 108 # T
id <<- 44 # VAV

###### Реактивноый опрос базы данных ######


myReact <<- reactive({
  usl <- USL()
  lsl <- LSL()
  n_clear <- n_clear()
  n_group <-n_group()
  sp <- SP()

  return(get_stat_data(usl, lsl, n_clear, n_group, id))
  
})  
######-------------------------------######



#Фукция клиентской части
function(input, output, session) {
  
  # разрешение реконекта
  session$allowReconnect(TRUE)
  #session$allowReconnect("force")
  
# Параметры из сессии передаются в глобальные параметры  

  USL <<- reactive(input$USL)
  LSL <<- reactive(input$LSL)
  n_clear <<- reactive(input$n_clear)
  n_group <<-reactive(input$n_group)
  SP <<-reactive(input$SP)

  
#Выходные данные - ТАБЛИЦЫ
output$tbl <- renderTable({
#  myReact()$clear_data
})
output$tbl_group <-renderTable({
#  myReact()$grup_data
})
output$tbl_mean <-renderTable({
#  myReact()$mean_data
  
})



#Выходные данные - ГРАФИКИ





# output$plot_data_clear <- renderPlot({
#  if(length(myReact()$clear_data$pointValue)>=2){
#  ggplot(data=myReact()$clear_data, aes(x = as.POSIXct(myReact()$clear_data$ts/1000, origin="1970-01-01"),y = myReact()$clear_data$pointValue, group=1)) +
#    geom_line(alpha = 0.5) + 
#    geom_point(colour = "seagreen", alpha = 0.8) + 
#    theme_minimal() + 
#    scale_x_datetime(date_labels = "%H:%M:%S") +
#    labs(title = "График чистых значений", x = "Время", y = "Значение") +
#    geom_hline(yintercept = USL(), colour = "darkred", linetype="dashed") +
#    geom_hline(yintercept = LSL(), colour = "darkblue", linetype="dashed")
#  }
# })

##################### - X карта 
#график средних значений групп   
output$plot_data_group <- renderPlot({
  if(length(myReact()$mean_data$mean)>=2){
    
    
    ggplot(myReact()$mean_data, aes(x = as.POSIXct(myReact()$mean_data$ts/1000, origin="1970-01-01"),y = myReact()$mean_data$mean)) +
      geom_line(aes(color="mean_line"),alpha = 0.5) + 
      geom_point(colour = "seagreen", alpha = 0.8) + 
      theme_minimal() +  
      scale_x_datetime(date_labels = "%H:%M:%S") +
      labs(title = "Значения выборочных средних", x = "Время", y = "Значение") +
      geom_hline(aes(yintercept = USL(), colour = "USL_line"), linetype="dashed") +
      geom_hline(aes(yintercept = LSL(), colour = "LSL_line"), linetype="dashed") +
      geom_hline(aes(yintercept = SP(), colour = "SP_line"), linetype="solid") +
      scale_colour_manual(name = 'Описание', 
                          values =c('mean_line'='black','USL_line'='darkred','LSL_line'='darkblue','SP_line'='red' ), 
                          labels = c(paste('Нижний предел (LSL = ',LSL(),')'),
                                     paste('Значение выборочной средней (n = ',n_group(),')'),
                                     paste('Уставка (SP = ',SP(),')'),
                                     paste('Верхний предел (USL = ',USL(),')')))
                                     
      
  }
})
#график диаграммы плотностей и распределения
output$plot_data_mean <- renderPlot({
  if(length(myReact()$mean_data$mean)>=2){
    ggplot(myReact()$mean_data, aes(mean)) +
      geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 6) +
      labs(title = "График распределения выборочных средних", x = "Значение", y = "Количество") +
      geom_vline(aes(xintercept = USL(), colour = "red"))+
      geom_vline(aes(xintercept = LSL(), colour = "blue")) + 
      geom_vline(aes(xintercept = myReact()$Mean(), colour = "black"), linetype="dashed") +
      geom_density(aes(y= ..count..),color="darkgreen", fill="lightgreen", alpha = 0.8, adjust = 1)+
      theme_minimal()+
      scale_colour_manual(name = 'Описание', 
                          values =c('red'='red','blue'='blue','black'='black'), 
                          labels = c(paste('Среднее значение процесса (Mean = ',format(round(myReact()$Mean(), 2), nsmall=2),')'),
                                     paste('Нижний предел (USL = ',LSL(),')'),
                                     paste('Верхний предел (USL = ',USL(),')')))
      
  }

})
#

##################### - R карта
#график размаха
output$range_plot_data_group <- renderPlot({
  if(length(myReact()$range_data$range)>=2){
    ggplot(myReact()$range_data, aes(x = as.POSIXct(myReact()$range_data$ts/1000, origin="1970-01-01"),y = myReact()$range_data$range, group=1)) +
      geom_line(alpha = 0.5) + 
      geom_point(colour = "seagreen", alpha = 0.8) + 
      theme_minimal() + 
      scale_x_datetime(date_labels = "%H:%M:%S") +
      labs(title = "Значения размахов выборок", x = "Время", y = "Значение")
  }
})    
# диаграмма 
    output$range_plot_data_histogramm <- renderPlot({
      if(length(myReact()$range_data$range)>=2){
        ggplot(myReact()$range_data, aes(range)) +
          geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 10) +
          labs(title = "Гистограмма размахов выборок", x = "Значение", y = "Количество") +

          theme_minimal()
      }
    })
    ##################### - S карта
    #график СКО
    output$std_plot_data_group <- renderPlot({
      if(length(myReact()$range_data$range)>=2){
        ggplot(myReact()$stdDev_data, aes(x = as.POSIXct(myReact()$stdDev_data$ts/1000, origin="1970-01-01"),y = myReact()$stdDev_data$stdDev, group=1)) +
          geom_line(alpha = 0.5) + 
          geom_point(colour = "seagreen", alpha = 0.8) + 
          theme_minimal() + 
          scale_x_datetime(date_labels = "%H:%M:%S") +
          labs(title = "Значения выборочных стандартных отклонений", x = "Время", y = "Значение")
      }
    })    
    # диаграмма 
    output$std_plot_data_histogramm <- renderPlot({
      if(length(myReact()$stdDev_data$stdDev)>=2){
        ggplot(myReact()$stdDev_data, aes(stdDev)) +
          geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 10) +
          labs(title = "Гистограмма выборочных стандартных отклонений", x = "Значение", y = "Количество") +
          
          theme_minimal()
      }
    })


###### S**2 карта
    #график дисперсий
    output$disp_plot_data_group <- renderPlot({
      if(length(myReact()$range_data$range)>=2){
        ggplot(myReact()$disp_data, aes(x = as.POSIXct(myReact()$disp_data$ts/1000, origin="1970-01-01"),y = myReact()$disp_data$disp, group=1)) +
          geom_line(alpha = 0.5) + 
          geom_point(colour = "seagreen", alpha = 0.8) + 
          theme_minimal() + 
          scale_x_datetime(date_labels = "%H:%M:%S") +
          labs(title = "Значения выборочных дисперсий", x = "Время", y = "Значение")
      }
    })    
    # диаграмма 
    output$disp_plot_data_histogramm <- renderPlot({
      if(length(myReact()$disp_data$disp)>=2){
        ggplot(myReact()$disp_data, aes(disp)) +
          geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 10) +
          labs(title = "Гистограмма выборочных дисперсий", x = "Значение", y = "Количество") +
          
          theme_minimal()
      }
    })  
    


####################################


#Выходные данные - ТЕКСТ
output$text_Cp <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Cp()
  }
  else{
    "Мало данных"
  }
})
output$text_Сr <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Cr()
  }
})
output$text_Mean <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Mean()
  }
})
output$text_Range <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Range()
  }
})
output$text_Avg <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Avg()
  }
})
output$text_Sigma <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Sigma()
  }

})
output$text_StdDev <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$StdDev()
  }
  
})

output$text_Skew <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Skew()
  }
})
output$text_Kurtosis <- renderText({
  if(length(myReact()$mean_data$mean)>=2){
    myReact()$Kurtosis()
  }
})
 
#############################################################################################



#выгружаем данные по запросу

#обработчик нажатия кнопки

get_data <- eventReactive(input$loadButton, {
 
  start_time <- reactive({as.POSIXct(paste(input$start_date," ", input$start_h,":",input$start_m,":00", sep = ""), format="%Y-%m-%d %H:%M:%S") })
  end_time <- reactive({as.POSIXct(paste(input$end_date," ", input$end_h,":",input$end_m,":00", sep = ""), format="%Y-%m-%d %H:%M:%S")})
  
  table_data <- get_pointValue_data(id, start_time(), end_time(), input$n_group_load, input$USL_load, input$LSL_load)
 
  return(table_data)
})

##################### - X карта
#график средних значений групп  



output$plot_data_group_load <- renderPlot({

    if(!is.null(get_data())){
    
    ggplot(get_data()$mean_data, aes(x = as.POSIXct(get_data()$mean_data$ts/1000, origin="1970-01-01"),y = get_data()$mean_data$mean)) +
      geom_line(aes(color="mean_line"),alpha = 0.5) + 
      geom_point(colour = "seagreen", alpha = 0.8) + 
      theme_minimal() +
      scale_x_datetime(date_labels = "%H:%M:%S") +
      labs(title = "Значения выборочных средних", x = "Время", y = "Значение") +
      geom_hline(aes(yintercept = input$USL_load, colour = "USL_line"), linetype="dashed") +
      geom_hline(aes(yintercept = input$LSL_load, colour = "LSL_line"), linetype="dashed") +
      geom_hline(aes(yintercept = input$SP_load, colour = "SP_line"), linetype="solid") +
      scale_colour_manual(name = 'Описание', 
                          values =c('mean_line'='black','USL_line'='darkred','LSL_line'='darkblue','SP_line'='red' ), 
                          labels = c(paste('Нижний предел (LSL = ',input$LSL_load,')'),
                                     paste('Значение выборочной средней (n = ',input$n_group_load,')'),
                                     paste('Уставка (SP = ',input$SP_load,')'),
                                     paste('Верхний предел (USL = ',input$USL_load,')')))
    }

})
#график диаграммы плотностей и распределения
output$plot_data_mean_load <- renderPlot({
  
  if(!is.null(get_data())){

    ggplot(get_data()$mean_data, aes(get_data()$mean_data$mean)) +
      geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 8) +
      labs(title = "График распределения выборочных средних", x = "Значение", y = "Количество") +
      geom_vline(aes(xintercept = input$USL_load, colour = "red"))+
      geom_vline(aes(xintercept = input$LSL_load, colour = "blue")) + 
      geom_vline(aes(xintercept = get_data()$Mean, colour = "black"), linetype="dashed") +
      geom_density(aes(y= ..count..),color="darkgreen", fill="lightgreen", alpha = 0.8, adjust = 1)+
      theme_minimal()+
      scale_colour_manual(name = 'Описание', 
                          values =c('red'='red','blue'='blue','black'='black'), 
                          labels = c(paste('Среднее значение процесса (Mean = ',format(round(get_data()$Mean, 2), nsmall=2),')'),
                                     paste('Нижний предел (USL = ',input$LSL_load,')'),
                                     paste('Верхний предел (USL = ',input$USL_load,')'))) 
      
}})
##################### - R карта
#график размаха
output$range_plot_data_group_load <- renderPlot({
  
  if(!is.null(get_data())){
    
    ggplot(get_data()$range_data, aes(x = as.POSIXct(get_data()$range_data$ts/1000, origin="1970-01-01"),y = get_data()$range_data$range, group=1)) +
      geom_line(alpha = 0.5) + 
      geom_point(colour = "seagreen", alpha = 0.8) + 
      theme_minimal() + 
      scale_x_datetime(date_labels = "%H:%M:%S") +
      labs(title = "Значения размахов выборок", x = "Время", y = "Значение")
  }

})    
# диаграмма 
output$range_plot_data_histogramm_load <- renderPlot({
  
  if(!is.null(get_data())){
    
    ggplot(get_data()$range_data, aes(range)) +
      geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 6) +
      labs(title = "Гистограмма размахов выборок", x = "Значение", y = "Количество") +
      
      theme_minimal()
  }

})
##################### - S карта
#график СКО
output$std_plot_data_group_load <- renderPlot({
  
  if(!is.null(get_data())){
    
    ggplot(get_data()$stdDev_data, aes(x = as.POSIXct(get_data()$stdDev_data$ts/1000, origin="1970-01-01"),y = get_data()$stdDev_data$stdDev, group=1)) +
      geom_line(alpha = 0.5) + 
      geom_point(colour = "seagreen", alpha = 0.8) + 
      theme_minimal() + 
      scale_x_datetime(date_labels = "%H:%M:%S") +
      labs(title = "Значения выборочных стандартных отклонений", x = "Время", y = "Значение")
  }
})    
# диаграмма 
output$std_plot_data_histogramm_load <- renderPlot({
  
  if(!is.null(get_data())){
    
    ggplot(get_data()$stdDev_data, aes(stdDev)) +
      geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 6) +
      labs(title = "Гистограмма выборочных стандартных отклонений", x = "Значение", y = "Количество") +
      
      theme_minimal()
  }

})
##################### - S**2 карта
#график дисперсий
output$disp_plot_data_group_load <- renderPlot({
  
  if(!is.null(get_data())){
    
  ggplot(get_data()$disp_data, aes(x = as.POSIXct(get_data()$disp_data$ts/1000, origin="1970-01-01"),y = get_data()$disp_data$disp, group=1)) +
    geom_line(alpha = 0.5) + 
    geom_point(colour = "seagreen", alpha = 0.8) + 
    theme_minimal() + 
    scale_x_datetime(date_labels = "%H:%M:%S") +
    labs(title = "Значения выборочных дисперсий", x = "Время", y = "Значение")
  }
})    
# диаграмма 
output$disp_plot_data_histogramm_load <- renderPlot({
  
  if(!is.null(get_data())){
    
  ggplot(get_data()$disp_data, aes(disp)) +
    geom_histogram(color="darkblue", fill="lightblue", alpha = 0.5, bins = 6) +
    labs(title = "Гистограмма выборочных дисперсий", x = "Значение", y = "Количество") +
    
    theme_minimal()
  }
  
})





#Выходные данные - ТЕКСТ
output$text_Cp_load <- renderText({
  if(!is.null(get_data())){
    
  get_data()$Cp

  }
})
output$text_Сr_load <- renderText({
  
  if(!is.null(get_data())){
    
  get_data()$Cr
}
    })
output$text_Mean_load <- renderText({
  
  if(!is.null(get_data())){
    
  get_data()$Mean
    
  }
})
output$text_Range_load <- renderText({
  
  if(!is.null(get_data())){
  
  get_data()$Range
    
  }
})
output$text_Avg_load <- renderText({
  
  if(!is.null(get_data())){
    
  get_data()$Avg
  }
})
output$text_Sigma_load <- renderText({
  
  if(!is.null(get_data())){
    
  get_data()$Sigma
  }
})
output$text_StdDev_load <- renderText({
  
  if(!is.null(get_data())){
  get_data()$StdDev
  }  
})

output$text_Skew_load <- renderText({
  if(!is.null(get_data())){
    
  get_data()$Skew
  }
})
output$text_Kurtosis_load <- renderText({
  if(!is.null(get_data())){
  get_data()$Kurtosis
  }
})

}



