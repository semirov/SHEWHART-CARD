library(markdown)
library(shiny)
library(ggplot2)

#Навигационная панель
navbarPage("Статистика!",
           tabPanel("Карты контроля (реальное время)",
           tabsetPanel(type = "tabs",
           tabPanel("X-Карта",
                    sidebarLayout(
                      sidebarPanel(width = 3,align = "center", h5("Расчетная формула:"),br(), img(src="mean_formula.PNG", align = "center",height = 60)),
                    mainPanel(
                       
                                plotOutput("plot_data_group"),
                                
                            #plotOutput("plot_data_clear"),
                            
                             plotOutput("plot_data_mean")
                        
                        
                      
                    ))),
           tabPanel("R-Карта",
                    sidebarLayout(
                      sidebarPanel(width = 3,align = "center", h4("Расчетная формула:"),br(), img(src="range_formula.PNG", align = "center",height = 20)),
                    mainPanel(
                      
                      
                      plotOutput("range_plot_data_group"),
                      plotOutput("range_plot_data_histogramm")
                      
                      
                      
                    ))),
           tabPanel("S-Карта",
                    sidebarLayout(
                      sidebarPanel(width = 3,align = "center", h4("Расчетная формула:"),br(), img(src="sd_formula.png", align = "center",height = 60)),
                    mainPanel(
                      
                      
                      plotOutput("std_plot_data_group"),
                      plotOutput("std_plot_data_histogramm")
                      
                      
                      
                    ))),
           tabPanel("S**2-Карта",
                    sidebarLayout(
                      sidebarPanel(width = 3,align = "center", h4("Расчетная формула:"),br(), img(src="disp_formula.png", align = "center",height = 60)),
                      mainPanel(
                        
                        plotOutput("disp_plot_data_group"),
                        
                        plotOutput("disp_plot_data_histogramm")
                        
                        
                        
                      ))),
           tabPanel("Статистические параметры",
                    fluidRow( h4("Cp"),
                              column(2, textOutput("text_Cp")),
                              column(8, "- потенциальная пригодность процесса ")
                    ),
                    hr(),
                    fluidRow( h4("Сr"),
                              column(2, textOutput("text_Сr")),
                              column(8, "- коэффициени пригодности процесса")
                    ),
                    hr(),
                    fluidRow( h4("Mean"),
                              column(2, textOutput("text_Mean")),
                              column(8, "- ожидаемое среднее значение процесса")
                    ),
                    hr(),
                    fluidRow( h4("Avg"),
                              column(2, textOutput("text_Avg")),
                              column(8, "- фактическое среднее значение процесса")
                    ),
                    hr(),
                    fluidRow( h4("Range"),
                              column(2, textOutput("text_Range")),
                              column(8, "- значение размаха выборки")
                    ),
                    hr(),
                    fluidRow( h4("Sigma"),
                              column(2, textOutput("text_Sigma")),
                              column(8, "- фактическое стандартное (среднеквадратическое) отклонение процесса")
                    ),
                    hr(),
                    fluidRow( h4("StdDev"),
                              column(2, textOutput("text_StdDev")),
                              column(8, "- фактическое стандартное (среднеквадратическое) отклонение выборок")
                    ),
                    hr(),
                    fluidRow( h4("Skew"),
                              column(2, textOutput("text_Skew")),
                              column(8, "- cимметрия кривой распределения")
                    ),
                    hr(),
                    fluidRow( h4("Kurtosis"),
                              column(2, textOutput("text_Kurtosis")),
                              column(8, "- относительная высота кривой распределения")
                    ),
                    hr(),
                    fluidRow( h4("USL"),
                              column(2, numericInput("USL",label = NULL,  value = 225)),
                              column(8, "- верхний установочный предел")
                    ),
                    hr(),
                    fluidRow( h4("LSL"),
                              column(2, numericInput("LSL",label = NULL, value = 215)),
                              column(8, "- нижний установочный предел")
                    ),
                    hr(),
                    fluidRow( h4("SP"),
                              column(2, numericInput("SP",label = NULL, value = 220)),
                              column(8, "- Уставка")
                    ),
                    hr()
                    
                    
                    
                    
           ),
           
           tabPanel("Настройки",
                    titlePanel("Настройка входных данных"),
                    sidebarLayout(
                      
                      sliderInput("n_clear", 
                                  "Число чистых элементов в кадре:", 
                                  value = 1000,
                                  min = 1, 
                                  max = 2000),
                      sliderInput("n_group", 
                                  "Число элементов в группе усреднения:", 
                                  value = 5,
                                  min = 1, 
                                  max = 100)
                      
                    )
           )
           
           )),
           tabPanel("Карты контроля (архив)",
                    sidebarLayout(
                      sidebarPanel(
                        h4("Начало"),
                        dateInput("start_date", label = h5("Дата:"), value = Sys.time(), language = "ru"),
                        sliderInput("start_h", h5("Часы:"), min=0, max=23, value=format(Sys.time(), "%H"), step = 1),
                        sliderInput("start_m", h5("Минуты:"),min = 0, max = 59, value =as.numeric(format(Sys.time(), "%M")) - 5, step = 1),
         
                        h4("Конец"),
                        dateInput("end_date", label = h5("Дата:"), value = Sys.time(), language = "ru"),
                        sliderInput("end_h", h5("Часы:"), min=0, max=23, value=format(Sys.time(), "%H"), step = 1),
                        sliderInput("end_m", h5("Минуты:"),min = 0, max = 59, value = format(Sys.time(), "%M"), step = 1),
                        sliderInput("n_group_load", h4("Число точек в подгруппе:"),min = 1, max = 100, value = 5, step = 1),
                        h4("USL - Верхний предел"),
                        numericInput("USL_load",label = NULL,  value = 225),
                        h4("LSL - Нижний предел"),
                        numericInput("LSL_load",label = NULL, value = 215),
                        h4("SP - Уставка"),
                        numericInput("SP_load",label = NULL, value = 220),
                        hr(),
                        actionButton("loadButton", "Загрузить данные")
                                     
                        
                      ),
                      mainPanel(
                        tabsetPanel(type = "tabs", 
                                    tabPanel("X - карта",
                                             plotOutput("plot_data_group_load"),
                                             plotOutput("plot_data_mean_load")
                                             ),
                                    tabPanel("R - карта",
                                             plotOutput("range_plot_data_group_load"),
                                             plotOutput("range_plot_data_histogramm_load")
                                             
                                             ),
                                    tabPanel("S - карта",
                                             plotOutput("std_plot_data_group_load"),
                                             plotOutput("std_plot_data_histogramm_load")
                                             
                                             ),
                                    tabPanel("S**2 - карта",
                                             plotOutput("disp_plot_data_group_load"),
                                             plotOutput("disp_plot_data_histogramm_load")
                                             
                                    ),
                                    tabPanel("Статистические параметры",
                                             fluidRow( h4("Cp"),
                                                       column(2, textOutput("text_Cp_load")),
                                                       column(8, "- потенциальная пригодность процесса ")
                                             ),
                                             hr(),
                                             fluidRow( h4("Сr"),
                                                       column(2, textOutput("text_Сr_load")),
                                                       column(8, "- коэффициени пригодности процесса")
                                             ),
                                             hr(),
                                             fluidRow( h4("Mean"),
                                                       column(2, textOutput("text_Mean_load")),
                                                       column(8, "- ожидаемое среднее значение процесса")
                                             ),
                                             hr(),
                                             fluidRow( h4("Avg"),
                                                       column(2, textOutput("text_Avg_load")),
                                                       column(8, "- фактическое среднее значение процесса")
                                             ),
                                             hr(),
                                             fluidRow( h4("Range"),
                                                       column(2, textOutput("text_Range_load")),
                                                       column(8, "- значение размаха выборки")
                                             ),
                                             hr(),
                                             fluidRow( h4("Sigma"),
                                                       column(2, textOutput("text_Sigma_load")),
                                                       column(8, "- фактическое стандартное (среднеквадратическое) отклонение процесса")
                                             ),
                                             hr(),
                                             fluidRow( h4("StdDev"),
                                                       column(2, textOutput("text_StdDev_load")),
                                                       column(8, "- фактическое стандартное (среднеквадратическое) отклонение выборок")
                                             ),
                                             hr(),
                                             fluidRow( h4("Skew"),
                                                       column(2, textOutput("text_Skew_load")),
                                                       column(8, "- cимметрия кривой распределения")
                                             ),
                                             hr(),
                                             fluidRow( h4("Kurtosis"),
                                                       column(2, textOutput("text_Kurtosis_load")),
                                                       column(8, "- относительная высота кривой распределения")
                                             ),
                                             hr()
                                             
                                             )
                                    
                                    )
                        
                        
                        
                        
                      )
                    )
                    
                    
                    )
              
          
           
)
                      
 #эти данные должны быть в базе
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Cp', 'Потенциальная пригодность процесса');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Сr', 'Коэффициени пригодности процесса');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Mean', 'Ожидаемое среднее значение процесса');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Avg', 'Фактическое среднее значение процесса');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Std', 'Фактическое стандартное (среднеквадратическое) отклонение процесса');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Skew', 'Симметрия кривой распределения');
#INSERT INTO `stat_test`.`param` (`name`, `description`) VALUES ('Kurtosis', 'Относительная высота кривой распределения');















                     

   
  
