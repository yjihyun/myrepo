library(shiny);library(shinythemes);library(scales);library(rlang)
library(gganimate);library(tidyverse);library(ggplot2);library(lubridate);library(tidyr);library(maps);library(ggthemes); library(readr); library(stringr); library(car);library(dplyr);library(plotly);library(leaflet);library(shinyWidgets)
pacman::p_load(ggrepel, sf, USAboundaries, dygraphs, timetk, xts, geofacet)

#setwd("C:/Users/YOO/Desktop/Senior Project")
#acci <- read.csv("US_Accidents_Dec19.csv", header=TRUE)
train <- read.csv("train.csv", header = TRUE)

##########################function
area_chart = function(filtered_train, level_selectt = c("Month","Day of Week","Time","Humidity","Weather")) {
  if (level_selectt=="Humidity") { #습도 #mutate
    train2 <- filtered_train %>% mutate(humid = case_when(
      Humidity... <30 ~"Dry",
      Humidity... <60 ~"Comfortable",
      TRUE ~"Humid"
    )) %>%  group_by(humid,State) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x = State, y = count, fill = humid,
                           text = paste0("Humidity : ",Humidity...,"\n","Accident : ",count))) + 
      xlab("State")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Day of Week") { #요일
    train2 <- train %>%  group_by(wday(as.Date(Start_Time)),State) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x =State, label = TRUE), y = count, fill = wday(as.Date(Start_Time),
                           text = paste0(wday(as.Date(Start_Time), label = TRUE),"\n","Accident : ",count))) + 
      xlab("State")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Month") { #월 #mutate - season?
    train2 <- filtered_train %>%  group_by(month(Start_Time),State,) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x =State, y = count, fill =  month(Start_Time, label = TRUE, abbr = FALSE),
                           text = paste0(month(Start_Time, label = TRUE, abbr = FALSE), "\n", "Accident : ",count))) + 
      xlab("State")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Time") { #시간 #mutate - 아침,점심,저녁,밤
    train2 <- filtered_train%>% mutate(divi = case_when(
      hour(Start_Time) >5 | hour(Start_Time) <12 ~"Morning", #5am~12pm #7시간
      hour(Start_Time) <17 ~"Afternoon", #12~5pm #5시간
      hour(Start_Time) <21 ~"Evening", #5~9pm #4시간 
      TRUE ~"Night" #9pm~5am #8시간 
    )) %>%  group_by(divi,State,) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x = divi, y = count, fill = State,
                           text = paste0("Hour : ",divi, "\n", "Accident : ",count))) + 
      xlab("State")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Weather") { #날씨  
    train2 <- filtered_train %>%  group_by(Weather_Condition) %>% summarise(count2 = n()) %>%  top_n(5, count2)
    train3 <- train %>% filter(Weather_Condition %in% train2$Weather_Condition) %>% group_by(Weather_Condition,State) %>% mutate(count = n()) 
    g = ggplot(train3, aes(x = State, y = count, fill = Weather_Condition,
                           text = paste0(Weather_Condition, "\n", "Accident : ",count))) + 
      xlab("State")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}
# function to plot cumulative cases by region #plot2
type_chart = function(filtered_train, level_selectt = c("Month","Day of Week","Time","Humidity","Weather")) {
  if (level_selectt=="Humidity") { #습도 
    train2 <- filtered_train %>%  group_by(Humidity...,State) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x = Humidity..., y = count, fill = State,
                           text = paste0("Humidity",Humidity..., "\n", State, ": ",count))) + 
      xlab("Humidity(%)")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Day of Week") { #요일
    train2 <- train %>%  group_by(wday(as.Date(Start_Time)),State) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x =wday(as.Date(Start_Time), label = TRUE), y = count, fill = State,
                           text = paste0(wday(as.Date(Start_Time), label = TRUE), "\n", State, ": ",count))) + 
      xlab("Day of Week")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Month") { #월
    train2 <- filtered_train %>%  group_by(month(Start_Time),State,) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x = month(Start_Time, label = TRUE, abbr = FALSE), y = count, fill = State,
                           text = paste0(month(Start_Time, label = TRUE, abbr = FALSE), "\n", State, ": ",count))) + 
      xlab("Month")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Time") { #시간
    train2 <- filtered_train %>%  group_by(hour(Start_Time),State,) %>% mutate(count = n()) 
    g = ggplot(train2, aes(x = hour(Start_Time), y = count, fill = State,
                           text = paste0("Hour : ",hour(Start_Time), "\n", State, ": ",count))) + 
      xlab("Time")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  
  if (level_selectt=="Weather") { #날씨  
    train2 <- filtered_train %>%  group_by(Weather_Condition) %>% summarise(count2 = n()) %>%  top_n(5, count2)
    train3 <- train %>% filter(Weather_Condition %in% train2$Weather_Condition) %>% group_by(Weather_Condition,State) %>% mutate(count = n()) 
    g = ggplot(train3, aes(x = Weather_Condition, y = count, fill = State,
                           text = paste0(Weather_Condition, "\n", State, ": ",count))) + 
      xlab("Top 5 Weather")+
      geom_bar(position="stack", stat="identity") + 
      ylab("Count") + theme_bw() + 
      theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  }
  ggplotly(g, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

## delete up 
type <- function(train,started, ended){
  train1 <-  train %>% filter(as.Date(Start_Time) >= started & as.Date(Start_Time)  <= ended) %>% select(Start_Time ,Amenity,Bump,Crossing,Give_Way,Junction,No_Exit,Railway,Roundabout,Station,Stop,Traffic_Calming,Traffic_Signal,Turning_Loop)
  for(i in 2:ncol(train1)){
    {train1[, i] <- as.numeric(train1[, i])-1} }
  train2 <- gather(train1, type, count1, Amenity:Turning_Loop, factor_key=TRUE) %>%  
    group_by(type) %>% summarise(count = as.integer(sum(count1))) %>%  top_n(n=3, count)
  type_g <-  ggplot(train2, aes(x=reorder(type, -count), y=count)) +
    geom_bar(stat = "identity",fill = "steelblue")+
    theme(axis.text.x = element_text(angle = 25))+
    ggtitle("Top 3 Accident Type")+
    theme_minimal()+
    labs(y = "# of Car Accident", x = "Type")
  return(type_g)
}
#main graph
main <- function(train,started,ended){
  mg <-  train %>% 
    filter(as.Date(Start_Time) >= started & as.Date(Start_Time)  <= ended) %>% 
    ggplot() +geom_sf(data = us_state, fill = NA) +
    geom_point( mapping = aes(x = Start_Lng, y = Start_Lat, colour = as.factor(Severity)),size = 1)+coord_sf()+
    scale_color_brewer(palette = "Blues", labels = c(1,2,3,4))+
    labs(fill = "Severity")+
    theme_map()+
    guides(fill = guide_legend(nrow=1))+
    theme(legend.position = "bottom")
  return(mg)
}
#leaflet main 
main_leaflet <- function(train,started,ended){
mdata <- train %>% filter(as.Date(Start_Time) >= started & as.Date(Start_Time)  <= ended) %>% select(Start_Lat,Start_Lng,Start_Time,Severity,State,County)
mlf <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lat = subset(mdata,Severity == 1)$Start_Lat,
    lng = subset(mdata,Severity == 1)$Start_Lng,
    color = "lightblue1",
    stroke = FALSE, fillOpacity = 0.5
  )%>%
  addCircleMarkers(
    lat = subset(mdata,Severity == 2)$Start_Lat,
    lng = subset(mdata,Severity == 2)$Start_Lng,
    color = "skyblue",
    stroke = FALSE, fillOpacity = 0.5
  )%>%
  addCircleMarkers(
    lat = subset(mdata,Severity == 3)$Start_Lat,
    lng = subset(mdata,Severity == 3)$Start_Lng,
    color = "mediumturquoise",
    stroke = FALSE, fillOpacity = 0.5
  )%>%
  addCircleMarkers(
    lat = subset(mdata,Severity == 4)$Start_Lat,
    lng = subset(mdata,Severity == 4)$Start_Lng,
    color = "navy",
    stroke = FALSE, fillOpacity = 0.5,
    label = sprintf("<strong>State: %s </strong><br/>County: %s<br/>Date: %s<br/>Severity: 4", mdata$State, mdata$County, as.character(as.Date(mdata$Start_Time))) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px", "color" = "navy"),
      textsize = "15px", direction = "auto"))
  
return(mlf)
}

#return

#####
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "US Car Accident", id="nav",
tabPanel("Mapper",
         div(class="outer",
             tags$head(includeCSS("styles.css")),
             div(leafletOutput("mainPlot",width="100%",height="1000px"),

             absolutePanel(id = "controls", class = "panel panel-default",
                           top = 255, left = 55, width = 250, fixed=TRUE,
                           draggable = TRUE, height = "auto",
                           
                           span(h3(textOutput("count_case"), align = "right"), style="color:#006d2c"), 
                           plotOutput("num_plot", height="150px", width="100%"),
                           sliderInput("Date_range_selector", "Select Date Range",
                               min = as.Date("2016-02-01","%Y-%m-%d"),
                               max = as.Date("2019-12-31","%Y-%m-%d"),
                               value=c(as.Date("2016-02-01"),as.Date("2019-12-31")),
                               timeFormat="%Y-%m-%d",
                               animate=animationOptions(interval = 100, loop = FALSE)) )   ))),

###edit

#page 2

tabPanel("Region plots",
         
         sidebarLayout(
           sidebarPanel(
             
             "Filter",
             span(tags$i(h6("Please choose outcome and state")), style="color:#045a8d"),
             
             pickerInput("level_select", "Step 1 - Select Outcome",
                         choices = c("Month","Day of Week","Time","Humidity","Weather"),
                         selected = c("Month"),
                         multiple = FALSE),
             
             pickerInput("state_select","Step 2 - Select State",
                         choices = as.character(train$State),
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Please Select State(s)"),
                         selected = train$State,
                         multiple = TRUE)
           ),
           
           mainPanel(
             tabsetPanel(
               tabPanel("By Area", plotlyOutput("area_chart_page2")), #plot1
               tabPanel("By Type", plotlyOutput("type_chart_page2")) #plot2
             )
           )
         )
),

####edit ended
tabPanel("Data",
         numericInput("maxrows", "Rows to show", 25),
         verbatimTextOutput("rawtable"),
         downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
         "Data Source: ", tags$a(href="https://www.kaggle.com/sobhanmoosavi/us-accidents",  "Sobhan Moosavi")),
tabPanel("About",
         tags$div(
           tags$h4("Last update"), 
           h6(paste0("June 2020")),
           tags$br(),tags$br(),tags$h4("Sources"),
           tags$b("US Accidents: "), tags$a(href="https://www.kaggle.com/sobhanmoosavi/us-accidents", "US Accidents (3.0 million records) - Kaggle"),tags$br(),
           tags$br(),tags$br(),"Author: Jihyun Yoo",
           tags$br(),"Advisor: Garrett Saunders,Ph.D",tags$br(),
           tags$br(),tags$br(),tags$h4("Contact"),
           "5827756a@gmail.com",tags$br(),tags$br(),
           ))
))

server <- function(input, output, session) {
  output$count_case <- renderText({
    paste0(nrow(train %>% filter(as.Date(Start_Time) >= input$Date_range_selector[1] & as.Date(Start_Time)  <= input$Date_range_selector[2])), " cases")
  })

  output$num_plot <- renderPlot({
    type(train,as.Date(input$Date_range_selector[1]),as.Date(input$Date_range_selector[2]))
    } )
  
  output$mainPlot <- renderLeaflet({
     main_leaflet(train,as.Date(input$Date_range_selector[1]),as.Date(input$Date_range_selector[2]))
})
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(train, input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  ######### delete down
  #create data
  filtered_train = reactive({
    dataf = train
    dataf %>% filter(State %in% input$state_select)
  })
  
  # # update region selections
  # observeEvent(input$level_select, {
  #     updatePickerInput(session = session, inputId = "state_select", 
  #                       choices = as.character(train$State), 
  #                       selected = train$State) }, ignoreInit = TRUE)
  
  #plot1 - by area
  output$area_chart_page2 <- renderPlotly({
    #filtered_train <- train %>% filter(State %in% input$state_select)
    area_chart(filtered_train(), level_selectt = input$level_select)
  })
  #plot2 - by type 
  output$type_chart_page2 <- renderPlotly({
    #filtered_train <- train %>% filter(State %in% input$state_select)
    area_chart(filtered_train(), level_selectt = input$level_select)
  })
  
  ####
}



##
shinyApp(ui = ui, server = server)