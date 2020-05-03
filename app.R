# Load packages ----
library(shiny)
library(ggplot2)
library(reshape)
library(scales)
#library (readr)
#library(rsconnect)

dat_temp <- read.csv(url("https://raw.githubusercontent.com/Stramon1um/adso_isotherm_models/master/temp.csv"), header = TRUE, sep=";")

dat_temp_2 <- melt(dat_temp, id.vars = c("data","time"), variable_name = "sensor", measure.vars=c("t1", "t2")) 

dat_temp_2$iso <- paste(dat_temp_2$data, dat_temp$time, sep=" ")

dat_temp_2$iso <- as.POSIXct(as.character(dat_temp_2$iso), format="%d/%m/%y %H:%M:%S", tz="UCT")

ui <- fluidPage(
  titlePanel("Maurone's Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      
      dateRangeInput("date", "Date range",
                     start = "2015-07-19",
                     end   = "2015-08-18",
                     min = "2015-07-19",
                     max = "2015-08-18",
                     format = "dd/mm/yyyy",
                     separator = " - "),
      
      #checkboxInput("sensor1", "t1",
                    #value = TRUE),
      
      #checkboxInput("sensor2", "t2",
                    #value = TRUE),
      
      selectInput("sensor", 
                  label = "Choose a sensor to display",
                  choices = c("Sensor 1", "Sensor 2",
                              "All"),
                  selected = "All")
      
    ),
    mainPanel(htmlOutput("min_max"),
              plotOutput("plot"),
              textOutput("errore"))
  )
)

server <- function(input, output) {
  
  #my_range <- reactive({
    #as.POSIXct(input$date[1])
  #})
  
  #my_range_2 <- reactive({
    #as.POSIXct(input$date[2])
  #})
  
  #data <- switch(input$sensor, 
                 #"Sensor 1" = dat_temp_2, sensor == "t1",
                 #"Percent Black" = dat_temp_2, sensor == "t1",
                # "All" = dat_temp_2)
  
  #data<- reactive({
    #if(input$sensor1 & input$sensor2) {
     # ggplot(dat_temp_2, aes(iso, value, color=sensor))+geom_line()+scale_x_datetime(limits = lims_3(), labels=date_format("%d %b"), breaks="2 day")+theme_bw()
   # } else {
     # if(!input$sensor1 & input$sensor2) {
      #  ggplot(subset(dat_temp_2, sensor == "t2"), aes(iso, value, color=sensor))+geom_line()+scale_x_datetime(limits = lims_3(), labels=date_format("%d %b"), breaks="2 day")+theme_bw()
     # } else {
     #   ggplot(subset(dat_temp_2, sensor == "t1"), aes(iso, value, color=sensor))+geom_line()+scale_x_datetime(limits = lims_3(), labels=date_format("%d %b"), breaks="2 day")+theme_bw()
     # }
   # }
  #})
  
  #plot<- reactive({
    #if(input$sensor1 & input$sensor2) {
     # return(dat_temp_2)
    #} else {
      #if(!input$sensor1 & input$sensor2) {
       # subset(dat_temp_2, sensor == "t2")
      #} else {
       # subset(dat_temp_2, sensor == "t1")
     # }
    #}
  #})
  
  #output$errore <- renderText({ 
   # if(!input$sensor1 & !input$sensor2) {
     # "You have selected"
   # }
  #})
  
  output$min_max  <- renderText({
    #paste("<b>Trend Date Range</b>")
    paste("You have chosen a range that goes from: <b>", 
          #paste(as.character(input$date), collapse = " to ", "</b>"))
          as.character(input$date[1]),"</b>to<b>",as.character(input$date[2]),"</b>")
    #)
  })
  
  #lims_3 <- reactive({
   # as.POSIXct(c(as.POSIXct(input$date[1]),as.POSIXct(input$date[2])), format="%d/%m/%y %H:%M:%S", tz="UCT")
  #})
  
  #output$min_max <- renderText({ 
   # my_range()
  #}) 
  
  
  
  output$plot <- renderPlot({
    
    data <- switch(input$sensor, 
                   "Sensor 1" = subset(dat_temp_2, sensor=="t1"),
                   "Sensor 2" = subset(dat_temp_2, sensor=="t2"),
                   "All" = dat_temp_2
                   )
    lims_3 <- reactive({
      as.POSIXct(c(as.POSIXct(input$date[1]),as.POSIXct(input$date[2])), format="%d/%m/%y %H:%M:%S", tz="UCT")
    })
    
    #lims <- as.POSIXct(strptime(c("2015-07-19 23:00","2015-07-25 16:00"), format = "%Y-%m-%d %H:%M"))
    #lims_2 <- as.POSIXct(c(my_range(),my_range_2()), format="%d/%m/%y %H:%M:%S", tz="UCT")
    #lims_3 <- as.POSIXct(c(as.POSIXct(input$date[1]),as.POSIXct(input$date[2])), format="%d/%m/%y %H:%M:%S", tz="UCT")
    ggplot(data, aes(iso, value, color=sensor))+geom_line()+scale_x_datetime(limits = lims_3(), labels=date_format("%d %b"), breaks="2 day")+theme_bw()
  }) 
  
}
# Run the app
shinyApp(ui, server)