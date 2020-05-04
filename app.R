# Load packages ----
library(shiny)
library(ggplot2)
library(reshape)
library(scales)
library(plotly)
#library(rsconnect)

dat_temp <- read.csv(url("https://raw.githubusercontent.com/Stramon1um/shiny_app/master/temp_fixed.csv"), header = TRUE, sep=",")

#dat_temp_2 <- melt(dat_temp, id.vars = c("data","time"), variable_name = "sensor", measure.vars=c("t1", "t2")) 

#dat_temp_2$iso <- paste(dat_temp_2$data, dat_temp$time, sep=" ")

#dat_temp_2$iso <- as.POSIXct(as.character(dat_temp_2$iso), format="%d/%m/%y %H:%M:%S", tz="UCT")

#dat_temp_3 <- read.csv("temp_2.csv", header = TRUE, sep=",")

dat_temp$iso <- as.POSIXct(dat_temp$iso)

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
      
      selectInput("sensor", 
                  label = "Choose a sensor to display",
                  choices = c("Sensor 1", "Sensor 2",
                              "All"),
                  selected = "All"),
      width = 3 #il totale tra side e main è 12
      
    ),
    mainPanel(htmlOutput("min_max"),
              plotlyOutput("plot",height = 800), width = 9)
  )
)

server <- function(input, output) {
  
  output$min_max  <- renderText({
    paste("You have chosen a range that goes from: <b>", 
          #paste(as.character(input$date), collapse = " to ", "</b>"))
          as.character(input$date[1]),"</b>to<b>",as.character(input$date[2]),"</b>")
  })
  
  output$plot <- renderPlotly({
    
    data <- switch(input$sensor, 
                   "Sensor 1" = subset(dat_temp, sensor=="t1"),
                   "Sensor 2" = subset(dat_temp, sensor=="t2"), 
                   "All" = dat_temp
    )
    
    color_line <- switch(input$sensor, 
                   "Sensor 1" = "red",
                   "Sensor 2" = "blue", 
                   "All" = c("red","blue")
    )
    
    lims_3 <- reactive({
      as.POSIXct(c(as.POSIXct(input$date[1]),as.POSIXct(input$date[2])), format="%d/%m/%y %H:%M:%S", tz="UCT")
    })
    
    ggplotly(
      ggplot(data, aes(iso, value, color=sensor))+geom_line()+scale_x_datetime(limits = lims_3(), labels=date_format("%d-%m"), breaks="2 day")+scale_color_manual(values=c(color_line))+xlab("Day")+ylab("Temperature")+theme_bw()+theme(axis.text = element_text(size = 16), axis.text.x = element_text(angle = 30, hjust = 1), axis.title = element_text(size = 18, face = "bold"), legend.title = element_blank(), legend.text = element_text(size = 16)),
    tooltip=c("sensor","value"))
    }) 
  
}
# Run the app
shinyApp(ui, server)