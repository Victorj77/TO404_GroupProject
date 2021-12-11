#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(geosphere)
library(lubridate)
library(ggplot2)
library(dplyr)
library(knitr)
library(leaflet)

citi <- read.csv("citibike.csv", stringsAsFactors = FALSE)
citi$X <- NULL
str(citi)
citi$start.station.id <- as.factor(citi$start.station.id)
citi$end.station.id <- as.factor(citi$end.station.id)
citi$bikeid <- as.factor(citi$bikeid)
citi$usertype <- as.factor(citi$usertype)
# Fix gender
citi$gender <- ifelse(citi$gender == 1, "male", ifelse(citi$gender == 2, "female", "unknown"))
citi$gender <- as.factor(citi$gender)
# Create a column for approximate age
citi$age <- 2021 - citi$birth.year


library(lubridate)
citi$starttime <- as.POSIXct(strptime(citi$starttime, "%Y-%m-%d %H:%M:%S"))
citi$stoptime <- as.POSIXct(strptime(citi$stoptime, "%Y-%m-%d %H:%M:%S"))
citi$starthour <- hour(citi$starttime)
citi$day <- as.Date(citi$starttime)
citi$month <- as.factor(month(citi$starttime))
citi$numWeekday <- wday(citi$starttime)
citi$dayid <- as.factor(ifelse(citi$numWeekday < 6, "Weekday", "Weekend"))
citi$weekNum <- as.numeric(strftime(citi$starttime, format = "%V"))

library(geosphere)
citi$distmeters <- distHaversine(cbind(citi$start.station.latitude, citi$start.station.longitude), cbind(citi$end.station.latitude, citi$end.station.longitude))
citi$speed <- citi$distmeters / citi$tripduration


library(ggplot2)
citi %>%
    group_by(starthour) %>%
    summarize(count = n(),
              dist = mean(distmeters, na.rm = TRUE),
              dur = mean(tripduration, na.rm = TRUE),
              speed = dist/dur) %>%
    ggplot(aes(x=starthour, y = count, fill = count)) + geom_col()

citi %>%
    group_by(month) %>%
    summarize(count = n(),
              dist = mean(distmeters, na.rm = TRUE),
              dur = mean(tripduration, na.rm = TRUE),
              speed = dist/dur) %>%
    ggplot(aes(x=month, y = count, fill = count)) + geom_col()


#Merging Datasets
weather <- read.csv("NYCWeather2019.csv")
weather$STATION <- NULL
weather$NAME <- NULL
weather$DATE <- as.Date(weather$DATE, format= "%m/%d/%Y")
weather$TAVG <- (weather$TMAX + weather$TMIN)/2
combined_gender <- merge(citi, weather, by.x = "day", by.y = "DATE")

#Formatting data
combined_gender %>%
  group_by(starthour) %>%
  summarize(count = n(),
            dist = mean(distmeters, na.rm = TRUE),
            dur = mean(tripduration, na.rm = TRUE),
            speed = dist/dur) %>%
  ggplot(aes(x=starthour, y = count, fill = count)) + geom_col()

combined_gender %>%
  group_by(month) %>%
  summarize(count = n(),
            dist = mean(distmeters, na.rm = TRUE),
            dur = mean(tripduration, na.rm = TRUE),
            speed = dist/dur) %>%
  ggplot(aes(x=month, y = count, fill = count)) + geom_col()

topbikes <- citisample %>%
  group_by(start.station.name) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(n=10)
topbikes



# Define UI for application that draws a histogram
ui <- (fluidPage(
    titlePanel("CitiBike Data"),
    sidebarLayout(
        sidebarPanel(
            selectInput("start.station.name", "Start station name",
                        choices = c(topbikes))
        ),
        mainPanel(plotOutput("coolplot"),plotOutput("dayplot"),plotOutput("tavgplot"),
                  br(), br(),
                  tableOutput("results"))
    )
)
)

list(citi$start.station.name)

# Define server logic required to draw a histogram
server <- (function(input, output) {
  output$coolplot <- renderPlot({
    filtered <-
      combined_gender %>%
      filter(
        start.station.name == input$start.station.name#,
        #end.station.name == input$end.station.name
      )
    ggplot(filtered, aes(x = starttime, fill = usertype)) +
      geom_histogram()
  })
  output$dayplot <- renderPlot({
    filtered <-
      combined_gender %>%
      filter(
        start.station.name == input$start.station.name#,
        #end.station.name == input$end.station.name
      )
    ggplot(filtered, aes(x = dayid, fill = usertype)) +
      geom_bar()
  })
  output$tavgplot<- renderPlot({
    filtered <-
      combined_gender %>%
      filter(
        start.station.name == input$start.station.name#,
        #end.station.name == input$end.station.name
      )
    ggplot(filtered, aes(x = TAVG, fill = usertype)) +
      geom_histogram()
  })
}
)


# Run the application 
shinyApp(ui = ui, server = server)
