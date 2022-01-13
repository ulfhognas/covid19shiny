library(shiny)
library(shinythemes)
library(ggplot2)

cov19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header=T)
confirmed19 <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header=T)

cov19<- cov19[,-c(3,4)]
confirmed19<- confirmed19[,-c(3,4)]
l <- ncol(confirmed19)

#creates a global sum for all countries and regions
globalcount <- function(somedata){
    l <- ncol(somedata)
    global <- somedata[1,]
    global[1:2] <- c("","Globally")
    global[3:l] <- colSums(somedata[,3:l])
    global
}

#function to sum up countries which are divided into regions
#this is China, Australia, and Canada
totalcount <- function(Country, somedata){
    l <- ncol(somedata)
    countryrows <- which(somedata$Country.Region==Country)
    newcount <- somedata[countryrows[1],]
    newcount[1] <- ""
    newcount[3:l]<-colSums(somedata[countryrows,3:l])
    newcount
}

#making total count rows for those three countries
cov19 <- rbind(cov19, totalcount("China",cov19), 
               totalcount("Australia",cov19),
               totalcount("Canada",cov19),
               globalcount(cov19))

confirmed19 <- rbind(confirmed19, totalcount("China",confirmed19), 
                     totalcount("Australia",confirmed19),
                     totalcount("Canada",confirmed19),
                     globalcount(confirmed19))

cov19 <- cov19[order(cov19$Country.Region),]
confirmed19 <- confirmed19[order(confirmed19$Country.Region),]

#function that calculated 7-day MA for a vector
#output is a vector with first and last three slots as NA
ma7 <- function(v){
    l <- length(v)
    ma <- rep(NA,l)
    for (i in 4:(l-3)){
        ma[i] <- mean(v[(i-3):(i+3)])
    }
    ma
}

#read today's date
nom <- substring(names(cov19[ncol(cov19)]),2)
nom <- gsub("[.]","-",nom)
nom <- as.Date(nom, "%m-%d-%y")

covidplot <- function(country, daysback = (ncol(cov19)-2), type = "deaths",
                      today = nom){
    if (type == "deaths"){
        X <- cov19
    } else {
        X <- confirmed19
    }
    x <- diff(as.numeric(X[which(X$Country.Region==country & 
                                        X$Province.State==""),
                              -c(1,2)]))
    n <- length(x)
    ma <- ma7(as.numeric(x))
    dates <- seq(as.Date("2020-01-23"), nom, by="days")
    df <- data.frame(x = x, ma = ma, date = dates)
    
    df <- df[(n-daysback+1):n,]

    ggplot(df, aes(x=date, y=x, group = 1)) +
        geom_point(size = 0.7) +
        geom_line(aes(y=ma), size = 1, colour = "aquamarine2") +
        ylim(0, max(x)) +
        ylab(type) + 
        ggtitle(type) +
        theme(plot.title = element_text(size = 16, face = "bold"))
}

countrynames <- cov19$Country.Region[which(cov19$Province.State=="")]

#read today's date
nom <- substring(names(cov19[ncol(cov19)]),2)
nom <- gsub("[.]","-",nom)
nom <- as.Date(nom, "%m-%d-%y")


# Define UI 
ui <- fluidPage(
    theme = bslib::bs_theme(
        bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198"),
    titlePanel("Covid-19, Seven-day Moving Averages"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Choose country:", selected = "US", 
                        choices = countrynames),
            sliderInput(inputId = "num", 
                        label = "Days covered", min = 10, max = (ncol(cov19)-2),
                        value = (ncol(cov19)-2)),
            textOutput("sometext")
        ),
        
        mainPanel(
            plotOutput("deaths"),
            plotOutput("confirmed")
        )
    )
)

# 
server <- function(input, output) {
    thematic::thematic_shiny()
    output$deaths <- renderPlot({covidplot(input$country, daysback = input$num, 
                                           type = "deaths")})
    output$confirmed <- renderPlot({covidplot(input$country, daysback = input$num,
                                              type = "confirmed cases")})
    output$sometext <- renderText({"The data come from Center for System Science and Engineering at Johns Hopkins, updated daily. Contact me if you have questions or suggestions: ulf.hognas@gmail.com"})
}

# Run the application 
shinyApp(ui = ui, server = server)
