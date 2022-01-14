library(shiny)
library(shinythemes)
library(ggplot2)
library(shinydashboard)

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
                      daterange,
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
    startindex <- which(df$date == daterange[1])
    stopindex <- which(df$date == daterange[2])
    df <- df[startindex:stopindex,]

    plot1 <- ggplot(df, aes(x=date, y=x, group = 1)) +
        geom_point(size = 0.7) +
        geom_line(aes(y=ma), size = 1, colour = "aquamarine2") +
        ylim(0, max(x)) +
        ylab(type) + 
        ggtitle(paste("Daily",type)) +
        theme(plot.title = element_text(size = 16, face = "bold"))
    cumulative <- as.integer(sum(df$x))
    return(list(cumul = cumulative, plot = plot1))
}

countrynames <- cov19$Country.Region[which(cov19$Province.State=="")]

#read today's date
nom <- substring(names(cov19[ncol(cov19)]),2)
nom <- gsub("[.]","-",nom)
nom <- as.Date(nom, "%m-%d-%y")
dates <- seq(as.Date("2020-01-23"), nom, by="days")




# Define UI 
ui <- fluidPage(
    theme = bslib::bs_theme(
        bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198"),
    titlePanel("Covid-19, Seven-day Moving Averages"),
    sidebarLayout(
        sidebarPanel(
            selectInput("country", "Country:", selected = "US", 
                        choices = countrynames),
            sliderInput("daterange", "Date range:", 
                           min = as.Date("2020-01-23"),
                           max = nom,
                        value = c(as.Date("2020-01-23"),nom)
                           ),
            textOutput("sometext4"),
            textOutput("cumuldeaths"),
            textOutput("cumulconfirmed"),
            textOutput("sometext"),
            textOutput("hopkins"),
            textOutput("link")
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
    output$cumuldeaths <- renderText(paste(
                                          covidplot(input$country,
                                                    daterange = input$daterange,
                                                    type = "deaths")$cumul,
                                          "deaths and"))
    output$cumulconfirmed <- renderText(paste(
                                         covidplot(input$country,
                                                   daterange = input$daterange,
                                                   type = "confirmed")$cumul,
                                         "confirmed cases."))
    output$sometext4 <- renderText({"In the selected range:"})
    output$deaths <- renderPlot({covidplot(input$country, 
                                           daterange = input$daterange, 
                                           type = "deaths")$plot})
    output$confirmed <- renderPlot({covidplot(input$country, 
                                              daterange =  input$daterange,
                                              type = "confirmed cases")$plot})
    output$sometext <- renderText({
        "Made by Ulf Högnäs: ulf.hognas@gmail.com"
        })
    output$hopkins <- renderText({
        "The data are drawn from a repository maintained by CSSE at Johns Hopkins University:"
        })
    output$link <- renderText({
        "https://github.com/CSSEGISandData/COVID-19"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
