library(shiny)

shinyUI(fluidPage(
    titlePanel("COVID-19 Tracker"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", 
                        "Choose a state:", 
                        choices = c("NATIONAL", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
                        selected = "NATIONAL"),
            width = 2
        ),
        mainPanel(
            plotOutput("covid_19_plot", height = 750, width = 1400)
        )
    )
))
