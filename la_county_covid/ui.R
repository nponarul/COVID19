#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("LA Covid data exploration"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "dateRange",
                "Choose a date range",
                "2020-03-01",
                Sys.Date()
            ),
        selectInput(
            "plotType",
            "Choose a plot type",
            list("Daily", "Cumulative", "N-day Average"),
            selected = "Daily"
        ),
        selectInput(
            "fields",
            "Choose fields to display",
            list("Confirmed Cases", "Hospitalizations", "Deaths"),
            selected = "Hospitalizations",
            multiple = TRUE
        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
