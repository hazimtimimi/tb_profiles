# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country profiles using JSON data retrieved from the
# WHO global tuberculosis database
# Hazim Timimi, February 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)
library(jsonlite)


# get the data collection year and list of countries. This is only called once (not reactive)
# and is shared across all sessions.
# Make sure to specify the data being read are UTF-8 encoded
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

json_url <- "https://extranet.who.int/tme/generateJSON.asp"
year_countries <- fromJSON(readLines(paste0(json_url, "?ds=countries"), warn = FALSE, encoding = "UTF-8"))

dcyear <- year_countries$dcyear
countries <- year_countries$countries
rm(year_countries)


# Simple reusable function to return label text from a label tag in a dataframe with
# columns called label_tag and label_text. Note this is a 'non-reactive' function.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ltxt <- function(df, tag){

    df[ df$label_tag==tag , "label_text"]

}

# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}

# Non-reactive functions to build estimate strings
# as best (lo-hi)
format_estimate <- function(best, lo, hi, style="n"){

    if (style=="k"){
        # format number using spaces for thousands separator

        return(paste0(rounder(best), " (", rounder(lo), "-", rounder(hi), ")"))

    } else if (style=="%") {
        # append percent sign to best
        return(paste0(best, "% (", lo, "-", hi, ")"))

    } else {
        # keep numbers as they are

        return(paste0(best, " (", lo, "-", hi, ")"))
    }

}

# Load the standard themes
source("set_plot_theme.R")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- function(request) {

    fluidPage(

        # Application title
        #titlePanel("Country profile test"),

        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                # Note that in Windows the word  Русский doesn't render correctly, but seems OK in Linux/iOS.
                # If the code below also doesn't work when running on shinyapps.io
                # then try the workaround suggested at https://stackoverflow.com/questions/59832593/how-to-render-inequalities-in-r-shiny-selectinput-dropdown-list

                uiOutput(outputId = "countries"),
                selectInput(inputId = "lan",
                            label = "",
                            choices = c("English" = "EN",
                                        "Español" = "ES",
                                        "Français" = "FR",
                                        "Русский" = "RU"),
                            selectize = FALSE)
            ),

            # Display result
            mainPanel(
                textOutput(outputId = "heading_main", container = h1),
                textOutput(outputId = "heading_estimates", container = h2),

                tableOutput(outputId = "estimates_table"),

                textOutput(outputId = "heading_drestimates", container = h2),
                tableOutput(outputId = "drestimates_table"),

                plotOutput(outputId = "inc_chart"),

                # add some space
                tags$div(style="margin:5em;",
                         HTML("&nbsp;")
                        ),


                plotOutput(outputId = "mort_chart")
            )
        )
    )
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Backend server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {


    # Include bookmarks in the URL
    source("bookmark_url.R", local = TRUE)

    # Create the country selection list in the selected language
    source("select_country.R", local = TRUE)


    # Get the JSON file for the labels to be used
    # Make sure to specify the data being read are UTF-8 encoded
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    plabs <- reactive({

       url <- paste0(json_url, "?ds=labels&lan=", input$lan)
       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))

       # return the dataframe
       return(json$labels)
    })


    # Get the profile data as a JSON file for the chosen country
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    pdata <- reactive({

       url <- paste0(json_url, "?ds=data&iso2=", input$iso2)
       json <- fromJSON(readLines(url, warn = FALSE))
       return(json)
    })


    # Build the output objects to display in the application
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    source("build_output.R", local = TRUE)


}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
