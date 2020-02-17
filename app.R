# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country profiles using JSON data retrieved from the
# WHO global tuberculosis database
# Hazim Timimi, February 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Alpha test version 0.1"

library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
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


# Load general, non-reactive functions
source("general_functions.R")

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
                            selectize = FALSE),

                tags$p(tags$i(app_version))
            ),

            # Display result
            mainPanel(
                textOutput(outputId = "main_heading", container = h1),
                tabsetPanel(
                    tabPanel(title = HTML("Estimates<br />(tables)"),
                             textOutput(outputId = "estimates_heading", container = h2),
                             tableOutput(outputId = "estimates_table"),

                             textOutput(outputId = "drestimates_heading", container = h2),
                             tableOutput(outputId = "drestimates_table"),

                             textOutput(outputId = "uhc_heading", container = h2),
                             tableOutput(outputId = "uhc_table")
                             ),

                    tabPanel(title = HTML("Estimates<br />(charts)"),

                            plotOutput(outputId = "inc_chart"),

                            # add some space
                            tags$div(style="margin:1em;",
                                  HTML("&nbsp;")
                                 ),

                            plotOutput(outputId = "mort_chart")
                             ),

                    tabPanel(title = HTML("Notifications<br />(tables)"),

                            textOutput(outputId = "notifs_heading", container = h2),
                            tableOutput(outputId = "notifs_table"),

                            textOutput(outputId = "tbhiv_heading", container = h2),
                            tableOutput(outputId = "tbhiv_table"),

                            textOutput(outputId = "drtb_heading", container = h2),
                            tableOutput(outputId = "drtb_table")

                            ),

                    tabPanel(title = HTML("Notifcations<br />(chart)"),

                            plotOutput(outputId = "agesex_chart")

                            ),

                    # tabPanel(title = HTML("Outcomes<br />(tables & charts)")
                    #
                    #
                    #         ),

                    tabPanel(title = HTML("Financing<br />(tables & charts)"),
                            plotOutput(outputId = "budget_chart")


                            )

                )

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


    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Build all the output objects to display in the application
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


    output$main_heading <- renderText({ ltxt(plabs(), "head") })


    source("build_tab_estimates_tables.R", local = TRUE)

    source("build_tab_estimates_charts.R", local = TRUE)

    source("build_tab_notifs_tables.R", local = TRUE)

    source("build_tab_notifs_charts.R", local = TRUE)

    source("build_tab_finance.R", local = TRUE)

    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")