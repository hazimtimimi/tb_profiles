# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country profiles using JSON data retrieved from the
# WHO global tuberculosis database
# Hazim Timimi, February 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 0.6"

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

    fixedPage(title = "TB profile",

        # Add CSS so that the selectors are not rendered upon printing
        # Use the id I defined for their container row in code below ("selectors")
        # Do the same for the metadata footer
        # Also add a link to the boostrap CSS Sandstone theme downloaded from
        # https://bootswatch.com/3/sandstone/bootstrap.css

        tags$head(
            tags$style(HTML("
                            @media only print {
                                #selectors, #metadata {display:none;}
                            }

                            ")
                       ),

            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")

                 ),

        fixedRow(id="selectors",


                   column(width = 7,
                          tags$div(class = "navbar navbar-inverse",
                                    uiOutput(outputId = "countries")
                                    )
                   ),

                   column(width = 5,

                          tags$div(class = "navbar navbar-inverse",

                             # Note that in Windows the word  Русский doesn't render correctly, but seems OK in Linux/iOS.
                            # If the code below also doesn't work when running on shinyapps.io
                            # then try the workaround suggested at https://stackoverflow.com/questions/59832593/how-to-render-inequalities-in-r-shiny-selectinput-dropdown-list

                            radioButtons(inputId = "lan",
                                        label = "",
                                        choices = c("English" = "EN",
                                                    "Español" = "ES",
                                                    "Français" = "FR",
                                                    "Русский" = "RU"),
                                        inline = TRUE
                                        )
                                      )

                   )
                 ),


        fixedRow(id="main_content",

                textOutput(outputId = "main_heading", container = h1),

                tags$p(
                    tags$i(
                        textOutput(outputId = "population", inline = TRUE))),

                fixedRow(
                    column(width = 7,

                             # Use htmloutput so can use HTML superscript tags for callouts to footnotes
                             htmlOutput(outputId = "estimates_heading", container = h2),
                             tableOutput(outputId = "estimates_table"),

                             htmlOutput(outputId = "drestimates_heading", container = h2),
                             tableOutput(outputId = "drestimates_table"),

                             htmlOutput(outputId = "uhc_heading", container = h2),
                             tableOutput(outputId = "uhc_table"),

                             htmlOutput(outputId = "foot_est"),
                             htmlOutput(outputId = "foot_mdr_defn"),

                             textOutput(outputId = "notifs_heading", container = h2),
                             tableOutput(outputId = "notifs_table"),

                             textOutput(outputId = "tbhiv_heading", container = h2),
                             tableOutput(outputId = "tbhiv_table"),

                             textOutput(outputId = "drtb_heading", container = h2),
                             tableOutput(outputId = "drtb_table"),

                             htmlOutput(outputId = "foot_pulm"),
                             htmlOutput(outputId = "foot_newunk"),
                             htmlOutput(outputId = "foot_rrmdr"),

                             textOutput(outputId = "outcomes_heading", container = h2),
                             tableOutput(outputId = "oucomes_table"),

                             textOutput(outputId = "prevtx_heading", container = h2),
                             tableOutput(outputId = "prevtx_table"),

                             # The financing table should only be shown if dc_form_description is set to 'Long form'
                             # or if it is a short form with budget > 0 (rounded to nearest million)
                             conditionalPanel(condition = "output.show_finance >= 1",

                                 textOutput(outputId = "finance_heading", container = h2),
                                 tableOutput(outputId = "budget_table")
                             )

                           ),

                    column(width = 5,
                             htmlOutput(outputId = "inc_chart_head", container = h2),
                             textOutput(outputId = "inc_chart_subhead", container = h4),
                             plotOutput(outputId = "inc_chart"),

                             htmlOutput(outputId = "mort_chart_head", container = h2),
                             textOutput(outputId = "mort_chart_subhead", container = h4),
                             plotOutput(outputId = "mort_chart"),

                             textOutput(outputId = "agesex_chart_head", container = h2),
                             textOutput(outputId = "agesex_chart_subhead", container = h4),
                             plotOutput(outputId = "agesex_chart"),

                             textOutput(outputId = "tsr_chart_head", container = h2),
                             textOutput(outputId = "tsr_chart_subhead", container = h4),
                             plotOutput(outputId = "tsr_chart"),

                             # The financing chart should only be shown if dc_form_description is set to 'TRUE'Long form'
                             conditionalPanel(condition = "output.show_finance == 2",

                                 textOutput(outputId = "budget_chart_head", container = h2),
                                 textOutput(outputId = "budget_chart_subhead", container = h4),
                                 plotOutput(outputId = "budget_chart")
                             )
                           )

                ),

                # Add the footer that goes on every page
                htmlOutput(outputId = "foot_main", inline = TRUE),

                # Add an "About" bit of metadata
                HTML(paste0("<div id='metadata'>",
                            "<i>",
                            app_version,
                            ", Source code on <a href='https://github.com/hazimtimimi/tb_profiles' target='_blank'>Github</a>. ",
                            "Data are also available on the TB Report app for <a href='https://apps.apple.com/us/app/tb-report/id1483112411' target='_blank'>iOS</a> and
                      <a href='https://play.google.com/store/apps/details?id=uk.co.adappt.whotbreport&hl=en_us' target='_blank'>Android</a>
                      and as <a href='https://www.who.int/tb/country/data/download/en/' target='_blank'>CSV files</a>.</i><br /><br /></div>"))

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
       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })


    # Show or hide the finance data depending on the country selected
    # They should only be shown if dc_form_description is set to 'Long form'
    # Or of it is 'Short form' and the total budget has been reported and is greater than 0
    # when rounded to the nearest million (show the table only, not the chart)
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_finance <- eventReactive(input$iso2, ignoreNULL = TRUE, ignoreInit = TRUE, {

        if (pdata()$profile_properties[, "dc_form_description"] == "Long form"){
            result <- 2

        } else if (pdata()$profile_properties[, "dc_form_description"] == "Short form" &
                   rounder(NZ(pdata()$profile_data[, "tot_req"])) > 0) {
            result <- 1

        } else {
            result <- 0
        }
        return(result)
    })

    # Need this to make sure browser can switch Finance elements back on again if hidden
    outputOptions(output, "show_finance", suspendWhenHidden = FALSE)


    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Build all the output objects to display in the application
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    source("build_header.R", local = TRUE)

    source("build_tab_estimates_tables.R", local = TRUE)

    source("build_tab_estimates_charts.R", local = TRUE)

    source("build_tab_notifs_tables.R", local = TRUE)

    source("build_tab_notifs_charts.R", local = TRUE)

    source("build_tab_outcomes.R", local = TRUE)

    source("build_tab_prevtx.R", local = TRUE)

    source("build_tab_finance.R", local = TRUE)

    # Add the footer that goes on every page
    output$foot_main <- renderText({  HTML(paste("<br /><hr />",
                                                 ltxt(plabs(), "generated"),
                                                 format(Sys.Date(), "%Y-%m-%d"),
                                                 ltxt(plabs(), "by_who")
                                                 )
                                           ) })

    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
