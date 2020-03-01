# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country profiles using JSON data retrieved from the
# WHO global tuberculosis database
# Hazim Timimi, February 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Beta test version 0.4"

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

        # Add CSS so that the sidebar is not rendered upon printing
        # Use the id I defined for it in code below ("side_bar")
        # And same for the tabset, id of "main_tabs"
        tags$head(
            tags$style(HTML("
                            @media only print {
                                #side_bar, #main_tabs {display:none;}
                            }")
                       )
                 ),


        sidebarLayout(
            sidebarPanel(id="side_bar",

                # Note that in Windows the word  Русский doesn't render correctly, but seems OK in Linux/iOS.
                # If the code below also doesn't work when running on shinyapps.io
                # then try the workaround suggested at https://stackoverflow.com/questions/59832593/how-to-render-inequalities-in-r-shiny-selectinput-dropdown-list

                radioButtons(inputId = "lan",
                            label = "",
                            choices = c("English" = "EN",
                                        "Español" = "ES",
                                        "Français" = "FR",
                                        "Русский" = "RU"),
                            ),

                uiOutput(outputId = "countries"),

                HTML("<p style='margin-top:10em;'>&nbsp;</p><hr /><p>Data also available on the TB Report app for <a href='https://apps.apple.com/us/app/tb-report/id1483112411' target='_blank'>iOS</a> and
                      <a href='https://play.google.com/store/apps/details?id=uk.co.adappt.whotbreport&hl=en_us' target='_blank'>Android</a>
                      and as <a href='https://www.who.int/tb/country/data/download/en/' target='_blank'>CSV files</a></p>"),


                tags$p(tags$i(app_version)),

                HTML("<p><i>Source code on <a href='https://github.com/hazimtimimi/tb_profiles' target='_blank'>Github</a></i></p>")

            ),


            mainPanel(

                textOutput(outputId = "main_heading", container = h1),

                tags$p(
                    tags$i(
                        textOutput(outputId = "population", inline = TRUE))),

                tabsetPanel(id = "main_tabs",
                    tabPanel(title = htmlOutput(outputId = "est_tab_name", inline = TRUE), value = "est_tab",

                             # Use htmloutput so can use HTML superscript tags for callouts to footnotes
                             htmlOutput(outputId = "estimates_heading", container = h2),
                             tableOutput(outputId = "estimates_table"),

                             # Plot the two charts side by side
                             fluidRow(column(width = 6,
                                             plotOutput(outputId = "inc_chart")
                                             ),
                                      column(width = 6,
                                             plotOutput(outputId = "mort_chart")
                                             )
                                     ),


                             htmlOutput(outputId = "drestimates_heading", container = h2),
                             tableOutput(outputId = "drestimates_table"),

                             htmlOutput(outputId = "uhc_heading", container = h2),
                             tableOutput(outputId = "uhc_table"),

                             htmlOutput(outputId = "foot_est"),
                             htmlOutput(outputId = "foot_mdr_defn")

                             ),

                    tabPanel(title = htmlOutput(outputId = "not_tab_name", inline = TRUE), value = "not_tab",

                             textOutput(outputId = "notifs_heading", container = h2),
                             tableOutput(outputId = "notifs_table"),

                             plotOutput(outputId = "agesex_chart"),

                             textOutput(outputId = "tbhiv_heading", container = h2),
                             tableOutput(outputId = "tbhiv_table"),

                             textOutput(outputId = "drtb_heading", container = h2),
                             tableOutput(outputId = "drtb_table"),

                             htmlOutput(outputId = "foot_pulm"),
                             htmlOutput(outputId = "foot_newunk"),
                             htmlOutput(outputId = "foot_rrmdr")

                            ),

                    tabPanel(title = htmlOutput(outputId = "out_tab_name", inline = TRUE), value = "out_tab",

                             textOutput(outputId = "outcomes_heading", container = h2),

                             tableOutput(outputId = "oucomes_table"),

                             plotOutput(outputId = "tsr_chart")

                            ),

                    tabPanel(title = htmlOutput(outputId = "tpt_tab_name", inline = TRUE), value = "tpt_tab",

                             textOutput(outputId = "prevtx_heading", container = h2),

                             tableOutput(outputId = "prevtx_table")


                            ),


                    # The financing tab should only be shown if dc_form_description is set to 'TRUE'Long form'
                    tabPanel(title = htmlOutput(outputId = "fin_tab_name", inline = TRUE), value = "fin_tab",

                            textOutput(outputId = "finance_heading", container = h2),

                            tableOutput(outputId = "budget_table"),

                            plotOutput(outputId = "budget_chart")

                            )

                ),

                # Add the footer that goes on every page
                htmlOutput(outputId = "foot_main", inline = TRUE)


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
       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })


    # Show or hide the finance tab depending on the country selected
    # The financing tab should only be shown if dc_form_description is set to 'Long form'
    # Or of it is 'Short form' and the total budget has been reported and is greater than 0
    # when rounded to the nearest million
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    observeEvent(input$iso2, ignoreNULL = TRUE, ignoreInit = TRUE, {

        if (pdata()$profile_properties[, "dc_form_description"] == "Long form"){
            showTab(inputId = "main_tabs", target = "fin_tab")

        } else if (pdata()$profile_properties[, "dc_form_description"] == "Short form" &
                   rounder(NZ(pdata()$profile_data[, "tot_req"])) > 0) {
            showTab(inputId = "main_tabs", target = "fin_tab")

        } else {
            hideTab(inputId = "main_tabs", target = "fin_tab")

        }

    })


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
