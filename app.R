# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country and group (regional/global) profiles using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, February 2020
#               Updated November 2020 to include group profiles (version 2.0)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 2.4"

library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(jsonlite)


# get the data collection year and list of countries and groups. This is only called once (not reactive)
# and is shared across all sessions.
# Make sure to specify the data being read are UTF-8 encoded
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


json_url <- "https://extranet.who.int/tme/generateJSON.asp"

year_countries <- fromJSON(readLines(paste0(json_url, "?ds=countries"), warn = FALSE, encoding = "UTF-8"))

dcyear <- year_countries$dcyear
countries <- year_countries$countries
rm(year_countries)

# Get the groups
aggregate_groups <- fromJSON(readLines(paste0(json_url, "?ds=groups"), warn = FALSE, encoding = "UTF-8"))
aggregate_groups <- aggregate_groups$groups

# Load general, non-reactive functions
source("general_functions.R")

# Load the standard themes
source("set_plot_theme.R")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- function(request) {

    fixedPage(title = "TB profile",

        # Add a link to the boostrap CSS Sandstone theme downloaded from
        # https://bootswatch.com/3/sandstone/bootstrap.css
        # and a link to a print-specific CSS to hide selectors and metadata and to
        # retain two columns when printing

        tags$head(

            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
            tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media="print"),

            # Make sure columns with numeric values don't wrap
            tags$style(HTML("#estimates_table td:nth-child(2),
                             #estimates_table td:nth-child(3),
                             #uhc_table td:nth-child(2),
                             #notifs_table td:nth-child(2),
                             #drtb_table td:nth-child(2),
                             #tbhiv_table td:nth-child(2),
                             #outcomes_table td:nth-child(3),
                             #prevtx_table td:nth-child(2) {
                               white-space: nowrap;
                             }
                             #tbhiv_table th:nth-child(2), #outcomes_table th:nth-child(3) {
                               text-align: right !important;
                             }
                             #generation {
                               margin-top: 10px;
                               border-top: 0.25px solid #BCBCBC;
                             }
                            "))

                 ),

        fixedRow(id="selectors",

                   column(width = 2,
                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;",

                                   uiOutput(outputId = "entitytypes")

                                   )
                   ),

                 # Choose whether to show the list of countries or the list of groups
                 # based on the choice made above

                   column(width = 5,
                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;",

                                   uiOutput(outputId = "entities")

                                    )
                   ),

                   column(width = 5,

                          tags$div(class = "navbar navbar-inverse",
                                   style = "padding-left: 20px;",

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
                textOutput(outputId = "population", container = h5),

                fixedRow(
                    column(width = 7,

                             # Use htmloutput so can use HTML superscript tags for callouts to footnotes
                             htmlOutput(outputId = "estimates_heading", container = h3),
                             tableOutput(outputId = "estimates_table"),

                             htmlOutput(outputId = "drestimates_heading", container = h3),
                             tableOutput(outputId = "drestimates_table"),

                             htmlOutput(outputId = "uhc_heading", container = h3),
                             tableOutput(outputId = "uhc_table"),

                             textOutput(outputId = "notifs_heading", container = h3),
                             tableOutput(outputId = "notifs_table"),

                             textOutput(outputId = "tbhiv_heading", container = h3),
                             tableOutput(outputId = "tbhiv_table"),

                             textOutput(outputId = "drtb_heading", container = h3),
                             tableOutput(outputId = "drtb_table"),


                             textOutput(outputId = "outcomes_heading", container = h3),
                             tableOutput(outputId = "outcomes_table"),

                             textOutput(outputId = "prevtx_heading", container = h3),
                             tableOutput(outputId = "prevtx_table"),

                             # The financing table should only be shown if dc_finance_display is true
                             conditionalPanel(condition = "output.show_finance == 1",

                                 textOutput(outputId = "finance_heading", container = h3),
                                 tableOutput(outputId = "funding_table")
                             )

                           ),

                    column(width = 5,
                             htmlOutput(outputId = "inc_chart_head", container = h3),
                             textOutput(outputId = "inc_chart_subhead", container = h5),
                             plotOutput(outputId = "inc_chart", height = "250px"),

                             htmlOutput(outputId = "mort_chart_head", container = h3),
                             textOutput(outputId = "mort_chart_subhead", container = h5),
                             plotOutput(outputId = "mort_chart", height = "250px"),

                             textOutput(outputId = "agesex_chart_head", container = h3),
                             textOutput(outputId = "agesex_chart_subhead", container = h5),
                             plotOutput(outputId = "agesex_chart", height = "250px"),

                           # in 2021 data collection year replaced the treatment success time series
                           # chart with a chart on cases attributable to five risk factors
                           # This should only be shown if the estimates exist for the entity
                           conditionalPanel(condition = "output.show_attributable_cases == 1",

                                            textOutput(outputId = "rf_chart_head", container = h3),
                                            textOutput(outputId = "rf_chart_subhead", container = h5),
                                            plotOutput(outputId = "rf_chart", height = "250px")
                           ),


                             # The financing chart should only be shown if dc_finance_display is true
                             conditionalPanel(condition = "output.show_finance == 1",

                                 textOutput(outputId = "funding_chart_head", container = h3),
                                 textOutput(outputId = "funding_chart_subhead", container = h5),
                                 plotOutput(outputId = "funding_chart", height = "250px")
                             )
                           )

                ),


                 # Footnotes
                 htmlOutput(outputId = "foot_est"),
                 htmlOutput(outputId = "foot_dr_defn"),

                 htmlOutput(outputId = "foot_pulm"),
                 htmlOutput(outputId = "foot_newunk"),
                 htmlOutput(outputId = "foot_rrmdr"),

                # Aggregated finance footer that doesn't appear for countries
                htmlOutput(outputId = "foot_aggfin"),

                # Footer that goes on every page
                htmlOutput(outputId = "generation"),

                # Add an "About" bit of metadata
                HTML(paste0("<div id='metadata'>",
                            "<i>",
                            app_version,
                            ", Source code on <a href='https://github.com/hazimtimimi/tb_profiles' target='_blank'>Github</a>. ",
                            "Data are also available on the TB Report app for <a href='https://apps.apple.com/us/app/tb-report/id1483112411' target='_blank'>iOS</a> and
                      <a href='https://play.google.com/store/apps/details?id=uk.co.adappt.whotbreport&hl=en_us' target='_blank'>Android</a>
                      and as <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>CSV files</a>.</i><br /><br /></div>"))

            )
        )

}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Backend server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {


    # Include bookmarks in the URL
    source("bookmark_url.R", local = TRUE)

    # Make sure language choice is valid in case someone messes with URL
    # Note that Shiny will change input$lan to null if it doesn't match the
    # available choices, so use general function check_lan()

    # Create the entity type radio button selector using the chosen language
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    type_name_list <- reactive({

        country <- switch (check_lan(input$lan),
                "FR" = "Pays",
                "ES" = "País",
                "RU" = "Страна",
                "Country" #default!
            )

        group <- switch (check_lan(input$lan),
                "FR" = "Groupe",
                "ES" = "Grupo",
                "RU" = "Группа",
                "Group" #default!
            )

        # Return a list
        return(list(country, group))

    })

    # Build the entity type radio buttons
    output$entitytypes <- renderUI({

        radioButtons(inputId = "entity_type",
                     label = "",
                     choiceNames = type_name_list(),
                     choiceValues = list("country", "group"),

                     # next line needed to avoid losing the selected country when the language is changed
                     selected = input$entity_type,
                     inline = FALSE)
    })


    # Create the country or group selection lists in the selected language
    source("select_entity.R", local = TRUE)

    # Get the JSON file for the labels to be used
    # Make sure to specify the data being read are UTF-8 encoded
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    plabs <- reactive({

       url <- paste0(json_url, "?ds=labels&lan=", check_lan(input$lan))
       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))

       # return the dataframe
       return(json$labels)
    })


    # Get the profile data as a JSON file for the chosen country or group
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    pdata <- reactive({

        if (check_entity_type(input$entity_type) == "group") {
            url <- paste0(json_url, "?ds=group_data&group_code=", input$group_code)
        } else {
            url <- paste0(json_url, "?ds=data&iso2=", input$iso2)
        }

       json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
       return(json)
    })


    # Show or hide the cases attributable to 5 risk factors
    # Some entities don;t have these estimates and the attributable_cases
    # part of the profile data will be null rather than a data frame.
    # If there is no data frame then hide the chart and its heading/subheading
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_attributable_cases <- reactive ({

        req(pdata()$attributable_cases)

        if (is.data.frame(pdata()$attributable_cases)){
            result <- 1
        } else {
            result <- 0
        }

        return(result)
    })

    # Need this to make sure browser can switch attributable cases  elements back on again if hidden
    outputOptions(output, "show_attributable_cases", suspendWhenHidden = FALSE)


    # Show or hide the finance data depending on the country selected
    # They should only be shown if dc_finance_display is true
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    output$show_finance <- reactive({

        req(pdata()$profile_properties)

        if (isTRUE(pdata()$profile_properties[, "dc_finance_display"])){
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
    output$generation <- renderText({  HTML(paste(ltxt(plabs(), "generated"),
                                                 format(Sys.Date(), "%Y-%m-%d"),
                                                 ltxt(plabs(), "by_who")
                                                 )
                                           ) })

    }

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
