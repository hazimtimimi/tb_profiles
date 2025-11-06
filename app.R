# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB country and group (regional/global) profiles using JSON data
# retrieved from the WHO global tuberculosis database
# Hazim Timimi, February 2020
#               Updated November 2020 to include group profiles (version 2.0)
#               Updated September 2024 switching to hicharter graphs (Version 3.0)
#               Updated August 2025 to change country code input paramater from iso2 tp
#                     iso3 code to make it easier for profiles to be called from WHO
#                     country pages such as https://www.who.int/countries/AFG (Version 4.0)
#               Updated October 2025 to produce compact profiles for entities using the compact
#                     data collection form and for which disaggregated estimates are not produced.
#                     Also displayed more detailed charts for entities with agegroup_option==220
#                     because we now have incidence estimates for the more granular age groups such
#                     as 5-9 years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 5.1"

library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(highcharter)
library(jsonlite)
library(gtbreport)


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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- function(request) {

  fixedPage(
    title = "TB profile",

    # Add a link to the boostrap CSS Sandstone theme downloaded from
    # https://bootswatch.com/3/sandstone/bootstrap.css
    # and a link to a print-specific CSS to hide selectors and metadata and to
    # retain two columns when printing

    tags$head(

      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "boot_print.css", media="print"),

      # Make sure columns with numeric values don't wrap
      # Add extra space below charts
      tags$style(HTML("#estimates_table td:nth-child(2),
                             #estimates_table td:nth-child(3),
                             #uhc_table td:nth-child(2),
                             #notifs_table td:nth-child(2),
                             #drtb_table td:nth-child(2),
                             #tbhiv_table td:nth-child(2),
                             #outcomes_table td:nth-child(3),
                             #prevtx_table td:nth-child(2),
                             #funding_table td:nth-child(2) {
                               white-space: nowrap;
                             }
                             #tbhiv_table th:nth-child(2), #outcomes_table th:nth-child(3) {
                               text-align: right !important;
                             }
                             .chart {
                               margin-bottom: 50px;
                             }
                             #generation {
                               margin-top: 10px;
                               border-top: 0.25px solid #BCBCBC;
                             }
                            "))

    ),


    # - - - - - - - - - - - - - - - - - - - - - - - - -
    # Options selectors (entity, language) ----
    # - - - - - - - - - - - - - - - - - - - - - - - - -

    fixedRow(
      id="selectors",

      column(
        width = 2,
        tags$div(class = "navbar navbar-inverse",
                 style = "padding-left: 20px;",

                 uiOutput(outputId = "entitytypes")

        )
      ),

      # Choose whether to show the list of countries or the list of groups
      # based on the choice made above

      column(
        width = 5,
        tags$div(class = "navbar navbar-inverse",
                 style = "padding-left: 20px;",

                 uiOutput(outputId = "entities")

        )
      ),

      column(
        width = 5,
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

    # - - - - - - - - - - - - - - - - - - - - - - - - -
    # Main content ----
    # - - - - - - - - - - - - - - - - - - - - - - - - -

    fixedRow(
      id="main_content",

      column(
        width = 12,

        textOutput(outputId = "main_heading_entity_name", container = h1),
        textOutput(outputId = "snapshot_note", container = p),
        HTML("<br />"),

        tabsetPanel(
          id = "tab",

          # - - - - - - - - - - - - - - - - - - - - - - - - -
          # Panel for the charts ----
          # - - - - - - - - - - - - - - - - - - - - - - - - -

          tabPanel(
            title = htmlOutput(outputId = "charts_tab_name"),
            value = "charts",

            HTML("<br />"),

            fixedRow(
              column(
                width = 6,
                class = "chart",
                highchartOutput(outputId = "inc_chart", height = "300px")
              ),
              column(
                width = 6,
                class = "chart",
                highchartOutput(outputId = "mort_chart", height = "300px")
              )
            ),

            # Only show the other charts if the entity does not use the compact form
            conditionalPanel(condition = "output.compact_form == 0",

                fixedRow(
                  column(
                    width = 6,
                    class = "chart",
                    highchartOutput(outputId = "agesex_chart", height = "350px")
                  ),
                  column(
                    width = 6,
                    class = "chart",
                    # Only show estimates by risk factors if the estimates exist for the entity
                    conditionalPanel(condition = "output.show_attributable_cases == 1",

                                     highchartOutput(outputId = "rf_chart", height = "350px")
                    )
                  )
                ),

                fixedRow(
                  column(
                    width = 6,
                    class = "chart",
                    highchartOutput(outputId = "rr_prop_chart", height = "300px")
                  ),
                  column(
                    width = 6,
                    class = "chart",
                    highchartOutput(outputId = "rr_inc_chart", height = "300px")
                  )
                ),

                fixedRow(
                  column(
                    width = 6,
                    class = "chart",
                    highchartOutput(outputId = "tpt_chart", height = "300px")
                  ),
                  column(
                    width = 6,
                    class = "chart",
                    # The financing chart should only be shown if dc_finance_display is true
                    conditionalPanel(condition = "output.show_finance == 1",

                                     highchartOutput(outputId = "funding_chart", height = "300px")
                    )
                  )
                )
            )
          ),



          # - - - - - - - - - - - - - - - - - - - - - - - - -
          # Panel for the data tables ----
          # - - - - - - - - - - - - - - - - - - - - - - - - -

          tabPanel(
            title = htmlOutput(outputId = "tables_tab_name"),
            value = "tables",

            fixedRow(
              column(
                width = 12,

                textOutput(outputId = "population", container = h3),
                htmlOutput(outputId = "population_source"),

                textOutput(outputId = "estimates_heading", container = h3),
                htmlOutput(outputId = "estimates_source"),
                tableOutput(outputId = "estimates_table"),

                textOutput(outputId = "estimates_changes_heading", container = h3),
                htmlOutput(outputId = "estimates_changes_note"),
                tableOutput(outputId = "estimates_changes_table"),

                textOutput(outputId = "uhc_heading", container = h3),
                tableOutput(outputId = "uhc_table"),
                htmlOutput(outputId = "modelled_catastrophic_costs_source"),

                textOutput(outputId = "notifs_heading", container = h3),
                tableOutput(outputId = "notifs_table"),

                # Only show the other charts if the entity does not use the compact form
                conditionalPanel(condition = "output.compact_form == 0",

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
                                       htmlOutput(outputId = "finance_inclusions_exclusions"),
                                       tableOutput(outputId = "funding_table")
                      )
                  )
              )
            )
          )
        ),


        # - - - - - - - - - - - - - - - - - - - - - - - - -
        # Footer  ----
        # - - - - - - - - - - - - - - - - - - - - - - - - -

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

  # Create the entity type radio button selector using the chosen language ----
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



    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get the JSON file for the labels to be used ----
  # Make sure to specify the data being read are UTF-8 encoded
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  plabs <- reactive({

    url <- paste0(json_url, "?ds=labels&lan=", check_lan(input$lan))
    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))

    # return the dataframe
    return(json$labels)
  })

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Convert an iso3 code passed as a parameter to an iso2 code ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  iso2 <- reactive({
    req(input$iso3)

    iso2 <- countries |>
      filter(iso3==input$iso3) |>
      select(iso2) |>
      as.character()

    return(iso2)
  })

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get the profile data as a JSON file for the chosen country or group ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  pdata <- reactive({

    if (check_entity_type(input$entity_type) == "group") {
      url <- paste0(json_url, "?ds=group_data&group_code=", input$group_code)
    } else {
      url <- paste0(json_url, "?ds=data&iso2=", iso2())
    }

    json <- fromJSON(readLines(url, warn = FALSE, encoding = 'UTF-8'))
    return(json)
  })


  # Get the entity name as a reactive function ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  entity_name <- reactive({

    # Make sure there is a chosen language
    req(input$lan)

    if (check_entity_type(input$entity_type) == "group") {

      name <- switch(check_lan(input$lan),
                     "FR" = pdata()$profile_properties[, "group_description_FR"],
                     "ES" = pdata()$profile_properties[, "group_description_ES"],
                     "RU" = pdata()$profile_properties[, "group_description_RU"],
                     pdata()$profile_properties[, "group_description"] #default!
      )

    } else {

      # Always default back to country
      name <- switch(check_lan(input$lan),
                     "FR" = pdata()$profile_properties[, "country_FR"],
                     "ES" = pdata()$profile_properties[, "country_ES"],
                     "RU" = pdata()$profile_properties[, "country_RU"],
                     pdata()$profile_properties[, "country"] #default!
      )
    }

    return(name)

  })



  # Build the page header using the entity name ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$main_heading_entity_name <- renderText({

    # Make sure there is a chosen language
    req(input$lan)

    return(paste0(ltxt(plabs(), "head"), ": ", entity_name()))


  })



  # Build the snapshot date note ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$snapshot_note <- renderText({

    req(pdata()$profile_data)

    return(paste0(ltxt(plabs(), "snapshot_date"), ": ", pdata()$profile_data[, "snapshot_date"]))


  })



  # Get the tab names ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$charts_tab_name <- renderText({ ltxt(plabs(), "charts") })
  output$tables_tab_name <- renderText({ ltxt(plabs(), "tables") })


  # See if the entity only used the compact form introduced in 2025. In such cases
  # only need to show a few data items

  output$compact_form <- reactive ({

    req(pdata()$profile_properties)

    if (pdata()$profile_properties[, "dc_form_description"] == "Compact form"){
      result <- 1
    } else {
      result <- 0
    }

    return(result)
  })

  # Need this to make sure browser can switch attributable cases  elements back on again if hidden
  outputOptions(output, "compact_form", suspendWhenHidden = FALSE)


  # Show or hide the cases attributable to 5 risk factors ----
  # Some entities don't have these estimates and the attributable_cases
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


  # Show or hide the finance data depending on the country selected ----
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
  # Define the 2025 End TB Strategy milestones ----
  #
  # - a 50% drop in incidence per 100,000 population compared to 2015
  # - a 75% drop in total TB deaths (HIV-negative + HIV-positive) compared to 2015
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  milestone_yr <- "2025"
  inc_milestone_vs_2015 <- 0.5     # a 50% drop in incidence rate compared to 2015
  mort_milestone_vs_2015 <- 0.25   # a 75% drop in total TB deaths compared to 2015




  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Build all the output objects to display in the application ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  source("build_charts.R", local = TRUE)

  source("build_estimates_tables.R", local = TRUE)

  source("build_surveillance_tables.R", local = TRUE)

  source("build_finance_table.R", local = TRUE)




  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Build the footer  ----
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  output$generation <- renderText({

    req(pdata()$profile_data)


    HTML(paste(ltxt(plabs(), "generated"),
               format(Sys.Date(), "%Y-%m-%d"),
               ltxt(plabs(), "by_who")
    )
    )
  })


}

# Run the application
shinyApp(ui = ui, server = server, enableBookmarking = "url")
