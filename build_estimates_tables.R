
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the estimates tables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Population ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$population <- renderText({

  # Make sure data are loaded
  req(pdata()$profile_estimates)

  paste0(ltxt(plabs(), "pop"),
         ", ",
         dcyear - 1,
         ": ",
         rounder_mil(pdata()$profile_estimates[, "e_pop_num"]/1e6),
         " ",
         ltxt(plabs(), "million"))
})

output$population_source <- renderText({ HTML(paste0("<i>", ltxt(plabs(), "est_unpd"),
                                                     # If showing Ukraine 2023 then add comment about the estimate
                                                     ifelse(input$entity_type == "country" & input$iso2 == "UA" & dcyear==2024,
                                                            paste0(" ",
                                                                   ltxt(plabs(), "note_ukraine")),
                                                            ""),
                                                     "</i>")) })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Incidence and mortality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$estimates_heading <- renderText({ ltxt(plabs(), "estimates") })

output$estimates_source <- renderText({  HTML(paste0("<i>",
                                                     ltxt(plabs(), "foot_est"),
                                                     # Insert extra notes for Cambodia and Ukraine 2024
                                                     ifelse(input$entity_type == "country" & input$iso2 == "KH" & dcyear==2024,
                                                            paste0("<br />",
                                                                   ltxt(plabs(), "note_cambodia")),
                                                            ""),
                                                     ifelse(input$entity_type == "country" & input$iso2 == "UA" & dcyear==2024,
                                                            paste("<br />",
                                                                  ltxt(plabs(), "note_ukraine"),
                                                                  ltxt(plabs(), "note_ukraine_2")),
                                                            ""),
                                                     "</i>")) })


estimates_table_content <- reactive({

  # make sure there are data to display
  req(pdata()$profile_estimates)


  # build the dataframe manually
  row_head <- c(paste0(ltxt(plabs(), "incidence_tb"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "incidence_tbhiv"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "incidence_rr"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "mortality_hivneg"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "mortality_hivpos"), ", ", dcyear-1)
  )


  est_num <- c( format_estimate(pdata()$profile_estimates[, "e_inc_num"],
                                pdata()$profile_estimates[, "e_inc_num_lo"],
                                pdata()$profile_estimates[, "e_inc_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_inc_tbhiv_num"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_inc_rr_num"],
                                pdata()$profile_estimates[, "e_inc_rr_num_lo"],
                                pdata()$profile_estimates[, "e_inc_rr_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_mort_exc_tbhiv_num"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_mort_tbhiv_num"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_num_hi"],
                                style="k")
                )



  est_rate <- c(format_estimate(pdata()$profile_estimates[, "e_inc_100k"],
                                pdata()$profile_estimates[, "e_inc_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_inc_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_inc_rr_100k"],
                                pdata()$profile_estimates[, "e_inc_rr_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_rr_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_mort_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_100k_hi"])
                )


  df <- data.frame(row_head, est_num, est_rate)

  # add the column names
  names(df) <- c("", ltxt(plabs(), "number"), ltxt(plabs(), "rate_100k"))
  return(df)
})

output$estimates_table <- renderTable({ estimates_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      na="")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Changes in incidence and mortality since 2015----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$estimates_changes_heading <- renderText({ ltxt(plabs(), "estimates_changes") })

output$estimates_changes_note <- renderText({

  changes_note <- paste0("<i>",
                         ltxt(plabs(), "changes_note_1"),
                         " ",
                         ltxt(plabs(), "changes_note_2"),
                         "</i>")

  changes_note <- stringr::str_replace(changes_note, "%s0", milestone_yr)

  changes_note <- stringr::str_replace(changes_note, "%s1", paste0((1-inc_milestone_vs_2015)*100, "%"))

  changes_note <- stringr::str_replace(changes_note, "%s2", paste0((1-mort_milestone_vs_2015)*100, "%"))

  return(changes_note)

})

estimates_changes_table_content <- reactive({

  # make sure there are data to display
  req(pdata()$epi_timeseries)


  # build the dataframe manually
  row_head <- c(paste0(ltxt(plabs(), "delta_inc"), dcyear-1),
                paste0(ltxt(plabs(), "delta_deaths"), dcyear-1)
  )

 est_changes <- c(

   pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_inc_100k"],
                          pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_inc_100k"],
                          ltxt(plabs(), "reduction"),
                          ltxt(plabs(), "increase")),

   pct_change_description(pdata()$epi_timeseries[pdata()$epi_timeseries$year == 2015, "e_mort_num"],
                          pdata()$epi_timeseries[pdata()$epi_timeseries$year == dcyear-1, "e_mort_num"],
                          ltxt(plabs(), "reduction"),
                          ltxt(plabs(), "increase"))

 )

 df <- data.frame(row_head, est_changes)
 return(df)
})

output$estimates_changes_table <- renderTable({ estimates_changes_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      # right-align the data column
                                      align = "lr",
                                      # suppress column headers
                                      colnames = FALSE,
                                      na="")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# UHC and social protection ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$uhc_heading <- renderText({ ltxt(plabs(), "uhc")  })


uhc_table_content <- reactive({

  # make sure there are data to display
  req(pdata()$profile_estimates)

  # build the table manually
  row_head <-  c(paste0(ltxt(plabs(), "tx_coverage_v2"), ", ", dcyear - 1),

                 paste0(ltxt(plabs(), "cfr"), ", ", dcyear - 1),

                 # catastrophic costs: check whether the data exist and whether they
                 # are from a survey or from modelled estimates
                 case_when(
                   check_entity_type(input$entity_type) == "country" & !is.na(pdata()$profile_estimates[, "catast_pct"]) ~

                     paste0(ltxt(plabs(), "catastrophic_costs"),
                            " (",
                            ltxt(plabs(), "national survey"),
                            # If showing Nepal 2023 then state results are provisional
                            ifelse(input$iso2 == "NP" & dcyear==2024,
                                   ltxt(plabs(), "note_nepal"),
                                   ""),
                            "), ",
                            pdata()$profile_estimates[, "catast_survey_year"]
                     ),

                   check_entity_type(input$entity_type) == "country" & !is.na(pdata()$profile_estimates[, "catast_model_pct"]) ~

                     paste0(ltxt(plabs(), "catastrophic_costs"),
                            " (",
                            ltxt(plabs(), "modelled"),
                            "*), ",
                            pdata()$profile_estimates[, "catast_model_year"]
                     ),

                   check_entity_type(input$entity_type) == "group" & !is.na(pdata()$profile_estimates[, "catast_pct"]) ~

                     paste0(ltxt(plabs(), "catastrophic_costs"),
                            " (",
                            ltxt(plabs(), "pooled"),
                            " ",
                            pdata()$profile_estimates[, "catast_survey_year"],
                            ")"

                     ),

                   .default = ltxt(plabs(), "catastrophic_costs")
                 ))

  row_data <- c(# treatment coverage
                format_estimate(pdata()$profile_estimates[, "c_cdr"],
                                pdata()$profile_estimates[, "c_cdr_lo"],
                                pdata()$profile_estimates[, "c_cdr_hi"],
                                style="%"),

                # case fatality ratio -- convert from a ratio to percent
                format_estimate(pdata()$profile_estimates[, "cfr"] * 100,
                                pdata()$profile_estimates[, "cfr_lo"] * 100,
                                pdata()$profile_estimates[, "cfr_hi"] * 100,
                                style="%"),

                # catastrophic costs: check whether the data exist and whether they
                # are from a survey or from modelled estimates
                case_when(
                  !is.na(pdata()$profile_estimates[, "catast_pct"]) ~
                    format_estimate(pdata()$profile_estimates[, "catast_pct"],
                                    pdata()$profile_estimates[, "catast_pct_lo"],
                                    pdata()$profile_estimates[, "catast_pct_hi"],
                                    style="%"),

                  check_entity_type(input$entity_type) == "country" & !is.na(pdata()$profile_estimates[, "catast_model_pct"]) ~
                    format_estimate(pdata()$profile_estimates[, "catast_model_pct"],
                                    pdata()$profile_estimates[, "catast_model_pct_lo"],
                                    pdata()$profile_estimates[, "catast_model_pct_hi"],
                                    style="%"),

                  .default = ""
                ))

  df <- data.frame(row_head, row_data)

  return(df)
})

uhc_table_to_render <-

output$uhc_table <- renderTable({ uhc_table_content() },
                                  striped = TRUE,
                                  hover = TRUE,
                                  width = "100%",
                                  # suppress column headers
                                  colnames = FALSE,
                                  na="")

output$modelled_catastrophic_costs_source <- renderText({

  # make sure there are data to display
  req(pdata()$profile_estimates)

  # Provide a reference to the modelling paper if estimates are used
  ifelse(is.na(pdata()$profile_estimates[, "catast_pct"]) & !is.na(pdata()$profile_estimates[, "catast_model_pct"]),
         HTML(paste("*<i>",
                    html_link(pdata()$profile_estimates[, "source_catast_costs"]),
                    "</i>")
         ),
         "")

  })

