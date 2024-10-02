
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

output$population_source <- renderText({ HTML(paste("<i>", "|estimated by the UN Population Division|","</i>")) })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Incidence and mortality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$estimates_heading <- renderText({ ltxt(plabs(), "estimates") })

output$estimates_source <- renderText({  HTML(paste("<i>", ltxt(plabs(), "foot_est"),"</i>")) })


estimates_table_content <- reactive({

  # make sure there are data to display
  req(pdata()$profile_estimates)


  # build the dataframe manually
  row_head <- c(paste0(ltxt(plabs(), "incidence_tb"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "incidence_tbhiv"), ", ", dcyear-1),
                # next use ** as callout marker for the second footnote
                #paste0(ltxt(plabs(), "incidence_rr"), "**"),
                paste0("|Rifampicin-resistant TB (MDR/RR-TB) incidence|", ", ", dcyear-1),
                paste0(ltxt(plabs(), "mortality_hivneg"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "mortality_hivpos"), ", ", dcyear-1))


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
  names(df) <- c("", ltxt(plabs(), "number"),  ltxt(plabs(), "rate_100k"))
  return(df)
})

output$estimates_table <- renderTable({ estimates_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      na="")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Proportion of cases with rifampicin resistance
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$drestimates_heading <- renderText({ #HTML(paste0(ltxt(plabs(), "rrmdr_est_pct"), "*, ", dcyear-1))  })
  "|Estimated proportion of pulmonary bacteriologially confirmed TB cases resistant to rifampicin|" })


drestimates_table_content <- reactive({

  # make sure there are data to display
  req(pdata()$profile_estimates)

  # build the dataframe manually
  row_head <- c(paste0(ltxt(plabs(), "new"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "ret"), ", ", dcyear-1))

  est_pct <- c( format_estimate(pdata()$profile_estimates[, "e_rr_pct_new"],
                                pdata()$profile_estimates[, "e_rr_pct_new_lo"],
                                pdata()$profile_estimates[, "e_rr_pct_new_hi"],
                                style="%"),


                format_estimate(pdata()$profile_estimates[, "e_rr_pct_ret"],
                                pdata()$profile_estimates[, "e_rr_pct_ret_lo"],
                                pdata()$profile_estimates[, "e_rr_pct_ret_hi"],
                                style="%")
  )
  df <- data.frame(row_head, est_pct)

  return(df)

})

output$drestimates_table <- renderTable({ drestimates_table_content() },
                                        striped = TRUE,
                                        hover = TRUE,
                                        width = "100%",
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
  req(pdata()$profile_data)

  # build the table manually
  row_head <-  c(paste0(ltxt(plabs(), "tx_coverage"), ", ", dcyear - 1),

                 paste0(ltxt(plabs(), "cfr"), ", ", dcyear - 1),

                 # catastrophic costs: check whether the data exist and whether they
                 # are from a survey or from modelled estimates
                 case_when(
                   !is.na(pdata()$profile_data[, "catast_pct"]) ~ paste0(ltxt(plabs(), "catastrophic_costs"),
                                                                                 " (|national_survey|), ",
                                                                                 pdata()$profile_data[, "catast_survey_year"]
                                                                                 ),

                   !is.na(pdata()$profile_data[, "catast_model_pct"]) ~ paste0(ltxt(plabs(), "catastrophic_costs"),
                                                                                " (|modelled estimate*|), ",
                                                                                pdata()$profile_data[, "catast_model_year"]
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
                  !is.na(pdata()$profile_data[, "catast_pct"]) ~ format_estimate(pdata()$profile_data[, "catast_pct"],
                                                                                 pdata()$profile_data[, "catast_pct_lo"],
                                                                                 pdata()$profile_data[, "catast_pct_hi"],
                                                                                 style="%"),

                  !is.na(pdata()$profile_data[, "catast_model_pct"]) ~ format_estimate(pdata()$profile_data[, "catast_model_pct"],
                                                                                       pdata()$profile_data[, "catast_model_pct_lo"],
                                                                                       pdata()$profile_data[, "catast_model_pct_hi"],
                                                                                       style="%"),

                  .default = ""
                ))

  df <- data.frame(row_head, row_data)

  # Remove the second row on catastrophic costs if the selected entity is a group
  if (check_entity_type(input$entity_type) == "group") {
    df <- df[-2,]
  }

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
  req(pdata()$profile_data)


  ifelse(is.na(pdata()$profile_data[, "catast_pct"]) & !is.na(pdata()$profile_data[, "catast_model_pct"]),
         HTML(paste("*<i>",
                    html_link(pdata()$profile_data[, "source_catast_costs"]),
                    "</i>")
         ),
         "")

  })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Add footnotes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$foot_dr_defn <- renderText({ HTML(paste("**<i>", ltxt(plabs(), "dr_definition"), "</i>")) })

