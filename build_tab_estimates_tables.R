
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the first tab (estimates table)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$estimates_heading <- renderText({ HTML(paste0(ltxt(plabs(), "estimates"), "*, ", dcyear-1)) })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Incidence and mortality estimates table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


estimates_table_content <- reactive({

  # build the dataframe manually
  row_head <- c(ltxt(plabs(), "incidence_tb"),
                ltxt(plabs(), "incidence_tbhiv"),
                # next use ** as callout marker for the second footnote
                paste0(ltxt(plabs(), "incidence_rr"), "**"),
                ltxt(plabs(), "mortality_hivneg"),
                ltxt(plabs(), "mortality_hivpos"))


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
# 2. Proportion of cases with RR-TB table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$drestimates_heading <- renderText({ HTML(paste0(ltxt(plabs(), "rrmdr_est_pct"), "*, ", dcyear-1))  })


drestimates_table_content <- reactive({

  # build the dataframe manually
  row_head <- c(ltxt(plabs(), "new"),
                ltxt(plabs(), "ret"))

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
# 3. UHC and social protection table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$uhc_heading <- renderText({ HTML(paste0(ltxt(plabs(), "uhc"), "*"))  })


uhc_table_content <- reactive({

  # build the table manually

  row_head <-  c(paste0(ltxt(plabs(), "tx_coverage"), ", ", dcyear - 1),

                 ifelse(!is.na(pdata()$profile_data[, "catast_survey_year"]),
                         paste0(ltxt(plabs(), "catastrophic_costs"), ", ",
                                pdata()$profile_data[, "catast_survey_year"]),
                         ltxt(plabs(), "catastrophic_costs")),

                  paste0(ltxt(plabs(), "cfr"), ", ", dcyear - 1))

  row_data <- c(# treatment coverage
                format_estimate(pdata()$profile_estimates[, "c_cdr"],
                                pdata()$profile_estimates[, "c_cdr_lo"],
                                pdata()$profile_estimates[, "c_cdr_hi"],
                                style="%"),

                # catastrophic costs -- only a few countries have this data
                # so need first to check whether the data exist
                ifelse(!is.na(pdata()$profile_data[, "catast_pct"]),
                        format_estimate(pdata()$profile_data[, "catast_pct"],
                                        pdata()$profile_data[, "catast_pct_lo"],
                                        pdata()$profile_data[, "catast_pct_hi"],
                                        style="%"),
                       ""
                       ),

                # case fatality ratio -- convert from a ratio to percent
                format_estimate(pdata()$profile_estimates[, "cfr"] * 100,
                                pdata()$profile_estimates[, "cfr_lo"] * 100,
                                pdata()$profile_estimates[, "cfr_hi"] * 100,
                                style="%"))

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

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Add footnotes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$foot_est <- renderText({  HTML(paste("*<i>", ltxt(plabs(), "foot_est"),"</i>")) })
output$foot_dr_defn <- renderText({ HTML(paste("**<i>", ltxt(plabs(), "dr_definition"), "</i>")) })

