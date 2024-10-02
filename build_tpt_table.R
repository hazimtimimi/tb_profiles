# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for TB preventive treatment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$prevtx_heading <- renderText({ ltxt(plabs(), "prevtx") })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Preventive treatment table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output
output$prevtx_table <- renderTable({


  # build the data frame manually
  data.frame( c(paste0(str_replace_all(ltxt(plabs(), "prevtx_hiv"), "[()]", ""), ", ", dcyear-1),
                paste0("|% of estimated number of household contacts of bacteriologically-confirmed TB cases on preventive treatment|", ", ", dcyear-1)
                ),

              c(
             # calculate pct_hiv_ipt
             if (check_entity_type(input$entity_type) == "group") {
               # For groups need to use the aggregated data view results
               display_cap_pct(pdata()$profile_data[, "hiv_ipt_pct_numerator"],
                               pdata()$profile_data[, "hiv_ipt_pct_denominator"])

             } else {
               # default is country data
               display_cap_pct(pdata()$profile_data[, "hiv_ipt"],
                                  pdata()$profile_data[, "hiv_reg_new"])

             },


              format_estimate(pdata()$profile_estimates[, "e_prevtx_hh_contacts_pct"],
                              pdata()$profile_estimates[, "e_prevtx_hh_contacts_pct_lo"],
                              pdata()$profile_estimates[, "e_prevtx_hh_contacts_pct_hi"],
                              style="%")
               )
            ) },


            striped = TRUE,
            hover = TRUE,
            width = "100%",
            colnames = FALSE,
            na="")

