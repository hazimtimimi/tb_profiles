# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for TB preventive treatment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$prevtx_heading <- renderText({ paste0(ltxt(plabs(), "prevtx"), ", ", dcyear-1) })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Preventive treatment table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output
output$prevtx_table <- renderTable({


  # build the data frame manually
  data.frame( c(ltxt(plabs(), "prevtx_hiv"),
                ltxt(plabs(), "prevtx_kids")),

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


              format_estimate(pdata()$profile_estimates[, "e_prevtx_kids_pct"],
                              pdata()$profile_estimates[, "e_prevtx_kids_pct_lo"],
                              pdata()$profile_estimates[, "e_prevtx_kids_pct_hi"],
                              style="%")
               )
            ) },


            striped = TRUE,
            hover = TRUE,
            width = "100%",
            colnames = FALSE,
            na="")

