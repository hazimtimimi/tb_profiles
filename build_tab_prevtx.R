# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
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

              # format the estimates
              c(display_cap_pct(pdata()$profile_data[, "hiv_ipt"],
                                pdata()$profile_data[, "hiv_reg_new"]),

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

