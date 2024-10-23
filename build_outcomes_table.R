# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$outcomes_heading <- renderText({ ltxt(plabs(), "tsr_cohort") })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. treatment success rate table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output

outcomes_table_content <- reactive({

  # don't do anything until country profile data are available
  req(pdata()$profile_data)

  # build the data frame manually
  df <- data.frame(

            c(#labels depend on whether or not the country includes relapses with new
               ifelse(NZ(pdata()$profile_data[, "rel_with_new_flg"]) == 1,
                      paste("|People with a new or relapse case of TB started on treatment for TB,|", dcyear - 2),
                      #paste(ltxt(plabs(), "tsr_newrel"), dcyear - 2),
                      paste("|People with a new case of TB started on treatment for TB in|", dcyear - 2)
                      #paste(ltxt(plabs(), "tsr_new"), dcyear - 2)
                      ),

               ifelse(NZ(pdata()$profile_data[, "rel_with_new_flg"]) == 1,
                      paste("|People with a previously treated case of TB (but not a relapse case) started on treatment for TB,|", dcyear - 2),
                      #paste(ltxt(plabs(), "tsr_ret_nrel"), dcyear - 2),
                      paste("|People with a previously treated case of TB started on treatment for TB,|", dcyear - 2)
                      #paste(ltxt(plabs(), "tsr_allret"), dcyear - 2)
                      ),

               paste("|People with a new or relapse case of TB who are living with HIV started on treatment for TB,|", dcyear - 2),
               paste("|People started on treatment for TB that is resistant to rifampicin (RR/MDR-TB),|", dcyear - 2),
               paste("|People started on treatment for TB that is resistant to both rifampicin and fluoroquinolones (pre-XDR-TB/XDR-TB),|", dcyear - 2)
               #paste(ltxt(plabs(), "tsr_tbhiv"), dcyear - 2),
               #paste(ltxt(plabs(), "tsr_mdr"), dcyear - 3),
               #paste(ltxt(plabs(), "tsr_xdr"), dcyear - 3)
               ),

             # treatment success rates
             c(rounder_pct(pdata()$profile_data[, "c_new_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_ret_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_tbhiv_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_mdr_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_xdr_tsr"])),

             # cohort size
             c(rounder(pdata()$profile_data[, "newrel_coh"]),
               rounder(pdata()$profile_data[, "ret_nrel_coh"]),
               rounder(pdata()$profile_data[, "tbhiv_coh"]),
               rounder(pdata()$profile_data[, "mdr_coh"]),
               rounder(pdata()$profile_data[, "xdr_coh"]))
            )

  # add the column names
  names(df) <- c("", ltxt(plabs(), "success"),  ltxt(plabs(), "cohort"))
  return(df)

})


output$outcomes_table <- renderTable({ outcomes_table_content() },


            striped = TRUE,
            hover = TRUE,
            width = "100%",
            # right-align the cohort column
            align = "llr",
            na="")

