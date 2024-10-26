
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the country-reported surveillance data tables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$notifs_heading <- renderText({ ltxt(plabs(), "notifs") })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Main notifications ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notifs_data <- reactive({

  # make sure there are data to display
  req(pdata()$profile_data)

  # Calculate total number of pulmonary new and relapse cases
  # Convert any missing values to zero
  pulmonary <- NZ(pdata()$profile_data[, "new_labconf"]) + NZ(pdata()$profile_data[, "new_clindx"]) +
               NZ(pdata()$profile_data[, "ret_rel_labconf"]) + NZ(pdata()$profile_data[, "ret_rel_clindx"])

  # calculate percent of men, women and children in new and relapse
  # in 2019 instead of c_newinc used c_tot_agesex as the denominator,
  # which is the total number of cases reported in the age/sex table.
  # Use NZ() with the numerator t0 0% when there are no men or no women when
  # numbers hadn't all been filled in and when denominator > 0

  pct_women <- display_cap_pct(NZ(pdata()$profile_data[, "c_newrel_women"]),
                               pdata()$profile_data[, "c_tot_agesex"])

  pct_men <- display_cap_pct(NZ(pdata()$profile_data[, "c_newrel_men"]),
                             pdata()$profile_data[, "c_tot_agesex"])

  # to avoid weirdness of percentages not adding up to 100, will calculate %children as 100 - %men - %women to avoid rounding errors.
	# Not entirely satisfactory but avoid people questioning why rounded percentages don't add up to 100%
  # Use NZ() with the numerator to avoid errors when there are no men or no women

  pct_014 <- ifelse((pct_women == "" & pct_men == ""),
                    "",
                    100 -
                    get_cap_pct(NZ(pdata()$profile_data[, "c_newrel_women"]), pdata()$profile_data[, "c_tot_agesex"]) -
                    get_cap_pct(NZ(pdata()$profile_data[, "c_newrel_men"]), pdata()$profile_data[, "c_tot_agesex"]) )

  if (pct_014 != "") {

    # now format pct_014 as a string
    pct_014 <- ifelse((pct_014 == 0 & NZ(pdata()$profile_data[, "c_newrel_014"]) > 0),
                      # show this to avoid a false 0%
                      "<1%",
                      paste0(signif(pct_014, 2), "%"))
  }


  data_column <- c(rounder(pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_rdx
                   display_cap_pct(pdata()$profile_data[, "newinc_rdx"],
                                   pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_hivtest
                   if (check_entity_type(input$entity_type) == "group") {
                     # For groups need to use the aggregated data view results
                     display_cap_pct(pdata()$profile_data[, "hivtest_pct_numerator"],
                                     pdata()$profile_data[, "hivtest_pct_denominator"])

                   } else {
                     # default is country data
                     display_cap_pct(pdata()$profile_data[, "newrel_hivtest"],
                                     pdata()$profile_data[, "c_newinc"])

                   },

                   # calculate pct_pulmonary
                   display_cap_pct(pulmonary,
                                   pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_pulm_bacconf
                   display_cap_pct((pdata()$profile_data[, "new_labconf"] + pdata()$profile_data[, "ret_rel_labconf"]),
                                   pulmonary),

                   pct_women,

                   pct_men,

                   pct_014,

                   rounder(pdata()$profile_data[, "c_notified"])

                  )
  return(data_column)

})


# Combine the data with the row headers manually and render for output
output$notifs_table <- renderTable({ data.frame(c( paste0(ltxt(plabs(), "c_newinc"), ", ", dcyear - 1),
                                                paste0("      — ",ltxt(plabs(), "pct_rdx")),
                                                paste0("      — ",ltxt(plabs(), "pct_hivtest")),
                                                paste0("      — ",ltxt(plabs(), "pct_pulmonary")),
                                                paste0("      — ",ltxt(plabs(), "pct_pulm_bacconf")),
                                                paste0("      — ", str_replace_all(ltxt(plabs(), "pct_women"), "[()]", "")),
                                                paste0("      — ", str_replace_all(ltxt(plabs(), "pct_men"), "[()]", "")),
                                                paste0("      — ",ltxt(plabs(), "pct_children")),
                                                paste0(ltxt(plabs(), "tot_notified"), ", ", dcyear - 1) ),
                                                notifs_data()
                                                )  },
                                   striped = TRUE,
                                   hover = TRUE,
                                   width = "100%",
                                   # right-align the data column
                                   align = "lr",
                                   # suppress column headers
                                   colnames = FALSE,
                                   na="",
                                   # Use this xtable print option to prevent the HTML &ge;
                                   # character entity from being displayed as "&ge;"
                                   # (the character may be needed in some label strings intended for HTML output)
                                   sanitize.text.function=function(x){x})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# TB/HIV ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$tbhiv_heading <- renderText({ ltxt(plabs(), "newrel_tbhiv_care") })


tbhiv_data <- reactive({

  # make sure there are data to display
  req(pdata()$profile_data)

  # numbers
  num_data <- c(rounder(pdata()$profile_data[, "newrel_hivpos"]),
                rounder(pdata()$profile_data[, "newrel_art"]))

  # percentages
  if (check_entity_type(input$entity_type) == "group") {
     # For groups need to use the aggregated data view results
      pct_data <- c( # calculate pct_hivtestpositive
                     display_cap_pct(pdata()$profile_data[, "hivtest_pos_pct_numerator"],
                                     pdata()$profile_data[, "hivtest_pos_pct_denominator"]),

                     # calculate pct_art
                     display_cap_pct(pdata()$profile_data[, "hiv_art_pct_numerator"],
                                     pdata()$profile_data[, "hiv_art_pct_denominator"]) )

    } else {
      # default is country data
      pct_data <- c( # calculate pct_hivtestpositive
                     display_cap_pct(pdata()$profile_data[, "newrel_hivpos"],
                                     pdata()$profile_data[, "newrel_hivtest"]),

                     # calculate pct_art
                     display_cap_pct(pdata()$profile_data[, "newrel_art"],
                                     pdata()$profile_data[, "newrel_hivpos"]) )

  }


  df <- data.frame(c(paste0(ltxt(plabs(), "hivtest_pos"), ", ", dcyear-1),
                     paste0(ltxt(plabs(), "art"), ", ", dcyear-1)),
                   num_data, pct_data)

  # add the column names
  names(df) <- c("", ltxt(plabs(), "number"), "(%)")
  return(df)

})

# Combine the data with the row headers manually and render for output
output$tbhiv_table <- renderTable({ tbhiv_data()  },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      # right-align the data columns
                                      align = "lrr",
                                      na="")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DR-TB ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$drtb_heading <- renderText({ ltxt(plabs(), "drtb_care") })

drtb_data <- reactive({

  # make sure there are data to display
  req(pdata()$profile_data)

  c(# calculate dst_pct for new pulmonary bac-confirmed cases
    display_cap_pct(pdata()$profile_data[, "r_rlt_new"],
                    pdata()$profile_data[, "pulm_labconf_new"]),

    # calculate dst_pct for previously treated pulmonary bac-confirmed cases
    display_cap_pct(pdata()$profile_data[, "r_rlt_ret"],
                    pdata()$profile_data[, "pulm_labconf_ret"]),

    # Calculate pct rr-tb tested for FQ susceptibility
    display_cap_pct(pdata()$profile_data[, "rr_dst_rlt_fq"],
                    pdata()$profile_data[, "rr"]),

    rounder(pdata()$profile_data[, "conf_rr_nfqr"]),
    rounder(pdata()$profile_data[, "conf_rr_nfqr_tx"]),
    rounder(ifelse(is.na(pdata()$profile_data[, "unconf_rr_nfqr_tx"]) & is.na(pdata()$profile_data[, "conf_rr_nfqr_tx"]),
                   NA,
                   NZ(pdata()$profile_data[, "unconf_rr_nfqr_tx"]) + NZ(pdata()$profile_data[, "conf_rr_nfqr_tx"]))),
    rounder(pdata()$profile_data[, "conf_rr_fqr"]),
    rounder(pdata()$profile_data[, "conf_rr_fqr_tx"])

  )


})

# Callouts to footnotes in the table using plain characters as I couldn't use HTML superscript
# entities, and unicode characters for superscript 4 and 5 didn;t work when testing


output$drtb_table <- renderTable({

  data.frame(
    c(paste0(ltxt(plabs(), "r_rlt_pct_new"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "r_rlt_pct_ret"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "rr_dst_rlt_fq_pct"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "rr_nfqr"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "conf_rr_nfqr_tx_v2"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "tot_rr_nfqr_tx"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "rr_fqr"), ", ", dcyear-1),
    paste0(ltxt(plabs(), "conf_rr_fqr_tx"), ", ", dcyear-1)
                                                 ),
                                               drtb_data()
                                              )  },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      # right-align the data column
                                      align = "lr",
                                      # suppress column headers
                                      colnames = FALSE,
                                      na="")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Treatment success rate and cohort size ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$outcomes_heading <- renderText({ ltxt(plabs(), "tsr_cohort") })


# Combine the data with the row headers manually and render for output

outcomes_table_content <- reactive({

  # don't do anything until country profile data are available
  req(pdata()$profile_data)

  # build the data frame manually
  df <- data.frame(

    c(#labels depend on whether or not the country includes relapses with new
      ifelse(NZ(pdata()$profile_data[, "rel_with_new_flg"]) == 1,
             paste(ltxt(plabs(), "tsr_newrel"), dcyear - 2),
             paste(ltxt(plabs(), "tsr_new"), dcyear - 2)
      ),

      ifelse(NZ(pdata()$profile_data[, "rel_with_new_flg"]) == 1,
             paste(ltxt(plabs(), "tsr_ret_nrel"), dcyear - 2),
             paste(ltxt(plabs(), "tsr_allret"), dcyear - 2)
      ),

      paste(ltxt(plabs(), "tsr_tbhiv"), dcyear - 2),
      paste(ltxt(plabs(), "tsr_mdr"), dcyear - 3),
      paste(ltxt(plabs(), "tsr_xdr"), dcyear - 3)
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



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# TB preventive treatment ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$prevtx_heading <- renderText({ ltxt(plabs(), "prevtx") })

# Combine the data with the row headers manually and render for output
output$prevtx_table <- renderTable({


  # build the data frame manually
  data.frame( c(paste0(ltxt(plabs(), "prevtx_hiv"), ", ", dcyear-1),
                paste0(ltxt(plabs(), "prevtx_con"), ", ", dcyear-1)
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

