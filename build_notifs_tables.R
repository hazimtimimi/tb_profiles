
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the third tab (notifications tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$notifs_heading <- renderText({ ltxt(plabs(), "notifs") })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Main notifications ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notifs_data <- reactive({

  # make sure there are data to display
  req(pdata()$profile_data)

  # Calculate total numner of pulmonary new and relapse cases
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
output$notifs_table <- renderTable({ data.frame(c( paste0("|New and relapse TB cases|", ", ", dcyear - 1),  #ltxt(plabs(), "tot_newrel"),
                                                paste0("      — ",ltxt(plabs(), "pct_rdx")),
                                                paste0("      — ",ltxt(plabs(), "pct_hivtest")),
                                                paste0("      — ",ltxt(plabs(), "pct_pulmonary")),
                                                paste0("      — |% of pulmonary cases that are bacteriologically confirmed|"),
                                                paste0("      — ", str_replace_all(ltxt(plabs(), "pct_women"), "[()]", "")),
                                                paste0("      — ", str_replace_all(ltxt(plabs(), "pct_men"), "[()]", "")),
                                                paste0("      — |% people aged 0-14 years|"),
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

output$tbhiv_heading <- renderText({ "|TB/HIV care|" }) #paste0(ltxt(plabs(), "newrel_tbhiv_care"), ", ", dcyear-1)  })


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


  df <- data.frame(c(paste0("|New and relapse TB patients with known HIV status who are HIV-positive|", ", ", dcyear-1),  #ltxt(plabs(), "hivtest_pos"),
                     paste0("|New and relapse HIV-positive TB patients on antiretroviral therapy|", ", ", dcyear-1)), #ltxt(plabs(), "art")) ),
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

    ifelse(dcyear == 2019,
           rounder(pdata()$profile_data[, "rr_sldst"]),
           # Change the variable displayed from 2020 data collection year onwards
           rounder(pdata()$profile_data[, "rr_dst_rlt_fq"])),


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
    c(paste0("|% of new bacteriologically confirmed pulmonary TB cases tested for rifampicin resistance|", ", ", dcyear-1),
    paste0("|% of previously treated bacteriologically confirmed pulmonary TB cases tested for rifampicin resistance|", ", ", dcyear-1),
    paste0("|Laboratory-confirmed TB cases with rifampicin resistance tested for resistance to fluoroquinolones (would this be better as a % and moved to the third row?)|", ", ", dcyear-1),
    paste0("|Laboratory-confirmed TB cases with rifampicin resistance and no known fluoroquinolone resistance|", ", ", dcyear-1),
    paste0("|Laboratory-confirmed TB cases with rifampicin resistance and no known fluoroquinolone resistance started on treatment|", ", ", dcyear-1),
    paste0("|Total number of TB cases with rifampicin resistance and no known fluoroquinolone resistance started on treatment|", ", ", dcyear-1),
    paste0("|Laboratory-confirmed TB cases with rifampicin resistance and fluoroquinolone resistance (pre-XDR-TB or XDR-TB)|", ", ", dcyear-1),
    paste0("|Laboratory-confirmed TB cases with rifampicin resistance and fluoroquinolone resistance started on treatment|", ", ", dcyear-1)
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


