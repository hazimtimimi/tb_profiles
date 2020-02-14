
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the third tab (notifications tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$heading_notifs <- renderText({ paste0(ltxt(plabs(), "notifs"), ", ", dcyear-1)  })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Main notifications table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

notifs_data <- reactive({


  # Calculate total numner of pulmonary new and relapse cases
  pulmonary <- pdata()$profile_data[, "new_labconf"] + pdata()$profile_data[, "new_clindx"] +
               pdata()$profile_data[, "ret_rel_labconf"] + pdata()$profile_data[, "ret_rel_clindx"]

  # calculate percent of men, women and children in new and relapse
  # in 2019 instead of c_newinc used c_tot_agesex as the denominator,
  # which is the total number of cases reported in the age/sex table.
  #
  pct_women <- display_cap_pct(pdata()$profile_data[, "c_newrel_women"],
                               pdata()$profile_data[, "c_tot_agesex"])

  pct_men <- display_cap_pct(pdata()$profile_data[, "c_newrel_men"],
                             pdata()$profile_data[, "c_tot_agesex"])

  # to avoid weirdness of percentages not adding up to 100, will calculate %children as 100 - %men - %women to avoid rounding errors.
	# Not entirely satisfactory but avoid people questioning why rounded percentages don't add up to 100%

  pct_014 <- ifelse((pct_women == "" & pct_men == ""),
                    "",
                    100 -
                    get_cap_pct(pdata()$profile_data[, "c_newrel_women"], pdata()$profile_data[, "c_tot_agesex"]) -
                    get_cap_pct(pdata()$profile_data[, "c_newrel_men"], pdata()$profile_data[, "c_tot_agesex"]) )

  # now format pct_014 as a string
  pct_014 <- ifelse((pct_014 == 0 & pdata()$profile_data[, "c_newrel_014"] > 0),
                    # show this to avoid a false 0%
                    "<1%",
                    paste0(signif(pct_014, 2), "%"))



  data_column <- c(rounder(pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_rdx
                   display_cap_pct(pdata()$profile_data[, "newinc_rdx"],      # !!!! TBD: adjust based on rdx_data_available
                                   pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_hivtest
                   display_cap_pct(pdata()$profile_data[, "newrel_hivtest"],
                                   pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_pulmonary
                   display_cap_pct(pulmonary,
                                   pdata()$profile_data[, "c_newinc"]),

                   # calculate pct_pulm_bacconf
                   display_cap_pct((pdata()$profile_data[, "new_labconf"] + pdata()$profile_data[, "ret_rel_labconf"]),
                                   pulmonary),

                   pct_014,

                   pct_women,

                   pct_men,

                   rounder(pdata()$profile_data[, "c_notified"])

                  )
  return(data_column)

})


# Combine the data with the row headers manually and render for output
output$notifs_table <- renderTable({ data.frame(c( ltxt(plabs(), "tot_newrel"),
                                                paste(" - ",ltxt(plabs(), "pct_rdx")),
                                                paste(" - ",ltxt(plabs(), "pct_hivtest")),
                                                paste(" - ",ltxt(plabs(), "pct_pulmonary")),
                                                paste(" - ",ltxt(plabs(), "pct_pulm_bacconf")),
                                                paste(" - ",ltxt(plabs(), "pct_children")),
                                                paste(" - ",ltxt(plabs(), "pct_women")),
                                                paste(" - ",ltxt(plabs(), "pct_men")),
                                                ltxt(plabs(), "tot_notified") ),
                                                notifs_data()
                                                )  },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      # suppress column headers
                                      colnames = FALSE,
                                      na="")

