# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Add call out to footnote if finance is an aggregate group
output$finance_heading <- renderText({ ltxt(plabs(), "funding") })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Expenditure (received funding)
#    and, if this is a country-level profile, the budget (committed funding) table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine the data with the row headers manually and render for output
output$funding_table <- renderTable({

  # Make sure there are data to display
  req(pdata()$profile_properties)

  funding_sum <- sum_with_nulls(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"],
                                pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])

  # build the data frame manually, but only if dc_finance_display is true
  # 1. Country-level version showing expenditure and budget
  if (isTRUE(pdata()$profile_properties[, "dc_finance_display"]) & check_entity_type(input$entity_type) != "group"){

    data.frame(c(paste0("|Funding available for TB prevention, diagnostic and treatment services|", ", ", dcyear-1),
                 #paste0(ltxt(plabs(), "funding"), ", ", dcyear-1),
                 paste0("      — ", ltxt(plabs(), "fund_domestic")),
                 paste0("      — ", ltxt(plabs(), "fund_international")),
                 paste0(ltxt(plabs(), "ntp_budget"), ", ", dcyear),
                 paste0("      — ", ltxt(plabs(), "fund_domestic")),
                 paste0("      — ", ltxt(plabs(), "fund_international")),
                 #paste0("      — ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_domestic")),
                 #paste0("      — ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_international")),
                 paste0("      — | %", ltxt(plabs(), "source_unfunded"), "|")
                 ),

               c(paste("US$ ", rounder_mil(funding_sum), "|million|"),

                 # calculate pct_domestic and international for expenditure (received funding)
                 display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]),
                                 funding_sum),

                 display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                                 funding_sum),

                 paste("US$ ", rounder_mil(pdata()$profile_data[, "tot_req"]), "|million|"),

                 # calculate pct_domestic, international and unfunded for budget (committed funding)
                 display_cap_pct(pdata()$profile_data[, "tot_domestic"],
                                 pdata()$profile_data[, "tot_req"]),

                 display_cap_pct(pdata()$profile_data[, "tot_international"],
                                 pdata()$profile_data[, "tot_req"]),

                 display_cap_pct(pdata()$profile_data[, "tot_gap"],
                                 pdata()$profile_data[, "tot_req"])

                 )
    )

  }
  # 1. Aggregate version showing expenditure only
  else if (isTRUE(pdata()$profile_properties[, "dc_finance_display"]) & check_entity_type(input$entity_type) == "group") {


    data.frame(c(paste0(ltxt(plabs(), "funding"), ", ", dcyear-1, " ", ltxt(plabs(), "usd_millions")),
                 paste0("      — ", ltxt(plabs(), "fund_domestic")),
                 paste0("      — ", ltxt(plabs(), "fund_international"))
    ),

    c(rounder(funding_sum),

      # calculate pct_domestic and international for expenditure (received funding)
      display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]),
                      funding_sum),

      display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                      funding_sum)

    )
    )

  }

},

striped = TRUE,
hover = TRUE,
width = "100%",
# right-align the data column
align = "lr",
# suppress column headers
colnames = FALSE,
na="")



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Add explanation of what finance numbers include and exclude
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$finance_inclusions_exclusions <- renderText({

  ifelse(check_entity_type(input$entity_type) == "group",
         HTML(paste("<i>",
                    ltxt(plabs(), "foot_aggfin_lmc"),
                    ltxt(plabs(), "foot_aggfin_ghs"),
                    "</i>")),
         HTML(paste("<i>",
                    "|Excludes funding for inpatient and outpatient care.|",
                    "</i>"))
  )

})


