# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Add call out to footnote if finance is an aggregate group
output$finance_heading <- renderText({ ifelse(check_entity_type(input$entity_type) == "group",
                                              paste0(ltxt(plabs(), "funding"),"***"),
                                              ltxt(plabs(), "funding")
                                              ) })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Expenditure (received funding)
#    and, if this is a country-level profile, the budget (committed funding) table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Combine the data with the row headers manually and render for output
output$funding_table <- renderTable({

  # Make sure there are data to display
  req(pdata()$profile_properties)

  # build the data frame manually, but only if dc_finance_display is true
  # 1. Country-level version showing expenditure and budget
  if (isTRUE(pdata()$profile_properties[, "dc_finance_display"]) & check_entity_type(input$entity_type) != "group"){

    data.frame(c(paste0(ltxt(plabs(), "funding"), ", ", dcyear-1, " ", ltxt(plabs(), "usd_millions")),
                 paste0("- ", ltxt(plabs(), "fund_domestic")),
                 paste0("- ", ltxt(plabs(), "fund_international")),
                 paste0(ltxt(plabs(), "ntp_budget"), ", ", dcyear, " ", ltxt(plabs(), "usd_millions")),
                 paste0("- ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_domestic")),
                 paste0("- ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_international")),
                 paste0("- ", ltxt(plabs(), "source_unfunded"))
                 ),

               c(rounder(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                           NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])),

                 # calculate pct_domestic and international for expenditure (received funding)
                 display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]),
                                 NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                                   NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])),

                 display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                                 NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                                   NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])),

                 rounder(pdata()$profile_data[, "tot_req"]),

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
                 paste0("- ", ltxt(plabs(), "fund_domestic")),
                 paste0("- ", ltxt(plabs(), "fund_international"))
    ),

    c(rounder(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])),

      # calculate pct_domestic and international for expenditure (received funding)
      display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]),
                      NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                        NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"])),

      display_cap_pct(NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]),
                      NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "a_domestic_funds"]) +
                        NZ(pdata()$funding_timeseries[pdata()$funding_timeseries$year == dcyear-1, "b_international_funds"]))

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
# 2. Expenditure (committed funding) chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Move heading and subheading out of ggplot2
# because ggplot2 headings don't wrap when space is restricted

# Add callout to footnote if finance is an aggregate group
output$funding_chart_head <- renderText({ ifelse(check_entity_type(input$entity_type) == "group",
                                                paste0(ltxt(plabs(), "funding"),"***"),
                                                ltxt(plabs(), "funding")
                                                ) })


output$funding_chart_subhead <- renderText({ ltxt(plabs(), "usd_millions") })


output$funding_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$funding_timeseries)

  # First make sure there are some data to display
  # There will only be the year column if no data, so check number of columns

  ndata_cols <- ncol(pdata()$funding_timeseries) - 1

  # Only plot the data if have at least one year with data

  if (ndata_cols > 0){

    plotobj <- pdata()$funding_timeseries %>%

      # Convert to long format
      pivot_longer(cols = -year,
                   names_to = "funding",
                   values_to = "fund_amount",
                   # drop empty values
                   values_drop_na = TRUE) %>%

      ggplot(aes(x=year, y=fund_amount, fill = funding)) +
      geom_col(position = position_stack(reverse = TRUE)) +
      profile_theme()  +
      scale_fill_manual("",
                        values = funding_palette(),
                        labels = c("a_domestic_funds" = ltxt(plabs(), "source_domestic"),
                                   "b_international_funds" = ltxt(plabs(), "source_international"))) +
      scale_x_continuous(name="", seq(dcyear-5, dcyear-1))

  } else {

    plotobj <- NA
  }

  return(plotobj)

})



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Add footnote for aggregated finance
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$foot_aggfin <- renderText({  ifelse(check_entity_type(input$entity_type) == "group",
                                           HTML(paste("***<i>",
                                                      ltxt(plabs(), "foot_aggfin_lmc"),
                                                      ltxt(plabs(), "foot_aggfin_ghs"),
                                                      "</i>")),
                                           "") })


