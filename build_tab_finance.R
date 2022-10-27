# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Add callout to footnote if finance is an aggregate group
output$finance_heading <- renderText({ ifelse(check_entity_type(input$entity_type) == "group",
                                              paste0(ltxt(plabs(), "finance"),"***"),
                                              ltxt(plabs(), "finance")
                                              ) })


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Budget table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output
output$budget_table <- renderTable({

  # Make sure there are data to display
  req(pdata()$profile_properties)

  # build the data frame manually
  # There are two versions depending on whether dc_finance_display is true or false
  if (isTRUE(pdata()$profile_properties[, "dc_finance_display"])){

        data.frame(c(paste0(ltxt(plabs(), "ntp_budget"), ", ", dcyear, " ", ltxt(plabs(), "usd_millions")),
                     paste0("- ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_domestic")),
                     paste0("- ", ltxt(plabs(), "funding_source"), ", ", ltxt(plabs(), "source_international")),
                     paste0("- ", ltxt(plabs(), "source_unfunded"))),

                   c(rounder(pdata()$profile_data[, "tot_req"]),

                    # calculate pct_domestic, international and unfunded
                    display_cap_pct(pdata()$profile_data[, "tot_domestic"],
                                    pdata()$profile_data[, "tot_req"]),

                    display_cap_pct(pdata()$profile_data[, "tot_international"],
                                    pdata()$profile_data[, "tot_req"]),

                    display_cap_pct(pdata()$profile_data[, "tot_gap"],
                                    pdata()$profile_data[, "tot_req"]))
                  )

  } else {

        data.frame(paste0(ltxt(plabs(), "ntp_budget"), ", ", dcyear, " ", ltxt(plabs(), "usd_millions")),
                   rounder(pdata()$profile_data[, "tot_req"])
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
# 2. Budget chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Move heading and subheading out of ggplot2
# because ggplot2 headings don't wrap when space is restricted

# Add callout to footnote if finance is an aggregate group
output$budget_chart_head <- renderText({ ifelse(check_entity_type(input$entity_type) == "group",
                                                paste0(ltxt(plabs(), "budget"),"***"),
                                                ltxt(plabs(), "budget")
                                                ) })


output$budget_chart_subhead <- renderText({ ltxt(plabs(), "usd_millions") })

output$budget_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$profile_finance)

  # Additional check for aggregates -- variable names are different
  req(check_entity_type(input$entity_type) != "group")

  # First make sure there are some data to display
  # There will only be the year column if no data, so check number of columns

  ndata_cols <- ncol(pdata()$profile_finance) - 1

  # Only plot the data if have at least one year with data

  if (ndata_cols > 0){

      plotobj <- pdata()$profile_finance %>%

        # Convert to long format
        pivot_longer(cols = -year,
                     names_to = "budget",
                     values_to = "b_tot",
                     # drop empty values
                     values_drop_na = TRUE) %>%

        ggplot(aes(x=year, y=b_tot, fill = budget)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        profile_theme()  +
        scale_fill_manual("",
                          values = budget_palette(),
                          labels = c("a_domestic" = ltxt(plabs(), "source_domestic"),
                                     "b_international" = ltxt(plabs(), "source_international"),
                                     "c_gap" = ltxt(plabs(), "source_unfunded"))) +
        scale_x_continuous(name="", seq(dcyear-4, dcyear))

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


