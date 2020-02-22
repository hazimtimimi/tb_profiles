# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$finance_heading <- renderText({ ltxt(plabs(), "finance") })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Budget table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output
output$budget_table <- renderTable({


  # build the data frame manually
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
            ) },


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


output$budget_chart <-  renderPlot({

  # First make sure there are some data to display
  nyears <- pdata()$profile_finance %>% filter(!is.na(b_tot)) %>% nrow()
  
  # Only plot the data if have at least one year with data
  
  if (nyears > 0){

      plotobj <- pdata()$profile_finance %>%
    
        ggplot(aes(x=year, y=b_tot, fill = budget)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        profile_theme()  +
        scale_fill_manual("",
                          values = budget_palette(),
                          labels = c("a_domestic" = ltxt(plabs(), "source_domestic"),
                                     "b_international" = ltxt(plabs(), "source_international"),
                                     "c_gap" = ltxt(plabs(), "source_unfunded"))) +
    
         ggtitle(ltxt(plabs(), "budget"),
                 subtitle = ltxt(plabs(), "usd_millions") )
  } else {
    
      plotobj <- NA
  }
  
  return(plotobj)
  
})
