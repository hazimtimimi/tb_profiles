# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Budget chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$budget_chart <-  renderPlot({

  #  Added a legend resulting in more complicated code

  pdata()$profile_finance %>%

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
})
