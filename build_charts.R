
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$chartdemo <- renderHighchart({

  highcharts_demo()

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Incidence chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$inc_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)


  df <- pdata()$epi_timeseries  |>
    # restrict to years starting 2010
    filter(year >=2010)

  highchart()  |>

    hc_title(text = "Estimated TB incidence and notifications") |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "rate_100k_yr")),
             min = 0,
             tickAmount = 3,
             # avoid unnecessary vertical space
             endOnTick = FALSE,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    hc_add_series(type = "line",
                  name = "Incidence per 100 000 population",
                  data = df,
                  hcaes(x = year,
                        y = e_inc_100k),

                  lineWidth = 6,
                  color = standard_palette("incidence"),
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = "Uncertainty interval",
                  data = df,
                  hcaes(x = year,
                        low = e_inc_100k_lo,
                        high = e_inc_100k_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = standard_palette("incidence"),
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "line",
                  name = "Cases notified per 100 000 population",
                  data = df,
                  hcaes(x = year,
                        y = c_newinc_100k),

                  color = "#555555",
                  marker = list(enabled = FALSE))


})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Mortality chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$mort_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  df <- pdata()$epi_timeseries  |>
    # restrict to years starting 2010
    filter(year >=2010)

  highchart()  |>

    hc_title(text = "Estimated HIV-negative TB mortality") |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "rate_100k_yr")),
             min = 0,
             tickAmount = 3,
             # avoid unnecessary vertical space
             endOnTick = FALSE,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    hc_add_series(type = "line",
                  name = "Estimated HIV-negative TB mortality per 100 000 population",
                  data = df,
                  hcaes(x = year,
                        y = e_mort_exc_tbhiv_100k),

                  lineWidth = 6,
                  color = standard_palette("mortality_exc_tbhiv"),
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = "Uncertainty interval",
                  data = df,
                  hcaes(x = year,
                        low = e_mort_exc_tbhiv_100k_lo,
                        high = e_mort_exc_tbhiv_100k_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = standard_palette("mortality_exc_tbhiv"),
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE))

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Cases attributable to 5 risk factors chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$rf_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(is.data.frame(pdata()$attributable_cases))

  df <- pdata()$attributable_cases |>

    # Get the labels for each of the five risk factor categories
    right_join(filter(plabs(), label_tag %in% c("alc", "dia", "hiv", "smk", "und") ),
               by = c("risk_factor"="label_tag")) |>

    # Order by descending number of attributable cases
    arrange(desc(best)) |>
    # Preserve the order by setting risk_factor as a factor variable
    mutate(label_text = factor(label_text, levels = rev(label_text)))


  highchart()  |>

    hc_title(text = paste0(ltxt(plabs(), "attrib_cases"), ", ", dcyear-1)) |>

    hc_chart(inverted = TRUE) |>

    hc_xAxis(title = list(text = ""),
             categories = df$label_text) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "number")),
             min = 0,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    # Stop line appearing between dots on hover
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            enabled = FALSE)))) |>

    # Drop the legend
    hc_legend(enabled = FALSE) |>

    hc_add_series(type = "line",
                  name = "Best",
                  data = df,
                  hcaes(y = best),

                  lineWidth = 0,
                  color = "#91A93E") |>

    hc_add_series(type = "errorbar",
                  name = "Uncertainty intervals",
                  data = df,
                  hcaes(low = lo,
                        high = hi),

                  linkedTo = ":previous",
                  color = "#91A93E",
                  whiskerLength = 15,
                  whiskerWidth = 4)


})
