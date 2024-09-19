
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$chartdemo <- renderHighchart({

  highcharts_demo()

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Incidence chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Move heading and subheading out of ggplot2
# because ggplot2 headings don't wrap when space is restricted

output$inc_chart_head <- renderText({ paste0("<span style='color: ",  standard_palette("incidence"), ";'>",
                                             ltxt(plabs(), "inc"),
                                             "</span>, <span style='color: black;'>",
                                             ltxt(plabs(), "notifs_totnewrel"),
                                             "</span>, <span style='color: ",  standard_palette("tbhiv_incidence"), ";'>",
                                             ltxt(plabs(), "incidence_tbhiv"),
                                             "</span>") })
output$inc_chart_subhead <- renderText({ ltxt(plabs(), "rate_100k_yr") })

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




#
#
#
#     # quick fix if TB/HIV estimates are all NAs, make sure
#     # data type is numeric (problem with Andorra etc)
#     mutate(e_inc_tbhiv_100k = as.numeric(e_inc_tbhiv_100k),
#            e_inc_tbhiv_100k_lo = as.numeric(e_inc_tbhiv_100k_lo),
#            e_inc_tbhiv_100k_hi = as.numeric(e_inc_tbhiv_100k_hi)) %>%
#
#     ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
#     geom_line(linewidth=1, aes(colour="notifs")) +
#
#     geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
#                 fill=standard_palette("incidence"),
#                 alpha=0.4) +
#
#     geom_line(aes(y=e_inc_100k, colour="inc"),
#               linewidth=1) +
#
#     geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi),
#                 fill=standard_palette("tbhiv_incidence"),
#                 alpha=0.4) +
#
#     geom_line(aes(y=e_inc_tbhiv_100k, colour="tbhivinc"),
#               linewidth=1,
#               # The next option suppresses warnings about missing values
#               # from appearing in the console
#               na.rm = TRUE) +
#
#     scale_x_continuous(name="", breaks = seq(2010, dcyear-1, by=2)) +
#
#     profile_theme() +
#
#     # Build the legend using a manual colour scale
#     scale_color_manual(name = "",
#                        breaks = c("inc", "notifs", "tbhivinc"),
#                        values = c("inc" = standard_palette("incidence"),
#                                   "notifs" = "black",
#                                   "tbhivinc" = standard_palette("tbhiv_incidence")),
#                        labels = c("inc" = ltxt(plabs(), "incidence_tb"),
#                                   # couldn't get R to interpret \n or <br /> embedded in the label itself so
#                                   # either split labels into two and use "\n" to join them (which does get interpreted!)
#                                   # or don't use legends and instead use colour coding in the title
#                                   "notifs" = ltxt(plabs(), "notifs_totnewrel"),
#                                   "tbhivinc" = ltxt(plabs(), "incidence_tbhiv"))
#     ) +
#
#     # suppress legend
#     theme(legend.position="none")

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Mortality chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Move heading and subheading out of ggplot2
# because ggplot2 headings don't wrap when space is restricted

output$mort_chart_head <- renderText({  paste0("<span style='color: ",  standard_palette("mortality_exc_tbhiv"), ";'>",
                                               ltxt(plabs(), "mortality_hivneg"),
                                               "</span>") })
output$mort_chart_subhead <- renderText({ ltxt(plabs(), "rate_100k_yr") })

output$mort_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  pdata()$epi_timeseries %>%

    # start time series at year 2010
    filter(year >= 2010) %>%

    ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
    geom_line(linewidth=1,
		          colour=standard_palette("mortality_exc_tbhiv")) +

    geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                fill=standard_palette("mortality_exc_tbhiv"),
                alpha=0.4) +

    scale_x_continuous(name="", breaks = seq(2010, dcyear-1, by=2)) +

    profile_theme()

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Cases attributable to 5 risk factors chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$rf_chart_head <- renderText({ paste0(ltxt(plabs(), "attrib_cases"), ", ", dcyear-1) })

output$rf_chart_subhead <- renderText({ paste0("(", ltxt(plabs(), "number"), ")") })

output$rf_chart <-  renderPlot({

  # Make sure there are data to plot
  req(is.data.frame(pdata()$attributable_cases))

  pdata()$attributable_cases %>%

    # Get the labels for each of the five risk factor categories
    right_join(filter(plabs(), label_tag %in% c("alc", "dia", "hiv", "smk", "und") ),
               by = c("risk_factor"="label_tag")) %>%

    # Order by descending number of attributable cases
    arrange(desc(best)) %>%
    # Preserve the order by setting risk_factor as a factor variable
    mutate(label_text = factor(label_text, levels = rev(label_text))) %>%

    # Plot
    ggplot(aes(y=label_text,
               x=best)) +
    geom_point() +
    # Some records have null entries for lo and hi which triggers an error so
    # set them to zero using NZ() because this only happens when best is zero.
    geom_pointrange(aes(xmin=NZ(lo), xmax=NZ(hi)),
                    size=1.2,
                    colour='Darkgreen') +
    expand_limits(x=0) +
    # USe space separators to label large numbers; don't display fractions
    scale_x_continuous(labels = function(x){ifelse(x %% 1 == 0, rounder(x),"")},
                       expand = expansion(mult = c(0.05, 0.15))) +

    profile_theme()

})
