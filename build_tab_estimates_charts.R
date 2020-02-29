
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the second tab (estimates charts)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Incidence chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$inc_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  #  Added a legend resulting in more complicated code
  #  The alternative is to dispense with ggtitle() and
  #  instead use Shiny textoutput and styles to render a multi-coloured heading
  #  Legend code here is based on https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/

  pdata()$epi_timeseries %>%

     # quick fix if TB/HIV estimates are all NAs, make sure
     # data type is numeric (problem with Andorra etc)
     mutate(e_inc_tbhiv_100k = as.numeric(e_inc_tbhiv_100k),
            e_inc_tbhiv_100k_lo = as.numeric(e_inc_tbhiv_100k_lo),
            e_inc_tbhiv_100k_hi = as.numeric(e_inc_tbhiv_100k_hi)) %>%

     ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
     geom_line(size=1, aes(colour="notifs")) +

     geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                 fill=standard_palette("incidence"),
                 alpha=0.4) +

     geom_line(aes(y=e_inc_100k, colour="inc"),
               size=1) +

     geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi),
                 fill=standard_palette("tbhiv_incidence"),
                 alpha=0.4) +

     geom_line(aes(y=e_inc_tbhiv_100k, colour="tbhivinc"),
               size=1,
               # The next option suppresses warnings about missing values
               # from appearing in the console
               na.rm = TRUE) +

     scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, dcyear-1)) +

     profile_theme() +

     ggtitle(ltxt(plabs(), "inc"),
             subtitle = ltxt(plabs(), "rate_100k_yr") ) +

     # Build the legend usual a manual colour scale
     scale_color_manual(name = "",
                        breaks = c("inc", "notifs", "tbhivinc"),
                        values = c("inc" = standard_palette("incidence"),
                                   "notifs" = "black",
                                   "tbhivinc" = standard_palette("tbhiv_incidence")),
                        labels = c("inc" = ltxt(plabs(), "incidence_tb"),
                                   "notifs" = ltxt(plabs(), "notifs_totnewrel"),
                                   "tbhivinc" = ltxt(plabs(), "incidence_tbhiv"))) +

    # Use guide_legend() to force the legends into two columns only. This prevented
    # long text from being truncated in the output
    guides(colour = guide_legend(ncol = 2, byrow = TRUE))


})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Mortality chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$mort_chart <-  renderPlot({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  pdata()$epi_timeseries %>%
    ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
    geom_line(size=1,
		          colour=standard_palette("mortality_exc_tbhiv")) +

    geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                fill=standard_palette("mortality_exc_tbhiv"),
                alpha=0.4) +

     scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, dcyear-1)) +

     profile_theme() +


     ggtitle(ltxt(plabs(), "mortality_hivneg"),
             subtitle = ltxt(plabs(), "rate_100k_yr") )


})
