
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the second tab (estimates charts)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

     # Build the legend using a manual colour scale
     scale_color_manual(name = "",
                        breaks = c("inc", "notifs", "tbhivinc"),
                        values = c("inc" = standard_palette("incidence"),
                                   "notifs" = "black",
                                   "tbhivinc" = standard_palette("tbhiv_incidence")),
                        labels = c("inc" = ltxt(plabs(), "incidence_tb"),
                                   # couldn't get R to interpret \n or <br /> embedded in the label itself so
                                   # either split labels into two and use "\n" to join them (which does get interpreted!)
                                   # or don't use legends and instead use colour coding in the title
                                   "notifs" = ltxt(plabs(), "notifs_totnewrel"),
                                   "tbhivinc" = ltxt(plabs(), "incidence_tbhiv"))
                        ) +

    # suppress legend
    theme(legend.position="none")

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
    ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
    geom_line(size=1,
		          colour=standard_palette("mortality_exc_tbhiv")) +

    geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                fill=standard_palette("mortality_exc_tbhiv"),
                alpha=0.4) +

     scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, dcyear-1)) +

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
    ggplot(aes(x=label_text,
               y=best)) +
    geom_point() +
    # Some records have null entries for lo and hi which triggers an error so
    # set them to zero using NZ() because this only happens when best is zero.
    geom_pointrange(aes(ymin=NZ(lo), ymax=NZ(hi)),
                    size=1.2,
                    colour='Darkgreen') +
    expand_limits(y=0) +
    # USe space separators to label large numbers; don't display fractions
    scale_y_continuous(labels = function(x){ifelse(x %% 1 == 0, rounder(x),"")},
                       expand = expansion(mult = c(0.05, 0.15))) +
    coord_flip() +
    profile_theme()

})
