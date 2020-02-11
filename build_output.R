# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build all the output objects to display in the application
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



output$heading_main <- renderText({ ltxt(plabs(), "head") })

output$heading_estimates <- renderText({ paste0(ltxt(plabs(), "estimates"), ", ", dcyear-1)  })

output$test_value <- renderText({ pdata()$profile_estimates[, "e_inc_num"] })
output$test_value_lohi <- renderText({ paste0("(", pdata()$profile_estimates[, "e_inc_num_lo"],
                                              "-", pdata()$profile_estimates[, "e_inc_num_hi"], ")") })

output$test_value_rowhead <- renderText({ ltxt(plabs(), "incidence_tb") })


estimates_table_content <- reactive({

  # build the dataframe manually
  row_head <- c(ltxt(plabs(), "incidence_tb"),
                ltxt(plabs(), "incidence_tbhiv"),
                ltxt(plabs(), "incidence_rr"),
                ltxt(plabs(), "mortality_hivneg"),
                ltxt(plabs(), "mortality_hivpos"))


  est_num <- c( format_estimate(pdata()$profile_estimates[, "e_inc_num"],
                                pdata()$profile_estimates[, "e_inc_num_lo"],
                                pdata()$profile_estimates[, "e_inc_num_hi"],
                                style="k"),


                format_estimate(pdata()$profile_estimates[, "e_inc_tbhiv_num"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_inc_rr_num"],
                                pdata()$profile_estimates[, "e_inc_rr_num_lo"],
                                pdata()$profile_estimates[, "e_inc_rr_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_mort_exc_tbhiv_num"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_hi"],
                                style="k"),

                format_estimate(pdata()$profile_estimates[, "e_mort_tbhiv_num"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_num_lo"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_num_hi"],
                                style="k")
                )



  est_rate <- c(format_estimate(pdata()$profile_estimates[, "e_inc_100k"],
                                pdata()$profile_estimates[, "e_inc_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_inc_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_tbhiv_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_inc_rr_100k"],
                                pdata()$profile_estimates[, "e_inc_rr_100k_lo"],
                                pdata()$profile_estimates[, "e_inc_rr_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_hi"]),

                format_estimate(pdata()$profile_estimates[, "e_mort_tbhiv_100k"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_100k_lo"],
                                pdata()$profile_estimates[, "e_mort_tbhiv_100k_hi"])
                )


  df <- data.frame(row_head, est_num, est_rate)

  # add the column names
  names(df) <- c("", ltxt(plabs(), "number"),  ltxt(plabs(), "rate_100k"))
  return(df)
})

output$estimates_table <- renderTable({ estimates_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      na="")


output$heading_drestimates <- renderText({ paste0(ltxt(plabs(), "rrmdr_est_pct"), ", ", dcyear-1)  })


drestimates_table_content <- reactive({

  # build the dataframe manually
  row_head <- c(ltxt(plabs(), "new"),
                ltxt(plabs(), "ret"))

  est_pct <- c( format_estimate(pdata()$profile_estimates[, "e_rr_pct_new"],
                                pdata()$profile_estimates[, "e_rr_pct_new_lo"],
                                pdata()$profile_estimates[, "e_rr_pct_new_hi"],
                                style="%"),


                format_estimate(pdata()$profile_estimates[, "e_rr_pct_ret"],
                                pdata()$profile_estimates[, "e_rr_pct_ret_lo"],
                                pdata()$profile_estimates[, "e_rr_pct_ret_hi"],
                                style="%")
                )
  df <- data.frame(row_head, est_pct)

  return(df)

})

output$drestimates_table <- renderTable({ drestimates_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      width = "100%",
                                      # suppress column headers
                                      colnames = FALSE,
                                      na="")


output$inc_chart <-  renderPlot({

  #  Added a legend resulting in more complicated code
  #  The alternative is to dispense with ggtitle() and
  #  instead use Shiny textoutput and styles to render a multi-coloured heading
  #  Legend code here is based on https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/

  pdata()$epi_timeseries %>%
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
               size=1) +

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


output$mort_chart <-  renderPlot({


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
