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

  # est_num_best <- c(pdata()$profile_estimates[, "e_inc_num"],
  #                   pdata()$profile_estimates[, "e_inc_tbhiv_num"],
  #                   pdata()$profile_estimates[, "e_inc_rr_num"],
  #                   pdata()$profile_estimates[, "e_mort_exc_tbhiv_num"],
  #                   pdata()$profile_estimates[, "e_mort_tbhiv_num"])

  est_num <- c( paste0(pdata()$profile_estimates[, "e_inc_num"],
                       " (", pdata()$profile_estimates[, "e_inc_num_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_num_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_inc_tbhiv_num"],
                       " (", pdata()$profile_estimates[, "e_inc_tbhiv_num_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_tbhiv_num_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_inc_rr_num"],
                       " (", pdata()$profile_estimates[, "e_inc_rr_num_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_rr_num_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_mort_exc_tbhiv_num"],
                       " (", pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_lo"],
                       "-", pdata()$profile_estimates[, "e_mort_exc_tbhiv_num_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_mort_tbhiv_num"],
                       " (", pdata()$profile_estimates[, "e_mort_tbhiv_num_lo"],
                       "-", pdata()$profile_estimates[, "e_mort_tbhiv_num_hi"], ")"))

  # est_rate_best <- c(pdata()$profile_estimates[, "e_inc_100k"],
  #                    pdata()$profile_estimates[, "e_inc_tbhiv_100k"],
  #                    pdata()$profile_estimates[, "e_inc_rr_100k"],
  #                    pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k"],
  #                    pdata()$profile_estimates[, "e_mort_tbhiv_100k"])

  est_rate <- c(paste0(pdata()$profile_estimates[, "e_inc_100k"],
                       " (", pdata()$profile_estimates[, "e_inc_100k_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_100k_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_inc_tbhiv_100k"],
                       " (", pdata()$profile_estimates[, "e_inc_tbhiv_100k_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_tbhiv_100k_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_inc_rr_100k"],
                       " (", pdata()$profile_estimates[, "e_inc_rr_100k_lo"],
                       "-", pdata()$profile_estimates[, "e_inc_rr_100k_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k"],
                       " (", pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_lo"],
                       "-", pdata()$profile_estimates[, "e_mort_exc_tbhiv_100k_hi"], ")"),

                paste0(pdata()$profile_estimates[, "e_mort_tbhiv_100k"],
                       " (", pdata()$profile_estimates[, "e_mort_tbhiv_100k_lo"],
                       "-", pdata()$profile_estimates[, "e_mort_tbhiv_100k_hi"], ")"))

  df <- data.frame(row_head, est_num, est_rate)

  # add the column names
  names(df) <- c("", ltxt(plabs(), "number_k"),  ltxt(plabs(), "rate_100k"))
  return(df)
})

output$estimates_table <- renderTable({ estimates_table_content() },
                                      striped = TRUE,
                                      hover = TRUE,
                                      na="")

output$inc_chart <-  renderPlot({


  pdata()$epi_timeseries %>%
     #select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi, c_newinc_100k) %>%
     ggplot(aes(x=year, y=c_newinc_100k, ymin=0)) +
     geom_line(size=1) +

     geom_ribbon(aes(x=year, ymin=e_inc_100k_lo, ymax=e_inc_100k_hi),
                 fill=standard_palette("incidence"),
                 alpha=0.4) +

     geom_line(aes(y=e_inc_100k),
               size=1,
               colour=standard_palette("incidence")) +

     geom_ribbon(aes(x=year, ymin=e_inc_tbhiv_100k_lo, ymax=e_inc_tbhiv_100k_hi),
                 fill=standard_palette("tbhiv_incidence"),
                 alpha=0.4) +

     geom_line(aes(y=e_inc_tbhiv_100k),
               size=1,
               colour=standard_palette("tbhiv_incidence")) +

     scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, dcyear-1)) +
     scale_y_continuous(name = ltxt(plabs(), "rate_100k_yr") ) +

     profile_theme() +


     ggtitle(ltxt(plabs(), "inc") )



})
