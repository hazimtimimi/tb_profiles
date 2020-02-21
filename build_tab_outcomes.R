# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the sixth tab (financing charts and tables)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$outcomes_heading <- renderText({ ltxt(plabs(), "tsr_cohort") })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. treatment success rate table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Combine the data with the row headers manually and render for output

outcomes_table_content <- reactive({

  # build the data frame manually
  df <- data.frame(

            c(paste(ltxt(plabs(), "tsr_newrel"), dcyear - 2),
               paste(ltxt(plabs(), "tsr_ret_nrel"), dcyear - 2),
               paste(ltxt(plabs(), "tsr_tbhiv"), dcyear - 2),
               paste(ltxt(plabs(), "tsr_mdr"), dcyear - 3),
               paste(ltxt(plabs(), "tsr_xdr"), dcyear - 3)),

             # treatment success rates
             c(rounder_pct(pdata()$profile_data[, "c_new_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_ret_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_tbhiv_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_mdr_tsr"]),
               rounder_pct(pdata()$profile_data[, "c_xdr_tsr"])),

             # cohort size
             c(rounder(pdata()$profile_data[, "newrel_coh"]),
               rounder(pdata()$profile_data[, "ret_nrel_coh"]),
               rounder(pdata()$profile_data[, "tbhiv_coh"]),
               rounder(pdata()$profile_data[, "mdr_coh"]),
               rounder(pdata()$profile_data[, "xdr_coh"]))
            )

  # add the column names
  names(df) <- c("", ltxt(plabs(), "success"),  ltxt(plabs(), "cohort"))
  return(df)

})


output$oucomes_table <- renderTable({ outcomes_table_content() },


            striped = TRUE,
            hover = TRUE,
            width = "100%",
            # right-align the cohort column
            align = "llr",
            na="")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. treatment success rate chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$tsr_chart <-  renderPlot({

    pdata()$profile_outcomes %>%

    # Make sure all columns are numeric
    # (if the whole series for a column is NA then the column is not numeric!)
    mutate_at(vars(-year), as.numeric) %>%

    ggplot(aes(x=year, ymin=0, ymax=100)) +

    geom_line(aes(y=c_new_tsr,
                  colour="c_new_tsr"),
              size=2) +

    geom_line(aes(y=c_ret_tsr,
                  colour="c_ret_tsr"),
              size=2) +

    geom_line(aes(y=c_tbhiv_tsr,
                  colour="c_tbhiv_tsr"),
              size=2) +

    geom_line(aes(y=c_mdr_tsr,
                  colour="c_mdr_tsr"),
              size=2) +

    geom_line(aes(y=c_xdr_tsr,
                  colour="c_xdr_tsr"),
              size=2) +

    profile_theme() +

     ggtitle(ltxt(plabs(), "tsr"),
             subtitle = "(%)" ) +

    scale_colour_manual("",
                        breaks = c("c_new_tsr", "c_ret_tsr", "c_tbhiv_tsr", "c_mdr_tsr", "c_xdr_tsr" ),
                        labels = c(ltxt(plabs(), "tsr_newrel_short"),
                                   ltxt(plabs(), "tsr_ret_nrel_short"),
                                   ltxt(plabs(), "tsr_hiv_pos"),
                                   ltxt(plabs(), "tsr_rr_mdr"),
                                   ltxt(plabs(), "tsr_xdr_short")),
                        values = c("c_new_tsr"="blue",
                                   "c_ret_tsr"="green",
                                   "c_tbhiv_tsr"="red",
                                   "c_mdr_tsr"="orange",
                                   "c_xdr_tsr"="black")) +

    # Use guide_legend() to force the legends into two columns only. This prevented
    # long text from being truncated in the output
    guides(colour = guide_legend(ncol = 2, byrow = TRUE))


})

