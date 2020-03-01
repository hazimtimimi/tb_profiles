# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build header to show country name and population
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$main_heading <- renderText({

    # Make sure there is a chosen language
    req(input$lan)

    if (input$lan == "FR") {
        return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_FR"]))

    } else if (input$lan == "ES") {
        return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_ES"]))

    } else if (input$lan == "RU") {
        return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_RU"]))

    } else {
        return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country"]))
    }

} )

output$population <- renderText({

    # Make sure data are loaded
    req(pdata()$profile_estimates)

    paste0(ltxt(plabs(), "pop"),
           " ",
           dcyear - 1,
           ": ",
           rounder_mil(pdata()$profile_estimates[, "e_pop_num"]/1e6),
           " ",
           ltxt(plabs(), "million"))
})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build tab names in the appropriate language
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$est_tab_name <- renderText({ ltxt(plabs(), "est_tab_name") })

output$not_tab_name <- renderText({ ltxt(plabs(), "not_tab_name") })

output$out_tab_name <- renderText({ ltxt(plabs(), "out_tab_name") })

output$tpt_tab_name <- renderText({ ltxt(plabs(), "tpt_tab_name") })

output$fin_tab_name <- renderText({ ltxt(plabs(), "fin_tab_name") })
