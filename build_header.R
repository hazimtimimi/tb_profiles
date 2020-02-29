# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build header to show country name and population
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$main_heading <- renderText({ ltxt(plabs(), "head") })

output$country <- renderText({

    # Need this very first line so as not to trigger an error when
    # the application first loads ... annoying
    if (length(input$lan) == 0 ){
        # return an empty string
        return("")

    } else if (input$lan == "FR") {
        return(pdata()$profile_properties[, "country_FR"])

    } else if (input$lan == "ES") {
        return(pdata()$profile_properties[, "country_ES"])

    } else if (input$lan == "RU") {
        return(pdata()$profile_properties[, "country_RU"])

    } else {
        return(pdata()$profile_properties[, "country"])
    }

} )

output$population <- renderText({

    paste0(ltxt(plabs(), "pop"),
           " ",
           dcyear - 1,
           ": ",
           rounder_mil(pdata()$profile_estimates[, "e_pop_num"]/1e6),
           " ",
           ltxt(plabs(), "million"))
})

