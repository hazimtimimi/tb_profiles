# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build header to show country name and population
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$main_heading <- renderText({

    # Make sure there is a chosen language
    req(input$lan)

    if (check_entity_type(input$entity_type) == "group") {

        if (input$lan == "FR") {
                return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "group_description_FR"]))

            } else if (input$lan == "ES") {
                return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "group_description_ES"]))

            } else if (input$lan == "RU") {
                return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "group_description_RU"]))

            } else {
                return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "group_description"]))
            }

    } else {

        # Always default back to country

        if (input$lan == "FR") {
            return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_FR"]))

        } else if (input$lan == "ES") {
            return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_ES"]))

        } else if (input$lan == "RU") {
            return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country_RU"]))

        } else {
            return(paste0(ltxt(plabs(), "head"), ": ", pdata()$profile_properties[, "country"]))
        }

    }


} )


