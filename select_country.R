# Create a list of sorted countries names according to the language chosen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
countrylist <- reactive({

    country_name_lan <- switch (input$lan,
            "FR" = "country_FR",
            "ES" = "country_ES",
            "RU" = "country_RU",
            "country" #default!
        )

    countrylist <- countries %>%
        select(iso2, name = country_name_lan) %>%
        arrange(name)

    # Return a named list not a dataframe because that is what selectInput() needs
    return(setNames(countrylist[,"iso2"], countrylist[,"name"]))

})


# Build the select country control dynamically server-side depending on the language chosen
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
output$countries <- renderUI({
    selectInput(inputId = "iso2",
                label = "",
                choices = countrylist(),
                selected = "AF",
                selectize = FALSE)
})

