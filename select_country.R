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
        # Needed to use all_of() otherwise get a warning about "using an external
        # vector in selections is ambiguous"
        select(iso2, name = all_of(country_name_lan)) %>%
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
                # next line needed to avoid losing the selected country when the language is changed
                selected = input$iso2,
                selectize = FALSE)
})

