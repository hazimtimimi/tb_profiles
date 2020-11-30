# Create a list of sorted country or regional names according to the
# chosen entity type and language
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

entity_list <- reactive({

    if (check_entity_type(input$entity_type) == "group") {

    group_name_lan <- switch (check_lan(input$lan),
            "FR" = "group_description_FR",
            "ES" = "group_description_ES",
            "RU" = "group_description_RU",
            "group_description" #default!
        )

    grouplist <- aggregate_groups %>%
        # Needed to use all_of() otherwise get a warning about "using an external
        # vector in selections is ambiguous"
        select(group_code, name = all_of(group_name_lan))

    # Return a named list not a dataframe because that is what selectInput() needs
    return(setNames(grouplist[,"group_code"], grouplist[,"name"]))

    } else {

    # always default back to country

    country_name_lan <- switch (check_lan(input$lan),
            "FR" = "country_FR",
            "ES" = "country_ES",
            "RU" = "country_RU",
            "country" #default!
        )

    entity_list <- countries %>%
        # Needed to use all_of() otherwise get a warning about "using an external
        # vector in selections is ambiguous"
        select(iso2, name = all_of(country_name_lan)) %>%
        arrange(name)

    # Return a named list not a dataframe because that is what selectInput() needs
    return(setNames(entity_list[,"iso2"], entity_list[,"name"]))

    }


})


# Build the select entity control dynamically server-side depending on the
# chosen entity type and language
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$entities <- renderUI({

    if (check_entity_type(input$entity_type) == "group") {
        already_selected <- input$group_code
    } else {
        already_selected <- input$iso2
    }

    selectInput(inputId = ifelse(check_entity_type(input$entity_type) == "group", "group_code","iso2"),
                label = "",
                choices = entity_list(),
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE)
})

