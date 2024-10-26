# Tuberculosis country and group profiles
Show a set of tuberculosis-related indicators for a chosen country, area or group of countries.

## Features

* Choose an entity:
  * one of 215 countries and areas
  * one of the 6 World Health Organization (WHO) regions 
  * the global aggregate of all 215 countries and areas
* Choose the language for the interface and results: English, French, Russian or Spanish
* View the selected entities data as charts and data tables.

## Components and data source

This is an app built using [Shiny](https://shiny.rstudio.com/) and hosted at [shinyapps.io](https://worldhealthorg.shinyapps.io/tb_profiles/). It uses data published by the WHO's [Global Tuberculosis Programme](https://www.who.int/teams/global-tuberculosis-programme/data).

The app pulls data directly from WHO's global tuberculosis (TB) database. Unfortunately there is no standard api with which to interrogate the database, so I built a script and some queries to return JSON files specifically for this app.

The script can return five types of JSON file:

1. https://extranet.who.int/tme/generateJSON.asp?ds=countries. This returns a list of all countries and areas which report TB data annually to WHO. The list includes the entity name in English, French, Spanish and Russian.

2. https://extranet.who.int/tme/generateJSON.asp?ds=groups. This returns a list of all groups for which profiles are available, currently six WHO regions countries and the global group of all countries and areas which report TB data annually to WHO. The list includes the entity name in English, French, Spanish and Russian.

3. https://extranet.who.int/tme/generateJSON.asp?ds=labels&lan=XX  (where XX is a two-letter language code, EN=English, ES=Spanish, FR=French, RU=Russian). This returns all the text that appear in the user interface such as headings, paragraphs, tables, legends, etc., in the chosen language.

4. https://extranet.who.int/tme/generateJSON.asp?ds=data&iso2=XX (where XX is a country [ISO2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)). This returns all the data that appear in the tables and charts for the chosen country or area.

5. https://extranet.who.int/tme/generateJSON.asp?ds=group_data&group_code=XXX (where XXX is a code for a WHO region or the global group). This returns all the data that appear in the tables and charts for the group of countries and areas.

## Graphics

Starting with version 3.0 (October 2024) I switched from using standard [ggplot2](https://ggplot2.tidyverse.org/) charts to interactive javascript-based Highcharts using the [highcharter package](https://jkunst.com/highcharter/).

## Data updates

WHO collects TB data annually from all countries and areas and publishes them in the  [Global Tuberculosis Report](https://www.who.int/teams/global-tuberculosis-programme/data), usually in October of each year.

