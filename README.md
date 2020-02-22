# Tuberculosis country profiles
Show a selecton of important indicators concerning tuberculosis (TB) in a chosen country or area. Such a selection is known as a *country profile*.

## Features

* Choose from 216 countries and areas
* Choose interface and results in English, French, Russian or Spanish
* View results as tables and charts displayed under a series of thematic tabs

## Components and data source

This is an app built using [Shiny](https://shiny.rstudio.com/). It uses data published by the World Health Organization's [Global Tuberculosis Programme](https://www.who.int/tb/data).

The app pulls data directly from the global TB database. Unfortunately there is no standard api with which to interogate the database, so I built a script and some queries to return JSON files specifically for this app.

The script can return three types of JSON file:

1. https://extranet.who.int/tme/generateJSON.asp?ds=countries. This returns a list of all countries and areas which report TB data annually to WHO. The list includes the entity name in English, French, Spanish and Russian.

2. https://extranet.who.int/tme/generateJSON.asp?ds=labels&lan=XX  (where XX is a two-letter language code, EN=English, ES=Spanish, FR=French, RU=Russian). This returns all the text that appear in the user interface such as headings, paragraphs, tables, legends, etc., in the chosen language.

3. https://extranet.who.int/tme/generateJSON.asp?ds=data&iso2=XX (where XX is a country [ISO2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)). This returns all the data that appear in the tables and charts for the chosen country or area.

## Data updates

WHO collects TB data annually from all countries and areas and publishes them in the  [Global Tuberculosis Report](https://www.who.int/tb/publications/global_report/en/), usually in October of each year.


