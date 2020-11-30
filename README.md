# Tuberculosis country and group profiles
Show a selecton of important indicators concerning tuberculosis (TB) for a chosen country, area or group of countries.

## Features

* Choose a country or area (from a list of 215 countries and areas) or a group (from a list of 6 WHO regions and the global group of all 215 countries and areas)
* Choose the language for the interface and results (from English, French, Russian or Spanish)
* View the results as a two-column output of tables and charts.

## Components and data source

This is an app built using [Shiny](https://shiny.rstudio.com/) and hosted at [shinyapps.io](https://worldhealthorg.shinyapps.io/tb_profiles/). It uses data published by the World Health Organization's [Global Tuberculosis Programme](https://www.who.int/tb/data).

The app pulls data directly from the global TB database. Unfortunately there is no standard api with which to interogate the database, so I built a script and some queries to return JSON files specifically for this app.

The script can return five types of JSON file:

1. https://extranet.who.int/tme/generateJSON.asp?ds=countries. This returns a list of all countries and areas which report TB data annually to WHO. The list includes the entity name in English, French, Spanish and Russian.

2. https://extranet.who.int/tme/generateJSON.asp?ds=groups. This returns a list of all groups for which profiles are available, currently six WHO regions countries and the global group of all countries and areas which report TB data annually to WHO. The list includes the entity name in English, French, Spanish and Russian.

3. https://extranet.who.int/tme/generateJSON.asp?ds=labels&lan=XX  (where XX is a two-letter language code, EN=English, ES=Spanish, FR=French, RU=Russian). This returns all the text that appear in the user interface such as headings, paragraphs, tables, legends, etc., in the chosen language.

4. https://extranet.who.int/tme/generateJSON.asp?ds=data&iso2=XX (where XX is a country [ISO2 code](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)). This returns all the data that appear in the tables and charts for the chosen country or area.

5. https://extranet.who.int/tme/generateJSON.asp?ds=group_data&group_code=XXX (where XXX is a code for a WHO region or the global group). This returns all the data that appear in the tables and charts for the group of countries and areas.


## Data updates

WHO collects TB data annually from all countries and areas and publishes them in the  [Global Tuberculosis Report](https://www.who.int/tb/publications/global_report/en/), usually in October of each year.


***

## Graphics

The app uses standard [ggplot2](https://ggplot2.tidyverse.org/) charts. I experimented with converting the charts to [plotly](https://plot.ly/r/) for a bit of interactivity,  but decided against it because:

1. The URLs get really long and messed up; I like the simple URLs which simply show the language, country ISO2 code and tab to display.

2. The style of headings is slightly different and needs tweaking, plus ggplot2 subheadings do not get automatically converted meaning I'd need to fudge it a bit as shown in [this post](https://datascott.com/blog/subtitles-with-ggplotly/).

3. The dark gray horizontal gridlines render as dark gray when printing so have to faff about with themes to overcome this.

None of these are deal breakers, but they made enough of a difference to convince me that the marginal gain was not worth the extra effort. 

To convert a chart to using plotly:

```

# In app.R:
# - - - - - 
# include the plotly package

library(plotly)

# In ui():
# - - - - 
# Change plotOutput() to plotlyOutput(), for example:

plotlyOutput(outputId = "mort_chart")

# In server():
# - - - - - -
# Change renderPlot() to renderPlotly(), and within it return the result
# of applying ggplotly() to the plot object generated by ggplot(), for example:

output$mort_chart <-  renderPlotly({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  plot <- pdata()$epi_timeseries %>%
          ggplot(aes(x=year, y=e_mort_exc_tbhiv_100k, ymin=0)) +
          geom_line(size=1,
      		          colour=standard_palette("mortality_exc_tbhiv")) +

          geom_ribbon(aes(x=year, ymin=e_mort_exc_tbhiv_100k_lo, ymax=e_mort_exc_tbhiv_100k_hi),
                      fill=standard_palette("mortality_exc_tbhiv"),
                      alpha=0.4) +

           scale_x_continuous(name="", breaks = c(2000, 2005, 2010, 2015, dcyear-1)) +

           profile_theme() +

           ggtitle(ltxt(plabs(), "mortality_hivneg"),
                   subtitle = ltxt(plabs(), "rate_100k_yr") )

    # convert to plotly
    return(ggplotly(plot))

})

```
