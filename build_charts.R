
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the charts
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Incidence ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$inc_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  df <- pdata()$epi_timeseries  |>
    # restrict to years starting 2010
    filter(year >=2010)

  # Calculate the milestone for 2025
  milestone_inc <- df |>
    filter(year == 2015) |>
    mutate(ms = e_inc_100k * inc_milestone_vs_2015) |>
    select(ms)


  highchart()  |>

    hc_title(text = ltxt(plabs(), "est_inc_rate")) |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "rate_100k")),
             min = 0,
             tickAmount = 3,
             # avoid unnecessary vertical space
             endOnTick = FALSE,
             allowDecimals = FALSE,
             # Add dashed line to show 2025 milestone
             plotLines = list(
               list(color = '#999999',
                    dashStyle = 'dash',
                    value = milestone_inc$ms,
                    width = 2,
                    label = list(text =  stringr::str_replace(ltxt(plabs(), "milestone"), "%s0", milestone_yr),
                                 align = "left",
                                 style = list(color = '#333333')
                    )
               ))) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE,
               valueDecimals = 0) |>

    # Drop the legend
    hc_legend(enabled = FALSE) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "est_inc_100k"),
                  data = df,
                  hcaes(x = year,
                        y = e_inc_100k),

                  lineWidth = 6,
                  color = gtbreport::palette_gtb("inc"),
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = ltxt(plabs(), "ui"),
                  data = df,
                  hcaes(x = year,
                        low = e_inc_100k_lo,
                        high = e_inc_100k_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = gtbreport::palette_gtb("inc"),
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "c_newinc_100k"),
                  data = df,
                  hcaes(x = year,
                        y = c_newinc_100k),

                  color = "#555555",
                  marker = list(enabled = FALSE)) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_incidence"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))


})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Mortality ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$mort_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$epi_timeseries)

  df <- pdata()$epi_timeseries  |>
    # restrict to years starting 2010
    filter(year >=2010)

  # Calculate the milestone for 2025
  milestone_deaths <- df |>
    filter(year == 2015) |>
    mutate(ms = e_mort_num * mort_milestone_vs_2015) |>
    select(ms)

  highchart()  |>

    hc_title(text = ltxt(plabs(), "est_deaths")) |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "number")),
             min = 0,
             tickAmount = 3,
             # avoid unnecessary vertical space
             endOnTick = FALSE,
             allowDecimals = FALSE,
             # Add dashed line to show 2025 milestone
             plotLines = list(
               list(color = '#999999',
                    dashStyle = 'dash',
                    value = milestone_deaths$ms,
                    width = 2,
                    label = list(text =  stringr::str_replace(ltxt(plabs(), "milestone"), "%s0", milestone_yr),
                                 align = "left",
                                 style = list(color = '#333333')
                    )
               ))) |>


    hc_tooltip(crosshairs = TRUE,
               shared = TRUE,
               valueDecimals = 0) |>

    # Drop the legend
    hc_legend(enabled = FALSE) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "e_mort_num"),
                  data = df,
                  hcaes(x = year,
                        y = e_mort_num),

                  lineWidth = 6,
                  color = "black",
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = ltxt(plabs(), "ui"),
                  data = df,
                  hcaes(x = year,
                        low = e_mort_num_lo,
                        high = e_mort_num_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = "black",
                  fillOpacity = 0.3,
                  marker = list(enabled = FALSE)) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_deaths"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Cases attributable to 5 risk factors ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$rf_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(is.data.frame(pdata()$attributable_cases))

  df <- pdata()$attributable_cases |>

    # Get the labels for each of the five risk factor categories
    right_join(filter(plabs(), label_tag %in% c("alc", "dia", "hiv", "smk", "und") ),
               by = c("risk_factor"="label_tag")) |>

    # Order by descending number of attributable cases
    arrange(desc(best)) |>
    # Preserve the order by setting risk_factor as a factor variable
    mutate(label_text = factor(label_text, levels = rev(label_text)))


  highchart()  |>

    hc_title(text = paste0(ltxt(plabs(), "est_cases_risk_factors"), ", ", dcyear-1)) |>

    hc_chart(inverted = TRUE) |>

    hc_xAxis(title = list(text = ""),
             categories = df$label_text) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "number")),
             min = 0,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    # Stop line appearing between dots on hover
    hc_plotOptions(
      series = list(
        states = list(
          hover = list(
            enabled = FALSE)))) |>

    # Drop the legend
    hc_legend(enabled = FALSE) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "est_number"),
                  data = df,
                  hcaes(y = best),
                  lineWidth = 0,
                  color = gtbreport::palette_gtb("inc"),
                  marker = list(radius = 6)) |>

    hc_add_series(type = "errorbar",
                  name = ltxt(plabs(), "ui"),
                  data = df,
                  hcaes(low = lo,
                        high = hi),
                  linkedTo = ":previous",
                  color = gtbreport::palette_gtb("inc"),
                  opacity = 0.5,
                  lineWidth = 4,
                  whiskerLength = 15,
                  whiskerWidth = 4) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_risk_factors"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))


})




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Notifications by age group and sex ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


output$agesex_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$profile_incnum_agesex)

  # Get the age/sex disaggregated estimates and notifications
  agesex <- pdata()$profile_incnum_agesex
  notifs_agesex <- pdata()$profile_data |>
    select(agegroup_option, starts_with("newrel_m"), starts_with("newrel_f"))


  # If no disaggregated notifications reported then set the agegroup option to zero
  if (NZ(notifs_agesex$agegroup_option) %in% c(0, 220, 221)) {

    notifs_agesex <- notifs_agesex |>
      select(contains("04"),
             contains("514"),
             contains("1524"),
             contains("2534"),
             contains("3544"),
             contains("4554"),
             contains("5564"),
             contains("65"))

  } else if (notifs_agesex$agegroup_option == 222) {

    notifs_agesex <- notifs_agesex |>
      select(contains("014"),
             contains("1524"),
             contains("2534"),
             contains("3544"),
             contains("4554"),
             contains("5564"),
             contains("65"))

  } else if (notifs_agesex$agegroup_option == 223) {

    notifs_agesex <- notifs_agesex |>
      select(contains("04"),
             contains("514"),
             contains("15plus"))

  }


  # switch to long format
  notifs_agesex_long <- notifs_agesex |>
    pivot_longer(cols = starts_with("newrel_"),
                 names_to = c("sex", "age_group"),
                 # thanks to Hadley, help on pivot_longer icludes
                 # and example of doing this with TB variables!
                 names_pattern = "newrel_(.)(.*)",
                 values_to = "notifs")

  # faff about with the estimates age/sex table
  # so can match to notifs age/sex
  agesex <- agesex |>
    mutate(age_group = str_remove(age_group,"-")) |>
    mutate(age_group = ifelse(age_group=="65plus", "65", age_group))

  agesex <- agesex |>
    inner_join(notifs_agesex_long, by = c("age_group", "sex"))

  # Rename the 5-14 age group

  agesex$age_group <- str_replace(agesex$age_group, "514", "0514")

  agesex <- agesex |>
    arrange(sex, age_group)

  agesex$age_group <- factor(agesex$age_group,
                             levels=c("04", "0514", "014", "1524", "2534", "3544", "4554", "5564", "65", "15plus"),
                             labels=c("0-4", "5-14", "0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "\u226565", "\u226515"))

  # Find the maximum high bound
  max_hi <- max(agesex$hi)

  highchart()  |>

    hc_title(text = paste0(ltxt(plabs(), "est_notif_age_sex"), ", ", dcyear-1)) |>

    hc_chart(inverted = TRUE) |>

    hc_xAxis(title = list(text = ltxt(plabs(), "age_group")),
             categories = as.character.factor(agesex$age_group),
             reversed = FALSE) |>

    hc_yAxis(title = list(text = ""),
             # Build a formatter function to label the axis only with whole positive numbers
             # i.e. don't show a label if there is a remainder after dividing by 1 (x % 1)
             # Have to manually convert large numbers to "k" "M" format as Highcharts doesn't
             # provide this. Its Highcharts.numberFormat() function returns long values which
             # get cluttered for charts with large numbers
             labels = list(formatter = JS("function(){

                                          x = Math.abs(this.value);

                                          if (x % 1 != 0) {
                                            return '';
                                          } else if (x >= 1000000) {
                                            return Highcharts.numberFormat(x/1000000, 1) + 'M';
                                          } else if (x >= 10000) {
                                            return Highcharts.numberFormat(x/1000, 0) + 'k';
                                          } else {
                                            return Highcharts.numberFormat(x, 0);
                                          }
                                          }"),
                           # prevent autorotation to avoid losing vertical space
                           autoRotation = 0),
             # Set minimum and maximum values to be the same absolute number so that
             # zero is in the middle of the axis
             max = max_hi,
             min = -1 * max_hi) |>

    hc_tooltip(
      formatter = JS("function(){

				var returnString;

				/* Adjust the tooltip depending on the series being highlighted */

				if (this.series.type == 'errorbar') {

					returnString = Highcharts.numberFormat(Math.abs(this.point.low), 0) + '–' +
									Highcharts.numberFormat(Math.abs(this.point.high), 0);

					} else {

					returnString = Highcharts.numberFormat(Math.abs(this.point.y), 0);

					}

				return this.series.name + ' ( ' + this.point.category +'): <b>' + returnString + '</b>';
			}"),
      crosshairs = TRUE) |>

    hc_add_series(type = "line",
                  name = paste0(ltxt(plabs(), "est_number"), " (", tolower(ltxt(plabs(), "female")), ")"),
                  showInLegend = FALSE,
                  # Stop default stacking of line series
                  stacking = NA,
                  lineWidth = 0,
                  data = filter(agesex, sex == "f") |> mutate(best = -1*best),

                  hcaes(x = age_group,
                        y = best),

                  color = gtbreport::palette_gtb("inc"),
                  opacity = 0.7,
                  marker = list(symbol = "circle",
                                radius = 6),
                  # Stop line appearing between dots on hover
                  states = list(hover = list(enabled = FALSE))
    )  |>

    hc_add_series(type = "errorbar",
                  name = paste0(ltxt(plabs(), "ui"), " (", tolower(ltxt(plabs(), "female")), ")"),
                  data = filter(agesex, sex == "f") |> mutate(lo = -1*lo,
                                                              hi = -1*hi),
                  hcaes(low = lo,
                        high = hi),

                  linkedTo = ":previous",
                  color = gtbreport::palette_gtb("inc"),
                  opacity = 0.5,
                  lineWidth = 4,
                  whiskerLength = 15,
                  whiskerWidth = 4) |>

    hc_add_series(type = "line",
                  name = paste0(ltxt(plabs(), "est_number"), " (", tolower(ltxt(plabs(), "male")), ")"),
                  showInLegend = FALSE,
                  # Stop default stacking of line series
                  stacking = NA,
                  lineWidth = 0,
                  data = filter(agesex, sex == "m"),

                  hcaes(x = age_group,
                        y = best),

                  color = gtbreport::palette_gtb("inc"),
                  opacity = 0.7,
                  marker = list(symbol = "circle",
                                radius = 6),
                  # Stop line appearing between dots on hover
                  states = list(hover = list(enabled = FALSE)) )  |>

    hc_add_series(type = "errorbar",
                  name = paste0(ltxt(plabs(), "ui"), " (", tolower(ltxt(plabs(), "male")), ")"),
                  data = filter(agesex, sex == "m"),
                  hcaes(low = lo,
                        high = hi),

                  linkedTo = ":previous",
                  color = gtbreport::palette_gtb("inc"),
                  opacity = 0.5,
                  lineWidth = 4,
                  whiskerLength = 15,
                  whiskerWidth = 4) |>

    hc_add_series(type = "bar",
                  name = paste0(ltxt(plabs(), "notifs_totnewrel"), " (", tolower(ltxt(plabs(), "female")), ")"),
                  stacking = "normal",
                  data = filter(agesex, sex == "f") |> mutate(notifs = -1*notifs),

                  hcaes(x = age_group,
                        y = notifs),

                  color = gtbreport::palette_gtb("female")) |>

    hc_add_series(type = "bar",
                  name = paste0(ltxt(plabs(), "notifs_totnewrel"), " (", tolower(ltxt(plabs(), "male")), ")"),
                  stacking = "normal",
                  data = filter(agesex, sex == "m"),
                  hcaes(x = age_group,
                        y = notifs),
                  color = gtbreport::palette_gtb("male")) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_agegroup_sex"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))

})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Prevalence of rif resistance among pulmonary bacteriologically confirmed TB  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$rr_prop_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$rr_timeseries)

  rr <- pdata()$rr_timeseries

  highchart()  |>

    hc_title(text = ltxt(plabs(), "est_rr_pct")) |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = "%"),
             min = 0,
             tickAmount = 3,
             endOnTick = FALSE,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "pulm_bc_ret"),
                  data = rr,
                  hcaes(x = year,
                        y = e_rr_pcnt_ret),

                  lineWidth = 6,
                  color = "#F1DC27",
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = ltxt(plabs(), "ui"),
                  data = rr,
                  hcaes(x = year,
                        low = e_rr_pcnt_ret_lo,
                        high = e_rr_pcnt_ret_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = "#F1DC27",
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE)) |>

  hc_add_series(type = "line",
                  name = ltxt(plabs(), "pulm_bc_new"),
                  data = rr,
                  hcaes(x = year,
                        y = e_rr_pcnt_new),

                  lineWidth = 6,
                  color = "#F49A20",
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = ltxt(plabs(), "ui"),
                  data = rr,
                  hcaes(x = year,
                        low = e_rr_pcnt_new_lo,
                        high = e_rr_pcnt_new_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = "#F49A20",
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE)) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_RR_prevalence"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))

})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Incidence of rif-resistant TB  ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$rr_inc_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$rr_timeseries)

  rr <- pdata()$rr_timeseries

  highchart()  |>

    hc_title(text = ltxt(plabs(), "est_rr_num")) |>

    hc_xAxis(title = list(text = "")) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "number")),
             min = 0,
             tickAmount = 3,
             endOnTick = FALSE,
             allowDecimals = FALSE) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    # Drop the legend
    hc_legend(enabled = FALSE) |>

    hc_add_series(type = "line",
                  name = ltxt(plabs(), "e_inc_rr_num"),
                  data = rr,
                  hcaes(x = year,
                        y = e_inc_rr_num),

                  lineWidth = 6,
                  color = gtbreport::palette_gtb("inc"),
                  marker = list(enabled = FALSE)) |>

    hc_add_series(type = "arearange",
                  name = ltxt(plabs(), "ui"),
                  data = rr,
                  hcaes(x = year,
                        low = e_inc_rr_num_lo,
                        high = e_inc_rr_num_hi),

                  lineWidth = 0,
                  linkedTo = ":previous",
                  color = gtbreport::palette_gtb("inc"),
                  fillOpacity = 0.3,
                  zIndex = 0,
                  marker = list(enabled = FALSE)) |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_RR_incidence"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))

})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Provision of TPT ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$tpt_chart <- renderHighchart({

  # Make sure there are data to plot
  req(pdata()$tpt_timeseries)

  tpt <- pdata()$tpt_timeseries

  #Make sure there are some non-null values in the dataframe
  req(sum(!is.na(tpt$hiv)) +
        sum(!is.na(tpt$contact_04)) +
        sum(!is.na(tpt$contact_5plus)) > 0)

  # Combine the disaggregated contact numbers
  tpt <- tpt |>
    mutate(hh_contacts = sum_with_nulls(contact_04, contact_5plus)) |>
    select(year, hiv, hh_contacts)


  highchart()  |>

    hc_title(text = ltxt(plabs(), "tpt")) |>

    hc_chart(type = "column") |>

    hc_xAxis(categories = tpt$year) |>

    hc_yAxis(title = list(text = ltxt(plabs(), "number")),
             min = 0,
             reversedStacks = FALSE) |>

    hc_plotOptions(series = list(stacking = "normal")) |>

    hc_tooltip(crosshairs = TRUE,
               shared = TRUE) |>

    hc_add_series(name = ltxt(plabs(), "plhiv"),
                  data = tpt$hiv,
                  color = "#FFC425") |>

    hc_add_series(name = ltxt(plabs(), "hh_contacts"),
                  data = tpt$hh_contacts,
                  color = "#66b032") |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_TPT"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                   )
                 ))


})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Expenditure (committed funding) ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

output$funding_chart <-  renderHighchart({

  # Make sure there are data to plot
  req(pdata()$funding_timeseries)

  funding_data <- pdata()$funding_timeseries

  # Make sure there are some data to display
  # There will only be the year column if no data, so check number of columns

  ndata_cols <- ncol(funding_data) - 1

  # Only plot the data if have at least one year with data
  req(ndata_cols > 0)

  highchart()  |>

    hc_title(text = ltxt(plabs(), "funding_available_tb_services")) |>

    hc_chart(type = "column") |>

    hc_xAxis(categories = funding_data$year) |>

    hc_yAxis(title = list(text = ifelse(check_entity_type(input$entity_type) != "group",
                                        ltxt(plabs(), "usd_millions"),
                                        #The scale is constant US$ if showing aggregate finance chart
                                        ltxt(plabs(), "usd_millions_constant"))),
             min = 0,
             reversedStacks = FALSE) |>

    hc_plotOptions(series = list(stacking = "normal")) |>

    hc_tooltip(
      formatter = JS(
        paste(
          "function(){

				return this.points.reverse().reduce(function (s, point) {
                return s + '<br/>' +
                '<span style=\"color:' + point.series.color + '\">&#9679</span>' +
                point.series.name + ': US$ ' +
                    Highcharts.numberFormat(point.y,1) + '",
          ltxt(plabs(), "million"),
          "';
            }, this.x) +
            '<br/><b>",
          ltxt(plabs(), "total_funding"),
          ": US$ ' +
            Highcharts.numberFormat(this.points[0].total,1) +
            ' ",
          ltxt(plabs(), "million"),
          "</b>';
			}")
      ),
      shared = TRUE,
      crosshairs = TRUE) |>

    hc_add_series(name = ltxt(plabs(), "domestic_funding"),
                  data = funding_data$a_domestic_funds,
                  color = "#7DAAD4") |>

    hc_add_series(name = ltxt(plabs(), "international_funding"),
                  data = funding_data$b_international_funds,
                  color = "#E9C04F") |>

    hc_exporting(enabled = TRUE,
                 filename = paste0("GTB_report_", dcyear, "_funding"),
                 chartOptions = list(
                   subtitle=list(
                     text=entity_name()
                     )
                   ))

})
