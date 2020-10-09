
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Build output for the notifications charts tab
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Notifications by age and sex chart
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


agesex_data <- reactive({

  # Make sure there are data to plot
  req(pdata()$profile_incnum_agesex)

  # Get the age/sex disaggregated estimates and notifications
  agesex <- pdata()$profile_incnum_agesex
  notifs_agesex <- pdata()$profile_data %>%
    select(starts_with("newrel_m"), starts_with("newrel_f"))


  # Find out which age groups have been reported by the country
  # 1. Adults -- if the 15plus variables are not NA but the disaggregated
  # ones are NA then we only want to plot 15plus

  notifs_adults_disag <- notifs_agesex %>%
    select(contains("1524"),
           contains("2534"),
           contains("3544"),
           contains("4554"),
           contains("5564"),
           contains("65"))

  notifs_adults_agg <-  notifs_agesex %>%
    select(contains("15plus"))

  flg_15plus_only <- ifelse(all(is.na(notifs_adults_disag)) & !all(is.na(notifs_adults_agg)),
                            TRUE,
                            FALSE)

  # Filter out unwanted data elements
  if (flg_15plus_only) {

    # remove the disaggregated adult age groups
    notifs_agesex <- notifs_agesex %>%
      select(-contains("1524"),
              -contains("2534"),
              -contains("3544"),
              -contains("4554"),
              -contains("5564"),
              -contains("65"))


  } else {

    # remove the aggregated adult age group
    notifs_agesex <- notifs_agesex %>%
      select(-contains("15plus"))

  }


  # 2. Children -- check if the 0-14 variables are not NA but the disaggregated
  # ones are NA then we only want to plot 0-14

  notifs_kids_disag <- notifs_agesex %>%
    select(contains("04"),
           contains("514"))

  notifs_kids_agg <-  notifs_agesex %>%
    select(contains("014"))

  flg_014_only <- ifelse(all(is.na(notifs_kids_disag)) & !all(is.na(notifs_kids_agg)),
                            TRUE,
                            FALSE)

  # Filter out unwanted data elements
  if (flg_014_only) {

    # remove the disaggregated adult age groups
    notifs_agesex <- notifs_agesex %>%
      select(-contains("04"),
              -contains("514"))


  } else {

    # remove the aggregated adult age group
    notifs_agesex <- notifs_agesex %>%
      select(-contains("014"))

  }



  # switch to long format
  notifs_agesex_long <- notifs_agesex %>%
    pivot_longer(cols = starts_with("newrel_"),
                 names_to = c("sex", "age_group"),
                 # thanks to Hadley, help on pivot_longer icludes
                 # and example of doing this with TB variables!
                 names_pattern = "newrel_(.)(.*)",
                 values_to = "notifs")

  # faff about withthe estimates age/sex table
  # so can match wiht notifs age/sex
  agesex <- agesex %>%
    mutate(age_group = str_remove(age_group,"-")) %>%
    mutate(age_group = ifelse(age_group=="65plus", "65", age_group))

  agesex <- agesex %>%
    inner_join(notifs_agesex_long, by = c("age_group", "sex"))


  agesex$age_group <- factor(agesex$age_group,
                             levels=c("04", "514", "014", "1524", "2534", "3544", "4554", "5564", "65", "15plus"),
                             labels=c("0-4", "5-14", "0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "\u226565", "\u226515"))


  return(agesex)


})


# Move heading and subheading out of ggplot2
# because ggplot2 headings don't wrap when space is restricted

output$agesex_chart_head <- renderText({ paste0(ltxt(plabs(), "inc"),
                                                ", ",
                                                ltxt(plabs(), "age_sex_notifs"),
                                                ", ",
                                                dcyear - 1)  })

output$agesex_chart_subhead <- renderText({ paste0("(", ltxt(plabs(), "number"), ")") })



output$agesex_chart <-  renderPlot({


  # Create quick function ss that the chart axis shows
  # absolute numbers with space separators
  abs_rounder <- function(x){
    rounder(abs(x))
  }


  agesex_data() %>%
    # Multiply all the female numbers by -1
    mutate(best = ifelse(sex=="f", best * -1, best ),
           notifs = ifelse(sex=="f", notifs * -1, notifs )) %>%

    ggplot() +

    geom_bar(aes(x=age_group, y=best, fill="inc"),
             stat="identity",
             size=.3,
             colour="black",
             position="identity") +

    # USe space separators to label large numbers and ignore minus sign
    scale_y_continuous(labels = abs_rounder) +
    scale_fill_manual(breaks = c("f", "m", "inc"),
                      values=agesex_palette(),
                      labels = c("f" = ltxt(plabs(), "female"),
                                 "m" = ltxt(plabs(), "male"),
                                 "inc" = ltxt(plabs(), "inc"))) +

    geom_bar(aes(x=age_group, y=notifs,fill=sex),
             stat="identity",
             width = 0.5,
             size=.3,
             position="identity") +

    coord_flip() +

    profile_theme()


})

