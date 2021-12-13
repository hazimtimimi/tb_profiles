# Graph theme components ----
# Based on the BBC ggplot2 theme at https://github.com/bbc/bbplot/blob/master/R/bbc_style.R
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

profile_theme <- function(base_size=10, base_family="") {

  gray <- "#BCBCBC"
  charcoal <- "#222222"


  theme(

  #Text format:
  plot.title = element_text(family=base_family,
                            #size=28,
                            #face="bold",
                            color=charcoal),
  plot.subtitle = element_text(family=base_family,
                               #size=18,
                               margin=ggplot2::margin(9,0,9,0)),

  plot.margin = margin(0,20,30,0),

  #Legend format
  legend.position = "top",
  legend.text.align = 0,
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(family=base_family,
                             size=14,
                             color=charcoal),

  #Axis format
  axis.title = element_blank(),
  axis.text = element_text(family=base_family,
                           size=12,
                           color=charcoal),
  axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 5, 10, 5)),
  axis.ticks = ggplot2::element_blank(),
  axis.line = ggplot2::element_blank(),

  #Grid lines
  #This removes all minor gridlines and adds major y gridlines.
  panel.grid.minor = ggplot2::element_blank(),
  panel.grid.major.y = ggplot2::element_line(color=gray,
                                             size = 0.25),
  panel.grid.major.x = ggplot2::element_blank(),

  #Blank background
  panel.background = ggplot2::element_blank(),

  )

}


# Standard colours used in the Global TB Report
# - - - - - - - - - - - - - - - - - - - - - - -
#
standard_palette <- function(type){

  switch(type,
        "incidence" = "#91A93E",        # green
        "tbhiv_incidence" = "#ED1D24",   # pinky red
        "mortality_exc_tbhiv" = "#1D91D1", # light blue
        "art" = "#0091D1"  # blue
  )

}

budget_palette <- function(){

  c("a_domestic"      =  "#7DAAD4",  # funded domaestically (blue)
    "b_international" =  "#E9C04F",  # Funded internationally (orange)
    "c_gap"           =  "#D84D3F"   # Unfunded (red)
    )

}


agesex_palette <- function() {

 c("inc" = "#EEEEEE", # incidence (almost white)
   "f" = "#732969",  # Females (Purple used in 2020 global report)
   "m" = "#8BC63F"   # Males   (Green used in 2020 global report)
 )

}
