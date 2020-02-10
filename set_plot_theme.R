# Graph theme components ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

profile_theme <- function(base_size=10, base_family="") {

  gray <- "#4D4D4D"
  black <- "#000000"


  theme_bw(base_size=base_size, base_family=base_family) +
    theme(
      line = element_line(colour = gray),
      rect = element_rect(fill = "white", colour = NA),
      text = element_text(colour = black),
      axis.ticks.x = element_line(colour = gray),
      axis.ticks.y = element_blank(),
      legend.key = element_rect(colour = NA),
      ## Examples do not use grid lines
      panel.border = element_rect(colour = gray),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill="white", colour=NA),
      strip.text = element_text(hjust=0),
      strip.text.x = element_text(size=10),
      plot.title = element_text(hjust=0)
    )
}



# Standard colours used by Sue

standard_palette <- function(type){

  switch(type,
        "incidence" = "#91A93E",        # green
        "tbhiv_incidence" = "#ED1D24",   # pinky red
        "mortality_exc_tbhiv" = "#1D91D1", # light blue
        "art" = "#0091D1"  # blue
  )

}
