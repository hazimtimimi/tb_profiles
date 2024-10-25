# Simple reusable function to return label text from a label tag in a dataframe with
# columns called label_tag and label_text. Note this is a 'non-reactive' function.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ltxt <- function(df, tag){

    df[ df$label_tag==tag , "label_text"]

}

# Look for a URL string and convert to an HTML link
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
html_link <- function(x) {

  sub('(https?://\\S+)',
      "<a href='\\1' target='_blank'>\\1</a>",
      x,
      ignore.case = TRUE)

}

# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
}

# display a percentage to 2 sig figs
rounder_pct <- function(x) {

    ifelse(is.na(x), "",
           paste0(signif(x, 2), "%")
           )
}

# display millions
rounder_mil <- function(x) {

    ifelse(is.na(x), "",
           ifelse(x > 0 & x < 10,
                  signif(x, 2),
                  rounder(x)))
}


# Replace an NA value with zero
NZ <- function(x){
  ifelse(is.na(x), 0, x)
}



# Calculate a sum when one or more of the numbers could be NA
# Return NA only if both are NA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sum_with_nulls <- function(x, y){

  ifelse(is.na(x) & is.na(y),
         NA,
         NZ(x) + NZ(y))

}

# Calculate % using numerator and denominator, format the output and cap at 100%
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

display_cap_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | is.na(denominator) | denominator == 0, "",
         ifelse(numerator > denominator, ">100%", paste0(signif(numerator * 100 / denominator, 2), "%")))

  return(pct)
}

# Calculate % using numerator and denominator, cap at 100, no formating
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

get_cap_pct <- function(numerator, denominator) {

  pct <- ifelse(is.na(numerator) | is.na(denominator) | denominator == 0, "",
         ifelse(numerator > denominator, 100, signif(numerator * 100 / denominator, 2)))

  return(pct)
}

# Calculate % change and describe it as increase or decrease
pct_change_description <- function(x1, x2, s_reduction, s_increase){

  if (NZ(x1)==0 | is.na(x2)) {
    # return an empty string
    return("")

  } else if (x1 == x2){
    return("0%")

  } else if (x2 < x1){
    return(stringr::str_replace(s_reduction, "%s0", paste0(signif(abs(x2 - x1) * 100/x1,2), "%")))

  } else if (x1 < x2){
    return(stringr::str_replace(s_increase, "%s0", paste0(signif((x2 - x1) * 100/x1,2), "%")))

  }

}

# Non-reactive functions to build estimate strings
# as best (lo-hi)
format_estimate <- function(best, lo, hi, style="n"){

    # Need this very first line so as not to trigger an error when
    # the application first loads ... annoying
    if (length(best) == 0 ){
        # return an empty string
        return("")

    } else if (is.na(best)){
        # return an empty string
        return("")

    } else if (style=="k"){
        # format number using spaces for thousands separator
        return(paste0(rounder(best), " (", rounder(lo), "–", rounder(hi), ")"))

    } else if (style=="%") {
        # append percent sign to best
        return(paste0(best, "% (", lo, "–", hi, ")"))

    } else {
        # keep numbers as they are
        return(paste0(best, " (", lo, "–", hi, ")"))
    }

}


# Validate inputs received via the URL: if an invalid choice is made, Shiny sets the input value to null
# So convert to a defaultvalue

# Replace a null entity_type with "country"   -- although invalid iso2 code doesn't seem to trigger an errors
check_entity_type <- function(x){
  ifelse(is.null(x), "country", x)
}

# Replace a null language value with "EN"
check_lan <- function(x){
  ifelse(is.null(x), "EN", x)
}

# Replace a null group_code with "AFR"
check_group_code <- function(x){
  ifelse(is.null(x), "AFR", x)
}
