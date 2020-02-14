# Simple reusable function to return label text from a label tag in a dataframe with
# columns called label_tag and label_text. Note this is a 'non-reactive' function.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ltxt <- function(df, tag){

    df[ df$label_tag==tag , "label_text"]

}

# Simple rounding function that returns a string rounded to the nearest integer and
# uses a space as the thousands separator as per WHO standard.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

rounder <- function(x) {

    ifelse(is.na(x), NA,
           formatC(round(x,0), big.mark=" ", format="d")
           )
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


# Non-reactive functions to build estimate strings
# as best (lo-hi)
format_estimate <- function(best, lo, hi, style="n"){

    if (style=="k"){
        # format number using spaces for thousands separator

        return(paste0(rounder(best), " (", rounder(lo), "-", rounder(hi), ")"))

    } else if (style=="%") {
        # append percent sign to best
        return(paste0(best, "% (", lo, "-", hi, ")"))

    } else {
        # keep numbers as they are

        return(paste0(best, " (", lo, "-", hi, ")"))
    }

}


# Replace a missing data column with zero
NZ <- function(df, cname){

      ifelse(cname %in% colnames(df),
             df[, cname],
             0)
}
