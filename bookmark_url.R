 # Include bookmarks in the URL -- see https://shiny.rstudio.com/articles/bookmarking-state.html
 # (include this file in server.R)
 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 observe({
     # Trigger this observer every time an input changes
     # (Get all the input parameters, including via URL)
     reactiveValuesToList(input)
     session$doBookmark()
 })

 onBookmarked(function(url) {
     # Update the URL showing in the user's browser so that the current
     # inputs can be seen and the state is bookmarkable
     updateQueryString(url)
 })
