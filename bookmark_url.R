 # Include bookmarks in the URL -- see https://shiny.rstudio.com/articles/bookmarking-state.html
 # (include this file in server.R)
 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

 observe({
         # Trigger this observer every time an input changes
         # (Get all the input parameters, including via URL)
         reactiveValuesToList(input)

         # Avoid cluttering URL with inconsistent codes
         # so drop group_code or iso2 depending on the
         # selected entity_type
         if (check_entity_type(input$entity_type) != "group") {
                 setBookmarkExclude("group_code")
         } else {
                 setBookmarkExclude("iso2")
         }

         session$doBookmark()
 })

 onBookmarked(function(url) {
         # Update the URL showing in the user's browser so that the current
         # inputs can be seen and the state is bookmarkable
         updateQueryString(url)
 })
