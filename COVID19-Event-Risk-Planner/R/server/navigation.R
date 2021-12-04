observe({
  query = getQueryString()
  params = names(query)
  if ("global" %in% params) {
    updateNavbarPage(session, "nav-page", "global")
  } else if (any(c("game", "quiz") %in% params)) {
    updateNavbarPage(session, "nav-page", "game")
  }
  ref_tags = c("src", "utm_source", "utm_medium", "utm_content", "utm_campaign")
  ref_content <<- list()
  for (tag in ref_tags){
    ref_content[[tag]] <<- ifelse(tag %in% params, query[[tag]], "NULL")
  }
})



observeEvent(input$to_usa, {
  updateTabsetPanel(session, "nav-page", "usa")
})
observeEvent(input$to_global, {
  updateTabsetPanel(session, "nav-page", "global")
})
observeEvent(input$to_data, {
  updateTabsetPanel(session, "nav-page", "about")
  updateTabsetPanel(session, "abouttabs", "data")

})
observeEvent(input$to_data_global, {
  updateTabsetPanel(session, "nav-page", "about")
  updateTabsetPanel(session, "abouttabs", "data")

})

observeEvent(input$to_game, {
  updateNavbarPage(session, "nav-page", "game")
})
