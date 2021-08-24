about_tabset= tabPanel(
  value = "about",
  "About",
  fluid = TRUE,
  tabsetPanel(
    id="abouttabs",
    tabPanel(
      value = "Aboutcontent",
      "About",
      fluid = TRUE,
      mainPanel(includeMarkdown("About.md"))
    ),
    tabPanel(
      value = "press",
      "Press",
      fluid = TRUE,
      mainPanel(includeMarkdown("Press.md"))
    ),
    tabPanel(
      value = "data",
      "Data source",
      fluid = TRUE,
      mainPanel(includeMarkdown("Data.md"))
    ),
    tabPanel(
      value = "previous",
      "Previously Released Charts",
      fluid = TRUE,
      mainPanel(
        tags$img(src = "twitter_image_031020.jpg"),
        tags$br(),
        tags$br(),
        tags$img(src = "figevent_checker_apr30.png"),
        tags$br(),
        tags$br(),
        tags$img(src = "figevent_checker_georgia_042720.jpg  ")
      )
    )
  )
)