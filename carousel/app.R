library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      carousel(
        id = "mycarousel",
        carouselItem(
          caption = "Item 1",
          tags$img(src = "https://placehold.it/900x500/3c8dbc/ffffff&text=I+Love+Bootstrap")
        ),
        carouselItem(
          caption = "Item 2",
          tags$img(src = "https://placehold.it/900x500/39CCCC/ffffff&text=I+Love+Bootstrap")
        )
      )
    ),
    title = "Carousel"
  ),
  server = function(input, output) { }
)