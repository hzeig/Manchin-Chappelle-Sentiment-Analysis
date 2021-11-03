library(shiny)

## chill practice space
dc_full <- read.csv("dc_full.csv")
jm_full <- read.csv("jm_full.csv")

p <- ggplot() +
  geom_bar(aes(x = dc_full$subreddit, y = dc_full$thread.score), stat = 'identity') +
  geom_bar(aes(x = jm_full$subreddit, y = jm_full$thread.score), stat = 'identity')



ui <- fluidPage(
  # selectInput("text", "Text Type", choices = "")
  checkboxGroupInput("text", "Text type:", choices = list("title", "post", "comment"), selected = list("title", "post", "comment")),
  uiOutput("comparison")
)




server <- function(input, output) {
  output$comparison <- renderText({
    select(dc_full, input$text)
  })
}
  

# Run the application 
shinyApp(ui, server)
