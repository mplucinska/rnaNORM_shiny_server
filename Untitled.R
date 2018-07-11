library(shiny)
library(plotly)

ui <- bootstrapPage(
  plotlyOutput('plot')
)

# Define the server code
server <- function(input, output) {
  str_ex <- read_delim("~/Desktop/str_ex.", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
  str_ex <- str_ex[order(str_ex$X3),] 
  output$plot <- renderPlotly({
    ay <-list(
      #zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    ax <-list(
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    p <- plot_ly(data = str_ex, x = ~X1, y = ~X4,   mode = 'markers',
                     symbol = ~X3, 
                     symbols = c("triangle-right", "circle", "triangle-left" ),
                     marker = list(size = c(10,2,10))
    )  %>%
    layout(
      xaxis = ax,
      yaxis = ay
    )
    #add_markers(error_x = ~list(value = std.error))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
