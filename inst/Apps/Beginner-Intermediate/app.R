# Layout Template from: https://shiny.rstudio.com/gallery/datatables-demo.html

library(shiny)
library(ggplot2)  # for the diamonds dataset
library(gapminder)
library(dplyr)

ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(width=3,
      p("Put Something Here"),
      h2("Challenge:"),
      h4("Uphold the GapMinder Mission:"),
      p("The object of the foundation shall be achieved by:"),
      tags$ol(
      tags$li(strong("use and development of information technology for easily understandable visualization of statistics and other information;")),
      tags$li("ownership, protection and free dissemination of the development results;"),
      tags$li("use, together with various cooperation partners, of the development results with a view to making statistics and other information about development available and understandable to broad user groups via the Internet and other media.")
      ),
      p("https://www.gapminder.org"),
      p("https://www.gapminder.org/ignorance"),
      br(),

      h4("Ideas:"),
      tags$ul(
        tags$li("Make existing charts interactive, everybody likes interactive."),
        tags$li("Make a new interactive ggplot scatter plot."),
        tags$li("When you hover over a data point, descriptive text pops up. (ggplot & shiny, or plotly)"),
        tags$li("Have summary stats appear below scatterplot about selected (brushed) datapoints.")


      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Gapmider Data", DT::dataTableOutput("mytable1")),
        tabPanel("ggplot2 facet_wrap", mainPanel(plotOutput("faceted",width = "1100px", height = "750px"))),
        tabPanel("Violin Plot",( plotOutput( "violins")))
      )
    )
  )
)

server <- function(input, output) {

  # choose columns to display
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(gapminder::gapminder)
  })

  # sorted columns are colored now because CSS are attached to them
  output$faceted <- renderPlot({
    gapminder %>%
      ggplot(aes(x = gdpPercap, fill = continent)) +
      facet_wrap( ~ year) +
      scale_x_log10() +
      geom_density(alpha = 0.6)   })

  # customize the length drop-down menu; display 5 rows per page by default
  output$violins <- renderPlot({

    ggplot(gapminder %>%  filter(year==1982)) +
      geom_violin(aes(x = continent, y = lifeExp, fill = continent))+
      ggtitle("Violin Plot of Continent vs Life Expectancy")

      })

}

shinyApp(ui, server)
