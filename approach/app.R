library(shiny)
library(tmap)
library(dplyr)
library(DT)

product_list<-readRDS("data/rds/product_list.rds")
product_order_reviews_location<-readRDS("data/rds/product_order_reviews_location.rds")

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".my-title-panel { background-color: lightblue; }")
    )
  ),
  div(class = "my-title-panel",
      titlePanel("Indicator Performance across States for Specific Products")
  ),
  
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      selectInput("indicator", label="pick an indicator", choices=c("no_orders", "ave_price","total_price"),selected="no_orders", multiple=FALSE),
      selectInput("product", label="pick a product category", choices=product_list, selected="cool_stuff", multiple=FALSE),
      helpText("Select an option from the dropdown menu")),
    # Main Panel
    mainPanel(h3("Tmap plot and Interactive Data Table"),tmapOutput("my_map"), DT::dataTableOutput(outputId = "my_table"))
  )
)

# Define the server
server <- function(input, output) {
  dataset<-reactive({
    product_order_reviews_location |>
      filter(product_category_name_english==input$product) |> 
      select(ADM1_EN, input$indicator, geometry)
  })
  # Render the tmap in the output element
  output$my_map <- renderTmap({
    # Create the tmap
    tm_shape(shp=dataset())+
      tm_fill(input$indicator,
              style="quantile",
              palette="Blues")+
      tm_borders(lwd = 1, col = "black")
  })
  
  output$my_table<-DT::renderDataTable({
    DT::datatable(data=select(dataset(),-geometry))
  })
}

# Run the app
shinyApp(ui, server)