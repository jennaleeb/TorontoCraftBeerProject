library(shiny)
library(rio)
library(DT)
library(xlsx)
library(hash)
library(dplyr)
library(vegan)
library(d3heatmap)

beer_data <- import("beer_binary_cleaned.csv")
beer_data[1] <- NULL
beer.dist <- dist(beer_data[,6:50], "binary")
beer.dist <- as.matrix(beer.dist, labels=TRUE)
colnames(beer.dist) <- rownames(beer.dist) <- beer_data[,2]
rownames(beer_data) <- beer_data[,2]

closestBeer <- function(beer_name) {
  beer_vec <- beer.dist[beer_name,]
  beer_vec <- sort(beer_vec)
  beer_frame <- as.data.frame(beer_vec[2:6])
  colnames(beer_frame) <- paste("Euclidean Distance to", beer_name)
  round(beer_frame,3)
}

closestBeer("Nut Brown")

# To make heat map?
beerDistClosest <- function(beer_name) {
  # make some empty matrices
  dist_frame <- as.data.frame(matrix(0, ncol = 50, nrow = 0))
  m <- as.data.frame(matrix(0, ncol = 50, nrow = 0))
  
  # call the function to find the closest scotches
  beer_closest_list <- closestBeer(beer_name)
  rbind(dist_frame, beer_data[grep(beer_name, rownames(beer_name)),]) -> dist_frame
  
  # make a new frame
  for (i in 1:length(rownames(beer_closest_list))) {
    rbind(m, beer_data[grep(rownames(beer_closest_list)[i], rownames(beer_data)),]) -> dist_frame[i,]
  }
  colnames(dist_frame) <- colnames(beer_data)
  
  dist_frame
}

ui <- fluidPage(
  
  includeCSS("main.css"),
  tags$link(
    rel = "stylesheet",
    href = 'https://fonts.googleapis.com/css?family=Raleway:400,100,700'
  ),
  
  h1("An Analysis of Ontario Craft Beer", align = "center"),
  
  br(),
  # widget to choose scotch
  # output to display table and metric
  
  fluidRow(
    
    column(8, offset = 2, 
           selectInput('beer_selector_main', ' ', c('Choose a favourite beer'='', beer_data$`Beer Name`), selectize=TRUE, selected = "Nut Brown")
    )
  ),
  
  fluidRow(
    column(8, offset = 2,
           dataTableOutput('tableNeigbour')
    )
  ),
  
  br(),
  
  fluidRow(
    column(8, offset = 3,
           d3heatmapOutput("heatmap", width = "80%", height = "400px")
    )
  ),
  
  br(),
  
  h2("Raw Data"),
  dataTableOutput('tableRaw')
  
)

server <- function(input, output){
  output$tableNeigbour = renderDataTable(
    if(!is.null(input$beer_selector_main)) {
      closestBeer(input$beer_selector_main)
    },
    
    options = list(searching = FALSE, paging = FALSE)
    
  )
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(dist(beerDistClosest(input$beer_selector_main), "binary"), colors="YlOrRd")
  })
  
  
  output$tableRaw = renderDataTable({
    beer_data
  })
}

shinyApp(ui=ui, server=server)