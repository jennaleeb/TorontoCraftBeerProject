library(shiny)
library(rio)
library(DT)
library(xlsx)
library(hash)
library(dplyr)

scotch_data <- import("data/scotch_data_cleaned.csv")
scotch_data[,1] <- NULL
scotch.dist <- dist(scotch_data[,2:69], "binary")
scotch.dist <- as.matrix(scotch.dist, labels=TRUE)
colnames(scotch.dist) <- rownames(scotch.dist) <- scotch_data[,1]
head(scotch.dist)
# find the 5 most similar whiskeys to Bunnah

bunnah_vec <- scotch.dist["Bunnahabha",]
bunnah_vec <- sort(bunnah_vec)
bunnah_vec[2:6]
bunnah_frame <- as.data.frame(bunnah_vec[2:6])
colnames(bunnah_frame) <- "Closest Scotches to Bunnahabha"
row.names(bunnah_frame)

# Make a function

closestScotch <- function(scotch_name) {
  scotch_vec <- scotch.dist[scotch_name,]
  scotch_vec <- sort(scotch_vec)
  scotch_frame <- as.data.frame(scotch_vec[2:6])
  colnames(scotch_frame) <- paste("Distance to", scotch_name)
  round(scotch_frame,3)
}

key <- read.xlsx2("scotch_key.xlsx", 1, startRow = 1)
names(key)
col.ids <- names(scotch_data)
col.ids[2:69]
hash(col.ids[2:69], names(key))

# figure out how to get colnames w/ 1's from dataset

ui <- fluidPage(

  h1("A Nearest-Neighbour Analysis of Whiskey", align = "center"),
  
  # widget to choose scotch
  # output to display table and metric
  
  fluidRow(
    column(8, offset = 4,
           selectInput('name', ' ', c('Choose a favourite scotch'='', scotch_data.name), selectize=TRUE, width = '25em')
           )
    ),
  
  fluidRow(
    column(4, offset = 2,
           dataTableOutput('tableNeigbour')), 
    
    column(6,
           dataTableOutput('tableNeigbourr'))
  ),
  
  h2("Raw Data"),
  dataTableOutput('tableRaw')
  
)

server <- function(input, output){
  output$tableNeigbour = renderDataTable(
    if(!is.null(input$name)) {
      closestScotch(input$name)
    },

    options = list(searching = FALSE, paging = FALSE)

  )
  
  
  output$tableRaw = renderDataTable({
    scotch_data
  })
}

shinyApp(ui=ui, server=server)