library(shiny)
library(rio)

scotch_data <- import("data/scotch_data_cleaned.csv")
scotch_data[,1] <- NULL
scotch.dist <- dist(scotch_data[,2:69], "binary")
scotch.dist <- as.matrix(scotch.dist, labels=TRUE)
colnames(scotch.dist) <- rownames(scotch.dist) <- scotch_data[,1]
head(scotch.dist)
# find the 5 most similar whiskeys to Bunnah
bunnah_vec <- scotch.dist[20,]
bunnah_vec <- sort(bunnah_vec)
bunnah_vec[2:6]
bunnah_frame <- as.data.frame(bunnah_vec[2:6])
scotch_data.name <- scotch_data$V2
ui <- fluidPage(
  h1("A Nearest-Neighbour Analysis of Whiskey"),
  
  # widget to choose scotch
  # output to display table and metric
  
  h2("Nearest-Neighbour"),
  fluidRow(
    column(4,
           selectInput('name', 'Choose a favourite scotch', c(Choose='', scotch_data.name), selectize=TRUE)),
    
    column(1,
           actionButton('choose', "Go")
    ),
    
    
    
    column(4,
           dataTableOutput('tableNeigbour'))
    ),
    
  
  
  
  
  h2("Raw Data"),
  dataTableOutput('tableRaw')
  
)

server <- function(input, output){
  output$tableNeigbour = renderDataTable(
    bunnah_frame,
    options = list(searching = FALSE, paging = FALSE)
  )
  
  output$tableRaw = renderDataTable({
    scotch_data
  })
}

shinyApp(ui=ui, server=server)