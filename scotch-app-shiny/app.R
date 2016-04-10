library(shiny)
library(rio)
library(DT)
library(xlsx)
library(hash)
library(dplyr)
library(vegan)
library(d3heatmap)

scotch_data <- import("data/scotch_data_cleaned.csv")
head(scotch_data)
scotch_data[,1] <- NULL
scotch.dist <- dist(scotch_data[,2:69], "binary")
scotch.dist <- as.matrix(scotch.dist, labels=TRUE)
colnames(scotch.dist) <- rownames(scotch.dist) <- scotch_data[,1]
rownames(scotch_data) <- scotch_data[,1]
head(scotch.dist)

hc <- hclust(dist(scotch_data[,2:69], "binary"))
plot(hc)
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

rownames(closestScotch("Bunnahabha"))
scotch_data[grep("Bunnahabha", rownames(scotch_data)),]

scotchDistClosest <- function(scotch_name) {
  dist_frame <- as.data.frame(matrix(0, ncol = 69, nrow = 0))
  scotch_closest_list <- closestScotch(scotch_name)
  rbind(dist_frame, scotch_data[grep(scotch_name, rownames(scotch_data)),]) -> dist_frame
  for (i in 1:length(rownames(scotch_closest_list))) {
    rbind(m, scotch_data[grep(rownames(scotch_closest_list)[i], rownames(scotch_data)),]) -> dist_frame[i,]
  }
  colnames(dist_frame) <- colnames(scotch_data)
  
  dist_frame
}


d3heatmap(dist(scotchDistClosest("Bunnahabha"), "binary"), dendrogram = "row", colors="YlOrRd")
help(d3heatmap)

rbind(scotchDistClosest("Bunnahabha"), b)



# Using Jaccard method
if (FALSE) {
scotch.dist2 <- vegdist(lapply(scotch_data[2:109,], as.numeric), method = "jaccard", binary = TRUE)
View(as.matrix(scotch.dist2))
scotch.dist2 <- as.matrix(scotch.dist2)
colnames(scotch.dist2) <- rownames(scotch.dist2) <- scotch_data[,1]

bunnah_vec2 <- scotch.dist2["Bunnahabha",]
bunnah_vec2 <- sort(bunnah_vec2)
bunnah_vec2[2:6]
bunnah_frame2 <- as.data.frame(bunnah_vec2[2:6])
colnames(bunnah_frame2) <- "Closest Scotches to Bunnahabha"
row.names(bunnah_frame2)

data.num <- as.numeric(unlist(scotch_data[2:109,]))
scotch_data$V2
View(data.num)
key <- read.xlsx2("scotch_key.xlsx", 1, startRow = 1)
names(key)
col.ids <- names(scotch_data)
col.ids[2:69]
hash(col.ids[2:69], names(key))
}
# figure out how to get colnames w/ 1's from dataset

ui <- fluidPage(

  h1("A Nearest-Neighbour Analysis of Whiskey", align = "center"),
  
  br(),
  # widget to choose scotch
  # output to display table and metric
  
  fluidRow(
    
    column(4, offset = 2, 
           selectInput('name', ' ', c('Choose a favourite scotch'='', scotch_data$V2), selectize=TRUE, width = '25em', selected = "Aberfeldy")
           ),

    column(4,
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
    if(!is.null(input$name)) {
      closestScotch(input$name)
    },

    options = list(searching = FALSE, paging = FALSE)

  )
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(dist(scotchDistClosest(input$name), "binary"), colors="YlOrRd")
  })
  
  
  output$tableRaw = renderDataTable({
    scotch_data
  })
}

shinyApp(ui=ui, server=server)