# Scotch whiskey example
library(rio)
library(dplyr)
library(knitr)
library(ggplot2)

install.packages("clusteval")
library(clusteval)

scotch_data <- import("Scotch_XL3.0.xlsx")
head(scotch_data)
# delete first row
scotch_data <- scotch_data[-1,]
# delete first column
scotch_data <- scotch_data[,-1]
ncol(scotch_data)
# only keep tasting notes
scotch_data <- scotch_data[,1:69]
# delete last 2 rows that don't have stuff
scotch_data <- scotch_data[1:109,] 

write.csv(scotch_data, "scotch_data_cleaned.csv")

bunnah <- as.numeric(as.vector(scotch_data[20,2:69]))
ardbeg <- as.numeric(as.vector(scotch_data[3,2:69]))
glenglas <- as.numeric(as.vector(scotch_data[49,2:69]))
tulli <- as.numeric(as.vector(scotch_data[109,2:69]))

# This gives me the distance function in the book!! Closer value to 0 = more similar
dist(t(cbind(unlist(bunnah), unlist(ardbeg))), "binary")

# These do the same thing
dist(t(cbind(as.numeric(as.vector(scotch_data[20,2:69])), as.numeric(as.vector(scotch_data[3,2:69])))), "binary")
dist(t(cbind(unlist(scotch_data[20,2:69]), unlist(scotch_data[3,2:69]))), "binary")

nrow(scotch_data)

distanceMetric <- function(rowNum, data){
  results <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    scotch_name <- data[i,1]
    distMet <- dist(t(cbind(unlist(data[rowNum-1,2:69]), unlist(data[i,2:69]))), "binary")
    results[i] <- distMet
    results <- sort(results)
  }
  return(results)
}

test_bunnah <- distanceMetric(21, scotch_data)

apply(scotch_data, 1, distanceMetric)

scotch_data[1,1]
help(dist)

n = c(2, 3, 5) 
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE) 
df = data.frame(n, s, b)

name <- scotch_data[1,1]
dist <- dist(t(cbind(unlist(scotch_data[20,2:69]), unlist(scotch_data[1,2:69]))), "binary")
data.frame(name,1)
newdataframe[name, dist]

# Ok this makes the nice matrix!! (using different methods (binary/eucl..) seems to be the same)

scotch_data <- import("scotch_data_cleaned.csv")
scotch_data[,1] <- NULL
scotch.dist <- dist(scotch_data[,2:69], "binary")
scotch.dist <- as.matrix(scotch.dist, labels=TRUE)
colnames(scotch.dist) <- rownames(scotch.dist) <- scotch_data[,1]

# find the 5 most similar whiskeys to Bunnah
bunnah_vec <- scotch.dist[20,]
bunnah_vec <- sort(bunnah_vec)
bunnah_vec[2:6]

# Make a heat map
dim <- ncol(scotch.dist)
image(1:dim, 1:dim, scotch.dist, axes = FALSE, xlab="", ylab="")

axis(1, 1:dim, scotch_data[1:109,1], cex.axis = 0.5, las=3)
axis(2, 1:dim, scotch_data[1:109,1], cex.axis = 0.5, las=1)

text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", scotch.dist), cex=0.6)

# another approach
install.packages("d3heatmap")
library(d3heatmap)
d3heatmap(scotch.dist, scale = "column", dendrogram = "none", color = "Blues")
