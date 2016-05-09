library(rio)
library(dplyr)
library(knitr)
library(ggplot2)
library(clusteval)
library(Hmisc)

# Purpose of this file is to take the survey responses and make a data set of binary values for each beer
beer_data <- import("Beer Survey (Responses).csv")



# Multiple factors --> binaries
# Remove white spaces and get unique levels
taste.lev <- unique(gsub(" ", "", unlist(strsplit(beer_data$Taste, ",")), fixed = TRUE))
mnames <- gsub(" ", "_", paste("taste", taste.lev, sep = "."))
result <- matrix(data = "0", nrow = length(beer_data$Taste), ncol = length(taste.lev))
char.var <- as.character(beer_data$Taste)
for (i in 1:length(taste.lev)) {
  result[grep(taste.lev[i], char.var, fixed = TRUE), i] <- "1"
}
result <- data.frame(result, stringsAsFactors = TRUE)
colnames(result) <- mnames
beer_data <- cbind(beer_data,result)
View(beer_data)

# turning it into a function
multiToBinary <- function(data, attr_name, colnum) {
  col <- data[,colnum]
  lev <- unique(gsub(" ", "", unlist(strsplit(col, ",")), fixed = TRUE))
  mnames <- gsub(" ", "_", paste(toString(attr_name), lev, sep = "."))
  result <- matrix(data = "0", nrow = length(col), ncol = length(lev))
  char.var <- as.character(col)
  for (i in 1:length(lev)) {
    result[grep(lev[i], char.var, fixed = TRUE), i] <- "1"
  }
  result <- data.frame(result, stringsAsFactors = TRUE)
  colnames(result) <- mnames
  data <- cbind(data,result)
}

# import raw response data and call these functions
beer_data <- multiToBinary(beer_data, "taste", 6)
beer_data <- multiToBinary(beer_data, "body", 7)
beer_data <- multiToBinary(beer_data, "opacity", 5)
colnames(beer_data)

# automate in a loop (all you need is original data, multiToBinary function and this
feature_names <- cbind("Opacity" = 4, "Taste" = 5, "Body" = 6, "Carbonation" = 7, "Colour" = 11)

for (i in 1:length(feature_names)) {
  beer_data <- multiToBinary(beer_data, colnames(feature_names)[i], feature_names[i])
}

# cleanup
beer_data$Timestamp <- NULL
View(beer_data)
beer_data$Taste.WellBalanced <- NULL

write.csv(beer_data, "beer_binary_cleaned.csv")
