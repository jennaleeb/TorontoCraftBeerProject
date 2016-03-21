library(rio)
library(dplyr)
library(knitr)
library(ggplot2)
library(clusteval)
library(Hmisc)

beer_data <- import("BeerSurveyResponses_Test.csv")

# use model.matrix to expand factors into binaries
cbind(iris, model.matrix(~ Species + 0, data=iris))

beer_data$Taste

beer_data %>%
cbind(., with(., model.matrix(~ Colour + 0)),
                  with(., model.matrix(~ Opacity + 0)),
                  with(., model.matrix(~ Taste + 0)))



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

beer_data <- multiToBinary(beer_data, "taste", 6)
beer_data <- multiToBinary(beer_data, "body", 7)



