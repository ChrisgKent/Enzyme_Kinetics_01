library(tidyverse)

data <- paste0("data", 1:4)

# Provide this function with a vector of names of the files to be read
reading_data <- function(data){
  for(j in data){
    i <- read_csv(paste0("data/part_3/",j, ".csv"))
    assign(x = j, value = i, envir = parent.frame())
  }
}

reading_data(data)