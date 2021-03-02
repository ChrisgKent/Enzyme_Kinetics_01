library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(modelr)


data <- paste0("data", 1:4)

# Provide this function with a vector of names of the files to be read
reading_data <- function(data){
  for(j in data){
    i <- read_csv(paste0("data/part_3/",j, ".csv"))
    return(i)
  }
}
# Applies the data vector to the reading function. 
# Then turns the list into a single dataframe, effectivly using an rbind()
x <- sapply(data, simplify = FALSE, reading_data) %>% ldply() 
names(x) <- c("data_num", "time", "abs")

# Generates a function which creates models for the data.
kinetics_model <- function(df) {
  lm(abs ~ time, data = x[x$time >= 50,])}


x_nest <- x %>% group_by(data_num) %>% 
  nest()
x_nest <- x_nest %>% mutate(model = map(data, kinetics_model))
x_nest <-  x_nest %>% mutate(resids = map2(data, model, add_residuals),
                             pred = map2(data, model, add_predictions))







#Testing polts
x %>% group_by(data_num) %>% filter(time <= 200) %>% ggplot(aes(time,abs))+
  geom_point(alpha = 0.6)+
  theme_bw()+
  facet_wrap(vars(data_num), nrow = 2)+
  geom_smooth(method = "lm", 
              data = filter(x,time %in% 50:200), 
              alpha = 0.3, col = "red")



