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
x_mod <- x %>% filter(time >= 50)

kinetics_model <- function(df) {
  lm(abs ~ time, data = df)}

x_nest <- x_mod %>% group_by(data_num) %>% 
  nest()

x_nest <- x_nest %>% mutate(model = map(data, kinetics_model))
x_nest <-  x_nest %>% mutate(resids = map2(data, model, add_residuals),
                             pred = map2(data, model, add_predictions))


resids <- unnest(x_nest, resids) %>% select(data_num, time, abs, resid)
pred <- unnest(x_nest, pred) %>% select(data_num, time, abs, pred)

# Residue Plot
resid_plot <- ggplot(resids, aes(time,resid))+
  geom_hline(yintercept = 0, col = "red")+
  geom_point(alpha = 0.3)+
  facet_wrap(vars(data_num), nrow = 2)+
  theme_bw()+
  labs(title = "Residue plot for data")

#Plotting model
data_plot <- ggplot(x, aes(x = time))+
  geom_point(aes(y=abs), alpha = 0.3)+
  facet_wrap(vars(data_num), nrow = 2)+
  theme_bw()+
  geom_line(aes(y=pred), data = pred, col = "red")

data_plot
resid_plot



