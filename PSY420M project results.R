library(tidyverse)

data <- read_csv("labs/project/isobel.csv")
data

# frequency plot
ggplot (data, aes(x = value, color = group)) + 
  geom_freqpoly(binwidth = 1) +
  ggtitle("Frequency Plot of Drinking Habits for Married Men with Kids and Single Men without Kids") +
  xlab("# of days per week an alcholic beverage consumed") +
  ylab("count") 

# boxplot
ggplot (data, aes(x = group, y = value)) + geom_boxplot() + 
  ggtitle("Impact of Marital and Parental Status of Men on Drinking Habits") +
  xlab("marital and parental status of men") + 
  ylab("# of days per week an alcholic beverage consumed")

# unpaired/independent samples t-test
t.test(data = data, value ~ group)

# SE
datasum <- data %>% group_by(group) %>% 
  summarize(datacounts = n(), datameans = mean(value), dataSDs = sd(value)) %>%
  mutate(datases = dataSDs/sqrt(datacounts)) %>% mutate(dataCIs = 1.96*datases)
datasum

