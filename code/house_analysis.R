#House's Debate Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")
#install.packages("igraph")

#Load the package 
library(stm)
library(igraph)
library(ggplot2)
library(dplyr)

set.seed(2008)
#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")
First_STM <- readRDS(file = "data/First_STM_house.rds")
prep <- readRDS(file = "data/prep_house.rds")
processed <- readRDS(file = "data/processed_house.rds")

#Now we separate our data into neat bits for our analysis 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
meta$year <- as.integer(meta$year)

topic_number = c(5,8,19,18,10,2,14,13,4,7)
topic_labels = c("Topic 5 – Fiscal Policy", 
                  "Topic 8 – Technology regulations", 
                  "Topic 19 – Balanced budget", 
                  "Topic 18 – Defense (?)", 
                  "Topic 10 – Budget technicalities", 
                  "Topic 2 – Social Security and Medicare", 
                  "Topic 14 – Education and Veterans", 
                  "Topic 13 – Corporate taxation", 
                  "Topic 4 – Government Shutdown", 
                  "Topic 7 – Energy sector" )

#INPUT HERE: Now we plot the effect of CRISIS, a categorical variable 
Sys.setlocale(locale="en_US.UTF-8")
plot(prep, 
     covariate = "crisis", #NOTE: Write the variable you want to assess
     topics = topic_number,#NOTE: Write the topics you want to test
     model = First_STM, method = "difference", 
     cov.value1 = 'crisis', #NOTE:Name category 1 in your variable that you want to assess
     cov.value2 = 'non-crisis', #NOTE:Name category 2 in your variable that will be your comparison
     xlab = "Non-Crisis ... Crisis", #NOTE: Write what you want on the x axis
     main = "Effect of Non-Crisis vs. Crisis", #NOTE: Write what you want on the main title
     xlim = c(-.25, .25),
     labeltype = "custom", 
     custom.labels = topic_labels
     ) #NOTE: Insert custom theme names

#INPUT HERE: Now we plot the effect of PARTY, a categorical variable 
plot(prep, 
     covariate = "party", #NOTE: Write the variable you want to assess
     topics = topic_number,#NOTE: Write the topics you want to test
     model = First_STM, method = "difference", 
     cov.value1 = 'R', #NOTE:Name category 1 in your variable that you want to assess
     cov.value2 = 'D', #NOTE:Name category 2 in your variable that will be your comparison
     xlab = "Democratic ... Republican", #NOTE: Write what you want on the x axis
     main = "Effect of Democratic vs. Republican", #NOTE: Write what you want on the main title
     xlim = c(-.2, .2),
     labeltype = "custom", 
     custom.labels = topic_labels
     ) #NOTE: Insert custom theme names

#Topics
topic_distributions <- First_STM$theta
topic_distribution_with_year <- cbind(meta$year, topic_distributions)
topic_distribution_df <- as.data.frame(topic_distributions)
topic_distribution_df$year = meta$year

for (i in seq_along(topic_number)) {
  topic_index <- topic_number[i]
  topic_label <- topic_labels[i]
  yearly_distribution <- topic_distribution_df %>%
    group_by(year) %>%
    summarise(Average = mean(get(paste0("V", topic_index))))
  p <- ggplot(yearly_distribution, aes(x = year, y = Average)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = yearly_distribution$year) +
    labs(x = "Fiscal Year", y = "Average Topic Distribution",
         title = paste("Trend of", topic_label)) +
    theme_classic()
  ggsave(filename = paste0("t", topic_index, ".png"), plot = p, width = 6, height = 4)
}

#INPUT HERE: We repeat the same model as before, but now we specify a single topic (easier to plot)

#Geography
meta_count <- meta %>% count(state) %>% filter(n > 15) %>% arrange(desc(n))
ggplot(meta_count, aes(x = reorder(state, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "State", y = "Speech Count (>15)") +
  coord_flip() +
  theme_classic()
