#President's Message Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")

#Load the package 
library(stm)

#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")
processed <- readRDS(file = "data/processed_president.rds")

#Now we separate our data into neat bits for our analysis 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
First_STM <- readRDS(file = "data/First_STM_president.rds")
#Now we analyze it

#First, we identify topics and interpret them

#Plot the most prevalent topics in this model
par(mar=c(2,2,2,2))
plot(First_STM)


# INPUT HERE: Find the words that most represent a topic
labelTopics(First_STM,
            topics= c(3,9,17,7,19,15), #NOTE: Write which topic you want to assess
            n=5) #NOTE: Write how many top words you want to pull up


#Now that we understand topics, we can see if their prevalence differs based on certain factors


#INPUT HERE: Specify a model that assesses the prevalence of your topics 
prep_president <- estimateEffect(c(1:20) ~ crisis + party + crisis*party, #NOTE: your formula for prevalence, 
                       #Your DV/output is 1:"k" 
                       #Your IV/Input is your key factors that you think impact prevalence
                       First_STM,meta = out$meta, uncertainty = "Global")
saveRDS(prep_president, file = "data/prep_president.rds")

