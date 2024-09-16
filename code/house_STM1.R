#House's Debate Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")

#Load the package 
library(stm)

#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")
processed <- readRDS(file = "data/processed_house.rds")

#Now we separate our data into neat bits for our analysis 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

set.seed(2008)





# INPUT HERE: Find the best K value
set.seed(2008)
findingk <- searchK(out$documents, out$vocab, 
                    K = c(19:21), #NOTE: Write the k values you want to test
                    prevalence =~ crisis * party, #NOTE: Write a regression formula for variables that will impact topic prevalence 
                    #content =~ crisis + party,#NOTE: Write a regression formula for variables that will impact topic content 
                    data = meta, verbose=FALSE)


#Plot our results
plot(findingk)

#If you get an error, try one of the codes below. If they don't work, restart R and try again.  
#par(mar=c(2,2,2,2))
#layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE), respect = TRUE)



#INPUT HERE: Fit a model with our given k and topic prevalence and content equations

set.seed(2008)
First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 20, #NOTE: Write "k" value here
                 prevalence =~ crisis * party, #NOTE: Write a regression formula for variables that will impact topic prevalence 
                 #content =~ crisis,#NOTE: Write a regression formula for variables that will impact topic content 
                 max.em.its =75, data = out$meta,init.type = "Spectral", 
                 verbose = FALSE)

First_STM_crisis_prev <- stm(documents = out$documents, vocab = out$vocab,
                        K = 20, #NOTE: Write "k" value here
                        prevalence =~ crisis, #NOTE: Write a regression formula for variables that will impact topic prevalence 
                        #content =~ crisis,#NOTE: Write a regression formula for variables that will impact topic content 
                        max.em.its =75, data = out$meta,init.type = "Spectral", 
                        verbose = FALSE)

First_STM_crisis_cont <- stm(documents = out$documents, vocab = out$vocab,
                             K = 20, #NOTE: Write "k" value here
                             #prevalence =~ crisis, #NOTE: Write a regression formula for variables that will impact topic prevalence 
                             content =~ crisis,#NOTE: Write a regression formula for variables that will impact topic content 
                             max.em.its =75, data = out$meta,init.type = "Spectral", 
                             verbose = FALSE)

First_STM_party <- stm(documents = out$documents, vocab = out$vocab,
                 K = 20, #NOTE: Write "k" value here
                 prevalence =~ party, #NOTE: Write a regression formula for variables that will impact topic prevalence 
                 content =~ party,#NOTE: Write a regression formula for variables that will impact topic content 
                 max.em.its =75, data = out$meta,init.type = "Spectral", 
                 verbose = FALSE)

#We built the model!
saveRDS(First_STM, file = "data/First_STM_house.rds")
#Now we analyze it
First_STM <- readRDS(file = "data/First_STM_house.rds")

#First, we identify topics and interpret them


#Plot the most prevalent topics in this model
par(mar=c(2,2,2,2))
plot(First_STM)
#plot(First_STM_crisis_cont)

labelTopics(First_STM,
            topics= c(9,4,13), #NOTE: Write which topic you want to assess
            n=10) #NOTE: Write how many top words you want to pull up
sink("output/labelTopics-selected.txt", append=FALSE, split=TRUE)

sageLabels(First_STM,n=10)
sink("output/sageLabels-selected.txt", append=FALSE, split=TRUE)
print(sageLabels(First_STM,n=10))
sink()

#beta
plot(First_STM, type = "perspectives", topics = c(5, 10))
