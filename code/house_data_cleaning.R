#House's Debate Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")

#Load the package 
library(stm)

#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")

# read text and basic cleaning
speeches_paths <- c("data/hein-daily/speeches_107.txt",
                    "data/hein-daily/speeches_107.txt",
                    "data/hein-daily/speeches_108.txt",
                    "data/hein-daily/speeches_108.txt",
                    "data/hein-daily/speeches_109.txt",
                    "data/hein-daily/speeches_110.txt",
                    "data/hein-daily/speeches_110.txt",
                    "data/hein-daily/speeches_110.txt",
                    "data/hein-daily/speeches_111.txt",
                    "data/hein-daily/speeches_112.txt")

maps_paths <- c("data/hein-daily/107_SpeakerMap.txt",
                "data/hein-daily/107_SpeakerMap.txt",
                "data/hein-daily/108_SpeakerMap.txt",
                "data/hein-daily/108_SpeakerMap.txt",
                "data/hein-daily/109_SpeakerMap.txt",
                "data/hein-daily/110_SpeakerMap.txt",
                "data/hein-daily/110_SpeakerMap.txt",
                "data/hein-daily/110_SpeakerMap.txt",
                "data/hein-daily/111_SpeakerMap.txt",
                "data/hein-daily/112_SpeakerMap.txt")
speech_list <- list()
map_list <- list()
data_list <- list()
speech_line <- list(c(25349,89),
                 c(113464,203),
                 c(22078,231),
                 c(131692,266),
                 c(14950,182),
                 c(9917,115),
                 c(127081,32),
                 c(170707,65),
                 c(69547,64),
                 c(22559,231)
                 )


# House speeches for ten fiscal years
for (i in 1:3){
  column_names <- read.table(speeches_paths[i], sep = "|", nrows = 1, header = FALSE)
  speech_list[[i]] <- read.table(speeches_paths[i], sep = "|", 
                               skip = speech_line[[i]][1], nrows = speech_line[[i]][2], header = FALSE)
  colnames(speech_list[[i]]) <- as.character(unlist(column_names))
  map_list[[i]] <- read.delim(maps_paths[i], sep = "|", header = TRUE)
  data_list[[i]] <- merge(speech_list[[i]], map_list[[i]], by = "speech_id", all.x = TRUE)
}

column_names <- read.table(speeches_paths[3], sep = "|", nrows = 1, header = FALSE)
speech3a <- read.table(speeches_paths[3], sep = "|", 
                             skip = 20215, nrows = 220, header = FALSE)
colnames(speech3a) <- as.character(unlist(column_names))
map3a <- read.delim(maps_paths[3], sep = "|", header = TRUE)
data3a <- merge(speech3a, map3a, by = "speech_id", all.x = TRUE)

for (i in 4:10){
  column_names <- read.table(speeches_paths[i], sep = "|", nrows = 1, header = FALSE)
  speech_list[[i]] <- read.table(speeches_paths[i], sep = "|", 
                                 skip = speech_line[[i]][1], nrows = speech_line[[i]][2], header = FALSE)
  colnames(speech_list[[i]]) <- as.character(unlist(column_names))
  map_list[[i]] <- read.delim(maps_paths[i], sep = "|", header = TRUE)
  data_list[[i]] <- merge(speech_list[[i]], map_list[[i]], by = "speech_id", all.x = TRUE)
}

#merge

data <- rbind(
  cbind(data_list[[1]], year = "2002", crisis='non-crisis'),
  cbind(data_list[[2]], year = "2003", crisis='non-crisis'),
  cbind(data_list[[3]], year = "2004", crisis='non-crisis'),
  cbind(data3a, year = "2004", crisis='non-crisis'),
  cbind(data_list[[4]], year = "2005", crisis='non-crisis'),
  cbind(data_list[[5]], year = "2006", crisis='crisis'),
  cbind(data_list[[6]], year = "2007", crisis='crisis'),
  cbind(data_list[[7]], year = "2008", crisis='crisis'),
  cbind(data_list[[8]], year = "2009", crisis='crisis'),
  cbind(data_list[[9]], year = "2010", crisis='crisis'),
  cbind(data_list[[10]], year = "2011", crisis='non-crisis'))

data<- na.omit(data)

saveRDS(data, file = "data/data_house.rds")


#INPUT HERE: Clean your text data
data <- readRDS(file = "data/data_house.rds")
processed<- textProcessor(data$speech, #Note: write the column that has the text 
                          metadata = data, #NOTE: write the name of your data set
                          lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE,  
                          removepunctuation=TRUE, stem=TRUE, 
                          wordLengths=c(3,Inf),  #Here, we remove words shorter than 3 letters
                          sparselevel=1, language="en",  verbose=TRUE, 
                          onlycharacter= FALSE, striphtml=FALSE, 
                          customstopwords=c("economic","economics","economical","economies","economy",
                                            "america","america’s","american","americans",
                                            "speaker","can","minute", "minutes","gentleman","gentlewoman",
                                            "just","now","time","madam","miss",
                                            "will","that","ladies","gentlemen",
                                            "take","what","there","across",
                                            "one","year","want","back","say",
                                            "yea","nay","get","rollcall","aye","noe",
                                            "yield","chairman","committee","committees",
                                            "amendment","amendments","amend","amends","amending",
                                            "budget","member","members", "vote","votes",
                                            "voter","voters", "house", "rule",
                                            "come","let","resolution", "debate", "debates", 
                                            "thank", "ask", "asks", "asking", 
                                            "distinguish", "distinguished", "may", 
                                            "question","questions", "work", "works", "working", 
                                            "bill","bills","congress", "congressional",
                                            "process", "senate","senator","senators",
                                            "side","sides", "friend","friends", 
                                            "know","known","knowing","knows", 
                                            "colleague","colleagues", "think","thinks","thinking",
                                            "unanimous","unanimously",
                                            "caucus","caucuses", "make","makes")) #NOTE: write custom stopwords here

saveRDS(processed, file = "data/processed_house.rds")
