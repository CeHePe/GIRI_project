#President's Message Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")

#Load the package 
library(stm)

#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")

# read text and basic cleaning
file_paths <- c("data/Budget Message of the President 2007.txt", 
                "data/Budget Message of the President 2008.txt", 
                "data/Budget Message of the President 2009.txt", 
                "data/Budget Message of the President 2010.txt", 
                "data/Budget Message of the President 2011.txt", 
                "data/Budget Message of the President 2012.txt")

data_list <- list()
for (i in file_paths) {
  # read text
  text <- readLines(i, warn = FALSE)
  # merge every row
  text <- paste(text, collapse = " ")
  text <- gsub("�", ".", text)
  text <- gsub("\\. ", ".\n", text)
  text <- gsub(" \n", "\n", text)
  data_list[[i]] <- data.frame(Text = text)
}

names(data_list) <- paste("data", 7:12, sep = "")
list2env(data_list, envir = .GlobalEnv)

# function that convert text in first row into rows
convert_first_row_to_rows <- function(data_df) {
  first_row_text <- as.character(data_df[1, "Text"])
  rows <- unlist(strsplit(first_row_text, "\n"))
  new_data_df <- data.frame(Text = rows)
  
  return(new_data_df)
}

for (i in 7:12) {
  data_df_name <- paste("data", i, sep = "")
  converted_data_df <- convert_first_row_to_rows(get(data_df_name))
  assign(data_df_name, converted_data_df)
  
}

#merge all six
data <- rbind(
  cbind(data7, year = "2007", crisis='non-crisis', party='R', state='TX'),
  cbind(data8, year = "2008", crisis='non-crisis', party='R', state='TX'),
  cbind(data9, year = "2009", crisis='crisis', party='R', state='TX'),
  cbind(data10, year = "2010", crisis='crisis', party='D', state='IL'),
  cbind(data11, year = "2011", crisis='non-crisis', party='D', state='IL'),
  cbind(data12, year = "2012", crisis='non-crisis', party='D', state='IL')
)
colnames(data)[colnames(data) == "V1"] <- "text"

#INPUT HERE: Clean your text data
processed_president<- textProcessor(data$Text, #Note: write the column that has the text 
                          metadata = data, #NOTE: write the name of your data set
                          lowercase=TRUE, removestopwords=TRUE, removenumbers=TRUE,  
                          removepunctuation=TRUE, stem=TRUE, 
                          wordLengths=c(3,Inf),  #Here, we remove words shorter than 3 letters
                          sparselevel=1, language="en",  verbose=TRUE, 
                          onlycharacter= FALSE, striphtml=FALSE, 
                          customstopwords=c("economic","economics","economical","economies","economy",
                                            "america","america’s","american","americans",
                                            "can")) #NOTE: write custom stopwords here

saveRDS(processed_president, file = "data/processed_president.rds")
