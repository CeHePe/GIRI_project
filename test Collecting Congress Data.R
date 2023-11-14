#GIRI Class 1 R tutorial on Collecting News Data

# First, we need to install our R packages. Only do this the first time you run this code.

#install.packages("tidyRSS")
#install.packages("rvest")
#install.packages("base64url")
#install.packages("stringr")
#install.packages("readr")

# Now, we load these packages into R
library(tidyRSS)
library(rvest)
library(base64url)
library(stringr)
library(readr)

# We will search for news articles through Google News.

#First, we input our search terms for our topic of interest using Google search operators.Here, we will specify key words, sites, dates.

search_term <- 'budget'


# Next, we collect the news articles that meet our search criteria on Google News.
url <- paste0('https://www.congress.gov/quick-search/congressional-record?wordsPhrases=', 
              URLencode(search_term, reserved= T), 
              '&wordVariants=on&searchIn=on&congressGroups%5B%5D=0&congresses%5B%5D=118',
              '&congresses%5B%5D=117&congresses%5B%5D=116&congresses%5B%5D=115&dateOperator=equal',
              '&startDate=&endDate=&dateIsOption=yesterday&sectionSenate=on&sectionHouse=on',
              '&representative%5B%5D=&senator%5B%5D=')

articles <- tidyfeed(url)

# ATTENTION: For the purposes of the tutorial, we will only process the first 3 articles that we collected. In the future, exclude the next line of code.

articles= articles[1:3,]


# Now, we have a collection of articles. However, these articles do not include the full text. We can to scrap the links for the full text of the articles.


#Before scraping, we have to reformat the urls into the proper format



articles$url= base64_urldecode(articles$item_guid)
##########
articles$url=iconv(articles$url, from="ISO-8859-1", to="UTF-8")

articles$url=gsub(".*(http)", "\\1", articles$url)

articles$url=gsub("(html).*", "\\1", articles$url)
##########
articles$url=gsub("<d2>.*", "", articles$url)


#Now we scrape the links, collecting each paragraph of text and taking a 10 second break between each article. 


all_para <- c()
for(n in 1:nrow(articles)){
  html <- read_html(articles$url[n])
  para <- html_text(html_nodes(html, 'p'))
  all_para <- c(all_para, para)
  Sys.sleep(10)
}

#We convert our collected data into a data frame and get rid of any repeat paragraphs. 

para_tbl <- as.data.frame(table(all_para))
para_tbl <- subset(para_tbl, Freq < 2)

# Now we export the data frame into a csv

write_excel_csv(para_tbl,"articles.csv")

# In excel: (1) use TEXTJOIN(" ", TRUE, A:A) to merge the text rows. (2) Copy this cell. (3) Paste Special (Values Only) into the Spreadsheet

