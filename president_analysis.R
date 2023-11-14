#President's Message Analysis
rm(list = ls())

#Install the stm package
#install.packages("stm")

#Load the package 
library(stm)

#Load the data
setwd("C:/Users/scene/Documents/R/GIRI/project")
First_STM <- readRDS(file = "data/First_STM_president.rds")
prep <- readRDS(file = "data/prep_president.rds")


#INPUT HERE: Now we plot the effect of CRISIS, a categorical variable 
Sys.setlocale(locale="en_US.UTF-8")
plot(prep, 
     covariate = "crisis", #NOTE: Write the variable you want to assess
     topics = c(12,16,13,15,14,19),#NOTE: Write the topics you want to test
     model = First_STM, method = "difference", 
     cov.value1 = 'crisis', #NOTE:Name category 1 in your variable that you want to assess
     cov.value2 = 'non-crisis', #NOTE:Name category 2 in your variable that will be your comparison
     xlab = "Non-Crisis ... Crisis", #NOTE: Write what you want on the x axis
     main = "Effect of Non-Crisis vs. Crisis", #NOTE: Write what you want on the main title
     xlim = c(-.2, .2),
     labeltype = "custom", 
     custom.labels = c("Fiscal Policy & Reform", 
                       "Employment & Unemployment", 
                       "Economic & Industrial Development", 
                       "International Relations & Global Challenges", 
                       "Economic Crises & Financial Troubles", 
                       "Social Security & Future Planning")) #NOTE: Insert custom theme names

#INPUT HERE: Now we plot the effect of PARTY, a categorical variable 
plot(prep, 
     covariate = "party", #NOTE: Write the variable you want to assess
     topics = c(12,16,13,15,14,19),#NOTE: Write the topics you want to test
     model = First_STM, method = "difference", 
     cov.value1 = 'R', #NOTE:Name category 1 in your variable that you want to assess
     cov.value2 = 'D', #NOTE:Name category 2 in your variable that will be your comparison
     xlab = "Democratic ... Republican", #NOTE: Write what you want on the x axis
     main = "Effect of Democratic vs. Republican", #NOTE: Write what you want on the main title
     xlim = c(-.2, .2),
     labeltype = "custom", 
     custom.labels = c("Fiscal Policy & Reform", 
                       "Employment & Unemployment", 
                       "Economic & Industrial Development", 
                       "International Relations & Global Challenges", 
                       "Economic Crises & Financial Troubles", 
                       "Social Security & Future Planning")) #NOTE: Insert custom theme names

#INPUT HERE: Now we plot the effect of CRISIS*PARTY, a categorical variable 
plot(prep, 
     covariate = "crisis":"party", #NOTE: Write the variable you want to assess
     topics = c(12,16,13,15,14,19),#NOTE: Write the topics you want to test
     model = First_STM, method = "difference", 
     cov.value1 = 'R', #NOTE:Name category 1 in your variable that you want to assess
     cov.value2 = 'D', #NOTE:Name category 2 in your variable that will be your comparison
     xlab = "Democratic ... Republican", #NOTE: Write what you want on the x axis
     main = "Effect of Democratic vs. Republican", #NOTE: Write what you want on the main title
     xlim = c(-.2, .2),
     labeltype = "custom", 
     custom.labels = c("Fiscal Policy & Reform", 
                       "Employment & Unemployment", 
                       "Economic & Industrial Development", 
                       "International Relations & Global Challenges", 
                       "Economic Crises & Financial Troubles", 
                       "Social Security & Future Planning")) #NOTE: Insert custom theme names






















#INPUT HERE: Now we plot the effect of Day, a continuous variable 
plot(prep, 
     "day", #NOTE: Write the variable you want to assess 
     method = "continuous", 
     topics =c(16), #NOTE: Write the topic you want to test
     model = First_STM, printlegend = FALSE, xaxt = "n", 
     xlab = "Time (2008)") #NOTE: Write what you want on the x axis

#We can alter the X axis to measure months
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),labels = monthnames)

?poliblog5k

#Now we want to see if these factors interact

#INPUT HERE: We repeat the same model as before, but now we specify a single topic (easier to plot)
prep <- estimateEffect(c(16) ~ #NOTE: Write your single topic here
                         rating*s(day), 
                       First_STM, metadata = out$meta, uncertainty = "Global")

plot(prep, covariate = "day", #NOTE:Write the name of the continuous variable here
     model = First_STM, method = "continuous", xaxt = "n",xlab = " ", 
     moderator = "blog", #NOTE: Write the name of the categorical variable here
     moderator.value = "tp", linecol = "blue", #NOTE: Write which category you want to draw and pick a color
     ylim = c(-.1, .6),printlegend = F)

#Repeat the process, but now for the other category 
plot(prep, covariate = "day", #NOTE:Write the name of the continuous variable here
     model = First_STM, method = "continuous", xaxt = "n",xlab = " ", 
     moderator = "blog", #NOTE: Write the name of the categorical variable here
     moderator.value = "at", linecol = "red", #NOTE: Write which category you want to draw and pick a color
     add = T, printlegend = F)

#We can alter the X axis to measure months
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),labels = monthnames)
legend(2, .6, c("tp", "at"),lwd = 2, col = c("blue", "red"))




# We can also check the stats for our prevalence differences 

prep <- estimateEffect(c(1:19) ~ rating*day, First_STM,
                       meta = out$meta, uncertainty = "Global")
sum=summary(prep, 
            topics=c(13))#NOTE: Write topic you are interested in
sum$tables
