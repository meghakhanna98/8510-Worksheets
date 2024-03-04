#Exploratory Data Analysis Checklist
---
title: "Exploratory Data Analysis with Peng's Checklist"
author: "Megha Khanna"
date: "2024-03-03"
---

#Formulate your question
# @What are the trends in the distribution of LGBTQ+ venues across different cities over the years in the gayguides dataset?

#Read in your data
library(dplyr)
library(DigitalMethodsData)
data("gayguides")
census_regions <- read.csv("https://raw.githubusercontent.com/regan008/DigitalMethodsData/main/raw/censusregions.csv")

#Check the packaging
nrow(gayguides)
ncol(gayguides)

#Run (str)
str(gayguides)

#Look at the top and the bottom of your data
head(gayguides)
tail(gayguides)

#Check your “n”s
summary(gayguides$Year)
table(gayguides$city)

#Validate with at least one external data source
#The data can be validated through Bob Damron Address Book Data, 1965-1985

#Try the easy solution first
#1)
library(ggplot2)

ggplot(gayguides, aes(x = Year, group = city, color = city)) + geom_line(aes(y = ..count..), stat = "count") + theme(legend.position = "none")
ggplot(subset(gayguides, Year == 1985), aes(x = city)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2)
library(dplyr)
avg.venues <- gayguides %>%
  group_by(city, Year) %>%
  summarize(avg.venues = mean(Number.of.Venues, na.rm = TRUE))
ptint(avg.venues)

sorted_data <- gayguides %>%
  arrange(Year)
print(sorted_data)

#Challenge your solution

#1) 
library(dplyr)
gayguides <- gayguides %>%
mutate(decade = floor(Year / 10) * 10)
#2)
boxplot(gayguides$Year ~ gayguides$city, las = 2)
#3)
ggplot(gayguides, aes(x = Year)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Years in Gay Guides Dataset", x = "Year", y = "Count")

#Follow up questions
#For the follow up question, we can find an average of the acceptance among the cities each year. Also, with the help of data, we can search within the intricate details of each cities. 
