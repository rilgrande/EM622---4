
# Roger IL Grande
# EM-622 Project 4


# Load the necessary libraries
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plyr)
library(reshape)
library(tidyquant)

# Set working directory
setwd("~/Documents/R Projects")

OlympicData <- read.csv("olympic_athletes-wrangled.csv", header = TRUE) # Import the dataset

View(OlympicData) # View the data


str(OlympicData) # Check if the data are imported as correct forms (looks good for the question in this assignment, all characters except year which is an integer)


# Find the number of gold medals for each host country since 2000
# I am interpreting this as "the number of gold medals Stanford students won in each host country since 2000."
# This can be done by filtering, as there is only one host country per year

# Filter the data frame by year, every year since 2000. Count the number of rows in each data frame, which is the number of gold medals that year/in that host country
year_2000_gold <- filter(OlympicData, Year == "2000", Medal == "Gold", na.rm = TRUE)
gold_count_2000 <- nrow(year_2000_gold)

year_2004_gold <- filter(OlympicData, Year == "2004", Medal == "Gold", na.rm = TRUE)
gold_count_2004 <- nrow(year_2004_gold)

year_2008_gold <- filter(OlympicData, Year == "2008", Medal == "Gold", na.rm = TRUE)
gold_count_2008 <- nrow(year_2008_gold)

year_2012_gold <- filter(OlympicData, Year == "2012", Medal == "Gold", na.rm = TRUE)
gold_count_2012 <- nrow(year_2012_gold)

year_2016_gold <- filter(OlympicData, Year == "2016", Medal == "Gold", na.rm = TRUE)
gold_count_2016 <- nrow(year_2016_gold)

# Establish columns for the new data frame showing the total gold medals won in each host country
Host_Country <- c("Australia", "Greece", "China", "England", "Brazil")
Year <- c(2000, 2004, 2008, 2012, 2016)
Gold_Total <- c(gold_count_2000, gold_count_2004, gold_count_2008, gold_count_2012, gold_count_2016)

gold.count.summary <- data.frame(Host_Country, Year, Gold_Total) # Create a new data frame to summarize the gold medal counts

str(gold.count.summary) # Double-check data types

View(gold.count.summary) # View the data frame


# Examples of categorical variables are race, gender, age group, and educational level

# Question: How many gold medals were won in women's sports vs men's sports?

# First, the original data (OlympicData) needs to be sorted properly based on women's or men's sports
# Add women's or men's designation to sports that do not currently have one
OlympicData$Sport <- gsub("Equestrian", "Women's Equestrian", OlympicData$Sport)
OlympicData$Sport <- gsub("Baseball", "Men's Baseball", OlympicData$Sport)
OlympicData$Sport <- gsub("Softball", "Women's Softball", OlympicData$Sport)
OlympicData$Sport <- gsub("Wrestling", "Men's Wrestling", OlympicData$Sport)
OlympicData$Sport <- gsub("Synchronized Swimming", "Women's Synchornized Swimming", OlympicData$Sport)
OlympicData$Sport <- gsub("Field Hockey", "Women's Field Hockey", OlympicData$Sport)

# Replace sport name with just male or female
OlympicData$Sport <- gsub(".*Women's.*", "Female", OlympicData$Sport)
OlympicData$Sport <- gsub(".*Men's.*", "Male", OlympicData$Sport)

# Replace medal color with yes or no for gold or no gold
OlympicData$Medal <- gsub(".*Gold.*", "Yes", OlympicData$Medal)
OlympicData$Medal <- gsub(".*Silver.*", "No", OlympicData$Medal)
OlympicData$Medal <- gsub(".*Bronze.*", "No", OlympicData$Medal)


# Create a margin table showing gold vs non-gold medals for women's and men's sports, then create a Mosaic Plot with this information

# Convert data frame to table
OlympicConvert <- OlympicData[, c("Sport", "Medal")]
View(OlympicConvert) # Data frame

colnames(OlympicConvert) <- c("Gender", "Gold") # Change the column names to improve plot readability

OlympicTable <- table(OlympicConvert)
View(OlympicTable) # Table


# Create the Mosaic Plot
plot(margin.table(OlympicTable, c(1, 2)))




# Q2 Financial Data

# Lockheed Martin Corporation


# Download stock price data
lmt <- tq_get('LMT',
               from = "2020-01-01",
               to = "2021-03-01",
               get = "stock.prices")
head(lmt)

# Filter data to get the row that contains the max value
max_lmt <- lmt %>% 
  filter(adjusted==max(lmt$adjusted))

# Filter data to get the row that contains the min value
min_lmt <- lmt %>% 
  filter(adjusted==min(lmt$adjusted))

# Create the plot for Lockheed Martin with markers
lmt %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(colour = "blue") +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Lockheed Martin Stock Price 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,500,10)) +
  geom_point(data = max_lmt, 
             aes(x = date,y = adjusted), 
             color = 'green',
             size = 4) +
  geom_point(data = min_lmt, 
             aes(x = date,y = adjusted), 
             color = 'red',
             size = 4)
  

# L#Harris Technologies


# Download stock price data
lhx <- tq_get('LHX',
              from = "2020-01-01",
              to = "2021-03-01",
              get = "stock.prices")
head(lhx)

# Filter data to get the row that contains the max value
max_lhx <- lhx %>% 
  filter(adjusted==max(lhx$adjusted))

# Filter data to get the row that contains the min value
min_lhx <- lhx %>% 
  filter(adjusted==min(lhx$adjusted))

# Create the plot for L3Harris with markers
lhx %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(colour = "red") +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "L3Harris Stock Price 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,500,10)) +
  geom_point(data = max_lhx, 
             aes(x = date,y = adjusted), 
             color = 'green',
             size = 4) +
  geom_point(data = min_lhx, 
             aes(x = date,y = adjusted), 
             color = 'red',
             size = 4)


# Boeing Company


# Download stock price data
ba <- tq_get('BA',
              from = "2020-01-01",
              to = "2021-03-01",
              get = "stock.prices")
head(ba)

# Filter data to get the row that contains the max value
max_ba <- ba %>% 
  filter(adjusted==max(ba$adjusted))

# Filter data to get the row that contains the min value
min_ba <- ba %>% 
  filter(adjusted==min(ba$adjusted))

# Create the plot for Boeing with markers
ba %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line(colour = "purple") +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Boeing Stock Price 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(0,500,10)) +
  geom_point(data = max_ba, 
             aes(x = date,y = adjusted), 
             color = 'green',
             size = 4) +
  geom_point(data = min_ba, 
             aes(x = date,y = adjusted), 
             color = 'red',
             size = 4)


# Plot all three stocks on the same graph with max/min markers on each stock

tq_get(c("LMT","LHX","BA"), from = "2020-01-01",
       to = "2021-03-01", get="stock.prices") %>%
  ggplot(aes(date, adjusted, color=symbol)) +
  geom_line() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "Defense Stock Prices 2020") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = max_lmt, 
             aes(x = date,y = adjusted), 
             color = 'blue',
             size = 4) +
  geom_point(data = min_lmt, 
             aes(x = date,y = adjusted), 
             color = 'black',
             size = 4) +
  geom_point(data = max_lhx, 
             aes(x = date,y = adjusted), 
             color = 'red',
             size = 4) +
  geom_point(data = min_lhx, 
             aes(x = date,y = adjusted), 
             color = 'black',
             size = 4) +
  geom_point(data = max_ba, 
             aes(x = date,y = adjusted), 
             color = 'purple',
             size = 4) +
  geom_point(data = min_ba, 
             aes(x = date,y = adjusted), 
             color = 'black',
             size = 4) +
  scale_color_manual("Stock", values=c("Purple","Red", "Blue"))

