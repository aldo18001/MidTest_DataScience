library(tidyverse)
library(naniar)
library(forcats)
library(hrbrthemes)
library(corrplot)
library(RColorBrewer)
library(ggplot2)

imdb_top_1000 <- read_csv("C:/Users/Lenovo/Downloads/imdb_top_1000.csv",
                          col_types = cols(Certificate = col_factor(levels = c()),
                                           Runtime = col_number()))
View(imdb_top_1000)

colSums(is.na(imdb_top_1000)) #Cek Missing Value

gg_miss_var(imdb_top_1000) #draws a ggplot of the number of missings in each variable

str(imdb_top_1000)

imdb_top_1000$Gross <- as.integer(gsub(",","",imdb_top_1000$Gross))

imdb_top_1000$Released_y <- lapply(imdb_top_1000$Released_Year, as.character)
imdb_top_1000$Released_Year <- as.integer(imdb_top_1000$Released_y)

which(is.na(imdb_top_1000$Released_Year))

imdb_top_1000$Series_Title[967] #Apollo film
imdb_top_1000$Released_Year[967] <- 1995


imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Decade = if_else(Released_Year >= 2000,
                          paste0(Released_Year  %/% 10 * 10, "'s"),
                          paste0((Released_Year - 1900) %/% 10 * 10, "'s")))

imdb_top_1000 %>%
  filter(!is.na(Gross)) %>%
  group_by(Decade) %>%
  summarise(count=n())

#Finding the different percentiles of Runtime

quantile(imdb_top_1000$Runtime, c(0.33,0.67,1))

#Creating a new variable Duration and grouping the runtime in three groups as Short, Medium and Long

imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Duration = ifelse(Runtime <=108, "Short", ifelse(Runtime <= 130, "Medium", "Long")))


imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Type_Of_Movie = ifelse(Released_Year <=1953, "Old", ifelse(Released_Year <= 1983, "Classic", "Modern")))



imdb_top_1000$New_Genre <- gsub("([A-Za-z]+).*", "\\1", imdb_top_1000$Genre)

New_Genre <- imdb_top_1000 %>%
  group_by(New_Genre) %>%
  summarize(Average= mean(Gross, na.rm=TRUE))

#Inflation Factor each decade ( Valuation to 2021)

infl <- data.frame(decade = c("20's","30's", "40's", "50's", "60's", "70's", "80's", "90's", "2000's","2010's", "2020's"), 
                   one_plus_inflation = c(15.03, 19.2, 14.61,9.81, 8.35, 4.89, 2.44, 1.73, 1.35, 1.11,1 ))



