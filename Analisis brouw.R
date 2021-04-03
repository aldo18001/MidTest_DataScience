library(tidyverse)
library(naniar)
library(forcats)
library(hrbrthemes)
library(corrplot)
library(RColorBrewer)

imdb_top_1000 <- read_csv("C:/Users/Lenovo/Downloads/imdb_top_1000.csv", 
                          +     col_types = cols(Certificate = col_factor(levels = c()), 
                                                 +         Runtime = col_number()))
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

#Arranging and plotting how many films are there from different certificate
ggplot(data = imdb_top_1000 %>%
         filter(!is.na(Certificate)) %>%
         group_by(Certificate) %>%
         summarise(count=n()), aes(x=reorder(Certificate,-count), y=count, fill=Certificate))+
  geom_bar(stat = 'identity')+  
  xlab('Type of Certificate')

imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Decade = if_else(Released_Year >= 2000,
                          paste0(Released_Year  %/% 10 * 10, "'s"),
                          paste0((Released_Year - 1900) %/% 10 * 10, "'s")))

imdb %>%
  filter(!is.na(Gross)) %>%
  group_by(Decade) %>%
  summarise(count=n())


decade_avg<- imdb_top_1000 %>%
  group_by(Decade) %>%
  summarize(average = mean(Gross, na.rm = TRUE))

decade_avg

ggplot(data=decade_avg[-4,],aes(x=reorder(Decade, -average), y=average, fill=Decade)) +
  geom_bar(stat = 'identity') +
  xlab('Decade')+
  ylab('Average Earning')+
  theme(legend.position = c(0.9,0.76))

#Finding the different percentiles of Runtime

quantile(imdb_top_1000$Runtime, c(0.33,0.67,1))

#Creating a new variable Duration and grouping the runtime in three groups as Short, Medium and Long

imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Duration = ifelse(Runtime <=108, "Short", ifelse(Runtime <= 130, "Medium", "Long")))

ggplot(data= imdb_top_1000 %>%
         group_by(Duration) %>%
         summarise(count=n()), aes(y=count, x=Duration, fill=count)) +
  geom_bar(stat= 'identity')+
  xlab('Duration of the Movies') +
  ylab("Number of Movies")

ggplot(imdb_top_1000 %>%
         filter(!is.na(Gross)) %>%
         group_by(Duration) %>%
         summarise(Average = mean(Gross, na.rm = TRUE)), aes(y=Average, x= Duration, fill= Average)) +
  geom_bar(stat='identity') +
  xlab('Duration of the Movies') +
  ylab("Average Earning")

imdb_top_1000 <- imdb_top_1000 %>%
  mutate(Type_Of_Movie = ifelse(Released_Year <=1953, "Old", ifelse(Released_Year <= 1983, "Classic", "Modern")))

ggplot(data=imdb_top_1000 %>%
         group_by(Type_Of_Movie, Duration) %>%
         summarise(count=n()), aes(y=count, x=Type_Of_Movie, fill=Duration)) +
  geom_bar(stat='identity', position = 'fill')+
  labs(fill='Type of Movie')+
  xlab('Type of Movie') +
  ylab(element_blank())

imdb_top_1000$New_Genre <- gsub("([A-Za-z]+).*", "\\1", imdb_top_1000$Genre)

New_Genre <- imdb_top_1000 %>%
  group_by(New_Genre) %>%
  summarize(Average= mean(Gross, na.rm=TRUE))

ggplot(data=New_Genre[-9,], aes(x=reorder(New_Genre, -Average), y=Average, fill= Average)) +
  geom_bar(stat='identity') +
  theme(legend.position = c(0.9,0.8),
        axis.text.x= element_text(angle = 90, vjust=0.5))+
  xlab('Genre')+
  ylab('Average Earning')
