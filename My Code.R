library(data.table)
library(stringr)
library(VIM)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(formattable)
library(plotly)
library(rpart)
library(rpart.plot)


imdb$Genre <- str_replace_all(imdb$Genre, fixed(" "), "")

IMDB <- imdb

spl <- unique(unlist(strsplit(imdb$Genre, ",")))






genres.df <- as.data.frame(IMDB[,c("Genre", "IMDB_Rating")])
genres.df
# separate different genres into new columns
genres.df$Action <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Music") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$Genre), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)

# get the mean of imdb score for different genres
means <- rep(0,20)

for (i in 1:20) {
  means[i] <- mean(genres.df$IMDB_Rating[genres.df[i+2]==1], na.rm = TRUE)
}
# plot the means
barplot(means, main = "Average imdb scores for different genres")

IMDB <- subset(IMDB, select = -c(Genre))

colSums(sapply(IMDB, is.na))

missing.values <- aggr(IMDB, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, 
                       cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)

IMDB <- IMDB[!is.na(IMDB$Gross), ]
IMDB <- IMDB[!is.na(IMDB$Meta_score), ]
IMDB <- IMDB[!is.na(IMDB$Certificate), ]
dim(IMDB)

dim(imdb)

ggplot(IMDB, aes(Released_Year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB <- IMDB[IMDB$Released_Year >= 1980,]

IMDB %>%
  filter(Released_Year %in% c(2000:2019)) %>%
  arrange(desc(Gross)) %>%
  top_n(20, Gross) %>%
  ggplot(aes(x=IMDB_Rating, y=Gross/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=Series_Title)) +
  labs(x = "IMDB_Rating", y = "Gross Profit $million", title = "Top 10 Gross Profit") +
  theme(plot.title = element_text(hjust = 0.5))


IMDB %>%
  filter(Released_Year %in% c(1980:2019)) %>%
  arrange(desc(Gross)) %>%
  top_n(10, Gross) %>%
  ggplot(aes(x=Certificate, y=Gross/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=Series_Title)) +
  labs(x = "IMDB_Rating", y = "Gross Profit $million", title = "Top 10 Gross Profit") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB %>%
  group_by(Director) %>%
  summarise(avg_imdb = mean(IMDB_Rating)) %>%
  arrange(desc(avg_imdb)) %>%
  top_n(20, avg_imdb) %>%
  formattable(list(avg_imdb = color_bar("orange")), align = 'l')


IMDB %>%
  plot_ly(x = ~No_of_Votes, y = ~IMDB_Rating, 
          color = ~Duration , 
          mode = "markers", text = ~Duration, alpha = 0.7, type = "scatter")
