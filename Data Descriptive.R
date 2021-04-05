#Arranging and plotting how many films are there from different certificate
ggplot(data = imdb_top_1000 %>%
         filter(!is.na(Certificate)) %>%
         group_by(Certificate) %>%
         summarise(count=n()), aes(x=reorder(Certificate,-count), y=count, fill=Certificate))+
  geom_bar(stat = 'identity')+  
  xlab('Type of Certificate')

#Average Earning each decade
decade_avg<- imdb_top_1000 %>%
  group_by(Decade) %>%
  summarize(average = mean(Gross, na.rm = TRUE))

decade_avg

ggplot(data=decade_avg[-4,],aes(x=reorder(Decade, -average), y=average/1000000, fill=Decade)) +
  geom_bar(stat = 'identity') +
  xlab('Decade')+
  ylab('Average Earning in million')+
  theme(legend.position = c(0.9,0.76))

#sorted by decade
new_decade_avg <- rbind(decade_avg[1,],decade_avg[5,],decade_avg[6,],decade_avg[7,],
                        decade_avg[8,],decade_avg[9,],decade_avg[10,],decade_avg[11,],
                        decade_avg[2,],decade_avg[3,],decade_avg[4,])
new_decade_avg <- cbind(new_decade_avg,infl$one_plus_inflation)

new_decade_avg <- new_decade_avg %>%
  mutate(adjusted_avg = new_decade_avg$average*new_decade_avg$`infl$one_plus_inflation`)
new_decade_avg

ggplot(data=new_decade_avg[-4,],aes(x=reorder(Decade, -adjusted_avg), y=adjusted_avg/1000000, fill=Decade)) +
  geom_bar(stat = 'identity') +
  xlab('Decade')+
  ylab('Average Earning in million')+
  theme(legend.position = c(0.9,0.76))

#How many movie in each category by duration
ggplot(data= imdb_top_1000 %>%
         group_by(Duration) %>%
         summarise(count=n()), aes(y=count, x=Duration, fill=count)) +
  geom_bar(stat= 'identity')+
  xlab('Duration of the Movies') +
  ylab("Number of Movies")

#Total earning group by duration
ggplot(imdb_top_1000 %>%
         filter(!is.na(Gross)) %>%
         group_by(Duration) %>%
         summarise(Average = mean(Gross, na.rm = TRUE)), aes(y=Average, x= Duration, fill= Average)) +
  geom_bar(stat='identity') +
  xlab('Duration of the Movies') +
  ylab("Average Earning")

#Duration composition for each type of movie
ggplot(data=imdb_top_1000 %>%
         group_by(Type_Of_Movie, Duration) %>%
         summarise(count=n()), aes(y=count, x=Type_Of_Movie, fill=Duration)) +
  geom_bar(stat='identity', position = 'fill')+
  labs(fill='Duration')+
  xlab('Type of Movie') +
  ylab(element_blank())

#Average earning group by genre ( first genre)
ggplot(data=New_Genre[-9,], aes(x=reorder(New_Genre, -Average), y=Average/1000000, fill= Average/1000000)) +
  geom_bar(stat='identity') +
  theme(legend.position = c(0.9,0.8),
        axis.text.x= element_text(angle = 90, vjust=0.5))+
  xlab('Genre')+
  ylab('Average Earning in million')
