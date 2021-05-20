library(tidyverse)

apple_data<-read.csv("C:/Users/user/Downloads/apple_mobility_data.csv")

long_data<-gather(apple_data,key=day,value = mobility_data, 'X2020.01.13':'X2020.08.20',na.rm=TRUE)

country_averages<-long_data%>%
                  filter(transportation_type=="walking")%>%
                    group_by(country)%>%
                      summarise(walking_average=mean(mobility_data, na.rm=TRUE))%>%
                        filter(!is.na(country))
                  
ggplot(country_averages,aes(y= reorder(country,walking_average),weight=walking_average))+
  geom_bar(fill="blue")+
  xlab("RRWD")

library(lubridate)
long_data$day <- strptime(as.character(long_data$day), "X%Y.%m.%d")



library(rtweet)

senators_data$twitter_url[1]

test<-get_timeline("https://twitter.com/SenDanSullivan")

?get_timeline()

gsub("https://twitter.com/","",senators_data$twitter_url[1])

twitter_url_remover<-function(x){
  handle<-gsub("https://twitter.com/","",x)
  return(handle)
}

twitter_url_remover("https://twitter.com/SenDanSullivan")

tweet_holder<-as.data.frame(NULL)

for (i in 1:5) {
  handle<-twitter_url_remover(senators_data$twitter_url[2])
  tweets<-get_timeline(handle)
  tweet_holder<-rbind(tweet_holder,tweets)
  print(i)
}

senators_data$screen_name<-gsub("https://twitter.com/","",senators_data$twitter_url)

tweet_holder$screen_name

library(tidyverse)
merged_dataset<-left_join(tweet_holder, senators_data)
