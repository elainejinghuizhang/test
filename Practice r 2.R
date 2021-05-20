library(tidyverse)
library(ggplot2)

ggplot(opioid_data,aes(y=num_pills, x=obamacare))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x)+
  theme_minimal()

summary(lm(num_pills~obamacare+population,data = opioid_data))



tinytex::install_tinytex()
tinytex:::is_tinytex()



install.packages('rtweet')
library(rtweet)


senators_data$twitter_url[1]

test<-get_timeline("https://twitter.com/SenDanSullivan")
 
gsub("https://twitter.com/", "",senators_data$twitter_url[1])
 
twitter_url_remover<-function(x){
  handle<-gsub("https://twitter.com/", "",x)
  return(handle)
}
twitter_url_remover("https://twitter.com/SenDanSullivan")

tweet_holder<-as.data.frame(NULL)

for (i in 1:5){
  handle<-twitter_url_remover(senators_data$twitter_url[2])
  tweets<-get_timeline(handle)
  tweet_holder<-rbind(tweet_holder,tweets)
  print(i)
}
?get_timeline
  
senators_data$screen_name<-gsub("https://twitter.com/","",senators_data$twitter_url)
tweet_holder$screen_name

library(tidyverse)
merged_data<-left_join(tweet_holder,senators_data)

apple_data <- read.csv("C:/Users/user/Downloads/apple_mobility_data.csv")
long_data<-gather(apple_data, key=day, value=mobility_data, `X2020.01.13`:`X2020.08.20`,na.rm=TRUE)

country_averages<-long_data %>%
                  filter(transportation_type=="walking")%>%
                  group_by(country)%>%
                    summarise(walking_average=mean(mobility_data,na.rm=TRUE))%>%
                      filter(!is.na(country))

ggplot(country_averages, aes(y=reorder(country, walking_average),weight=walking_average))+
  geom_bar(fill="blue")+
  xlab("RRWDS")+
  theme_minimal()

library(lubridate)
as_date(long_data$day, format= "X%Y.%m.%d")

long_data$day<-as_date(long_data$day, format= "X%Y.%m.%d")

Italy_Spain_data<-long_data %>%
  filter(country==c("Italy", "Spain"), transportation_type=="walking")%>%
    group_by(country,day)%>%
      summarise(walking_average=mean(mobility_data,na.rm=TRUE))

ggplot(Italy_Spain_data, aes(x=day, y=walking_average,group=country,colour=country))+
         geom_line()+
          facet_wrap(~country)
         
       
       