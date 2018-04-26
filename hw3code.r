#commmeeeeeeeeeeent github1
getwd()
setwd('C:\\Users\\sprot\\Documents\\R\\data_viz\\hw_group')
library(ggplot2)
library(tidyverse)
library(grid)
rm(list = ls())
movie <- read.csv("movie.csv")
head(movie)
str(movie)
sum(is.na(movie$Adjusted_Gross))
movie$Adjusted_Gross2<-as.numeric(gsub(",", "", movie$Adjusted_Gross))
str(movie$Adjusted_Gross2)
# Question 1: Does higher rating movie also have a higher adjusted gross revenue?
ggplot(movie, aes(x=IMDb_Rating, y=Adjusted_Gross2))+geom_point()+geom_smooth(Fill = NA)

ggplot(movie, aes(x=IMDb_Rating, y=Adjusted_Gross2, color=Genre))+geom_point()+facet_wrap(~Genre, ncol = 5)+geom_smooth(fill = NA)

# Question 2: Shall we make a short movie or a long movie?
class(movie$Runtime_min)
str(movie$Runtime_min)
class(movie$Adjusted_Profit2)
movie$Profit2<-as.numeric(gsub(",", "", movie$Profit))
movie$Budget2<-as.numeric(gsub(",", "", movie$Budget))
#ROI
movie$ROI <- (movie$Profit2/movie$Budget2)
# ggplot(movie, aes(x=Runtime_min, y=Profit2))+geom_point()+geom_smooth(Fill = NA)
# ggplot(movie, aes(x=Runtime_min, y=Profit2, color = Genre))+
#   geom_point()+facet_wrap(~Genre, ncol = 5)+geom_smooth(fill = NA)+ylim(0, 1700)+ xlim(75, 200)
# ggplot(movie, aes(x=Runtime_min, y=ROI, color = Genre))+
#   geom_point()+facet_wrap(~Genre, ncol = 5)+geom_smooth(fill = NA)
# 
# ggplot(movie, aes(x=Runtime_min, y=ROI, color = Genre))+
#   geom_point()+
#   geom_smooth(fill = NA)+facet_wrap(~Genre, ncol = 5)+
#   ylim(0, 90)

ggplot(movie, aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+
  facet_wrap(~Genre, ncol = 5)+ylim(0, 10)

filter1<-movie$Genre %in% c("action")
movie2<-movie[filter1,]

ggplot(movie2, aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(80, 190)+ylim(0, 30)

ggplot(movie2, aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+theme(aspect.ratio=1/2)


#one above the other, ROI & Profit ($)
#Need to make labels / ticks larger so they are visible. Also differentiate colors? (Not necessary)
anim_roi = movie %>%
  filter(Genre=='animation') %>%
ggplot(aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(80, 120)+ylim(0, 15)+
  ylab('ROI (Profit / Budget)') + xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

anim_prof = movie %>%
  filter(Genre=='animation') %>%
  ggplot(aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(80, 120)+ylim(0, 1000)+ylab('Profit ($mill)') + xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(anim_roi), ggplotGrob(anim_prof), size = "last"))

action_roi = movie %>%
  filter(Genre=='action') %>%
  ggplot(aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(85, 180)+ylim(0, 25)+ylab('ROI (Profit / Budget)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

action_prof = movie %>%
  filter(Genre=='action') %>%
  ggplot(aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(80, 180)+ylim(0, 1000)+ylab('Profit ($mill)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(action_roi), ggplotGrob(action_prof), size = "last"))

adv_roi = movie %>%
  filter(Genre=='adventure') %>%
  ggplot(aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(85, 175)+ylim(0, 20)+ylab('ROI (Profit / Budget)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

adv_prof = movie %>%
  filter(Genre=='adventure') %>%
  ggplot(aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(85, 180)+ylim(0, 1000)+ylab('Profit ($mill)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(adv_roi), ggplotGrob(adv_prof), size = "last"))

com_roi = movie %>%
  filter(Genre=='comedy') %>%
  ggplot(aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(75, 150)+ylim(0, 20)+ylab('ROI (Profit / Budget)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

com_prof = movie %>%
  filter(Genre=='comedy') %>%
  ggplot(aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(75, 150)+ylim(0, 600)+ylab('Profit ($mill)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(com_roi), ggplotGrob(com_prof), size = "last"))

drama_roi = movie %>%
  filter(Genre=='drama') %>%
  ggplot(aes(x=Runtime_min, y=ROI, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(95, 150)+ylim(0, 20)+ylab('ROI (Profit / Budget)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

drama_prof = movie %>%
  filter(Genre=='drama') %>%
  ggplot(aes(x=Runtime_min, y=Profit2, color = Genre))+
  geom_point()+geom_smooth(fill = NA, color='Black')+xlim(95, 150)+ylim(0, 750)+ylab('Profit ($mill)')+ xlab('Runtime (minutes)') +
  theme(axis.text=element_text(size=11, face="bold"),axis.title=element_text(size=13,face="bold"))

grid.newpage()
grid.draw(rbind(ggplotGrob(drama_roi), ggplotGrob(drama_prof), size = "last"))

# Question 3: If a movie does well in US, does it also usually do well overseas?
movie$Overseas_rev2<-as.numeric(gsub(",", "", movie$Overseas_rev))
movie$US_rev2 <- as.numeric(gsub(",","", movie$US_rev))
ggplot(movie, aes(US_rev2, Overseas_rev2))+geom_point()+geom_smooth(Fill = NA)
ggplot(movie, aes(US_rev2, Overseas_rev2, color=Genre))+geom_point()+geom_smooth(Fill = NA)+facet_wrap(~Genre, ncol = 5)

# Question 4: Now you have explored many variables and how they affect revenue, please recommend a strategy for Netflix’s next investment in a movie
