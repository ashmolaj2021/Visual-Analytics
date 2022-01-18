library(ggplot2)
# 1. Assignment

#Data loading
Happy_world <- read.csv("https://gist.githubusercontent.com/sandravizz/8b7cf476e4d07331eee00f1fa0249e12/raw/65395ac49ea7154a43f47f455ec26e270c0fb4a3/World%2520Happiness%2520Report")

#There is also a dark theme option with black background
theme_set(dark_theme_gray()+ theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 14, hjust = 0, color = decoration_color),
  axis.ticks = element_blank(),
  axis.title = element_text(size = 10, hjust = 0.5, color = decoration_color),
  legend.title = element_blank(),
  panel.background =element_rect(fill = fill_color),
  strip.background =element_rect(fill = fill_color), 
  plot.background = element_rect(fill = fill_color),
  legend.background = element_rect(fill = fill_color)
))
#Data checking
names(Happy_world)
head(Happy_world, n=10)
str(Happy_world)
summary(Happy_world)



ggplot(Happy_world, aes(Happiness.Score)) + 
  geom_freqpoly(colour = main2_color,binwidth = 1) 


ggplot(Happy_world, aes(Happiness.Score)) +
  geom_freqpoly(colour = main2_color,binwidth=1) +
  facet_wrap(. ~Region)



ggplot(Happy_world, aes(Economy..GDP.per.Capita.)) + 
  geom_freqpoly(colour = main2_color,binwidth=0.3)

ggplot(Happy_world, aes(Economy..GDP.per.Capita.)) + 
  geom_freqpoly(colour = main2_color,binwidth=0.3) +
  facet_wrap(. ~Region)


 
#Histogram 

ggplot(Happy_world, aes(Happiness.Score)) + 
  geom_histogram(colour = "black", fill = main2_color,binwidth = 0.3)

ggplot(Happy_world, aes(Happiness.Score,fill = Region)) + 
  geom_histogram(colour = "black",position = "dodge", binwidth = 0.3) +
  facet_wrap(. ~Region)


ggplot(Happy_world, aes(Economy..GDP.per.Capita.)) + 
  geom_histogram(colour = "black", fill = main2_color,binwidth = 0.1)


ggplot(Happy_world, aes(Economy..GDP.per.Capita.,fill = Region)) + 
  geom_histogram(colour = "black",position = "dodge", binwidth = 0.1)+
  facet_wrap(. ~Region)










#Relationship analysis

#Basic scatter plot 
ggplot(Happy_world, aes(x=Economy..GDP.per.Capita.,y=Happiness.Score)) + 
  geom_point(color=main2_color)

#Basic scatter plot - adjusting the size

ggplot(Happy_world, aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+
  geom_point(size=1,color=main2_color)
  
#Basic scatter plot - adjusting the opacity

ggplot(Happy_world, aes(x=Economy..GDP.per.Capita.,y=Happiness.Score))+
  geom_point(size=1,alpha=1,color=main2_color)+ylim(4, 8) +xlim(0.5, 2)



#Small multiples- two variables
#theme_set(theme_bw())


ggplot(Happy_world, aes(x=Economy..GDP.per.Capita.,y=Happiness.Score)) +
  geom_point(color=main2_color, size=1,alpha=1)+
  facet_wrap(~ Region,ncol=3)

ggplot(Happy_world, aes(x=Economy..GDP.per.Capita.,y=Happiness.Score)) +
  geom_point(color=main2_color, size=1,alpha=1)+
  facet_wrap(~ Region,ncol=3)+stat_smooth(color=decoration_color)





