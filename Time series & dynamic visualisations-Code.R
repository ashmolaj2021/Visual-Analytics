#Data loading 

gdp <- read.csv("https://raw.githubusercontent.com/ashmolaj2021/GDP_Capita/main/income_per_person_gdppercapita_ppp_inflation_adjusted%20(1).csv")

if(!'reshape2'%in%installed.packages()){
  install.packages('reshape2')}
library(reshape2)
gdp <- melt(gdp, id.vars=1, value.name="income", variable.name="year")

#Remove X from year variables 
gdp$year<-as.numeric(gsub("X","",as.character(gdp$year)))
as.Date(data(year, 12, 31))  # end of year

#change column name "ï..country' to 'country'
colnames(gdp)[colnames(gdp) == "ï..country"] <- "country"

if(!'countrycode'%in%installed.packages()){
  install.packages('countrycode')}

#Add a new column Continent to the dataset
library(countrycode)
gdp$continent <- countrycode(sourcevar = gdp[,"country"],
                              origin = "country.name",
                              destination = "continent")

#Defining the general colors to avoid hard coding  
fill_color = '#111111'
decoration_color = '#cccccc'
main1_color = '#f20675'
main2_color = '#1ce3cd'

#Dark theme
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

names(gdp)
head(gdp, n=10)
str(gdp)
summary(gdp)
view(gdp)

#Line chart for all categories over time
g1 <- ggplot(gdp, aes(year, income, group=country))
g1 + geom_line()

#Line chart for all categories over time coloured by the group variable (e.g. continent, genre)
g1 <- ggplot(gdp, aes(year, income, group=country, color=continent))
g1 + geom_line()

#Small multiple line chart by group variable without trend line
ggplot() +
  geom_line(data=gdp, aes(year, income, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) + 
  facet_wrap(~ continent, ncol=5, strip.position = "bottom") +
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "GDP per Capita")

#Small multiple line chart by group variable with trend line
ggplot() +
  geom_line(data=gdp, aes(year, income, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) + 
  facet_wrap(~ continent, ncol=5, strip.position = "bottom") +
  geom_smooth(data=gdp, aes(year, income, group = 1), lwd = 1, method = 'loess', span = 2, se = TRUE) +
  scale_color_manual(values=c("#478adb", "#cccccc", "#f20675", "#bcc048", "#1ce3cd")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "GDP per Capita")

# filter on one group - continent Europe

europe <- dplyr::filter(gdp, continent == "Europe")

#Line chart for all countries in Europe with trend line

ggplot() +
  geom_line(data=europe, aes (year, income, group = country), lwd = 0.3, show.legend = FALSE, color= decoration_color) + 
  facet_wrap(~ continent, ncol=5, strip.position = "bottom") +
  geom_smooth(data=europe, aes(year, income, group = 1), lwd = 1, method = 'loess', span = 1, se = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "GDP per Capita in Europe")


# small multiples by countries in Europe
ggplot(europe, aes(year, income)) +
  geom_line(color=main2_color, size=1.2) +
  facet_wrap(~country) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "GDP per Capita in Europe") 


#Line chart with all lines (without the filter) in the background and lines from the filtered group highlighted through colour, line-width and transparency 
ggplot() +
  geom_line(data = transform(gdp, continent = NULL), aes (year, income, group = country), alpha = 0.5, lwd = 0.3, colour = "yellow") +
  geom_line(data=europe, aes (year, income, group = country), lwd = 0.3, show.legend = FALSE, color= main2_color) +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Europe's GDP per Capita and showing all data in the background") 



write.table(gdp, file, append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.csv(gdp, file = "gdpflourish.csv", row.names = FALSE)
#read.csv("my_data.csv")

directory <-getwd()
directory
library(dplyr)

