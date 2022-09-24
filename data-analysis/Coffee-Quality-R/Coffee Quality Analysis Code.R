library(ggplot2)
library(readxl)
library(tidyverse)
library(magrittr)
library(lubridate)
library(reshape)
library(tidyr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)

setwd("C:/Users/c44519/Desktop/Bellevue/Statisitcs/Term Project")

# Load in Data
arabica <- read.csv('arabica_data_cleaned.csv', na.strings=c(""," ","NA"))
coffee_price <- read_xlsx('Average price of coffee worldwide from 1998 to 2018, by type of coffee.xlsx', sheet = 'Cleaned Data')
arabica_production <- read_xlsx('World Arabica coffee production from 2005 to 2020.xlsx', sheet = 'Cleaned Data')

# Explore Data
head(arabica)
summary(arabica)
str(arabica)
colSums(is.na(arabica))
glimpse(arabica)
unique(arabica$Country.of.Origin)
unique(arabica$Harvest.Year)
unique(arabica$Bag.Weight)
unique(arabica$Variety)

arabica_metrics <- arabica[, c('Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 'Clean.Cup', 'Sweetness','Cupper.Points', 'Total.Cup.Points', 
                       'Moisture', 'Category.One.Defects', 'Quakers', 'Category.Two.Defects')]

metric_columns <- c('Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 'Clean.Cup', 'Sweetness','Cupper.Points', 'Total.Cup.Points', 
                    'Moisture', 'Category.One.Defects', 'Quakers', 'Category.Two.Defects')
summary(arabica_metrics)

# Drop Columns
arabica <- arabica[, c('Country.of.Origin', 'Number.of.Bags', 'Bag.Weight', 'Harvest.Year', 'Grading.Date', 'Variety', 'Processing.Method', 
                       'Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 'Clean.Cup', 'Sweetness','Cupper.Points', 'Total.Cup.Points', 
                       'Moisture', 'Category.One.Defects', 'Quakers', 'Color', 'Category.Two.Defects', 'Expiration', 'Certification.Body', 'unit_of_measurement', 'altitude_mean_meters')]
# Transformations
arabica <- arabica[-c(1311),]
arabica[arabica == 'Cote d?Ivoire'] <- "Ivory Coast"
arabica[arabica == 'May-August'] <- NA
arabica[arabica == 'January Through April'] <- NA
arabica[arabica == 'Abril - Julio'] <- NA
arabica[arabica == 'mmm'] <- NA
arabica[arabica == 'TEST'] <- NA
arabica[arabica == 'August to December'] <- NA
arabica[arabica == 'Mayo a Julio'] <- NA
arabica[arabica == '2009/2010'] <- 2010
arabica[arabica == 'Fall 2009'] <- 2009
arabica[arabica == '4T/10'] <- 2010
arabica[arabica == '47/2010'] <- 2010
arabica[arabica == '1t/2011'] <- 2011
arabica[arabica == '2017 / 2018'] <- 2018
arabica[arabica == '2016 / 2017'] <- 2018
arabica[arabica == '4t/2011'] <- 2011
arabica[arabica == '2009 / 2010'] <- 2010
arabica[arabica == 'December 2009-March 2010'] <- 2010
arabica[arabica == '23 July 2010'] <- 2010
arabica[arabica == 'Abril - Julio /2011'] <- 2011
arabica[arabica == '4T72010'] <- 2010
arabica[arabica == '2010-2011'] <- 2011
arabica[arabica == '2014/2015'] <- 2015
arabica[arabica == '1T/2011'] <- 2011
arabica[arabica == 'Spring 2011 in Colombia.'] <- 2011
arabica[arabica == '08/09 crop'] <- 2009
arabica[arabica == 'March 2010'] <- 2010
arabica[arabica == '2015/2016'] <- 2016
arabica[arabica == '2009-2010'] <- 2010
arabica[arabica == '2011/2012'] <- 2012
arabica[arabica == '4t/2010'] <- 2010
arabica[arabica == '3T/2011'] <- 2011
arabica[arabica == 'Sept 2009 - April 2010'] <- 2010
arabica[arabica == '2009 - 2010'] <- 2010
arabica[arabica == 'January 2011'] <- 2011
arabica[arabica == '4T/2010'] <- 2010
arabica[arabica == '2016/2017'] <- 2017
arabica[arabica == '2013/2014'] <- 2014
arabica[arabica == '23-Jul-10'] <- 2010
arabica[arabica == 'Mar-10'] <- 2010
arabica[arabica == 'Jan-11'] <- 2011

# Make coffee price format longer
coffee_price <- pivot_longer(coffee_price, c("Colombian Mild Arabicas", "Other Mild Arabicas", "Brazilian Natural Arabicas", "Robustas"), names_to = "Type", values_to = "Price")

# Date Formatting
arabica$Grading.Date <- parse_date_time(arabica$Grading.Date, orders = "mdy")
arabica$Expiration <- parse_date_time(arabica$Expiration, orders = "mdy")
arabica$Harvest.Year <- as.Date(arabica$Harvest.Year, format = "%Y")
coffee_price$Year <- as.Date(coffee_price$Year, format = "%Y")
arabica_production$Year <- as.Date(as.character(arabica_production$Year), format = "%Y")

coffee_price$Type <- as.factor(coffee_price$Type)
arabica$Country.of.Origin <- as.factor(arabica$Country.of.Origin)
arabica$Variety <-as.factor(arabica$Variety)
arabica$Processing.Method <-as.factor(arabica$Processing.Method)
arabica$Color <-as.factor(arabica$Color)
arabica$Certification.Body <-as.factor(arabica$Certification.Body)
arabica$unit_of_measurement <-as.factor(arabica$unit_of_measurement)

str(coffee_price)
str(arabica_production)
str(arabica)
str(arabica$Country.of.Origin)

# EDA
## coffee price
ggplot(coffee_price, aes(Year,Price, colour = Type)) + geom_line() + ggtitle("Coffee Price By Type")

ggplot(coffee_price, aes(Price)) + geom_histogram(bins=10) + ggtitle("Coffee Price Histogram")

ggplot(coffee_price, aes(Year, Price,colour = Type)) + geom_point() + ggtitle("Coffee Price By Type")

## coffee production
ggplot(arabica_production, aes(Year, Production)) + geom_line() + ggtitle("Coffee Production")

ggplot(arabica_production, aes(Production)) + geom_histogram(bins=5) + ggtitle("Coffee Production Histogram") + labs(y='Count of Observations')

ggplot(arabica_production, aes(Year, Production)) + geom_point() + ggtitle("Coffee Production")

## arabica coffee quality
ggplot(arabica, aes(Harvest.Year, Total.Cup.Points)) + geom_line() + ggtitle("Arabica Total Quality Points")
ggplot(arabica, aes(Grading.Date, Total.Cup.Points, colour = Color)) + geom_point() + ggtitle("Arabica Total Quality Points") + labs(y='Total Points',x='Grading Date')

ggplot(arabica, aes(Total.Cup.Points)) + geom_histogram(bins=20) + ggtitle("Arabica Total Quality Histogram") + labs(x='Total Points',y='Count of Coffee')

ggplot(arabica, aes(Grading.Date, Total.Cup.Points, colour = Country.of.Origin)) + geom_point() + ggtitle("Coffee Price By Type")

ggplot(arabica, aes(Color, Total.Cup.Points)) + geom_bar(stat = "identity")

ggplot(arabica, aes(Harvest.Year, Total.Cup.Points)) + geom_bar(stat = "identity") + labs(x='Total Points',y='Harvest Year') + ggtitle('Total Points by Harvest Year')
ggplot(arabica, aes(Processing.Method, Total.Cup.Points)) + geom_bar(stat = "identity") + labs(x='Total Points',y='Processing Metho') + ggtitle('Total Points by Processing Method')

# Different ways to look at data
arabica %>% group_by(Country.of.Origin) %>% summarise(total_points=sum(Total.Cup.Points), mean_points=mean(Total.Cup.Points))
arabica %>% group_by(Color) %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% group_by(Processing.Method) %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% group_by(Harvest.Year) %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% group_by(year(Grading.Date)) %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% group_by(year(Expiration)) %>% summarise(sum(Total.Cup.Points), mean(Total.Cup.Points))
arabica %>% group_by(Country.of.Origin) %>% summarise(mean(Aroma))
summary(arabica_metrics)

# Slicing and Dicing
# all us territory in one dataset 
arabica$World_Location <- ifelse(arabica$Country.of.Origin == 'United States' |
                               arabica$Country.of.Origin == 'United States (Hawaii)' | 
                               arabica$Country.of.Origin == 'United States (Puerto Rico)', 'All US',
                             'Rest of World')

rest_of_world <- arabica %>% filter(Country.of.Origin != 'United States' & Country.of.Origin != 'United States (Hawaii)' & Country.of.Origin != 'United States (Puerto Rico)' )

# Add columns
arabica_production$pct_change <- (arabica_production$Production/lead(arabica_production$Production) - 1) * 100

arabica$Type <- ifelse(arabica$Variety == 'Bourbon' |
                         arabica$Variety == 'Pacamara' |
                         arabica$Variety == 'SL28' |
                         arabica$Variety == 'SL34' |
                         arabica$Variety == 'SL14' |
                         arabica$Variety == 'Yellow Bourbon' |
                         arabica$Variety == 'Mundo Novo' |
                         arabica$Variety == 'Ethiopian Heirlooms' |
                         arabica$Variety == 'Typica' | 
                         arabica$Variety == 'Catimor' |
                         arabica$Variety == 'Caturra', 'Bourbon/Typica', 
                       ifelse(arabica$Variety == 'Ethiopian Yirgacheffe' |
                                arabica$Variety == 'Sumatra' |
                                arabica$Variety == 'Hawaiian Kona' |
                                arabica$Variety == 'Gesha' |
                                arabica$Variety == 'Catuai' |
                                arabica$Variety == 'Sumatra Lintong' |
                                arabica$Variety == 'Java' |
                                arabica$Variety == 'Pacas' |
                                arabica$Variety == 'Mandheling' |
                                arabica$Variety == 'Ruiru 11' |
                                arabica$Variety == 'Arusha' |
                                arabica$Variety == 'Marigojipe' |
                                arabica$Variety == 'Blue Mountain' | 
                                arabica$Variety == 'Pache Comun', 'Arabica',
                              ifelse(is.na(arabica$Variety) |
                                       arabica$Variety == 'Other', 'Other', 
                                     ifelse(arabica$Variety == 'Peaberry' |
                                              arabica$Variety == 'Moka Peaberry', 'Peaberry', 'Other'))))

# merge data
arabica_price <- coffee_price %>% filter(Type != 'Robustas')
arabica_price <- arabica_price %>% select(c('Year', 'Price'))
arabica_price <- arabica_price %>% group_by(Year) %>% summarise(avg_price = mean(Price))
price_production <- merge(arabica_price, arabica_production, by = 'Year')

ggplot(all_us, aes(Grading.Date, Total.Cup.Points, colour = Color)) + geom_point() + ggtitle("Arabica Total Quality Points") + labs(y='Total Points',x='Grading Date')
ggplot(rest_of_world, aes(Grading.Date, Total.Cup.Points, colour = Color)) + geom_point() + ggtitle("Arabica Total Quality Points") + labs(y='Total Points',x='Grading Date')


ggplot(arabica, aes(Harvest.Year, Total.Cup.Points, colour = World_Location)) + geom_line() + ggtitle("Arabica Total Quality Points")
freq(arabica)

freq(arabica$Processing.Method)
freq(arabica$Color)
freq(arabica$Type)
freq(arabica$Country.of.Origin)
plot_num(arabica[, c('Aroma', 'Flavor', 'Aftertaste', 'Acidity', 'Body', 'Balance', 'Uniformity', 'Clean.Cup',
                     'Sweetness', 'Cupper.Points', 'Total.Cup.Points', 'Moisture', 'Quakers')])

ggplot(data = arabica, aes(x=Country.of.Origin,y=Total.Cup.Points, color=Country.of.Origin)) + 
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')
ggplot(data = arabica, aes(x=Type,y=Total.Cup.Points, color=Type)) + 
  geom_boxplot()+ 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')+facet_wrap(~Color,nrow = 1)

ggplot(data=arabica, aes(x=Country.of.Origin, y=(mean(Total.Cup.Points)))) +
  geom_bar(stat = "identity") +
  coord_flip() +  labs(title="Are Males better at math?", x="", y="% difference from female")


avg_country <- arabica %>% group_by(Country.of.Origin) %>% summarise(avg = mean(Total.Cup.Points)) %>% arrange(desc(avg))

ggplot(avg_country, aes(reorder(x=Country.of.Origin, avg), y=avg)) +
  geom_bar(stat = "identity") +
  coord_flip() +  labs(title="Avergae Total Points per Country", x="", y="Average Points")

avg_color <- arabica %>% group_by(Color) %>% summarise(avg = mean(Total.Cup.Points))

ggplot(avg_color, aes(reorder(x=Color, avg), y=avg)) +
  geom_bar(stat = "identity") +
  coord_flip() +  labs(title="Avergae Total Points per Color", x="", y="Average Points")

avg_type <- arabica %>% group_by(Type) %>% summarise(avg = mean(Total.Cup.Points))

ggplot(avg_type, aes(reorder(x=Type, avg), y=avg)) +
  geom_bar(stat = "identity") +
  coord_flip() +  labs(title="Avergae Total Points per Color", x="", y="Average Points")

avg_process <- arabica %>% group_by(Processing.Method) %>% summarise(avg = mean(Total.Cup.Points))

ggplot(avg_process, aes(reorder(x=Processing.Method, avg), y=avg)) +
  geom_bar(stat = "identity") +
  coord_flip() +  labs(title="Avergae Total Points per Color", x="", y="Average Points")
