# Code to explore publicly available SHELDUS data
# Author: Jenna Tipaldo
# Updated: Sept 2025, for CACHE Workshops


###############################################################################
################# Load (or install) required packages #########################
###############################################################################

#to help install packages if you don't already have them
packages <- c('tidyverse', 'tigris','sf','RColorBrewer','gganimate','gifski','classInt')

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies=TRUE)
}

#load them one-by-one
library(tidyverse) #tidyverse helps with data cleaning
library(tigris) #tigris pulls census data and shapefiles (requires internet connection)
library(sf) #sf visualizes shapefiles
library(RColorBrewer) #RColorBrewer is useful for color palettes
library(gganimate) #gganimate is useful for animation 
# gganimate cheatsheet: https://rstudio.github.io/cheatsheets/gganimate.pdf
library(gifski) #gifski renders .gif files
library(classInt) #classInt helps with calculating quantiles

###############################################################################
######################### Load in your data ###################################
###############################################################################

# Set your working directory
setwd("") #put the working directory folder path in the quotes

# Read in your data
SHELDUS<-read.csv("") #put the file name in the quotes

###############################################################################
############## Explore the variables and dataframe structure ##################
###############################################################################

# View the variable names
colnames(SHELDUS)

# Get a preview of the data
head(SHELDUS) #head() shows the first few rows of data, alternatively try tail()

# View the structure of the dataframe including variable names, types, and a preview of the data
str(SHELDUS)

###############################################################################
######################### Clean the data ######################################
###############################################################################

#clean variable names (R does not like "." to end a variable name nor numbers to start a variable name)

SHELDUS<- SHELDUS%>%
    rename(
      PropertyDmgAdj2023 = 'PropertyDmg.ADJ.2023.'
    )

###############################################################################
######################### Explore the data ####################################
###############################################################################

# Create a table of how many records per year
table(SHELDUS$Year)

# Create a bar graph of how many records per year across all counties (barplot of records per year)
ggplot(SHELDUS, aes(x=Year))+ #ggplot base of the graph
  geom_bar() +  #tells ggplot to make a bar graph
  scale_x_continuous(breaks=seq(min(SHELDUS$Year),max(SHELDUS$Year),1)) + #puts all years on the x-axis
  scale_y_continuous(labels = scales::comma) #adds commas on Y axis

# Create a bar graph of records per hazard per year across all counties
ggplot(SHELDUS, aes(x=Year))+ #ggplot base of the graph
  geom_bar() +  #tells ggplot to make a bar graph
  facet_wrap(~Hazard) + #creates a graph for each hazard type
  scale_x_continuous(breaks=seq(min(SHELDUS$Year),max(SHELDUS$Year),2)) + #puts all years on the x-axis
  scale_y_continuous(labels = scales::comma) #adds commas on Y axis


# Create a table of how many records per county
table(SHELDUS$county_fips)

###############################################################################
######################### Summarize by county #################################
###############################################################################

# Group by county and calculate summaries across the entire time period
countysummary <-SHELDUS%>%
  group_by(County_FIPS)%>%
  dplyr::summarize(count=n(), #get a count of rows per county
                   hazardtypes=length(unique(Hazard)), #how many types of hazards, note: if your data includes the "All" category, subtract 1 for "All" category 
                   fatalities=sum(Fatalities),
                   propertydamage=sum(PropertyDmgAdj2023) 
  ) 


# Visualize the fatalities in each county over the entire time period using a histogram

ggplot(countysummary, aes(x=fatalities))+
  geom_histogram(binwidth = 1) 
  #argument binwidth controls the width of bins, alternatively specify # of bins with argument bins, for example bins = 10


###############################################################################
################### Summarize by county and by year ##########################$
###############################################################################

countyyearsummary <-SHELDUS%>%
  
  # First pick variables of interest
  select(County_FIPS, Year, Fatalities, PropertyDmgAdj2023)%>%
  
  # Next group by county and year
  group_by(County_FIPS, Year)%>%
  summarise(Fatalities = sum(Fatalities, na.rm = T),
            PropertyDmgAdj2023 = sum(PropertyDmgAdj2023, na.rm = T))%>%
  ungroup()%>%
  
  # Then sort the data by year
  arrange(Year)%>% 
  
  # Next group by county to fill in missing years (SHELDUS only has records in years where there is one)
  group_by(County_FIPS)%>%
  complete(Year=min(SHELDUS$Year):max(SHELDUS$Year))%>% #add rows/observations for counties without any records for a specific year
  fill(County_FIPS, .direction = "updown")%>% #fill in county code to maintain these added rows for missing years
  ungroup()%>%
  
  # Finally fill in missing values with 0s
  mutate(Fatalities= if_else(is.na(Fatalities),0,Fatalities),
         PropertyDmgAdj2023= if_else(is.na(PropertyDmgAdj2023),0,PropertyDmgAdj2023))


# Visualize the amount of property damage per year in each county
ggplot(countyyearsummary, aes(x=Year, y=PropertyDmgAdj2023, group = County_FIPS, color=County_FIPS))+
  geom_line()+
  theme_classic()+ #removes gridlines
  theme(legend.position="none")+ #removes legend
  scale_y_continuous(labels = scales::comma)+ #adds commas on Y axis
  scale_x_continuous(breaks=seq(min(SHELDUS$Year),max(SHELDUS$Year),1))+ 
  ylab("Property Damage (2023 Dollars)")+
  xlab("Year")
