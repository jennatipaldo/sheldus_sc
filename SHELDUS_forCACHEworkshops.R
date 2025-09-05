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

#if (!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
#load packages 

#lapply(packages, library, character.only = TRUE) #load them all in one step

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
head(SHELDUS)

# View the structure of the dataframe including variable names, types, and a preview of the data
str(SHELDUS)

###############################################################################
######################### Explore the data ####################################
###############################################################################

# Create a table of how many records per year
table(SHELDUS$Year)

# Create a graph of how many records per year across all counties (barplot of records per year)
ggplot(SHELDUS, aes(x=Year))+ #ggplot base of the graph
  geom_bar() +  #tells ggplot to make a bar graph
  scale_x_continuous(breaks=seq(min(SHELDUS$Year),max(SHELDUS$Year),1)) + #puts all years on the x-axis
  scale_y_continuous(labels = scales::comma) #adds commas on Y axis

# Create a table of how many records per county
table(SHELDUS$county_fips)


