##===========================================================================================##
##===================Advanced Macroeconomics Tutorial 8 Winter 25/26 ========================##
##===========================================================================================##


# load packages
library(eurostat)
library(tidyverse)

# Clear workspace
rm(list=ls())

# load dataset into variable
namq_10_gdp <- get_eurostat("namq_10_gdp")

# define some keys
country = "DE"
firstobs = "2005-01-01"
adjust = "SCA"
index = "CP_MEUR"
index1 = "PD10_EUR"

# create tables with the needed data
GDP_nom <- subset(namq_10_gdp, geo == country &
                    TIME_PERIOD >= firstobs &
                    unit == index & 
                    s_adj == adjust &
                    na_item == "B1GQ")

GDP_defl <- subset(namq_10_gdp, geo == country &
                     TIME_PERIOD >= firstobs &
                     unit == index1 & 
                     s_adj == adjust &
                     na_item == "B1GQ")

GOV_nom <- subset(namq_10_gdp, geo == country &
                    TIME_PERIOD >= firstobs &
                    unit == index & 
                    s_adj == adjust &
                    na_item == "P3_S13")

GOV_defl <- subset(namq_10_gdp, geo == country &
                     TIME_PERIOD >= firstobs &
                     unit == index1 & 
                     s_adj == adjust &
                     na_item == "P3_S13")

# rename some variables
names(GDP_nom)[names(GDP_nom) == "values"] <- "values_GDP_nom"
names(GDP_defl)[names(GDP_defl) == "values"] <- "values_GDP_defl"
names(GOV_nom)[names(GOV_nom) == "values"] <- "values_GOV_nom"
names(GOV_defl)[names(GOV_defl) == "values"] <- "values_GOV_defl"

# merge the data sets to one
data <- GDP_nom %>% left_join(., GDP_defl, by="TIME_PERIOD") %>% left_join(., GOV_nom, by="TIME_PERIOD") %>% left_join(., GOV_defl, by="TIME_PERIOD")

# rename some variables
names(data)[names(data) == "values_GDP_nom"] <- "GDP_nom"
names(data)[names(data) == "values_GDP_defl"] <- "GDP_defl"
names(data)[names(data) == "values_GOV_nom"] <- "GOV_nom"
names(data)[names(data) == "values_GOV_defl"] <- "GOV_defl"

# mutate time variable and select needed variables
data <- data %>%
  mutate(TIME = year(TIME_PERIOD),
         TIME = ifelse(quarter(TIME_PERIOD)==2,TIME+1/4, TIME),
         TIME = ifelse(quarter(TIME_PERIOD)==3,TIME+2/4, TIME),
         TIME = ifelse(quarter(TIME_PERIOD)==4,TIME+3/4, TIME)) %>%
  select(TIME, GDP_nom, GDP_defl, GOV_nom, GOV_defl)

data <- data %>% arrange(TIME)

# create csv file with the data (adjust path)
write.csv(data, file = "C:\\Users\\nb\\Desktop\\AdvMacro\\Winter 25\\Problem-Set-08\\Eurostat_Data.csv", row.names = FALSE, fileEncoding = "UTF-8")

