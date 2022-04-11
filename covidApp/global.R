library(tidyverse)
library(lubridate)

## load in covid data, enable if there is need to initiate covid_data df
# covid_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", stringsAsFactors = FALSE, check.names =  FALSE)

## load in iso data, enable if there is need to initiate iso_data df
isourl = "https://gist.githubusercontent.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv"
iso_data = read.csv(isourl, sep = ",", strip.white = TRUE)

covid_data = covid_data %>% 
  mutate(
    date = ymd(date)
  )

## numerical variables
# num_vars = colnames(covid_data)[sapply(covid_data, is.numeric)]
num_vars = covid_data %>% 
  select(where(is.numeric)) %>% 
  colnames()

## all countries (iso codes)
iso_all = covid_data %>% 
  select(iso_code) %>% 
  distinct()
