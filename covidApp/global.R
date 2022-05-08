library(shiny)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
library(plotly)
library(dygraphs)
library(xts)

## load in covid data, enable if there is need to initiate covid_data df
# covid_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", stringsAsFactors = FALSE, check.names =  FALSE)

## load in iso data, enable if there is need to initiate iso_data df
# isourl = "https://gist.githubusercontent.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv"
# iso_data = read.csv(isourl, sep = ",", strip.white = TRUE)

## FASTER, load in snapshot of covid and iso datasets
# save(iso_data, covid_data, file = "snapshot.RData")
load("data/snapshot.RData")

# load in geojson polygons for countries
# countries = geojsonio::geojson_read("https://datahub.io/core/geo-countries/r/countries.geojson", what = "sp")
countries = geojsonio::geojson_read("data/countries.geojson", what = "sp")

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

## all countries (location name)
loc_all = covid_data %>% 
  select(location) %>% 
  distinct()

## prediction model, anything that does not rely on user input
any_not_na = function(x) {any(!is.na(x))}

# here a simiple linear model, demonstration only
lmfit = lm(people_vaccinated ~ population + gdp_per_capita + total_vaccinations,
           data = covid_data %>% select(where(is.numeric)))