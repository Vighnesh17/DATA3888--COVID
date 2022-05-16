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
# write.csv(covid_data, "data/covid_data_latest.csv")

## load in iso data, enable if there is need to initiate iso_data df
# isourl = "https://gist.githubusercontent.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv"
# iso_data = read.csv(isourl, sep = ",", strip.white = TRUE)

## FASTER, load in snapshot of covid_data, iso_data, countries geo data, policy
# save(iso_data, covid_data, file = "data/snapshot.RData")
load("data/snapshot.RData")

# load in geojson polygons for countries
# countries = geojsonio::geojson_read("https://datahub.io/core/geo-countries/r/countries.geojson", what = "sp")
# countries = geojsonio::geojson_read("data/countries.geojson", what = "sp")

## load in availability data
# policy <- read.csv("../Availability/covid-vaccination-policy.csv")

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

## heatmap bins, define boundaries of each interval (x1, x2]
bins = seq(0,1,0.2) %>% append(Inf)
# test on HDI, later change to VRI
pal = colorBin("YlOrRd", domain = covid_data$human_development_index, bins = bins)

## prediction model, anything that does not rely on user input
any_not_na = function(x) {any(!is.na(x))}

# here a simple linear model, demonstration only
lmfit = lm(people_vaccinated ~ population + gdp_per_capita + total_vaccinations,
           data = covid_data %>% select(where(is.numeric)))

## varname for time lag df
varname_lag = c("people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred")

## smoother function, returns smoothed column
Lowess <- function(data, f) {
  lowess_fit <- lowess(data, f = f)
  return(lowess_fit$y)
}

## lag value calculation function
lagValue <- function(FirstDose, SecondDose, windowsize) {
  # vector for all measures of distance between matrices
  dist_vector = c()
  i = 1
  while (i <= windowsize){
    # select different subsets of matrices, calculate the distances between the 2 matrices and store the distance. This while loop will contain information for 1st vaccine lag
    FirstDose_subset <- FirstDose[i:nrow(FirstDose),1]
    SecondDose_subset <- SecondDose[1:(1 - i + nrow(SecondDose)),1]
    dist_FirstDose <- proxy::dist(t(FirstDose_subset), t(SecondDose_subset), method = "Minkowski", p = 2)
    dist_vector = c(dist_vector, dist_FirstDose)
    i = i + 1
  }
  
  
  j = 1
  while (j <= windowsize){
    # select different subsets of matrices, calculate the distances between the 2 matrices and store the distance. This while loop will contain information for 2nd vaccine lag
    FirstDose_subset1 <- FirstDose[1:(1 - j + nrow(FirstDose)),1]
    SecondDose_subset1 <- SecondDose[j:nrow(SecondDose),1]
    dist_SecondDose <- proxy::dist(t(FirstDose_subset1), t(SecondDose_subset1), method = "Minkowski", p = 2)
    dist_vector = c(dist_vector, dist_SecondDose)
    j = j + 1
  }
  
  # select min value index which corresponds to value of the lag
  return(which.min(dist_vector))
}

## lag value convert function
lagType <- function(lag, windowsize) { # Function to convert indice value given by lagValue to a value for the Time Lag.
  # Any lag values that are greater than windowsize were part of the 2nd half of the 'dist_vector' from the lagValue function, the half of the vector for the 2nd vaccine lag.
  # Therefore need to subtract off all the days from the 1st half of the 'dist_vector' to get number of days for 2nd vaccine lag.
  # No such issue for 1st vaccine lag as all values are within first half.
  if (lag > windowsize){
    return(c(LagType = "Second Dose Lag", Lag = lag - windowsize - 1))
  } else {
    return(c(LagType = "First Dose Lag", Lag = lag - 1))
  }
}