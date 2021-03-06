library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(lubridate)
library(kableExtra)
library(leaflet)
options("rgdal_show_exportToProj4_warnings"="none")
library(maps)
library(plotly)
library(dygraphs)
library(xts)
library(DT)
library(sigmoid)
library(rgdal)

## load in covid data, enable if there is need to initiate covid_data df
# covid_data <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv", stringsAsFactors = FALSE, check.names =  FALSE)
# write.csv(covid_data, "data/covid_data_latest.csv")

## load in iso data, enable if there is need to initiate iso_data df
# isourl = "https://gist.githubusercontent.com/metal3d/5b925077e66194551df949de64e910f6/raw/c5f20a037409d96958553e2eb6b8251265c6fd63/country-coord.csv"
# iso_data = read.csv(isourl, sep = ",", strip.white = TRUE)

## FASTER, load in snapshot of covid_data, iso_data, countries geo data, policy
# save(covid_data, countries, policy, file = "covidApp/data/snapshot.RData")
# load("data/snapshot.RData")

# load in geojson polygons for countries
# countries = rgdal::readOGR("data/ne_50m_admin_0_countries", layer = "ne_50m_admin_0_countries")

## load in availability data
# policy <- read.csv("data/covid-vaccination-policy.csv")
# policy$Day = ymd(policy$Day)
# policy$vaccination_policy = as.factor(policy$vaccination_policy)
# 
# covid_data = covid_data %>%
#   mutate(
#     date = ymd(date)
#   )
# 
# # merge covid_data with policy
# covid_policy = full_join(covid_data, select(policy, -Entity),
#                          by = c("iso_code" = "Code", "date" = "Day"))


# calculate VRI -----------------------------------------------------------

# ## function to find r and vri, data cleaning included in function
source("ct_model.R")

# ## calculate VRI
# r_list = ct_model(covid_data, log.y = FALSE, model = "logis", model2 = "custom.start", scal.start = 4, lrc.start = -5)

# ----

## save all cleaned data into RData, load in when app is run
# WARNING: make sure work space is clean before saving
# save.image(file = "data/snapshot.RData")
load("data/snapshot.RData")

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


# VRI ---------------------------------------------------------------------

## combine VRI with countries geo data
# all variables pre-loaded in previous section
# collect all countries
vri_data = bind_rows(r_list$vri_data)

# scale vri?
vri_data$vri_scaled = sigmoid::sigmoid(vri_data$vri, SoftMax = TRUE)

# add data to countries polygons
countries@data = left_join(countries@data, vri_data, by = c("ADM0_A3" = "iso_code"))

## heatmap bins, define boundaries of each interval (x1, x2]
bins = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
# palette function, test on HDI, later change to VRI
pal = colorBin("YlOrRd", domain = vri_data$vri_scaled, bins = bins, reverse = TRUE)
# pal = colorBin("RdYlGn", domain = vri_data$vri_scaled, bins = bins)


# Time Lag ----------------------------------------------------------------

## smoother function, returns smoothed column
Lowess <- function(data, f) {
  lowess_fit <- lowess(data, f = f)
  return(lowess_fit$y)
}

## lag value calculation function
lagValue <- function(FirstDose, SecondDose, windowsize, method=c("euclidean", "manhattan", "minkowski"), p=2) {
  # vector for all measures of distance between matrices
  dist_vector = c()
  i = 1
  while (i <= windowsize){
    # select different subsets of matrices, calculate the distances between the 2 matrices and store the distance. This while loop will contain information for 1st vaccine lag
    FirstDose_subset <- FirstDose[i:nrow(FirstDose),1]
    SecondDose_subset <- SecondDose[1:(1 - i + nrow(SecondDose)),1]
    dist_FirstDose <- proxy::dist(t(FirstDose_subset), t(SecondDose_subset), method = method, p = p)
    dist_vector = c(dist_vector, dist_FirstDose)
    i = i + 1
  }
  
  
  j = 1
  while (j <= windowsize){
    # select different subsets of matrices, calculate the distances between the 2 matrices and store the distance. This while loop will contain information for 2nd vaccine lag
    FirstDose_subset1 <- FirstDose[1:(1 - j + nrow(FirstDose)),1]
    SecondDose_subset1 <- SecondDose[j:nrow(SecondDose),1]
    dist_SecondDose <- proxy::dist(t(FirstDose_subset1), t(SecondDose_subset1), method = method, p = p)
    dist_vector = c(dist_vector, dist_SecondDose)
    j = j + 1
  }
  
  # select min value index which corresponds to value of the lag
  return(which.min(dist_vector))
}  

## lag value convert function, to explain time lag
lagType <- function(lag, windowsize) { 
  # Function to convert indice value given by lagValue to a value for the Time Lag.
  # Any lag values that are greater than windowsize were part of the 2nd half of the 'dist_vector' from the lagValue function, the half of the vector for the 2nd vaccine lag.
  # Therefore need to subtract off all the days from the 1st half of the 'dist_vector' to get number of days for 2nd vaccine lag.
  # No such issue for 1st vaccine lag as all values are within first half.
  if (lag > windowsize){
    return(c(LagType = "Second Dose Lag", Lag = lag - windowsize - 1))
  } else {
    return(c(LagType = "First Dose Lag", Lag = lag - 1))
  }
}

## time lag dataframe
#filter out small islands and invalid countries
lag_covid = covid_data %>%
  filter(population > 1500000) %>%
  filter(gdp_per_capita > 0)

# delete certain countries
countries_lag <- unique(lag_covid$location)
deleted <- c("Afghanistan", "Antigua and Barbuda", "Bangladesh","Benin", "Bhutan", "Bonaire Sint Eustatius and Saba", "Botswana", "Burundi","Burkina Faso", "Cameroon", "Cote d'Ivoire", "Democratic Republic of Congo", "Ethiopia","Eritrea", "Gabon", "Ghana", "Guernsey", "Guinea", "Kenya", "Kuwait", "Liberia", "Laos", "Namibia", "Nepal","Nicaragua", "Niger", "Nigeria", "Palestine", "Philippines", "Pitcairn", "Puerto Rico", "Rwanda", "Saint Helena", "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Sudan", "Tokelau", "Turkmenistan","Tanzania", "Uganda","Yemen", "World", "Zambia")
countries_lag = countries_lag[!(countries_lag %in% deleted)]

# select relevant columns
lag_covid = select(lag_covid, "date", "location", "people_vaccinated_per_hundred", "people_fully_vaccinated_per_hundred")

# filter by fixed time period
start_date = "2021-02-01"
end_date = "2021-08-01"
lag_covid = lag_covid %>% filter(date >= start_date & date < end_date)

# fill NAs with 0
lag_covid$people_vaccinated_per_hundred[is.na(lag_covid$people_vaccinated_per_hundred)] = 0
lag_covid$people_fully_vaccinated_per_hundred[is.na(lag_covid$people_fully_vaccinated_per_hundred)] = 0

## calculate time lag vectors for all countries not deleted
# initiate vectors
lag_vector_1 <- c()
lag_vector_2 <- c()
lag_vector_3 <- c()
# loop through each country and calculate 3 time lags: manhattan distance, euclidean distance and minkowski (p=3) time lags
z = 1
while (z <= length(countries_lag)){
  # only select records for certain country and only select 1st and 2nd vaccine columns
  lagCovid_filtered = filter(lag_covid, location == countries_lag[z])
  combined_matrix <- cbind(lagCovid_filtered[,3], lagCovid_filtered[,4])
  
  # In the dataset, there are missing values. Will replace these missing values (0) with the value from the date before. Do it for both 1st and 2nd vaccine columns
  
  for (i in 1:nrow(combined_matrix)){
    if (i == 1){
    } else{
      if (combined_matrix[i,1] == 0){
        combined_matrix[i,1] = combined_matrix[i-1, 1]
      }
    }
  }
  
  for (j in 1:nrow(combined_matrix)){
    if (j == 1){
    } else{
      if (combined_matrix[j,2] == 0){
        combined_matrix[j,2] = combined_matrix[j-1, 2]
      }
    }
  }
  
  # Apply smoothing function to 1st and 2nd vaccine columns. f = 0.15 is an arbitrary value
  
  combined_matrix_smooth<- as.matrix(apply(combined_matrix, 2, Lowess, f = 0.15))
  
  # Store each column separately as individual matrices
  FirstDose_matrix = as.matrix(combined_matrix_smooth[,1])
  SecondDose_matrix = as.matrix(combined_matrix_smooth[,2])
  
  # Input the individual matrices into the lagValue function to find the lag between the 1st and 2nd dose for a particular country
  lag1 <- lagValue(FirstDose_matrix, SecondDose_matrix, windowsize=100, method = "euclidean")
  lag2 <- lagValue(FirstDose_matrix, SecondDose_matrix, windowsize=100, method = "manhattan")
  lag3 <- lagValue(FirstDose_matrix, SecondDose_matrix, windowsize=100, method = "minkowski", p = 3)
  
  #store value of lag
  lag_vector_1 <- c(lag_vector_1, lag1)
  lag_vector_2 <- c(lag_vector_2, lag2)
  lag_vector_3 <- c(lag_vector_3, lag3)
  
  z = z + 1
}

# label the lag values with the corresponding country
names(lag_vector_1) <- countries_lag
names(lag_vector_2) <- countries_lag
names(lag_vector_3) <- countries_lag

# Apply convert function to each countries Time lag value 
lag_df_1 = mapply(lagType, lag = lag_vector_1, windowsize = 100)
lag_df_2 = mapply(lagType, lag = lag_vector_2, windowsize = 100)
lag_df_3 = mapply(lagType, lag = lag_vector_3, windowsize = 100)

# combine into one dataframe
total_lag = cbind(t(lag_df_1), t(lag_df_2), t(lag_df_3))
colnames(total_lag) = c("LagType", "Lag: Euclidean distance", "LagType", "Lag: Manhattan distance", "LagType", "Lag: Minkowksi distance (P=3)")
total_lag = total_lag %>% as_tibble(rownames = "location") %>% 
  mutate("Lag: Euclidean distance" = as.numeric(`Lag: Euclidean distance`),
         "Lag: Manhattan distance" = as.numeric(`Lag: Manhattan distance`),
         "Lag: Minkowksi distance (P=3)" = as.numeric(`Lag: Minkowksi distance (P=3)`))

# add time lag data to countries
countries@data = left_join(countries@data, total_lag, by = "location")
bins2 = c(10, 30, 40, 50, 60, 80, 100)
# palette function
pal2 = colorBin("OrRd", domain = total_lag$`Lag: Euclidean distance`, bins = bins2)