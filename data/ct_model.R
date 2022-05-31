ct_model = function(df, log.y = FALSE, model = c("logis", "asymp", "linear"), model2 = "none") {
  # cleaning
  covid_clean = df %>% 
    filter(str_length(iso_code) <= 3) %>% 
    select(iso_code, location, date, people_vaccinated, population, aged_65_older, gdp_per_capita, cardiovasc_death_rate, population_density, hospital_beds_per_thousand, human_development_index, extreme_poverty, diabetes_prevalence, life_expectancy) %>% 
    filter(people_vaccinated != 0 & !is.na(people_vaccinated)) %>% 
    group_by(location) %>% 
    mutate(t_days = difftime(date, min(date), units = "days") %>% as.integer())
  
  # initiate r_list, to store location, r, fit, ... respectively
  r_list = list("location" = c(), "r" = c(), "fit" = c(), "y.real" = c(), "date" = c(), "t_days" = c(), "vri_data" = c(), "iso_code" = c())
  
  ## formulae, based on selected model, people_vaccinated, and log.y
  # if log y
  if (log.y) {
    # for lm
    f2 = formula(log(people_vaccinated) ~ t_days)
    # for nls
    if (model == "logis") {
      f1 = formula(log(people_vaccinated) ~ SSlogis(t_days, Asym, xmid, scal))
      # for custom starting values
      f3 = formula(log(people_vaccinated) ~ Asym / (1 + exp((xmid - t_days)/scal)))
    } else if (model == "asymp") {
      f1 = formula(log(people_vaccinated) ~ SSasymp(t_days, Asym, R0, lrc))
      # for custom starting values
      f3 = formula(log(people_vaccinated) ~ Asym + (R0 - Asym)*exp( -t_days*exp(lrc) ))
    }
    # if not log y
  } else {
    # for lm
    f2 = formula(people_vaccinated ~ t_days)
    # for nls
    if (model == "logis") {
      f1 = formula(people_vaccinated ~ SSlogis(t_days, Asym, xmid, scal))
      # for custom starting values
      f3 = formula(people_vaccinated ~ Asym / (1 + exp((xmid - t_days)/scal)))
    } else if (model == "asymp") {
      f1 = formula(people_vaccinated ~ SSasymp(t_days, Asym, R0, lrc))
      # for custom starting values
      f3 = formula(people_vaccinated ~ Asym + (R0 - Asym)*exp( -t_days*exp(lrc) ))
    }
  }
  
  ## for each country (location)
  for (loc in unique(covid_clean$location)) {
    # subset cleaned data to one country
    covid_subset = covid_clean %>% filter(location == loc) %>% ungroup()
    iso = unique(covid_subset$iso_code)
    
    # nls model total 3 parameters, need to have at least 4 observations
    if (nrow(covid_subset) <= 3) {
      fit = NULL
    } else {
      # try to fit model
      fit = tryCatch(
        # main function, fit asymptote/logistic/linear regression model
        expr = {
          if (model == "linear") {
            lm(f2, data = covid_subset)
          } else {
            nls(f1, data = covid_subset)
          }
        },
        # if error (usually SS failed), fit another model
        error = function(error_message){
          
          # fit linear model
          if (model2 == "linear") {
            return( lm(f2, data = covid_subset) )
            # fit none
          } else if (model2 == "none") {
            return( NULL )
            
            # fit logis with custom made starting values
          } else if (model2 == "custom.start" & model == "logis") {
            y = ifelse(log.y, log(covid_subset$people_vaccinated), covid_subset$people_vaccinated)
            Asym0 = max(y)
            halfy.point = which.min(abs(y - 1/2*max(y)))
            xmid0 = covid_subset$t_days[halfy.point]
            scal0 = 4    # chosen based on manual trial and error on some failed SSlogis cases
            return( tryCatch(
              expr = nls(f3, data = covid_subset, start = list(Asym = Asym0, xmid = xmid0, scal = scal0), control = list(maxiter = 100, minFactor = 1/2048, warnOnly = TRUE)),
              # if still fails, then return NULL (most likely not enough points)
              error = function(e) { return( NULL ) }
            ) )
            
            # fit asymp with custom made starting values
          } else if (model2 == "custom.start" & model == "asymp") {
            y = ifelse(log.y, log(covid_subset$people_vaccinated), covid_subset$people_vaccinated)
            Asym0 = max(y)
            R00 = min(y)
            lrc0 = -5    # chosen based on manual trial and error on some failed SSasymp cases
            return( tryCatch(
              expr = nls(f3, data = covid_subset, start = list(Asym = Asym0, R0 = R00, lrc = lrc0), control = list(maxiter = 100, minFactor = 1/2048, warnOnly = TRUE, scaleOffset = 0.1)),
              # if still fails, then return NULL (most likely not enough points)
              error = function(e) { return( NULL ) }
            ) )
          }
          
        }
      )
    }
    
    # calculate my r value, may need transform before put into vri formula
    if (class(fit) == "lm") {
      r = coef(fit)["t_days"]
    } else if (class(fit) == "nls") {
      if (model == "logis") { scal = coef(fit)["scal"]
      r = 1/scal
      } else if (model == "asymp") { lrc = coef(fit)["lrc"]
      r = exp(lrc)
      }
    } else if (class(fit) == "NULL") {
      r = NA
    }
    
    # vri = r * last people_vaccinated / end population
    last_entry = tail(covid_subset, 1)
    vri = r * last_entry$people_vaccinated / last_entry$population
    
    median_data = covid_subset %>% 
      select(-date, -t_days, -location, -iso_code, -people_vaccinated) %>% 
      summarise(across(everything(), median, na.rm = TRUE))
    
    vri_data = median_data %>% 
      mutate(iso_code = iso, location = loc, vri = vri, .before = 1)
    
    r_list[[1]] = c(r_list[[1]], loc)
    r_list[[2]] = c(r_list[[2]], r)
    r_list[[3]] = c(r_list[[3]], list(fit))
    r_list[[4]] = c(r_list[[4]], list(covid_subset$people_vaccinated))
    r_list[[5]] = c(r_list[[5]], list(covid_subset$date))
    r_list[[6]] = c(r_list[[6]], list(covid_subset$t_days))
    r_list[[7]] = c(r_list[[7]], list(vri_data))
    r_list[[8]] = c(r_list[[8]], iso)
  }
  
  ## Measuring model fitness, residual standard error (RSE).
  for (i in seq_len(length(r_list$fit))) {
    if ( is.null(r_list$fit[[i]]) ) {
      r_list$rse[[i]] = NA
    } else if (log.y) {
      # residuals = real - fitted
      resid = r_list$y.real[[i]] - exp(fitted(r_list$fit[[i]]))
      # degrees of freedom = n - k - 1, k = number of parameters we want to predict
      df = summary(r_list$fit[[i]])$df[2]
      r_list$rse[[i]] = (sum(resid^2)/df)^(1/2)
    } else {
      r_list$rse[[i]] = sigma(r_list$fit[[i]])
    }
  }
  
  return(r_list)
}