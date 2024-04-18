#' Almond Yield Function
#' Computes the almond yield anomoly in response to climate
#' @param df a dataframe consisting of the following variables : year, month, minimum daily temperature, maximum daily temperature, daily precipitationx

#' @return minimum yield, maximum yield, mean yield 
#' 
#' @author Annie Adams and Carly Caswell


#function to calculate almond yield anamoly
almond_yield = function(df,Tmincoeff1=-0.015, Tmincoeff2=-0.0046, Pcoeff1=-0.07, Pcoeff2=0.0043, intercept=0.28) {
  
  #group temperatures by year, find min temperature in Febuary for every year
  feb_min_temp <- df %>% 
    group_by(year) %>% 
    filter(month == 2) %>% 
    summarize(min_temp = min(tmin_c))
  
  # group precipitation by year, find total precipitation in January in every year
  jan_mean_precip <- df %>% 
    group_by(year) %>% 
    filter( month == 1) %>% 
    summarize(total_precip = sum(precip))
  
  # almond yield anamoly formula utilizing febuary tempearture values and january precipitation values
  almond_yield_calculation <- full_join(feb_min_temp, jan_mean_precip) %>% mutate(almond_yield_calculation =   Tmincoeff1*feb_min_temp + Tmincoeff2*feb_min_temp^2 + Pcoeff1*jan_mean_precip + Pcoeff2*jan_mean_precip^2 + intercept)
  
  
  Tmincoeff1*feb_min_temp + Tmincoeff2*feb_min_temp^2 + Pcoeff1*jan_mean_precip + Pcoeff2*jan_mean_precip^2 + intercept)
  
  #store minimum yield value from almond yield calculation
  # min_yield <- min(almond_yield_calculation)
  # 
  # #store maximum yield value from almond yield calculation
  # max_yield <- max(almond_yield_calculation)
  # 
  # #store average yield value from almond yield calculation
  # mean_yield <- mean(almond_yield_calculation)
  # 
  # #return minimum, maximum, and mean yield values 
  return(almond_yield_calculation)
  
}