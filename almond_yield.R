#' Almond Yield Function
#' Computes the almond yield anomoly in response to climate
#' @param df a dataframe consisting of the following variables : year, month, minimum daily temperature, maximum daily temperature, daily precipitationx

#' @return minimum yield, maximum yield, mean yield 
#' 
#' @author Annie Adams and Carly Caswell


#function to calculate almond yield anamoly
almond_yield = function(df) {
  
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
  almond_yield_calculation <- ( -0.015 * feb_min_temp$min_temp - .0046*feb_min_temp$min_temp^2 - .07 * jan_mean_precip$total_precip + .0043 * jan_mean_precip$total_precip^2 + 0.28)
  
  #store minimum yield value from almond yield calculation
  min_yield <- min(almond_yield_calculation)
  
  #store maximum yield value from almond yield calculation
  max_yield <- max(almond_yield_calculation)
  
  #store average yield value from almond yield calculation
  mean_yield <- mean(almond_yield_calculation)
  
  #return minimum, maximum, and mean yield values 
  return(print(paste0("Minimum yield: ", min_yield,  " tons/acre      ",
                      "Maximum yield: ", max_yield,  " tons/acre        ",
                      "Mean yield: ", mean_yield,  " tons/acre ")))
  
}