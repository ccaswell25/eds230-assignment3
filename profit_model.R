#' Almond Profit Function
#' Computes the almond profit from the almond yield anomoly in response to climate

#' @param  yield_data from almond_yield function
#' @param  cost_per_hectare default = 100
#' @param  price_per_kg default = 200
#' @param  discount_rate
#' @param  years 
#' @return profit, npv 
#' 
#' @author Annie Adams and Carly Caswell


##Create function for profit
####Use the almond yield function output as an input using "call"

# Call almond yield data
source("almond_yield.R")
almond_yield

  
# Function to calculate almond profit
calculate_almond_profit <- function(input_data, cost_per_hectare = 100, price_per_kg = 200, discount_rate, years) {
  
  #Compute almond yield
  yield_data <- almond_yield(input_data)
  
  #Calculate Revenue
  revenue <- yield_data * price_per_kg
  
  #Define Cost
  total_cost <- cost_per_hectare
  
  # Calculate Profit
  profit <- revenue - total_cost
  
  # Calculate Net Present Value
  discount_factors <- (1 + discount_rate)^(-1 * (1:years))
  discounted_cash_flows <- c(-total_cost, rep(profit, years - 1)) * discount_factors
  npv <- sum(discounted_cash_flows)
  
  return(print(paste0("Profit ($): ", profit,  "NPV ($): ",npv)))
  
}
