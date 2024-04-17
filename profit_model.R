#' Almond Profit Function
#' Computes the almond profit from the almond yield anomoly in response to climate

#' @param  yield_data output from almond_yield function
#' @param  cost_per_hectare default = 100
#' @param  price_per_kg default = 200
#' @param  discount_rate
#' @param  years 
#' @return profit, npv 
#' 
#' @author Annie Adams and Carly Caswell


##Create a function for profit

# Call almond yield function
source("almond_yield.R")
almond_yield

  
# Function to calculate almond profit
calculate_almond_profit <- function(cost_per_hectare = 100, price_per_kg = 200, year) {
  
  #Compute almond yield
  yield_data <- almond_yield(clim,Tmincoeff1=-0.015, Tmincoeff2=-0.0046, Pcoeff1=-0.07, Pcoeff2=0.0043, intercep=0.28)
  
  #Calculate Revenue
  revenue <- yield_data * price_per_kg
  
  #Define Cost
  total_cost <- cost_per_hectare
  
  # Calculate Profit
  profit <- revenue - total_cost

  return(print(paste0("Profit ($): ", profit)))
  
}

