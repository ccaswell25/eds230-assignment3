#' Almond Profit Function
#' Computes the almond profit from the almond yield anomoly in response to climate

#' @param  yield_data yield of almonds in tons/acre
#' @param  cost_per_hectare cost of almonds per hectare , default = 100
#' @param  price_per_kg price of almonds per kg
#' @param  discount_rate 
#' @param  years 
#' @return profit, npv 
#' 
#' @author Annie Adams and Carly Caswell


##Create a function for profit

# Call almond yield function
source("almond_yield_function.R")
almond_yield

  
# Function to calculate almond profit
calculate_almond_profit <- function(df, 
                                    t_min_coef_1 = -0.015, 
                                    t_min_coef_2 = -0.0046, 
                                    precip_coef_1 = -0.07, 
                                    precip_coef_2 = 0.0043, 
                                    intercept = 0.28, cost_per_hectare = 100, price_per_kg = 200, year) {
  
  #Compute almond yield
  yield_data <- almond_yield(df,Tmincoeff1, Tmincoeff2, Pcoeff1, Pcoeff2, intercept)
  

  
  #Calculate Revenue
  revenue <- yield_data * price_per_kg
  
  #Define Cost
  total_cost <- cost_per_hectare
  
  # Calculate Profit
  profit <- revenue - total_cost

  return(profit)
  
}

