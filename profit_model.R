#Almond Profit Model

##Compute NPV
function (value, time, discount = 0.12)
   {
   result = value/(1 + discount)^time
   return(result)
}

##Create function for profit
####Use the almond yield function output as an input using "call"
baseline = x

#Call almond yield data
yield_data <- 

# Function to calculate almond profit
calculate_almond_profit <- function(yield_data, cost_per_hectare, price_per_kg) {
  revenue <- yield_data * price_per_kg
  total_cost <- cost_per_hectare
  # Calculate profit
  profit <- revenue - total_cost
  
  return(profit)
}