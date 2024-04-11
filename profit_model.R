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