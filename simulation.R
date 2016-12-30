if(!require("ineq")) install.packages("ineq"); library("ineq")

set.seed(123456)

load("soep.Rda")
soep$pglfs <- NULL

soep$female <- ifelse(as.numeric(soep$sex)==8, 1,0)
soep$married <- ifelse(as.numeric(soep$d11104)==7, 1,0)
soep$sex <- NULL
soep$d11104 <- NULL
soep$pid <- NULL
soep$syear <- NULL


names(soep) <- c("branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married")

#SMA Branche#Geschlecht

soep$sma <- soep$branche
soep <- droplevels(soep)



 
#Population parameters:
popgini <- tapply(soep$income, soep$sma, function(x){Gini(x)})
popgini

#Simulation
#Fehlerterm
n <- 5000
e <- rnorm(n,0,1) 

#die Wahrscheinlichkeit ist abhängig von education
p <-  (0.3*soep$edu)
#Normierung auf 0 bis 1
p1 <-  0.25 * (exp(p) / (1+ exp(p)))
summary(p1)
