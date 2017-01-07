if(!require("ineq")) install.packages("ineq"); library("ineq")
if(!require("emdi")) install.packages("emdi"); library("emdi")
if(!require("doBy")) install.packages("doBy"); library("doBy")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("data.table")) install.packages("data.table"); library("data.table")


set.seed(3)
s <- 30
evaluation <- NULL

for(i in 1:s) {

load("soep.Rda")
soep$pglfs <- NULL
soep$female <- ifelse(as.numeric(soep$sex)==8, 1,0)
soep$married <- ifelse(as.numeric(soep$d11104)==7, 1,0)
soep$sex <- NULL
soep$d11104 <- NULL
soep$pid <- NULL
soep$syear <- NULL
N <- nrow(soep)
soep$id <- 1:N
names(soep) <- c("branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married", "id")
soep$east <- as.numeric(soep$east)-1



#SMA Branche#Geschlecht

soep$sma <- soep$branche
soep <- droplevels(soep)

#Pop Data for two-level model
pop <- summaryBy(branche + income + expPT + expFT + edu + east + seniority + female + married ~ sma, data=soep, FUN=mean)
names(pop) <- c("sma", "branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married")

#census
ids <- sample(soep$id, 1000,  replace = FALSE )
census <- soep[soep$id %in% ids, ]


#Simulation
n <- 5000
#Fehlerterm
e <- rnorm(N,0,1) 


#die Wahrscheinlichkeit ist abhängig von education
soep$p <-  (0.3*soep$edu)+e
#Normierung auf 0 bis 1
soep$p1 <-  0.25 * (exp(soep$p) / (1+ exp(soep$p)))
summary(soep$p1)

ids <- sample(soep$id, n,  replace = FALSE, prob = soep$p1)

sample <- soep[soep$id %in% ids, ]
directgini <- as.data.frame(tapply(sample$income, sample$sma, function(x){Gini(x)}))
directgini <- setDT(directgini, keep.rownames = TRUE)[]
names(directgini) <- c("Domain", "Gini")

popgini <- as.data.frame(tapply(soep$income, soep$sma, function(x){Gini(x)}))
popgini <- setDT(popgini, keep.rownames = TRUE)[]
names(popgini) <- c("Domain", "Gini")

ebp_est <- ebp(income ~ expPT + expFT + edu + east + seniority + female + married, census, "sma", sample, "sma", L=100  , interval = c(-1, 2),   parallel_mode = "local", cpus = 1, custom_indicator = NULL, na.rm = T)
ebpgini <- estimators(object = ebp_est, MSE = F, CV = F, indicator = c("Gini"))

df <- merge(popgini, directgini, by="Domain")
ginitbl <- merge(df, ebpgini$ind, by="Domain")
names(ginitbl) <- c("Domain", "Population", "Direct", "EBP")

#mse
mse_ebp <- mean((ginitbl$Population - ginitbl$Direct )^2)
mse_direct <- mean((ginitbl$Population - ginitbl$EBP)^2)

#mean absolute bias
mab_ebp <- mean(abs(ginitbl$Population - ginitbl$Direct ))
mab_direct <- mean(abs(ginitbl$Population - ginitbl$EBP))

#number of not estimatiale domains
nr_ebp <- nrow(popgini)-nrow(ebpgini$ind)
nr_direct <- nrow(popgini)- nrow(samplegini)

results <- cbind(mse_ebp, mse_direct, mab_ebp, mab_direct, nr_ebp, nr_direct)
evaluation <- rbind(evaluation, results)

s <- s+1
}


summary(evaluation)
