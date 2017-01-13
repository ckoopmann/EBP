#Armutsindikatoren und ihre Schätzung
#EBP - Simulationsstudie mit informativen Stichproben
#Christian Koopman, Felix Skarke, Enno Tammena

if(!require("emdi")) install.packages("emdi"); library("emdi")
if(!require("doBy")) install.packages("doBy"); library("doBy")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("reldist")) install.packages("reldist"); library("reldist")
if(!require("sampleSelection")) install.packages("sampleSelection"); library("sampleSelection")
set.seed(2)

#No. of simulations
s <- 20
#Größe der informativen Stichprobe
n <- 15000
#Größe des "Zensus" für die Daten auf Small Area-Ebene
c <- 3000
#Auswertungsvektor
evaluation <- NULL


load("soep.Rda")



#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {
#Soep Daten als Population aus Load_Data.R
load("soep.Rda")
      

#Populationsdaten für das  two-level model
#pop <- summaryBy(branche + income + expPT + expFT + edu + east + seniority + female + married ~ sma, data=soep, FUN=mean)
#names(pop) <- c("sma", "branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married")

#Census Daten für EBP
ids <- sample(soep$id, c,  replace = FALSE )
census <- soep[soep$id %in% ids, ]

#Informative Stichprobe
#die Wahrscheinlichkeit ist abhängig von education
#Fehlerterm bei der Ziehungswahrscheinlichkeit
#e <- rnorm(N,0,1)
soep$p <-  soep$income
#+e
#Normierung auf 0 bis 1
soep$p1 <-  soep$p/max(soep$p)
summary(soep$p1)

ids <- sample(soep$id, n,  replace = FALSE, prob = soep$p1)
sample <- soep[soep$id %in% ids, ]

#wie richtig weights berechnen??
sample$weights <- 1/sample$p1

#Berechnung des gewichteten Gini
directgini <- sample[,.(DirectGini = gini(income, weights =weights)), by = sma]
directgini <- setDT(directgini, keep.rownames = TRUE)[]
names(directgini) <- c("Domain", "Gini")

#Berechnung des ungewichteten Gini
unwgini <- as.data.frame(tapply(sample$income, sample$sma, function(x){gini(x)}))
unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
names(unwgini) <- c("Domain", "Gini")

#Berechnung des wahren Gini
popgini <- as.data.frame(tapply(soep$income, soep$sma, function(x){gini(x)}))
popgini <- setDT(popgini, keep.rownames = TRUE)[]
names(popgini) <- c("Domain", "Gini")

#Berechnung des Ginis mittels EBP
ebp_est <- ebp(fixed = income ~ expPT + expFT  + east + seniority + female + married,
               pop_data = census, pop_domains = "sma", smp_data = sample, smp_domains = "sma", L= 50,transformation = "no", MSE = F,  B = 50,na.rm = T)
ebpgini <- estimators(object = ebp_est, MSE = F, CV = F, indicator = c("Gini"))

#Berechnung des Ginis mittels EBP + sample selection
#sample$weight <- weights
#was machen wir mit den weights auf der sme ebene?
census$weights <- 1/(nrow(census))
ebp_estw <- ebp_est
      #ebp(income ~ expPT + expFT + weights  + east + seniority + female + married, census, "sma", sample, "sma", L= 50, MSE = F,  B = 50,na.rm = TRUE)
ebpginiw <- estimators(object = ebp_estw, MSE = F, CV = F, indicator = c("Gini"))

#Zusammenführen der Ergebnisse in eine Tabelle
df <- merge(popgini, directgini, by="Domain")
ginitbl <- merge(df, unwgini, by="Domain")
ginitbl <- merge(ginitbl, ebpgini$ind, by="Domain")
names(ginitbl) <- c("Domain", "Population",  "Direct", "Ungewichtet", "EBP")
ginitbl <- merge(ginitbl, ebpginiw$ind, by="Domain")
names(ginitbl) <- c("Domain", "Population",  "Direct", "Ungewichtet", "EBP", "EBP_W")

#Berechnung MSE
mse_ebp <- mean((ginitbl$Population - ginitbl$EBP )^2)
mse_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W )^2)
mse_direct <- mean((ginitbl$Population - ginitbl$Direct)^2)
mse_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet)^2)

#Berechnung mean absolute bias
mab_ebp <- mean(abs(ginitbl$Population - ginitbl$EBP ))
mab_ebpw <- mean(abs(ginitbl$Population - ginitbl$EBP_W ))
mab_direct <- mean(abs(ginitbl$Population - ginitbl$Direct))
mab_unw <- mean(abs(ginitbl$Population - ginitbl$Ungewichtet))

#Berechnung number of not estimatebale domains
nr_ebp <- nrow(popgini)-nrow(ebpgini$ind)
nr_ebpw <- nrow(popgini)-nrow(ebpginiw$ind)
nr_direct <- nrow(popgini)- nrow(directgini)
nr_unw <- nrow(popgini)- nrow(unwgini)

#Abspeichern der Ergebnismatrix
results <- cbind(mse_ebp, mse_ebpw, mse_direct, mse_unw, mab_ebp, mab_ebpw, mab_direct, mab_unw, nr_ebp, nr_ebpw, nr_direct, nr_unw)
evaluation <- rbind(evaluation, results)

s <- s+1
}

#Auswertung
summary(evaluation)



