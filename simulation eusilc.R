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
s <- 1
#Größe der informativen Stichprobe
n <- 5000
#Größe des "Zensus" für die Daten auf Small Area-Ebene
c <- 3000
#Auswertungsvektor
evaluation <- NULL

eusilcA_pop

N <- nrow(eusilcA_pop)

#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {
#eusilcA_pop Daten als Population aus Load_Data.R
eusilcA_pop$id <- 1:N
#Populationsdaten für das  two-level model
#pop <- summaryBy(branche + income + expPT + expFT + edu + east + seniority + female + married ~ sma, data=eusilcA_pop, FUN=mean)
#names(pop) <- c("sma", "branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married")
eusilcA_pop$sma <- eusilcA_pop$district
#Census Daten für EBP
# ids <- sample(eusilcA_pop$id, c,  replace = FALSE )
#census <- eusilcA_pop[eusilcA_pop$id %in% ids, ]
# census <- eusilcA_pop
#Informative Stichprobe
#die Wahrscheinlichkeit ist abhängig von education
#Fehlerterm bei der Ziehungswahrscheinlichkeit
#e <- rnorm(N,0,1)
eusilcA_pop$p <-  scale(eusilcA_pop$cash)+1

#+e
eusilcA_pop$id <- seq(1:nrow(eusilcA_pop))
ids <- sample(eusilcA_pop$id, n,  replace = FALSE, prob = eusilcA_pop$p)
sample <- as.data.table(eusilcA_pop[eusilcA_pop$id %in% ids, ])

#wie richtig weights berechnen??
sample$weights <- 1/sample$p

#Berechnung des ungewichteten Gini
unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
names(unwgini) <- c("Domain", "Gini")

#Berechnung des gewichteten Gini
directgini <- sample[,.(DirectGini = gini(eqIncome, weights = weights)), by = sma]
directgini <- setDT(directgini, keep.rownames = TRUE)[]
names(directgini) <- c("Domain", "Gini")

#Berechnung des wahren Gini
popgini <- as.data.frame(tapply(eusilcA_pop$eqIncome, eusilcA_pop$sma, function(x){gini(x)}))
popgini <- setDT(popgini, keep.rownames = TRUE)[]
names(popgini) <- c("Domain", "Gini")

#Berechnung des Ginis mittels EBP
ebp_est <- ebp(eqIncome ~ gender + eqsize + rent +  self_empl + unempl_ben + 
                  age_ben + surv_ben + sick_ben + dis_ben +   fam_allow + 
                     house_allow + cap_inv + tax_adj, eusilcA_pop, "sma", sample, "sma", L=50)

ebpgini <- estimators(object = ebp_est, MSE = F, CV = F, indicator = c("Gini"))

#Berechnung des Ginis mittels EBP + sample selection
#sample$weight <- weights
#was machen wir mit den weights auf der sme ebene?
#census$weights <- 1/(nrow(census))
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
 