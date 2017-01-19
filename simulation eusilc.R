#Armutsindikatoren und ihre Schätzung
#EBP - Simulationsstudie mit informativen Stichproben
#Christian Koopman, Felix Skarke, Enno Tammena

if(!require("emdi")) install.packages("emdi"); library("emdi")
if(!require("doBy")) install.packages("doBy"); library("doBy")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("reldist")) install.packages("reldist"); library("reldist")
if(!require("sampleSelection")) install.packages("sampleSelection"); library("sampleSelection")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
set.seed(2)

######################
##sampling weights####
######################
cutoff <- 45000
groups <- 10
hist(eusilcA_pop$cash)
eusilcA_pop_temp <- eusilcA_pop %>% arrange(cash) %>% filter(cash <= cutoff) # Datensatz temporär teilen
eusilcA_pop_temp_rich <- eusilcA_pop %>% arrange(cash) %>% filter(cash > cutoff)

eusilcA_pop_temp$cash_group <- cut(eusilcA_pop_temp$cash,groups) # cuten nach Anzahl der Grupppen
levels(eusilcA_pop_temp$cash_group) <- c(1:groups)

eusilcA_pop_temp_rich$cash_group <- groups 
eusilcA_pop_temp_rich$cash_group <- factor(eusilcA_pop_temp_rich$cash_group)

eusilcA_pop_split <- rbind(eusilcA_pop_temp,eusilcA_pop_temp_rich) # wieder zusammenfügen der Datensätze

summary(eusilcA_pop_split$cash_group)
#relative für pop prop
relative_frequency <- rep(NA,groups)
for(i in 1:groups){
      relative_frequency[i] <-sum(eusilcA_pop_split$cash_group==i)/length(eusilcA_pop_split$cash_group)
}
relative_frequency
#absolute für sample prop
absolute_frequency <- rep(NA,groups)
for(i in 1:groups){
      absolute_frequency[i] <-sum(eusilcA_pop_split$cash_group==i)
}
absolute_frequency

sample_size_groups <- 100


eusilcA_pop_split$sample_prop <- rep(NA,nrow(eusilcA_pop_split))
for(i in 1:nrow(eusilcA_pop_split)){
      temp_help_sample <- eusilcA_pop_split$cash_group[i]
      eusilcA_pop_split$sample_prop[i] <- sample_size_groups/absolute_frequency[temp_help_sample]
}

eusilcA_pop_split$pop_prop <- rep(NA,nrow(eusilcA_pop_split)) # pop prop zuweisen
for(i in 1:nrow(eusilcA_pop_split)){
      temp_help_pop <- eusilcA_pop_split$cash_group[i]
      eusilcA_pop_split$pop_prop[i] <- relative_frequency[temp_help_pop]
}

eusilcA_pop_split <- eusilcA_pop_split[sample(nrow(eusilcA_pop_split),replace = F),] # reshuffle Daten da noch geordnet von Grupppenaufteilung
eusilcA_pop_split$sample_weight <- eusilcA_pop_split$pop_prop/eusilcA_pop_split$sample_prop # neue Variable sample weights erzeugt
summary(eusilcA_pop_split$sample_weight)
      
# eusilcA_pop$cash_group <- cut(eusilcA_pop$cash[eusilcA_pop$cash <= cutoff],groups)
# levels(eusilcA_pop$cash_group) <- c(1:groups)
# relative_frequency <- rep(NA,groups)
# for(i in 1:groups){
# relative_frequency[i] <-sum(eusilcA_pop_split$cash_group==i)/length(eusilcA_pop_split$cash_group)
# }
# relative_frequency
# 
# absolute_frequency <- rep(NA,groups)
# for(i in 1:groups){
#      absolute_frequency[i] <-sum(eusilcA_pop$cash_group==i)
# }
# absolute_frequency
# 
# 
# 
# eusilcA_pop$eqIncome_group <- cut(eusilcA_pop$eqIncome,groups)
# levels(eusilcA_pop$eqIncome_group) <- c(1:groups)
# relative_frequency_eqincome <- rep(NA,groups)
# for(i in 1:groups){
#       relative_frequency_eqincome[i] <-sum(eusilcA_pop$eqIncome_group==i)/length(eusilcA_pop$eqIncome_group)
# }
# relative_frequency_eqincome
# 
# absolute_frequency_eqincome <- rep(NA,groups)
# for(i in 1:groups){
#       absolute_frequency_eqincome[i] <-sum(eusilcA_pop$eqIncome_group==i)
# }
# absolute_frequency_eqincome
# 
# 
# hist(eusilcA_pop$eqIncome)
# sum(eusilcA_pop$eqIncome<30000)/length(eusilcA_pop$eqIncome)

#No. of simulations
s <- 1
#Größe der informativen Stichprobe
n <- 1000
#Größe des "Zensus" für die Daten auf Small Area-Ebene
c <- 3000
#Auswertungsvektor
evaluation <- NULL

#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {
#eusilcA_pop Daten als Population aus Load_Data.R
eusilcA_pop_split
  
#Populationsdaten für das  two-level model
#pop <- summaryBy(branche + income + expPT + expFT + edu + east + seniority + female + married ~ sma, data=eusilcA_pop, FUN=mean)
#names(pop) <- c("sma", "branche", "income", "expPT", "expFT", "edu", "east", "seniority", "female", "married")
eusilcA_pop_split$sma <- eusilcA_pop_split$district
#Census Daten für EBP
# ids <- sample(eusilcA_pop$id, c,  replace = FALSE )
#census <- eusilcA_pop[eusilcA_pop$id %in% ids, ]
# census <- eusilcA_pop
#Informative Stichprobe
#die Wahrscheinlichkeit ist abhängig von education
#Fehlerterm bei der Ziehungswahrscheinlichkeit
#e <- rnorm(N,0,1)




#eusilcA_pop$p <-  scale(eusilcA_pop$cash)+1

#+e
eusilcA_pop_split$id <- seq(1:nrow(eusilcA_pop_split))
ids <- sample(eusilcA_pop_split$id, n,  replace = FALSE, prob = eusilcA_pop_split$sample_prop)
sample <- as.data.table(eusilcA_pop_split[eusilcA_pop_split$id %in% ids, ])
str(sample)

summary(sample$cash_group)
#wie richtig weights berechnen??
#sample$sample_weight <- 1/sample$p

#Berechnung des ungewichteten Gini
unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
names(unwgini) <- c("Domain", "Gini")

#Berechnung des gewichteten Gini
directgini <- sample[,.(DirectGini = gini(eqIncome, weights = sample_weight)), by = sma]
directgini <- setDT(directgini, keep.rownames = TRUE)[]
names(directgini) <- c("Domain", "Gini")

#Berechnung des wahren Gini
popgini <- as.data.frame(tapply(eusilcA_pop_split$eqIncome, eusilcA_pop_split$sma, function(x){gini(x)}))
popgini <- setDT(popgini, keep.rownames = TRUE)[]
names(popgini) <- c("Domain", "Gini")

#Berechnung des Ginis mittels EBP
ebp_est <- ebp(eqIncome ~ gender + eqsize + rent +  self_empl + unempl_ben + 
                  age_ben + surv_ben + sick_ben + dis_ben +   fam_allow + 
                     house_allow + cap_inv + tax_adj, eusilcA_pop_split, "sma", sample, "sma", L=50)

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
 