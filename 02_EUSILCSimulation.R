#Armutsindikatoren und ihre Schätzung
#EBP - Simulationsstudie mit informativen Stichproben
#Christian Koopman, Felix Skarke, Enno Tammena


#To Do Liste

#Root mean square Error 
#Relative Bias
#Absolute Bias
#(Siehe Folie 46)
#

if(!require("emdi")) install.packages("emdi"); library("emdi")
if(!require("doBy")) install.packages("doBy"); library("doBy")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("reldist")) install.packages("reldist"); library("reldist")
if(!require("sampleSelection")) install.packages("sampleSelection"); library("sampleSelection")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("gpclib")) install.packages("gpclib"); library("gpclib")
if(!require("MASS")) install.packages("MASS"); library("MASS")
source("function_simulationsdatensatz.R")
rotate <- function(x) t(apply(x, 2, rev))

set.seed(1234)

####################
#Inhaltsverzeichnis#
####################
# Erweiterung des Datensatzes
# Festlegen der Samplegrößen in den SMA
# Simulation
#Ziehung des Samples
#Berechnung der Ginis mit Verschiedenen Verfahren
#Zusammenfügung der Ergebnisse
# Auswertung

# Erweiterung des Datensatzes 
population <- eusilcA_pop
#Zu kleine müssen zunächst raus, da hier keine fälle in allen Einkommenskategorien vorliegen
tbl <- table(population$district)
drop <- names(tbl)[tbl <=  20]
levels(population$district)[levels(population$district) %in% drop] <- NA
population <- na.omit(population)
population <- droplevels(population)

#Es wird zufällig festgelegt, wer wie oft dupliziert wird
population$expansion <-  round(runif(nrow(population), 1, 8))
population$expanded <-  ifelse(population$expansion>1, T, F)
#Fehlerterm wird zufällig gezogen
dt <- data.table(population)
population.expanded <- dt[rep(seq(.N), expansion), !"expansion", with=F]
population.expanded$error <-  rnorm(nrow(population.expanded), 0, 5000)

summary(population.expanded$eqIncome)

#Die duplizierten Fälle erhalten den Fehler hinzu
population.expanded$eqIncome <- ifelse(population.expanded$expanded==T, population.expanded$eqIncome+population.expanded$error, population.expanded$eqIncome)


population <- population.expanded

#Es wird sichergestellt, dass das Einkommen min. 0 Euro ist
population$eqIncome <- ifelse(population$eqIncome<0, 0, population$eqIncome)

#Es wird geprüft, ob das spätere EBP modell noch Sinn macht (ja!)
lm <- lm(eqIncome ~ gender + eqsize + cash + 
               self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
               fam_allow + house_allow + cap_inv + tax_adj, data = population)
summary(lm)
library(car)
vif(lm)

#SMA ist District
population$sma <- population$district
#Es wird nochmal geprüft, ob genug Fälle da sind (bei den gewählten Paramtern passiert jetzt hier nichts mehr)
tbl <- table(population$sma)
drop <- names(tbl)[tbl <=  150]
levels(population$sma)[levels(population$sma) %in% drop] <- NA
population <- na.omit(population)
population <- droplevels(population)

#Gruppierungsvariable für Einkommen wird erstellt
population$groupedincome <- cut(population$eqIncome, breaks = c(0, 7500, 12500, 22500, 27500, max(population$eqIncome)), include.lowest = T )
population$groupedincome <- factor(population$groupedincome, labels =c(1:5))

#Gewichte werden berechnet - wieviele Personen repräsentiert eine Person pro Gruppe
#Wir ziehen später gleichverteilt aus jeder Einkommensgruppe g Fälle -- ?? Stichprobengröße insgesamt ist 2000 oder?
g <- 2000

#Es wird einmal gesamplet, um die Anzahl je SMA festzulegen
sp_org <-split(population, population$groupedincome)
samples_org <- lapply(sp_org, function(x) x[sample(1:nrow(x), g, replace = FALSE),])
sample_org <- do.call(rbind, samples_org)
population$gewichtung<-NA
for (area in levels(sample_org$sma)) {
      #Der Datensatz wird temporär nach SMA aufgeteilt
      data_temp <- population[population$sma %in% area,]
      smagroupsize <- table(data_temp$groupedincome)
      #da gleichverteilt gezogen wird, ist die gewichtung 
      h <- round((nrow(sample_org[sample_org$sma %in% area,]))/5)
      gewichte <-  as.numeric(smagroupsize/h)
      #jedem Fall wird sein Gewicht zugeordnet
      for(i in 1:5)  population$gewichtung <- ifelse(population$sma %in% area &  population$groupedincome == i,  gewichte[i],  population$gewichtung)
}
population$freq <- round(population$gewichtung)



#No. Simulations
s <- 10

#Größe des "Zensus" für die Daten auf Small Area-Ebene (habe den wieder eingstellt, sonst dauert es ewig)
c <- 25000
census <- sample_n(population, c)
#Emdi Einstellungen
l <- 50 
b <- 10


#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {
      #Es wird ein Sample aus der Population gezogen
      sample_1 <- NULL
      for (area in levels(sample_org$sma)) {
            #Basierend aus dem Anfangssample festgelegt, wieviele Fälle wir aus jeder SMA ziehen, wobei diese Fälle auf die Einkommensgrupppen aufgeteilt werden
            h <- round((nrow(sample_org[sample_org$sma %in% area,]))/5)
            #Der Datensatz wird temporär nach SMA aufgeteilt
            data_temp <- population[population$sma %in% area,]
            #Der temporäre Datensatz wird nach einkommen aufgeteilt
            sp <-split(data_temp, data_temp$groupedincome)
            #Aus jeder Einkommensklasse werden gleich viele gezogen
            samples <- lapply(sp, function(x) x[sample(1:nrow(x), h, FALSE),])
            sample <- do.call(rbind, samples)
            sample_1 <- rbind(sample_1, sample) 
      }
      sample <- sample_1
      
      #Hier sieht man, dass immer gleich viele in jeder SMA sind pro Schleifendurchlauf
      table(sample$sma)-table(sample_org$sma)
      
      #Berechnung des Ginis mittels EBP + Gewichtung
      #das Sample wird gemäß Gewichtung erweitert
      dt <- data.table(sample)
      sample.expanded <- dt[rep(seq(.N), freq), !"freq", with=F]
      #wegen Konvergenzproblemen wird tryCatch eingebaut
      ebp_estw  <- tryCatch(
            ebp(fixed = eqIncome ~ gender + eqsize + cash + 
                      self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                      fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "sma", smp_data = sample.expanded, smp_domains = "sma", L=l),
            error=function(e) e
      )
      if(inherits(ebp_estw, "error")){
            print("Error Caught")
            next
      } 
      ebpginiw <- estimators(object = ebp_estw, MSE = F, CV = F, indicator = c("Gini"))
      # EBP_weighted[[i]] <- ebpginiw$ind[,2]
      
      #Berechnung des ungewichteten Gini mittels EBP
      ebp_estw  <- tryCatch(
            ebp_est <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
                                  self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                                  fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "sma", smp_data = sample, smp_domains = "sma", L=l, MSE = T, B= b),
            error=function(e) e)
      if(inherits(ebp_estw, "error")){
            print("Error Caught")
            next
      } 
      
      ebpgini <- estimators(object = ebp_est, MSE = T, CV = F, indicator = c("Gini"))

      #Berechnung des ungewichteten Gini
      unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
      unwgini <- setDT(unwgini, keep.rownames = TRUE)[]

      names(unwgini) <- c("Domain", "Gini")

      
      #Berechnung des gewichteten Gini
      sample <- as.data.table(sample)
      gewgini <- sample[,.(gewGini = gini(eqIncome, weights = gewichtung)), by = sma]
      gewgini <- setDT(gewgini, keep.rownames = TRUE)[]
      names(gewgini) <- c("Domain", "Gini")

      
      #Berechnung des wahren Gini -- ausserhalb der Schleife, weil der ja konstant bleibt
      popgini <- as.data.frame(tapply(population$eqIncome, population$sma, function(x){gini(x)}))
      popgini <- setDT(popgini, keep.rownames = TRUE)[]
      names(popgini) <- c("Domain", "Gini")

      
      #Zusammenführen der Ergebnisse in eine Tabelle
      df <- merge(popgini, gewgini, by="Domain") # Es werden immer nur 91 Zeilen gemerged weil gewgini nur 91 districts hat. Liegt das am data.table Befehl?
      ginitbl <- merge(df, unwgini, by="Domain")
      ginitbl <- merge(ginitbl, ebpgini$ind[,c(1,2)], by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP")
      ginitbl <- merge(ginitbl, ebpginiw$ind, by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP", "EBP_W")

      ginitbl <- merge(ginitbl, ebpgini$ind[,c(1,3)], by = "Domain")
      
      #Save Samplesizes
      samplesizes <- data.table(Domain = names(table(sample_org$sma)), SampleSize = as.vector(table(sample_org$sma)))
      ginitbl <- merge(ginitbl, samplesizes, by = "Domain")
      
      ginitbl$Simulation <- i
      if(exists("EvaluationDataByRegion")){
            EvaluationDataByRegion <- rbind(EvaluationDataByRegion,ginitbl)
      }
      else{
            EvaluationDataByRegion <- ginitbl
      }
      print(i)
}

EvaluationDataPath <- "EvaluationData"
EvaluationDataFilename <- paste0("Evaluation","NoSim",s,"Date",as.character(Sys.Date()))
save(EvaluationDataByRegion, file = paste(EvaluationDataPath, EvaluationDataFilename, sep = "/"))
