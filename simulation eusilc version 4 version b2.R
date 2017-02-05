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
# population$error[which(population$eqIncome==0)]
# population$expanded[which(population$eqIncome==0)]
# hist(population$error)
summary(population.expanded$eqIncome)

# for(i in 2:nrow(population.expanded)){
#       if(population.expanded$expanded[i]==FALSE | population.expanded$expanded[i-1]==FALSE){
#             population.expanded$eqIncome[i] <- population.expanded$eqIncome[i]
#       }else{
#             population.expanded$eqIncome[i] <- population.expanded$eqIncome[i] + population.expanded$error[i]
#       }
#       print(i)
# }
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
#Die idee, den ganzen Datensatz zu simulieren wurde verworfen, hat aber ähnliche Ergebnisse erzielt
#population <- simulationsdatensatz()

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
g <- 999

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
s <- 2

#Größe des "Zensus" für die Daten auf Small Area-Ebene (habe den wieder eingstellt, sonst dauert es ewig)
c <- 25000
census <- sample_n(population, c)
#Emdi Einstellungen
l <- 50 
b <- 10


#Auswertungsvektoren 
# evaluation <- NULL
# EBPlong <- NULL
# EBP_Wlong <- NULL     
# unwlong <- NULL
# gewlong <- NULL
# mselong <- NULL

# EBP <- list()
# EBP_weighted <- list()
# Gini <- list()
# Gini_weighted <- list()
# EBP_MSE <- list()

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
      # EBP[[i]] <- ebpgini$ind[,2]
      # EBP_MSE[[i]] <- ebpgini$ind[,3]
      #Berechnung des ungewichteten Gini
      unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
      unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
      #unwgini[unwgini == 0] <- gini(sample$eqIncome) # Das geht nicht nicht gibt eine Fehlermeldung. Außerdem gibts ja sowohl einen Gini von 0, als auch NAs. Was soll der Code denn tun?
      names(unwgini) <- c("Domain", "Gini")
      # Gini[[i]] <- unwgini$Gini
      
      
      #Berechnung des gewichteten Gini
      sample <- as.data.table(sample)
      gewgini <- sample[,.(gewGini = gini(eqIncome, weights = gewichtung)), by = sma]
      gewgini <- setDT(gewgini, keep.rownames = TRUE)[]
      # missing.districts <- levels(population$district)[-which(levels(population$district) %in% as.character(unique(sample$district)))]
      # outofsample_temp <- data.table(missing.districts,rep(NA, times = length(missing.districts)))
      # names(outofsample_temp) <- c("Domain", "Gini")
      # gewgini <- rbindlist(list(gewgini,outofsample_temp))
      # gewgini[gewgini == 0] <-  gini(sample$eqIncome, weights = sample$gewichtung)
      # gewgini[is.na(gewgini)] <-  gini(sample$eqIncome, weights = sample$gewichtung)
      names(gewgini) <- c("Domain", "Gini")
      # gewgini <- gewgini[order(gewgini[[1]])]
      # Gini_weighted[[i]] <- gewgini$Gini
      
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

      msetbl <- ebpgini$ind[,c(1,3)]
      # 
      # #Berechnung number of not estimatebale domains
      # nr_ebp <- nrow(popgini)-nrow(na.omit(ebpgini$ind))
      # nr_ebpw <- nrow(popgini)-nrow(na.omit(ebpginiw$ind))
      # nr_gew <- nrow(popgini)- nrow(na.omit(gewgini)) # NAs tauchen im data.table gar nicht auf: bei 5 fehlenden districts gibt es 91 Werte aber keine NAs.
      # nr_unw <- nrow(popgini)- nrow(na.omit(unwgini)) # hier anders 96 districts und 5 NAs 
      # 
      # #Berechnung MSE über alle SMA
      # mse_ebp <- mean((ginitbl$Population - ginitbl$EBP )^2, na.rm=T)
      # mse_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W )^2, na.rm=T)
      # mse_gew <- mean((ginitbl$Population - ginitbl$gew)^2, na.rm=T)
      # mse_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet)^2, na.rm=T)
      # 
      # #Berechnung mean absolute bias über alle SMA
      # mab_ebp <- mean(abs(ginitbl$Population - ginitbl$EBP ),na.rm = T)
      # mab_ebpw <- mean(abs(ginitbl$Population - ginitbl$EBP_W),na.rm = T)
      # mab_gew <- mean(abs(ginitbl$Population - ginitbl$gew),na.rm = T)
      # mab_unw <- mean(abs(ginitbl$Population - ginitbl$Ungewichtet),na.rm = T)
      # 
      # #mean bias über alle SMA
      # mb_ebp <- mean((ginitbl$Population - ginitbl$EBP ),na.rm = T)
      # mb_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W),na.rm = T)
      # mb_gew <- mean((ginitbl$Population - ginitbl$gew),na.rm = T)
      # mb_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet),na.rm = T)      
      # 
      # #Ergebnismatrix
      # results <- cbind(mse_unw, mse_gew, mse_ebp, mse_ebpw, mab_unw,  mab_gew, mab_ebp, mab_ebpw, mb_unw,  mb_gew, mb_ebp, mb_ebpw, nr_ebp, nr_ebpw, nr_gew, nr_unw)
      # evaluation <- rbind(evaluation, results)
      # 
      # #Die Ginis für die einzelnen SMA werden gespeichert
      # EBPlong <- cbind(EBPlong, ginitbl$EBP)
      # EBP_Wlong <- cbind(EBP_Wlong, ginitbl$EBP_W)      
      # unwlong <- cbind(unwlong, ginitbl$Ungewichtet)
      # gewlong <- cbind(gewlong, ginitbl$gew)
      # mselong <- cbind(mselong, msetbl)
  
      
}

#Auswertung
#Berechnung von MSE
# 
# MSE_EBP_long <- rowMeans(((EBPlong-ginitbl$Population)^2))
# MSE_EBP_Wlong <- rowMeans(((EBP_Wlong-ginitbl$Population)^2))
# MSE_unw_long <- rowMeans(((unwlong-ginitbl$Population)^2))
# MSE_gew_long <- rowMeans(((gewlong-ginitbl$Population)^2))
# 
# final <- rbind(MSE_unw_long, MSE_gew_long, MSE_EBP_long, MSE_EBP_Wlong)
# final <- as.data.frame(final)
# names(final) <- as.vector(ginitbl$Domain)
# 
# rowMeans(final)






#Timo meinte, das hier macht ja keinen Sinn...
# summary(evaluation)
# boxplot(evaluation[,c(1:4)])
# boxplot(evaluation[,c(5:8)])
# boxplot(evaluation[,c(9:12)])
# 
# boxplot((EBPlong-ginitbl$Population)^2)





#Problem bleibt dass emdi abbricht also hier eine Funktion um Nullelemente zu beseitigen:
# rmNullObs <- function(x) {
#       x <- Filter(Negate(is.NullOb), x)
#       lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
#}
