#Armutsindikatoren und ihre Schätzung
#EBP - Simulationsstudie mit informativen Stichproben
#Christian Koopman, Felix Skarke, Enno Tammena

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

set.seed(1234)

#No. of simulations
s <- 50
#Größe der informativen Stichprobe -> Ergibt sich aus der gruppenzahl und größe dort

#Größe des "Zensus" für die Daten auf Small Area-Ebene
#c <- 25000
#Auswertungsvektor
evaluation <- NULL

#population data
population <- eusilcA_pop
#oder simulationsdatensatz 
#population <- simulationsdatensatz()

population$sma <- population$district

#gruppierungsvariable für einkommen
population$groupedincome <- cut(population$eqIncome, breaks = c(seq(0, 45000, by = 5000), max(population$eqIncome)), include.lowest = T )
population$groupedincome <- factor(population$groupedincome, labels =c(1:10))

#gewichte - wieviele personen repräsentiert eine person pro gruppe
#wir ziehen später gleichverteilt aus jeder gruppe g fälle
g <- 400
popgroupsize <- table(population$groupedincome)
#also ist die gewichtung populationsgruppengrösse/g 
gewichte <-  as.numeric(popgroupsize/g)
#jedem fall wird sein gewicht zugeordnet
population$gewichtung<-NA
for(i in 1:10) population$gewichtung <- ifelse(population$groupedincome == i, gewichte[i], population$gewichtung)

#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {

      census <- population
      
      #take a sample of size g from each group
      sp <-split(population, population$groupedincome)
      samples <- lapply(sp, function(x) x[sample(1:nrow(x), g, replace = FALSE),])
      sample <- do.call(rbind, samples)
      #count the empty SMAs
      c_0 <- sum(table(sample$sma)==0)
      
      #Berechnung des Ginis mittels EBP + Gewichtung
      sample$freq <- round(sample$gewichtung)
      dt <- data.table(sample)
      sample.expanded <- dt[rep(seq(.N), freq), !"freq", with=F]
      #wegen konvergenzproblemen wird tryCatch eingebaut
      ebp_estw  <- tryCatch(
            ebp(fixed = eqIncome ~ gender + eqsize + cash + 
                      self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                      fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "district", smp_data = sample.expanded, smp_domains = "district", L=50),
            error=function(e) e
      )
      if(inherits(ebp_estw, "error")){
            print("Error Caught")
            next
      } 
      ebpginiw <- estimators(object = ebp_estw, MSE = F, CV = F, indicator = c("Gini"))
      
      #Berechnung des ungewichteten Gini
      unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
      unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
      unwgini[unwgini == 0] <- gini(sample$eqIncome) # Das geht nicht nicht gibt eine Fehlermeldung. Außerdem gibts ja sowohl einen Gini von 0, als auch NAs. Was soll der Code denn tun?
      names(unwgini) <- c("Domain", "Gini")
      
      #Berechnung des gewichteten Gini
      sample <- as.data.table(sample)
      gewgini <- sample[,.(gewGini = gini(eqIncome, weights = gewichtung)), by = sma]
      gewgini <- setDT(gewgini, keep.rownames = TRUE)[]
      missing.districts <- levels(population$district)[-which(levels(population$district) %in% as.character(unique(sample$district)))]
      outofsample_temp <- data.table(missing.districts,rep(NA, times = length(missing.districts)))
      names(outofsample_temp) <- c("Domain", "Gini")
      gewgini <- rbindlist(list(gewgini,outofsample_temp))
      gewgini[gewgini == 0] <-  gini(sample$eqIncome, weights = sample$gewichtung)
      gewgini[is.na(gewgini)] <-  gini(sample$eqIncome, weights = sample$gewichtung)
      names(gewgini) <- c("Domain", "Gini")
      gewgini <- gewgini[order(gewgini[[1]])]
      
      #Berechnung des wahren Gini
      popgini <- as.data.frame(tapply(population$eqIncome, population$sma, function(x){gini(x)}))
      popgini <- setDT(popgini, keep.rownames = TRUE)[]
      names(popgini) <- c("Domain", "Gini")
      
      #Berechnung des Ginis mittels EBP
      ebp_est <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
                            self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                            fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "district", smp_data = sample, smp_domains = "district", L=50)
      
      ebpgini <- estimators(object = ebp_est, MSE = F, CV = F, indicator = c("Gini"))
      
      #Zusammenführen der Ergebnisse in eine Tabelle
      df <- merge(popgini, gewgini, by="Domain") # Es werden immer nur 91 Zeilen gemerged weil gewgini nur 91 districts hat. Liegt das am data.table Befehl?
      ginitbl <- merge(df, unwgini, by="Domain")
      ginitbl <- merge(ginitbl, ebpgini$ind, by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP")
      ginitbl <- merge(ginitbl, ebpginiw$ind, by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP", "EBP_W")
      
      #Berechnung number of not estimatebale domains
      nr_ebp <- nrow(popgini)-nrow(na.omit(ebpgini$ind))
      nr_ebpw <- nrow(popgini)-nrow(na.omit(ebpginiw$ind))
      nr_gew <- nrow(popgini)- nrow(na.omit(gewgini)) # NAs tauchen im data.table gar nicht auf: bei 5 fehlenden districts gibt es 91 Werte aber keine NAs.
      nr_unw <- nrow(popgini)- nrow(na.omit(unwgini)) # hier anders 96 districts und 5 NAs 
      nr_empty_sma <- c_0
      
      #Berechnung MSE
      mse_ebp <- mean((ginitbl$Population - ginitbl$EBP )^2, na.rm=T)
      mse_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W )^2, na.rm=T)
      mse_gew <- mean((ginitbl$Population - ginitbl$gew)^2, na.rm=T)
      mse_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet)^2, na.rm=T)
      
      #Berechnung mean absolute bias
      mab_ebp <- mean(abs(ginitbl$Population - ginitbl$EBP ),na.rm = T)
      mab_ebpw <- mean(abs(ginitbl$Population - ginitbl$EBP_W),na.rm = T)
      mab_gew <- mean(abs(ginitbl$Population - ginitbl$gew),na.rm = T)
      mab_unw <- mean(abs(ginitbl$Population - ginitbl$Ungewichtet),na.rm = T)
      
      #mean bias
      mb_ebp <- mean((ginitbl$Population - ginitbl$EBP ),na.rm = T)
      mb_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W),na.rm = T)
      mb_gew <- mean((ginitbl$Population - ginitbl$gew),na.rm = T)
      mb_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet),na.rm = T)      
      
      #Abspeichern der Ergebnismatrix
      results <- cbind(mse_unw, mse_gew, mse_ebp, mse_ebpw, mab_unw,  mab_gew, mab_ebp, mab_ebpw, mb_unw,  mb_gew, mb_ebp, mb_ebpw, nr_ebp, nr_ebpw, nr_gew, nr_unw, nr_empty_sma)
      evaluation <- rbind(evaluation, results)
      
}

#über alle durchläufe bias und varianz
#varianz

#Auswertung
summary(evaluation)


boxplot(evaluation[,c(1:4)])
boxplot(evaluation[,c(5:8)])
boxplot(evaluation[,c(9:12)])

