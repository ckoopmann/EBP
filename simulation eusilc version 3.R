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
set.seed(2)

#No. of simulations
s <- 100
#Größe der informativen Stichprobe -> Ergibt sich aus der gruppenzahl und größe dort

#Größe des "Zensus" für die Daten auf Small Area-Ebene
c <- 25000
#Auswertungsvektor
evaluation <- NULL

#population data
population <- eusilcA_pop
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

population$gewichtung<-NA
for(i in 1:10) population$gewichtung <- ifelse(population$groupedincome == i, gewichte[i], population$gewichtung)

population$ebpgewichtung<-1/population$gewichtung

#Beginn der Simulation mit s Durchläufen
for(i in 1:s) {

      census <- sample_n(population, c)
      
      #take a sample of size g from each group
      sp <-split(population, population$groupedincome)
      samples <- lapply(sp, function(x) x[sample(1:nrow(x), g, FALSE),])
      sample <- do.call(rbind, samples)
      #count the empty SMAs
      c_0 <- sum(table(sample$sma)==0)
      
      #Berechnung des ungewichteten Gini
      unwgini <- as.data.frame(tapply(sample$eqIncome, sample$sma, function(x){gini(x)}))
      unwgini <- setDT(unwgini, keep.rownames = TRUE)[]
      unwgini[unwgini == 0] <- gini(sample$eqIncome)
      names(unwgini) <- c("Domain", "Gini")
      
      #Berechnung des gewichteten Gini
      sample <- as.data.table(sample)
      gewgini <- sample[,.(gewGini = gini(eqIncome, weights = gewichtung)), by = sma]
      gewgini <- setDT(gewgini, keep.rownames = TRUE)[]
      gewgini[gewgini == 0] <-  gini(sample$eqIncome, weights = sample$gewichtung)
      names(gewgini) <- c("Domain", "Gini")
      
      #Berechnung des wahren Gini
      popgini <- as.data.frame(tapply(population$eqIncome, population$sma, function(x){gini(x)}))
      popgini <- setDT(popgini, keep.rownames = TRUE)[]
      names(popgini) <- c("Domain", "Gini")
      
      #Berechnung des Ginis mittels EBP
      ebp_est <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
                            self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                            fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "district", smp_data = sample, smp_domains = "district", L=1)
      
      ebpgini <- estimators(object = ebp_est, MSE = F, CV = F, indicator = c("Gini"))
      
      sample$freq <- round(sample$gewichtung*0.5)
      dt <- data.table(sample)
      sample.expanded <- dt[rep(seq(.N), freq), !"freq", with=F]
      #Berechnung des Ginis mittels EBP + Gewichtung
      ebp_est <- ebp( fixed = eqIncome ~ gender + eqsize + cash + 
                            self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + 
                            fam_allow + house_allow + cap_inv + tax_adj, pop_data = census, pop_domains = "district", smp_data = sample.expanded, smp_domains = "district", L=1)
      
      ebp_estw <- ebp_est
      ebpginiw <- estimators(object = ebp_estw, MSE = F, CV = F, indicator = c("Gini"))
      
      
      #Zusammenführen der Ergebnisse in eine Tabelle
      df <- merge(popgini, gewgini, by="Domain")
      ginitbl <- merge(df, unwgini, by="Domain")
      ginitbl <- merge(ginitbl, ebpgini$ind, by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP")
      ginitbl <- merge(ginitbl, ebpginiw$ind, by="Domain")
      names(ginitbl) <- c("Domain", "Population",  "gew", "Ungewichtet", "EBP", "EBP_W")
      
      #Berechnung number of not estimatebale domains
      nr_ebp <- nrow(popgini)-nrow(na.omit(ebpgini$ind))
      nr_ebpw <- nrow(popgini)-nrow(na.omit(ebpginiw$ind))
      nr_gew <- nrow(popgini)- nrow(na.omit(gewgini))
      nr_unw <- nrow(popgini)- nrow(na.omit(unwgini))
      nr_empty_sma <- c_0
      
      #Berechnung MSE
      mse_ebp <- mean((ginitbl$Population - ginitbl$EBP )^2, na.omit=T)
      mse_ebpw <- mean((ginitbl$Population - ginitbl$EBP_W )^2, na.omit=T)
      mse_gew <- mean((ginitbl$Population - ginitbl$gew)^2, na.omit=T)
      mse_unw <- mean((ginitbl$Population - ginitbl$Ungewichtet)^2, na.omit=T)
      
      #Berechnung mean absolute bias
      mab_ebp <- mean(abs(ginitbl$Population - ginitbl$EBP ))
      mab_ebpw <- mean(abs(ginitbl$Population - ginitbl$EBP_W ))
      mab_gew <- mean(abs(ginitbl$Population - ginitbl$gew))
      mab_unw <- mean(abs(ginitbl$Population - ginitbl$Ungewichtet))
      
      #Abspeichern der Ergebnismatrix
      results <- cbind(mse_ebp, mse_ebpw, mse_gew, mse_unw, mab_ebp, mab_ebpw, mab_gew, mab_unw, nr_ebp, nr_ebpw, nr_gew, nr_unw, nr_empty_sma)
      evaluation <- rbind(evaluation, results)
      
      s <- s+1
}

#Auswertung
summary(evaluation)


boxplot(evaluation[,c(1,2,3,4,5)])