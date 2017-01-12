library(foreign)
library(readstata13)
library(data.table)


pgen <- read.dta13("C:/Users/Tammena/Uni/Master Stat/econ project/SOEP-LONG_v31_stata_de+en/pgen.dta")
pgen <- as.data.table(pgen)
setkeyv(pgen,c("pid","syear"))



ppfadl <- read.dta13("C:/Users/Tammena/Uni/Master Stat/econ project/SOEP-LONG_v31_stata_de+en/ppfadl.dta")
ppfadl <- as.data.table(ppfadl)
setkeyv(ppfadl,c("pid", "syear"))

soep <- merge(pgen, ppfadl)

rm(list = ls())
library(foreign)
library(readstata13)
library(data.table)


pgen <- read.dta13("C:/Users/Tammena/Uni/Master Stat/econ project/SOEP-LONG_v31_stata_de+en/pgen.dta")
pgen <- as.data.table(pgen)
setkeyv(pgen,c("pid","syear"))



ppfadl <- read.dta13("C:/Users/Tammena/Uni/Master Stat/econ project/SOEP-LONG_v31_stata_de+en/ppfadl.dta")
ppfadl <- as.data.table(ppfadl)
setkeyv(ppfadl,c("pid", "syear"))

pequiv <- read.dta13("C:/Users/Tammena/Uni/Master Stat/econ project/SOEP-LONG_v31_stata_de+en/pequiv.dta")
pequiv <- as.data.table(pequiv)
setkeyv(pequiv,c("pid", "syear"))

soep <- merge(pgen, ppfadl)
soep <- merge(soep, pequiv)

#Choose variables of interest
soep <- soep[,.(pid, syear, e11107, pglfs,pglabgro, d11104,pgexpft,pgexppt,d11109,sex,l11102,pgerwzt)]

#Drop non-working
soep <- soep[as.numeric(pglfs) == 16,]

#Sort by syear and use unique to get latest value
#Sort by surve year in descending order
setorderv(soep, cols = "syear", order = -1)
#Extract first appearance of each pid
soep <- soep[!duplicated(pid),]

#Remove NAs indicated by negative Values
for(varName in names(soep)){
      soep <- soep[as.numeric(get(varName)) >= 0,]
      print(varName)
      print(nrow(soep))
}

soep <- na.omit(soep)
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
soep$east <- as.numeric(soep$east)-7

#SMA Branche#Geschlecht
soep$sma <- as.factor(paste(as.character(soep$branche), as.character(soep$female), sep = " - "))
soep <- droplevels(soep)

save(soep,file="soep.Rda")