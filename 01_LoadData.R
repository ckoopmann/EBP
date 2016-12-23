library(foreign)
library(readstata13)
library(data.table)


pgen <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/pgen.dta")
pgen <- as.data.table(pgen)
setkeyv(pgen,c("pid","syear"))



ppfadl <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/ppfadl.dta")
ppfadl <- as.data.table(ppfadl)
setkeyv(ppfadl,c("pid", "syear"))

soep <- merge(pgen, ppfadl)

rm(list = ls())
library(foreign)
library(readstata13)
library(data.table)


pgen <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/pgen.dta")
pgen <- as.data.table(pgen)
setkeyv(pgen,c("pid","syear"))



ppfadl <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/ppfadl.dta")
ppfadl <- as.data.table(ppfadl)
setkeyv(ppfadl,c("pid", "syear"))

pequiv <- read.dta("/Users/Christian/Statistik_Studium/EconProject/Daten/SOEP/Dateien_(5)_von_SOEP_Hotline_--_Michaela_Engelmann_--__SOEPlong-doku-v31L.xlsx_docu_v31_1.zip_SOE.../SOEP-LONG_v31_stata_de+en/pequiv.dta")
pequiv <- as.data.table(pequiv)
setkeyv(pequiv,c("pid", "syear"))

soep <- merge(pgen, ppfadl)
soep <- merge(soep, pequiv)

#Choose variables of interest
soep <- soep[,.(pid, syear, pglfs, d11104,pgexpft,pgexppt,d11109,sex,l11102,pgerwzt)]

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
