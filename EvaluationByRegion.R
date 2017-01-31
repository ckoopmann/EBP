if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("colorRamps")) install.packages("colorRamps"); library("colorRamps")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
load("EvaluationDataByRegion")

EvaluationByRegion <- EvaluationDataByRegion[,.(MSE = mean((value - Population)^2), MAE = mean(abs(value - Population)), MB = mean(value - Population), MeanEstimate = mean(value), PopulationValue = mean(Population)), by = .(Domain, variable)]

#Visualisation via map

# Define the color of the maps
ramp <- colorRamp(c("green","yellow", "red","black"))

Map <- readRDS("Austria_Shapefiles/AUT_adm2.rds")

OriginalMapData <- Map@data

EvaluationDataMap <- EvaluationByRegion[variable == "EBP",.(NAME_2 = Domain, MSE, MAE, MB, MeanEstimate, PopulationValue)]
Map@data <- merge(Map@data, EvaluationDataMap, by = "NAME_2", all.x = TRUE)
#Choose which methods to include (set to all methods)
VarSelection <- unique(EvaluationByRegion$variable)
#Choose which statistics to plot
StatSelection <- c("MSE", "MAE", "MB", "MeanEstimate", "PopulationValue")
for(currVar in unique(EvaluationByRegion$variable)){
      EvaluationDataMap <- EvaluationByRegion[variable == currVar,.(NAME_2 = Domain, MSE, MAE, MB, MeanEstimate, PopulationValue)]
      Map@data <- merge(OriginalMapData, EvaluationDataMap, by = "NAME_2", all.x = TRUE)
      for(currStat in StatSelection){
            title <- paste0(currStat, " of ",currVar)
            print(title)
            png(filename = paste0("Maps/",currVar,currStat,".png"))
            print(spplot(Map,currStat, main=title,
                   col.regions=c(rgb(ramp(seq(0, 1, length = 200)), max = 255)),par.strip.text=list(lines=1.3),
                   at=c(seq(min(Map@data[,currStat], na.rm = TRUE),max(Map@data[,currStat], na.rm = TRUE),length.out=100),Inf),colorkey=TRUE))
            dev.off()
      }
} 