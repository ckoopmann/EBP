if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("colorRamps")) install.packages("colorRamps"); library("colorRamps")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")

if(!exists("EvaluationDataPath")){
      EvaluationDataPath <- "EvaluationData"      
}
if(!exists("EvaluationDataFilename")){
      EvaluationDataFilename <- "EvaluationNoSim10Date2017-02-03"   
}
if(!exists("SaveMaps")){
      SaveMaps <- T 
}
if(!exists("SaveBoxPlots")){
      SaveBoxPlots <- T
}
      

load(paste(EvaluationDataPath, EvaluationDataFilename, sep = "/"))
EvaluationDataByRegion <- melt(EvaluationDataByRegion, id.vars = c("Domain","Simulation", "Population")) 
EvaluationByRegion <- EvaluationDataByRegion[,.(MSE = mean((value - Population)^2), MAE = mean(abs(value - Population)), MB = mean(value - Population), MeanEstimate = mean(value), PopulationValue = mean(Population)), by = .(Domain, variable)]

MapsLocation <- paste("Maps",EvaluationDataFilename,"", sep = "/")
dir.create(MapsLocation, showWarnings = FALSE)
if(SaveMaps){
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
                  png(filename = paste0(MapsLocation,currVar,currStat,".png"))
                  print(spplot(Map,currStat, main=title,
                               col.regions=c(rgb(ramp(seq(0, 1, length = 200)), max = 255)),par.strip.text=list(lines=1.3),
                               at=c(seq(min(Map@data[,currStat], na.rm = TRUE),max(Map@data[,currStat], na.rm = TRUE),length.out=100),Inf),colorkey=TRUE))
                  dev.off()
            }
      }
}

BoxplotLocation <- "Boxplot"
if(SaveBoxPlots){
      MSEVars <- c("gew", "Ungewichtet", "EBP", "EBP_W")
      MSEBySimulation <- EvaluationDataByRegion[variable %in% MSEVars,.(MSE = mean((value - Population)^2)), by = .(variable, Simulation)]
      BoxPlotMSEBySimulation <- ggplot(MSEBySimulation, aes(variable, MSE)) + geom_boxplot()
      
      MSEByDomain <- EvaluationDataByRegion[variable %in% MSEVars,.(MSE = mean((value - Population)^2)), by = .(variable, Domain)]
      BoxPlotMSEByDomain <- ggplot(MSEByDomain, aes(variable, MSE)) + geom_boxplot()
      
      MSEEstimatesByDomain <- EvaluationDataByRegion[variable == "Gini_MSE", .(MSE_Estimate = value),by = .(Domain, Simulation)]
      MSEEstimatesByDomain <- merge(MSEEstimatesByDomain, MSEByDomain[variable == "EBP"], by = "Domain")
      BoxPlotMSEEstimate <- ggplot(MSEEstimatesByDomain, aes(variable, MSE_Estimate - MSE)) + geom_boxplot()
}
 