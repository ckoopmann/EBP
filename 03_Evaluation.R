
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("colorRamps")) install.packages("colorRamps"); library("colorRamps")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("ggthemes")) install.packages("ggthemes"); library("ggthemes")
if(!require("crop")) install.packages("crop"); library("crop")

if(!exists("EvaluationDataPath")){
      EvaluationDataPath <- "EvaluationData"      
}
if(!exists("EvaluationDataFilename")){
      EvaluationDataFilename <- "EvaluationNoSim250Date2017-02-04"   
}
if(!exists("SaveMaps")){
      SaveMaps <- F
}
if(!exists("SaveBoxPlots")){
      SaveBoxPlots <- T
}
      

load(paste(EvaluationDataPath, EvaluationDataFilename, sep = "/"))
EvaluationDataByRegion <- melt(EvaluationDataByRegion, id.vars = c("Domain","Simulation", "Population")) 

EvaluationDataByRegion[,variable := as.factor(variable)]
levels(EvaluationDataByRegion$variable) <- c("Direct_Weighted", "Direct_Unweighted","EBP_Unweighted","EBP_Weighted", "MSE Estimate", "Sample Size")
EvaluationDataByRegion[,variable := factor(variable, levels = c("Direct_Unweighted","EBP_Unweighted","Direct_Weighted", "EBP_Weighted", "MSE Estimate", "Sample Size"))]

EvaluationByRegion <- EvaluationDataByRegion[,.(MSE = mean((value - Population)^2), 
                                                MAE = mean(abs(value - Population)), 
                                                MB = mean(value - Population),
                                                MRE = mean((value - Population)/Population),
                                                RMSE = mean((value - Population)^2)^(0.5),
                                                MeanEstimate = mean(value), 
                                                PopulationValue = mean(Population)), 
                                                by = .(Domain, variable)]

MapsLocation <- paste("Maps",EvaluationDataFilename,"", sep = "/")
dir.create(MapsLocation, showWarnings = FALSE)
if(SaveMaps){
      #Visualisation via map
      # Define the color of the maps
      ramp <- colorRamp(c("green","yellow", "red","black"))
      Map <- readRDS("Austria_Shapefiles/AUT_adm2.rds")
      OriginalMapData <- Map@data
      
      #Choose which methods to include (set to all methods)
      VarSelection <- unique(EvaluationByRegion$variable)
      #Choose which statistics to plot
      StatSelection <- c("MSE", "MAE", "MB","MRE","RMSE", "MeanEstimate", "PopulationValue")
      for(currVar in unique(EvaluationByRegion$variable)){
            EvaluationDataMap <- EvaluationByRegion[variable == currVar,.(NAME_2 = Domain, MSE, MAE, MB, MRE, RMSE, MeanEstimate, PopulationValue)]
            Map@data <- merge(OriginalMapData, EvaluationDataMap, by = "NAME_2", all.x = TRUE)
            for(currStat in StatSelection){
                  title <- paste0(currStat, " of ",currVar)
                  print(title)
                  png(filename = paste0(MapsLocation,currVar,currStat,".png"), width = 800, height = 400)
                  print(spplot(Map,currStat,
                               col.regions=c(rgb(ramp(seq(0, 1, length = 200)), max = 255)),par.strip.text=list(lines=1.3),
                               at=c(seq(min(Map@data[,currStat], na.rm = TRUE),max(Map@data[,currStat], na.rm = TRUE),length.out=100),Inf),colorkey=TRUE))
                  dev.off.crop(file = paste0(MapsLocation,currVar,currStat,".png"))
            }
      }
}


PlotLocation <- paste("Plots",EvaluationDataFilename,"", sep = "/")
dir.create(PlotLocation, showWarnings = FALSE)
if(SaveBoxPlots){
      MSEVars <- c("Direct_Weighted", "Direct_Unweighted","EBP_Unweighted","EBP_Weighted")
      MSEBySimulation <- EvaluationDataByRegion[variable %in% MSEVars,.(MSE = mean((value - Population)^2)), by = .(variable, Simulation)]
      BoxPlotMSEBySimulation <- ggplot(MSEBySimulation, aes(variable, MSE)) + geom_boxplot()
      
      MSEByDomain <- EvaluationDataByRegion[variable %in% MSEVars,.(MSE = mean((value - Population)^2)), by = .(variable, Domain)]
      BoxPlotMSEByDomain <- ggplot(MSEByDomain, aes(variable, MSE)) + geom_boxplot() +
            theme(text = element_text(size=20)) +  geom_rangeframe(sides = "l") + xlab("Estimator") + ylab("Mean Squared Error")
      ggsave(filename = paste0(PlotLocation, "BoxPlotMSEByDomain.png"), device = "png", plot = BoxPlotMSEByDomain, width = 24, height = 12, units = "cm")
      
      RMSEByDomain <- EvaluationDataByRegion[variable %in% MSEVars,.(RMSE = mean((value - Population)^2)^(.5)), by = .(variable, Domain)]
      BoxPlotRMSEByDomain <- ggplot(RMSEByDomain, aes(variable, RMSE)) + geom_boxplot() +
            theme(text = element_text(size=20)) +  geom_rangeframe(sides = "l") + xlab("Estimator") + ylab("Root Mean Squared Error")
      ggsave(filename = paste0(PlotLocation, "BoxPlotRMSEByDomain.png"), device = "png", plot = BoxPlotRMSEByDomain, width = 24, height = 12, units = "cm")
      
      
      MAEByDomain <- EvaluationDataByRegion[variable %in% MSEVars,.(MAE = mean(abs(value - Population))), by = .(variable, Domain)]
      BoxPlotMAEByDomain <- ggplot(MAEByDomain, aes(variable, MAE)) + geom_boxplot() + 
            theme(text = element_text(size=20)) +  geom_rangeframe(sides = "l") + xlab("Estimator") + ylab("Mean Absolute Error")
      ggsave(filename = paste0(PlotLocation, "BoxPlotMAEByDomain.png"), device = "png", plot = BoxPlotMAEByDomain, width = 24, height = 12, units = "cm")
      
      MREByDomain <- EvaluationDataByRegion[variable %in% MSEVars,.(MRE = mean((value - Population)/Population)), by = .(variable, Domain)]
      BoxPlotMREByDomain <- ggplot(MREByDomain, aes(variable, MRE)) + geom_boxplot() + 
            theme(text = element_text(size=20)) +  geom_rangeframe(sides = "l") + xlab("Estimator") + ylab("Relative Bias")
      ggsave(filename = paste0(PlotLocation, "BoxPlotMREByDomain.png"), device = "png", plot = BoxPlotMREByDomain, width = 24, height = 12, units = "cm")
      
      
      
      MSEEstimatesByDomain <- EvaluationDataByRegion[variable == "MSE Estimate", .(MSE_Estimate = value),by = .(Domain, Simulation)]
      MSEEstimatesByDomain <- merge(MSEEstimatesByDomain, MSEByDomain[variable == "EBP_Weighted"], by = "Domain")
      BoxPlotMSEEstimate <- ggplot(MSEEstimatesByDomain, aes(variable, (MSE_Estimate - MSE))) + geom_boxplot()
      
      
      HistogramMSEEstimate <-  ggplot(MSEEstimatesByDomain, aes((MSE_Estimate - MSE))) + geom_histogram() + 
            theme(text = element_text(size=20)) +  geom_rangeframe() + xlab("Error (MSE Estimate - Sample MSE)")
      ggsave(filename = paste0(PlotLocation, "HistogramMSEEstimate.png"), device = "png", plot = HistogramMSEEstimate, width = 24, height = 12, units = "cm")
      
}
 