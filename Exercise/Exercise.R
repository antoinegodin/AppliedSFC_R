library(PKSFC)
library("Rgraphviz")

#This needs to be done
pcss<-sfc.model("PcSS.txt",modelName="Portfolio Choice Model Steady State")

#Visual Check 
plot.dag(pcss, main="Steady State PC" )

#Find the numerical Steady State
datapcss<-simulate(pcss)

#Format the results
dataSS<-as.data.frame(datapcss$baseline)

#Generate the calibration file
t=2
sink("calibration.txt")
cat("#Example of parameter\n")
cat(paste("g=",dataSS$g[t],"\n"))
...
sink()

#Loading up the model without calibration
modelLines<-readLines("PC.txt")

#Loading up the calibration
calibrationLines<-readLines("calibration.txt")

#Finding the calibration to be added
indexCalib<-grep("CALIBRATION",modelLines)

#Adding the calibration into the model
totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])

#Writing the complete model
writeLines(totModel,"PCCalibrated.txt")

#Loading the model
PC<-sfc.model("PCCalibrated.txt",modelName="PC")

#Check the DAG
plot.dag(PC)

# Simulate the model
datapc<-simulate(PC)

#Plotting results - GDP and components
names<-c("y","cons","g")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="units",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Output","Consumption","Govt. Expenditure"))

#Plot the sankey
plot.sankey(as.data.frame(datapc$baseline),filename="TFM_PC.csv",period=2)