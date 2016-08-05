library(PKSFC)
library("Rgraphviz")

#This is done with values corresponding to Italy (thanks to Federico Bassi and Florian Botte)
pcss<-sfc.model("PcSS_ITA.txt",modelName="Portfolio Choice Model Steady State")

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
cat(paste("alpha2=",dataSS$alpha2[t],"\n"))
cat(paste("alpha1=",dataSS$alpha2[t],"\n"))
cat(paste("theta=",dataSS$theta[t],"\n"))
cat(paste("lambda0=",dataSS$lambda0[t],"\n"))
cat(paste("lambda1=",dataSS$lambda1[t],"\n"))
cat(paste("lambda2=",dataSS$lambda2[t],"\n"))
cat(paste("r_bar=",dataSS$r[t],"\n"))
cat(paste("v=",dataSS$v[t],"\n"))
cat(paste("b_h=",dataSS$b_h[t],"\n"))
cat(paste("b_s=",dataSS$b_s[t],"\n"))
cat(paste("h_s=",dataSS$h_s[t],"\n"))
cat(paste("h_h=",dataSS$h_h[t],"\n"))
cat(paste("b_cb=",dataSS$b_cb[t],"\n"))
cat(paste("r=",dataSS$r[t],"\n"))
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
result<-as.data.frame(datapc$baseline)
result$year<-rownames(result)

#Plotting results - GDP and components
names<-c("y","cons","g")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="units",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Output","Consumption","Govt. Expenditure"))

#Plot the sankey
plot.sankey(as.data.frame(datapc$baseline),filename="TFM_PC.csv",period=2)
