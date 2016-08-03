library(PKSFC)

source("compare.R")

detach(result)

#Generating the calibration
source("FileGen.R")

#Loading up the model without calibration
modelLines<-readLines("modellStripped.sfc")

#Loading up the calibration
calibrationLines<-readLines("calibration.txt")

#Finding the calibration to be added
indexCalib<-grep("CALIBRATION",modelLines)

#Adding the calibration into the model
totModel<-c(modelLines[1:(indexCalib-1)],calibrationLines,modelLines[(indexCalib+1):length(modelLines)])

#Writing the complete model
writeLines(totModel,"modelAuto.sfc")

#Loading the model
climate<-sfc.model("modelAuto.sfc",modelName="climate")

#Adding some scenarios
climate<-sfc.addScenario(climate,list(c("alpha1")),list(c(alpha1[t]*0.9)),c(2012),c(2040))
climate<-sfc.addScenario(climate,list(c("betag")),list(c(betag[t]*1.1)),c(2012),c(2040))
climate<-sfc.addScenario(climate,list(c("grprd")),list(c(0.01)),c(2012),c(2040))
climate<-sfc.addScenario(climate,list(c("grprd")),list(c(-0.01)),c(2012),c(2040))

# Simulate the model
data<-simulate(climate,tolValue = 1e-10, maxIter=100000)

#Getting the results into a nicer format
result<-as.data.frame(data$baseline)
result$year<-as.numeric(rownames(result))

conShock<-as.data.frame(data$scenario_1)
conShock$year<-as.numeric(rownames(conShock))

phaseShock<-as.data.frame(data$scenario_2)
phaseShock$year<-as.numeric(rownames(phaseShock))

pricePosShock<-as.data.frame(data$scenario_3)
pricePosShock$year<-as.numeric(rownames(pricePosShock))

priceNegShock<-as.data.frame(data$scenario_4)
priceNegShock$year<-as.numeric(rownames(priceNegShock))

#Creating a dataset with historical and simulated data
detach(Results)
cnCal<-colnames(Results)
cnRes<-colnames(result)
blockAdd<-as.data.frame(matrix(NA,ncol=ncol(result),nrow=17))
colnames(blockAdd)<-cnRes
for(name in cnRes){
  iCal<-grep(paste("^",name,"$",sep=""),cnCal,fixed=F)
  if(length(iCal)>0){
    blockAdd[name]<-Results[1:17,iCal]
  }else{
    blockAdd[name]<-result[2,name]
  }
}
rownames(blockAdd)<-blockAdd$year
result<-rbind(blockAdd,result[2:30,])
attach(result)

#Plotting results - GDP and components
names<-c("Y","C","Ic","Ig","Id","G")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="mln2012$",xlab="")
abline(v=2011)
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Output","Consumption","Investment","Green energy investment","Dirty energy investment","Govt. Expenditure"),cex=0.8, pt.cex = 1)
dev.copy2eps(file="../figures/NominalValues.eps")
dev.off()

names<-c("kgA","kdA")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="GW",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Green energy capacity","Dirty energy capacity"))
abline(v=2011)
dev.copy2eps(file="../figures/CapacityStocks.eps")
dev.off()

names<-c("Pkd","Pkg")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="2012$/GW",xlab="")
legend("topright",lwd=2,bty='n',col=1:7,lty=1,legend=c("Price of dirty GW","Price of green GW"))
abline(v=2011)
dev.copy2eps(file="../figures/CapacityPrices.eps")
dev.off()

names<-c("eg","ed","e")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="TWh",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Green energy","Dirty energy", "Total"))
abline(v=2011)
dev.copy2eps(file="../figures/EnergyProduction.eps")
dev.off()

names<-c("shareeg","shareed")
matplot(result$year,100*result[names],type="l",lty=1,lwd=2,col=1:7,ylab="%",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=c("Renewables","Dirty"))
abline(v=2011)
dev.copy2eps(file="../figures/EnergyShares.eps")
dev.off()

names<-c("gy","gY","gc","gic")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("gid","gig")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("ge","ged","geg")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("id","ig")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
abline(v=2011)
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("grc","grg","grd")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
abline(v=2011)
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("ukg","ukd")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("kgA","kdA")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Pkd","Pkg")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Pc")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Pe")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Lc","Lg","Ld")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("D","L")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Fc")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("Fg","Fd")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("eg","ed","e")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)

names<-c("shareeg","shareed")
matplot(result$year,result[names],type="l",lty=1,lwd=2,col=1:7,ylab="",xlab="")
legend("topleft",lwd=2,bty='n',col=1:7,lty=1,legend=names)
