library(plotrix)
library(pdfetch)
library(networkD3)
library(knitr)

## pdfecth

?pdfetch_EUROSTAT

## Example 1: Net lending per sector, UK

# Specifying the name of the flows of interests
names<-c("B9")

# Downloading the data by specifying the various filters
UKdata_raw = pdfetch_EUROSTAT(flowRef = "nasa_10_nf_tr", UNIT="CP_MNAC",NA_ITEM=names, GEO="UK", 
                              DIRECT="PAID",SECTOR=c("S11","S12","S13","S14_S15","S2"))

# Transforming the obtained data into a data.frame
UKdata<-as.data.frame(UKdata_raw)

# Setting readable names
colnames(UKdata)<-c("NFC","FC","Govt","HH","RoW")

# Matricial plot
matplot(1995:2015,UKdata,lwd=2,type="l",lty=1,ylab="",xlab="",col=2:6)

# Adding the horizontal line
abline(h=0,col=1)

# Adding a grid
grid()

# Adding a legend
legend("bottomleft",col=c(2:6),lwd=2,lty=1,legend=c("NFC","FC","Govt","HH","RoW"),bty='n')

## Example 2: Household income statement from 2014

# Selecting the flows
names<-c("B5G","D5","D61","D62","D7","D8","B6G","P3","B8G","P5G","D9","NP","B9")

# Obtaining the data
EZdata_raw = pdfetch_EUROSTAT("nasa_10_nf_tr", UNIT="CP_MNAC",NA_ITEM=names, GEO="EU28",
                              SECTOR=c("S14_S15"), TIME="2014")

# Transforming the data into a data.frame
EZdata<-as.data.frame(EZdata_raw)

# Automatic procedure to remove the non-interesting bit of the colnames
coln<-colnames(EZdata)
newcoln<-c()
HHdata<-c()

for(i in 1:length(coln)){
  name<-coln[i]
  tname<-strsplit(name,"\\.")[[1]]
  newname<-paste(tname[3:4],collapse=".")
  # If the column contains only NA, remove it from the dataset
  if(!is.na(EZdata[16,i])){
    newcoln<-c(newcoln,newname) 
    HHdata<-c(HHdata,EZdata[16,i])
  }
}

# Creating a new dataset with only values 2014
HHdata<-as.data.frame(t(HHdata))
colnames(HHdata)<-newcoln

# Creating the aggregates
HHdata_1<-as.data.frame(c(HHdata$PAID.B5G,-HHdata$PAID.D5,-HHdata$PAID.D61+HHdata$RECV.D61,
                          +HHdata$RECV.D62-HHdata$PAID.D62,-HHdata$PAID.D7+HHdata$RECV.D7,HHdata$PAID.B6G))
colnames(HHdata_1)<-"2014"
rownames(HHdata_1)<-c("Total Income","Taxes","Social Contributions","Social Benefits",
                      "Other transfers","Gross Disposable Income")
kable(HHdata_1)

HHdata_2<-as.data.frame(c(HHdata$PAID.B6G,-HHdata$PAID.P3,-HHdata$PAID.D8+HHdata$RECV.D8,
                          HHdata$PAID.B8G))
colnames(HHdata_2)<-"2014"
rownames(HHdata_2)<-c("Gross Disposable Income","Consumption","Adjustments in Pensions",
                      "Gross Savings")
kable(HHdata_2)

HHdata_3<-as.data.frame(c(HHdata$PAID.B8G,-HHdata$PAID.P5G,-HHdata$PAID.D9+HHdata$RECV.D9,
                          -HHdata$PAID.NP,HHdata$PAID.B9))

colnames(HHdata_3)<-"2014"
rownames(HHdata_3)<-c("Gross Savings","Gross Capital Formation","Capital Transfer",
                      "Net Non-Produced NF Assets","Net Lending Position")
kable(HHdata_3)


## Installing the dependent libraries and the package

install.packages("expm")
install.packages("igraph")
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

install.packages("path/PKSFC_1.5.tar.gz", repos = NULL, type="source")

## Testing
library(PKSFC)

sim<-sfc.model("SIM.txt",modelName="SIMplest model from chapter 3 of Godley and Lavoie (2007)")
datasim<-simulate(sim)
plot(sim$time,datasim$baseline[,"Yd"],type="l", xlab="", ylab="", lty=2, 
     ylim=range(datasim$baseline[,c("Yd","C")],na.rm=T))
lines(sim$time,datasim$baseline[,"C"],lty=3)
lines(sim$time,vector(length=length(sim$time))+datasim$baseline["2010","C"])
legend(x=1970,y=50,legend=c("Disposable Income","Consumption","Steady State"),lty=c(2,3,1),bty="n")


## Internal representation

print(sim)

## Output data structure

kable(datasim$baseline)


## Direct Acyclic graphs

simex<-sfc.model("SIMEX.txt",modelName="SIMplest model with expectation")
layout(matrix(c(1,2),1,2))
plot.dag(sim,main="SIM" )
plot.dag(simex,main="SIMEX" )

## In the case of a more complex model - Chapter 6

ch6 <- sfc.model("ch6.txt",modelName="Chapter6_openmodel")
graphs = generate.DAG.collapse( adjacency = ch6$matrix )
par(mfrow = c(1,3))
# first plot the orgianl grpah
plot_graph_hierarchy( graphs$orginal_graph, main = "orginal graph" )
# plot hte nodes that form the strongly connected compoent
plot_graph_hierarchy( graphs$SCC_graph, main = "SCC nodes" )
# plot the result ing DAG when we take the condensation of the graph
plot_graph_hierarchy( graphs$DAG, main = "resulting DAG" )

## Systems of dependent vs independent equations 

datasimex<-simulate(simex)
init = datasimex$baseline[66,]
simex<-sfc.addScenario(simex,"G",25,1960,2010,init)
datasimex<-simulate(simex)
datasim<-simulate(sim)
init = datasim$baseline[66,]
sim<-sfc.addScenario(sim,"G",25,1960,2010,init)
datasim<-simulate(sim)
plot(sim$time,datasim$scenario_1[,"H"],type="l",xlab="",ylab="",main="SIM")
lines(sim$time,datasim$scenario_1[,"C"],lty=2)
lines(sim$time,datasim$scenario_1[,"Yd"],lty=3)
legend(x=1944,y=130,legend=c("Wealth","Consumption","Disposable Income"),
       lty=c(1,2,3),bty="n")
plot(simex$time,datasimex$scenario_1[,"H"],type="l",xlab="",ylab="",main="SIMEX")
lines(simex$time,datasimex$scenario_1[,"C"],lty=2)
lines(simex$time,datasimex$scenario_1[,"Yd"],lty=3)
lines(simex$time,datasimex$scenario_1[,"Yd_e"],lty=4)
legend(x=1944,y=130,legend=c("Wealth","Consumption","Disposable Income",
                             "Expecetd Disposable Income"),lty=c(1,2,3,4),bty="n")
## Computational implications

ptm <- proc.time()
data1<-simulate(sim)
print(paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds"))

#  1. tolValue
ptm <- proc.time()
data2<-simulate(sim,tolValue = 1e-3)
print(paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds"))

# 2. maxIter
ptm <- proc.time()
data3<-simulate(sim, maxIter=10)
print(paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds"))

## Observing the results of the three simulations
kable(round(t(data1$baseline[c(1,2,20,40,66),c("G","Y","T","Yd","C","H","H_h")]),digits=3))
kable(round(t(data2$baseline[c(1,2,20,40,66),c("G","Y","T","Yd","C","H","H_h")]),digits=3))
kable(round(t(data3$baseline[c(1,2,20,40,66),c("G","Y","T","Yd","C","H","H_h")]),digits=3))

## Block Gauss-Seidel

print(simex)


## Simulation of SIMEX

ptm <- proc.time()
dataex<-simulate(simex,tolValue = 1e-10)
print(paste("Elapsed time is ",proc.time()[3]-ptm[3],"seconds"))

## Results for SIMEX

kable(round(t(dataex$baseline[c(1,2,20,40,66),c("G","Y","T","Yd","Yd_e","C","H","H_h")]),digits=3))

## Output data structure

kable(dataex$baseline)

## Checking the number of iteractions

# For sim, no simulate parameters:
kable(round(t(data1$baseline[c(1,2,20,40,66),c("iter block 1")]),digits=3))

# For sim, tolValue fixed:
kable(round(t(data2$baseline[c(1,2,20,40,66),c("iter block 1")]),digits=3))

# For sim, maxIter fixed:
kable(round(t(data2$baseline[c(1,2,20,40,66),c("iter block 1")]),digits=3))

# For simex, no simulate parameters
kable(round(t(dataex$baseline[c(1,2,20,40,66),c("iter block 1","iter block 2","iter block 3","iter block 4","iter block 5","iter block 6","iter block 7")]),digits=3))

## Buffer stocks, using model PC

pc<-sfc.model("PC.txt",modelName="Portfolio Choice Model")
# Changing Existing equations
pcRand<-sfc.editEqus(pc,list(
  list(var="cons",eq="alpha1*yde+alpha2*v(-1)"),
  list(var="b_h",eq="ve*(lambda0 + lambda1*r - lambda2*(yde/ve))")))

# Adding equations
pcRand<-sfc.addEqus(pcRand,list(
  list(var="yde",eq="yd(-1)*(1+rnorm(1,sd=0.1))",desc="Expected disposable income depending on random shocks"),
  list(var="h_d",eq="ve-b_h",desc="Money demand"),
  list(var="ve",eq="v(-1)+yde-cons",desc="Expected disposable income")))

# Adding the initial value of yd (to be used in the expectation function)
pcRand<-sfc.editVar(pcRand,var="yd",init=86.48648)

# Checking if the model is complete
pcRand<-sfc.check(pcRand)

plot.dag(pcRand,main="PC Random" )

datapcRand<-simulate(pcRand,maxIter=2)
plot(pcRand$time,datapcRand$baseline[,"h_h"],type="l",xlab="",ylab="",lty=1,lwd=2,
     ylim=c(1*min(datapcRand$baseline[,c("h_h","h_d")],na.rm=T),
            1.2*max(datapcRand$baseline[,c("h_h","h_d")],na.rm=T)))
lines(pcRand$time,datapcRand$baseline[,"h_d"],lty=2,lwd=2)
legend(x=1950,y=1.2*max(datapcRand$baseline[,c("h_h","h_d")],na.rm=T),
       legend=c("Money held","Money demand"),lty=c(2,1),lwd=2,bty="n")

## Sankey Diagrams

plot.sankey(as.data.frame(datapcRand$baseline),filename="TFM_PC.csv",period=2)
