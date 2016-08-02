##################################################################################
################ uploading libraries and setting working directory ###############
##################################################################################


library(PKSFC)
library("Rgraphviz")


setwd("C:/Users/Federico/Desktop/SFC-Simulation-model/Model-2/")


##################################################################################
######################### Running the model ######################################
##################################################################################


pc<-sfc.model("PcModel_ITA2.txt",modelName="Portfolio Choice Model")

pc<-sfc.check(pc,fill=FALSE)

plot_graph_hierarchy(graph=generate.DAG.collaspe(adjacency=t(pc$matrix))$orginal_graph, main="PC" )

datapc<-simulate(pc)

names(datapc)

tryITA<-datapc$baseline

pc$equations

par(mfrow=c(2,2))
plot (1:100, tryITA[,1],type = "l",xlab="time",ylim = c(1407569, 1707569),ylab="Income",main="",col=1)


################################################################################
### First scenario: reducing expenditure through the debt to income ratio#######
################################################################################



pc<-sfc.addScenario(model=pc,vars="austerity_exp",values=(1),inits=5,ends=100)


datapc<-simulate(pc)

names(datapc)

tryITAscenario1<-datapc$scenario_1


matplot (1:100, tryITA[,1],type = "l",xlab="time",ylim = c(0, 1707569),ylab="Income",main="",col=1)
lines (1:100, tryITAscenario1[,1],type = "l",xlab="",ylab="",main="",col = 2)


###################################################################################
### second scenario: increasing taxes thorugh the debt to income ratio ############
###################################################################################

pc<-sfc.addScenario(model=pc, vars="austerity_taxes",values=(1),inits=5,ends=100)

datapc<-simulate(pc)

names(datapc)

tryITAscenario2<-datapc$scenario_2


matplot (1:100, tryITA[,1],type = "l",xlab="time",ylim = c(1500000, 1627569),ylab="Income",main="",col=1)
lines (1:100, tryITAscenario2[,1],type = "l",xlab="",ylab="",main="",col = 2)


##################################################################################
################# third scenario: increasing the interest rate ###################
##################################################################################


pc<-sfc.addScenario(model=pc, vars="r_bar",values=(0.06),inits=5,ends=100)

datapc<-simulate(pc)

names(datapc)

tryITAscenario3<-datapc$scenario_3


matplot (1:100, tryITA[,1],type = "l",xlab="time",ylim = c(1600000, 1657569),ylab="Income",main="",col=1)
lines (1:100, tryITAscenario3[,1],type = "l",xlab="",ylab="",main="",col = 2)

###################################################################################
################################  THE END #########################################
###################################################################################