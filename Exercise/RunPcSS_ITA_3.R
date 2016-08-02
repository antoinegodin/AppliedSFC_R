library(PKSFC)
library("Rgraphviz")


setwd("C:/Users/Federico/Desktop/SFC-Simulation-model/Model-2/")

pcss<-sfc.model("PcSS_ITA2.txt",modelName="Portfolio Choice Model")

pcss<-sfc.check(pcss,fill=FALSE)

plot_graph_hierarchy(graph=generate.DAG.collaspe(adjacency=t(pcss$matrix))$orginal_graph, main="PC" )

datapcss<-simulate(pcss)

names(datapcss)

tryITAss<-datapcss$baseline

pcss$equations



################################################################################
################################################################################