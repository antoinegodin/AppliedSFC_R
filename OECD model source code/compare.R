compare<-function(data,name,relative=T,greyScale=F,ltyScale=F,legendPos="topleft",legends,ylab="",xlab="",lwd=2,...){
  #First build the matrix
  nbScen<-length(data)
  baseline<-data$baseline[,name]
  time<-rownames(data$baseline)
  if(relative){
    res<-as.data.frame(matrix(NA,ncol=(nbScen-1), nrow=length(time)))
    for(i in 2:nbScen){
      scen<-data[[i]]
      res[,(i-1)]<-scen[,name]/baseline
    }
  }else{
    res<-as.data.frame(matrix(NA,ncol=nbScen, nrow=length(time)))
    for(i in 1:nbScen){
      scen<-data[[i]]
      res[,i]<-scen[,name]
    }    
  }
  if(greyScale){
    cols<-paste("grey",seq(0,100,round(100/ncol(res),digit=0)))
    ltys<-1
  }else if(ltyScale){
    cols<-1
    ltys<-1:ncol(res)
  }else{
    cols<-1:ncol(res)
    ltys<-1
  }
  matplot(time,res,type="l",col=cols,lty=ltys,lwd=lwd,ylab=ylab,xlab=xlab,...)
  legend(legendPos,bty='n',col=cols,lty=ltys,legend=legends,lwd=lwd)  
}