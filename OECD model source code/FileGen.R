library(dplyr)

hpfilter <- function(x,lambda=6.25){ 
  eye <- diag(length(x)) 
  result <- solve(eye+lambda*crossprod(diff(eye,lag=1,d=2)),x) 
  return(result) 
} 

predHP<-function(xHP){
  predx<-2*xHP[length(xHP)]-xHP[length(xHP)-1]
  return(predx)
}


# Datasets Load ------------------------------------------------------------

Output<-read.csv("OECD_output.csv")
GovRevData<-read.csv("GovRev.csv")
GovDefData<-read.csv("GovDef.csv")
GovDebtData<-read.csv("GovDebt.csv")
SavRates<-read.csv("HouseholdsSavingRates.csv")
BS<-read.csv("OECD_BS.csv",check.names=F,na.strings="..")
GDP<-read.csv("GDP.csv")
EnCap<-read.csv("EnCap.csv")
EnCons<-read.csv("EnergyConsumption.csv")
OECDCountries<-read.csv("Countries.csv")
countries<-as.character(OECDCountries$name)

# GDP Components ----------------------------------------------------------

Results<-{}
Results$year<-Output$Year
Results<-as.data.frame(Results)
Results$Y<-Output$GDP+Output$X-Output$M
Results$G<-Output$G
Results$I<-Output$I
Results$C<-Results$Y-Results$G-Results$I
Results$Pc<-Output$GDP/Output$gdp
Results$y<-Output$gdp

attach(Results)

# Government --------------------------------------------------------------

#Expenditures
Results$g<-Results$G/Results$Pc

#Taxes
countriesGrev<-colnames(GovRevData)[colnames(GovRevData) %in% countries]
countriesGrev<-colnames(GDP)[colnames(GDP) %in% countriesGrev]
OECDGDPGrev<-GDP[1:nrow(GovRevData),countriesGrev]
OECDGovRev<-GovRevData[countriesGrev]
rev=OECDGovRev*OECDGDPGrev
avRev=rowSums(rev,na.rm=T)/rowSums(OECDGDPGrev*!is.na(OECDGovRev))
Results$Taxes<-Results$Y*avRev[1:19]/100

#Tax rate
Results$theta<-Results$Taxes/Results$Y

#Deficit and Uses
countriesGdef<-colnames(GovDefData)[colnames(GovDefData) %in% countries]
countriesGdef<-colnames(GDP)[colnames(GDP) %in% countriesGdef]
OECDGovDef<-GovDefData[countriesGdef]
OECDGDPGdef<-GDP[1:nrow(OECDGovDef),countriesGdef]
def=OECDGovDef*OECDGDPGdef
avDef=-rowSums(def,na.rm=T)/rowSums(OECDGDPGdef*!is.na(OECDGovDef))
Results$GovUses<-Results$Y*avDef[1:19]/100+Results$Taxes

#Net Lending Position, sectorial balance.
Results$NL_gov<-Results$Taxes-Results$GovUses


#Bonds time series, start from actual value in 1995
countriesGdebt<-colnames(GovDebtData)[colnames(GovDebtData) %in% countries]
countriesGdebt<-colnames(GDP)[colnames(GDP) %in% countriesGdebt]
OECDGovDebt<-GovDebtData[countriesGdebt]
OECDGDPGdebt<-GDP[1:nrow(GovDebtData),countriesGdebt]
debt=OECDGovDebt*OECDGDPGdebt
avDebt=rowSums(debt,na.rm=T)/rowSums(OECDGDPGdebt*!is.na(OECDGovDebt))

Results$B<-c(Results$Y[1:18]*avDebt/100,NA)
for(i in 2:length(Results$B)){
  Results$B[i]<-Results$B[i-1]-Results$NL_gov[i]
}

#Interests payments. Assumption: all non G uses are interest payments: this leads to a high interest rate, going to banks but being distributed afterwards to households via dividends
Results$BondsInterests<-Results$GovUses-Results$G
Results$iB<-c(NA,Results$BondsInterests[-1]/Results$B[-19])

detach(Results)
attach(Results)

# Households --------------------------------------------------------------


Results$c<-Results$C/Results$Pc

#Computing disposable income
countriesSav<-colnames(SavRates)[colnames(SavRates) %in% countries]
countriesSav<-colnames(GDP)[colnames(GDP) %in% countriesSav]
OECDGDPSav<-GDP[1:nrow(SavRates),countriesSav]
OECDSAV<-SavRates[countriesSav]
sav=OECDSAV*OECDGDPSav
avSav=rowSums(sav,na.rm=T)/rowSums(OECDGDPSav*!is.na(OECDSAV))

Results$YD<-Results$C/(1-avSav[1:19]/100)
Results$yd<-Results$YD/Results$Pc

#Net lending position
Results$NL_hh<-Results$YD-Results$C

#Deposits time series, start from actual value in 1995
Sys.setlocale('LC_ALL','C') 
dicLines<-readLines("geo.dic",skipNul=F)
dicRecords<-sapply(dicLines,strsplit,"\t")
dic<-matrix((unlist(sapply(dicRecords,rbind))),byrow=T,ncol=2)
rownames(dic)<-NULL
colnames(dic)<-c("V1", "V2")

Dep_C<-filter(BS,Sector=="S14_15",Transaction=="F2",Position=="ASS")
Dep_C<-Dep_C[,c(3,5:23)]
Dep_C<-merge(Dep_C, dic, by.x="Country", by.y="V1")
temp<-t(Dep_C[2:20])
colnames(temp)<-Dep_C$V2
temp<-as.data.frame(temp)
temp$Year<-rownames(temp)
Dep_C<-temp

countriesDep<-colnames(Dep_C)[colnames(Dep_C) %in% countries]
countriesDep<-colnames(GDP)[colnames(GDP) %in% countriesDep]
OECDGDPDep<-GDP[1:nrow(Dep_C),countriesDep]
OECDDep<-Dep_C[countriesDep]
DepShareGDP<-OECDDep/OECDGDPDep
avDepShare<-rowSums(OECDDep,na.rm=T)/rowSums(OECDGDPDep*!is.na(DepShareGDP))

Results$D<-avDepShare*Results$Y
for(i in 2:length(Results$D)){
  Results$D[i]<-Results$D[i-1]+Results$NL_hh[i]
}

Results$v<-Results$D/Results$Pc
Results$d<-Results$D/Results$Pc

#Consumption function
dum1<-1*(Results$year>=2006)
dum2<-Results$yd*(Results$year>=2006)
model.C<-lm(Results$c[-1]~Results$yd[-19]+Results$v[-19]-1)
#Results$alpha0<-coef(model.C)[3]
Results$alpha1<-coef(model.C)[1]#+coef(model.C)[4]
Results$alpha2<-coef(model.C)[2]

detach(Results)
attach(Results)

# Firms -------------------------------------------------------------------

#Energy capacity -> real investment
Results$LTg<-25
Results$LTd<-35
Results$Pkg<-c(EnCons$pkg,NA)
Results$Pkd<-c(EnCons$pkd,NA)
Results$kg<-c(rowSums(EnCap[c("Renewables")]),NA)
Results$Kg<-Results$kg*Results$Pkg
Results$kd<-c(rowSums(EnCap[c("Hydro","Combustible.fuels","Nuclear","Other")]),NA)
Results$Kd<-Results$kd*Results$Pkd
Results$ig<-c(NA,Results$kg[-1]-(1-1/Results$LTg[1])*Results$kg[-19])
Results$id<-c(NA,Results$kd[-1]-(1-1/Results$LTd[1])*Results$kd[-19])
Results$Ig<-Results$ig*Results$Pkg
Results$Id<-Results$id*Results$Pkd

detach(Results)
attach(Results)

#Cosumpton Firms investment
Results$Ic<-I-Id-Ig
Results$ic<-Results$Ic/Pc

#capital stock
Results$delta<-0.1
Results$grI<-c(NA,(Results$ic[-1]-Results$ic[-19])/Results$ic[-19])
avGr_I<-mean(Results$grI,na.rm=T)
kInit<-Results$ic[2]/(avGr_I+Results$delta)
Results$kc<-kInit
for(i in 2:length(Results$year)){
  Results$kc[i]<-Results$kc[i-1]*(1-Results$delta[i])+Results$ic[i]
}
Results$Kc<-Results$kc*Pc

#Energy Consumption
#Flling the gap in the energy time series
Results$eg<-c(EnCons$e_g,NA)
a<-(Results$eg[4]-Results$eg[1])/3
b<-Results$eg[1]-a
Results$eg[2]<-a*2+b
Results$eg[3]<-a*3+b
Results$ed<-c(EnCons$e_d,NA)
a<-(Results$ed[4]-Results$ed[1])/3
b<-Results$ed[1]-a
Results$ed[2]<-a*2+b
Results$ed[3]<-a*3+b
Results$etag<-c(EnCons$eta_g,NA)
a<-(Results$eta_g[4]-Results$eta_g[1])/3
b<-Results$eta_g[1]-a
Results$eta_g[2]<-a*2+b
Results$eta_g[3]<-a*3+b
Results$prod_eg<-c(EnCons$prod_e_g,NA)
a<-(Results$xi_kg[4]-Results$prod_eg[1])/3
b<-Results$xi_kg[1]-a
Results$xi_kg[2]<-a*2+b
Results$xi_kg[3]<-a*3+b
Results$xi_kd<-c(EnCons$prod_e_d,NA)
a<-(Results$xi_kd[4]-Results$xi_kd[1])/3
b<-Results$xi_kd[1]-a
Results$xi_kd[2]<-a*2+b
Results$xi_kd[3]<-a*3+b
Results$Pe<-c(EnCons$p_e,NA)
Results$Prd<-c(EnCons$p_r_d/1e3,NA)
Results$etad<-0.85
Results$e<-Results$ed+Results$eg
Results$Eg<-Results$eg*Results$Pe
Results$Ed<-Results$ed*Results$Pe
Results$shareeg<-Results$eg/Results$e
Results$shareed<-Results$ed/Results$e
detach(Results)
attach(Results)

#Loans
Results$iLc<-0.04
Results$iLd<-0.04
Results$iLg<-0.07

# levg<-0.4
# Results$taug<-0.05
# Results$Lg<-Results$Kg*levg
# Results$distcost<-0.1
# Results$inflow<-0
# Results$Fg<-NA
# for(i in 2:19){
#   Results$Fg[i]<-Eg[i]*(1-Results$distcost[i]+Results$taug[i])-Results$iLg[i-1]*Results$Lg[i-1]
#   Results$Lg[i]<-Results$Lg[i-1]+Ig[i]*levg
#   Results$inflow[i]<-Ig[i]*(1-levg)-Results$Fg[i]
# }
# Results$subsg<-Eg*(1-Results$distcost)+Results$iLg*Results$Lg
# Results$alphag<-(Results$iLg*(1+Results$iLg)^LTg)/((1+Results$iLg)^LTg-1)
# Results$UCg<-c(NA,Results$alphag[-1]*Kg[-19])
# Results$taug<-Results$subsg/Results$UCg
# 

levg<-0.7
Results$Lg<-Results$Kg*levg
Results$distcost<-0
Results$Fg<-Ig*(1-levg)
for(i in 2:19){
  Results$Lg[i]<-Results$Lg[i-1]+Ig[i]-Results$Fg[i]
}
Results$subsg<-Results$Fg-Eg*(1-Results$distcost)+Results$iLg*Results$Lg
Results$alphag<-(Results$iLg*(1+Results$iLg)^LTg)/((1+Results$iLg)^LTg-1)
Results$UCg<-c(NA,Results$alphag[-1]*Kg[-19])
Results$taug<-Results$subsg/Results$Eg


levd<-0.6
#in mln$/GWh
Results$xi_rd<-0.4
Results$Rd<-Results$Prd*ed/Results$xi_rd
Results$Ld<-Results$Kd*levd
Results$Fd<-0
for(i in 2:19){
  Results$Fd[i]<-Ed[i]*(1-Results$distcost[i])-Results$subsg[i]-Results$iLd[i-1]*Results$Ld[i-1]-Results$Rd[i]
  Results$Ld[i]<-Results$Ld[i-1]+Id[i]-Results$Fd[i]
}

Results$L<-Results$D
Results$Lc<-Results$L-Results$Ld-Results$Lg

Results$Fb<-c(NA,(Results$iLc*Results$Lc+Results$iLd*Results$Ld+Results$iLg*Results$Lg+Results$iB*Results$B)[-19]-Results$B[-1]+Results$B[-19])
Results$WB<-YD-Results$Fb
Results$N<-Output$N
Results$W<-Results$WB/Results$N

detach(Results)
attach(Results)

#Computig the "homogeneized gdp"
ad<-Pc/Pkd
ag<-Pc/Pkg
gdpHom<-c+g+ic+ig/ag+id/ad
#Productivities
Results$xi_nc<-gdpHom/Results$N
Results$xi_ec<-gdpHom/e
Results$xi_nd<-ad*Results$xi_nc
Results$xi_ng<-ag*Results$xi_nc
Results$xi_ed<-ad*Results$xi_ec
Results$xi_eg<-ag*Results$xi_ec
#Interests distributions
Results$intLc<-iLc*Lc
UCintc<-Results$intLc/gdpHom
UCintd<-UCintc/ad
UCintg<-UCintc/ag

#Unit costs
Results$UCc<-(W/Results$xi_nc+Pe/Results$xi_ec+UCintc)
Results$UCkd<-(W/Results$xi_nd+Pe/Results$xi_ed+UCintd)
Results$UCkg<-(W/Results$xi_ng+Pe/Results$xi_eg+UCintg)
Results$phic<-Pc/Results$UCc-1

detach(Results)
attach(Results)

#Productivity of capital, usg hp filetr to get the cycle out (Capacity utilisation)
prk<-c(NA,gdpHom[-1]/kc[-19])

prk_hp<-hpfilter(prk[2:13])
for(i in 1:6){
  prk_hp=c(prk_hp,predHP(prk_hp))
  prk_hp=hpfilter(prk_hp)
}
prk_hp<-c(NA,prk_hp)

Results$ukc<-0.8*(prk/prk_hp)
Results$xi_kc<-prk/Results$ukc

detach(Results)
attach(Results)

#Energy sector profits and growth


#TODO: What to do with the "loss" of energy while produced? Should enter as a cost for the producers, even the green.



#p_e in $/MWh
#e_g in TWh
#sales in mln$


Results$VCd<-Results$Rd/ed
Results$phie<-Pe/Results$VCd-1

#Profit rate
Results$retd<-Results$Fd/Kd
Results$retg<-Results$Fg/Kg

#Growth rates
Results$grg<-c(NA,(kg[-1]-kg[-19])/kg[-19])
Results$grd<-c(NA,(kd[-1]-kd[-19])/kd[-19])

Results$kdA<-etad*kd
Results$kgA<-etag*kg
Results$xi_kg<-8.760
Results$xi_kd<-8.760

Results$kdu<-1.15*ed/Results$xi_kd
Results$kgu<-1.15*eg/Results$xi_kg

Results$ukd<-Results$kdu/Results$kdA
Results$ukg<-Results$kgu/Results$kgA

detach(Results)
attach(Results)

dumretd<-(Results$year==2007)*1
model.grkd<-lm(grd[-1]~ukd[-19]+(iLd*Ld/Kd)[-19]-1)

#Accountig for the fact that the capacity utilisation of the green sector jumps to one
# Results$gamma0d<-0
# Results$gamma1d<-coef(model.grkd)[1]*(kdu-(1-ukg)*kgA)/kdu
# Results$gamma2d<-coef(model.grkd)[2]

Results$gamma0d<-0
Results$gamma1d<-coef(model.grkd)[1]
Results$gamma2d<-coef(model.grkd)[2]


model.grkg<-lm(grg[-1]~retg[-19]+ukg[-19])

#Accountig for the fact that the capacity utilisation of the green sector jumps to one
#Results$ukdMod<-(kdu-(1-ukg)*kgA)/kdA

#Results$gamma0g<-0
#Results$gamma1g<-0.8*coef(model.grkg)[1]
#Results$gamma2g<-0

#Here forcing the same impact of leverage on green firms, even if not statistically significant
Results$gamma0g<-coef(model.grkg)[1]
Results$gamma1g<-coef(model.grkg)[2]
Results$gamma2g<-coef(model.grkg)[3]*0.8


detach(Results)
attach(Results)

Results$Fc<-Y-W*N-Eg-Ed-iLc*Lc-Taxes
Results$retc<-Results$Fc/Kc
Results$levc<-iLc*Lc/Kc
Results$grc<-c(NA,(kc[-1]-kc[-19])/kc[-19])
dum<-Results$retc*(Results$year %in% c(2009,2010))
model.kc<-lm(Results$grc[-1]~Results$retc[-1]+dum[-1]-1)#+lev[-1])

Results$gamma0c<-0
Results$gamma1c<-coef(model.kc)[1]
Results$gamma2c<-0


detach(Results)
attach(Results)


# Growth rates ------------------------------------------------------------

#trends growth rates
#Govt Expenditures
Results$gr.g<-c(NA,(g[-1]-g[-19])/g[-19])
dum2<-(Results$year %in% c(2008,2009))*1
model.grg<-lm(Results$gr.g[-1]~dum2[-1])
Results$grgexp<-coef(model.grg)[1]

#Labor prod in non-energy sector
Results$gr.xi_nc<-c(NA,(xi_nc[-1]-xi_nc[-19])/xi_nc[-19])
dum2<-(Results$year %in% c(2008:2009))*1
model.grprnc<-lm(Results$gr.xi_nc~dum2)
Results$grprnc<-coef(model.grprnc)[1]

#Electricity prod in non-energy sector
Results$gr.xi_ec<-c(NA,(xi_ec[-1]-xi_ec[-19])/xi_ec[-19])
#dum2<-(Results$year %in% c(2008:2010))*1
model.grprec<-lm(Results$gr.xi_ec~1)
Results$grprec<-coef(model.grprec)[1]

#Labor prod in energy sector
Results$gr.prng<-c(NA,(xi_ng[-1]-xi_ng[-19])/xi_ng[-19])
dum2<-(Results$year %in% c(2008:2009))*1
model.grprng<-lm(Results$gr.prng~dum2)
Results$grprng<-coef(model.grprng)[1]

#Wages
Results$gr.W<-c(NA,(W[-1]-W[-19])/W[-19])
dum2<-(Results$year %in% c(2008:2009))*1
model.grW<-lm(Results$gr.W~dum2)
Results$grw<-coef(model.grW)[1]

#Wages
Results$grprd<-0


#Taug
model.taug<-lm(log(Results$taug[7:19])~year[7:19])
Results$betag<-coef(model.taug)[2]*10.108

Results$gc<-c(NA,(c[-1]-c[-19])/c[-19])
Results$gy<-c(NA,(y[-1]-y[-19])/y[-19])
Results$gY<-c(NA,(Y[-1]-Y[-19])/Y[-19])
Results$gic<-c(NA,(ic[-1]-ic[-19])/ic[-19])

detach(Results)
attach(Results)

# Generatig the file ------------------------------------------------------

sink("calibration.txt")
t=17

#Real government expenditure
cat("#Public expenditure\n")
cat(paste("g=",g[t],"\n"))
cat("#Productivity of labor\n")
cat(paste("xi_nc=",xi_nc[t],"\n"))
cat(paste("xi_ng=",xi_ng[t],"\n"))
cat(paste("xi_nd=",xi_nd[t],"\n"))
cat("#Productivity of capital, consumption sector\n")
cat(paste("xi_kc=",xi_kc[t],"\n"))
cat("#Productivity of capital, dirty energy sector\n")
cat(paste("xi_kd=",xi_kd[t],"\n"))
cat("#Productivity of capital, green energy sector\n")
cat(paste("xi_kg=",xi_kd[t],"\n"))
cat("#Energy productivity range from 2 to 3.3\n")
cat(paste("xi_ec=",xi_ec[t],"\n"))
cat("#Productivity of dirty energy imputs\n")
cat(paste("xi_rd=",xi_rd[t],"\n"))
cat("#Productivity of green energy imputs\n")
cat(paste("xi_rg=",0,"\n"))
cat("#Capacity factor for dirty energy production\n")
cat(paste("eta_d=",etad[t],"\n"))
cat("#Capacity factor for green energy production\n")
cat(paste("eta_g=",etag[t],"\n"))
cat("#Prices of inputs\n")
cat(paste("Prd=",Prd[t],"\n"))
cat(paste("Prg=",0,"\n"))
cat("#Life of capital for dirty energy production\n")
cat(paste("LTd=",LTd[t],"\n"))
cat("#Life of capital for dirty energy production\n")
cat(paste("LTg=",LTg[t],"\n"))
cat("#Mark-ups\n")
cat(paste("phie=",phie[t],"\n"))
cat(paste("phic=",phic[t],"\n"))
cat("#Growth function\n")
cat(paste("gamma0c=",gamma0c[t],"\n"))
cat(paste("gamma1c=",gamma1c[t],"\n"))
cat(paste("gamma2c=",gamma2c[t],"\n"))
cat(paste("gamma0d=",gamma0d[t],"\n"))
cat(paste("gamma1d=",gamma1d[t],"\n"))
cat(paste("gamma2d=",gamma2d[t],"\n"))
cat(paste("gamma0g=",gamma0g[t],"\n"))
cat(paste("gamma1g=",gamma1g[t],"\n"))
cat(paste("gamma2g=",gamma2g[t],"\n"))
cat("#Depreciation rate\n")
cat(paste("delta=",delta[t],"\n"))
cat("#Subsides and taxes on energy prices\n")
cat(paste("taud=",0,"\n"))#TODO
cat("#Consumption function\n")
cat(paste("alpha0=",0,"\n"))#TODO
cat(paste("alpha1=",alpha1[t],"\n"))
cat(paste("alpha2=",alpha2[t],"\n"))
cat("#Tax rate\n")
cat(paste("theta=",theta[t],"\n"))
cat("#Expectation parameter\n")
cat(paste("nu=",0.1,"\n"))#TODO
cat("#Wages\n")
cat(paste("W=",W[t],"\n"))#TODO
cat("#R&D\n")
cat(paste("RandD=",0,"\n"))#TODO
cat("#Interest rates settings\n")
cat(paste("kappa=",0,"\n"))#TODO
cat(paste("rb=",0.01,"\n"))#TODO
cat("#Price of Dirty energy Capital\n")
cat(paste("Pkd=",Pkd[t],"\n"))
cat("#Price of Green energy Capital\n")
cat(paste("Pkg=",Pkg[t],"\n"))
cat("################\n")
cat("#INITIAL VALUES#\n")
cat("################\n")
cat(paste("ukc=",ukc[t],"\n"))
cat(paste("ukd=",ukd[t],"\n"))
cat(paste("ukg=",1,"\n"))
cat(paste("kd=",kd[t],"\n"))
cat(paste("kg=",kg[t],"\n"))
cat(paste("kc=",kc[t],"\n"))
cat(paste("Kd=",Kd[t],"\n"))
cat(paste("Kg=",Kg[t],"\n"))
cat(paste("Kc=",Kc[t],"\n"))
cat(paste("iLc=",iLc[t],"\n"))
cat(paste("iLd=",iLd[t],"\n"))
cat(paste("iLg=",iLg[t],"\n"))
cat(paste("ib=",iB[t],"\n"))
cat(paste("yd=",yd[t],"\n"))
cat(paste("y=",y[t],"\n"))
cat(paste("c=",c[t],"\n"))
cat(paste("yde=",yd[t],"\n"))
cat(paste("D=",D[t],"\n"))
cat(paste("d=",d[t],"\n"))
cat(paste("B=",B[t],"\n"))
cat(paste("Bs=",B[t],"\n"))
cat(paste("Pc=",Pc[t],"\n"))
cat(paste("Ld=",Ld[t],"\n"))
cat(paste("Lg=",Lg[t],"\n"))
cat(paste("Lc=",Lc[t],"\n"))
cat(paste("L=",L[t],"\n"))
cat(paste("retc=",retc[t],"\n"))
cat(paste("retg=",retg[t],"\n"))
cat(paste("taug=",taug[t],"\n"))
cat("######Growth rates\n")
cat(paste("grgexp=",grgexp[t],"\n"))
cat(paste("grprnc=",grprnc[t],"\n"))
cat(paste("grprng=",grprng[t],"\n"))
cat(paste("grprnd=",grprng[t],"\n"))
cat(paste("grw=",grw[t],"\n"))
cat(paste("grprec=",grprec[t],"\n"))
cat(paste("grprd=",grprd[t],"\n"))
cat(paste("betag=",betag[t],"\n"))
sink()
#unlink("Output.txt")