#filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2003_4.txt', sep ="")
filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')


### Create subsets ###


monopsubset = subset(data, hhiDB1B==1)
nmonopsubset = subset(data, hhiDB1B!=1)


### Useful Packages ###

library(car)
library(lmtest)
library(sandwich)
library(rms)



### Variable transformation ###

#All dataset

distance = data$distance/100
distance2 = distance^2
maxdegree = data$maxdegree*10
mindegree = data$mindegree*10
maxcloseness = data$maxcloseness*10
mincloseness = data$mincloseness*10
maxbetweenness = data$maxbetweenness*10
minbetweenness = data$minbetweenness*10
meangdp = data$meanGDP/100000
meanpopulation = data$meanPopulation/1000
T100seats = data$T100seats/10000
herfindhal2 = data$hhiDB1B^2
marketShareRoute2 = data$marketShareRoute^2
maxeigenvector = data$maxeigenvector*10
mineigenvector = data$mineigenvector*10
density = data$density*10
routeeigenvector = data$routeeigenvector*10
routecloseness = data$routecloseness*10
routedegree = data$routedegree*10


monopdum = function(data)
{ 
  monop = rep(0,nrow(data))
  for (i in 1:nrow(data))
  {
    if (data$hhiDB1B[i]==1)
    {
      monop[i]=1
    }
    else
    {
      monop[i]=0
    }
  }
  return (monop)
}

monopoly = monopdum(data)
monopinteraction = monopoly*data$hhiDB1B
interAA = data$AA*data$WNpresence
interAS = data$AS*data$WNpresence
interB6 = data$B6*data$WNpresence
interDL = data$DL*data$WNpresence
interFL = data$FL*data$WNpresence
interF9 = data$F9*data$WNpresence
interSY = data$SY*data$WNpresence
interVX = data$VX*data$WNpresence
interUA = data$UA*data$WNpresence
interNK = data$NK*data$WNpresence
interUS = data$US*data$WNpresence
interCO = data$CO*data$WNpresence
interHP = data$HP*data$WNpresence
interNW = data$NW*data$WNpresence
interTZ = data$TZ*data$WNpresence
interYX = data$YX*data$WNpresence
#Subsets transformations

nmonophhiDB1B2 = nmonopsubset$hhiDB1B^2
nmonopdistance = nmonopsubset$distance/100
nmonopdistance2 = nmonopdistance^2
nmonopmaxdegree = nmonopsubset$maxdegree*10
nmonopmindegree = nmonopsubset$mindegree*10
nmonopmaxcloseness = nmonopsubset$maxcloseness*10
nmonopmincloseness = nmonopsubset$mincloseness*10
nmonopmaxbetweenness = nmonopsubset$maxbetweenness*10
nmonopminbetweenness = nmonopsubset$minbetweenness*10
nmonopmeangdp = nmonopsubset$meanGDP/100000
nmonopmeanpopulation = nmonopsubset$meanPopulation/1000
nmonopT100seats = nmonopsubset$T100seats/10000
nmonopherfindhal2 = nmonopsubset$hhiDB1B^2
nmonopmarketShareRoute2 = nmonopsubset$marketShareRoute^2
nmonopinterAA = nmonopsubset$AA*nmonopsubset$WNpresence
nmonopinterAS = nmonopsubset$AS*nmonopsubset$WNpresence
nmonopinterB6 = nmonopsubset$B6*nmonopsubset$WNpresence
nmonopinterDL = nmonopsubset$DL*nmonopsubset$WNpresence
nmonopinterFL = nmonopsubset$FL*nmonopsubset$WNpresence
nmonopinterF9 = nmonopsubset$F9*nmonopsubset$WNpresence
nmonopinterSY = nmonopsubset$SY*nmonopsubset$WNpresence
nmonopinterVX = nmonopsubset$VX*nmonopsubset$WNpresence
nmonopinterUA = nmonopsubset$UA*nmonopsubset$WNpresence
nmonopinterNK = nmonopsubset$NK*nmonopsubset$WNpresence
nmonopinterUS = nmonopsubset$US*nmonopsubset$WNpresence


monophhiDB1B2 = monopsubset$hhiDB1B^2
monopdistance = monopsubset$distance/100
monopdistance2 = monopdistance^2
monopmaxdegree = monopsubset$maxdegree*10
monopmindegree = monopsubset$mindegree*10
monopmaxcloseness = monopsubset$maxcloseness*10
monopmincloseness = monopsubset$mincloseness*10
monopmaxbetweenness = monopsubset$maxbetweenness*10
monopminbetweenness = monopsubset$minbetweenness*10
monopmeangdp = monopsubset$meanGDP/100000
monopmeanpopulation = monopsubset$meanPopulation/1000
monopT100seats = monopsubset$T100seats/10000
monopherfindhal2 = monopsubset$hhiDB1B^2
monopmarketShareRoute2 = monopsubset$marketShareRoute^2

### Models ###



# Meeting 3 bank of models with maxdegree #


model9 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model9)

model10 = lm(data$meanRealFare ~ distance + distance2 +  mincloseness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model10)

model11 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model11)



# Robustness without distance2 #


model12 = lm(data$meanRealFare ~ distance  + maxdegree + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model12)

model13 = lm(data$meanRealFare ~ distance +  mincloseness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model13)

model14 = lm(data$meanRealFare ~ distance + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model14)


# New models with max and min #


#Benchmark model without centrality measures


model24 = lm(data$meanRealFare ~ distance + distance2 + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model24)


#Models with minnetwork measures

model15 = lm(data$meanRealFare ~ distance + distance2 + mindegree + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model15)

model16 = lm(data$meanRealFare ~ distance + distance2 +  mincloseness + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model16)

model17 = lm(data$meanRealFare ~ distance + distance2 + minbetweenness + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + meanpopulation + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model17)


#Models with maxnetwork measures


model18 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model18)

model19 = lm(data$meanRealFare ~ distance + distance2 +  maxcloseness + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita  + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model19)

model20 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + data$absTempDiff + data$hhiDB1B + + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model20)



#Models with min and max network measures



model21 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model21)

model22 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model22)

model23 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model23)



#Models with the monopoly dummy variable


model25 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$hhiDB1B + monopinteraction + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model25)

model26 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$hhiDB1B + herfindhal2 + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model26)

model27 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$marketShareRoute + marketShareRoute2 + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model27)

model32 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + monopoly + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model32)

        #Model with monop and nonmonop routes separated (on subsets)

model30 = lm(nmonopsubset$meanRealFare ~ nmonopdistance + nmonopdistance2 + nmonopmaxdegree + nmonopmindegree + nmonopsubset$absTempDiff + nmonopsubset$hhiDB1B + nmonophhiDB1B2 + nmonopsubset$meanGDPperCapita + nmonopT100seats + nmonopsubset$AA + nmonopsubset$AS + nmonopsubset$B6 + nmonopsubset$DL + nmonopsubset$FL + nmonopsubset$NK + nmonopsubset$SY + nmonopsubset$UA + nmonopsubset$US + nmonopsubset$VX + nmonopsubset$F9,weights = nmonopsubset$pax)
summary(model30)

model31 = lm(monopsubset$meanRealFare ~ monopdistance + monopdistance2 + monopmaxdegree + monopmindegree + monopsubset$absTempDiff + monopsubset$meanGDPperCapita + monopT100seats + monopsubset$AA + monopsubset$AS + monopsubset$B6 + monopsubset$DL + monopsubset$FL + monopsubset$NK + monopsubset$SY + monopsubset$UA + monopsubset$US + monopsubset$F9 , weights = monopsubset$pax)
summary(model31)


# Models with Southwest dummmy variable


model28 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$marketShareRoute  + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model28)

model29 = lm(data$meanRealFare ~ distance + distance2 + maxdegree  + data$absTempDiff + data$hhiDB1B  + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model29)


#Models with eigenvector centrality

model33 = lm(data$meanRealFare ~ distance + distance2 + mineigenvector + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model33)

model34 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model34)

model35 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$hhiDB1B + data$meanGDPperCapita + T100seats + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))
summary(model35)


#Models with monopoly, concurrence and duopoly dummies

model60 = lm(data$meanRealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model60)

model36 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model36)

model37 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model37)

model38 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model38)

model39 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model39)


#Models with 25th percentile

model61 = lm(data$p25RealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model61)

model40 = lm(data$p25RealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model40)

model41 = lm(data$p25RealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model41)

model42 = lm(data$p25RealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model42)

model43 = lm(data$p25RealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model43)


# Models with 75th percentile

model62 = lm(data$p75RealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model62)

model44 = lm(data$p75RealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model44)

model45 = lm(data$p75RealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model45)

model46 = lm(data$p75RealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model46)

model47 = lm(data$p75RealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model47)


# Models with carriers density instead of carriers fixed effects


model48 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + density , weights = data$pax/sum(data$pax))
summary(model48)

model49 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + density , weights = data$pax/sum(data$pax))
summary(model49)

model50 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + density , weights = data$pax/sum(data$pax))
summary(model50)

model51 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + density, weights = data$pax/sum(data$pax))
summary(model51)



#Models with airport level HHi instead of route HHi

model52 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$minAirportHHI +  data$maxAirportHHI + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model52)

model53 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$minAirportHHI +  data$maxAirportHHI + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model53)

model54 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$minAirportHHI +  data$maxAirportHHI + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model54)

model55 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$minAirportHHI +  data$maxAirportHHI + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model55)


#Models with airport level marketshare instead of route marketshare

model56 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model56)

model57 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model57)

model58 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model58)

model59 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model59)


#Meeting 4 model with 2003 data


model63 = lm(data$meanRealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$CO + interCO + data$HP + interHP + data$NW + interNW + data$TZ + interTZ + data$YX + interYX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model63)

model64 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$CO + interCO + data$HP + interHP + data$NW + interNW + data$TZ + interTZ + data$YX + interYX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model64)

model65 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$CO + interCO + data$HP + interHP + data$NW + interNW + data$TZ + interTZ + data$YX + interYX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model65)

model66 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$CO + interCO + data$HP + interHP + data$NW + interNW + data$TZ + interTZ + data$YX + interYX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model66)

model67 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$CO + interCO + data$HP + interHP + data$NW + interNW + data$TZ + interTZ + data$YX + interYX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model67)


### Test for instruments with eigenvector centrality

model68 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$monopoly + data$competitive +  data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model68)


model69 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$minAirportMktshr +  data$maxAirportMktshr + data$meanGDPperCapita + T100seats + data$marketShareRoute +  data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model69)


### Models with route centrality


model70 = lm(data$meanRealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model70)

model71 = lm(data$meanRealFare ~ distance + distance2 + routeeigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model71)

model72 = lm(data$meanRealFare ~ distance + distance2 + routecloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model72)

model73 = lm(data$meanRealFare ~ distance + distance2 + routedegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model73)


#Models with 50th percentile

model74 = lm(data$p50RealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model74)

model75 = lm(data$p50RealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model75)

model76 = lm(data$p50RealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model76)

model77 = lm(data$p50RealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model77)

model78 = lm(data$p50RealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model78)




#Baseline model with airportHHI

model79 = lm(data$meanRealFare ~ distance + distance2 +  data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model79)

model80 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model80)

model81 = lm(data$meanRealFare ~ distance + distance2 + maxcloseness + mincloseness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model81)

model82 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mindegree + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model82)

model83 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + minbetweenness + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model83)



### Function to upload the table of results ###





uploadresreg = function(filepath,model)
{
  info = unlist(strsplit(filepath,split = '_'))
  x = length(info)
  filename = paste('C:\\Users\\lemercier\\Desktop\\Project\\Results\\RegressionResults\\',deparse(substitute(model)),info[2],sep = '_')
  print(filename)
  if (length(info)>3)
    for ( i in 3:length(info))
    {
      filename = paste(filename,info[i],sep = '_')
    }
  else {
    filename = paste(filename,info[3],sep = '_')
  }
}






regresults = function(model,data,filename)
{
  print(summary(model))
  results = data.frame(summary(model)$coefficients, adjR2 = rep(0,nrow(summary(model)$coefficients)))
  results$adjR2[1] = summary(model)$adj.r.squared
  results = signif(results,3)
  write.table(results,file = filename,col.names = T) 
  
  return (results)
}





filename = uploadresreg(filepath,model36) ### Create the filepath for a given model.
regresult = regresults(model36,data,filename) ### Create the file with the regression results in it.
coeftest(model67, vcov = vcovHC(model67, type = "HC0")) ### Print the results of the regression of the models with White's standard errors.
