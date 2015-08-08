###Necessary packages


library(car)
library(AER)
library(ivpack)

###Downloading of the data

filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')


### Data treatment (new varaibles and units)


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



### Model with Evans & Kessides instrument for Marketshareroute (OLS and 2SLS)


model36bis = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$marketShareRoute + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model36bis)
x = summary(model36bis)
coeftest(model36bis, vcov = vcovHC(model36bis, type = "HC0"))


iv36bis = ivreg(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$marketShareRoute + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 | distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$evans_kessides_iv + data$meanGDPperCapita + T100seats + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 + data$evans_kessides_iv, weights = data$pax/sum(data$pax))
summary(iv36bis)
robust.se(iv36bis)


### Model with Marketshareroute and capacity as intstruments for competitive and monopoly(OLS and 2SLS).


model36 = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model36)
y = summary(model36)
coeftest(model36, vcov = vcovHC(model36, type = "HC0"))

iv36= ivreg(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 | distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$evans_kessides_iv + T100seats + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 + data$evans_kessides_iv, weights = data$pax/sum(data$pax))
summary(iv36)
robust.se(iv36)


## Models with instruments only for monopoly or competitive (OLS and 2SLS)

model1iv = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model1iv)
y = summary(model1iv)
coeftest(model1iv, vcov = vcovHC(model1iv, type = "HC0"))


iv1= ivreg(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 | distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$evans_kessides_iv + data$monopoly + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 + data$evans_kessides_iv, weights = data$pax/sum(data$pax))
summary(iv1)
robust.se(iv1)



model2iv = lm(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 , weights = data$pax/sum(data$pax))
summary(model2iv)
y = summary(model2iv)
coeftest(model2iv, vcov = vcovHC(model2iv, type = "HC0"))


iv2= ivreg(data$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + data$monopoly + data$competitive + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 | distance + distance2 + maxeigenvector + mineigenvector + data$absTempDiff + T100seats + data$competitive + data$meanGDPperCapita  + data$AA + interAA + data$AS + interAS + data$B6 + interB6 + data$DL + interDL + data$FL + interFL + data$NK + interNK + data$SY + interSY + data$UA + interUA + data$US + interUS + data$VX + interVX + data$F9 + interF9 + data$evans_kessides_iv, weights = data$pax/sum(data$pax))
summary(iv2)
robust.se(iv2)