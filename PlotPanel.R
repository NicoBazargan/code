filepath1 = paste(getwd(),'/Desktop/network-data-master/build/output/data_20à3_4.txt', sep ="")
data20034 = read.table(file = filepath, header=T, dec='.', sep='\t')

filepath2 = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data20134 = read.table(file = filepath, header=T, dec='.', sep='\t')





distance03 = data20034$distance/100
distance203 = distance^2
maxdegree03 = data20034$maxdegree*10
mindegree03 = data20034$mindegree*10
maxcloseness03 = data20034$maxcloseness*10
mincloseness03 = data20034$mincloseness*10
maxbetweenness03 = data20034$maxbetweenness*10
minbetweenness03 = data20034$minbetweenness*10
meangdp03 = data20034$meanGDP/100000
meanpopulation03 = data20034$meanPopulation/1000
T100seats03 = data20034$T100seats/10000
herfindhal203 = data20034$hhiDB1B^2
marketShareRoute203 = data20034$marketShareRoute^2
maxeigenvector03 = data20034$maxeigenvector*10
mineigenvector03 = data20034$mineigenvector*10
density03 = data20034$density*10
routeeigenvector03 = data20034$routeeigenvector*10
routecloseness03 = data20034$routecloseness*10
routedegree03 = data20034$routedegree*10



interAA03 = data20034$AA*data20034$WNpresence
interAS03 = data20034$AS*data20034$WNpresence
interB603 = data20034$B6*data20034$WNpresence
interDL03 = data20034$DL*data20034$WNpresence
interFL03 = data20034$FL*data20034$WNpresence
interF903 = data20034$F9*data20034$WNpresence
interSY03 = data20034$SY*data20034$WNpresence
interVX03 = data20034$VX*data20034$WNpresence
interUA03 = data20034$UA*data20034$WNpresence
interNK03 = data20034$NK*data20034$WNpresence
interUS03 = data20034$US*data20034$WNpresence
interCO03 = data20034$CO*data20034$WNpresence
interHP03 = data20034$HP*data20034$WNpresence
interNW03 = data20034$NW*data20034$WNpresence
interTZ03 = data20034$TZ*data20034$WNpresence
interYX03 = data20034$YX*data20034$WNpresence



distance = data20134$distance/100
distance2 = distance^2
maxdegree = data20134$maxdegree*10
mindegree = data20134$mindegree*10
maxcloseness = data20134$maxcloseness*10
mincloseness = data20134$mincloseness*10
maxbetweenness = data20134$maxbetweenness*10
minbetweenness = data20134$minbetweenness*10
meangdp = data20134$meanGDP/100000
meanpopulation = data20134$meanPopulation/1000
T100seats = data20134$T100seats/10000
herfindhal2 = data20134$hhiDB1B^2
marketShareRoute2 = data20134$marketShareRoute^2
maxeigenvector = data20134$maxeigenvector*10
mineigenvector = data20134$mineigenvector*10
density = data20134$density*10
routeeigenvector = data20134$routeeigenvector*10
routecloseness = data20134$routecloseness*10
routedegree = data20134$routedegree*10



interAA = data20134$AA*data20134$WNpresence
interAS = data20134$AS*data20134$WNpresence
interB6 = data20134$B6*data20134$WNpresence
interDL = data20134$DL*data20134$WNpresence
interFL = data20134$FL*data20134$WNpresence
interF9 = data20134$F9*data20134$WNpresence
interSY = data20134$SY*data20134$WNpresence
interVX = data20134$VX*data20134$WNpresence
interUA = data20134$UA*data20134$WNpresence
interNK = data20134$NK*data20134$WNpresence
interUS = data20134$US*data20134$WNpresence
interCO = data20134$CO*data20134$WNpresence
interHP = data20134$HP*data20134$WNpresence
interNW = data20134$NW*data20134$WNpresence
interTZ = data20134$TZ*data20134$WNpresence
interYX = data20134$YX*data20134$WNpresence


model2013 = lm(data20134$meanRealFare ~ distance + distance2 + maxeigenvector + mineigenvector + data20134$absTempDiff + data20134$monopoly + data20134$competitive + data20134$meanGDPperCapita + T100seats + data20134$AA + interAA + data20134$AS + interAS + data20134$B6 + interB6 + data20134$DL + interDL + data20134$FL + interFL + data20134$NK + interNK + data20134$SY + interSY + data20134$UA + interUA + data20134$US + interUS + data20134$VX + interVX + data20134$F9 + interF9 , weights = data20134$pax/sum(data20134$pax))
summary(model2013)