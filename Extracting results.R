#filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2003_4.txt', sep ="")
filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')



### Useful Packages ###

library(car)
library(lmtest)
library(sandwich)
library(rms)


### Models bank ###



# Weighted OLS model 
model1 = lm(data$meanRealFare~data$distance, weights=data$pax/sum(data$pax))


# First model with distance and network measure (max degree of airports on a route)


model2 = lm(data$meanRealFare~data$distance+data$maxdegree,weights = data$pax/sum(data$pax))


## Model with distance and three centrality measures


model3 = lm(data$meanRealFare~data$ditance + data$maxdegree+data$maxcloseness+data$maxbetweenness,weights = data$pax/sum(data$pax))


## Model with distance, centrality measures (the 3, min and max) and controls (carrier dummies, gdp, pop, etc..)


model4 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9 + data$meanGDP + data$meanPopulation + data$mindegree + data$minbetweenness +data$mincloseness,weights = data$pax/sum(data$pax))


## Same model without mindegree, minbetweenness and mincloseness


model5 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9 + data$meanGDP + data$meanPopulation,weights = data$pax/sum(data$pax))


## Same model without any controls except carrier dummies


model6 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))





### Models with scaled variables ###




distance = data$distance/100
distance2 = distance^2
maxdegree = data$maxdegree*10
mincloseness = data$mincloseness*10
maxbetweenness = data$maxbetweenness*10
meangdp = data$meanGDP/100000
meanpopulation = data$meanPopulation/1000



#Model with all controls but without centrality measures



model7 = lm(data$meanRealFare ~ distance + distance2  + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))


# Model with all controls and the thre centrality measures (maxdegree, mincloseness and maxbetweenness)


model8 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mincloseness + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))


# Model with all controls and maxdegree


model9 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))


# Model with all controls and mincloseness


model10 = lm(data$meanRealFare ~ distance + distance2 +  mincloseness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))


# Model with all controls and maxbetweenness


model11 = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax/sum(data$pax))





###  Same models for 2003Q4 data




model8bis = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mincloseness + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$CO + data$DL + data$FL + data$F9 +data$HP  + data$NK + data$NW  + data$SY + data$TZ + data$UA + data$US,weights = data$pax/sum(data$pax))
model9bis = lm(data$meanRealFare ~ distance + distance2 + maxdegree + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor +  data$AA + data$AS + data$B6 + data$CO + data$DL + data$FL + data$F9 +data$HP  + data$NK + data$NW  + data$SY + data$TZ + data$UA + data$US,weights = data$pax/sum(data$pax))
model10bis = lm(data$meanRealFare ~ distance + distance2 +  mincloseness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor +  data$AA + data$AS + data$B6 + data$CO + data$DL + data$FL + data$F9 +data$HP  + data$NK + data$NW  + data$SY + data$TZ + data$UA + data$US,weights = data$pax/sum(data$pax))
model11bis = lm(data$meanRealFare ~ distance + distance2 + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$CO + data$DL + data$FL + data$F9 +data$HP  + data$NK + data$NW  + data$SY + data$TZ + data$UA + data$US,weights = data$pax/sum(data$pax))








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





filename = uploadresreg(filepath,model3) ### Create the filepath for a given model.
regresult = regresults(model3,data,filename) ### Create the file with the regression results in it.
coeftest(model9, vcov = hccm) ### Print the results of the regression of the models with White's standard errors.