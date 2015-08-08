#Useful library
library(ggplot2)




# We download the data from a '.txt' file to a dataframe.

filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')





### Models ###




# Weighted OLS model 
model1 = lm(data$meanRealFare~data$distance, weights=data$pax/sum(data$pax))



#First model with netwwork measure (max degree of airports on a route)

model2 = lm(data$meanRealFare~data$distance+data$maxdegree,weights = data$pax/sum(data$pax))



model3 = lm(data$meanRealFare~data$distance+data$maxdegree+data$maxcloseness+data$maxbetweenness,weights = data$pax/sum(data$pax))


#Models with dummies and different control variables.



model4 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$WN + data$meanGDP + data$meanPopulation + data$mindegree + data$minbetweenness +data$mincloseness,weights = data$pax/sum(data$pax))


model5 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$WN + data$meanGDP + data$meanPopulation,weights = data$pax/sum(data$pax))


model6 = lm(data$meanRealFare ~ data$distance + data$maxdegree + data$maxcloseness + data$maxbetweenness + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$WN,weights = data$pax/sum(data$pax))


model7 = lm(data$meanRealFare ~ data$distance + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$WN,weights = data$pax/sum(data$pax))


## Model 8

distance = data$distance/100
distance2 = distance^2
maxdegree = data$maxdegree*10
mincloseness = data$mincloseness*10
maxbetweenness = data$maxbetweenness*10
meangdp = data$meanGDP/100000
meanpopulation = data$meanPopulation/1000



model8 = lm(data$meanRealFare ~ distance + distance2 + maxdegree + mincloseness + maxbetweenness + data$absTempDiff + data$hhiDB1B + data$marketShareRoute + meangdp + meanpopulation + data$T100loadfactor + data$AA + data$AS + data$B6 + data$DL + data$FL + data$NK + data$SY + data$UA + data$US + data$VX + data$F9,weights = data$pax^2)



#### Automated function ####





printplot = function(model,data)
{
  

# Definition of the model.


  fv = fitted(model) # We obtain a vector of fitted values.
  residuals = resid(model) # We obtain a vector of residuals.



# We plot the variation of residuals.

  plot(residuals, type = "l", main = 'Variations of residuals in the model')
  dev.print(jpeg,filename = paste('C:\\Users\\lemercier\\Desktop\\Project\\Results\\graphics\\residuals',deparse(substitute(model)),'.jpeg', sep = ''),quality = 100, width = 2500, height = 1500, res = 300)



#We plot the comparison between real data and fitted values plus 45-degree line and regression line of y~yhat

  x=seq(0,900,1)
  y=seq(0,900,1)


  plot(data$meanRealFare,fv, main = 'Comparison of empirical and \n theoretical meanRealFare',type = "p",xlim = c(0,900),ylim = c(0,900),pch=1,xlab = 'Empirical MeanReaFare',ylab = 'Modelled meanRealFare')
  lines(x~y,col='red',lwd=2)
  abline(lm(fv~data$meanRealFare,weights = data$pax/sum(data$pax)),col = 'blue',lwd = 2)
  dev.print(jpeg,filename = paste('C:\\Users\\lemercier\\Desktop\\Project\\Results\\graphics\\comparisondatafv',deparse(substitute(model)),'.jpeg', sep = ''),quality = 100, width = 2500, height = 2500, res = 300)



#We plot the empirical and modelled Kernel densities

#First, we create the adequate dataset

  datagraph1 = data.frame(meanRealFare = data$meanRealFare, Type = rep('Empirical',length(data$meanRealFare)),pax = data$pax)
  datagraph2 = data.frame(meanRealFare = fv, Type = rep('Modelled',length(fv)),pax = data$pax)
  datagraph = rbind(datagraph1,datagraph2)


  return (datagraph)
}









printplot2 = function(model,data)
{
  
  
  # Definition of the model.
  
  #We plot the empirical and modelled Kernel densities
  
  #First, we create the adequate dataset
  

  
  gplot1 = ggplot(datagraph, aes(x = datagraph$meanRealFare, weights = datagraph$pax/sum(data$pax), colour=datagraph$Type)) + geom_density( size = 1) + ggtitle('Modelled and empirical repartition of meanRealFare variable \n for all routes and carrier') + xlab('MeanRealFare (2013 q4 $)') + ylab('Probability')
  print(gplot1)
  dev.print(jpeg,filename = paste('C:\\Users\\lemercier\\Desktop\\Project\\Results\\graphics\\densities',deparse(substitute(model)),'.jpeg', sep = ''),quality = 100, width = 2500, height = 1500, res = 300)

}





datagraph = printplot(model8,data)
printplot2(model8,data)
