library("Kendall")
library("plotrix")

filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')





bccc = Kendall(data$maxcloseness,data$maxbetweenness)
bcdc = Kendall(data$maxdegree,data$maxbetweenness)
bcec = Kendall(data$maxeigenvector,data$maxbetweenness)
ccdc = Kendall(data$maxcloseness,data$maxdegree)
ccec = Kendall(data$maxcloseness,data$maxeigenvector)
dcec = Kendall(data$maxdegree,data$maxeigenvector)

vector = c(bccc$tau,bcdc$tau,bcec$tau,ccdc$tau,ccec$tau,dcec$tau)
chrvector = c("bccc","bcdc","bcec","ccdc","ccec","dcec")

radial.plot(vector,labels = chrvector,rp.type = "p", radial.lim = c(0,1))


kendallradialplot = function(data)
{
  carrier = sort(unique(data$carrier))
  kendallmatrix = matrix(,length(carrier),6)
  rownames(kendallmatrix) = carrier
  colnames(kendallmatrix) = chrvector
  for (i in 1:length(carrier))
  {
    newdata = subset(data,data$carrier == carrier[i])
    kendallmatrix[i,1] = Kendall(newdata$maxcloseness,newdata$maxbetweenness)$tau
    kendallmatrix[i,2] = Kendall(newdata$maxdegree,newdata$maxbetweenness)$tau
    kendallmatrix[i,3] = Kendall(newdata$maxeigenvector,newdata$maxbetweenness)$tau
    kendallmatrix[i,4] = Kendall(newdata$maxcloseness,newdata$maxdegree)$tau
    kendallmatrix[i,5] = Kendall(newdata$maxcloseness,newdata$maxeigenvector)$tau
    kendallmatrix[i,6] = Kendall(newdata$maxdegree,newdata$maxeigenvector)$tau

  }
  return (kendallmatrix)
}

radialmatrix = kendallradialplot(data)

radial.plot(radialmatrix,labels = chrvector,rp.type = "p", radial.lim = c(0,1))
