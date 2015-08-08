filepath = paste(getwd(),'/Desktop/network-data-master/build/output/data_2013_4.txt', sep ="")
data = read.table(file = filepath, header=T, dec='.', sep='\t')




#Function which compute several descriptive statistics and return a dataframe with all these statistics by carriers.




countstat = function(data) 
{
  airlines = sort(unique(data$carrier))
  nbairlines = length(airlines)
  countdata = data.frame(carriers = airlines,pax=rep(0,length(airlines)), nbroutes = rep(0,length(airlines)), nbairports = rep(0,length(airlines)), minFare = rep(0,length(airlines)), maxFare = rep(0,length(airlines)), mindistance = rep(0,length(airlines)), maxdistance = rep(0,length(airlines)), stringsAsFactors=F)
  
  for (i in 1:length(airlines))
  {
    subsample = subset(data,carrier == airlines[i])
    
    countdata$pax[i] = sum(subsample$pax)
    countdata$nbroutes[i] = nrow(subsample)
    countdata$nbairports[i] = length(unique(append(subsample$origin,subsample$dest)))
    countdata$minFare[i] = min(subsample$meanRealFare)
    countdata$maxFare[i] = max(subsample$meanRealFare)
    countdata$mindistance[i] = min(subsample$distance)
    countdata$maxdistance[i] = max(subsample$distance)
  }
  
  return (countdata)
}




#Create a text file from the descriptive statistic dataframe



uploaddesstat = function(filepath,dataframe)
{
  info = unlist(strsplit(filepath,split = '_'))
  x = length(info)
  filename = paste('C:\\Users\\lemercier\\Desktop\\Project\\Results\\Descriptive Statistics\\desstat',info[2],sep = '_')
  if (length(info)>3)
    for ( i in 3:length(info))
    {
      filename = paste(filename,info[i],sep = '_')
    }
  else {
    filename = paste(filename,info[3],sep = '_')
  }
  write.table(countdata,file = filename,col.names = T,row.names=F)
}








### Results ###



countdata = countstat(data)
# We create a dataframe with descriptive statistics
uploaddesstat(filepath,countdata)
