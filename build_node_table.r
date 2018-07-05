
fileList <- list.files()

saveNodes <- list()
for (b in 1:length(fileList)){
  
  MyTrees <- ape::read.tree(fileList[[b]])
  n=0
  nodeDates = matrix(ncol = length(MyTrees[[1]]$node.label), nrow=1000000) ###arbitrary number of rows, can change it to the exact number of trees in datedist file if you want
  for (i in MyTrees){
    dates <- as.numeric(i$node.label)
    n <- n + 1
    nodeDates[n,] <- dates
  }
  
  saveNodes[[b]] <- nodeDates[complete.cases(nodeDates),]
  
  if (b == 1){
    resMat <- data.frame(matrix(NA, ncol=length(fileList), nrow=ncol(saveNodes[[b]])))
    names(resMat) <- fileList
  }
  
  for (t in 1:ncol(saveNodes[[b]])){
    #hpd <- TeachingDemos::emp.hpd(saveNodes[[b]][,t], conf=0.95)  
    hpd <- bfp::empiricalHpd(saveNodes[[b]][,t], level=.95)
    mn <- mean(saveNodes[[b]][,t])
    resMat[t,b] <- paste(round(mn, 1), ' (', round(hpd[1], 1), '-', round(hpd[2], 1), ')', sep='')
  }
  
  print(paste('Tree', b, 'complete.'))
  
}


