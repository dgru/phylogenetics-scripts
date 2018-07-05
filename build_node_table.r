
fileList <- list.files('~/Dropbox (MIT)/chitinase_data_shared/datedists/', full.names = T)

saveNodes <- list()
for (b in 1:length(fileList)){ # for each model
  
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
    resMat.mn <- data.frame(matrix(NA, ncol=length(fileList)+1, nrow=ncol(saveNodes[[b]])))
    #resMat.mn[,1] <- nodes
    resMat.hpd <- data.frame(matrix(NA, ncol=length(fileList)+1, nrow=ncol(saveNodes[[b]])))
    #resMat.hpd[,1] <- nodes
    
  }
  
  i1 <- c(unlist(str_locate(fileList[b], 'model')))
  i2 <- unlist(str_locate_all(fileList[b], '_'))
  names(resMat.mn)[b + 1] <- substr(fileList[b], i1[1], i2[min(which(i2 > i1))]-1)
  names(resMat.hpd)[b + 1] <- substr(fileList[b], i1[1], i2[min(which(i2 > i1))]-1)
  
  for (t in 1:ncol(saveNodes[[b]])){ # for each node of interest in this model
    
    hpd <- bfp::empiricalHpd(saveNodes[[b]][,t], level=.95)
    mn <- mean(saveNodes[[b]][,t])
    resMat.mn[t, b + 1] <- round(mn, 0)
    resMat.hpd[t, b + 1] <- paste('(',round(hpd[1], 0), '-', round(hpd[2], 0), ')', sep='')
    
  }
  
  print(paste('Tree', b, 'complete.'))
  
}

nodes <- read.table('~/Dropbox (MIT)/chitinase_data_shared/nodes_of_interest.csv', sep=',')

resMat.all <- cbind(resMat.mn, resMat.hpd)
resMat.all <- resMat.all[,order(names(resMat.all))]
resMat.all <- resMat.all[,c(1:(ncol(resMat.all)-2))]
resMat.all <- resMat.all[nodes[,1],]
resMat.all <- cbind(nodes, resMat.all)
write.table(resMat.all, file='~/Dropbox (MIT)/chitinase_data_shared/chitinase_table.csv', sep=',', col.names = T, row.names = F)
