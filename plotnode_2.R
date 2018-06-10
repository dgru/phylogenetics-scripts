# the directory where your file(s) live
#setwd('~/Documents/WHOI/RCode/GruenDS/')
setwd("/Users/dgruen/Desktop/clusterfucklocal/datedists_2018_06_10")

# get list of all files in your current directory
fileList <- list.files()
# make a vector of all files in your working directory whose filename contains 'datedist'
fileList <- fileList[grep('datedist', fileList)]
#fileList <- fileList[-grep('.pdf', fileList)]

saveNodes <- plotNode(inputFiles=fileList, node=c(147,150,155,165,174,181,186,212,214,215), plotLayout=c(5,2), returnData = TRUE, plotTrees = TRUE, plot=TRUE)
# rows, columns

#' @param inputFiles is character vector of filenames in your working directory.
#'   the lines and their corresponding color order will be determined by the
#'   order of the files. a printout from the function while its running will
#'   tell you which color corresponds to which input file.
#' @param node is numeric vector corresponding to the node of interest. CAUTION:
#'   this currently calls treeMatrix[,node] (e.g. uses the input node number to
#'   select a column of the tree matrix). This is NOT checked for errors or
#'   mismatches in the tree output!
#' @param plot is logical indicating whether or not to draw a plot
#' @param plotLayout is 2 element vector that determines the number of rows,
#'   columns desired in output plot
#'

plotNode <- function(inputFiles, node, plot=TRUE, plotLayout=NULL, returnData=FALSE, plotTrees=FALSE){
  
  #gg_color_hue <- function(n) {
  #  hues = seq(15, 375, length = n + 1)
  #  hcl(h = hues, l = 65, c = 100)[1:n]
  #}
  
  #pal <- rep(brewer.pal(6, 'Set1'), 4)
  pal <- rep(RColorBrewer::brewer.pal(6, 'Set1'), 4)
  
  fileList <- as.list(inputFiles)
  print(paste('Reading', length(fileList), 'input files and building trees...'))
  saveNodes <- list()
  for (b in 1:length(fileList)){
    MyTrees <- ape::read.tree(fileList[[b]])
    # old code to build the indiv trees
    if(plotTrees){
      pdf(paste(fileList[[b]],'-plotTrees.pdf',sep=''), width=12,height=25)
      tempTree = MyTrees[[1]]
      tempTree$node.label = seq(length(tempTree$node.label))
      plot(tempTree, show.node.label = TRUE, cex=0.7) ###this will visualize the node numbers
      dev.off()
      invisible(readline(prompt=paste("Plot for", fileList[[b]], ". Press [enter] to continue")))
      
    }
    
    n=0
    nodeDates = matrix(ncol = length(MyTrees[[1]]$node.label), nrow=1000000) ###arbitrary number of rows, can change it to the exact number of trees in datedist file if you want
    for (i in MyTrees){
      dates <- as.numeric(i$node.label)
      n <- n + 1
      nodeDates[n,] <- dates
    }
    
    saveNodes[[b]] <- nodeDates[complete.cases(nodeDates),]
    print(paste('Tree', b, 'complete.'))
    
  }
  
  if (plot){
    print(paste('Building plots for', length(node), 'nodes...'))
    if (is.null(plotLayout)) stop('If plot=TRUE then you need to specify plotLayout.')
    pdf('try.pdf', height=12, width=8)
    par(mfrow = plotLayout)
    print(paste('colors are specified in the following order as:'))
    print(paste(sapply(pal[1:b], plotrix::color.id)))
    print(paste('and match the following input files'))
    print(paste(fileList))
    
    for (tt in 1:length(node)){
      for (b in 1:length(saveNodes)){
        # starting density plots before redo density call with common bandwidth
        startDens <- lapply(saveNodes, FUN=function(x) density(x[,node[tt]]))
        
        if (b == 1){
          # get metrics to specify plot
          old.bw <- unlist(lapply(startDens, FUN=function(x) x$bw))
          new.bw <- diff(range(old.bw)) / 2 + min(old.bw)
          newDens <- lapply(saveNodes, FUN=function(x) density(x[,node[tt]], bw=new.bw))
          xlims <- rev(range(unlist(lapply(newDens, FUN=function(x) range(x$x)))))
          ylims <- range(unlist(lapply(newDens, FUN=function(x) range(x$y))))
          
          # build empty plot
          plot(0, 0, xlim = xlims, ylim = ylims, type='n', xlab='Years (Ma)', ylab='Density')
          title(paste('Node', node[tt]))
          
        }
        
        # then the lines
        #lines(newDens[[b]], col=gg_color_hue(length(saveNodes))[b], lwd=1.5, lty=ceiling(b / 5))
        lines(newDens[[b]], col=pal[b], lwd=1.5, lty=ceiling(b / 5))
        
      } # end iteration through each file on individual plot
      
    } # end node iteration
    dev.off()
  } # end if (plot)
  
  if(returnData) return(saveNodes)
  
}

#=======================================
## below is the very rough code used to plot multiple nodes from a single file
#=======================================

# the directory where your file(s) live
setwd('~/Documents/WHOI/RCode/GruenDS/datedist/')
#setwd("/Users/dgruen/Desktop/clusterfucklocal/datedists_2018_05_02")

# get list of all files in your current directory
fileList <- list.files()
# make a vector of all files in your working directory whose filename contains 'datedist'
fileList <- fileList[grep('datedist', fileList)]

saveNodes <- plotNode(inputFiles=fileList, node=c(104,105,106,108), plotLayout=c(2,2), returnData = TRUE, plotTrees = F)

#Plot1:
#  File: outfile_pb_molclock_18_05_01_ugam_sample.datedist
#Nodes: 151, 176, 178, 208

#Plot2:
#  File: outfile_pb_molclock_18_05_02_ln_sample.datedist
#Nodes: 151, 176, 178, 208

pdf('plot_nodes_2May2018.pdf', height=12, width=8, useDingbats = F)
node <- c(151,176,178,208)
par(mfrow=c(2,1))
for (b in c(1,3)){ # for files 1 and 3 in fileList
  # starting density plots before redo density call with common bandwidth to get appropriate axis limits
  #startDens <- lapply(saveNodes, FUN=function(x) density(x[,node[tt]]))
  #xlims <- rev(range(unlist(lapply(newDens, FUN=function(x) range(x$x)))))
  #if (b == 1){
  
  ## keep axis limits fixed for now
  xlims <- c(750,100)
  ylims <- c(0,.015)
  #}
  
  # if (b == 3){
  #   xlims <- c(750,350)
  #   ylims <- c(0,.015)
  # }
  
  # build an empty plot
  plot(0, 0, xlim = xlims, ylim = ylims, type='n', xlab='Years (Ma)', ylab='Density')
  title(paste(fileList[[b]]))
  legend('topleft', as.character(node), col=gg_color_hue(length(node)), lty=1)
  
  # calculate then add a line for each node
  for (tt in 1:length(node)){
    # get metrics to specify plot
    old.bw <- unlist(lapply(startDens, FUN=function(x) x$bw))
    new.bw <- diff(range(old.bw)) / 2 + min(old.bw)
    newDens <- lapply(saveNodes, FUN=function(x) density(x[,node[tt]], bw=new.bw))
    
    # then the lines
    lines(newDens[[b]], col=gg_color_hue(length(node))[tt], lwd=1.5, lty=ceiling(b / 5))
  }
  
  # go on to next file according to b
}
dev.off()


