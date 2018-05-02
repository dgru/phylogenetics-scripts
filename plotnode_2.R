
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
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
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
    
    par(mfrow = plotLayout)
    print(paste('colors are specified in the following order as:'))
    print(paste(sapply(gg_color_hue(b), plotrix::color.id)))
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
        lines(newDens[[b]], col=gg_color_hue(b)[b], lwd=1.5, lty=ceiling(b / 5))
        
      } # end iteration through each file on individual plot
      
    } # end node iteration
    
  } # end if (plot)
  
  if(returnData) return(saveNodes)
  
}

# the directory where your file(s) live
#setwd('~/Documents/WHOI/RCode/GruenDS/datedist/')
setwd("/Users/dgruen/Desktop/clusterfucklocal/datedists_2018_05_02")

# get list of all files in your current directory
fileList <- list.files()
# make a vector of all files in your working directory whose filename contains 'datedist'
fileList <- fileList[grep('datedist', fileList)]

saveNodes <- plotNode(inputFiles=fileList, node=c(104,105,106,108,110), plotLayout=c(2,1), returnData = TRUE, plotTrees = F)

