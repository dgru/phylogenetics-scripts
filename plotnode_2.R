
# the directory where your file(s) live
setwd('~/Documents/WHOI/RCode/GruenDS/')
#setwd("/Users/dgruen/Desktop/clusterfucklocal")

# get list of all files in your current directory
fileList <- list.files()
# make a vector of all files in your working directory whose filename contains 'datedist'
fileList <- fileList[grep('datedist', fileList)]

saveNodes <- plotNode(inputFiles=fileList, node=c(104,105), plotLayout=c(2,1), returnData = TRUE)







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

plotNode <- function(inputFiles, node, plot=TRUE, plotLayout=NULL, returnData=FALSE){

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
    #tempTree = MyTrees[[1]]
    #tempTree$node.label = seq(length(tempTree$node.label))
    #plot(tempTree, show.node.label = TRUE, cex=0.7) ###this will visualize the node numbers

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



## END. old stuff below
#=============================

  n=0
  nodeDates = matrix(ncol = length(MyTrees[[1]]$node.label), nrow=1000000) ###arbitrary number of rows, can change it to the exact number of trees in datedist file if you want
  for (i in MyTrees){
    dates = as.numeric(i$node.label)
    n=n+1
    nodeDates[n,]=dates
  }

  nodeDates = nodeDates[complete.cases(nodeDates),]
  print(str(nodeDates))
  print(node)
  #dim(nodeDates)
  if (plot) plot(density(nodeDates[,node])) ###this is the plot, save output

}


library(ape)
#library(HDInterval)
setwd('~/Documents/WHOI/RCode/GruenDS/')
#setwd("/Users/dgruen/Desktop/clusterfucklocal")
input = "outfile_pb_molclock_18_0109_dsg_sample.datedist" ###datedist file here
#nodeOfInterest = 104 ###but do this for nodes 104 and 105
fileList <- list.files()
fileList <- fileList[grep('datedist', fileList)]
par(mfrow=c(2,3))
plotNode(input=fileList[1], node=104)
plotNode(input=fileList[2], node=104)
plotNode(input=fileList[3], node=104)
plotNode(input=fileList[1], node=105)
plotNode(input=fileList[2], node=105)
plotNode(input=fileList[3], node=105)

# single node
# up to 10 files / node

# how to deal w bandwidth
# x/y limits

