#' For each in a set of combinations, run ch.distOverlapFlex Function
#'
#' This function runs ch.distOverlap for all the combinations of distributions identified in df.combns.
#' @param values a vector of the values in the distributions.
#' @param items a vector that identifies the item that each value is linked to.
#' @param itemSet a vector of items that you want to run the bootstraps for.
#' @param df.combns a vector of combinations that identifies the items (by index number of itemSet) whose values should be combined and compared against every other combination in this vector.
#' @param numRuns the number of runs to do in the bootstrap. DEFAULT = 1000.
#' @param outFile the name of a text file that will be output as the bootstrap runs.  This is filled incrementally just in case the program dies. DEFAULT = NULL - will not write a file
#' @param combFun If there are multiple items contributing to a single distribution, this function describes how the values will be combined across items in both the X and Y distributions. The function must combine elements of a list that might be of different different lengths. The default just flattens the list into one large vector.  DEFAULT = ch.maxAveComb  (with probMax = 0.5)
#' @keywords distribution overlap bootstrap values
#' @return a dataframe with the overlap statistics for each set of distributions compared. See ch.distOverlap
#' @export
#' @examples ch.batchOverlapFlex (data$responses, data$items,itemSet,df.combns,numRuns=1000, outFile ="out.txt" )

ch.batchOverlapFlex <- function(values, items, itemSet, df.combns, numRuns=1000, outFile = NULL, combFun = ch.maxAveComb, ...) {

  data <- data.frame(values, items)

  numCombinations <- nrow(df.combns)
  maxNumPerSide <- ncol(df.combns)


  for(l in 1:(numCombinations - 1)){
    print(paste(l, "of", (numCombinations - 1), sep=" "))

    totalNumOverlaps <- numCombinations-l

    averageP <- vector(mode="numeric", length = totalNumOverlaps)
    sdP <- vector(mode="numeric", length = totalNumOverlaps)
    overlap <- vector(mode="numeric", length = totalNumOverlaps)
    direction <- vector(mode="numeric", length = totalNumOverlaps)

    itemAarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))
    itemBarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))

    i <- 1
    for(k in l:(numCombinations - 1)){
      xValue <- list()
      yValue <- list()
      #get the raw data for each item
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- toString(itemSet[df.combns[l,m]])
        itemBarray[i,m] <- toString(itemSet[df.combns[k+1,m]])
        if (itemAarray[i,m] != "NA") {
          xValue[[paste("v", m, sep="")]] <- data$values[data$items==itemAarray[i,m]]
        }
        if (itemBarray[i,m] != "NA") {
          yValue[[paste("v", m, sep="")]] <- data$values[data$items==itemBarray[i,m]]
        }
      }
      #run bootstrap to get overlap information
      pOut <- ch.distOverlapFlex(xValue,yValue,numRuns,combFun = combFun, ...)

      averageP[i] <-pOut["percent"]
      sdP[i] <-pOut["sd"]
      overlap[i] <-pOut["overlap"]
      direction[i] <-pOut["direction"]
      i=i+1
    }
      #put data in dataframe
      alldat <-data.frame(itemAarray, itemBarray, averageP = averageP,sdp = sdP, overlap = overlap,direction = direction)
      #rename columns
      for(nps in 1:maxNumPerSide) {
        colnames(alldat)[nps] <- paste("IA", nps, sep="")
        colnames(alldat)[nps+maxNumPerSide] <- paste("IB", nps, sep="")
      }
      append1 <- ifelse(l == 1, FALSE, TRUE)
      cName1 <- ifelse(l == 1, TRUE, FALSE)

      if(!is.null(outFile)) {
        write.table(alldat, file=outFile, quote=F, sep="\t", row.names = F, col.names = cName1, append=append1)
      }
  }

  outData <- read.table(file=outFile, header=T, sep="\t", quote="\"")

  return(outData)
}
