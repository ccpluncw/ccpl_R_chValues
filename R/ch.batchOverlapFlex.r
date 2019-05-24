#' For each in a set of combinations, run ch.distOverlapFlex Function
#'
#' This function runs ch.distOverlap for all the combinations of distributions identified in df.combns.
#' @param values a vector of the values in the distributions.
#' @param items a vector that identifies the item that each value is linked to.
#' @param itemSet a vector of items that you want to run the bootstraps for.
#' @param df.combns a vector of combinations that identifies the items (by index number of itemSet) whose values should be combined and compared against every other combination in this vector.
#' @param numRuns the number of runs to do in the bootstrap.
#' @param combFun If there are multiple items contributing to a single distribution, this function describes how the values will be combined across items. The function must combine the rows of a matrix and return the result as a vector (e.g., rowMeans). The default just flattens the matrix into one large vector. DEFAULT = as.vector
#' @param outFile the name of a text file that will be output as the bootstrap runs.  This is filled incrementally just in case the program dies. DEFAULT = NULL - will not write a file
#' @keywords distribution overlap bootstrap values
#' @return a dataframe with the overlap statistics for each set of distributions compared. See ch.distOverlap
#' @export
#' @examples ch.batchOverlap (data$responses, data$items,itemSet,df.combns, "out.txt" )

ch.batchOverlapFlex <- function(values, items, itemSet, df.combns, numRuns, combFun = as.vector, outFile = NULL) {
  data <- data.frame(values, items)

  numCombinations <- nrow(df.combns)
  maxNumPerSide <- ncol(df.combns)
  totalNumOverlaps <- (numCombinations^2- numCombinations)/2

  averageP <- vector(mode="numeric", length = totalNumOverlaps)
  sdP <- vector(mode="numeric", length = totalNumOverlaps)
  overlap <- vector(mode="numeric", length = totalNumOverlaps)
  direction <- vector(mode="numeric", length = totalNumOverlaps)

  itemAarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))
  itemBarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))

  i <- 1
  for(l in 1:(numCombinations - 1)){
    print(paste(l, "of", (numCombinations - 1), sep=" "))

    for(k in l:(numCombinations - 1)){
      xValue <- list()
      yValue <- list()

      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- toString(itemSet[df.combns[l,m]])
        itemBarray[i,m] <- toString(itemSet[df.combns[k+1,m]])
        if (!is.na(itemAarray[i,m])) {
          xTmp <- data$values[data$items==itemAarray[i,m]]
          varName <- paste("v", m, sep="")
          xValue[[varName]] <- xTmp
        }
        if (!is.na(itemBarray[i,m])) {
          yTmp <- data$values[data$items==itemBarray[i,m]]
          varName <- paste("v", m, sep="")
          yValue[[varName]] <- yTmp
        }
      }

      #run bootstrap to get overlap information
      pOut <- ch.distOverlapFlex(xValue,yValue,numRuns,combFun = combFun)

      averageP[i] <-pOut["percent"]
      sdP[i] <-pOut["sd"]
      overlap[i] <-pOut["overlap"]
      direction[i] <-pOut["direction"]
      i=i+1
    }
      #put data in dataframe
      alldat <-data.frame(itemAarray[1:(i-1),], itemBarray[1:(i-1),], averageP = averageP[1:(i-1)],sdp = sdP[1:(i-1)], overlap = overlap[1:(i-1)],direction = direction[1:(i-1)] )
      #rename columns
      for(nps in 1:maxNumPerSide) {
        colnames(alldat)[nps] <- paste("IA", nps, sep="")
        colnames(alldat)[nps+maxNumPerSide] <- paste("IB", nps, sep="")
      }
      if(!is.null(outFile)) {
        write.table(alldat, file=outFile, quote=F, sep="\t", row.names = F, append=F)
      }
  }

  return(alldat)
}
