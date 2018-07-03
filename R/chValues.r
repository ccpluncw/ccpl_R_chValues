#This is a package for the Values experiments.

#' New combinations Function
#'
#' This function creates a vector of all the combinations of itemList taken minNumPerSide at a time to maxNumPerSide at a time.
#' @param numItems the total number of items used for the combinations.
#' @param minNumPerSide the minimum number of items in a combination. DEFAULT = 1.
#' @param maxNumPerSide the maxmimun number of items in a combination. DEFAULT = 1.
#' @keywords combinations values
#' @return a vector of all the combinations (by item number)
#' @export
#' @examples ch.combnVector (5, 1,3)

ch.combnVector <- function(numItems, minNumPerSide = 1, maxNumPerSide = 1) {

    numSideSeq <- seq(minNumPerSide,maxNumPerSide,1)

    df.combns <- NULL
    for(i in numSideSeq) {
        #get combination
        tmpC <- t(combn(numItems, i))
        #add self matching conditions for item combinations greater than 1
        if(i > 1) {
          selfMatch <- array(rep(1:numItems,i), dim=c(numItems,i))
          tmpC <- rbind(tmpC, selfMatch)
        }
        #fill extra columns with NA and make into a dataframe
        columnsNeeded <- maxNumPerSide - ncol(tmpC)
        if(columnsNeeded > 0) {
          for (j in 1:columnsNeeded) {
            extraNA <- array(NA,dim=c(nrow(tmpC),columnsNeeded))
          }
          tmpC <- data.frame (tmpC,extraNA)
        } else {
          tmpC <- data.frame (tmpC)
        }
        #rename columns to generic
        for(j in 1:ncol(tmpC)) {
          colnames(tmpC)[j] <- paste("I", j, sep="")
        }
        #combine into common dataframe
        if (is.null(df.combns)) {
          df.combns <- tmpC
        } else {
          df.combns <- rbind(df.combns, tmpC)
        }
    }
    return(df.combns)
}

#' Bootstrap the overlap of two distributions Function
#'
#' This function does a bootstrap to calculate the percentage of samples that Distribution X is greater than Distribution Y.
#' @param xDist Distribution X.
#' @param yDist Distribution Y.
#' @param numRuns the number of runs in the bootstrap. DEFAULT = 100.
#' @keywords distribution overlap bootstrap values
#' @return the average percentage of samples that xDist was greater than yDist, the sd of the average percentage, Overlap = a transformation of average percentage so 1=complete overlap and 0 = no overlap, direction = 1 if xDist > yDist; else -1
#' @export
#' @examples ch.distOverlap (xValue, yValue,100)

ch.distOverlap <- function(xDist, yDist, numRuns) {
  percentage <- vector(mode="numeric", length = numRuns)

  for(j in 1:numRuns){
    samplesize <- (length(xDist)+length(yDist))/2

    rx <-sample(xDist, size=samplesize, replace=TRUE, prob=NULL)
    ry <-sample(yDist, size=samplesize, replace=TRUE, prob=NULL)

    winner <-ifelse(rx>ry, 1, 0)
    percentage[j] <-sum(winner)/length(winner)
  }
  averageP <-mean(percentage)
  sdP <-sd(percentage)
  overlap <-1-abs(((averageP-0.5)/0.5))
  direction <-ifelse(averageP>=0.5,1,-1)

  return(c(percent = averageP, sd = sdP, overlap = overlap, direction = direction))
}

#' For each in a set of combinations, run ch.distOverlap Function
#'
#' This function runs ch.distOverlap for all the combinations of distributions identified in df.combns.
#' @param values a vector of the values in the distributions.
#' @param items a vector that identifies the item that each value is linked to.
#' @param itemSet a vector of items that you want to run the bootstraps for.
#' @param df.combns a vector of combinations that identifies the items (by index number of itemSet) whose values should be combined and compared against every other combination in this vector.
#' @param numRuns the number of runs to do in the bootstrap.
#' @param outFile the name of a text file that will be output as the bootstrap runs.  This is filled incrementally just in case the program dies. DEFAULT = NULL - will not write a file
#' @keywords distribution overlap bootstrap values
#' @return a dataframe with the overlap statistics for each set of distributions compared. See ch.distOverlap
#' @export
#' @examples ch.batchOverlap (data$responses, data$items,itemSet,df.combns, "out.txt" )

ch.batchOverlap <- function(values, items, itemSet, df.combns, numRuns, outFile = NULL) {
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
      xValue <- NULL
      yValue <- NULL

      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- toString(itemSet[df.combns[l,m]])
        itemBarray[i,m] <- toString(itemSet[df.combns[k+1,m]])
        if (!is.na(itemAarray[i,m])) {
          xTmp <- data$values[data$items==itemAarray[i,m]]
          if(is.null(xValue)) {
            xValue <- xTmp
          } else {
            xValue <- append(xValue,xTmp)
          }
        }
        if (!is.na(itemBarray[i,m])) {
          yTmp <- data$values[data$items==itemBarray[i,m]]
          if(is.null(yValue)) {
            yValue <- yTmp
          } else {
            yValue <- append(yValue,yTmp)
          }
        }
      }

      #run bootstrap to get overlap information
      pOut <- ch.distOverlap(xValue,yValue,numRuns)
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

#' Extends ch.batchOverlap to two separate X and Y datasets Function
#'
#' This function runs ch.distOverlap for all the combinations of distributions identified in df.combns.
#' @param xValues a vector of the values in the X Distributions.
#' @param xItems a vector that identifies the item that each xValue is linked to.
#' @param yValues a vector of the values in the Y Distributions.
#' @param yItems a vector that identifies the item that each yValue is linked to.
#' @param itemSet a vector of items that you want to run the bootstraps for.
#' @param df.combns a vector of combinations that identifies the items (by index number of itemSet) whose values should be combined and compared against every other combination in this vector.
#' @param outFile the name of a text file that will be output as the bootstrap runs.  This is filled incrementally just in case the program dies. DEFAULT = NULL - will not write a file
#' @keywords distribution overlap bootstrap values
#' @return a dataframe with the overlap statistics for each set of distributions compared. See ch.distOverlap
#' @export
#' @examples ch.batchOverlapXY (data1$responses, data1$items,data2$responses, data2$items,itemSet,df.combns, "out.txt" )

ch.batchOverlapXY <- function(xValues, xItems, yValues, yItems, itemSet, df.combns, outFile = NULL) {
  xData <- data.frame(values = xValues, items = xItems)
  yData <- data.frame(values = yValues, items = yItems)

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
      xValue <- NULL
      yValue <- NULL

      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- toString(itemSet[df.combns[l,m]])
        itemBarray[i,m] <- toString(itemSet[df.combns[k+1,m]])
        if (!is.na(itemAarray[i,m])) {
          xTmp <- xData$values[xData$items==itemAarray[i,m]]
          if(is.null(xValue)) {
            xValue <- xTmp
          } else {
            xValue <- append(xValue,xTmp)
          }
        }
        if (!is.na(itemBarray[i,m])) {
          yTmp <- yData$values[yData$items==itemBarray[i,m]]
          if(is.null(yValue)) {
            yValue <- yTmp
          } else {
            yValue <- append(yValue,yTmp)
          }
        }
      }

      #run bootstrap to get overlap information
      pOut <- ch.distOverlap(xValue,yValue,numRuns)
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
