#' Bootstrap the overlap of two distributions Function with a flexible function
#'
#' This function does a bootstrap to calculate the percentage of samples that Distribution X is greater than Distribution Y. When Distribution X (or Y) are the combination of multiple items, then it uses combFun (a function) to combine the values of those items into a single distribution.
#' @param xDist Distribution X.
#' @param yDist Distribution Y.
#' @param numRuns the number of runs in the bootstrap. DEFAULT = 100.
#' @param combFun If there are multiple items contributing to a single distribution, this function describes how the values will be combined across items. The function must combine the rows of a matrix and return the result as a vector (e.g., rowMeans). The default just flattens the matrix into one large vector. DEFAULT = as.vector
#' @keywords distribution overlap bootstrap values
#' @return the average percentage of samples that xDist was greater than yDist, the sd of the average percentage, Overlap = a transformation of average percentage so 1=complete overlap and 0 = no overlap, direction = 1 if xDist > yDist; else -1
#' @export
#' @examples ch.distOverlap (xValue, yValue,100)

ch.distOverlapFlex <- function(xDist, yDist, numRuns, combFun = as.vector) {
  percentage <- vector(mode="numeric", length = numRuns)

  namesX <- names(xDist)
  aveLengthX <- mean(lengths(xDist))
  namesY <- names(yDist)
  aveLengthY <- mean(lengths(yDist))

  for(j in 1:numRuns){
    samplesize <- (aveLengthX+aveLengthY)/2

    rx <- matrix(, nrow=samplesize, ncol=0)
    for(i in 1:length(namesX)) {
      rx <-cbind(rx, sample(xDist[[namesX[i]]], size=samplesize, replace=TRUE, prob=NULL))
    }
    ry <- matrix(, nrow=samplesize, ncol=0)
    for(i in 1:length(namesY)) {
      ry <-cbind(ry, sample(yDist[[namesY[i]]], size=samplesize, replace=TRUE, prob=NULL))
    }
    xIn <- combFun (rx)
    yIn <- combFun (ry)

    winner <-ifelse(xIn>yIn, 1, ifelse(xIn<yIn, 0, .5))
    percentage[j] <-sum(winner)/length(winner)
  }
  averageP <-mean(percentage)
  sdP <-sd(percentage)
  overlap <-1-abs(((averageP-0.5)/0.5))
  direction <-ifelse(averageP>=0.5,1,-1)

  return(c(percent = averageP, sd = sdP, overlap = overlap, direction = direction))
}
