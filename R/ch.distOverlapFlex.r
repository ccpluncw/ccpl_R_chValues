#' Bootstrap the overlap of two distributions Function with a flexible function
#'
#' This function does a bootstrap to calculate the percentage of samples that Distribution X is greater than Distribution Y. When Distribution X (or Y) are the combination of multiple items, then it uses combFun (a function) to combine the values of those items into a single distribution.
#' @param xDist a list containing the values for Distribution X.
#' @param yDist a list containing the values for Distribution Y.
#' @param numRuns the number of runs in the bootstrap. DEFAULT = 100.
#' @param combFun If there are multiple items contributing to a single distribution, this function describes how the values will be combined across items in both the X and Y distributions. The function must combine elements of a list that might be of different different lengths. The default just flattens the list into one large vector.  DEFAULT = ch.maxAveComb  (with probMax = 0.5)
#' @keywords distribution overlap bootstrap values
#' @return the average percentage of samples that xDist was greater than yDist, the sd of the average percentage, Overlap = a transformation of average percentage so 1=complete overlap and 0 = no overlap, direction = 1 if xDist > yDist; else -1
#' @export
#' @examples ch.distOverlapFlex (xValue, yValue,100, combFun = ch.maxAveComb, probMax = 0.667)

ch.distOverlapFlex <- function(xDist, yDist, numRuns, combFun = ch.maxAveComb, ...) {

  percentage <- vector(mode="numeric", length = numRuns)

  aveLengthX <- mean(lengths(xDist))
  aveLengthY <- mean(lengths(yDist))
  samplesize <- (aveLengthX+aveLengthY)/2

  for(j in 1:numRuns){

    xIn <- sample(combFun(xDist, ...), size=samplesize, replace=TRUE, prob=NULL)
    yIn <- sample(combFun(yDist, ...), size=samplesize, replace=TRUE, prob=NULL)

    winner <-ifelse(xIn>yIn, 1, ifelse(xIn<yIn, 0, .5))
    percentage[j] <-sum(winner)/length(winner)
  }
  averageP <-mean(percentage)
  sdP <-sd(percentage)
  overlap <-1-abs(((averageP-0.5)/0.5))
  direction <-ifelse(averageP>=0.5,1,-1)

  return(c(percent = averageP, sd = sdP, overlap = overlap, direction = direction))
}
