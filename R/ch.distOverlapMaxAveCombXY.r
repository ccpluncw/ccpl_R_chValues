#' Bootstrap the overlap of two distributions Function with a flexible function
#'
#' This function does a bootstrap to calculate the percentage of samples that Distribution X is greater than Distribution Y. When Distribution X (or Y) are the combination of multiple items, then it uses combFun (a function) to combine the values of those items into a single distribution.
#' @param xDist a list containing the values for Distribution X.
#' @param yDist a list containing the values for Distribution Y.
#' @param numRuns the number of runs in the bootstrap. DEFAULT = 100.
#' @param probMaxX A probability that specifies the probability of samples that should be devoted to the xDist list column with the max(median()). When probMax = 0, the column with the max(median()) has no influence on either the average or above and beyond the average.  When probMax = 0.5, the column with the max(median()) is 50 percent of the output vector.  This is simply collapsing into a vector (as.vector) when there are only 2 vectors in the list. But it will have an outsized influence if there are more than two vector in the list.  When probMax = 1, the column with the max(median()) is 100 percent of the output vector.  DEFAULT = 0.5 (equal influence for two vectors)(probMax = .8 will produce a distribution in which .4 of the total are all the items (including the max) and .6 of the total are MAX items - when there are only two vectors in the list). Use ch.getProbMax() to calculate the appropriate probMax.
#' @param probMaxY The same as probMaxX, but for the yDist list.
#' @keywords distribution overlap bootstrap values
#' @return the average percentage of samples that xDist was greater than yDist, the sd of the average percentage, Overlap = a transformation of average percentage so 1=complete overlap and 0 = no overlap, direction = 1 if xDist > yDist; else -1
#' @export
#' @examples ch.distOverlapMaxAveCombXY (xValue, yValue,100, combFun = ch.maxAveComb, probMax = 0.667)

ch.distOverlapMaxAveCombXY <- function(xDist, yDist, numRuns, probMaxX = 0.5, probMaxY = 0.5) {

  percentage <- vector(mode="numeric", length = numRuns)

  aveLengthX <- mean(lengths(xDist))
  aveLengthY <- mean(lengths(yDist))
  samplesize <- (aveLengthX+aveLengthY)/2

  for(j in 1:numRuns){

    xIn <- sample(ch.maxAveComb(xDist, probMax = probMaxX), size=samplesize, replace=TRUE, prob=NULL)
    yIn <- sample(ch.maxAveComb(yDist, probMax = probMaxY), size=samplesize, replace=TRUE, prob=NULL)

    winner <-ifelse(xIn>yIn, 1, ifelse(xIn<yIn, 0, .5))
    percentage[j] <-sum(winner)/length(winner)
  }
  averageP <-mean(percentage)
  sdP <-sd(percentage)
  overlap <-1-abs(((averageP-0.5)/0.5))
  direction <-ifelse(averageP>=0.5,1,-1)

  return(c(percent = averageP, sd = sdP, overlap = overlap, direction = direction))
}
