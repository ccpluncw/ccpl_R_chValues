#' This function combines a list into a single vector
#'
#' This function combines a list into a single vector controling the percent of observations devoted to the list element with the max median.
#' @param df.data A list with each element filled with numbers.
#' @param probMax A probability that specifies the probability of samples that should be devoted to the column with the max(median()). DEFAULT = 0.5 (so it is simply a collapsing into a vector (as.vector)); (probMax = .8 will produce a distribution in which .4 of the total are all the items (including the max) and .6 of the total are MAX items.)
#' @keywords combine function vectors
#' @return a vector of values that combines all the columns in df.data with probMax samples devoted to the column with the max(median())
#' @export
#' @examples ch.maxAveComb (df.data, probMax = .7)

ch.maxAveComb <- function(list.data, probMax = 0.5) {

  if(length(list.data) > 1) {
    #find Column with the maximum median
    maxCol <-as.numeric(which.max(lapply(list.data,median)))
    #get the number of elements in the max column and ave columns
    maxN <- lengths(list.data[maxCol])
    aveN <- sum(lengths(list.data[-maxCol]))
    #estimate the maximum number of elements given the number of elements in the ave columns
    estMaxN <- (probMax/(1-probMax)) * aveN
    #put the entire max column in the outvector if it is less than or equal to the estimated Max N based on the number of elements in aveN.  Now sample from the AveN data.
    if(maxN <= estMaxN) {
      outVec <- as.vector(unlist(list.data[maxCol]))
      #remove the max column
      list.data <- list.data[-maxCol]
      #calculate the number of total observations from the "average" columns (not the Max column) to be output
      numAveObs <- round(((maxN/probMax) - maxN))
      #now sample the numObsPerAveRow from the remaining data
      tmpVec <- sample (as.vector(unlist(list.data)), size=numAveObs, replace=TRUE, prob=NULL)
    } else {
      #else put all the ave data in the outvec
      outVec <- as.vector(unlist(list.data[-maxCol]))
      #remove the ave columns
      list.data <- list.data[maxCol]
      #now sample the estMaxN from the remaining data
      tmpVec <- sample (as.vector(unlist(list.data)), size=estMaxN, replace=TRUE, prob=NULL)
    }
    #add those data to the outVec
    outVec <- c(outVec, tmpVec)

  } else {
    #if there is only one column, then output it
    outVec <- as.vector(unlist(list.data))
  }

  return(outVec)
}
