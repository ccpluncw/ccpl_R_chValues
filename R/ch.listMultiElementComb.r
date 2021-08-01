#' This function combines a list into a single vector
#'
#' This function combines a list into a single vector by applying a function across the elements. Essentially, this function will identify the minimum length of all the elements (minN), then sample each element with a size of minN. It will treat each sample as a column in a dataframe and apply rowMeans or rowSums.
#' @param df.data A list with each element filled with numbers.
#' @param combType A string that specifies the combination statistic.  Currently, two are implemented: "mean" and "sum"
#' @param replace A boolean that specifies whether the sample is take with replacement or not. DEFAULT = TRUE
#' @keywords combine function vectors
#' @return a vector of values that combines all the columns in df.data with probMax samples devoted to the column with the max(median())
#' @export
#' @examples ch.listMultiElementComb (df.data, combType = "mean")

ch.listMultiElementComb <- function(list.data, combType = "mean", replace = TRUE) {

  if(length(list.data) > 1) {
    #find the minimum length
    minN <-min(lengths(list.data))
    #get the number of elements in the max column and ave columns
    df.tmp <- data.frame(lapply(list.data, sample, size=minN, replace=replace, prob=NULL))
    if(combType == "mean") {
      outVec <- rowMeans(df.tmp)
    }
    if(combType == "sum") {
      outVec <- rowSums(df.tmp)
    }

  } else {
    #if there is only one column, then output it
    outVec <- as.vector(unlist(list.data))
  }

  return(outVec)
}
