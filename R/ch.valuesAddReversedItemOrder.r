#' A function to add the reversed order of Item1 and Item2 in the values/morals datasets
#'
#' This function adds the reversed order of Item1 and Item2 in the values/morals datasets.
#' @param data morals dataframe.
#' @param item1Col a string that specifies the name of the column in "data" that contains one of the items in the morals task.
#' @param item2Col a string that specifies the name of the column in "data" that contains the comparison item in the morals task.
#' @param dirOverlapCol a string that specifies the name of the column in "data" that contains the directional Overlap for the item in each trial.
#' @keywords morals values add reverse order item
#' @return the dataset with the added reversed order of Item1 and Item2.  Because this dataset doubles the actual data (all A-B item combinations are duplicated to be B-A combinations as well), do not use it for analysis unless you keep this in mind. The duplication is sometimes needed because the overlap dataset only outputs one of these orders. The data will add a "probeOrder" column that states whether the order of Item1 and Item2 are "original" or "reversed."
#' @export
#' @examples ch.valuesAddReversedItemOrder (data=myData, "Item1", "Item2", "dirOverlap")

ch.valuesAddReversedItemOrder <- function (data, item1Col, item2Col, dirOverlapCol) {

  data$probeOrder <- "original"

  #find all the item_i in probe 2 and put it in probe 1
  #this way we are finding all instances of item_i

  tmp2 <- data
  tmp2[[dirOverlapCol]] <- -1*tmp2[[dirOverlapCol]]
  tmp2$item2tmp <- tmp2[[item2Col]]
  tmp2[[item2Col]] <- tmp2[[item1Col]]
  tmp2[[item1Col]] <- tmp2$item2tmp
  tmp2$item2tmp <- NULL
  tmp2$probeOrder <- "reversed"

  data <-rbind(data,tmp2)

  return(data)

}
