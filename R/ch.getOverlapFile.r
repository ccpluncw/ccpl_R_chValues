#' A function to extract a subset of overlaps from a file
#'
#' @param overlapFile a tab delimited file of a full set of overlaps with all the items.
#' @param itemFile a vector of items that you want to extract the overlaps for.
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @param itemColumn1 the name of the column that contains one set of item values in the overlap. DEFAULT = "item1vector"
#' @param itemColumn2 the name of the column that contains the second set of item values in the overlap. DEFAULT = "item2vector"
#' @keywords values overlap file extract
#' @return a dataframe with the overlaps statistics for the items in itemFile
#' @export
#' @examples ch.getOverlapFile (Overlaps.txt, items.txt,output.txt)

ch.getOverlapFile <- function(overlapFile = NULL, itemFile = NULL, outputFile = NULL, itemColumn1 = "item1vector",itemColumn2 = "item2vector") {

  if (is.null(overlapFile)) {
    stop("overlapFile must be specified")
  }
  if (is.null(itemFile)) {
    stop("itemFile must be specified")
  }

  data1 <-read.table(overlapFile, header=T, sep="\t", quote = "\"")
  items <- read.table(itemFile, header=F, sep="\t", quote = "\"")

  #make items as similar as possible by changing case to lower and triming whitespace
  items$V1 <- tolower(trimws(items$V1))
  data1[[itemColumn1]] <- tolower(trimws(data1[[itemColumn1]]))
  data1[[itemColumn2]] <- tolower(trimws(data1[[itemColumn2]]))

  data2 <- data1[(data1[[itemColumn1]] %in% items$V1) & (data1[[itemColumn2]] %in% items$V1),]

  if(!is.null(outputFile)) {
    write.table(data2, file=outputFile, quote=F, sep="\t", row.names=F)
  }
  return(data2)
}
