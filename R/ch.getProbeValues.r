#' A function to extract a the raw value data for a set of probes.
#'
#' @param data a datafrane containing the raw value data from an experiment or set of experiments.
#' @param probeItems A vector of probe items that you want the value data to be extracted from "data"
#' @param promptColumn A string specifying the name of the column in "data" that contains the prompt names. DEFAULT = "prompt"
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @keywords prompt value file extract
#' @return a dataframe containing the raw values and associated information for the prompts in promptItems
#' @export
#' @examples ch.getProbeValues (df.myInputData, c("a nun", "your mother"), outputFile = "out.txt")

ch.getProbeValues <- function (data, probeItems, promptColumn = "prompt", outputFile = NULL) {

  probeItems <- as.character(probeItems)
  probeItems <- tolower(trimws(probeItems))

  data[[promptColumn]] <- as.character(data[[promptColumn]])
  data[[promptColumn]] <- tolower(trimws(data[[promptColumn]]))

  itemSet <- unique(na.omit(as.character(probeItems)))

  itemsNotInData <- unique(itemSet[! itemSet %in% unique(data[[promptColumn]])])

  if (length(itemsNotInData) > 0) {
    print("these items are in the itemSet, but not in the dataSet")
    print(itemsNotInData)
    print("fatalError: All items in itemSet must be in dataSet")
    stop()
  }


  df.out <- data[data[[promptColumn]] %in% itemSet, ]

  if(!is.null(outputFile)) {
    write.table(df.out, outputFile, row.names=F, col.names=T, quote=F, sep="\t")
  }
  return(df.out)
}
