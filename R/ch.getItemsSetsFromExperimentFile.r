#' A function to extract a the items present in an experiment file
#'
#' @param choiceDataframe A dataframe with data from the experiment.
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @param itemColumn the name of the column that contains the item values. DEFAULT = c("IA1", "IB1")
#' @param practiceColumn the name of the column that contains the values indicating whether or not the trials are practice. DEFAULT = NULL (if NULL, no practice filtering will occur)
#' @param practiceValue the value in practiceColumn that specifies the trial is a practice trial. DEFAULT = "practice"
#' @keywords items morals file extract
#' @return a vector with the unique items used in the experiment
#' @export
#' @examples ch.getItemSetsFromExperimentFile (experiment.txt,output.txt, probes, practice, 0)

ch.getItemSetsFromChoiceDataframe <- function (choiceDataframe = NULL, outputFile = NULL, itemColumns = c("IA1", "IB1"), practiceColumn = NULL, practiceValue = "practice") {

  if(is.null(choiceDataframe)) {
    stop("experimentFile must be specified")
  }

  if(!is.null(practiceColumn)) {
    choiceDataframe<-choiceDataframe[choiceDataframe[[practiceColumn]] != practiceValue,]
  }
  items <- unique(choiceDataframe[,itemColumns])

  if(!is.null(outputFile)) {
    write.table(items, outputFile, row.names=F, col.names=F, quote=F)
  }
  return(items)
}
