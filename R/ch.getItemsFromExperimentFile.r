#' A function to extract a the items present in an experiment file
#'
#' @param experimentFile a tab delimited file with data from the experiment.
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @param itemColumn the name of the column that contains the item values. DEFAULT = "ItemX"
#' @param practiceColumn the name of the column that contains the values indicating whether or not the trials are practice. DEFAULT = "typeOfScen"
#' @param practiceValue the value in practiceColumn that specifies the trial is a practice trial. DEFAULT = "practice"
#' @keywords items morals file extract
#' @return a vector with the unique items used in the experiment
#' @export
#' @examples ch.getItemsFromExperimentFile (experiment.txt,output.txt, probes, practice, 0)

ch.getItemsFromExperimentFile <- function (experimentFile = NULL, outputFile = NULL, itemColumn = "ItemX", practiceColumn = "typeOfScen", practiceValue = "practice") {

  if(is.null(experimentFile)) {
    stop("experimentFile must be specified")
  }

  dat<-read.table("data.all.Ex2.txt",header = T, quote="\"",sep="\t")
  dat<-dat[dat[[practiceColumn]] != practiceValue,]
  items <- unique(dat[[itemColumn]])

  if(!is.null(outputFile)) {
    write.table(items, "minwooItems.txt", row.names=F, col.names=F, quote=F)
  }
  return(items)
}
