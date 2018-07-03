#' A function to read the values dbfile
#'
#' This function reads the dbfile for the values analysis.  The dbfile contains key words and their values for the analysis.
#' @param dbfile the filename of the dbfile.
#' @keywords values dbfile read
#' @return a list with all the parameters
#' @export
#' @examples ch.readMoralsDBfile ("myDBfile.txt")

ch.readValuessDBfile <- function (filename = "valuesDBfile.txt") {

  params<-read.table(filename,row.names=1,header = F,sep=",")

  #### Filenames
  valuesTaskDataFile <- as.character(params['valuesTaskDataFile',1])
  valuesFilteredDataFile <- as.character(params['valuesFilteredDataFile',1])
  removeBadSNFile <- as.character(params['removeBadSNFile',1])
  bootstrapOutdataFile <- as.character(params['bootstrapOutdataFile',1])
  itemSetDataFile <- as.character(params['itemSetDataFile',1])
  itemStatsFile <- as.character(params['itemStatsFile',1])
  statsOutputFilename <- as.character(params['statsOutputFilename',1])

  ######_____SET SWITCHES AND THRESHOLDS______######
  minNumPerSide <- as.numeric(as.character(params['minNumPerSide',1]))
  maxNumPerSide <- as.numeric(as.character(params['maxNumPerSide',1]))
  lowRTthreshold <- as.numeric(as.character(params['lowRTthreshold',1]))
  highRTthreshold <- as.numeric(as.character(params['highRTthreshold',1]))
  minMedianRTthreshold <- as.numeric(as.character(params['minMedianRTthreshold',1]))
  medianTrespThresh <- as.numeric(as.character(params['medianTrespThresh',1]))
  keybRTtransform = as.character(params['keybRTtransform',1])
  dt.set = as.character(params['dt.set',1])
  numRuns <- as.numeric(as.character(params['numRuns',1]))

  paramList <- list (
    valuesTaskDataFile = valuesTaskDataFile,
    valuesFilteredDataFile = valuesFilteredDataFile,
    removeBadSNFile = removeBadSNFile,
    bootstrapOutdataFile = bootstrapOutdataFile,
    itemSetDataFile = itemSetDataFile,
    itemStatsFile = itemStatsFile,
    statsOutputFilename = statsOutputFilename,
    minNumPerSide = minNumPerSide,
    maxNumPerSide = maxNumPerSide,
    lowRTthreshold = lowRTthreshold,
    highRTthreshold = highRTthreshold,
    minMedianRTthreshold = minMedianRTthreshold,
    medianTrespThresh = medianTrespThresh,
    keybRTtransform = keybRTtransform,
    dt.set = dt.set,
    numRuns = numRuns
  )
  return(paramList)

}
