#' A function to calculate the overlaps of the values data using a bootstrap
#'
#' This function calculate the overlaps of the values data using a bootstrap.
#' @param data the values dataframe.
#' @param valueCol a string that specifies the name of the column in "data" that contains the participant's estimate of value for the item in each trial.
#' @param promptCol a string that specifies the name of the column in "data" that contains the prompt for each trial.
#' @param params a list of parameters that are read in using "ch.readValuesDBfile.r."
#' @param allItems a boolean to state whether all the items are to be bootsrapped.  If FALSE, then the "itemSetDataFile" in the params list must contain a subset of items to be bootstrapped. DEFAULT = TRUE.
#' @return a dataframe of the overlaps by probe combination.  It also writes the data to the file (bootstrapOutdataFile) specified in params.
#' @export
#' @examples ch.valuesBootstrapOverlaps (data=valuesData, "tValues", "prompt", params=parameters)

ch.valuesBootstrapOverlaps <- function (data, valuesCol, promptCol, params, allItems = T) {

  #throw an error if the min is less than the max
  if (params$minNumPerSide > params$maxNumPerSide) {
    print(paste("minNumPerSide (", params$minNumPerSide, ") is greater than maxNumPerSide (", params$maxNumPerSide, ").", sep=""))
    print("This is not allowed")
    print("I (the all mighty computer) am switching the two values")
    tmpMin <- params$minNumPerSide
    params$minNumPerSide <- params$maxNumPerSide
    params$maxNumPerSide <- tmpMin
    print(paste("Now, minNumPerSide = ", params$minNumPerSide, "and maxNumPerSide = ", params$maxNumPerSide, sep=" "))
  }

  if(allItems == T) {
    itemSet <-read.table(paste(params$dt.set,params$itemSetDataFile), header=F, sep="\t", quote="\"")
  } else {
    itemSet <-read.table(params$itemSetDataFile, header=F, sep="\t", quote="\"")
  }
  #drop extraneous columns and levels
  itemSet <- itemSet[,1]
  itemSet <- factor(itemSet)
  numItems <- length(itemSet)

  #make all the prompts as similar as possible
    itemSet <- tolower(trimws(itemSet))
    data[[promptCol]] <- tolower(trimws(data[[promptCol]]))

    itemsNotInData <- unique(itemSet[! itemSet %in% unique(data[[promptCol]])])

    if (length(itemsNotInData) > 0) {
      print("these items are in the itemSet, but not in the dataSet")
      print(itemsNotInData)
      print("fatalError: All items in itemSet must be in dataSet")
      stop()
    }

  #create a vector of all possible combinations on one side of the dilemma equation
  df.combns <- ch.combnVector(numItems, params$minNumPerSide,params$maxNumPerSide)

  for (i in 1:length(df.combns$I1)) {
    print(paste(itemSet[df.combns[i,1]],itemSet[df.combns[i,2]] ))
  }

  #Now use df.combns to do the bootstrap.  That is, use the values to index the items and grab samples, if the indexs are NA then ignore.
  #run the bootstrap for each combination
  alldat <- ch.batchOverlap(data[[valuesCol]], data[[promptCol]], itemSet, df.combns, params$numRuns, outFile = paste(params$dt.set,params$bootstrapOutdataFile))

  #notify me that the bootstrap is complete
  pushOut <- paste(getwd(), " ::    :: Values Bootstrap Complete: ", nrow(df.combns)-1, " done.", sep = "")
  RPushbullet::pbPost(type = "note", title="Values Bootstrap Complete", body = pushOut)

  return(alldat)

}
