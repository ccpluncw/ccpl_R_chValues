#' Calculate the distributional overlaps for item combinations specified in the df.prompts dataframe, multicore version
#'
#' This function runs ch.distOverlap for all the combinations of distributions identified in df.combns.
#' @param values A vector of the values in the distributions for each item.
#' @param items A vector that identifies the item that each value (in the values option) is linked to.
#' @param df.prompts A dataframe of item combinations that you want to run the bootstraps for. The each column should specify one item of the combination to be compared. Use "NA" for items in a column whereby no item is present.
#' @param itemAcolNames A vector of strings that identifies the column names in df.prompts that contain the items whose values should be combined and compared to those items identified in the itemBcolNames columns.
#' @param itemBcolNames A vector of strings that identifies the column names in df.prompts that contain the items whose values should be combined and compared to those items identified in the itemAcolNames columns.
#' @param numRuns the number of runs to do in the bootstrap. DEFAULT = 1000.
#' @param outFile the name of a text file that will be output as the bootstrap runs.  This is filled incrementally just in case the program dies. DEFAULT = NULL - will not write a file
#' @param multicore A boolean specifying whether the bootstrap runs in parallel using multiple cores. DEFAULT = TRUE
#' @param verbose A boolean specifying whether to print the progress of the bootstrap. DEFAULT = FALSE
#' @param combFun If there are multiple items contributing to a single distribution, this function describes how the values will be combined across items in both the X and Y distributions. The function must combine elements of a list that might be of different different lengths. The default just flattens the list into one large vector.  DEFAULT = ch.maxAveComb  (with probMax = 0.5)
#' @keywords distribution overlap bootstrap values
#' @return a dataframe with the overlap statistics for each set of distributions compared. See ch.distOverlap
#' @export
#' @examples ch.batchOverlapPromptFile (data$responses, data$items,itemSet,df.prompts, c("IA1", "IA2"), c("IB1"), 1000, outFile ="out.txt" )

ch.MCbatchOverlapPromptFile <- function(values, items, df.prompts, itemAcolNames = "IA",itemBcolNames = "IB", numRuns = 1000, outFile = NULL,  multicore = TRUE, verbose = FALSE, combFun = ch.maxAveComb, ...) {

  #set up for multicore processing
  if(multicore) {
    cl<- setUpParallel ()
  }

  data <- data.frame(values, items)

  promptCols <- ncol(df.prompts)
  for(i in 1:promptCols) {
    df.prompts[,i] <- as.character(df.prompts[,i])
    df.prompts[,i] <- tolower(trimws(df.prompts[,i]))
  }
  itemSet <- unique(c(na.omit(as.character(unlist(df.prompts, use.names=FALSE)))))

  data$items <- tolower(trimws(data$items))

  itemsNotInData <- unique(itemSet[! itemSet %in% unique(data$items)])

  if (length(itemsNotInData) > 0) {
    print("these items are in the itemSet, but not in the dataSet")
    print(itemsNotInData)
    print("fatalError: All items in itemSet must be in dataSet")
    stop()
  }


  totalNumOverlaps <- nrow(df.prompts)
  maxNumItemA <- length(itemAcolNames)
  maxNumItemB <- length(itemBcolNames)

  itemAarray <- array("XX", dim = c(1,maxNumItemA))
  itemBarray <- array("XX", dim = c(1,maxNumItemB))

  `%tmpDo%` <- ifelse(multicore, `%dopar%`, `%do%`)
  alldat <- foreach::foreach(l = 1:totalNumOverlaps, .combine="rbind") %tmpDo% {
    if(verbose) {
      print(paste(l, "of", totalNumOverlaps, sep=" "))
    }
    xValue <- list()
    yValue <- list()

      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumItemA) {
        itemAarray[1,m] <- toString(df.prompts[l,itemAcolNames[m]])
        if (itemAarray[1,m] != "NA" | is.na(itemAarray[1,m])) {
          xTmp <- data$values[data$items==itemAarray[1,m]]
          varName <- paste("v", m, sep="")
          xValue[[varName]] <- xTmp
        }
      }
      for(m in 1:maxNumItemB) {
        itemBarray[1,m] <- toString(df.prompts[l,itemBcolNames[m]])
        if (itemBarray[1,m] != "NA" | is.na(itemBarray[1,m])) {
          yTmp <- data$values[data$items==itemBarray[1,m]]
          varName <- paste("v", m, sep="")
          yValue[[varName]] <- yTmp
        }
      }
      #run bootstrap to get overlap information
      pOut <- ch.distOverlapFlex(xValue,yValue,numRuns,combFun = combFun, ...)

      data.frame(itemAarray, itemBarray, averageP = pOut["percent"],sdp = pOut["sd"], overlap = pOut["overlap"],direction = pOut["direction"])

  }

  #rename columns
  for(npsA in 1:maxNumItemA) {
    colnames(alldat)[npsA] <- paste("IA", npsA, sep="")
  }
  for(npsB in (npsA+1):(npsA+maxNumItemB)) {
    colnames(alldat)[npsB] <- paste("IB", (npsB-npsA), sep="")
  }
  if(!is.null(outFile)) {
    write.table(alldat, file=outFile, quote=F, sep="\t", row.names = F, append=F)
  }

  #end multi-core processing
  if(multicore) {
    endParallel (cl)
  }

  return(alldat)
}
