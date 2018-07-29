#' A function to filter the values data for analysis
#'
#' This function filters the values data for analysis by removing RT outliers and bad subjects based on RT. The thresholds for removal are set in the valuessDBfile that is read and stored in a parameter list.
#' @param data the values dataframe.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param promptCol a string that specifies the name of the column in "data" that contains the prompt for each trial.
#' @param valueCol a string that specifies the name of the column in "data" that contains the participant's estimate of value for the item in each trial.
#' @param params a list of parameters that are read in using "ch.readValuesDBfile.r."
#' @keywords values data filter
#' @return a dataframe of filtered data.  It also writes the items; the item statistics; and the data; to the files specified in params.
#' @export
#' @import chutils
#' @examples ch.filterValuesData (data=valuesData, "sn", "prompt", "resp", params=parameters)

ch.filterValuesData <- function (data, snCol, promptCol, valueCol, RTcol, params) {

# #### remove subjects who had computer troubles or did not comply with the cell phone use instructions
	data <- ch.removeBadSNs(data, snCol, params$removeBadSNFile)

#######_____REMOVE RESP ABOVE MAX PERCENTILE THRESHOLD_____#######
  data[[promptCol]]<-droplevels(data[[promptCol]])

  if (params$keybRTtransform == "log") {
		data$tValue <- ch.altLogTransform(data[[valueCol]])
  } else {
    data$tValue <- data[[valueCol]]
  }

	#remove SNs based on RT criteria
	outList <- ch.filterGrpBtwn(data, RTcol, snCol, lowThresh = params$minMedianRTthreshold, FUN=median)
	data <- outList$datKeptRaw
	numSN.belowRTthresh <- outList$numRemoved
	sn.removed.belowRTthresh	<- outList$datRemoved

	#remove SNs based on Values criteria
	outList <- ch.filterGrpBtwn(data, RTcol, "tValue", lowThresh = params$medianTrespThresh, FUN=median)
	data <- outList$datKeptRaw
	numSN.belowTValuesThresh <- outList$numRemoved
	sn.removed.belowTValuesThresh	<- outList$datRemoved

	#remove individual trials based on RT criteria above
	outList <- ch.filterDataBetween(data, RTcol, params$lowRTthreshold, params$highRTthreshold)
	data <- outList$datKept
	pRTremoved <- outList$pRemoved

	statsOutFN <- paste(params$dt.set, params$statsOutputFilename)
 	sink(statsOutFN, append=F)
	 	cat("\nminMedianRTthreshold\n\t",params$minMedianRTthreshold)
		cat("\nN subs RM RT Threshold:\n\t", numSN.belowRTthresh)
		print(sn.removed.belowRTthresh)
		cat("\nmedianTrespThresh\n\t",params$medianTrespThresh)
		cat("\nN subs RM tValues Threshold:\n\t", numSN.belowTValuesThresh)
		print(sn.removed.belowTValuesThresh)
		cat("\nlowRTthreshold\n\t",params$lowRTthreshold )
 		cat("\nhighRTthreshold\n\t",params$highRTthreshold)
	 	cat("\nprop datapoints RM:\n\t", pRTremoved)
 	sink(NULL)


	#get item statistics and save them to a dataset.
	df.itemStats <- as.data.frame(data %>% dplyr::group_by_(promptCol) %>% dplyr::summarise(
	      medianRT = median(eval(parse(text=RTcol))),
				tValueMean=mean(tValue),
				tValueSd=sd(tValue),
				tValueMedian=median(tValue),
				tValueQ25 = quantile(tValue,probs=c(.25), type=3),
				tValueQ75 = quantile(tValue,probs=c(.75), type=3),
				valueMean=mean(eval(parse(text=valueCol))),
				valueSd=sd(eval(parse(text=valueCol))),
				valueMedian=median(eval(parse(text=valueCol))),
				valueQ25 = quantile(eval(parse(text=valueCol)),probs=c(.25), type=3),
				valueQ75 = quantile(eval(parse(text=valueCol)),probs=c(.75), type=3)
	))

	items <- unique(data[[promptCol]])

  write.table(df.itemStats, file=paste(params$dt.set,params$itemStatsFile), quote=F, sep="\t", row.names = F, append=F)
  write.table(data, file=paste(params$dt.set,params$valuesFilteredDataFile), quote=F, sep="\t", row.names = F, append=F)
	write.table(items, file=paste(params$dt.set,params$itemSetDataFile), quote=F, sep="\t", row.names = F, , col.names = F, append=F)

	return(data)
}
