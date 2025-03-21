#' A function to filter the values data for analysis
#'
#' This function filters the values data for analysis by removing RT outliers and bad subjects based on RT. The thresholds for removal are set in the valuessDBfile that is read and stored in a parameter list.
#' @param data the values dataframe.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param promptCol a string that specifies the name of the column in "data" that contains the prompt for each trial.
#' @param valueCol a string that specifies the name of the column in "data" that contains the participant's estimate of value for the item in each trial.
#' @param RTcol a string that specifies the name of the column in "data" that contains the participant's reaction times for the item in each trial.
#' @param params a list of parameters that are read in using "ch.readValuesDBfile.r."
#' @keywords values data filter
#' @return a dataframe of filtered data.  It also writes the items; the item statistics; and the data; to the files specified in params.
#' @export
#' @import chutils
#' @importFrom dplyr %>%
#' @examples ch.filterValuesData (data=valuesData, "sn", "prompt", "resp", params=parameters)

ch.filterValuesData <- function (data, snCol, promptCol, valueCol, RTcol, params) {

# #### remove subjects who had computer troubles or did not comply with the cell phone use instructions
	data <- chutils::ch.removeBadSNs(data, snCol, params$removeBadSNFile)

#######_____REMOVE RESP ABOVE MAX PERCENTILE THRESHOLD_____#######
	data[[promptCol]] <- as.factor(data[[promptCol]])
  data[[promptCol]]<-droplevels(data[[promptCol]])

  if (params$keybRTtransform == "log") {
		data$tValue <- chutils::ch.altLogTransform(data[[valueCol]])
  } else {
		if(params$keybRTtransform == "none") {
			data$tValue <- data[[valueCol]]
		} else {
			data$tValue <- chutils::ch.altRootTransform(data[[valueCol]], root=as.numeric(as.character(params$keybRTtransform)))
  	}
	}

	#remove SNs based on RT criteria
	outList <- chutils::ch.filterGrpBtwn(data, RTcol, snCol, lowThresh = params$minMedianRTthreshold, FUN=median)
	data <- outList$datKeptRaw
	numSN.belowRTthresh <- outList$numRemoved
	sn.removed.belowRTthresh	<- outList$datRemoved

	#remove SNs based on Values criteria
	outList <- chutils::ch.filterGrpBtwn(data, RTcol, "tValue", lowThresh = params$medianTrespThresh, FUN=median)
	data <- outList$datKeptRaw
	numSN.belowTValuesThresh <- outList$numRemoved
	sn.removed.belowTValuesThresh	<- outList$datRemoved

	#remove individual trials based on RT criteria above
	outList <- chutils::ch.filterDataBetween(data, RTcol, params$lowRTthreshold, params$highRTthreshold)
	data <- outList$datKept
	pRTremoved <- outList$pRemoved

	statsOutFN <- paste(params$dt.set, params$statsOutputFilename)
 	sink(statsOutFN, append=F)
	 	cat("\nminMedianRTthreshold: ",params$minMedianRTthreshold, "\n")
		cat("\nN subs RM RT Threshold: ", numSN.belowRTthresh, "\n")
		print(sn.removed.belowRTthresh)
		cat("\nmedianTrespThresh: ",params$medianTrespThresh, "\n")
		cat("\nN subs RM tValues Threshold: ", numSN.belowTValuesThresh, "\n")
		print(sn.removed.belowTValuesThresh)
		cat("\nlowRTthreshold: ",params$lowRTthreshold, "\n" )
 		cat("\nhighRTthreshold: ",params$highRTthreshold, "\n")
	 	cat("\nprop datapoints RM: ", pRTremoved, "\n")
 	sink(NULL)


	#get item statistics and save them to a dataset.
	df.itemStats <- as.data.frame(data %>% dplyr::group_by(across(all_of(promptCol))) %>% dplyr::summarise(
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
