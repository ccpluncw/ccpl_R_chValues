#' A function to get the value stats for each item
#'
#' This function gets the value stats for each item and outputs those into a file.
#' @param data the values dataframe.
#' @param promptCol a string that specifies the name of the column in "data" that contains the prompt for each trial.
#' @param valueCol a string that specifies the name of the column in "data" that contains the participant's estimate of value for the item in each trial.
#' @param RTcol a string that specifies the name of the column in "data" that contains the participant's reaction times for the item in each trial.
#' @param outFile a string that specifies the name of the file to output the stats in. DEFAULT = NULL (no output)
#' @keywords values data filter
#' @return a dataframe of the value stats.  It also writes the item statistics to the outFile.
#' @export
#' @examples ch.getValuesSummaryStats (data=valuesData, "prompt", "respS", "respTime")

ch.getValuesSummaryStats <- function (data, promptCol, valueCol, RTcol, outFile = NULL) {

#get item statistics and save them to a dataset.
	df.itemStats <- as.data.frame(data %>% dplyr::group_by_(promptCol) %>% dplyr::summarise(
	      medianRT = median(eval(parse(text=RTcol))),
				valueMean=mean(eval(parse(text=valueCol))),
				valueSD=sd(eval(parse(text=valueCol))),
				valueMedian=median(eval(parse(text=valueCol))),
				valueQ25 = quantile(eval(parse(text=valueCol)),probs=c(.25), type=3),
				valueQ75 = quantile(eval(parse(text=valueCol)),probs=c(.75), type=3),
				valueBox = quantile(eval(parse(text=valueCol)),probs=c(.75), type=3) - quantile(eval(parse(text=valueCol)),probs=c(.25), type=3),
				N = sum(!is.na(eval(parse(text=valueCol))))
	))

	if(!is.null(outFile)) {
  	write.table(df.itemStats, file=outFile, quote=F, sep="\t", row.names = F, append=F)
	}

	return(df.itemStats)
}
