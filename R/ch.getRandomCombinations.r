#' A function to creates a data frame composed of two sets of combinations (take n items r at a time, order doesn't matter). This function chooses items randomly and effectively handles item modifiers
#'
#' This function to creates a data frame composed of two sets of combinations. First is creates a data frame for
#' Combination A (take n items r at a time, order doesn't matter). Then it does the same for Combination B. Then it
#' creates data frame that has every possible combination of the rows of each list.
#' This function effectively handles modifiers, but intelligently selecting modifiers and items separately.
#'
#' @param itemFileName A string that specifies the filename and path of the item file. The itemfile should be tab
#' delimited and have the following format:  Item <tab> Modifier1 <tab> Modifier2 <tab> etc. The modifiers are the possible
#' modifiers for the Item and will be presented in front of the item.
#' @param df.groups A dataframe that has a column for each group (2 groups = 2 columns) and a row for each group size combination. So if you want one item in each group, the dataframe would be data.frame(g1 = c(1), g2 = c(1)).  If you want every size combination of 1 and 2 items in each group, you would have: data.frame(g1 = c(1, 2, 1, 2), g2 = c(1, 1, 2, 2))
#' @param nStimPerGroup An integer specifying the number of stimuli to sample per group .
#' @keywords combination items random choice
#' @return dataframe with the all the combinations. The columns are labled IA.# and IB.# to indicate the group.:
#' @export
#' @examples ch.getRandomCombinations (itemSet = c("item1","item2","item3", "item4"), df.groups = data.frame(g1 = c(1, 2, 1, 2), g2 = c(1, 1, 2, 2)), nStimPerGroup = 1000)

ch.getRandomCombinations <- function (itemfileName, df.groups, numStimPerGroup) {

  # get maximum and minimum number of items per side
  maxNumPerSide <- max(df.groups)
  minNumPerSide <- min(df.groups)
  # get the total number of groups
  numGroups <- nrow(df.groups)

  # read in items and any modificiations. First column is items, remaining columns are modificiations
  expItems <- read.table(itemfileName, header=F, sep="\t", quote="\"")
  numCols <- ncol(expItems)
  names(expItems)[1] <- c("Item")

  #create a dataframe with items and modifications, and a separate dataframe with all possible combinations of item/modifications
  probes <- NULL
  if(numCols > 1) {
    for(i in 2:numCols) {
      modCol <- paste("mod", i-1, sep="")
      names(expItems)[i] <- modCol
      probes[[modCol]] <- paste(expItems[[modCol]], expItems$Item, sep = " ")
    }
  } else {
    #if there are no Mods, then the probe itself acts as a mod.
    modCol <- paste("mod", 1, sep="")
    probes[[modCol]] <- expItems$Item
  }
  probes <- data.frame(probes)

  #get the number of items and number of modifications
  numItems <- nrow(expItems)
  numMods <- numCols - 1
  #if there are no Mods, then the probe itself acts as a mod.
  if(numMods == 0) numMods <- 1

  #create blank arrays the size of the number of items
  itemAarray <- array("NA", dim = c(numStimPerGroup*numGroups,maxNumPerSide))
  itemBarray <- array("NA", dim = c(numStimPerGroup*numGroups,maxNumPerSide))

  #create arrays to hold the number of items
  itemAn <- array(0, dim = c(numStimPerGroup*numGroups,1))
  itemBn <- array(0, dim = c(numStimPerGroup*numGroups,1))

  #get sequences for random selection
  itemSeq <- seq(1,numItems)
  modSeq <- seq(1,numMods)
  i <- 1
  #for each group
  for(l in 1:(numGroups)){
    #for the total number of items per groups
    print(paste("Group: ",l, "of", numGroups, sep=" "))

    for(k in 1:(numStimPerGroup)){

      #randomly select items and modifications for IA and IB
      iA <- data.frame (sampRows = sample(itemSeq, df.groups[l,1], replace=F), sampCols = sample(modSeq, df.groups[l,1], replace = T))
      iB <- data.frame (sampRows = sample(itemSeq, df.groups[l,2], replace=F), sampCols = sample(modSeq, df.groups[l,2], replace = T))

      #Now make sure the items are not identical in IA and IB
      if(nrow(iA)==nrow(iB)) {
        while(dplyr::all_equal(iA, iB)==TRUE) {
          iA <- data.frame (sampRows = sample(itemSeq, df.groups[l,1], replace=F), sampCols = sample(modSeq, df.groups[l,1], replace = T))
          iB <- data.frame (sampRows = sample(itemSeq, df.groups[l,2], replace=F), sampCols = sample(modSeq, df.groups[l,2], replace = T))
        }
      }

      #for every item in iA, add the item into the itemAarray
      for(m in 1:length(iA$sampRows)) {
        itemAarray[i,m] <- as.character(probes[iA$sampRows[m],iA$sampCols[m]])
        itemAn[i] <- itemAn[i] + 1
      }

      #for every item in iB, add the item into the itemBarray
      for(m in 1:length(iB$sampRows)) {
        itemBarray[i,m] <- as.character(probes[iB$sampRows[m],iB$sampCols[m]])
        itemBn[i] <- itemBn[i] + 1
      }
    i = i+1
    }
  }

  #put the itemAarray and itemBarray in a single dataframe
  df.out <- data.frame(IA = itemAarray, IB = itemBarray, nIA = itemAn, nIB = itemBn)

  #do some houseclearning
  if(length(df.out) == 4) {
    names(df.out)[names(df.out) == "IA"] <- "IA.1"
    names(df.out)[names(df.out) == "IB"] <- "IB.1"
  }

  #return the combinations
  return (df.out)
}
