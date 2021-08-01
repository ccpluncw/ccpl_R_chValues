#this function generates the combinations of items for the stimuli for a single vector of items
#because this is a single vector version, there is only one option displayed. This is used for a "yes/no" experiment
#this version will work with modifications.

#' A function to creates a data frame composed of one sets of combinations (take n items r at a time, order doesn't matter) with intellegent use of modifiers.
#'
#' This function generates the combinations of a single vector of items. Because this is a single vector version,
#' there is often only one option displayed in a decision experiment (e.g., a "yes/no" experiment)
#' This function intellegently combines the items with their modifiers. Specifically, one modifier will be randomly chosen to add
#' to an item, and no group will be created so that there are two of the same items (regardless of their modifiers).
#'
#' @param itemDataframe A dataframe that contains the items and the modifiers.  The dataframe should be formatted as follows:
#' Item <tab> Modifier <tab> Modifier <tab> Modifier ...
#' @param minNumPerSide An integer that specifies the minimum number of items in the group. DEFAULT = 1
#' @param maxNumPerSide An integer that specifies the minimum number of items in the group. DEFAULT = 1
#' @param sample A string that specifies how to sample the items to create the combinations. The choices are:
#' (1) "byGroup" for each group size (1, 2, etc) the sample size will equal nStimPerGroup;
#' (2) "random" the entire set sample size will equal nStimPerGroup;
#' (3) "all" all the combinations will be generated, without taking a sample (nStimPerGroup is ignored in this case). DEFAULT = "all"
#' The samples will all be drawn randomly without replacement, unless the sample size is greater than the number of
#' possible combinations. In that case the sample will be drawn randomly with replacement.
#' @param nStimPerGroup An integer specifying the number of stim per group to select.  If nStimPerGroup == 0, then the program will select all the single trials, and use that total as nStimPerGroup. DEFAULT = 0.
#' @keywords combination items modifiers
#' @return dataframe with the all the combinations. The columns are labled IA.# to indicate they belong to the same group.:
#' @export
#' @examples ch.getCombinationsOfOneVectorWithMods (itemSet = c("item1","item2","item3", "item4"), minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0)
ch.getCombinationsOfOneVectorWithMods <- function(itemDataframe, minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0) {

  expItems <- itemDataframe
  numCols <- ncol(expItems)
  names(expItems)[1] <- c("Item")

  #create a dataframe with items and modifications, and a separate dataframe with all possible combinations of item/modifications
  probes <- NULL
  df.tmp <- NULL
  if(numCols > 1) {
    for(i in 2:numCols) {
      modNameCol <- paste("mod", i-1, sep="")
      names(expItems)[i] <- modNameCol
      df.tmp$Item <- expItems$Item
      df.tmp$mod <- expItems[[modNameCol]]
      df.tmp$probeMod <- paste(expItems[[modNameCol]], expItems$Item, sep = " ")
      df.tmp <- data.frame(df.tmp)
      probes <- ch.rbind(probes, df.tmp)
    }
  } else {
    #if there are no Mods, then the probe itself acts as a mod.
    probes$Item <- expItems$Item
    probes$mod <- "NA"
    probes$probeMod <- expItems$Item
  }
  probes <- data.frame(probes)

  nItems <- nrow(probes)

  df.tmp <- chValues::ch.combnVector (nItems, minNumPerSide,maxNumPerSide)
  df.tmp <- df.tmp[1:(nrow(df.tmp)-nItems),]

  df.tmp$stimPerGroup <- length(df.tmp) - rowSums(is.na(df.tmp))

  df.combns <- NULL

  if(sample != "all") {
    if(nStimPerGroup == 0) {
      size <-  nrow(df.tmp[df.tmp$stimPerGroup == 1,])
    } else {
      size <- nStimPerGroup
    }
    numStimTypes <- unique(df.tmp$stimPerGroup)
  }

  if(sample == "byGroup") {
    for(i in numStimTypes) {
      df.tmp1 <- df.tmp[df.tmp$stimPerGroup == i,]
      if(nrow(df.tmp1) > size) {
        df.tmp2 <- df.tmp1[sample(nrow(df.tmp1), size, replace = F), ]
      }
      if(nrow(df.tmp1) == size) {
        df.tmp2 <- df.tmp1
      }
      if(nrow(df.tmp1) < size) {
        df.tmp2 <- df.tmp1[sample(nrow(df.tmp1), size, replace = T), ]
      }
      df.combns <- ch.rbind(df.combns, df.tmp2)
    }
  }
  if(sample == "random") {
    if(nrow(df.tmp) > size) {
      df.combns <- df.tmp[sample(nrow(df.tmp), size, replace = F), ]
    }
    if(nrow(df.tmp1) == size) {
      df.combns <- df.tmp
    }
    if(nrow(df.tmp) < size) {
      df.combns <- df.tmp[sample(nrow(df.tmp), size, replace = T), ]
    }
  }
  if(sample == "all") {
    df.combns <- df.tmp
  }

  numCombinations <- nrow(df.combns)
  maxNumPerSide <- ncol(df.combns) - 1
  totalNumOverlaps <- (numCombinations^2- numCombinations)/2

  itemAarray <- array("XX", dim = c(numCombinations,maxNumPerSide))
  itemMod <- array("XX", dim = c(numCombinations,maxNumPerSide))
  item <- array("XX", dim = c(numCombinations,maxNumPerSide))

  itemAn <- array(0, dim = c(numCombinations,1))

  i <- 1
  for(l in 1:(numCombinations)){
    print(paste(l, "of", (numCombinations), sep=" "))
      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- as.character(probes$probeMod[df.combns[l,m]])
        itemMod[i,m] <- as.character(probes$mod[df.combns[l,m]])
        item[i,m] <- as.character(probes$Item[df.combns[l,m]])

        if (is.na(itemAarray[i,m])) {
          itemAarray[i,m] <- "NA"
        }
        if (is.na(itemMod[i,m])) {
          itemMod[i,m] <- "NA"
        }
        if (is.na(item[i,m])) {
          item[i,m] <- "NA"
        }

        if (itemAarray[i,m] != "NA") {
          itemAn[i] <- itemAn[i] + 1
        }
      }
      i = i+1
  }

    df.1 <- data.frame(IA = itemAarray, Item = item, Mod = itemMod, nIA = itemAn)
    df.out <- data.frame(df.1[(df.1$Item.1 != df.1$Item.2) | (df.1$nIA == 1),])


  if(length(df.out) <= 4) {
    names(df.out)[names(df.out) == "IA"] <- "IA.1"
  }

  return(df.out)
}
