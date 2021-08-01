#' A function to creates a data frame composed of two sets of combinations (take n items r at a time, order doesn't matter).
#'
#' This function to creates a data frame composed of two sets of combinations. First is creates a data frame for
#' Combination A (take n items r at a time, order doesn't matter). Then it does the same for Combination B. Then it
#' creates data frame that has every possible combination of the rows of each list.
#' @param data A vector of items that will be combined.
#' @param minNumPerSide An integer that specifies the minimum number of items in group. DEFAULT = 1
#' @param maxNumPerSide An integer that specifies the minimum number of items in group. DEFAULT = 1
#' @param sample A string that specifies how to sample the items to create the combinations. The choices are:
#' (1) "byGroup" for each group size (1, 2, etc) the sample size will equal nStimPerGroup;
#' (2) "random" the entire set sample size will equal nStimPerGroup;
#' (3) "all" all the combinations will be generated, without taking a sample (nStimPerGroup is ignored in this case). DEFAULT = "all"
#' The samples will all be drawn randomly without replacement, unless the sample size is greater than the number of
#' possible combinations. In that case the sample will be drawn randomly with replacement.
#' @param nStimPerGroup An integer specifying the number of stim per group to select.  If nStimPerGroup == 0, then the program will select all the single trials, and use that total as nStimPerGroup.
#' @param includeReverseOrder A boolean to specify whether the combinations should be copied and the order reversed and added to the output. This is done so that there is not a side bias with the presentation of the items. DEFAULT = TRUE.
#' @keywords combination items
#' @return dataframe with the all the combinations. The columns are labled IA.# and IB.# to indicate the group.:
#' @export
#' @examples ch.getCombinations (itemSet = c("item1","item2","item3", "item4"), minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0, includeReverseOrder = TRUE)

ch.getCombinations <- function(itemSet, minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0, includeReverseOrder = TRUE) {
  nItems <- length(itemSet)
  df.tmp <- ch.combnVector (nItems, minNumPerSide,maxNumPerSide, addSelfMatch = FALSE)
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
      df.combns <- chutils::ch.rbind(df.combns, df.tmp2)
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

  itemAarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))
  itemBarray <- array("XX", dim = c(totalNumOverlaps,maxNumPerSide))

  itemAn <- array(0, dim = c(totalNumOverlaps,1))
  itemBn <- array(0, dim = c(totalNumOverlaps,1))

  i <- 1
  for(l in 1:(numCombinations - 1)){
    print(paste(l, "of", (numCombinations - 1), sep=" "))

    for(k in l:(numCombinations - 1)){
      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- as.character(itemSet[df.combns[l,m]])
        itemBarray[i,m] <- as.character(itemSet[df.combns[k+1,m]])

        if (is.na(itemAarray[i,m])) itemAarray[i,m] <- "NA"
        if (is.na(itemBarray[i,m])) itemBarray[i,m] <- "NA"

        if (itemAarray[i,m] != "NA") {
          itemAn[i] <- itemAn[i] + 1
        }
        if (itemBarray[i,m] != "NA")  {
          itemBn[i] <- itemBn[i] + 1
        }
      }
      i = i+1

    }
  }


  if (includeReverseOrder == TRUE) {
    #reverse order to get all the different combinations.
    df.1 <- data.frame(IA = itemAarray, IB = itemBarray, nIA = itemAn, nIB = itemBn)
    df.2 <- data.frame(IA = itemBarray, IB = itemAarray, nIA = itemBn, nIB = itemAn)
    df.out <- rbind(df.1, df.2)
  } else {
    df.1 <- data.frame(IA = itemAarray, IB = itemBarray, nIA = itemAn, nIB = itemBn)
    df.out <- df.1
  }

  if(length(df.out) == 4) {
    names(df.out)[names(df.out) == "IA"] <- "IA.1"
    names(df.out)[names(df.out) == "IB"] <- "IB.1"
  }

  return(df.out)
}
