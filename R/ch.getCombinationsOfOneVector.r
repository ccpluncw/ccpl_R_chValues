#' A function to creates a data frame composed of one sets of combinations (take n items r at a time, order doesn't matter).
#'
#' This function to creates a data frame composed of one sets of combinations. It creates a data frame for
#' Combination A (take n items r at a time, order doesn't matter).
#'
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
#' @keywords combination items
#' @return dataframe with the all the combinations. The columns are labled IA.# and IB.# to indicate the group.:
#' @export
#' @examples ch.getCombinationsOfOneVector (itemSet = c("item1","item2","item3", "item4"), minNumPerSide = 1, maxNumPerSide = 1, sample = "all", nStimPerGroup = 0)

ch.getCombinationsOfOneVector <- function(itemSet, minNumPerSide = 1, maxNumPerSide = 2, sample = "all", nStimPerGroup = 0) {
  nItems <- length(itemSet)
  df.tmp <- ch.combnVector (nItems, minNumPerSide,maxNumPerSide)
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

  itemAarray <- array("XX", dim = c(numCombinations,maxNumPerSide))

  itemAn <- array(0, dim = c(numCombinations,1))

  i <- 1
  for(l in 1:(numCombinations)){
    print(paste(l, "of", (numCombinations), sep=" "))
      #get the raw data for each item and append it into a single vector (like a single distribution)
      for(m in 1:maxNumPerSide) {
        itemAarray[i,m] <- as.character(itemSet[df.combns[l,m]])

        if (is.na(itemAarray[i,m])) itemAarray[i,m] <- "NA"

        if (itemAarray[i,m] != "NA") {
          itemAn[i] <- itemAn[i] + 1
        }
      }
      i = i+1
  }


    df.1 <- data.frame(IA = itemAarray, nIA = itemAn)
    df.out <- df.1

  if(length(df.out) == 2) {
    names(df.out)[names(df.out) == "IA"] <- "IA.1"
  }

  return(df.out)
}
