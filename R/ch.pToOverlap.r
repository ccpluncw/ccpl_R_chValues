#' This function converts p(D1 > D2) to Overlap.
#'
#' Assume two overlapping distributions, D1 and D2. Randomly select a value from D1 and D2.  p(D1 > D2) is the probability that you select a value from D1 that is greater than D2.  Overlap = 1 - [abs (p(D1 > D2) - 0.5) / 0.5].  This function converts p(D1 > D2) to Overlap.
#' @param pIn When randomly sampling from two distributions (D1 and D2), this is the p(D1 > D2) .
#' @return Overlap
#' @keywords overlap meanP p(D1 > D2)
#' @export
#' @examples ch.pToOverlap (0.5)

ch.pToOverlap <- function(pIn) {

  if(pIn > 1 | pIn < 0) {
    stop("pIn must be between 0 and 1")
  }
  overlap <-1-abs(((pIn-0.5)/0.5))

  return(overlap)
}
