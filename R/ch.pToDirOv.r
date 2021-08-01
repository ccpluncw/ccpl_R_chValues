#' This function converts Overlap and direction to a directional overlap.
#'
#' Assume two overlapping distributions, D1 and D2. Randomly select a value from D1 and D2.  p(D1 > D2) is the probability that you select a value from D1 that is greater than D2.  Overlap = 1 - [abs (p(D1 > D2) - 0.5) / 0.5].  Directional Overlap = abs(overlap - 1) * direction. Directional overlap goes from -1 (no overlap D1 smaller than D2) to 0 (D1 == D2) to 1 (no overlap D1 larger than D2)
#' @param pIn When randomly sampling from two distributions (D1 and D2), this is the p(D1 > D2) .
#' @param direction A number that equals -1 or 1. The direction specifies the whether D1 or D2 was the greater distribution with the greater value. It is output when p(D1 > D2) is calculated with the bootstrap.
#' @return Directional Overlap
#' @keywords overlap meanP p(D1 > D2) directional overlap
#' @export
#' @examples ch.pToDirOv (0.5, 1)

ch.pToDirOv <- function(pIn, dirIn) {

  if(pIn > 1 | pIn < 0) {
    stop("pIn must be between 0 and 1")
  }
  if(dirIn %in% c(-1,1) ) {
    stop("Direction must equal 1 or -1")
  }


  ovIn <-ch.pToOverlap(pIn)
  dirOv <- ch.overlapToDirOv(ovIn,dirIn)
  return(dirOv)
}
