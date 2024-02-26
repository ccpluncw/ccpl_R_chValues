#' This function converts Overlap and direction to a directional overlap.
#'
#' Assume two overlapping distributions, D1 and D2. Randomly select a value from D1 and D2.  p(D1 > D2) is the probability that you select a value from D1 that is greater than D2.  Overlap = 1 - [abs (p(D1 > D2) - 0.5) / 0.5].  Directional Overlap = abs(overlap - 1) * direction. Directional overlap goes from -1 (no overlap D1 smaller than D2) to 0 (D1 == D2) to 1 (no overlap D1 larger than D2)
#' @param overlap A number that equals 1 - [abs (p(D1 > D2) - 0.5) / 0.5].
#' @param direction A number that equals -1 or 1. The direction specifies the whether D1 or D2 was the greater distribution with the greater value. It is output when p(D1 > D2) is calculated with the bootstrap.
#' @return Directional Overlap
#' @keywords overlap meanP p(D1 > D2) directional overlap
#' @export
#' @examples ch.overlapToDirOv (0.5, 1)

ch.overlapToDirOv <- function(overlap, direction) {
  if(!(direction %in% c(-1,1)) ) {
    stop("Direction must equal 1 or -1")
  }
  #change direction if overlap is an odd number
  if(overlap > 1) {
    direction <- direction * -1
  }
  #convert overlap to p to get rid of overlaps greater than 1
  pIn <- ch.overlapToP(overlap)
  #convert p back to overlap
  ovTmp <- ch.pToOverlap(pIn)
  #now calculate directional overlap
  dirOv <- abs(ovTmp - 1) * direction
  return(dirOv)
}
