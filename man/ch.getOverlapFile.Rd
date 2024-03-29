% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.getOverlapFile.r
\name{ch.getOverlapFile}
\alias{ch.getOverlapFile}
\title{A function to extract a subset of overlaps from a file}
\usage{
ch.getOverlapFile(
  overlapFile = NULL,
  itemFile = NULL,
  outputFile = NULL,
  itemColumn1 = "item1vector",
  itemColumn2 = "item2vector"
)
}
\arguments{
\item{overlapFile}{a tab delimited file of a full set of overlaps with all the items.}

\item{itemFile}{a vector of items that you want to extract the overlaps for.}

\item{outputFile}{the filename that you want the output written to. DEFAULT = NULL (no file written)}

\item{itemColumn1}{the name of the column that contains one set of item values in the overlap. DEFAULT = "item1vector"}

\item{itemColumn2}{the name of the column that contains the second set of item values in the overlap. DEFAULT = "item2vector"}
}
\value{
a dataframe with the overlaps statistics for the items in itemFile
}
\description{
A function to extract a subset of overlaps from a file
}
\examples{
ch.getOverlapFile (Overlaps.txt, items.txt,output.txt)
}
\keyword{extract}
\keyword{file}
\keyword{overlap}
\keyword{values}
