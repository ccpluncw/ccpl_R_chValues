% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ch.valuesAddReversedItemOrder.r
\name{ch.valuesAddReversedItemOrder}
\alias{ch.valuesAddReversedItemOrder}
\title{A function to add the reversed order of Item1 and Item2 in the values/morals datasets}
\usage{
ch.valuesAddReversedItemOrder(data, item1Col, item2Col, dirOverlapCol)
}
\arguments{
\item{data}{morals dataframe.}

\item{item1Col}{a string that specifies the name of the column in "data" that contains one of the items in the morals task.}

\item{item2Col}{a string that specifies the name of the column in "data" that contains the comparison item in the morals task.}

\item{dirOverlapCol}{a string that specifies the name of the column in "data" that contains the directional Overlap for the item in each trial.}
}
\value{
the dataset with the added reversed order of Item1 and Item2.  Because this dataset doubles the actual data (all A-B item combinations are duplicated to be B-A combinations as well), do not use it for analysis unless you keep this in mind. The duplication is sometimes needed because the overlap dataset only outputs one of these orders. The data will add a "probeOrder" column that states whether the order of Item1 and Item2 are "original" or "reversed."
}
\description{
This function adds the reversed order of Item1 and Item2 in the values/morals datasets.
}
\examples{
ch.valuesAddReversedItemOrder (data=myData, "Item1", "Item2", "dirOverlap")
}
\keyword{add}
\keyword{item}
\keyword{morals}
\keyword{order}
\keyword{reverse}
\keyword{values}
