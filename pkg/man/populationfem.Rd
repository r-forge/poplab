\name{populationfem}
\docType{data}
\alias{populationfem}
\title{Age-specific population counts (females)}
\description{
A tab-separated text-file containing female age-specific population counts for a 
defined calendar period.
}
\usage{populationfem}
\format{A matrix.

\itemize{
\item{(a) The first row:}{ file header consisting
of the keyword \samp{age} and the years for which population data 
are available. \cr For example,
\tabular{rrrrrr}{
\samp{age} \tab \samp{1954} \tab \samp{1956} \tab \samp{1957} \tab ...  \tab 
\samp{2007} \cr 
}
} 
\item{(b) The first column:}{ age (e.g. from 0 to 100)}
\item{(c) The remaining columns:}{ age-specific population counts  for each calender year}
} 
}

\source{Statistics Sweden home-page:
\emph{http://www.scb.se}
}
\keyword{datasets}