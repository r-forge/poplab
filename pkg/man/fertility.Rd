\name{fertility}
\docType{data}
\alias{fertility}
\title{Age-specific female fertility rates}
\description{
A tab-separated text-file containing age-specific female fertility rates for a defined calendar period.
}
\usage{fertility}
\format{A matrix.

\itemize{
\item{(a) The first row:}{ file header consisting
of the keyword \samp{age} and the years for which fertility rates 
are available. \cr For example, 
\tabular{rrrrrr}{
\samp{age} \tab \samp{1954} \tab \samp{1956} \tab \samp{1957} \tab ...  \tab \samp{2007} \cr 
}
} 
\item{(b) The first column:}{ age (e.g. from 0 to 100)}
\item{(c) The remaining columns:}{ age-specific fertility rates for calender year}
} 
}

\source{Statistics Sweden home-page:
\emph{http://www.scb.se}
}
\keyword{datasets}
