\name{mortalitymale}
\docType{data}
\alias{mortalitymale}
\title{Age-specific mortality rates (males)}
\description{
A tab-separated text-file containing age-specific male mortality rates for a defined 
calendar period.
}
\usage{mortalitymale}
\format{A matrix.

\itemize{
\item{(a) The first row:}{ file header consisting
of the keyword \samp{age} and the years for which mortality rates 
are available. \cr For example, it could look like
\tabular{rrrrrr}{
\samp{age} \tab \samp{1954} \tab \samp{1956} \tab \samp{1957} \tab ...  \tab 
\samp{2007} \cr 
}
} 
\item{(b) The first column:}{ age (e.g. from 0 to 100)}
\item{(c) The remaining columns:}{ age-specific male mortality rates for each calender year}
} 
}

\source{Statistics Sweden home-page:
\emph{http://www.scb.se}
}
\keyword{datasets}
