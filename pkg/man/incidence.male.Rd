\name{incidence.male}
\docType{data}
\alias{incidence.male}
\title{Age-specific incidence rates of disease (males)}
\description{
A tab-separated text-file containing age-specific male incidence rates for a defined 
calendar period.
}
\usage{incidence.male}
\format{A matrix.

\itemize{
\item{(a) The first row:}{ file header consisting
of the keyword \samp{age} and the years for which incidence rates 
are available. \cr For example, 
\tabular{rrrrrr}{
\samp{age} \tab \samp{1954} \tab \samp{1956} \tab \samp{1957} \tab ...  \tab 
\samp{2007} \cr 
}
} 
\item{(b) The first column:}{ age (e.g. from 0 to 100)}
\item{(c) The remaining columns:}{ age-specific male incidence rates of disease (or event of interest) for each calender year}
} 
}

\source{The National Board of Health and Welfare home-page:
\emph{http://www.socialstyrelsen.se/english/}
}
\keyword{datasets}