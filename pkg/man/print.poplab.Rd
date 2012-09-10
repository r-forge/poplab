\name{print.poplab}
\alias{print.poplab}
\title{Save to file a simulated population}

\description{
Save a simulated population to file in the folder specified in the function call. 
The name of the resulting file indicates whether a baseline or an evolved 
population was saved.  
} 

\usage{
\method{print}{poplab}(x, option, year, folder, \dots)
}

\arguments{
\item{x}{an object returned by the \code{\link[poplab]{create.baseline.complete}}
or the \code{\link[poplab]{simped}} function}
\item{option}{2 possible values \samp{"base"} or \samp{"current"} (see Details)}
\item{year}{last simulated year}
\item{folder}{folder (quoted) where the function saves the population. For 
example, it might be R_HOME/library/poplab/extdata/ directory, where R_HOME 
is the location where R is installed on your computer (see Examples below).}
\item{\dots}{further arguments for print command}
}

\details{
\code{year} the population simulated only up to this calendar \code{year} 
is saved (i.e. individuals born after this \code{year} are excluded).

\code{option} which simulated population should be saved to
file? \cr
\samp{"base"} specifies a virtual baseline population (either an object
returned by the \code{\link[poplab]{create.baseline.complete}} function
or the first list component returned by the \code{\link[poplab]{simped}}
function). \cr
\samp{"current"} specifies an evolved population simulated up to the calendar
year set by \code{year}; this population is the second list component returned
by the \code{\link[poplab]{simped}} function. 
}

\examples{

\dontrun{
For these examples it is necessary to use the simped function and assign the 
result to a variable called 'simpop'.
}

## simulate the population for the calendar period 1954-2007:

simpop <- 
simped (baseyear = 1954, endyear = 2007, population.male = "populationmale.txt",
population.fem = "populationfem.txt", fertility = "fertility.txt",
incidence.male = "incidence.male.txt", incidence.fem = "incidence.fem.txt",
mortality.male  = "mortalitymale.txt", mortality.fem = "mortalityfem.txt",
healthy = 0, sex.a = 3, fam.rel = "p", d.mod = "rr", famrisk = 2, mortratio = 1, 
print.option = FALSE, seed =19780720, 
folder = system.file("extdata", package = "poplab")) 


## save the population simulated up to the calendar year 1990 (although the 
## simped function simulated the population up to the year 2007); the name of
##  the created file will be 'simpop_endYr_1990.txt'
print.poplab(simpop, "current", year = 1990, folder = system.file("extdata", 
package = "poplab"))

## save the 1954 baseline population to a file named "base_pop_1954.txt"
print.poplab(simpop, "base", year = 1954, folder =
system.file("extdata", package = "poplab"))

}

\note{
An error will be generated when attempting to save an object returned by the 
\code{\link[poplab]{create.baseline.complete}} function (the value of the 
\code{pop} argument) with \code{option} set to \samp{"current"}.
}

\seealso{
\code{\link[poplab]{create.baseline.complete}}, \code{\link[poplab]{simped}},
\code{\link[poplab]{plot.poplab}}
}

\keyword{methods}
