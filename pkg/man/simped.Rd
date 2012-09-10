\name{simped}
\alias{simped}
\title{Simulate the evolution of a population of related individuals over a 
calendar period}

\description{
Simulate the evolution of a baseline population of related individuals over
a specified calendar period. Requires the period-specific fertility, mortality
and disease incidence rates, and the real population age profile for the first 
simulated year.

The function looks in the folder specified in its call for 8 input data files: 
age-specific population counts for males and females separately, female 
fertility, mortality rates for males and females separately, disease (e.g. 
acute appendicitis) incidence rates for males and females separately and the 
simulated baseline population (see \code{\link[poplab]{create.baseline.complete}}, 
\code{\link[poplab]{print.poplab}}). For diseases or conditions which affect only 
one gender (e.g., breast cancer), 7 files are sufficient, that is, there is only 
one incidence file for sex.a = 1 (males) or sex.a = 2 (females). 
}

\usage{
simped (baseyear, simyears, endyear, population.male,population.fem,fertility,
incidence.male,incidence.fem,mortality.male,mortality.fem,healthy,sex.a,fam.rel,
d.mod, famrisk, mortratio,print.option,seed, folder, name.base)
}

\arguments{
\item{baseyear}{the baseline year, needed to load the baseline population}
\item{simyears}{number of years to be simulated.}
\item{endyear}{last calendar year to be simulated.}
\item{population.male}{name (quoted) of the tab-separated file containing 
age-specific counts from the real male population (e.g. 
\samp{"populationmale.txt"}).}
\item{population.fem}{name (quoted) of the tab-separated file containing 
age-specific counts from the real female population (e.g. 
\samp{"populationfem.txt"}).}
\item{fertility}{name (quoted) of the tab-separated file containing age-specific female 
fertility rates (e.g. \samp{"fertility.txt"}).}
\item{incidence.male}{name (quoted) of the tab-separated file containing age-specific 
male incidence rates (e.g. \samp{"incidence.male.txt"}).}
\item{incidence.fem}{name (quoted) of the tab-separated file containing age-specific 
female incidence rates (e.g. \samp{"incidence.fem.txt"}).}
\item{mortality.male}{name (quoted) of the tab-separated file containing 
age-specific male mortality rates (e.g. \samp{"mortalitymale.txt"}).}
\item{mortality.fem}{name (quoted) of the tab-separated file containing 
age-specific female mortality rates (e.g. \samp{"mortalityfem.txt"}).}
\item{healthy}{0/1 values - disease incidence simulated/not simulated during the 
evolution of the population. The default is 1.}
\item{sex.a}{1/2/3 values - gender specification (1 = males, 2 = females, 3 = 
for both males and females) for familial aggregation of disease; used only if 
\code{healthy} is set to 0. The default is 2 
(mother/daughter or sister/sister disease aggregation).}
\item{fam.rel}{familial relationship of disease aggregation to choose from (see 
Details below); used only if \code{healthy} is set to 0.}
\item{d.mod}{familial model of disease aggregation to choose from (see Details 
below); used only if \code{healthy} is set to 0.}
\item{famrisk}{parameter for familial aggregation of disease; used only if 
\code{healthy} is set to 0. The default is 1 (i.e. no familial aggregation). 
See Details below.}
\item{mortratio}{mortality ratio for cases compared to disease free individuals. 
The default is 1 (i.e. cases have the same risk of death as disease free 
individuals).}
\item{print.option}{TRUE/FALSE values - option for saving the simulated pedigree 
to file. The default is TRUE.}
\item{seed}{seed used in the creation of the population. The default is 
\samp{NULL}. Useful when the user wants to reproduce the exact same simulation 
results (population).}
\item{folder}{folder (quoted) where the function looks for input files such as 
population counts, mortality, fertility and incidence rates. For example, it 
might be your R_HOME/library/poplab/extdata/
directory, where R_HOME is the location where R is installed on your computer 
(see Examples below).}
\item{name.base}{Ending string of the file name storing the baseline population. 
The default is "". See Details below.}
}

\details{
\code{baseyear}, \code{simyears} and \code{endyear}{ gives the calendar interval
to be simulated; the user may specify any combination of 2 out of these 3 
arguments and omit the other argument.}

The function will first identify if a virtual baseline population has been 
created and saved to file, by searching into the specified \code{folder}
for a file named, for example, \samp{base\_pop\_yyyy.txt} where \samp{yyyy} is the 
\code{baseyear}. \samp{base\_pop\_yyyy.txt} is the name format that the 
\code{\link[poplab]{print.poplab}} function uses to save a virtual baseline population 
to file, after its creation with the \code{\link[poplab]{create.baseline.complete}} 
function. This name format should not be used for other purposes. If the baseline population is found, the 
\code{\link[poplab]{simped}} function reads in this population and simulates 
further its evolution. If it is not found, a error message is printed i.e., 
\samp{object basepop not found}. There is a need to evoke the \code{\link[poplab]{create.baseline.complete}}
function prior to running \code{\link[poplab]{simped}}.

Immediately after reading in the baseline population, a plot of the 
simulated baseline age profile versus the real population age profile for the 
baseyear year is displayed and the function starts simulating the evolution of 
this population over the specified calendar period.

\code{d.mod}{ the options to choose from are: familial relative risk ("rr"),
familial odds-ratio ("or"), familial relative risk depending on the age at
incidence of the affected relative ("agesprr") and familial odds-ratio
depending on age at incidence of the affected relative ("agespor").
}

\code{fam.rel}{ the options to choose from are: familial aggregation occurs
through a parent ("p") and familial aggregation occurs thorugh a sibling ("s"). 
}

\code{famrisk}{ if this is a single value, it is expected 
that the value for \code{d.mod} is either "rr" or "or". For the familial models 
of disease where the risk and the odds, respectively, depend on the age at 
incidence of the affected relative, a numerical array is specified. The first 
part of the array represents the upper limit of the age categories and the last 
part are the actual coefficients of increase. The age-intervals are open to the 
left (see also Examples).}

\code{name.base}{ When reading in the simulated baseline population the default 
of the file name is \samp{base\_pop\_yyyy.txt} (where yyyy is the baseline year). 
Other specifications (e.g., "fam\_risk2") should be included in this parameter.}
}
\value{
A 2-component list. The first component (matrix) is the baseline population. The second component 
(matrix) is the evolved population. 
}

\examples{

\dontrun{
The syntax below will search into the specified folder for a file called 
'base_pop_1954.txt' (see Details). If the file is found, it is read-in as the 
baseline population otherwise an error message will be printed.

Next the evolution of the baseline population is simulated over the calendar 
period 1954 - 2007 ([baseyear, endyear]). Incident acute appendicitis is 
assigned (healthy=0) to the entire population (sex.a=3) via a model of familial 
relative risk (d.mod="rr"), where the risk ratio for people with an affected parents 
compared to those with both unaffected parents is 2 (fam.rel="p", famrisk=2). 
The mortality ratio for cases is 1 (mortratio=1). The resulting pedigree is saved to 
file (print.option+TRUE). 
}


simpop <- 
simped (baseyear = 1954, endyear = 2007, population.male = "populationmale.txt",
population.fem = "populationfem.txt", fertility = "fertility.txt",
incidence.male = "incidence.male.txt", incidence.fem = "incidence.fem.txt",
mortality.male  = "mortalitymale.txt", mortality.fem = "mortalityfem.txt",
healthy = 0, sex.a = 3, fam.rel = "p", d.mod = "rr", famrisk = 2, 
mortratio = 1, print.option = TRUE, seed =19780720, 
folder = system.file("extdata", package = "poplab")) 

## resulting baseline population
base <- simpop[[1]]

## resulting evolved population
evolved <- simpop[[2]]

}



\note{
If a call to this function is made without assigning its value to a variable, 
the result will be printed on the screen (most probably, a very large matrix)! 
Thus it is advisable to always assign the object returned by this function to a 
variable.
}

\seealso{
\code{\link[poplab]{create.baseline.complete}}, \code{\link[poplab]{plot.poplab}},
\code{\link[poplab]{print.poplab}}
}

\keyword{datagen}
