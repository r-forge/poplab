\name{create.baseline.complete}
\alias{create.baseline.complete}
\title{Create a baseline population of related individuals}

\description{
Create a baseline population for a given calendar year. Requires fertility and 
mortality rates, and age-profile for the specified year in order to create a 
population of related individuals of the correct age profile. Incidence of an 
event of interest (e.g., disease) is simulated when desired.
  
The function looks in the folder specified in its call for 7 input data files: 
age-specific population counts for males and females separately, female 
fertility, mortality rates for males and females separately, and incidence rates 
(e.g., of a disease) for males and females separately. For gender
specific conditions or diseases only 6 files are needed, that is, only one 
file with incidence rates.
}

\usage{
create.baseline.complete(baseyear, base.scale, runintime, population.male,
population.fem,fertility,incidence.male,incidence.fem,mortality.male,mortality.fem,
healthy,sex.a,fam.rel,d.mod, risk, mortratio, print.option, seed, folder)
}

\arguments{
\item{baseyear}{year for which to create a baseline population.}
\item{base.scale}{this factor multiplied by 100 is the sex-specific number of 
individuals in the starting population. The default is 500 i.e. 50000 females 
and approximately 50000 males in the starting population (see Details below).}
\item{runintime}{number of years the \code{baseyear} real population age profile 
is set back in time (see Details below). The default is 100.}
\item{population.male}{name (quoted) of the tab-separated file containing 
age-specific counts from the real male population (e.g. 
\samp{"populationmale.txt"}).}
\item{population.fem}{name (quoted) of the tab-separated file containing 
age-specific counts from the real female population (e.g. 
\samp{"populationfem.txt"}).}
\item{fertility}{name (quoted) of the tab-separated file containing age-specific female 
fertility rates 
(e.g. \samp{"fertility.txt"}).}
\item{incidence.male}{name (quoted) of the tab-separated file containing age-specific 
male disease incidence rates 
(e.g. \samp{"incidence.male.txt"}).}
\item{incidence.fem}{name (quoted) of the tab-separated file containing age-specific 
female disease incidence rates 
(e.g. \samp{"incidence.fem.txt"}).}
\item{mortality.male}{name (quoted) of the tab-separated file containing 
age-specific male mortality rates (e.g. \samp{"mortalitymale.txt"}).}
\item{mortality.fem}{name (quoted) of the tab-separated file containing 
age-specific female mortality rates (e.g. 
\samp{"mortalityfem.txt"}).}
\item{healthy}{0/1 values - incidence of event of interest simulated/not simulated 
during the creation of the baseline population. The default is 1 (without event of interest).}
\item{sex.a}{1/2/3 values - gender specification (1 = males, 2 = females, 3 = 
for both males and females) for familial aggregation of disease; used only if 
\code{healthy} is set to 0. The default is 2 (mother/daughter or sister/sister 
disease aggregation).}
\item{fam.rel}{familial relationship of disease aggregation to choose from (see 
Details below); used only if \code{healthy} is set to 0.}
\item{d.mod}{disease model for familial aggregation (see Details 
below); used only if \code{healthy} is set to 0.}
\item{risk}{parameter for familial aggregation of event of interest (e.g., disease); 
used only if \code{healthy} is set to 0. The default is 1 (i.e. no familial aggregation). 
See Details below.}
\item{mortratio}{mortality ratio for cases compared to disease free individuals. 
The default is 1 (i.e. cases have the same risk of death as disease free 
individuals).}
\item{print.option}{TRUE/FALSE values - option for saving the baseline 
population to file . The default is TRUE.}
\item{seed}{seed used in the creation of the population. The default is 
\samp{NULL}. Useful when the user wants to reproduce the exact same simulation 
results (population).}
\item{folder}{folder (quoted) where the function looks for input files such as 
population counts, mortality, fertility and incidence rates. For example, it 
may be  
R_HOME/library/poplab/extdata/ directory, where R_HOME is the 
location where R is installed on your computer (see Examples below).}
}

\value{The matrix containing the baseline population with columns "ID" "yob" "sex" "m" "f" "yod" & "yoi", 
i.e.,identity number, year of birth, gender, mother's identity number, father's identity number, year of 
death and year of incidence, respectively}

\details{
\code{base.scale} the exact number of male individuals is calculated in such a 
way as to maintain the ratio between the number of male individuals and the
number of female individuals as in the real population.

\code{runintime} to create a baseline population of related
individuals for the \code{baseyear}, a population of unrelated individuals
(the starting population) is constructed for the calendar year calculated
as \samp{(baseyear - runintime)}; this starting population has the age profile
of the \samp{baseyear} real population. For the number of years specified
by \code{runintime}, the \code{baseyear} fertility and mortality rates are 
applied to this starting population and their descendants, "arriving" in the 
baseyear with a complete population of related individuals.

\code{d.mod} the options to choose from are: familial relative risk ("rr"),
familial odds-ratio ("or"), familial relative risk depending on the age at
incidence of the affected relative ("agesprr") and familial odds-ratio
depending on age at incidence of the affected relative ("agespor").

\code{fam.rel} the options to choose from are: familial aggregation occurs
through a parent ("p") or familial aggregation occurs thorough a sibling ("s"). 

\code{risk} if this is a scalar (i.e., a single value), it is expected that  
\code{d.mod} is either "rr" or "or". For the familial models of disease where 
the risk and the odds, respectively, depend on the age at incidence of the 
affected relative, a numerical vector is specified. The first part of the vector 
represents the upper limit of the age categories and the last part are the 
relative rates. The age-intervals are open to the left (see 
also Examples).  
}

\examples{

\dontrun{
The syntax below constructs the baseline population for the calendar year 1954 
(baseyear). It applies the 1954 fertility and mortality rates for 100 years 
(runintime) to the virtual population of 1854 constructed from the 1954 age 
profile; the 1854 population consists of 50000 females and approximately
50000 males (base.scale=500), all unrelated individuals. Incident acute appendicitis 
is assigned during the creation (healthy=0). The mortality ratio for cases compared 
to non-cases is 1 (mortratio=1). The resulting population is saved to file 
(print.option=TRUE). The input files are "populationmale.txt", "populationfem.txt",  
, "fertility.txt", "incidence.male.txt", "mortalitymale.txt", "incidence.fem.txt" and  
"mortalityfem.txt" from the "extdata" folder of the poplab package.}


base <- 
create.baseline.complete (baseyear = 1954, base.scale = 500, runintime = 100, 
population.male = "populationmale.txt", population.fem = "populationfem.txt",
fertility = "fertility.txt", incidence.male = "incidence.male.txt",
incidence.fem = "incidence.fem.txt",mortality.male  = "mortalitymale.txt",
mortality.fem = "mortalityfem.txt",healthy = 0,sex.a = 3, fam.rel = "p", 
d.mod = "rr", risk = 2, mortratio = 1, print.option = TRUE, seed =19780720, 
folder = system.file("extdata", package = "poplab"))


\dontrun{
In the syntax below the familial relative risk (d.mod) depends on parental 
(fam.rel) age at incidence: the risk of disease (risk) is increased by a factor of 
2 for men and women whose parent(s) where younger than 40 years of age 
at diagnosis and by a factor of 1.5 for men and women whose parent(s) where older 
than or equal 40 years of age, compared to men and women with unaffected parents 
younger than 40 years of age and older than or equal to 40 years of age, respectively. 

base <- 
create.baseline.complete (baseyear = 1954, base.scale = 500, runintime = 100, 
population.male = "populationmale.txt", population.fem = "populationfem.txt",
fertility = "fertility.txt", incidence.male = "incidence.male.txt",
incidence.fem = "incidence.fem.txt",mortality.male  = "mortalitymale.txt",
mortality.fem = "mortalityfem.txt", healthy = 0,sex.a = 3, fam.rel = "p", 
d.mod = "rr", risk = c(40, 101, 2, 1.5), mortratio = 1, print.option = TRUE, 
seed =19780720, folder = system.file("extdata", package = "poplab"))
}

}

\note{
If a call to this function is made without assigning its value to a variable,
the result will be printed on the screen (most probably, a very large matrix!).
Thus it is advisable to always assign the object returned by this function to a
variable.
}

\seealso{
\code{\link[poplab]{simped}}, \code{\link[poplab]{plot.poplab}},
\code{\link[poplab]{print.poplab}}
}

\keyword{datagen}
