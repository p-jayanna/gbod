\name{gbod}
\alias{gbod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Grid Based Outlier Detection
}
\description{
Grid Based Outlier Detection in Large Data Sets.
}
\usage{
gbod(df, n, outlierpercent)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
gata dataframe with ertrag and durchsatz columns , grid scale, and outlier percentage
}
  \item{n}{
Grid scale 
}
  \item{outlierpercent}{
outlier percentage
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Grid Based Outlier Detection in Large Data Sets
Ying Gu ∗ , Ram Kumar Ganesan ∗ , Benjamin Bischke ∗ , Alexander Maier † , Thilo Steckel ‡ , Heinrich Warkentin ‡
, Ansgar Bernardi ∗ and Andreas Dengel ∗
∗
German Research Center for Artificial Intelligence † Fraunhofer-Application Center Industrial Automation (IOSB-INA)
‡ CLAAS E-Systems KGaA mbH & Co KG
}
\author{
Ying Gu, Praveenkumar Jayanna
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

df_csv <- read.csv('/home/praveen/AGATA/gbod.csv', stringsAsFactors = F)
finaldataset<-gbod(df_csv,10,5)
outlierplot(finaldataset)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
