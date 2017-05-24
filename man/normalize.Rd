\name{normalize}
\alias{normalize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
normalize function ... 
}
\description{
This function normalizes all the values to scale 10 ~
}
\usage{
normalize(df, n = 10, rename = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
agata dataframe with ertrag and durchsatz columns
}
  \item{n}{
Grid scale
}
  \item{rename}{
if rename is ture, then each column name will be appended with a surfix "_norm"~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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

## The function is currently defined as
function (df, n = 10, rename = TRUE) 
{
    df_norm = df
    df_max_ertrag <- max(df$ertrag, na.rm = TRUE)
    df_min_ertrag <- min(df$ertrag, na.rm = TRUE)
    df_norm$ertrag <- (df$ertrag - df_min_ertrag) * n/(df_max_ertrag - 
        df_min_ertrag)
    df_max_durchsatz <- max(df$durchsatz, na.rm = TRUE)
    df_min_durchsatz <- min(df$durchsatz, na.rm = TRUE)
    df_norm$durchsatz <- (df$durchsatz - df_min_durchsatz) * 
        n/(df_max_durchsatz - df_min_durchsatz)
    if (rename == TRUE) 
        colnames(df_norm) <- paste("norm", colnames(df_norm), 
            sep = "_")
    return(df_norm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
