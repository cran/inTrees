\name{Num2Level}
\alias{Num2Level}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal function
}
\description{
internal function
}
\usage{
Num2Level(rfList, splitV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rfList}{
%%     ~~Describe \code{rfList} here~~
}
  \item{splitV}{
%%     ~~Describe \code{splitV} here~~
}
}
\value{
  data frame with numeric variables converted to categorical variables. 
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
% \references{
% %% ~put references to the literature/web site here ~
% }
% \author{
% %%  ~~who you are~~
% }
% \note{
% %%  ~~further notes~~
% }

% %% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (rfList, splitV) 
{
    for (i in 1:rfList$ntree) {
        rfList$list[[i]] <- data.frame(rfList$list[[i]])
        rfList$list[[i]][, "prediction"] <- data.frame(dicretizeVector(rfList$list[[i]][, 
            "prediction"], splitV))
        colnames(rfList$list[[i]]) <- c("left daughter", "right daughter", 
            "split var", "split point", "status", "prediction")
    }
    return(rfList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ internal }
