\name{dataSimulate}
\alias{dataSimulate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulate data.
%%  ~~function to do ... ~~
}
\description{
This function simulates data. Team optimization data: choose a team with 10 players from 20 to play a game. The team would win if a) either player 1 or player 2 is in the team; and b) Player 1 and player 2 do not play together in the team. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dataSimulate(flag=1, nCol = 20, nRow = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{flag}{1: team optimization; 2: non-linear; 3: linear
%%     ~~Describe \code{flag} here~~
}
  \item{nCol}{the number of columns in the data set. must >= 2. 
%%     ~~Describe \code{nCol} here~~
}
  \item{nRow}{the number of rows in the data set.  
%%     ~~Describe \code{nRow} here~~
}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
	predictor variable matrix and target variable
}
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
res <- dataSimulate(flag=1)
X <- res$X; 
target <- res$target
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ simulate }