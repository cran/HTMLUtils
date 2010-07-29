\name{HTMLsortedTable}
\alias{HTMLsortedTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{create sortable table }
\description{create sortable table using JavaScript components in \code{JSCPATH} directory }
\usage{
HTMLsortedTable(x, TITLE = "", HEADER = "", file = "tmp.html", path = paste(Sys.getenv("HOME"), "/public_html/", sep = ""), JSCPATH = "jsc", debug = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data frame or matrix with column names }
  \item{TITLE}{title for the HTML page }
  \item{HEADER}{ header to display for the sorted table }
  \item{file}{ file to write to }
  \item{path}{ directory to create the file in }
  \item{JSCPATH}{ directory that contains the javascript components}
  \item{debug}{ level of verbosity}
}
\details{ }
\value{  }
\references{   }
\author{  Markus Loecher, Sense Networks <markus@sensenetworks.com>  }
\note{ }
\seealso{  }
\examples{ }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
