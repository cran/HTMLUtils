\name{HTMLsortedTable}
\alias{HTMLsortedTable}
\title{create sortable table }
\description{create sortable table using JavaScript components in \code{JSCPATH} directory}
\usage{HTMLsortedTable(x, TITLE = "", HEADER = "", file = "tmp.html", 
    JSCPATH = "jsc", path = paste(Sys.getenv("HOME"), "/public_html/", 
        sep = ""), debug = 0)}
\arguments{
  \item{x}{data frame or matrix with column names}
  \item{TITLE}{title for the HTML page}
  \item{HEADER}{header to display for the sorted table}
  \item{file}{file name of main page; '.html' extension will be added. The '\_main' and '\_menu' pages use this base as well.}
  \item{JSCPATH}{path that should contain the jsc components. If non existing, user will be prompted for installation.}
  \item{path}{directory to create the file in}
  \item{debug}{level of verbosity}
}



\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)" <markus.loecher@gmail.com>}





