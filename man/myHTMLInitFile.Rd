\name{myHTMLInitFile}
\alias{myHTMLInitFile}
\title{Begins / Ends a new HTML report output}
\description{Those two functions handle the beginning and the ending of a HTML report, by writing the 
HTML <body><head><title></title></head>...</body> tags and their options. When working manually, the user may need to use it's own functions or to explicitly write to a file using cat("", file=).}
\usage{myHTMLInitFile(outdir = tempdir(), filename = "index", extension = "html", 
    HTMLframe = TRUE, BackGroundColor = "FFFFFF", BackGroundImg = "", 
    Title = "R output", NavTitle = "", CSSFile = "R2HTML.css", 
    useLaTeX = TRUE, useGrid = TRUE, img.logo.path = paste(Sys.getenv("HOME"), 
        "/public_html/", sep = ""), img.logo = "logo-SenseNetworks.png", 
    img.href = "http://www.sensenetworks.com", JSCPATH = NULL, 
    APPEND = FALSE, REFRESH = "")}
\arguments{
  \item{outdir}{directory to store the output}
  \item{filename}{target HTML report filename}
  \item{extension}{target HTML report extension (htm, html,...)}
  \item{HTMLframe}{should the output be handled by frames [boolean]}
  \item{BackGroundColor}{option bgcolor for HTML tag <body>}
  \item{BackGroundImg}{option background for HTML tag <body>}
  \item{Title}{string to pass to HTML <title> tag}
  \item{NavTitle}{title of navigation page}
  \item{CSSFile}{path and name of a CSS file to use}
  \item{useLaTeX}{boolean - add required references to javascript AsciiMathML in order to use \code{as.latex}}
  \item{useGrid}{boolean - add required references to javascript grid in order to use R2HTML grid functions}
  \item{img.logo.path}{path to search for the logo pic in the frame}
  \item{img.logo}{filename of logo to display, if NULL no logo to display!}
  \item{img.href}{link of logo to point to.}
  \item{JSCPATH}{directory that contains the javascript code}
  \item{APPEND}{append to existing HTML page ?}
  \item{REFRESH}{Meta refresh is a method of instructing a web browser to automatically refresh the current web page after a given time interval.}
}



\author{"Markus Loecher, Berlin School of Economics and Law (BSEL)"  <markus.loecher@gmail.com>}





