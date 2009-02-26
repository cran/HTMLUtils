\name{myHTMLInitFile}
\alias{myHTMLInitFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Begins / Ends a new HTML report output }
\description{
  Those two functions handle the beginning and the ending of a HTML report, by writing the HTML <body><head><title></title></head>...</body> tags and their options. When working manually, the user may need to use it's own functions or to explicitly write to a file using cat("", file=).
}
\usage{
myHTMLInitFile(outdir = tempdir(), filename = "index", extension = "html", HTMLframe = TRUE, BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output", NavTitle = "", CSSFile = "R2HTML.css", useLaTeX = TRUE, useGrid = TRUE, img.logo.path = paste(Sys.getenv("HOME"), "/public_html/", sep = ""), img.logo = "logo-SenseNetworks.png", img.href = "http://www.sensenetworks.com", JSCPATH = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{outdir}{ directory to store the output }
  \item{filename}{ target HTML report filename }
  \item{extension}{ target HTML report extension (htm, html,...)  }
  \item{HTMLframe}{ should the output be handled by frames [boolean] }
  \item{BackGroundColor}{ option bgcolor for HTML tag <body>}
  \item{BackGroundImg}{ option background for HTML tag <body>}
  \item{Title}{ string to pass to HTML <title> tag  }
   \item{NavTitle}{ }
  \item{CSSFile}{ path and name of a CSS file to use }
  \item{useLaTeX}{ boolean - add required references to javascript AsciiMathML in order to use \code{as.latex}}
  \item{useGrid}{ boolean - add required references to javascript grid in order to use R2HTML grid fonctions}
  \item{img.logo.path}{ }
  \item{img.logo}{ }
  \item{img.href}{ }
  \item{JSCPATH}{ }
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
