\name{BasicHTML}
\alias{BasicHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ creates a basic HTML page displaying plots and annotations }
\description{
  Creates a basic HTML page displaying plots and annotations that can easily be navigated. The plots can be created either 'on the fly' by passing the appropriate commands or beforehand in which case just the filenames need to be passed.
}
\usage{
BasicHTML(cmds = NULL, HTMLobjects, Captions, MenuLabels, Comments,
         file = "tmp.html", title = "", width = 480, height = 480, 
         FRAMES = FALSE, JSCPATH = "jsc", LaunchPage = FALSE, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{cmds}{ list of commands that generates the plots. If missing, the graphfiles are assumed to exist already.}
   \item{HTMLobjects}{ list of graph filenames, either to be created by the list of commands or to be copied to the Figures subdirectory and/or dataframes to be displayed in sortable tables. }
  \item{Captions}{ vector of captions; these go directly below the graphs  }
  \item{MenuLabels}{ vector of labels for the main page.}
  \item{Comments}{ Text/comments to be written between the graphs }
  \item{file}{ file name of main page; '.html' extension will be added. The '_main' and '_menu' pages use this base as well. }
  \item{title}{ title to be written in the navigation/menu page }
  \item{width}{ width for all graphfiles }
  \item{height}{ height for all graphfiles }
  \item{FRAMES}{ is this an HTML page with frames ? }
  \item{JSCPATH}{ path that should contain the jsc components. If non existing, user will be prompted for installation. }
  \item{LaunchPage}{ launch the page ?}
  \item{verbose}{ level of verbosity}
  
}
\details{
  }
\value{
}
\references{  }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com>  }
\note{}
\seealso{ \link{FramedHTML} }
\examples{
	#example with plots and graphfiles being generated on the fly:
	BasicHTML(cmds = list("plot(rnorm(100));","plot(1:10);"), HTMLobjects = list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"),  MenuLabels = c("Marvel at the graph below","scatterplots are nice"), title="Test Page",width=480, height=480, verbose=1, JSCPATH = NULL)
	
	#example with plots and graphfiles having been generated beforehand:
	png("Fig1.png");
	plot(rnorm(100));
	dev.off()
	png("Fig2.png");
	plot(1:10);
	dev.off();
	
	BasicHTML( HTMLobjects = list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"), 
	MenuLabels = c("Marvel at the graph below","scatterplots are nice"), title="Test Page",
	width=480, height=480, verbose=1, JSCPATH = NULL);
	
	#example with absolute paths for graphfiles :
	Fig1 <- paste(tempdir(),"/Fig1.png",sep="")
	png(Fig1);
	plot(rnorm(100));
	dev.off()
	Fig2 <- paste(tempdir(),"/Fig2.png",sep="")
	png(Fig2);
	plot(1:10);
	dev.off();
	
	BasicHTML( HTMLobjects = list(Fig1, Fig2), Captions=c("Gaussian noise","seq 1:10"),  
	MenuLabels = c("Marvel at the graph below","scatterplots are nice"), title="Test Page",
	width=480, height=480, verbose=1, JSCPATH = NULL);
	#cleanup:
	system(paste("rm ", Fig1));system(paste("rm ", Fig2))

 #example with sorted table:
 \dontrun{x <- cbind.data.frame(x1 = round(rnorm(10),3), x2 = round(runif(10),3));}
 \dontrun{attr(x, "HEADER") <- "some random numbers";}
 \dontrun{BasicHTML(HTMLobjects = list("Fig1.png", x, "Fig2.png"), 
 	Captions=c("Gaussian noise","Gaussian and uniform random numbers", "seq 1:10"), 
 	file = paste(Sys.getenv("HOME"), "/public_html/tmp/tmp.html",sep=""), JSCPATH = "../jsc");}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
