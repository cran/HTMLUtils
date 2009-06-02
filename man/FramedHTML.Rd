\name{FramedHTML}
\alias{FramedHTML}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ creates a framed HTML page displaying plots and annotations}
\description{
  Creates a framed HTML page displaying plots and annotations that can easily be navigated. The plots can be created either 'on the fly' by passing the appropriate commands or beforehand in which case just the filenames need to be passed.
  The user has a great deal of flexibility in choosing appropriate directory structures.
}
\usage{
FramedHTML(cmds = NULL, basepath = paste(Sys.getenv("HOME"), "/public_html/", sep = ""), 
      path = "tmp", Graphpath = "Figures/", DiagnosticsPath = "Diagnostics", 
      file = "tmp", HTMLobjects, Captions=NULL, MenuLabels1=NULL, MenuLabels2=NULL, Comments=NULL,
      title = "", width = 480, height = 480, REFRESH = "", verbose = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cmds}{ list of commands that generates the plots. If missing, the graphfiles are assumed to exist already.}
  \item{basepath}{ base path of \samp{public_html} directory }
  \item{path}{ subdirectory of \code{basepath}; will be created if non existing  }
  \item{Graphpath}{ subdirectory of \samp{basepath/path/} containing the graphfiles; will be created if non existing }
  \item{DiagnosticsPath}{ subdirectory of \samp{basepath/path/} containing diagnostic output; will be created if non existing }
  \item{file}{ file name of main page; '.html' extension will be added. The '_main' and '_menu' pages use this base as well. }
  \item{HTMLobjects}{ list of graph filenames, either to be created by the list of commands or to be copied to the Figures subdirectory and/or dataframes to be displayed in sortable tables.}
  \item{Captions}{ vector of captions; these go directly below the graphs  }
  \item{MenuLabels1}{ vector of labels for the menu navigation page. It helps to keep these succinct and short ! }
  \item{MenuLabels2}{ vector of labels for the main page; these go on top of the individual graphs, so they are complementary to the captions. }
  \item{Comments}{ Text/comments to be written between the graphs }
  \item{title}{ title to be written in the navigation/menu page }
  \item{width}{ width for all graphfiles }
  \item{height}{ height for all graphfiles }
  \item{REFRESH}{Meta refresh is a method of instructing a web browser to automatically refresh the current web page after a given time interval. }
  \item{verbose}{ level of verbosity}
}
\details{
  
}
\value{
  }
\references{ }
\author{ Markus Loecher, Sense Networks <markus@sensenetworks.com> }
\note{ 
	There is not much eror checking. In particular, the lengths of the arguments\code{cmds, graphfiles, Captions, MenuLabels1, MenuLabels2} need to be all the same !
}
\seealso{ \link{BasicHTML} }
\examples{
	#example with plots and graphfiles being generated on the fly:
	FramedHTML(cmds = list("plot(rnorm(100));","plot(1:10);"), HTMLobjects =list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"), MenuLabels1 = c("Label1","Label2"), MenuLabels2 = c("Marvel at the graph below","scatterplots are nice"), Comments  = c("100 random numbers","Simple plot"), title="Test Page",width=480, height=480, verbose=1)
	
	#example with plots and graphfiles having been generated beforehand:
	png("Fig1.png");
	plot(rnorm(100));
	dev.off()
	png("Fig2.png");
	plot(1:10);
	dev.off();
	
	FramedHTML( HTMLobjects = list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"), 
	MenuLabels1 = c("Label1","Label2"), MenuLabels2 = c("Marvel at the graph below","scatterplots are nice"), 
	Comments  = c("100 random numbers","Simple plot"), title="Test Page",width=480, height=480, verbose=1);
	
	#example with absolute paths for graphfiles :
	Fig1 <- paste(tempdir(),"/Fig1.png",sep="")
	png(Fig1);
	plot(rnorm(100));
	dev.off()
	Fig2 <- paste(tempdir(),"/Fig2.png",sep="")
	png(Fig2);
	plot(1:10);
	dev.off();
	
	FramedHTML( HTMLobjects = list(Fig1, Fig2), Captions=c("Gaussian noise","seq 1:10"), 
	 MenuLabels1 = c("Label1","Label2"), MenuLabels2 = c("Marvel at the graph below","scatterplots are nice"), 
	 Comments  = c("100 random numbers","Simple plot"), title="Test Page",width=480, height=480, verbose=1);
	#cleanup:
	system(paste("rm ", Fig1));system(paste("rm ", Fig2))
	
	 #example with sorted table:
 \dontrun{x <- cbind.data.frame(x1 = round(rnorm(10),3), x2 = round(runif(10),3));}
 \dontrun{attr(x, "HEADER") <- "some random numbers";}
 \dontrun{FramedHTML(HTMLobjects = list("Fig1.png", x, "Fig2.png"), 
 	MenuLabels1 = c("Label1","Label2","Label3"), MenuLabels2 = c("Marvel at the graph below","JavaScript rocks","scatterplots are nice"),
 	Captions=c("Gaussian noise","Gaussian and uniform random numbers", "seq 1:10"),
 	 path = "tmp", file = "index");}
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

