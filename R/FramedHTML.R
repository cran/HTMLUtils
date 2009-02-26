`FramedHTML` <-
function(cmds=NULL, basepath = paste(Sys.getenv("HOME"), "/public_html/",sep=""), path="tmp", Graphpath = "Figures/", DiagnosticsPath = "Diagnostics", file="tmp", HTMLobjects, Captions=NULL, MenuLabels1=NULL, MenuLabels2=NULL, Comments=NULL, title="", width=480, height=480, verbose=1){
  #FramedHTML(cmds = c("plot(rnorm(100));","plot(1:10);"), HTMLobjects =list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"), MenuLabels1 = c("Label1","Label2"), MenuLabels2 = c("Marvel at the graph below","scatterplots are nice"), title="Test Page",verbose=1);
  #x <- cbind.data.frame(x1 = round(rnorm(10),3), x2 = round(runif(10),3));
  #FramedHTML(HTMLobjects =list(x,"Fig1.png", "Fig2.png"), Captions=c("jadejade","Gaussian noise","seq 1:10"), MenuLabels1 = c("sorted table","Label1","Label2"), MenuLabels2 = c("the magic of javascript","Marvel at the graph below","scatterplots are nice"), title="Test Page",verbose=1)

  
  #ArgNames <- names(formals(FramedHTML));
  #Args <- match.arg(ArgNames );
  #Args <- list();
  #return(Args)
  
  require(R2HTML);require(Cairo);  
  WD <- getwd();
  on.exit(setwd(WD))
  #The paths can be confusing as
  # (i)  the HTML file only wants relative paths, but Cairo wants absolute.
  # (ii) we don't know if the user passes the graphfiles with full, partial or no paths !
  #Solution: change current dir to the base path and assume that the graphfiles are just file names, no path info
  basepath <- makePathName(basepath, TRUE);
  setwd(basepath);
  path <- makePathName(path, TRUE);
  NoDirs <- sum("/" == unlist(strsplit(path,"")));#very fragile method of determining the number of subdirectories off the basepath !!
  setwd(path);
  Graphpath <- makePathName(Graphpath, TRUE);
   #If string, assume graphfile:
  graphIndex <- sapply(HTMLobjects,is.character);
  graphfiles <- unlist(HTMLobjects[graphIndex]);
 
  if (verbose > 1) browser();
  for (g in graphfiles) {
  	graphdir <- dirname(g);
  	gg <- basename(g);
  	if (graphdir == "." ) {ggg <- paste(WD, gg,sep="/");#no graphfile paths
  	} else if (substring(graphdir,1,1) != "/") {ggg <- paste(WD, g,sep="/");#relative graphfile paths 
  	} else {ggg <- g}#absolute paths
  	if (verbose) print(ggg);
    #if (file.exists(ggg) & !file.exists(paste(Graphpath, g,sep="/"))) {
    if (file.exists(ggg) ) {
    	if (verbose) cat("copying ", ggg, " to ", paste(basepath, path, Graphpath, gg, sep=""), "\n");
    	#system(paste("cp ", paste(WD, g,sep="/"), " ", Graphpath, g,sep="") );
    	system(paste("cp ", ggg, " ", paste(basepath, path, Graphpath, gg, sep="") ,sep="") );
    } else {cat(ggg, " does not exist, unable to copy !\n");}
  }
  graphfiles <- paste(Graphpath, basename(graphfiles),sep="");
  HTMLobjects[graphIndex] <- graphfiles;
  if (verbose) print(graphfiles)
  DiagnosticsPath <- makePathName(DiagnosticsPath, TRUE);
  #FullGraphpath <- makePathName(paste(path, Graphpath, sep =""), TRUE);
  #DiagnosticsPath <- makePathName(paste(path,DiagnosticsPath,sep=""), TRUE);
   JSCPATH <- paste(paste(rep("../", NoDirs),collapse=""), "jsc",sep="");
   if (substring(file,1,1) != "/") {outdir = "."} else {outdir = ""}
   
  targetBig <- myHTMLInitFile(outdir = outdir, file, HTMLframe =TRUE, NavTitle = title, Title = "", JSCPATH= JSCPATH, useLaTeX = FALSE)
  target <- targetBig["target"]
  target.menu <- targetBig["targetmenu"]
  target.main <- targetBig["targetmain"]
  file = target.main;
  HTMLCSS(file = file, CSSfile = "R2HTML");
  #HTML.title(as.title(title), HR=2, file=file);
  HTML.title(as.title(title), HR=2, file=target.menu);
  
  BasicHTML(cmds, HTMLobjects, Captions, MenuLabels2, Comments, file, title, width=width, height, FRAMES=TRUE, JSCPATH= JSCPATH, verbose=verbose);
  MenuNumber <- 1;
  for (i in seq(along= HTMLobjects)){
  	MenuLabel <- ""; 
  	  if (!missing(MenuLabels1)) if (!is.null(MenuLabels1)) if (length(MenuLabels1) == length(HTMLobjects)) {MenuLabel <- MenuLabels1[i]};
  	tmp <- paste("href='",target.main,"#Num", MenuNumber,"'",sep="");
    HTMLli(paste("<a class=command ", tmp," target=main> ", MenuLabel," </a>",sep=""),file= target.menu);
    MenuNumber = MenuNumber + 1;
  }
  
  CleanUpdevs();
  MyReportEnd(file=target.main);
  if (!is.null(WD)) setwd(WD);
  #return(targetBig)
}

