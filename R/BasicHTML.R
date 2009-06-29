`BasicHTML` <-
function(cmds=NULL, HTMLobjects, Captions, MenuLabels, Comments, file="tmp.html", title="", width=480, height=480, FRAMES=FALSE, JSCPATH = "jsc", LaunchPage = FALSE, APPEND = FALSE, verbose=0){
	#BasicHTML(cmds = c("plot(rnorm(100));","plot(1:10);"), HTMLobjects = list("Fig1.png", "Fig2.png"), Captions=c("Gaussian noise","seq 1:10"))
	#x <- cbind.data.frame(x1 = round(rnorm(10),3), x2 = round(runif(10),3));
	#attr(x, "HEADER") <- "some random numbers";
	#BasicHTML(HTMLobjects = list("Fig1.png", x, "Fig2.png"), Captions=c("Gaussian noise","Gaussian and uniform random numbers", "seq 1:10"), file = paste(Sys.getenv("HOME"), "/public_html/tmp/tmp.html",sep=""), JSCPATH = "../jsc")
	
  require(R2HTML);
  #require(Cairo);
  graphIndex <- sapply(HTMLobjects,is.character);
  graphfiles <- unlist(HTMLobjects[graphIndex]);
  header <- NULL;
  if (!all(graphIndex)) {
  	header <- HTMLsortedTable(x=NULL, JSCPATH = JSCPATH);
  	if (!file.exists(paste(JSCPATH,sep="")))  InstallJSC(paste(JSCPATH,sep=""));
  }
  
  if (is.null(cmds)){#no cmds passed, so we assume that the plots have already been created !
  	DoNotPlot <- TRUE;
  } else  {
  	stopifnot(length(cmds)==length(graphfiles));
    DoNotPlot <- FALSE;
    CairoWorks();
  }
  
  if (verbose >1) browser();
  
  if(! FRAMES & !APPEND ) MyReportBegin(file=file, title = title, header = header);
  if (APPEND & FRAMES ){
  	tmp <- scan(paste(substring(file,1,nchar(file)-10), "_menu.html",sep=""), what = "");
  	ExistingLabels <- grep("#Num", tmp);
  	MenuNumber <- length(ExistingLabels) + 1;
  } else {
    MenuNumber <- 1;}
  
  for (i in seq(along= HTMLobjects)){
  	if (FRAMES){
  	  if (!is.null(Comments)) if (nchar(Comments[i])>0) HTML(Comments[i]);
  	  MenuLabel <- ""; 
  	  if (!missing(MenuLabels)) if (!is.null(MenuLabels)) if (length(MenuLabels) == length(HTMLobjects)) {MenuLabel <- MenuLabels[i]};
  	  HTML(paste("<a name=Num", MenuNumber,">&nbsp;</a><p><xmp class=command>", MenuLabel,"</xmp>",sep=""),file= file)
      MenuNumber = MenuNumber + 1;
    }#end of if (FRAMES)  
    #if (is.character(HTMLobjects[[i]]) ){
    if (graphIndex[i]){
      graphfile <- HTMLobjects[[i]];
  	  if (!DoNotPlot){
        CairoPNG(graphfile,width=width,height=height);
        if (verbose) cat("creating graph", graphfile,"\n")
	    eval(parse(text=cmds[i]));
	    dev.off()
	  }
	  HTMLInsertGraph(graphfile,Caption=Captions[i], Align="left",file=file,WidthHTML=width, HeightHTML=height);
	} else if (is.data.frame(HTMLobjects[[i]]) | is.matrix(HTMLobjects[[i]])) {
	  HEADER <- "";
	  try(HEADER <- attr(HTMLobjects[[i]],"HEADER"));
	  htable <- HTMLsortedTable(HTMLobjects[[i]], HEADER = HEADER, file = NULL, JSCPATH = JSCPATH);
	  HTML('<p align= center >', file = file);HTML(htable, file = file);HTML('<BR>', file = file);
	  #HTML(x, file = file)
	}
  }


 #for now I am not adding a clean end of html footer in order to keep the pages open for appending operations
 #most browsers seem to be able to handle this just fine...
 # if(! FRAMES) MyReportEnd(file=file);
  if (LaunchPage) system(paste("open ", file))
}

