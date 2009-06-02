`myHTMLInitFile` <-
function (outdir = tempdir(), filename = "index", extension = "html", 
    HTMLframe = TRUE, BackGroundColor = "FFFFFF", BackGroundImg = "", 
    Title = "R output", NavTitle = "", CSSFile = "R2HTML.css", useLaTeX = TRUE, 
    useGrid = TRUE, img.logo.path =paste(Sys.getenv("HOME"), "/public_html/",sep=""), img.logo = "logo-SenseNetworks.png", img.href= 'http://www.sensenetworks.com', JSCPATH = NULL, REFRESH = "") 
{
 	ret <- try(tmp <- system("uname",TRUE));
        if (class(ret) != "try-error")
	  if (tmp == "Linux") {#means we have a Web server
		img.logo.path = paste("/~", system("whoami",TRUE),"/",sep="");#"/~markus/"
	 }
	img.logo = paste(img.logo.path, img.logo, sep="")
    if (HTMLframe == FALSE) {
        file <- file.path(outdir, paste(filename, ".", extension, 
            sep = ""))
        assign(".HTML.file", file, env = .GlobalEnv)
        txt <- ifelse(useLaTeX, "<html xmlns:mml=\"http://www.w3.org/1998/Math/MathML\">", 
            "<html xmlns=\"http://www.w3.org/1999/xhtml\" \n  xml:lang=\"en\">")
        txt <- c(txt, "<head>")
        txt <- c(txt, paste("<title>", Title, "</title>"))
        txt <- c(txt, paste("<link rel=stylesheet href=\"", CSSFile, 
            "\" type=text/css>", sep = ""))
        if (useLaTeX) 
            txt <- c(txt, "<object id=\"mathplayer\" classid=\"clsid:32F66A20-7614-11D4-BD11-00104BD3F987\"></object>\n<?import namespace=\"mml\" implementation=\"#mathplayer\"?>\n<script type=\"text/javascript\" src=\"ASCIIMathML.js\"></script>")
        if (useGrid) {
            txt <- c(txt, HTMLgrid_references())
            txt <- c(txt, "<script>\n   nequations=0;\n</script>")
        }
        txt <- c(txt, "</head>")
        body <- c("<body")
        if (useLaTeX) 
            body = c(body, " onload=\"translate()\"")
        body = c(body, paste(" bgcolor=", BackGroundColor))
        if (!is.null(BackGroundImg)) 
            body = c(body, paste(" background=\"", BackGroundImg, 
                "\"", sep = ""))
        body <- c(body, " >")
        body = paste(body, collapse = "")
        txt <- c(txt, body)
        txt <- paste(txt, collapse = "\n")
        cat(txt, file = file, append = FALSE)
    }
    else {
        filemenu <- paste(filename, "_menu.", extension, sep = "")
        filemain <- paste(filename, "_main.", extension, sep = "")
        absfilemenu <- file.path(outdir, filemenu)
        file <- absfilemain <- file.path(outdir, filemain)
        absfileindex <- file.path(outdir, paste(filename, ".", 
            extension, sep = ""))
        #cat(filemenu, filemain, absfilemenu, file, absfileindex);
        assign(".HTML.file", absfilemain, env = .GlobalEnv);
        if (is.numeric(REFRESH)) REFRESH <- paste('<meta http-equiv="refresh" content="',REFRESH,'" />',sep="");
        cat(paste("<html><head>\t\n <title>", Title, "</title>\n <meta http-equiv=content-type content=text/html;charset=iso-8859-1>\n ", REFRESH ," </head> \n <frameset cols=250,* border=1 frameborder=yes><frame src=", 
            filemenu, " name=menu scrolling=yes><frame src=", 
            filemain, " name=main scrolling=yes></frameset> \n </html>"), 
            append = FALSE, sep = "", file = absfileindex)
         if (nchar(img.href[1])==0){
        	IMGSTRING <- paste("<img src='",img.logo[1],"' width = 150>",sep="");
        	if (length(img.logo) > 1) 
        	  for (jjj in 2:length(img.logo)) IMGSTRING <- c(IMGSTRING, paste("<img src='",img.logo[jjj],"' width = 150>",sep=""));
        } else {
        	IMGSTRING <- paste("<a href='",img.href[1],"'> <img src='",img.logo[1],"' width = 150> </a>",sep="");    
        	if (length(img.logo) > 1) {
        	  if (length(img.href) != length(img.logo)) img.href <- rep(img.href, length(img.logo))
        	  for (jjj in 2:length(img.logo)) IMGSTRING <- c(IMGSTRING, paste("<a href='",img.href[jjj],"'> <img src='",img.logo[jjj],"' width = 150> </a>",sep=""));
        	}
        }
        cat("<html><head><link rel=stylesheet href=", CSSFile, 
            " type=text/css> </head><body bgcolor=\"#E5F5FF\">  <center> ",IMGSTRING," <hr size=1></center><br>", 
            sep = "", append = FALSE, file = absfilemenu)
        txt <- ifelse(useLaTeX, "<html xmlns:mml=\"http://www.w3.org/1998/Math/MathML\">", 
            "<html>")
        txt <- c(txt, "<head>")
        txt <- c(txt, paste("<title>", Title, "</title>"))
        txt <- c(txt, paste("<link rel=stylesheet href=\"", CSSFile, 
            "\" type=text/css>", sep = ""))
        if (useLaTeX) 
            txt <- c(txt, "<object id=\"mathplayer\" classid=\"clsid:32F66A20-7614-11D4-BD11-00104BD3F987\"></object>\n<?import namespace=\"mml\" implementation=\"#mathplayer\"?>\n<script type=\"text/javascript\" src=\"ASCIIMathML.js\"></script>")
        if (useGrid) {
            txt <- c(txt, HTMLgrid_references())
            txt <- c(txt, "<script>\n   nequations=0;\n</script>")
        }
        if (!is.null(JSCPATH)) 
           txt <- c(txt, paste('\n <link rel=\"stylesheet\" type=\"text/css\" href=\"',JSCPATH,'/jsComponents.css\" />
  <script type=\"text/javascript\" src=\"',JSCPATH,'/extra/jsRegionChart.js\"> </script>
  <script type=\"text/javascript\" src=\"',JSCPATH,'/extra/jsBarChart.js\"> </script>
  <script type=\"text/javascript\" src=\"',JSCPATH,'/jsComponents.js\"> </script>
  <script type=\"text/javascript\" src=\"',JSCPATH,'/extra/jsDebugger.js\"></script>
  <script type=\"text/javascript\" src=\"',JSCPATH,'/extra/jsDebuggerTools.js\"></script>
  <script type=\"text/javascript\" src=\"',JSCPATH,'/extra/jsHelloWorld.js\"></script>
  <link rel=\"stylesheet\" type=\"text/css\" href=\"',JSCPATH,'/extra/jsDebugger.css\" />
  <link rel=\"stylesheet\" type=\"text/css\" href=\"',JSCPATH,'/extra/jsHelloWorld.css\" />
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />
<meta http-equiv=\"Content-Style-Type\" content=\"text/css\" />',sep=""))
        txt <- c(txt, "</head>")
        body <- c("<body")
        if (useLaTeX) 
            body = c(body, " onload=\"translate()\"")
        body = c(body, paste(" bgcolor=", BackGroundColor))
        if (!is.null(BackGroundImg)) 
            body = c(body, paste(" background=\"", BackGroundImg, 
                "\"", sep = ""))
        body <- c(body, " >")
        body = paste(body, collapse = "")
        txt <- c(txt, body)
        txt <- paste(txt, collapse = "\n")
        cat(txt, file = absfilemain, append = FALSE)
    }
    if (HTMLframe){
      outdir <- makePathName(outdir);
      invisible(return(c(targetmain = file, targetmenu=paste(outdir,filemenu,sep=""), target= paste(outdir, filename,".",extension, sep = ""))));
    } else {
      invisible(return(file));
    }
}

