`MyReportBegin` <-
function(file = "report.html",  title = "My Report Title", header = NULL) {  
  if (is.null(header)) {
  	cat(paste("<html><head><title>", title, "</title></head>", "<body bgcolor=#D0D0D0>","<p align= left >",  sep = ""), file = file, append = FALSE);
  } else {
  	cat(paste("<html xmlns=\"http://www.w3.org/1999/xhtml\" \n  xml:lang=\"en\"> \n ", header,  "<body bgcolor=#D0D0D0>","<p align= left >",  sep = ""), file = file, append = FALSE);
  }
 }

