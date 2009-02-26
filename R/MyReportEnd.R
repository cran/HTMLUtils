`MyReportEnd` <-
function(file = "report.html") { 
 cat("\n<hr size=1></body></html>", 
 file = file, append = TRUE) 
 }

