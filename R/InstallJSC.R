InstallJSC <- function(JSCPATH){

  cat(JSCPATH, " does not exist, we recommend that we copy the relevant files, proceed ? (y/n):");
  ans <- readLines(n=1);
  if (ans == "y") {
  	system(paste("mkdir ", JSCPATH));
    jscDIR <- paste(system.file(package = "HTMLUtils"), "/jsc/",sep ="")
  	system(paste("cp -r ", jscDIR,"* ", JSCPATH,"/",sep=""))
  } else {
  	cat("The dynamically sortable tables will likely not work ...\n");
  }  
  
  }