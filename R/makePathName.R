`makePathName` <-
function(path, MakePath=TRUE, verbose=0){
  N <- nchar(path);
  if (substring(path,N,N) != "/"){
  	path <- paste(path, "/",sep="")
  }
   if (MakePath)
   if (!file.exists(path)) {
  	tmp <- dir.create(path);
  	if (!tmp) cat(path, "could not be created.\n");
  	if (tmp & verbose) cat(path, "successfully created.\n");
  	flush(stdout())
  }

  return(path)
}

