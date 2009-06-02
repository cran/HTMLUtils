`CairoWorks` <-
function(testfile = paste(tempdir(),"/tmp.png",sep=""), verbose = 0){
  ret <- try(require(Cairo));
  ret <- try(CairoPNG(testfile));
  if (class(ret)[1] == "try-error"){#use GDD instead:
  	  ret <- try(require(GDD));
  	  if (class(ret)[1] == "try-error"){
        CairoPNG <<- GDD;
      } else {
      	print("neither Cairo nor GDD appear to be working, will resort to png() now, which may or may not work depending on the availability of X11 drivers.");
      	CairoPNG <<- png;
      }
  	  if (verbose) print("replaced Cairo by GDD, as Cairo appears to not be working correctly")
   } else {#Cairo works
  	  dev.off();
  	  if (verbose) print("Cairo appears to be working")
    }
}

