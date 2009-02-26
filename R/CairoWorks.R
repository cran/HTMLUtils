`CairoWorks` <-
function(testfile = "/tmp/tmp.png", verbose = 0){
  ret <- try(CairoPNG(testfile));
    if (class(ret)[1] == "try-error"){#use GDD instead:
  	  require(GDD);
      CairoPNG <<- GDD;
  	  if (verbose) print("replaced Cairo by GDD, as Cairo appears to not be working correctly")
      } else {#Cairo works
  	  dev.off();
  	  if (verbose) print("Cairo appears to be working")
    }
}

