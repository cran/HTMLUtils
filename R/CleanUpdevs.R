`CleanUpdevs` <-
function(){
  OpenDevs <- dev.list();
  OpenPDFs <- OpenDevs[names(OpenDevs) %in% "pdf"];
  OpenPNGs <- OpenDevs[names(OpenDevs) %in% "png"];
  OpenCairos <- OpenDevs[names(OpenDevs) %in% "Cairo"];
  for (d in OpenPDFs) dev.off(which= d);
  for (d in OpenPNGs) dev.off(which= d);
  for (d in OpenCairos) dev.off(which= d);
}

