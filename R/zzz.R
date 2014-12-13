.onLoad = function(libname, pkgname) {
  circos.par("__tempdir__" = tempdir())
}

.onAttach = function(libname, pkgname) {
  circos.par("__tempdir__" = tempdir())
}
