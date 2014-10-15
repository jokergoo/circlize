.onLoad = function(libname, pkgname) {
  circos.par("__tempdir__" = tempdir())
}

.onAttach = function(libname, pkgname) {
  circos.par("__tempdir__" = tempdir())
}

.onUnload = function(libpath) {
  file.remove(circos.par("__tempdir__"))
}

.onDetach = function(libpath) {
  file.remove(circos.par("__tempdir__"))
}
