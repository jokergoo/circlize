setLoadActions(function(ns) {
	circos.par("__tempdir__" = tempdir())
})

.onAttach = function(lib, pkg, ...) {
	msg = paste0("circlize ", utils::packageDescription("dendextend")$Version, "\n",
		"Documentation: http://jokergoo.github.io/circlize_book/book\n",
		"Examples: http://jokergoo.github.io/circlize\n",
		"Citation: Gu Z, et al., circlize implements and enhances circular visualization in R, Bioinformatics 2014.")
	packageStartupMessage(msg)
}
