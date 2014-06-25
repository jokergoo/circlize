i = 1
while(1) {
	cat("\r")
	str = paste(paste(rep(" ", i), collapse = ""), "http://jokergoo.github.io/circlize", collapse = "")
	i = i+1
	if(nchar(str) > 80) {
		cat(paste(rep(" ", 80), collapse = ""))
		str = "http://jokergoo.github.io/circlize"
		i = 0
		cat("\r")
	}
	cat(str)
	Sys.sleep(0.2)
	flush.console()
}
