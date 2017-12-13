colorDictionary = new.env()

# == title 
# Generate random colors
#
# == param
# -n number of colors
# -hue the hue of the generated color. You can use following default color name: ``red``, ``orange``, 
#      ``yellow``, ``green``, ``blue``, ``purple``, ``pink`` and ``monochrome``. If the value is a hexidecimal color string such as ``#00FFFF``, 
#      the function will extract its hue value and use that to generate colors.
# -luminosity controls the luminosity of the generated color. The value should be a string containing ``bright``, ``light``, ``dark`` and ``random``.
# -transparency transparency, numeric value between 0 and 1.
#
# == details
# The code is adapted from randomColor.js (https://github.com/davidmerfield/randomColor ).
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# plot(NULL, xlim = c(1, 10), ylim = c(1, 8), axes = FALSE, ann = FALSE)
# points(1:10, rep(1, 10), pch = 16, cex = 5, 
#     col = rand_color(10, luminosity = "random"))
# points(1:10, rep(2, 10), pch = 16, cex = 5, 
#     col = rand_color(10, luminosity = "bright"))
# points(1:10, rep(3, 10), pch = 16, cex = 5, 
#     col = rand_color(10, luminosity = "light"))
# points(1:10, rep(4, 10), pch = 16, cex = 5, 
#     col = rand_color(10, luminosity = "dark"))
# points(1:10, rep(5, 10), pch = 16, cex = 5, 
#     col = rand_color(10, hue = "red", luminosity = "bright"))
# points(1:10, rep(6, 10), pch = 16, cex = 5, 
#     col = rand_color(10, hue = "green", luminosity = "bright"))
# points(1:10, rep(7, 10), pch = 16, cex = 5, 
#     col = rand_color(10, hue = "blue", luminosity = "bright"))
# points(1:10, rep(8, 10), pch = 16, cex = 5, 
#     col = rand_color(10, hue = "monochrome", luminosity = "bright"))
rand_color = function(n, hue = NULL, luminosity = "random", transparency = 0) {

	col = sapply(1:n, function(x) {
		H = pickHue(hue)
		S = pickSaturation(H, luminosity)
		B = pickBrightness(H, S, luminosity)
		hex(HSV(H, S/100, B/100))
	})
	add_transparency(col, transparency)
}

pickHue = function(hue) {

	hueRange = getHueRange(hue)
	hue = randomWithin(hueRange[1], hueRange[2])

	if (hue < 0) hue = 360 + hue

	return(hue)
}

pickSaturation = function(hue, luminosity) {

	if (hue == 'monochrome') {
	  return(0)
	}

	if (luminosity == 'random') {
	  return(randomWithin(0, 100))
	}

	saturationRange = getSaturationRange(hue);

	sMin = saturationRange[1]
	sMax = saturationRange[2]

	if(luminosity == "bright") {
	    sMin = 55
	} else if(luminosity == "dark") {
	    sMin = sMax - 10
	} else if(luminosity == "light") {
	    sMax = 55
	}

	return(randomWithin(sMin, sMax))
}

pickBrightness = function(H, S, luminosity) {

	bMin = getMinimumBrightness(H, S)
	bMax = 100

	if(luminosity == "dark") {
		bMax = bMin + 20
	} else if(luminosity == "light") {
	    bMin = (bMax + bMin)/2
	} else if(luminosity == "random") {
	    bMin = 0
	    bMax = 100
	}

	return(randomWithin(bMin, bMax))
}


getMinimumBrightness = function(H, S) {

	lowerBounds = getColorInfo(H)$lowerBounds

	for (i in seq_len(ncol(lowerBounds)-1)) {

		s1 = lowerBounds[1, i]
		v1 = lowerBounds[2, i]

		s2 = lowerBounds[1, i + 1]
		v2 = lowerBounds[2, i + 1]

		if (S >= s1 && S <= s2) {

			m = (v2 - v1)/(s2 - s1)
			b = v1 - m*s1

			return(m*S + b)
		}

	}

	return(0)
}

getHueRange = function(colorInput) {

	if(is.null(colorInput)) {
		return(c(0, 360))
	}

	if (is.numeric(colorInput)) {

		number = colorInput

		if (number < 360 && number > 0) {
			return(c(number, number))
		}

	}

	if (is.character(colorInput)) {

		if (!is.null(colorDictionary[[colorInput]])) {
			color = colorDictionary[[colorInput]]
			if (!is.null(color$hueRange)) return(color$hueRange)
		} else {
			color = col2rgb(colorInput)
			rgb = RGB(color[1], color[2], color[3])
			hue = as(rgb, "HSV")[1]
			return(c(hue, hue))
		}
	}

	return(c(0,360))
}

getSaturationRange = function(hue) {
	return(getColorInfo(hue)$saturationRange)
}

getColorInfo = function(hue) {
	hue = round(hue)
	if (hue >= 334 && hue <= 360) {
		hue = hue - 360
	}

	for (colorName in ls(envir = colorDictionary)) {
	   color = colorDictionary[[colorName]]
	   if (!is.null(color$hueRange) && hue >= color$hueRange[1] && hue <= color$hueRange[2]) {
	      return(colorDictionary[[colorName]])
	   }
	} 

	stop('Color not found, hue =', hue)
}

randomWithin = function(min, max) {
	runif(length(min), min = min, max = max)
}


defineColor = function(name, hueRange, lowerBounds) {

	sMin = lowerBounds[1, 1]
	sMax = lowerBounds[1, ncol(lowerBounds)]

	bMin = lowerBounds[2, ncol(lowerBounds)]
	bMax = lowerBounds[2, 1]

	colorDictionary[[name]] = list(
		hueRange = hueRange,
		lowerBounds = lowerBounds,
		saturationRange = c(sMin, sMax),
		brightnessRange = c(bMin, bMax)
	)
}

loadColorBounds = function() {

	defineColor(
		'monochrome',
		NULL,
		cbind(c(0,0), c(100,0))
	)

	defineColor(
		'red',
		c(-26,18),
		cbind(c(20,100),c(30,92),c(40,89),c(50,85),c(60,78),c(70,70),c(80,60),c(90,55),c(100,50))
	)

	defineColor(
		'orange',
		c(19,46),
		cbind(c(20,100),c(30,93),c(40,88),c(50,86),c(60,85),c(70,70),c(100,70))
	)

	defineColor(
		'yellow',
		c(47,62),
		cbind(c(25,100),c(40,94),c(50,89),c(60,86),c(70,84),c(80,82),c(90,80),c(100,75))
	)

	defineColor(
		'green',
		c(63,178),
		cbind(c(30,100),c(40,90),c(50,85),c(60,81),c(70,74),c(80,64),c(90,50),c(100,40))
	)

	defineColor(
		'blue',
		c(179, 257),
		cbind(c(20,100),c(30,86),c(40,80),c(50,74),c(60,60),c(70,52),c(80,44),c(90,39),c(100,35))
	)

	defineColor(
		'purple',
		c(258, 282),
		cbind(c(20,100),c(30,87),c(40,79),c(50,70),c(60,65),c(70,59),c(80,52),c(90,45),c(100,42))
	)

	defineColor(
		'pink',
		c(283, 334),
		cbind(c(20,100),c(30,90),c(40,86),c(60,84),c(80,80),c(90,75),c(100,73))
	)
}
 
loadColorBounds()
