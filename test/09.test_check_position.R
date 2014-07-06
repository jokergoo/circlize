is.points.ordered.on.circle(c(30, 50, 70, 90))    # TRUE
is.points.ordered.on.circle(c(90, 70, 50, 30))    # FALSE
is.points.ordered.on.circle(c(90, 70, 50, 30), clock.wise = TRUE)    # TRUE
is.points.ordered.on.circle(c(150, 50, 70, 180))  # FALSE 
is.points.ordered.on.circle(c(150, 30, 10, 330))  # FALSE
is.points.ordered.on.circle(c(270, 330, 10, 120)) # TRUE
is.points.ordered.on.circle(c(120, 10, 330, 270), clock.wise = TRUE) # TRUE
is.points.ordered.on.circle(c(120, 10, 330, 270)) # FALSE
is.points.ordered.on.circle(c(240, 280, 330, 360)) # TRUE
is.points.ordered.on.circle(c(360, 240, 280, 330)) # TRUE


################################
# test check.track.position











##################################
# test check.points.position