
# this file contains variables and functions related to
# global variables.

.SECTOR.DATA = NULL

# a list
.TRACK.END.POSITION = 1

# two layers list
.CELL.DATA = NULL

.CURRENT.TRACK.INDEX = NULL
.CURRENT.SECTOR.INDEX = NULL

.CIRCOS.PAR = list(
    start.degree = 0,
    gap.degree = 1,
    track.margin = c(0.01, 0.01),  # top margin and bottom margin
    unit.circle.segments = 1000,   #to simulate smooth curve
    cell.padding = c(0.1, 0.1, 0.1, 0.1),
    default.track.height = 0.2,
    points.overflow.warning = TRUE
)

circos.par = function (...) {
    args = list(...)
    
    if(length(args) == 0) {
        return(.CIRCOS.PAR)
    }
    if(is.null(names(args))) {
        if(length(args) == 1) {
            return(.CIRCOS.PAR[[unlist(args)]])
        } else {
            return(.CIRCOS.PAR[unlist(args)])
        }
    }
    
    name = names(args)
    if(sum(is.null(name)) == 0) {
        for(i in seq_along(args)) {
            any.cell.created = has.any.cell.created()
            if(name[i] == "cell.padding" && any.cell.created) {
                warning("'cell.padding' can only be modified before any cell has been created.\n")
                next
            }
            if(name[i] == "start.degree" && any.cell.created) {
                warning("'start.degree' can only be modified before any cell has been created.\n")
                next
            }
            if(name[i] == "gap.degree" && any.cell.created) {
                warning("'gap.degree' can only be modified before any cell has been created.\n")
                next
            }
            .CIRCOS.PAR[[ name[i] ]] <<- args[[ name[i] ]]
        }
        return(invisible(.CIRCOS.PAR))
    }
}

has.any.cell.created = function() {
    return(! is.null(unlist(.CELL.DATA)))
}

# x is a vector
# no x but a xlim
# xlim as a matrix
circos.initialize = function(x = NULL, factors = NULL, xlim = NULL, start.degree = circos.par("start.degree"), clock.wise = FALSE) {

    .SECTOR.DATA <<- NULL
    .CELL.DATA <<- NULL
    .CURRENT.TRACK.INDEX <<- 0
    .CURRENT.SECTOR.INDEX <<- NULL
    .TRACK.END.POSITION <<- 1

    if(! is.factor(factors)) {
        factors = factor(factors)
    }
    le = levels(factors)
    
    # initialize .SECTOR.DATA
    # you can think it as the global x axis configuration
    # calculate start and end value for each sectors
    # there are several ways
    if(is.vector(x)) {
    
        if(length(x) != length(factors)) {
            stop("Length of data and length of factors differ.\n")
        }
        start.value = tapply(x, factors, min)
        end.value = tapply(x, factors, max)
    } else if(is.vector(xlim)) {
        if(length(xlim) != 2) {
            stop("xlim should 2")
        }    
        
        start.value = rep(xlim[1], length(le))
        end.value = rep(xlim[2], length(le))
    } else if(is.matrix(xlim)) {
        if(dim(xlim)[1] != length(le) || dim(xlim)[2] != 2) {
            stop()
        }
        
        start.value = apply(xlim, 1, function(x) x[1])
        end.value = apply(xlim, 1, function(x) x[2])
    }
    
    
    cell.padding = circos.par("cell.padding")
    
    sector.range = end.value - start.value
    start.value = start.value - cell.padding[2]*sector.range
    end.value = end.value + cell.padding[4]*sector.range
    n.sector = length(le)
    
    sector = vector("list", 5)
    names(sector) = c("factor", "start.value", "end.value", "start.degree", "end.degree")
    sector[["factor"]] = le
    sector[["start.value"]] = start.value
    sector[["end.value"]] = end.value
    
    gap.degree = circos.par("gap.degree")
    
    # degree per data
    unit = (360 - gap.degree*n.sector) / sum(sector.range)
    for(i in seq_len(n.sector)) {
        sector[["start.degree"]][i] =  gap.degree + ifelse(i == 1, start.degree, sector[["end.degree"]][i-1])
        sector[["end.degree"]][i] = sector[["start.degree"]][i] + sector.range[i]*unit   

    #    sector[["start.degree"]][i] = sector[["start.degree"]][i] %% 360
    #    sector[["end.degree"]][i] = sector[["end.degree"]][i] %% 360
    }
    
    sector = as.data.frame(sector)
    .SECTOR.DATA <<- sector
    
    # initialize .CELL.DATA which contains information of each cell
    # if content of that cell has been created, it means that the 
    # plotteing region for that cell has been created.
    .CELL.DATA <<- vector("list", length = length(le))
    names(.CELL.DATA) <<- le
    for(i in seq_along(.CELL.DATA)) {
        .CELL.DATA[[ le[i] ]] <<- vector("list", length = 0)
    }
    
    # draw everything in a unit circle
    plot(c(-1, 1), c(-1, 1), type = "n", ann = FALSE, axes = FALSE)
    return(invisible(sector))
}

circos.clear = function() {
    .SECTOR.DATA <<- NULL
    .CELL.DATA <<- NULL
    .CURRENT.TRACK.INDEX <<- 0
    .CURRENT.SECTOR.INDEX <<- NULL
    .TRACK.END.POSITION <<- 1
	
	.CIRCOS.PAR = list(
		start.degree = 0,
		gap.degree = 1,
		track.margin = c(0.01, 0.01),  # top margin and bottom margin
		unit.circle.segments = 1000,   #to simulate smooth curve
		cell.padding = c(0.1, 0.1, 0.1, 0.1),
		default.track.height = 0.2,
		points.overflow.warning = TRUE
	)

    
    return(invisible(NULL))
}
get.all.sector.index = function() {
    return(as.vector(.SECTOR.DATA$factor))
}

get.sector.data = function(sector.index) {
    sector.data = as.vector(as.matrix(.SECTOR.DATA[.SECTOR.DATA[[1]] == sector.index, 2:5]))
    names(sector.data) = colnames(.SECTOR.DATA)[2:5]
    return(sector.data)
}

get.current.track.index = function() {
    return(.CURRENT.TRACK.INDEX)   
}

set.current.track.index = function(x) {
    .CURRENT.TRACK.INDEX <<- x
    return(invisible(NULL))
}

get.max.track.index = function() {
    if(get.current.track.index() == 0) {
        return(0)
    } else {
        return(length(.CELL.DATA[[1]]))
    }
}

get.current.sector.index = function() {
    return(.CURRENT.SECTOR.INDEX)   
}

set.current.sector.index = function(x) {
    .CURRENT.SECTOR.INDEX <<- x
    return(invisible(NULL))
}

get.track.end.position = function(track.index = get.current.track.index()) {
    
    if(track.index == 0) {
        return(1)
    } else {
        return(.TRACK.END.POSITION[track.index])
    }
}

set.track.end.position = function(track.index = get.current.track.index(), y) {
    
    .TRACK.END.POSITION[track.index] <<- y
    return(invisible(NULL))
}

get.cell.data = function(sector.index, track.index) {
    .CELL.DATA[[sector.index]][[track.index]]
}

set.cell.data = function(sector.index, track.index, ...) {
    .CELL.DATA[[sector.index]][[track.index]] <<- list(...)
    return(invisible(NULL))
}

has.cell = function(sector.index, track.index) {
    if(sector.index %in% names(.CELL.DATA) &&
       track.index <= length(.CELL.DATA[[sector.index]]) &&
       !is.null(.CELL.DATA[[sector.index]][[track.index]])) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# ** title
# Label the sector index and the track index of each cell
show.index = function() {
    sectors = names(.CELL.DATA)
    for(i in seq_along(sectors)) {
        for(j in seq_along(.CELL.DATA[[ sectors[i] ]])) {
            d = .CELL.DATA[[ sectors[i] ]][[j]]
            circos.text(mean(d$xlim), mean(d$ylim), sector.index = sectors[i],
                        track.index = j, labels = paste(sectors[i], j, sep = ":"), 
                        direction = "horizontal")
        }
    }
}
