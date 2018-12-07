#' Step Length
#'
#' Calculate step length with data.table
#'
#' @param DT data.table
#' @param coords character vector, length 2, coordinate column names. UTM required.
#' @param time character time column name.

# Set columns
time.col <- 'datetime'
coord.cols <- c('EASTING', 'NORTHING')

# Create lag and dif column names
lag.cols <- paste('lag', coord.cols, sep = '')
difference.cols <- c('difX', 'difY')

lag.time.col <- paste0('lag', time.col)
dif.time.col <- paste0('dif', time.col)

# Use shift  to create lagged cols
locs[order(get(time.col)), (lag.cols) := shift(.SD, 1, NA, 'lag'),
		 by = .(ANIMAL_ID, year),
		 .SDcols = coord.cols]

# Find the difference squared between all points in each x,y separately
locs[, (difference.cols) := .((get(coord.cols[1]) - get(lag.cols[1])) ^2,
															(get(coord.cols[2]) - get(lag.cols[2])) ^2)]

# Square root the summed difference for a simple step length
locs[, simpleStep := sqrt(rowSums(.SD)),
		 .SDcols = difference.cols]

## Delta Time
locs[order(get(time.col)), (lag.time.col) := shift(.SD, 1, NA, 'lag'),
		 by = .(ANIMAL_ID, year),
		 .SDcols = time.col]

# difference in time in hours
locs[, (dif.time.col) := as.numeric(get(time.col) - get(lag.time.col), units = 'hours')]

# Simple step length divided by time difference
locs[, moveRate := simpleStep / (get(dif.time.col))]
