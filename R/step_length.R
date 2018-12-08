#' Step Length
#'
#' Calculate step length with data.table
#'
#' @param DT data.table
#' @param coords character vector, length 2, coordinate column names. UTM required.
#' @param time character time column name.
#' @param splitBy character vector of column names to split step length calculation by. default is id and yr (individual identifier and year as numeric).
#' @param type default: lag. alternative: lead.
#' @param moverate calculate movement rate? stepLength / dif time, unit hours.
#' @param preserve preserve intermediate cols? default: no.
#' @import data.table
#'
#' @export
#'
#' @examples
#'
step_length <- function(DT, coords = c('EASTING', 'NORTHING'), time = 'datetime',
												splitBy = c('id', 'yr'), type = 'lag', moverate = FALSE) {

	yr <- .SD <- stepLength <- moveRate <- NULL

	shiftXY <- paste0('lag', coords)
	difXY <- paste0('dif', coords)

	if (DT[, data.table::uniqueN(data.table::year(.SD)), .SDcols = time] > 1) {
		DT[, yr := data.table::year(.SD[1]), .SDcols = time]
	}

	DT[order(get(time)),
		 (shiftXY) := data.table::shift(.SD, n = 1, fill = NA, type),
		 by = c(id, yr),
		 .SDcols = coords]

	DT[, (difXY) :=
		 	.((get(.SD[1]) - get(.SD[3])) ^ 2,
		 		(get(.SD[2]) - get(.SD[4])) ^ 2),
		 .SDcols = c(coords, shiftXY)]

	DT[, stepLength := sqrt(rowSums(.SD)),
			 .SDcols = difXY]

	if (!preserve) {
		set(DT, j = c(shiftXY, difXY), value = NULL)
	}

	if (moverate) {
		shiftT <- paste0('lag', time)
		difT <- paste0('dif', time)

		DT[order(get(time)), (shiftT) := data.table::shift(.SD, 1, NA, 'lag'),
				 by = c(id, yr),
				 .SDcols = time]

		DT[, (difT) := as.numeric(get(.SD[1]) - get(.SD[2]), units = 'hours'),
				 .SDcols = c(time, shiftT)]

		DT[, moveRate := .SD[1] / .SD[2],
			 .SDcols = c('stepLength', difT)]


	}

}
