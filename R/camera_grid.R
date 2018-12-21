#' Camera grid
#'
#' @param DT
#' @param id
#' @param case "queen", "rook" or "bishop".
#' @param distance
#' @param coords
#'
#' @return
#' @export
#'
#' @examples
camera_grid <- function(DT, id, case, distance, coords) {
	# NSE
	focal <- camX <- camY <- NULL;

	if (case == 'queen') {
		move <- data.table::CJ(c(-distance, 0, distance),
													 c(-distance, 0, distance))
	} else if (case == 'rook') {
		move <- data.table::CJ(c(-distance, distance),
													 c(-distance, distance))
	} else if (case == 'bishop') {
		move <- data.table::data.table(c(0, distance, 0, -distance),
																	 c(distance, 0, -distance, 0))
	}

	DT <- DT[rep(1:.N, times = nrow(move))]
	DT[, c('camX', 'camY') := .SD + move,
		 .SDcols = coords,
		 by = id]
	DT[camX == get(coords[[1]]) & camY == get(coords[[2]]),
		 focal := TRUE]
}
