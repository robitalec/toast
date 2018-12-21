#' Camera grid
#'
#' @param DT
#' @param id
#' @param case
#' @param distance
#' @param coords
#'
#' @return
#' @export
#'
#' @examples
camera_grid <- function(DT, id, case, distance, coords) {
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

	DT[rep(1:.N, times = nrow(move)),
		 .SD + move,
		 .SDcols = coords,
		 by = c(id)]
}
