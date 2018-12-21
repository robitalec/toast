#' Camera grid
#'
#' @param DT
#' @param id
#' @param n
#' @param distance
#' @param coords
#'
#' @return
#' @export
#'
#' @examples
camera_grid <- function(DT, id, n, distance, coords) {
	DT[rep(1:.N, times = n + 1),
		 .SD +  CJ(c(-distance, 0, distance), c(-distance, 0, distance)),
		 .SDcols = coords,
		 by = id]
}
