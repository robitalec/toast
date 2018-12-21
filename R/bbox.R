#' Bounding
#'
#' @param DT
#' @param x
#' @param y
#' @param by
#'
#' @return
#' @export
#'
#' @examples
bounding <- function(DT, x, y, by = NULL) {
	DT[, data.table::CJ(x = c(min(.SD[[1]], na.rm = TRUE),
														max(.SD[[1]], na.rm = TRUE)),
											y = c(min(.SD[[2]], na.rm = TRUE),
														max(.SD[[2]], na.rm = TRUE))),
		 .SDcols = c(x, y),
		 by = by]
}
