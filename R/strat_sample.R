#' Stratified polygon sampling
#'
#' For each unique value in 'col', sample 'n' points and optionally return as a \code{data.table}.
#'
#' @param x
#' @param n
#' @param col
#' @param returnDT
#'
#' @return Either a \code{sf} object or a \code{data.table} with a \code{sfc} (simple feature column).
#' @export
#'
#' @examples
strat_sample <- function(x, n, col, returnDT = TRUE) {
	lvls <- unique(x[[col]])
	DT <- lapply(lvls, function(l) {
		sf::st_sf(col = col, geometry = sf::st_sample(x[x[[col]] == l, ], n))
	})

	if(returnDT) {
		return(rbindlist(DT))
	} else {
		return(do.call(rbind, DT))
	}
}
