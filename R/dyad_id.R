#' Dyad ID
#'
#' Build unique ID for dyads of focal individuals (and neighbours).
#'
#' This function builds a dyad ID: focal A -> neighbour B (and focal B -> neighbour A) = dyad 1.
#'
#'
#' @param DT data.table.
#' @param focal focal individual.
#' @param neighbour neighbours, optional - see Details.
#'
#' @return
#'
#' If neighbour is not provided, all potential dyads are returned as a "dyad dictionary".
#'
#' Alternatively, when observed neighbours are provided, a "dyadID" column is added to the data.table with the individuals
#'
#' @export
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' DT <- fread(system.file("extdata", "DT.csv", package = "toast"))
#'
#' dyadDict <- dyad_id(DT, focal = 'ID', neighbour = NULL)
dyad_id <- function(DT = NULL, focal = 'id', neighbour= NULL) {
	# NSE errors
	. <- NULL

	check_col(DT, focal, 'focal')
	check_type(DT, focal, c('numeric', 'character'))

	if (is.null(neighbour)) {
		ids <- DT[, data.table::CJ(unique(.SD[[1]]), unique(.SD[[1]])),
							.SDcols = focal]

		g <- igraph::graph_from_edgelist(
			as.matrix(ids),
			directed = FALSE
		)

		simpler <- igraph::simplify(g)
		out <- data.table::data.table(
			igraph::get.edgelist(simpler),
			as.numeric(igraph::E(simpler)))

		nms <- c('focal', 'neighbour', 'dyadID')
		data.table::setnames(out, nms)

		return(out)
	}

	if (!is.null(neighbour)) {
		check_col(DT, neighbour, 'neighbour')
		check_type(DT, neighbour, c('numeric', 'character'))

		stop('in dev')
	# if ('dyadID' %in% colnames(DT)) {
	# 	warning('dropping dyadID from DT')
	# 	DT[, dyadID := NULL]
	# }

	# # Double merge
	# # First where left goes to dyadID
	# # Then where right goes to dyadID2 and the order revered in by.y
	# dyads <- merge(
	# 	merge(DT,
	# 				edgeDT,
	# 				by.x = c(id, neighbour),
	# 				by.y = edgeNames[1:2],
	# 				all.x = TRUE),
	# 	edgeDT[, .(id1, id2, dyadID2 = dyadID)],
	# 	by.x = c(id, neighbour),
	# 	by.y = edgeNames[2:1],
	# 	all.x = TRUE)
	#
	# # Then dyadID filled with dyadID2
	# # and dyadID2 dropped
	# dyads[is.na(dyadID), dyadID := dyadID2][, dyadID2 := NULL][]
	}
}
