#' build ee asset
#'
#' Build an Earth Engine asset from a data.table of locs.
#'
#'
#' @param out pathname of output shapefile folder. Includes location of the output folder but does not matter if that folder exists. The last portion of the pathname will be used for the layer name e.g.: 'path/to/caribou' layer name = 'caribou'
build_ee_asset <-
	function(DT,
					 out,
					 projection,
					 id = 'id',
					 coords = c('X', 'Y'),
					 extra = NULL,
					 overwrite = FALSE) {
		extra <- c(id, extra)
		pts <- SpatialPointsDataFrame(DT[, ..coords],
																	proj4string = CRS(projection),
																	data = DT[, ..extra])
		writeOGR(pts,
						 out,
						 tail(tstrsplit(t, '/'), n = 1L),
						 driver = "ESRI Shapefile",
						 overwrite_layer = overwrite)

		# zip('output/elkshp.zip', dir('output/elkshp', full.names = TRUE))

	}
