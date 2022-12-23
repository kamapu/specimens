#' @name map_specimens
#'
#' @rdname map_specimens
#'
#' @title Display location of specimens in a leaflet widget
#'
#' @description
#' Display of specimens' location in a leaflet widget. Markers sharing locations
#' will be clustered.
#'
#' @param x A [specimens-class] object.
#' @param add_cols A character vector with the names of columns to be displayed
#'     in the popup labels of markers.
#' @param sep A character value used as separator between values of different
#'     columns in the popup labels.
#' @param date_format A character value indicating the format used for the
#'     collection date. It is passed to the function [format.Date()].
#' @param p_tiles Character value passed to [addProviderTiles()]. The tiles
#'     used as background in the map.
#' @param ... Further arguments passed among methods.
#'
#' @export
map_specimens <- function(x, ...) {
  UseMethod("map_specimens", x)
}

#' @rdname map_specimens
#'
#' @aliases map_specimens,specimens-method
#'
#' @method map_specimens specimens
#'
#' @examples
#' \dontrun{
#' map_specimens(churo_survey)
#' }
#'
#' @export
map_specimens.specimens <- function(x,
                                    add_cols = c(
                                      "coll_date", "leg",
                                      "taxon_name"
                                    ),
                                    sep = " | ",
                                    date_format = "%d.%m.%Y",
                                    p_tiles = "OpenStreetMap",
                                    ...) {
  if (is(x@collections, "sf")) {
    crs <- st_crs(x@collections, parameters = TRUE)$epsg
  } else {
    stop("Slot 'collections' in 'x' has to be of class 'sf'")
  }
  x <- as(x, "data.frame")
  y <- x[, c("spec_id", "coord_x", "coord_y")]
  # format columns
  x$spec_id <- paste("spec: ", x$spec_id)
  x$coll_nr <- paste("coll: ", x$coll_nr)
  x$coll_date <- format(x$coll_date, date_format)
  # merge to labels
  y$labels <- do.call(paste, c(as.list(x[, unique(c(
    "spec_id", "coll_nr",
    add_cols
  ))]), list(sep = sep)))
  # do map
  m <- y %>%
    st_as_sf(coords = c("coord_x", "coord_y"), crs = crs) %>%
    leaflet() %>%
    addProviderTiles(p_tiles) %>%
    addMarkers(
      popup = ~labels,
      clusterOptions = markerClusterOptions()
    )
  return(m)
}
