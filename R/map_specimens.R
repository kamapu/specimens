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
#' This function will coerce to a data fram using the function [as_data.frame()].
#'
#' @param x A [specimens-class] object.
#' @param add_cols A character vector with the names of columns to be displayed
#'     in the popup labels of markers.
#' @param sep A character value used as separator between values of different
#'     columns in the popup labels.
#' @param date_format A character value indicating the format used for the
#'     collection date. It is passed to the function [format.Date()].
#' @param coords A character vector with the names of the columns containing the
#'     coordinate values. Default adjusted to an object using the [sf][sf] at
#'     the slot **collections**.
#' @param crs An integer including the EPSG ID of the spatial reference system,
#'     which is passed to [st_crs()].
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
                                    coords = c("longitude", "latitude"),
                                    crs = 4326,
                                    p_tiles = "OpenStreetMap",
                                    ...) {
  if (class(x@collections)[1] == "sf") {
    crs <- st_crs(x@collections, parameters = TRUE)$epsg
  }
  x <- as_data.frame(x)
  y <- x[, c("spec_id", coords)]
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
    st_as_sf(coords = coords, crs = crs) %>%
    leaflet() %>%
    addProviderTiles(p_tiles) %>%
    addMarkers(
      popup = ~labels,
      clusterOptions = markerClusterOptions()
    )
  return(m)
}
