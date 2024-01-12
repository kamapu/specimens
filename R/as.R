#' @name coerce-methods
#' @rdname coerce-methods
#'
#' @title Coerce specimens to data frame objects
#'
#' @description
#' A straight-forward method to coerce objects of class [specimens-class] to
#' [data.frame]. In the case that the collection is an object of class [sf],
#' coordinate values will be extracted as **coord_x** and **coord_y**.
#'
#' In the case of multiple determinations in the history of a specimen only the
#' newest one will be included in the data frame.
#'
#' @param from A [specimens-class] or a sf object.
#' 
#' @example examples/as.R
#'
#' @return
#' A [data.frame] object.
#'
#' @aliases coerce,sf,data.frame-method
setAs("sf", "data.frame", function(from) {
  classes <- do.call(c, lapply(from, function(x) class(x)[1]))
  if ("sfc_POINT" %in% classes) {
    coords <- st_coordinates(from)
    from$coord_x <- coords[, "X"]
    from$coord_y <- coords[, "Y"]
  }
  from <- st_drop_geometry(from)
  return(from)
})

#' @name coerce-methods
#' @rdname coerce-methods
#' @aliases coerce,specimens,data.frame-method
setAs("specimens", "data.frame", function(from) {
  if (class(from@collections)[1] == "sf") {
    from@collections <- as(from@collections, "data.frame")
  }
  OUT <- merge(from@specimens, from@collections,
    by = "coll_nr",
    all = TRUE, sort = FALSE
  )
  Det <- from@history[order(from@history$det_date, decreasing = TRUE), ]
  Det <- Det[!duplicated(Det$spec_id), ]
  return(merge(OUT, Det, by = "spec_id", all = TRUE, sort = FALSE))
})
