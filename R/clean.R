#' @name clean
#'
#' @title Delete orphaned records in specimens
#'
#' @description
#' Manipulation of slots may generate orphaned entries in [specimens-class]
#' objects, thus such information needs to be cleaned to make objects valid
#' again.
#'
#' @param object A [specimens-class] object.
#' @param ... Further arguments passed from or to other methods.
#'
#' @return A clean [specimens-class] object.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @rdname clean
#'
#' @exportMethod clean
setGeneric(
  "clean",
  function(object, ...) {
    standardGeneric("clean")
  }
)

#' @rdname clean
#'
#' @aliases clean,specimens-method
setMethod(
  "clean", signature(object = "specimens"),
  function(object, ...) {
    # Delete collections without specimens
    object@collections <- object@collections[object@collections$coll_nr %in%
      object@specimens$coll_nr, ]
    # Delete orphan specimens
    object@specimens <- object@specimens[object@specimens$coll_nr %in%
      object@collections$coll_nr, ]
    # Delete orphan determinations
    object@history <- object@history[object@history$spec_id %in%
      object@specimens$spec_id, ]
    return(object)
  }
)
