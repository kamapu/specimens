#' Function producing the overview of whole object.
#'
#' @keywords internal
view_specimens <- function(object) {
  Msg <- paste0(
    "## object of class 'specimens'\n",
    "   collections:    ", nrow(object@collections), "\n",
    "   specimens:      ", nrow(object@specimens), "\n",
    "   determinations: ", nrow(object@history), "\n"
  )
  cat(Msg)
}

#' Function producing the overview per collection.
#'
#' @keywords internal
view_collection <- function(object, format = "%d.%m.%Y") {
  n1 <- nrow(object@collections)
  t1 <- with(
    object@collections,
    cbind(
      rep("------------------------------\n", n1),
      paste0("collection: ", paste(coll_nr), "\n"),
      paste0("collector:  ", paste(leg), "\n"),
      paste0("date:       ", format(coll_date, format = format), "\n")
    )
  )
  rownames(t1) <- paste(object@collections$coll_nr)
  # Number of specimens per collection
  nr_spec <- aggregate(spec_id ~ coll_nr, data = object@specimens, FUN = length)
  nr_spec <- with(nr_spec, paste(spec_id)[match(rownames(t1), paste(coll_nr))])
  nr_spec[is.na(nr_spec)] <- "0"
  t1 <- cbind(t1, paste0("specimens:  ", nr_spec, "\n"))
  # Updated determination
  Det <- object@history[order(object@history$det_date, decreasing = TRUE), ]
  Det <- Det[!duplicated(Det$spec_id), ]
  Det$coll_nr <- with(object@specimens, coll_nr[match(Det$spec_id, spec_id)])
  Det <- split(Det, Det$coll_nr)
  show_det <- function(x) {
    x <- with(x, paste(spec_id, taxon_name, paste0(
      "(det: ", det, ", ",
      format(det_date, format = format), ")\n"
    )))
    paste(x, collapse = "")
    return(paste0("\n", x))
  }
  Det <- unlist(lapply(Det, show_det))
  # Adding indetermined specimens
  indet <- object@specimens[!object@specimens$spec_id %in%
    object@history$spec_id, c("spec_id", "coll_nr")]
  indet_n <- indet$coll_nr
  indet <- with(indet, paste0("\n", spec_id, " indet.\n"))
  names(indet) <- indet_n
  Det <- c(Det, indet)
  Det <- Det[match(rownames(t1), names(Det))]
  Det[is.na(Det)] <- ""
  t1 <- cbind(t1, Det)
  cat(c(t(t1), "------------------------------\n"))
}

#' @name summary
#'
#' @rdname summary
#'
#' @title Print overviews for 'specimens'
#'
#' @description
#' A method to display either an overview of the content of
#' [specimens-class] objects or an overview of selected taxa.
#'
#' @param object,x A [specimens-class] object.
#' @param coll_nr Vector with collections IDs to be displayed in the summary.
#'     The keyword "all" is set as default indicating that all collections will
#'     be included in the summary.
#' @param spec_id Vector with IDs of specimens that will result in a display of
#'     the determination history of the specimens but will be ignored if values
#'     for 'coll_nr' are provided.
#' @param format Character value indicating the format applied to dates in the
#'     displays. See [strptime()] for alternative formats.
#' @param ... Further arguments passed to or from another methods.
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @aliases summary,specimens-method
#'
#' @exportMethod summary
setMethod(
  "summary", signature(object = "specimens"),
  function(object, coll_nr = "all", spec_id, format = "%d.%m.%Y", ...) {
    if (coll_nr != "all") {
      object@collections <- object@collections[object@collections$coll_nr %in%
        coll_nr, ]
      object@specimens <- object@specimens[object@specimens$coll_nr %in%
        object@collections$coll_nr, ]
      view_collection(object, format = format)
    } else {
      if (!missing(spec_id)) {
        return(object@history[object@history$spec_id %in% spec_id, ])
      } else {
        view_collection(object, format = format)
      }
    }
  }
)

#' @rdname summary
#'
#' @aliases show,specimens-method
#'
#' @exportMethod show
setMethod(
  "show", signature(object = "specimens"),
  function(object) {
    view_specimens(object)
  }
)

#' @exportMethod print
if (!isGeneric("print")) {
  setGeneric(
    "print",
    function(x, ...) {
      standardGeneric("print")
    }
  )
}

#' @rdname summary
#'
#' @aliases print print,specimens-method
setMethod(
  "print", signature(x = "specimens"),
  function(x, ...) {
    view_specimens(x, ...)
  }
)
