#' @name specimens-class
#'
#' @title An S4 class containing specimen collections.
#'
#' @description
#' A list of collections of specimens considering duplicated specimens and
#' history of determination.
#' This package was targeting collection databases from the perspective of a
#' collector and designed for collections of herbarium specimens but also
#' applicable to collections of other organisms, for instance zoological
#' collections as well.
#'
#' @slot collections A data frame or an object of class [sf][sf::sf] including
#'     variables associated to a collection, meaning a sample of a species in a
#'     locality. Mandatory columns are here **coll_nr** (unique identifier
#'     of the collection), **leg** (name of collector), and **coll_date**
#'     (date of collection set as class [Date][as.Date]).
#' @slot specimens A data frame with information specifical to single specimens.
#'     It may be one specimen per collection or many (duplicates). This database
#'     list can contain the final destination of the specimens, in the case of
#'     plant specimens, the herbarium. Mandatory columns in this table are
#'     \strong{spec_id} (unique identifier for specimens) and **coll_nr**
#'     (a foreign key pointing to the homonimous column in slot 'collections').
#' @slot history A data frame containing the history of determination. In the
#'     case of herbarium specimens, the update of the species determination.
#'     Mandatory columns are **fid** (unique identifier), **spec_id**
#'     (foreign key pointing to homonimous columns in slot 'specimens'),
#'     **det** (the name of determining person), **det_date**
#'     (date of update set as class [Date][as.Date]), and **taxon_name**
#'     (the scientific name of specimen according to the determinator).
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @examples
#' showClass("specimens")
#'
#' ## Create an empty object
#' spec <- new("specimens")
#' @rdname specimens-class
setOldClass("data.frame")
setOldClass("sf")
setClassUnion("df_sf", c("data.frame", "sf"))

#' @rdname specimens-class
#' @exportClass specimens
setClass("specimens",
  # Definition of slots
  slots = c(
    collections = "df_sf",
    specimens = "data.frame",
    history = "data.frame"
  ),
  # Prototype
  prototype = list(
    collections = data.frame(
      coll_nr = integer(),
      leg = character(),
      coll_date = as.Date(NULL),
      stringsAsFactors = FALSE
    ),
    specimens = data.frame(
      spec_id = integer(),
      coll_nr = integer()
    ),
    history = data.frame(
      fid = integer(),
      spec_id = integer(),
      det = character(),
      det_date = as.Date(NULL),
      taxon_name = character(),
      stringsAsFactors = FALSE
    )
  ),
  # Validity procedures
  validity = function(object) {
    ## slot collections
    col_names <- c("coll_nr", "leg", "coll_date")
    if (!all(col_names %in% names(object@collections))) {
      col_names <- col_names[!col_names %in% names(object@collections)]
      return(paste0(
        "Following columns are missing in slot 'collections': '",
        paste0(col_names, collapse = "' '"), "'."
      ))
    }
    if (any(duplicated(object@collections$coll_nr))) {
      return(paste(
        "Duplicated values for 'coll_nr' in slot 'collections'",
        "are not allowed.'"
      ))
    }
    if (class(object@collections$coll_date) != "Date") {
      return(paste(
        "Column 'coll_date' in slot 'collections'",
        "have to be of class 'Date'."
      ))
    }
    ## slot specimens
    col_names <- c("spec_id", "coll_nr")
    if (!all(col_names %in% names(object@specimens))) {
      col_names <- col_names[!col_names %in% names(object@specimens)]
      return(paste0(
        "Following columns are missing in slot 'specimens': '",
        paste0(col_names, collapse = "' '"), "'."
      ))
    }
    if (any(duplicated(object@specimens$spec_id))) {
      return(paste(
        "Duplicated values for 'spec_id' in slot 'specimens'",
        "are not allowed.'"
      ))
    }
    if (!all(object@specimens$coll_nr %in% object@collections$coll_nr)) {
      no_fk <- with(object@specimens, coll_nr[!coll_nr %in%
        object@collections$coll_nr])
      return(paste0(
        "Following values for 'coll_nr' in slot 'specimens' ",
        "are missing in slot 'collections': '",
        paste0(no_fk, collapse = "' '"), "'."
      ))
    }
    # slot history
    col_names <- c("fid", "spec_id", "det", "det_date", "taxon_name")
    if (!all(col_names %in% names(object@history))) {
      col_names <- col_names[!col_names %in% names(object@history)]
      return(paste0(
        "Following columns are missing in slot 'history': '",
        paste0(col_names, collapse = "' '"), "'."
      ))
    }
    if (any(duplicated(object@history$fid))) {
      return(paste(
        "Duplicated values for 'fid' in slot 'history'",
        "are not allowed.'"
      ))
    }
    if (!all(object@history$spec_id %in% object@specimens$spec_id)) {
      no_fk <- with(object@history, spec_id[!spec_id %in%
        object@specimens$spec_id])
      return(paste0(
        "Following values for 'spec_id' in slot 'history' ",
        "are missing in slot 'specimens': '",
        paste0(no_fk, collapse = "' '"), "'."
      ))
    }
    if (class(object@history$det_date) != "Date") {
      return("Column 'det_date' in slot 'history' have to be of class 'Date'.")
    }
  }
)
