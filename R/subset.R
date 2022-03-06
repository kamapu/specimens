#' @name subset
#'
#' @title Subset method for specimens objects
#'
#' @description
#' Creating subsets of [specimens-class] objects by using logical operations.
#'
#' @param x Object of class [specimens-class].
#' @param subset Logical vector or logical operation to apply as subset.
#' @param slot Character value indicating the slot to be used for the subset.
#'     This is not applied to slot 'history'.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return An object of class [specimens-class].
#'
#' @author Miguel Alvarez \email{kamapu@@posteo.de}
#'
#' @aliases subset,specimens-method
#'
#' @exportMethod subset
setMethod(
  "subset", signature(x = "specimens"),
  function(x, subset, slot = "collections", ...) {
    slot <- grep(slot[1], slotNames(x)[slotNames(x) != "history"],
      ignore.case = TRUE
    )
    if (length(slot) == 0) {
      stop("Invalid value for argument 'slot'")
    }
    slot <- slotNames(x)[slot]
    subset <- substitute(subset)
    subset <- eval(subset, slot(x, slot), parent.frame())
    slot(x, slot) <- slot(x, slot)[subset, ]
    return(clean(x))
  }
)
