#' @name view_table
#'
#' @rdname view_table
#'
#' @title Display an interactive table using shiny
#'
#' @description
#' Table with specimens in an interactive session.
#'
#' Note that the shiny app will freeze your R session as long as the app is
#' running. The app will stop if you close the respective tab.
#'
#' @param x A [specimens-class] object.
#' @param add_cols A character vector with the names of columns to be displayed
#'     in the table.
#' @param date_format A character value indicating the format used for the
#'     collection date. It is passed to the function [format.Date()].
#' @param ... Further arguments passed among methods.
#'
#' @export
view_table <- function(x, ...) {
  UseMethod("view_table", x)
}

#' @rdname view_table
#'
#' @aliases view_table,specimens-method
#'
#' @method view_table specimens
#'
#' @examples
#' \dontrun{
#' view_table(churo_survey)
#' }
#'
#' @export
view_table.specimens <- function(x,
                                 add_cols = c(
                                   "coll_date", "leg",
                                   "taxon_name"
                                 ),
                                 date_format = "%d.%m.%Y",
                                 ...) {
  x <- as(x, "data.frame")
  # format columns
  x$coll_date <- format(x$coll_date, date_format)
  # app
  ui <- basicPage(
    ## h2("The mtcars data"),
    dataTableOutput("specimens")
  )
  server <- function(input, output, session) {
    output$specimens <- renderDataTable({
      x[, unique(c("spec_id", "coll_nr", add_cols))]
    })
    session$onSessionEnded(stopApp)
  }
  shinyApp(ui, server)
}
