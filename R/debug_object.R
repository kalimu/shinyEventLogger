#' Copying objects to global environment
#'
#' With \code{debug_objects} you can copy an object to the global environment
#' for further debugging or developing.
#'
#' @param ... Named objects to be copy to the global environment.
#' If there is only one unnamed object, its name in the global environment
#' will be the same as the name of the object passed to \code{...}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' if (interactive()) {
#'
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(log_init()),
#'     server = function(input, output) {
#'       set_logging()
#'       debug_objects(mtcars)
#'       debug_objects(df1 = head(mtcars), df2 = head(iris))
#'     }
#'   )
#'
#' }
#' }

debug_objects <- function(...) {

  session <- shiny::getDefaultReactiveDomain()
  input <- session$input

  objects <- eval(list(...))
  object_names <- names(objects)

  if (NROW(objects) == 1 && is.null(object_names))  {

    object_names <- deparse(substitute(...))

  }

  if (NROW(objects) > 1 &&
      (is.null(object_names) || any(object_names == ""))
      ) {

    stop("All objects must be named.")

  }

  Map(objects, object_names,
        f = function(object, object_name) {

          assign(object_name, value = object, envir = .GlobalEnv)

          message("Object named `", object_name,
                  "` was assigned to the global environment for debugging.\n")

  }) # end of walk2

  TRUE

} # end of debug_objects()

#' @export
debug_object <- function(...) {

  debug_objects(...)


}
