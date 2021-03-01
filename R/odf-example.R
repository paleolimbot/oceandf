
#' ODF example files
#'
#' @param path An ODF example name
#'
#' @return The full path to the file
#' @export
#'
#' @examples
#' odf_example("CTD_98911_10P_11_DN.ODF")
#'
odf_example <- function(path) {
  stopifnot(length(path) == 1)

  file <- system.file("extdata", path, package = "oceandf")
  if (identical(file, "")) {
    abort(glue("odf example '{ path }' does not exist"))
  }

  file
}
