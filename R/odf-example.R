
#' ODF example files
#'
#' @param path An ODF example name
#'
#' @return The full path to the file
#' @export
#'
#' @examples
#' odf_example("CTD_98911_10P_11_DN.ODF")
#' odf_example("CTD_PRD2002-001_26_1_DN.ODF")
#' odf_example("CTD_PRD2003001_1482_313_UP.ODF")
#' odf_example("MADCP_98911_1281_0505-78_7200.ODF")
#'
odf_example <- function(path) {
  stopifnot(length(path) == 1)

  file <- system.file("extdata", path, package = "oceandf")
  if (identical(file, "")) {
    abort(glue("odf example '{ path }' does not exist"))
  }

  file
}
