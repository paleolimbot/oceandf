
#' Read 'ODF' files
#'
#' ODF (Ocean Data) files are whitespace-delimited files
#' whose column names and are specified in a well-formatted header.
#'
#' @param file A file, URL, or connection. Files ending in .gz, .bz2, .xz, or
#'   .zip will be automatically uncompressed; URLs will be automatically
#'   downloaded. See [readr::read_lines()] for a full description of how
#'   this parameter is interpreted.
#' @param n_max Maximum number of rows to read.
#'   [read_odf_header_lines()].
#' @param parameter_header A previously read value obtained from
#'   [read_odf_parameter_header()].
#' @param col_names A vector of column names or `NULL` to guess
#'   using [odf_guess_col_names()]
#' @param col_types A [readr::cols()] spec or `NULL` to guess
#'   using [odf_guess_col_types()]
#' @param file_encoding The encoding used to encode the file. The default
#'   (latin1) is used to prevent an error involving invalid characters
#'   that are common in ODF files.
#'
#' @export
#'
#' @examples
#' odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")
#' read_odf(odf_file)
#'
#' odf_guess_col_names(odf_file)
#' odf_guess_col_types(odf_file)
#'
read_odf <- function(file, col_names = NULL, col_types = NULL,
                     n_max = -1,
                     file_encoding = "latin1") {
  header_lines <- read_odf_header_lines(file, file_encoding = file_encoding)
  header <- read_odf_header(file, header_lines)

  # don't parse into a nice tibble unless it's needed to guess
  if (is.null(col_names) || is.null(col_types)) {
    parameter_header <- read_odf_parameter_header(header = header)
  }

  col_names <- col_names %||%
    odf_guess_col_names(parameter_header = parameter_header)
  col_types <- col_types %||%
    odf_guess_col_types(parameter_header = parameter_header, col_names = col_names)

  # can't use readr::read_table() because quoting is not supported
  tbl <- utils::read.table(
    file,
    colClasses = "character",
    col.names = col_names,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    skip = length(header_lines) + 1,
    nrows = n_max, fileEncoding = file_encoding
  )

  tibble::as_tibble(
    readr::type_convert(
      tbl,
      col_types = col_types
    )
  )
}

#' @rdname read_odf
#' @export
odf_guess_col_names <- function(file,
                                parameter_header = read_odf_parameter_header(
                                  file,
                                  file_encoding = file_encoding
                                ),
                                file_encoding = "latin1") {
  if ("CODE" %in% names(parameter_header)) {
    names <- parameter_header$CODE
  } else if ("NAME" %in% names(parameter_header)) {
    names <- parameter_header$NAME
  } else {
    names <- rep("", nrow(parameter_header))
  }

  vctrs::vec_as_names(names, repair = "unique")
}

#' @rdname read_odf
#' @export
odf_guess_col_types <- function(file,
                                col_names = odf_guess_col_names(parameter_header = parameter_header),
                                parameter_header = read_odf_parameter_header(
                                  file,
                                  file_encoding = file_encoding
                                ),
                                file_encoding = "latin1") {
  type_names <- parameter_header$TYPE

  readr_types <- list(
    DOUB = readr::col_double(),
    INTE = readr::col_integer(),
    SYTM = odf_col_datetime()
  )[type_names]

  col_name_sytm <- grepl("SYTM", col_names)
  readr_type_null <- vapply(readr_types, is.null, logical(1))

  readr_types[readr_type_null] <- list(readr::col_guess())
  readr_types[readr_type_null & col_name_sytm] <- list(odf_col_datetime())

  names(readr_types) <- col_names
  do.call(readr::cols, readr_types)
}
