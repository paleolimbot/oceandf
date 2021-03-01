
#' Read 'ODF' files
#'
#' ODF (Ocean Data) files are whitespace-delimited files
#' whose column names and are specified in a loosely formatted header.
#'
#' @param file A file, URL, or connection. Files ending in .gz, .bz2, .xz, or
#'   .zip will be automatically uncompressed; URLs will be automatically
#'   downloaded. See [readr::read_lines()] for a full description of how
#'   this parameter is interpreted.
#' @param n_max Maximum number of rows to read.
#' @param header A previously read value obtained from [read_odf_header()].
#' @param header_lines A previously read value obtained from
#'   [read_odf_header_lines()].
#' @param parameter_header A previously read value obtained from
#'   [read_odf_parameter_header()].
#' @param col_names A vector of column names or `NULL` to guess
#'   using [odf_guess_col_names()]
#' @param col_types A [readr::cols()] spec or `NULL` to guess
#'   using [odf_guess_col_types()]
#' @param n_header The starting guess for number of header lines.
#' @param file_encoding The encoding used to encode the file. The default
#'   (windows-1252) reflects a guess based on a number of example ODF files.
#'
#' @export
#'
#' @examples
#' odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")
#' read_odf(odf_file)
#'
#' odf_guess_col_names(odf_file)
#' odf_guess_col_types(odf_file)
#' read_odf_parameter_header(odf_file)
#'
#' str(read_odf_header_lines(odf_file))
#' str(read_odf_header(odf_file))
#'
read_odf <- function(file, col_names = NULL, col_types = NULL,
                     n_max = -1,
                     file_encoding = "windows-1252") {
  header_lines <- read_odf_header_lines(file, file_encoding = file_encoding)
  header <- read_odf_header(file, header_lines)
  parameter_header <- read_odf_parameter_header(header = header)

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
    readr::type_convert(tbl, col_types = col_types)
  )
}

#' @rdname read_odf
#' @export
odf_guess_col_names <- function(file,
                                parameter_header = read_odf_parameter_header(
                                  file,
                                  file_encoding = file_encoding
                                ),
                                file_encoding = "windows-1252") {
  parameter_header$NAME
}

#' @rdname read_odf
#' @export
odf_guess_col_types <- function(file,
                                col_names = odf_guess_col_names(parameter_header = parameter_header),
                                parameter_header = read_odf_parameter_header(
                                  file,
                                  file_encoding = file_encoding
                                ),
                                file_encoding = "windows-1252") {
  type_names <- parameter_header$TYPE

  readr_types <- list(
    DOUB = readr::col_double(),
    INTE = readr::col_integer(),
    SYTM = readr::col_datetime("%d-%b-%Y %H:%M:%OS")
  )[type_names]

  readr_type_null <- vapply(readr_types, is.null, logical(1))
  readr_types[readr_type_null] <- list(readr::col_guess())
  names(readr_types) <- col_names
  do.call(readr::cols, readr_types)
}

#' @rdname read_odf
#' @export
read_odf_parameter_header <- function(file,
                                      header = read_odf_header(file, file_encoding = file_encoding),
                                      file_encoding = "windows-1252") {
  params <- lapply(header$PARAMETER_HEADER, tibble::as_tibble)
  params_tbl <- vctrs::vec_rbind(!!! params)
  params_tbl[] <- lapply(params_tbl, readr::parse_guess)
  params_tbl
}

#' @rdname read_odf
#' @export
read_odf_header <- function(file,
                            header_lines = read_odf_header_lines(file, file_encoding = file_encoding),
                            file_encoding = "windows-1252") {
  # extract components
  components <- stringr::str_match(
    header_lines,
    "^(\\s*)([A-Za-z0-9_]+\\s*=\\s*)?'?\\s*(.*?)\\s*'?\\s*,?\\s*$"
  )

  whitespace <- components[, 2]
  name <- stringr::str_remove(components[, 3], "\\s*=\\s*$")
  name[is.na(name)] <- ""
  value <- components[, 4]

  # model with two levels of whitespace: one for top level headers
  # one for everything else
  n_whitespace <- stringr::str_length(whitespace)
  is_top_header <- (n_whitespace == min(n_whitespace)) & (header_lines != whitespace)
  top_headers <- value[is_top_header]
  which_top_header <- cumsum(is_top_header)

  parsed <- rep(list(NULL), length(top_headers))
  for (i in seq_along(top_headers)) {
    value_i <- value[which_top_header == i][-1]
    names(value_i) <- name[which_top_header == i][-1]
    parsed[[i]] <- collapse_by_name(value_i)
  }

  names(parsed) <- top_headers
  collapse_by_name(parsed)
}

#' @rdname read_odf
#' @export
read_odf_header_lines <- function(file, n_header = 1000,
                                  file_encoding = "windows-1252") {
  header_lines(
    file,
    function(x) grepl("\\s*-- DATA --\\s*", x),
    n_header = n_header,
    file_encoding = file_encoding
  )
}

collapse_by_name <- function(x) {
  out_names <- unique(names(x))
  out <- rep(list(NULL), length(out_names))
  for (i in seq_along(out_names)) {
    items <- unname(x[names(x) == out_names[i]])
    out[[i]] <- items
  }

  names(out) <- out_names
  out
}
