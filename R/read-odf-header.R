
#' Read ODF header information
#'
#' @inheritParams read_odf
#' @param which The header name (e.g., "PARAMETER_HEADER")
#' @param n_header The starting guess for number of header lines.
#' @param header A previously read value obtained from [read_odf_header()].
#' @param header_lines A previously read value obtained from
#'
#' @export
#'
#' @examples
#' odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")
#' read_odf_parameter_header(odf_file)
#'
#' read_odf_header_tbl(odf_file, "CRUISE_HEADER")
#' str(read_odf_header_lines(odf_file))
#' str(read_odf_header(odf_file))
#'
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

#' @rdname read_odf_header
#' @export
read_odf_parameter_header <- function(file, col_types = readr::cols(),
                                      header = read_odf_header(file, file_encoding = file_encoding),
                                      file_encoding = "windows-1252") {
  read_odf_header_tbl(
    file,
    "PARAMETER_HEADER",
    col_types = col_types,
    header = header,
    file_encoding = file_encoding
  )
}

#' @rdname read_odf_header
#' @export
read_odf_header_tbl <- function(file, which, col_types = readr::cols(),
                                header = read_odf_header(file, file_encoding = file_encoding),
                                file_encoding = "windows-1252") {
  params <- lapply(header[[which]], tibble::as_tibble)
  params_tbl <- vctrs::vec_rbind(!!! params)
  readr::type_convert(params_tbl, col_types)
}

#' @rdname read_odf_header
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
