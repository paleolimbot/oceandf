
#' Read ODF header information
#'
#' @inheritParams read_odf
#' @param which The header name (e.g., "PARAMETER_HEADER"). Most ODF
#'   files have an ODF_HEADER, a CRUISE_HEADER, EVENT_HEADER,
#'   INSTRUMENT_HEADER, HISTORY_HEADER, PARAMETER_HEADER, and
#'   RECORD_HEADER.
#' @param n_header The starting guess for number of header lines.
#' @param header A previously read value obtained from [odf_parse_header()].
#' @param header_lines A previously read value obtained from
#'   [read_odf_header_lines()].
#' @param ... Overrides for the default column types.
#'
#' @export
#'
#' @examples
#' odf_file <- odf_example("CTD_98911_10P_11_DN.ODF")
#' read_odf_header(odf_file)
#' read_odf_header_tbl(odf_file, "CRUISE_HEADER")
#'
read_odf_header <- function(file,
                            header = odf_parse_header(file, file_encoding = file_encoding),
                            file_encoding = "latin1") {
  header_names <- names(header)
  names(header_names) <- header_names

  if (missing(file)) {
    file <- "<unknown file>"
  }

  lapply(
    header_names,
    function(x) read_odf_header_tbl(file = file, which = x, header = header)
  )
}

#' @rdname read_odf_header
#' @export
read_odf_parameter_header <- function(file, col_types = NULL,
                                      header = odf_parse_header(file, file_encoding = file_encoding),
                                      file_encoding = "latin1") {
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
read_odf_header_tbl <- function(file, which, col_types = NULL,
                                header = odf_parse_header(file, file_encoding = file_encoding),
                                file_encoding = "latin1") {
  if (!isTRUE(which %in% names(header))) {
    warning(glue::glue("Header '{ which[1] }' is missing."), immediate. = TRUE)
    return(tibble::tibble())
  }

  if (missing(file)) {
    file <- "<unknown file>"
  }

  tbls <- lapply(header[[which]], odf_header_as_tibble)
  tbl <- vctrs::vec_rbind(!!! tbls)

  if (is.null(col_types)) {
    col_types <- odf_header_cols_default()
    col_types$cols <-
      col_types$cols[intersect(names(col_types$cols), names(tbl))]
  }

  is_list <- vapply(tbl, is.list, logical(1))

  # this line throws important warnings that are not useful as implemented
  # use withCallingHandlers to give these warnings some context
  withCallingHandlers(
    tbl[!is_list] <- readr::type_convert(tbl[!is_list], col_types),
    warning = function(w) {
      w_text <- paste0(w$message, collapse = "\n")
      warning(
        glue::glue("Parse error in { file }/{ which }:\n{ w_text }"),
        call. = FALSE,
        immediate. = TRUE
      )
      tryInvokeRestart("muffleWarning")
    }
  )

  tbl
}

odf_header_as_tibble <- function(x) {
  list_cols <- intersect(names(x), c("EVENT_COMMENTS", "PROCESS"))
  x[list_cols] <- lapply(x[list_cols], list)
  tibble::as_tibble(x, .name_repair = "unique")
}

#' @rdname read_odf_header
#' @export
odf_col_datetime <- function() {
  readr::col_datetime("%d-%b-%Y %H:%M:%OS")
}

#' @rdname read_odf_header
#' @export
odf_header_cols_default <- function(...) {
  cols_default <- list(
    # cruise header
    START_DATE = odf_col_datetime(),
    END_DATE = odf_col_datetime(),

    # event header, history header
    CREATION_DATE = odf_col_datetime(),

    # event header
    ORIG_CREATION_DATE = odf_col_datetime(),
    START_DATE_TIME = odf_col_datetime(),
    END_DATE_TIME = odf_col_datetime(),
    INITIAL_LATITUDE = readr::col_double(),
    INITIAL_LONGITUDE = readr::col_double(),
    END_LATITUDE = readr::col_double(),
    END_LONGITUDE = readr::col_double(),
    MIN_DEPTH = readr::col_double(),
    MAX_DEPTH = readr::col_double(),
    SAMPLING_INTERVAL = readr::col_double(),
    SOUNDING = readr::col_double(),
    DEPTH_OFF_BOTTOM = readr::col_double(),

    # parameter header
    PRINT_FIELD_WIDTH = readr::col_double(),
    PRINT_DECIMAL_PLACES = readr::col_double(),
    # usually numeric but occasionally an old-style Fortran
    # double with D+.. instead of E+..
    DEPTH = readr::col_guess(),
    ANGLE_OF_SECTION = readr::col_double(),
    MAGNETIC_VARIATION = readr::col_double(),
    NUMBER_VALID = readr::col_double(),
    NUMBER_NULL = readr::col_double(),

    # record header
    NUM_HISTORY = readr::col_double(),
    NUM_CYCLE = readr::col_double(),
    NUM_PARAM = readr::col_double(),

    # polynomial cal header
    CALIBRATION_DATE = odf_col_datetime(),
    APPLICATION_DATE = odf_col_datetime(),
    NUMBER_COEFFICIENTS = readr::col_double(),

    .default = readr::col_character()
  )

  args <- rlang::list2(...)
  all_names <- unique(c(names(cols_default), names(args)))
  cols_list <- c(args, cols_default)[all_names]

  do.call(readr::cols, cols_list)
}

#' @rdname read_odf_header
#' @export
odf_parse_header <- function(file,
                             header_lines = read_odf_header_lines(file, file_encoding = file_encoding),
                             file_encoding = "latin1") {
  # extract components
  components <- stringr::str_match(
    header_lines,
    "^(\\s*)([A-Za-z0-9_]+\\s*=\\s*)?'?\\s*(.*?)\\s*'?\\s*,?\\s*$"
  )

  whitespace <- components[, 2]
  name <- stringr::str_remove(components[, 3], "\\s*=\\s*$")
  name[is.na(name)] <- ""
  value <- components[, 4]

  # Can model with two levels of whitespace: one for top level headers
  # and one for everything else OR declare a new top-level header
  # whenever the indentation decreases. The second appears to be
  # more robust with respect to slightly mangled files.

  n_whitespace <- stringr::str_length(whitespace)
  # is_top_header <- (n_whitespace == min(n_whitespace)) & (header_lines != whitespace)
  is_top_header <- c(TRUE, diff(n_whitespace) < 0)
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
read_odf_header_lines <- function(file, n_header = 1000,
                                  file_encoding = "latin1") {
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
