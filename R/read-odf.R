
read_odf_header <- function(file, n_header = 1000) {
  stopifnot(n_header > 0)

  lines <- readr::read_lines(file, n_max = n_header)
  end_header <- grepl("\\s*-- DATA --\\s*", lines)

  while ((length(lines) == n_header) && !any(end_header)) {
    n_header <- n_header * 2
    lines <- readr::read_lines(file, n_max = n_header)
    end_header <- grepl("\\s*-- DATA --\\s*", lines)
  }

  if (!any(end_header)) {
    abort(glue("Can't find '-- DATA --' at end of header in '{ file }'.\nIs it an ODF file?"))
  }

  lines[seq_len(which(end_header)[1] - 1)]
}

test_odf_file <- function() {
  Sys.getenv("R_OCEANDF_TEST_ODF", "")
}

has_test_odf <- function() {
  test_odf_file() != ""
}
