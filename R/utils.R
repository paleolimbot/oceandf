
header_lines <- function(file, end_header_function, n_header = 100,
                         file_encoding = "UTF-8") {
  stopifnot(n_header > 0)

  lines <- readr::read_lines(
    file,
    n_max = n_header,
    locale = readr::locale(encoding = file_encoding)
  )
  end_header <- end_header_function(lines)

  while ((length(lines) == n_header) && !any(end_header)) {
    n_header <- n_header * 2
    lines <- readr::read_lines(
      file,
      n_max = n_header,
      locale = readr::locale(encoding = file_encoding)
    )
    end_header <- end_header_function(lines)
  }

  if (!any(end_header)) {
    abort(glue("Can't find end of header in '{ file }'."))
  }

  lines[seq_len(which(end_header)[1] - 1)]
}
