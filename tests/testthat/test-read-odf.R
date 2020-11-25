
test_that("read_odf_header() works", {
  skip_if_not(has_test_odf())

  file <- test_odf_file()
  header <- read_odf_header_lines(file)
  expect_length(header, 881)
  expect_true(readr::read_lines(file, skip = 881, n_max = 1) == "-- DATA --")

  # force detection of header size
  header <- read_odf_header_lines(file, n_header = 10)
  expect_length(header, 881)

  # invalid file
  tmpfile <- tempfile()
  write("totally bogus information", tmpfile)
  expect_error(read_odf_header_lines(tmpfile), "Can't find")
  unlink(tmpfile)
})

test_that("read_odf_header() works", {
  skip_if_not(has_test_odf())

  file <- test_odf_file()
  header <- read_odf_header(file)
  expect_true(all(c("PARAMETER_HEADER", "ODF_HEADER") %in% names(header)))
})

test_that("read_odf_colmeta() works", {
  skip_if_not(has_test_odf())

  file <- test_odf_file()
  colmeta <- read_odf_colmeta(file)
  expect_is(colmeta, "tbl_df")
})

test_that("read_odf() works", {
  skip_if_not(has_test_odf())

  file <- test_odf_file()
  tbl <- read_odf(file)
  expect_is(tbl$SYTM_01, "POSIXct")
})
