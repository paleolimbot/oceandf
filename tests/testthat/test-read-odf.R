
test_that("read_odf_header() works", {
  skip_if_not(has_test_odf())

  file <- test_odf_file()
  header <- read_odf_header(file)
  expect_length(header, 881)
  expect_true(readr::read_lines(file, skip = 881, n_max = 1) == "-- DATA --")

  # force detection of header size
  header <- read_odf_header(file, n_header = 10)
  expect_length(header, 881)

  # invalid file
  tmpfile <- tempfile()
  write("totally bogus information", tmpfile)
  expect_error(read_odf_header(tmpfile), "Can't find")
  unlink(tmpfile)
})
