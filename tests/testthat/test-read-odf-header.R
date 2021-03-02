
test_that("read_odf_header_lines() works", {
  file <- odf_example("CTD_98911_10P_11_DN.ODF")
  header <- read_odf_header_lines(file)
  expect_length(header, 186)
  expect_equal(readr::read_lines(file, skip = 186, n_max = 1), " -- DATA --")

  # force detection of header size
  header <- read_odf_header_lines(file, n_header = 10)
  expect_length(header, 186)

  # invalid file
  tmpfile <- tempfile()
  write("totally bogus information", tmpfile)
  expect_error(read_odf_header_lines(tmpfile), "Can't find")
  unlink(tmpfile)
})

test_that("read_odf_header() works", {
  file <- odf_example("CTD_98911_10P_11_DN.ODF")
  header <- read_odf_header(file)
  expect_true(all(c("PARAMETER_HEADER", "ODF_HEADER") %in% names(header)))
})

test_that("read_odf_header() works when whitespace isn't constant", {
  file <- odf_example("CTD_PRD2002-001_26_1_DN.ODF")
  expect_vector(read_odf_header(file)$INSTRUMENT_HEADER, list())
})

test_that("read_odf_parameter_header() works", {
  file <- odf_example("CTD_98911_10P_11_DN.ODF")
  colmeta <- read_odf_parameter_header(file)
  expect_is(colmeta, "tbl_df")
})

test_that("read_odf_header_tbl() works", {

  files <- c(
    odf_example("CTD_98911_10P_11_DN.ODF"),
    odf_example("CTD_PRD2002-001_26_1_DN.ODF"),
    odf_example("CTD_PRD2003001_1482_313_UP.ODF"),
    odf_example("MADCP_98911_1281_0505-78_7200.ODF")
  )

  expect_warning(
    read_odf_header_tbl(files[1], "NONEXISTENT_HEADER"),
    "is missing"
  )

  for (file in files) {
    odf <- read_odf_header_tbl(file, "ODF_HEADER")
    expect_vector(odf, tibble::tibble(FILE_SPECIFICATION = character()))

    cruise <- read_odf_header_tbl(file, "CRUISE_HEADER")
    expect_setequal(
      names(cruise),
      c("COUNTRY_INSTITUTE_CODE", "CRUISE_NUMBER", "ORGANIZATION",
        "CHIEF_SCIENTIST", "START_DATE", "END_DATE", "PLATFORM", "CRUISE_NAME",
        "CRUISE_DESCRIPTION")
    )
    expect_vector(cruise$START_DATE, vctrs::new_datetime(tz = "UTC"))
    expect_vector(cruise$END_DATE, vctrs::new_datetime(tz = "UTC"))


    event <- read_odf_header_tbl(file, "EVENT_HEADER")
    expect_setequal(
      names(event),
      c("DATA_TYPE", "EVENT_NUMBER", "EVENT_QUALIFIER1", "EVENT_QUALIFIER2",
        "CREATION_DATE", "ORIG_CREATION_DATE", "START_DATE_TIME", "END_DATE_TIME",
        "INITIAL_LATITUDE", "INITIAL_LONGITUDE", "END_LATITUDE", "END_LONGITUDE",
        "MIN_DEPTH", "MAX_DEPTH", "SAMPLING_INTERVAL", "SOUNDING", "DEPTH_OFF_BOTTOM",
        "EVENT_COMMENTS")
    )
    expect_vector(event$START_DATE_TIME, vctrs::new_datetime(tz = "UTC"))
    expect_vector(event$END_DATE_TIME, vctrs::new_datetime(tz = "UTC"))
    expect_true(is.list(event$EVENT_COMMENTS))

    instrument <- read_odf_header_tbl(file, "INSTRUMENT_HEADER")
    expect_setequal(
      names(instrument),
      c("INST_TYPE", "MODEL", "SERIAL_NUMBER", "DESCRIPTION")
    )

    history <- read_odf_header_tbl(file, "HISTORY_HEADER")
    expect_setequal(
      names(history),
      c("CREATION_DATE", "PROCESS")
    )
    expect_vector(history$CREATION_DATE, vctrs::new_datetime(tz = "UTC"))
    expect_true(is.list(history$PROCESS))

    parameter <- read_odf_header_tbl(file, "PARAMETER_HEADER")
    # WMO_CODE can be missing
    expect_true(
      all(
        c("TYPE", "NAME", "UNITS", "NULL_VALUE", "PRINT_FIELD_WIDTH",
          "PRINT_DECIMAL_PLACES", "ANGLE_OF_SECTION", "MAGNETIC_VARIATION",
          "DEPTH", "MINIMUM_VALUE", "MAXIMUM_VALUE", "NUMBER_VALID", "NUMBER_NULL"
        ) %in% names(parameter)
      )
    )
    expect_vector(parameter$PRINT_DECIMAL_PLACES, double())

    record <- read_odf_header_tbl(file, "RECORD_HEADER")
    expect_vector(record$NUM_PARAM, double())
  }
})
