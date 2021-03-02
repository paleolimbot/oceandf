
test_that("read_odf() works", {
  file <- odf_example("CTD_98911_10P_11_DN.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("PRES", "TEMP", "COND", "PSAL", "POTM", "SIGP")
  )
  expect_true(all(vapply(tbl, is.double, logical(1))))

  file <- odf_example("CTD_PRD2002-001_26_1_DN.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("PRES_01", "DEPH_01", "TEMP_01", "CNDC_01", "PSAL_01", "SIGT_01")
  )
  expect_true(all(vapply(tbl, is.double, logical(1))))

  file <- odf_example("CTD_PRD2003001_1482_313_UP.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("PRES_01", "TEMP_01", "CRAT_01", "PSAL_01", "SIGT_01", "FLOR_01")
  )

  file <- odf_example("MADCP_98911_1281_0505-78_7200.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("EWCT_01", "NSCT_01", "PRES_01", "SYTM_01")
  )
  expect_vector(tbl$EWCT_01, double())
  expect_vector(tbl$NSCT_01, double())
  expect_vector(tbl$PRES_01, double())
  expect_vector(tbl$SYTM_01, vctrs::new_datetime(tzone = "UTC"))
})
