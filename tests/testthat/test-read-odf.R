
test_that("read_odf() works", {
  file <- odf_example("CTD_98911_10P_11_DN.ODF")
  tbl <- read_odf(file)
  expect_identical(
    colnames(tbl),
    c("PRES", "TEMP", "COND", "PSAL", "POTM", "SIGP")
  )
})
