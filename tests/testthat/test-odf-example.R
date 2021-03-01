
test_that("odf_example() works", {
  expect_identical(
    basename(odf_example("CTD_98911_10P_11_DN.ODF")),
    "CTD_98911_10P_11_DN.ODF"
  )

  expect_error(odf_example("bogus"), "does not exist")
})
