test_that("datatable output is produced fine, w/o arguments", {
  expect_equal(nrow(get_usdeurs()), 31)
  expect_equal(ncol(get_usdeurs()), 2)
})

test_that("datatable output is fine with arguments", {
  expect_equal(nrow(get_usdeurs(start_date = "abcd",5678)), 0)
  expect_equal(ncol(get_usdeurs(NA,NA)), 2)
  expect_equal(nrow(get_usdeurs("2023-01-32","2023-010-33")), 0)
  expect_equal(nrow(get_usdeurs("2023-05-01","2023-04-29")),0)
})

test_that("assertion failed: out of boundaries", {
  expect_error(get_usdeurs("2014-01-01","2014-02-01"),
               "Request failed after 3 attempts.")
  expect_error(get_usdeurs(start_date = "2000-01-10",end_date = "2000-12-20"),
               "Request failed after 3 attempts.")
})
