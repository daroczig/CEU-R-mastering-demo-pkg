test_that("make sure there is a EUR symbol at the beginning", {
  expect_equal(substr(euro(4), 1, 1), '€')
})

test_that('actual output', {
  expect_equal(euro(4), '€4')
  expect_equal(euro(4000), '€4,000')
  expect_equal(euro(4.221214132131), '€4.22')
})
