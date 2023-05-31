test_that("multiplication works", {
  expect_equal(substr(euro(4),1,1), '€')
})

test_that('actual_output',{
  expect_equal(euro(4),"€4")
  expect_equal(euro(4000),"€4,000")
  expect_equal(euro(4.2221231),"€4.22")

})
