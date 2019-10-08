
context("Structure of objects returned")

key <- Sys.getenv('NASS_KEY')


test_that("get_param_values returns objects of correct size and type", {
  r <- get_param_values(key, "short_desc")
  expect_equal(class(r), 'list')
  expect_equal(class(r[[1]]), 'list')
  expect_equal(class(r[[1]][[1]]), 'character')
  expect_equal(length(r), 1)
  expect_true(length(r[[1]]) > 0)
  expect_equal(length(r[[1]][[1]]), 1)
})
