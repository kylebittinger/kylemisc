context("Apply")

test_that("Names and values are passed to function by lapply_names", {
  expect_equal(lapply_names(list(A=1, B=2), paste), list(A="A 1", B="B 2"))
})
