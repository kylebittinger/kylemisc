context("Factors")

test_that("Values set to NA when levels are removed", {
  g <- factor(c(1,1,1,2,2,2))
  expect_equal(remove_levels(g, "1"), factor(c(NA,NA,NA,2,2,2)))
})

test_that("Removing unused levels does not affect values", {
  g <- factor(c(1,1,1,2,2,2), levels=c(1,2,3))
  expect_equal(as.numeric(remove_levels(g, "3")), as.numeric(g))
})

test_that("Renaming levels preserves their order", {
  g <- factor(c(1,1,1,2,2,2))
  expect_equal(levels(rename_levels(g, "2", "b")), c("1", "b"))
})

test_that("Multiple levels can be renamed", {
  g <- factor(c(1,1,1,2,2,2))
  expect_equal(
    levels(rename_levels(g, c("2", "1"), c("two", "one"))), 
    c("one", "two"))
})

test_that("New levels are added after existing levels", {
  g <- factor(c(2,2,2,3,3,3))
  expect_equal(levels(add_levels(g, 1)), c("2", "3", "1"))
})

test_that("Merging factors uses values from first factor unless it is NA", {
  g <- factor(c(NA, NA, 2, 3, 3, 3))
  h <- factor(c( 2,  2, 2, 3, 2, 2))
  expect_equal(merge_factors(g, h), factor(c(2, 2, 2, 3, 3, 3)))
})

test_that("New levels from merged factor placed at end", {
  g <- factor(c(NA, NA, 2, 3, 3, 3))
  h <- factor(c( 1,  2, 2, 3, 2, 2))
  expect_equal(
    merge_factors(g, h), 
    factor(c(1, 2, 2, 3, 3, 3), levels=c(2, 3, 1)))
})

test_that("Paste factors works for simple case", {
  g <- factor(c( 2, 3))
  h <- factor(c( 5, 6))
  expect_equal(paste_factors(g, h), factor(c("2 5", "3 6")))
})

test_that("Paste factors uses an empty string if second factor is blank", {
  g <- factor(c( 2,  3))
  h <- factor(c( 5, NA))
  expect_equal(paste_factors(g, h), factor(c("2 5", "3")))
})

test_that("Paste factors uses an empty string if first factor is blank", {
  g <- factor(c( 2, NA))
  h <- factor(c( 5,  6))
  expect_equal(paste_factors(g, h), factor(c("2 5", "6")))
})

test_that("Paste factors favors value of first factor if both are blank", {
  g <- factor(c( 2, NA))
  h <- factor(c( 5, ""))
  expect_equal(paste_factors(g, h), factor(c("2 5", NA)))
  
  g <- factor(c( 2, ""))
  h <- factor(c( 5, NA))
  expect_equal(paste_factors(g, h), factor(c("2 5", "")))
})