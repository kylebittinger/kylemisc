context("Permutations")

test_that("Shuffle within groups does not mix groups", {
  g <- factor(c(1,1,1,2,2,2))
  swg <- shuffle_within_groups(c(5,4,6,3,2,1), g)

  expect_equal(sort(swg[1:3]), c(4,5,6))
  expect_equal(sort(swg[4:6]), c(1,2,3))
})

test_that("Shuffle within groups works for unevenly sized groups", {
  g <- factor(c(1,1,1,2,2))
  swg <- shuffle_within_groups(c(5,4,6,3,2), g)

  expect_equal(sort(swg[1:3]), c(4,5,6))
  expect_equal(sort(swg[4:5]), c(2,3))
})

test_that("Shuffle between groups shuffles entire blocks", {
  g <- factor(c(1,1,2,2,3,3))
  sbg <- shuffle_between_groups(c(4,4,5,5,6,6), g)

  expect_equal(sbg[1], sbg[2])
  expect_equal(sbg[3], sbg[4])
  expect_equal(sbg[5], sbg[6])
})

test_that("Shuffle between groups works for unevenly sized blocks", {
  g <- factor(c(1,1,1,2,2,3,3,3))
  sbg <- shuffle_between_groups(c(4,4,4,5,5,6,6,6), g)

  expect_equal(sbg[1], sbg[2])
  expect_equal(sbg[1], sbg[3])

  expect_equal(sbg[4], sbg[5])

  expect_equal(sbg[6], sbg[7])
  expect_equal(sbg[6], sbg[8])
})

test_that("Shuffle between groups fails if vals not constant within groups", {
  g <- factor(c(1,1,2,2,3,3))
  expect_error(shuffle_between_groups(c(4,4,5,6,6,6), c(1,1,2,2,3,3)))
})

test_that("Shuffle between groups preserves levels", {
  g <- factor(c(1,1,2,2,3,3))
  x <- factor(c(4,4,5,5,6,6), levels=c("5", "b", "4", "6", "a"))
  sbg <- shuffle_between_groups(x, g)
  expect_equal(levels(sbg), levels(x))
})

test_that("Block adonis works for simple design", {
  x <- matrix(ncol=4, data=c(
     1,  1,  1,  1,  0,  0, 26, 21, 24, 29, 22, 22,
     0,  1,  1,  0,  0,  1,  4,  2,  2,  3,  6,  2,
    43, 43, 42, 37, 43, 41, 20, 27, 24, 18, 22, 25,
     6,  5,  6, 12,  7,  8,  0,  0,  0,  0,  0,  1))
  d <- vegdist(x)
  s <- data.frame(
    Treatment = gl(2, 6, labels=letters[1:2]),
    Subject = gl(6, 2, labels=letters[3:8]))
  set.seed(1)
  res <- block_adonis(d ~ Treatment, data=s, block_var="Subject", nperm=99)
  expect_equal(res$aov.tab["Treatment", "F.Model"], 185.14, tol=0.1)
  expect_lt(res$aov.tab["Treatment", "Pr(>F)"], 0.5)
})