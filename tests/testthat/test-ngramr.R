context("Google")

# h <- ngram(c("hacker", "programmer"), corpus = c("eng_2012", "eng_us_2012"), year_start = 1950, year_end=2008)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("google call", {
  expect_equal(dim(ngrami("dog", year_start = 1950, year_end = 2020)), c(70, 4))
})
