context("Google")

test_that("google calls work", {
  expect_equal(dim(ngrami("dog", year_start = 1950, year_end = 2020)), c(70, 4))
  expect_equal(dim(ngram(c("hacker", "programmer"),
                         corpus = c("eng_2012", "eng_us_2012"),
                         year_start = 1950, year_end=2008)),
               dim(hacker))
  expect_equal(dim(ngramw(c("dog", "cat"), year_start  = 1950)), c(70, 4))
})
