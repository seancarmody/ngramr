context("Package")
test_that("package data", {
  expect_equal(dim(hacker), c(236, 4))
  expect_equal(class(hacker)[1], "ngram")
  expect_equal(dim(corpuses), c(33, 8))
  expect_equal(dim(corpus_totals), c(12945, 5))
  expect_equal(unlist(corpus_totals[12945,], use.names = FALSE),
               c("es-2019", 2019, 1658430069, 10286019, 24720))
})

test_that("utility functions", {
  expect_equal(chunk(letters, len=4)[[4]], letters[12:15])
})

context("Google")
test_that("google calls", {
  skip_if_offline()
  skip_if(is.null(ngram("dog")), "Google Ngram calls not succeeding.")
  expect_equal(dim( ngrami("dog", year_start = 1950, year_end = 2020)), c(70, 4))
  expect_equal(dim(ngram(c("hacker", "programmer"), corpus = c("en-2012", "en-US-2012"), 
                         year_start = 1950, year_end = 2008)), dim(hacker))
  expect_equal(dim(ngramw(hacker)), c(118, 4))
  expect_equal(dim(ngram(c("military"), corpus = "en-2012", year_start = 1940, 
                         year_end = 2005, smoothing = 0, count = TRUE)), c(66, 5))
})

