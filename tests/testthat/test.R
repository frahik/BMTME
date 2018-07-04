library(testthat)
library(BMTME)

context('My first test')

test_that('Ignore this title', {
  # data('Wheat_IBCF')
  # data('Year_IBCF')
  #
  # M <- getMatrixForm(Wheat_IBCF)
  # Tidy <- getTidyForm(M)
  #
  # M.Y <- getMatrixForm(Year_IBCF, onlyTrait = T)
  # Tidy.Y <- getTidyForm(M.Y, onlyTrait = T)

  expect_equal(1, 1)
})