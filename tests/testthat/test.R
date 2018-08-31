library(testthat)
library(BMTME)

context('Fit and predict BME model with Mada data')

test_that('Fit and predict Mada data', {
  data("WheatMadaToy")

  LG <- cholesky(genoMada)
  ZG <- model.matrix(~0 + as.factor(phenoMada$GID))
  Z.G <- ZG %*% LG

  Y <- as.matrix(phenoMada[, -c(1)])

  fm <- BME(Y = Y, Z1 = Z.G, nIter = 100, burnIn = 50, thin = 2, bs = 50)

  pheno <- data.frame(GID = phenoMada[, 1], Env = '', Response = phenoMada[, 3])
  CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)

  pm <- BME(Y = Y, Z1 = Z.G, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV)
  pm_parallel <- BME(Y = Y, Z1 = Z.G, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV, parallelCores = 2)

  expect_is(fm, 'BME')
  expect_output(str(fm), 'List of 17')
  expect_is(fm$Y, 'matrix')

  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMECV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')

  expect_output(str(pm_parallel), 'List of 6')
  expect_is(pm_parallel, 'BMECV')
  expect_is(pm_parallel$results, 'data.frame')
  expect_is(pm_parallel$executionTime, 'numeric')
})

context('Fit and predict BMTME model with Iranian data')

test_that('Fit and predict Iranian data', {
  data("WheatIranianToy")

  LG <- cholesky(genoIranianToy)
  ZG <- model.matrix(~0 + as.factor(phenoIranianToy$GID))
  Z.G <- ZG %*% LG
  Z.E <- model.matrix(~0 + as.factor(phenoIranianToy$Env))
  ZEG <- model.matrix(~0 + as.factor(phenoIranianToy$GID):as.factor(phenoIranianToy$Env))
  G2 <- kronecker(diag(length(unique(phenoIranianToy$Env))), data.matrix(genoIranianToy))
  LG2 <- cholesky(G2)
  Z.EG <- ZEG %*% LG2
  Y <- as.matrix(phenoIranianToy[, -c(1, 2)])

  fm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 100, burnIn = 50, thin = 2, bs = 50)

  pheno <- data.frame(GID = phenoIranianToy[, 1], Env = phenoIranianToy[, 2], Response = phenoIranianToy[, 3])

  CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)

  pm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV)
  pm_parallel <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV, parallelCores = 2)

  expect_is(fm, 'BMTME')
  expect_output(str(fm), 'List of 19')
  expect_is(fm$Y, 'matrix')

  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMTMECV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')

  expect_output(str(pm_parallel), 'List of 6')
  expect_is(pm_parallel, 'BMTMECV')
  expect_is(pm_parallel$results, 'data.frame')
  expect_is(pm_parallel$executionTime, 'numeric')
})


