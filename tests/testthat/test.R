library(testthat)
library(BMTME)

context('Partitions test')
test_that('K-Folds', {
  data("WheatMadaToy")
  phenoMada <- (phenoMada[order(phenoMada$GID),])
  pheno <- data.frame(GID = phenoMada[, 1], Response = phenoMada[, 3])

  KFold_Test <- CV.KFold(pheno, set_seed = 123)

  expect_output(str(KFold_Test), 'List of 5')
  expect_is(KFold_Test, 'CrossValidation')
  expect_is(KFold_Test$CrossValidation_list, 'list')
  expect_output(str(KFold_Test$CrossValidation_list), 'List of 5')
  expect_is(KFold_Test$ng, 'numeric')
  expect_is(KFold_Test$Environments, 'character')
  expect_length(KFold_Test$Environments, length(phenoMada$GID))
  expect_is(KFold_Test$Traits, 'character')
  expect_length(KFold_Test$Traits, length(phenoMada$GID))

  data("WheatIranianToy")

  pheno <- data.frame(GID = phenoIranianToy[, 1], Env = phenoIranianToy[, 2], Response = phenoIranianToy[, 3])

  KFold_Test2 <- CV.KFold(pheno, DataSetID = 'GID', K = 5, set_seed = 123)

  expect_output(str(KFold_Test2), 'List of 5')
  expect_is(KFold_Test2, 'CrossValidation')
  expect_is(KFold_Test2$CrossValidation_list, 'list')
  expect_output(str(KFold_Test2$CrossValidation_list), 'List of 5')
  expect_is(KFold_Test2$ng, 'numeric')
  expect_is(KFold_Test2$Environments, 'character')
  expect_length(KFold_Test2$Environments, length(pheno$GID))
  expect_equal(unique(KFold_Test2$Environments), unique(as.character(pheno$Env)))
  expect_is(KFold_Test2$Traits, 'character')
  expect_length(KFold_Test2$Traits, length(pheno$GID))

})

test_that('RandomPartition',{
  data("MaizeToy")
  phenoMaizeToy <- phenoMaizeToy[order(phenoMaizeToy$Env, phenoMaizeToy$Line), ]
  pheno <- data.frame(GID = phenoMaizeToy[, 1], Env = phenoMaizeToy$Env, Response = phenoMaizeToy[, 3])

  RP_Test <- CV.RandomPart(pheno, set_seed = 123)

  expect_output(str(RP_Test), 'List of 6')
  expect_is(RP_Test, 'CrossValidation')
  expect_is(RP_Test$CrossValidation_list, 'list')
  expect_output(str(RP_Test$CrossValidation_list), 'List of 10')
  expect_is(RP_Test$Environments, 'character')
  expect_length(RP_Test$Environments, length(pheno$Env))
  expect_equal(unique(RP_Test$Environments), unique(pheno$Env))
  expect_is(RP_Test$Traits, 'character')
  expect_length(RP_Test$Traits, length(pheno$Env))

  data("WheatIranianToy")
  phenoIranianToy <- phenoIranianToy[order(phenoIranianToy$Env, phenoIranianToy$GID), ]
  pheno <- data.frame(GID = phenoIranianToy[, 1], Env = phenoIranianToy$Env,
                      Trait = rep(colnames(phenoIranianToy)[3:4], each = dim(phenoIranianToy)[1]),
                      Response = c(phenoIranianToy[, 3], phenoIranianToy[, 4]))
  RP_Test2 <- CV.RandomPart(pheno, Traits.testing = 'DTH', set_seed = 123)

  expect_output(str(RP_Test2), 'List of 6')
  expect_is(RP_Test2, 'CrossValidation')
  expect_is(RP_Test2$CrossValidation_list, 'list')
  expect_output(str(RP_Test2$CrossValidation_list), 'List of 10')
  expect_is(RP_Test2$Environments, 'character')
  expect_length(RP_Test2$Environments, length(pheno$Env))
  expect_equal(unique(RP_Test2$Environments), unique(as.character(pheno$Env)))
  expect_is(RP_Test2$Traits, 'character')
  expect_length(RP_Test2$Traits, length(pheno$Env))
})

context('BME function')
test_that('BME function with Mada data with NA values', {
  data("WheatMadaToy")
  phenoMada <- (phenoMada[order(phenoMada$GID),])
  phenoMada[sample.int(50, 15), 2:7] <- NA

  LG <- cholesky(genoMada)
  ZG <- model.matrix(~0 + as.factor(phenoMada$GID))
  Z.G <- ZG %*% LG
  Y <- as.matrix(phenoMada[, -c(1)])

  # Check fitting
  fm <- BME(Y = Y, Z1 = Z.G, nIter = 100, burnIn = 50, thin = 2, bs = 50)

  expect_is(fm, 'BME')
  expect_output(str(fm), 'List of 17')
  expect_is(fm$Y, 'matrix')
  expect_is(fm$yHat, 'matrix')
  expect_equal(dim(fm$Y), dim(fm$yHat))
  expect_gt(fm$NAvalues, 0)
  expect_output(print(fm), 'Multi-Environment Model Fitted with')
  expect_silent(plot(fm, trait = 'PH'))
})

test_that('BME function with Mada data', {
  data("WheatMadaToy")
  phenoMada <- (phenoMada[order(phenoMada$GID),])

  LG <- cholesky(genoMada)
  ZG <- model.matrix(~0 + as.factor(phenoMada$GID))
  Z.G <- ZG %*% LG
  Y <- as.matrix(phenoMada[, -c(1)])

  # Check fitting
  fm <- BME(Y = Y, Z1 = Z.G, nIter = 100, burnIn = 50, thin = 2, bs = 50)

  expect_is(fm, 'BME')
  expect_output(str(fm), 'List of 17')
  expect_is(fm$Y, 'matrix')
  expect_is(fm$yHat, 'matrix')
  expect_equal(dim(fm$Y), dim(fm$yHat))
  expect_equal(fm$NAvalues, 0)
  expect_output(print(fm), 'Multi-Environment Model Fitted with')
  expect_silent(plot(fm, trait = 'PH'))
  expect_is(residuals(fm), 'matrix')
  expect_false(any(is.nan(residuals(fm))))

  pheno <- data.frame(GID = phenoMada[, 1], Env = '', Response = phenoMada[, 3])
  CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)

  # Check predictive capacities of the model in only one testingSet
  pm_basic <- BME(Y = Y, Z1 = Z.G, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV$CrossValidation_list[[1]])
  expect_output(str(pm_basic), 'List of 6')
  expect_is(pm_basic, 'BMECV')
  expect_is(pm_basic$results, 'data.frame')
  expect_is(pm_basic$executionTime, 'numeric')
  expect_output(print(pm_basic), 'Fitted Bayesian Multi Environment model with:')
  # expect_silent(boxplot(pm_basic))

  # Check predictive capacities of the model with CrossValidation object
  pm <- BME(Y = Y, Z1 = Z.G, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV)
  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMECV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi Environment model with:')
  expect_identical(head(summary(pm), 4), head(summary(pm, 'extended'), 4)[,-7])
  expect_silent(boxplot(pm))
  expect_silent(boxplot(pm, select = 'MAAPE'))
  expect_silent(boxplot(pm, select = 'MAAPE', ordered = FALSE))

  if (Sys.info()[['sysname']] != 'Darwin') {
    # Check the work in parallel
    pm_parallel <- BME(Y = Y, Z1 = Z.G, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV, parallelCores = 2, progressBar = FALSE)

    expect_output(str(pm_parallel), 'List of 6')
    expect_is(pm_parallel, 'BMECV')
    expect_is(pm_parallel$results, 'data.frame')
    expect_is(pm_parallel$executionTime, 'numeric')
    expect_output(print(pm_parallel), 'Fitted Bayesian Multi Environment model with:')
    expect_silent(boxplot(pm_parallel))

    expect_error(expect_equivalent(pm, pm_parallel))
  }
})

context('BMTME function')

test_that('BMTME function with Iranian data with NA values', {
  data("WheatIranianToy")
  removePos <- sample.int(50, 15)
  phenoIranianToy <- (phenoIranianToy[order(phenoIranianToy$GID),])
  phenoIranianToy$DTH[removePos] <- NA

  LG <- cholesky(genoIranianToy)
  ZG <- model.matrix(~0 + as.factor(phenoIranianToy$GID))
  Z.G <- ZG %*% LG
  Z.E <- model.matrix(~0 + as.factor(phenoIranianToy$Env))
  ZEG <- model.matrix(~0 + as.factor(phenoIranianToy$GID):as.factor(phenoIranianToy$Env))
  G2 <- kronecker(diag(length(unique(phenoIranianToy$Env))), data.matrix(genoIranianToy))
  LG2 <- cholesky(G2)
  Z.EG <- ZEG %*% LG2
  Y <- as.matrix(phenoIranianToy[, -c(1, 2)])

  #Check fitting
  fm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 100, burnIn = 50, thin = 2, bs = 50)
  expect_is(fm, 'BMTME')
  expect_output(str(fm), 'List of 19')
  expect_is(fm$Y, 'matrix')
  expect_is(fm$yHat, 'matrix')
  expect_equal(dim(fm$Y), dim(fm$yHat))
  expect_gt(fm$NAvalues, 0)
  expect_output(print(fm), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
  expect_silent(plot(fm, trait = 'DTH'))

  # With more traits removed
  phenoIranianToy[removePos, 3:4] <- NA
  Y <- as.matrix(phenoIranianToy[, -c(1, 2)])

  #Check fitting
  fm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 100, burnIn = 50, thin = 2, bs = 50)
  expect_is(fm, 'BMTME')
  expect_output(str(fm), 'List of 19')
  expect_is(fm$Y, 'matrix')
  expect_is(fm$yHat, 'matrix')
  expect_equal(dim(fm$Y), dim(fm$yHat))
  expect_gt(fm$NAvalues, 0)
  expect_output(print(fm), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
  expect_silent(plot(fm, trait = 'DTH'))

})

test_that('BMTME function with Iranian data', {
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

  #Check fitting
  fm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 100, burnIn = 50, thin = 2, bs = 50)
  expect_is(fm, 'BMTME')
  expect_output(str(fm), 'List of 19')
  expect_is(fm$Y, 'matrix')
  expect_is(fm$yHat, 'matrix')
  expect_equal(dim(fm$Y), dim(fm$yHat))
  expect_equal(fm$NAvalues, 0)
  expect_output(print(fm), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
  expect_silent(plot(fm, trait = 'DTH'))
  expect_is(residuals(fm), 'matrix')
  expect_false(any(is.nan(residuals(fm))))

  # Check predictive capacities of the model
  pheno <- data.frame(GID = phenoIranianToy[, 1], Env = phenoIranianToy[, 2], Response = phenoIranianToy[, 3])

  CrossV <- CV.RandomPart(pheno, NPartitions = 4, PTesting = 0.2, set_seed = 123)
  pm_basic <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV$CrossValidation_list$partition1)

  expect_output(str(pm_basic), 'List of 5')
  expect_is(pm_basic, 'BMTMECV')
  expect_is(pm_basic$results, 'data.frame')
  expect_is(pm_basic$executionTime, 'numeric')
  expect_output(print(pm_basic), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
  # expect_silent(boxplot(pm))

  # Check predictive capacities of the model
  pm <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 10, burnIn = 5, thin = 2, bs = 50, testingSet = CrossV)

  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMTMECV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
  expect_identical(head(summary(pm)), head(summary(pm, 'extended'), 4)[,-7])
  expect_silent(boxplot(pm))
  expect_silent(boxplot(pm, select = 'MAAPE'))

  if (Sys.info()[['sysname']] != 'Darwin') {
    # Check the work in parallel
    pm_parallel <- BMTME(Y = Y, X = Z.E, Z1 = Z.G, Z2 = Z.EG, nIter = 10,
                         burnIn = 5, thin = 2, bs = 50, testingSet = CrossV,
                         parallelCores = 2, progressBar = FALSE)

    expect_output(str(pm_parallel), 'List of 6')
    expect_is(pm_parallel, 'BMTMECV')
    expect_is(pm_parallel$results, 'data.frame')
    expect_is(pm_parallel$executionTime, 'numeric')
    expect_output(print(pm_parallel), 'Fitted Bayesian Multi-Trait Multi-Environment Model with:')
    expect_silent(boxplot(pm_parallel))

    expect_error(expect_equivalent(pm, pm_parallel))
  }
})

context('BMORS function')
test_that('BMORS function with Wheat data with NA values', {
  data("WheatToy")
  phenoWheatToy <- phenoWheatToy[order(phenoWheatToy$Env, phenoWheatToy$Gid),]
  phenoWheatToy[sample.int(50, 15), 3] <- NA
  phenoWheatToy[sample.int(50, 15), 4] <- NA

  LG <- cholesky(genoWheatToy)
  ZG <- model.matrix(~0 + as.factor(phenoWheatToy$Gid))
  Z.G <- ZG %*% LG
  ETA <- list(Gen = list(X = Z.G, model = 'BRR'))

  Y_Error <- phenoWheatToy[, c(3,4)]
  expect_error(BMORS(Y_Error, ETA = ETA, nIter = 10, burnIn = 5, thin = 2,
                     digits = 4))


  Y <- as.matrix(phenoWheatToy[, c(3,4)])
  # Check predictive capacities of the model
  pm <- BMORS(Y, ETA = ETA, nIter = 10, burnIn = 5, thin = 2, digits = 4)
  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMORS')
  expect_is(pm$results, 'data.frame')
  expect_true(all(is.na(pm$results$Observed)))
  expect_gt(pm$NAvalues, 0)
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Output Regression Stacking model with:')

  pm <- BMORS(Y, ETA = ETA, nIter = 10, burnIn = 5, thin = 2, progressBar = FALSE,
              predictor_Sec_complete =  TRUE, digits = 4)
  expect_output(str(pm), 'List of 6')
  expect_is(pm, 'BMORS')
  expect_is(pm$results, 'data.frame')
  expect_true(all(is.na(pm$results$Observed)))
  expect_gt(pm$NAvalues, 0)
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Output Regression Stacking model with:')
})

test_that('BMORS function with Wheat full data', {
  data("WheatToy")
  phenoWheatToy <- phenoWheatToy[order(phenoWheatToy$Env, phenoWheatToy$Gid),]
  LG <- cholesky(genoWheatToy)
  ZG <- model.matrix(~0 + as.factor(phenoWheatToy$Gid))
  Z.G <- ZG %*% LG
  ETA <- list(Gen = list(X = Z.G, model = 'BRR'))

  pheno <- phenoWheatToy[, c(1:3)] #Use only the first trait to do a cv
  colnames(pheno) <- c('Line', 'Env', 'Response')
  CrossValidation <- CV.RandomPart(pheno, NPartitions = 10, PTesting = 0.2, set_seed = 123)
  Y <- as.matrix(phenoWheatToy[, c(3,4)])
  # Check predictive capacities of the model
  pm <- BMORS(Y, ETA = ETA, nIter = 10, burnIn = 5, thin = 2,
              testingSet = CrossValidation,  digits = 4)

  expect_output(str(pm), 'List of 5')
  expect_is(pm, 'BMORSCV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Output Regression Stacking model with:')
  expect_identical(head(summary(pm), 4), head(summary(pm, 'extended'), 4)[,-7])
  expect_silent(boxplot(pm))
  expect_silent(boxplot(pm, select = 'MAAPE'))
  expect_silent(plot(pm))
  expect_silent(plot(pm, 'MAAPE'))

  if (Sys.info()[['sysname']] != 'Darwin') {
    # Check the work in parallel
    pm_parallel <- BMORS(Y, ETA = ETA, nIter = 10, burnIn = 5, thin = 2, progressBar = FALSE,
                         testingSet = CrossValidation,  digits = 4, parallelCores = 2)

    expect_output(str(pm_parallel), 'List of 5')
    expect_is(pm_parallel, 'BMORSCV')
    expect_is(pm_parallel$results, 'data.frame')
    expect_is(pm_parallel$executionTime, 'numeric')
    expect_output(print(pm_parallel), 'Fitted Bayesian Multi-Output Regression Stacking model with:')
    expect_silent(boxplot(pm_parallel))

    expect_error(expect_equivalent(pm, pm_parallel))
  }
})

context('BMORS_Env function')
test_that('BMORS_Env function with Maize data', {
  data('MaizeToy')
  phenoMaizeToy <- phenoMaizeToy[order(phenoMaizeToy$Env, phenoMaizeToy$Line),]
  LG <- cholesky(genoMaizeToy)
  ZG <- model.matrix(~0 + as.factor(phenoMaizeToy$Line))
  Z.G <- ZG %*% LG
  ETA <- list(Gen = list(X = Z.G, model = 'BRR'))

  dataset <- phenoMaizeToy[, 2:5] #Must Include in the first column the environments

  # Check predictive capacities of the model
  pm <- BMORS_Env(dataset, testingEnv = 'EBU', ETA = ETA, covModel = 'BRR', nIter = 10,
                  burnIn = 4, thin = 2, progressBar = FALSE, digits = 3)

  expect_output(str(pm), 'List of 5')
  expect_is(pm, 'BMORSENV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Output Regression Stacking model for n environments with:')
  expect_silent(barplot(pm))

  pm <- BMORS_Env(dataset, 'EBU', ETA, 'BRR', nIter = 10,
                  burnIn = 4, thin = 2, digits = 3, predictor_Sec_complete = TRUE)

  expect_output(str(pm), 'List of 5')
  expect_is(pm, 'BMORSENV')
  expect_is(pm$results, 'data.frame')
  expect_is(pm$executionTime, 'numeric')
  expect_output(print(pm), 'Fitted Bayesian Multi-Output Regression Stacking model for n environments with:')
  expect_silent(barplot(pm))
})
