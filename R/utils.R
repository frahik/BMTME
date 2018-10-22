#' @title Summary.BMTMECV
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMTMECV object} Objeto BMTMECV, resultado de ejecutar BMTME()
#' @param information compact, extended, complete
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs n
#'
#' @export
summary.BMTMECV <- function(object, information = 'compact', digits = 4, ...) {
  if (!inherits(object, "BMTMECV")) stop("This function only works for objects of class 'BMTMECV'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = T)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = T),
              SE_Pearson = sd(Pearson, na.rm = T)/sqrt(n()), Pearson = mean(Pearson, na.rm = T))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(presum, finalSum)
                }
  )
  return(out)
}

#' @title Summary.BMECV
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMECV object} Objeto BMECV, resultado de ejecutar BME()
#' @param information compact, extended, complete
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs n
#'
#' @export
summary.BMECV <- function(object, information = 'compact', digits = 4, ...) {
  if (!inherits(object, "BMECV")) stop("This function only works for objects of class 'BMECV'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = T)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = T),
              SE_Pearson = sd(Pearson, na.rm = T)/sqrt(n()), Pearson = mean(Pearson, na.rm = T))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(presum, finalSum)
                }
  )
  return(out)
}

#' @title Summary.BMORSCV
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMORSCV object} Objeto BMORSCV, resultado de ejecutar MTME()
#' @param information compact, extended, complete
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs n
#'
#' @export
summary.BMORSCV <- function(object, information = 'compact', digits = 4, ...){
  if (!inherits(object, "BMORSCV")) stop("This function only works for objects of class 'BMORSCV'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = T)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = T),
              SE_Pearson = sd(Pearson, na.rm = T)/sqrt(n()), Pearson = mean(Pearson, na.rm = T))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(presum, finalSum)
                }
  )
  return(out)
}


#' @title Summary.BMORSENV
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMORSENV object} Objeto BMORSENV, resultado de ejecutar MTME()
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs
#'
#' @export
summary.BMORSENV <- function(object, digits = 4, ...){
  if (!inherits(object, "BMORSENV")) stop("This function only works for objects of class 'BMORSENV'")

  object$results %>%
    group_by(Environment, Trait) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed-Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Pearson, MAAPE) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> out

  return(out)
}

#' Print BMTME information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test of package
#' @export
#'
print.BMTME <- function(x, ...){
  cat('Fitted Bayesian Multi-Trait Multi-Environment Model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n\n',
      'Predicted Values: \n')

  print.default(format(x$yHat, digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' Print BME information object
#'
#' @param x object a
#' @param ... more objects
#'
#' @return test of a package
#' @export
#'
print.BME <- function(x, ...){
  cat('Multi-Environment Model Fitted with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n\n',
      'Predicted Values: \n')

  print.default(format(x$yHat, digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' Print BMTMECV information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test
#' @importFrom utils head
#' @export
#'
print.BMTMECV <- function(x, ...){
  cat('Fitted Bayesian Multi-Trait Multi-Environment Model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$results$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' Print BMECV information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test
#' @importFrom utils head
#' @export
#'
print.BMECV <- function(x, ...){
  cat('Fitted Bayesian Multi Environment model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$results$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}


#' Print BMORSCV information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test
#' @importFrom utils head
#' @export
#'
print.BMORSCV <- function(x, ...){
  cat('Fitted Bayesian Multi Trait Multi Environment Regressor Stacking model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$results$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' Print BMORSENV information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test
#' @importFrom utils head
#' @export
#'
print.BMORSENV <- function(x, ...){
  cat('Fitted Bayesian Multi Trait Multi Environment Regressor Stacking model for n environments with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Some predicted values: \n')

  print.default(format(head(x$results$Predicted, 20), digits = 3), print.gap = 2L, quote = FALSE)

  cat('\nPredictive capacity of the model: \n')

  print.data.frame(summary(x, digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}


#' @title residuals.BME
#'
#' @description Solo es una prueba
#'
#' @param object \code{BME object} Objeto BME, resultado de ejecutar BME
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
residuals.BME <- function(object, digits = 4, ...) {
  if (!inherits(object, "BME")) stop("This function only works for objects of class 'BME'")
	return(round(object$Y - object$yHat, digits))
}

#' @title residuals.BMTME
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMTME object} Objeto BMTME, resultado de ejecutar BMTME
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
residuals.BMTME <- function(object, digits = 4, ...) {
  if (!inherits(object, "BMTME")) stop("This function only works for objects of class 'BMTME'")
	return(round(object$Y - object$yHat, digits))
}

#' @title plot.BME
#'
#' @description Solo es una prueba
#'
#' @param x \code{BME object}.
#' @param trait \code{string} name of the trait to plot.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BME <- function(x, trait = '', ...){
  ### Check that object is compatible
  if (!inherits(x, "BME")) stop("This function only works for objects of class 'BME'")
  response <- x$Y[,trait]
  predictions <- x$yHat[,trait]
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = paste("BME fitted model in the trait", trait), xlim = limits, ylim = limits, xlab = 'Observed values', ylab = 'Predicted values', ...);
  abline(a = 0, b = 1, lty = 3)
}

#' @title plot.BMTME
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMTME object}.
#' @param trait \code{string} name of the trait to plot.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BMTME <- function(x, trait = '', ...){
  ### Check that object is compatible
  if (!inherits(x, "BMTME")) stop("This function only works for objects of class 'BMTME'")
  response <- x$Y[,trait]
  predictions <- x$yHat[,trait]
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = paste("BMTME fitted model in the trait", trait), xlim = limits,
       ylim = limits, xlab = 'Observed values', ylab = 'Predicted values', ...)
  abline(a = 0, b = 1, lty = 3)
}

#' @title Plot BMORSCV graph
#'
#' @description Plot from BMORSCV object
#'
#' @param x \code{BMORSCV object} BMORSCV object, result of use the MTME() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the MTME Object, else ('MAAPE'), plot the MAAPE of the BMORSCV Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics arrows axis plot
#' @export
plot.BMORSCV <- function(x, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(x, "BMORSCV")) stop("This function only works for objects of class 'BMORSCV'", call. = FALSE)

  results <- summary(x)
  results <- results[order(results[, select]),]

  if (select == "Pearson") {
    results$SE <- 1.96 * results$SE_Pearson
    ylab <- "Pearson's Correlation"
  } else if (select == "MAAPE") {

    results$SE <- 1.96 * results$SE_MAAPE[which(results$Fold == 'Average_all')]
    ylab <- select
  }
  x.labels <- paste0(results$Trait, '_', results$Env)
  plot.x <- 1:length(x.labels)
  plot(plot.x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = ylab, xlab = '', xaxt = "n", ...)
  axis(1, at = plot.x, labels = x.labels, las = 2)
  arrows(plot.x, results[, select] - results$SE, plot.x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title barplot BMORSENV graph
#'
#' @description Plot from BMORSENV object
#'
#' @param height \code{BMORSENV object} BMORSENV object, result of use the MTME() function
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the MTME Object, else ('MAAPE'), plot the MAAPE of the BMORSENV Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics barplot
#' @export
barplot.BMORSENV <- function(height, select = 'Pearson', ...){
  ### Check that object is compatible
  if (!inherits(height, "BMORSENV")) stop("This function only works for objects of class 'BMORSENV'", call. = FALSE)

  results <- summary(height)
  results <- results[order(results[, select]),]
  results$TxE <- paste(results$Trait, results$Environment, sep = '_')
  barplot(results[, select], xlab = "Trait x Environment", names.arg = results$TxE)
}


#' @title boxplot.BMECV
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMECV object} Objeto BMECV, resultado de ejecutar BME()
#' @param select \code{string} Pearson or MAAPE
#' @param ordered \code{logic} TRUE or FALSE
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMECV <- function(x, select = 'Pearson', ordered = TRUE, ...){
  ### Check that object is compatible
  if (!inherits(x, "BMECV")) stop("This function only works for objects of class 'BMECV'")

  results <- summary(x, 'complete')

  if (select == "Pearson") {
    plot.y <- results$Pearson
    ylab <- "Pearson's Correlation"
  } else if (select == "MAAPE") {
    plot.y <- results$MAAPE
    ylab <- "MAAPE"
  }

  if (length(unique(results$Env)) > 1 && length(unique(results$Trait)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered && select != 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = T))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = T))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  } else if (length(unique(results$Trait)) > 1)  {
    if (ordered && select != 'MAAPE') {
      results$Trait  <- with(results, reorder(Trait, Pearson, median, na.rm = T))
    } else if (ordered && select == 'MAAPE') {
      results$Trait  <- with(results, reorder(Trait, MAAPE, median, na.rm = T))
    }
    boxplot(plot.y ~ results$Trait, col = "grey", xlab = 'Traits', ylab = ylab, ...)
  }
  else {
    boxplot(plot.y, col = "grey", xlab = 'Environment', ylab = ylab, ...)
  }
}

#' @title boxplot.BMTMECV
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMTMECV object} Objeto BMTMECV, resultado de ejecutar BMTME()
#' @param select \code{string} Pearson or MAAPE
#' @param ordered \code{logic} TRUE or FALSE
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMTMECV <- function(x, select = 'Pearson', ordered = TRUE, ...){
  ### Check that object is compatible
  if (!inherits(x, "BMTMECV")) stop("This function only works for objects of class 'BMTMECV'")

  results <- summary(x, 'complete')

  if (select == "Pearson") {
    plot.y <- results$Pearson
    ylab <- "Pearson's Correlation"
  } else if (select == "MAAPE") {
    plot.y <- results$MAAPE
    ylab <- "MAAPE"
  }

  if (length(unique(results$Env)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered && select != 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = T))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = T))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  }else{
    boxplot(plot.y, col = "grey", xlab = 'Environment', ylab = ylab, ...)
  }
}

#' @title boxplot.BMORSCV
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMORSCV object} Objeto BMORSCV, resultado de ejecutar MTME()
#' @param select \code{string} Pearson or MAAPE
#' @param ordered \code{logic} TRUE or FALSE
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMORSCV <- function(x, select = 'Pearson', ordered = TRUE, ...){
  ### Check that object is compatible
  if (!inherits(x, "BMORSCV")) stop("This function only works for objects of class 'BMORSCV'", call. = FALSE)

  results <- summary(x, 'complete')

  switch(select,
          Pearson = {
            plot.y <- results$Pearson
            ylab <- "Pearson's Correlation"
          }, MAAPE = {
            plot.y <- results$MAAPE
            ylab <- "MAAPE"
          }, CC = {
            plot.y <- results$CC
            ylab <- "Classification correct average"
          },
          stop('Error in select parameter.', call. = )
  )

  if (length(unique(results$Env)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered && select != 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = T))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = T))
    }

    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  }else{
    boxplot(plot.y, col = "grey", ylab = ylab, ...)
  }
}

