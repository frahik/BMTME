#' @title summary.BMTMECV
#'
#' @description Produces a summary of the results of the fitted model adding
#' the predictive capabilities of the model, as well as the MAAPE error rate
#' and the respective standard errors.
#'
#' @param object \code{BMTMECV object} an BMTMECV object for which a summary is desired.
#' @param information The type of summary to obtain from the model (compact, extended, complete), by default is compact.
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if n ungroup
#' @export
summary.BMTMECV <- function(object, information = 'compact', digits = 4, ...) {
  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed - Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = TRUE)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = TRUE),
              SE_Pearson = sd(Pearson, na.rm = TRUE)/sqrt(n()), Pearson = mean(Pearson, na.rm = TRUE))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(finalSum, presum)
                }
  )
  return(out)
}

#' @title summary.BMECV
#'
#' @description Produces a summary of the results of the fitted model adding
#' the predictive capabilities of the model, as well as the MAAPE error rate
#' and the respective standard errors.
#'
#' @param object \code{BMECV object} an BMECV object for which a summary is desired.
#' @param information The type of summary to obtain from the model (compact, extended, complete), by default is compact.
#'
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if n ungroup
#'
#' @export
summary.BMECV <- function(object, information = 'compact', digits = 4, ...) {
  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed - Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = TRUE)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = TRUE),
              SE_Pearson = sd(Pearson, na.rm = TRUE)/sqrt(n()), Pearson = mean(Pearson, na.rm = TRUE))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  # presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(finalSum, presum)
                }
  )
  return(out)
}

#' @title summary.BMORSCV
#'
#' @description Produces a summary of the results of the fitted model adding
#' the predictive capabilities of the model, as well as the MAAPE error rate
#' and the respective standard errors.
#'
#' @param object \code{BMORSCV object} an BMORSCV object for which a summary is desired.
#' @param information The type of summary to obtain from the model (compact, extended, complete), by default is compact.
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor sd
#' @importFrom dplyr summarise group_by select '%>%' mutate_if n ungroup
#'
#' @export
summary.BMORSCV <- function(object, information = 'compact', digits = 4, ...){
  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed - Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Partition, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MAAPE = sd(MAAPE, na.rm = TRUE)/sqrt(n()), MAAPE = mean(MAAPE, na.rm = TRUE),
              SE_Pearson = sd(Pearson, na.rm = TRUE)/sqrt(n()), Pearson = mean(Pearson, na.rm = TRUE))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MAAPE, SE_MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
                compact = finalSum,
                complete = presum,
                extended = {
                  finalSum$Partition <- 'All'
                  # presum$Partition <- as.character(presum$Partition)
                  presum$SE_Pearson <- NA
                  presum$SE_MAAPE <- NA
                  rbind(finalSum, presum)
                }
  )
  return(out)
}


#' @title summary.BMORSENV
#'
#' @description Produces a summary of the results of the fitted model adding
#' the predictive capabilities of the model, as well as the MAAPE error rate
#' and the respective standard errors.
#'
#' @param object \code{BMORSENV object} an BMORSENV object for which a summary is desired.
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if ungroup
#'
#' @export
summary.BMORSENV <- function(object, digits = 4, ...){
  object$results %>%
    group_by(Environment, Trait) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MAAPE = mean(atan(abs(Observed - Predicted)/abs(Observed)))) %>%
    select(Environment, Trait, Pearson, MAAPE) %>%
    ungroup() %>%
    mutate_if(is.numeric, list(~round(., digits))) %>%
    as.data.frame() -> out

  return(out)
}

#' Print BMTME information object
#'
#' @param x an BMTME object used to print.
#' @param ...  Further arguments passed to or from other methods.
#'
#' @export
#'
print.BMTME <- function(x, ...){
  cat('Fitted Bayesian Multi-Trait Multi-Environment Model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n\n',
      '\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BME information object
#'
#' @param x an BME object used to print.
#' @param ...  Further arguments passed to or from other methods.
#' @export
#'
print.BME <- function(x, ...){
  cat('Multi-Environment Model Fitted with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n\n',
      '\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BMTMECV information object
#'
#' @param x an BMTMECV object used to print.
#' @param ...  Further arguments passed to or from other methods.
#' @importFrom utils head
#' @export
#'
print.BMTMECV <- function(x, ...){
  cat('Fitted Bayesian Multi-Trait Multi-Environment Model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Predictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BMECV information object
#'
#' @param x an BMECV object used to print.
#' @param ...  Further arguments passed to or from other methods.
#'
#' @importFrom utils head
#' @export
#'
print.BMECV <- function(x, ...){
  cat('Fitted Bayesian Multi Environment model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Predictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BMORS information object
#'
#' @param x an BMORS object used to print.
#' @param ...  Further arguments passed to or from other methods.
#' @importFrom utils head
#' @export
#'
print.BMORS <- function(x, ...){
  cat('Fitted Bayesian Multi-Output Regression Stacking model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      '\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BMORSCV information object
#'
#' @param x an BMORSCV object used to print.
#' @param ...  Further arguments passed to or from other methods.
#' @importFrom utils head
#' @export
#'
print.BMORSCV <- function(x, ...){
  cat('Fitted Bayesian Multi-Output Regression Stacking model with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Predictive capacity of the model: \n')

  print.data.frame(summary(x, 'compact', digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more detailed information.')
  invisible(x)
}

#' Print BMORSENV information object
#'
#' @param x an BMORSENV object used to print.
#' @param ...  Further arguments passed to or from other methods.
#' @return test
#' @importFrom utils head
#' @export
#'
print.BMORSENV <- function(x, ...){
  cat('Fitted Bayesian Multi-Output Regression Stacking model for n environments with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'Runtime: ', x$executionTime ,' seconds \n\n',
      'Predictive capacity of the model: \n')

  print.data.frame(summary(x, digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more detailed information.')
  invisible(x)
}


#' @title residuals.BME
#'
#' @description extracts model residuals from BME objects returned by modeling function BME.
#'
#' @param object an \code{BME object} for which the extraction of model residuals is meaningful.
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
residuals.BME <- function(object, digits = 4, ...) {
  return(round(object$Y - object$yHat, digits))
}

#' @title residuals.BMTME
#'
#' @description  extracts model residuals from BMTME objects returned by modeling function BMTME.
#'
#' @param object an \code{BMTME object} for which the extraction of model residuals is meaningful.
#' @param digits number of digits.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
residuals.BMTME <- function(object, digits = 4, ...) {
  return(round(object$Y - object$yHat, digits))
}

#' @title plot.BME
#'
#' @description Simple scatter plot comparing the observed values against the predicted values.
#'
#' @param x an \code{BME object} for which the plot of model is meaningful.
#' @param trait \code{string} Name of the trait to plot.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BME <- function(x, trait = '', ...){
  response <- x$Y[,trait]
  predictions <- x$yHat[,trait]
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = paste("BME fitted model in the trait", trait), xlim = limits, ylim = limits, xlab = 'Observed values', ylab = 'Predicted values', ...)
  abline(a = 0, b = 1, lty = 3)
}

#' @title plot.BMTME
#'
#' Simple scatter plot comparing the observed values against the predicted values.
#'
#' @param x an \code{BMTME object} for which the plot of model is meaningful.
#' @param trait \code{string} Name of the trait to plot.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BMTME <- function(x, trait = '', ...){
  response <- x$Y[,trait]
  predictions <- x$yHat[,trait]
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = paste("BMTME fitted model in the trait", trait), xlim = limits,
       ylim = limits, xlab = 'Observed values', ylab = 'Predicted values', ...)
  abline(a = 0, b = 1, lty = 3)
}

#' @title Plot BMORSCV
#' Simple scatter plot comparing the observed values against the predicted values.
#'
#' @param x an \code{BMORS object} for which the plot of model is meaningful.
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the BMORSCV Object, else ('MAAPE'), plot the MAAPE of the BMORSCV Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics arrows axis plot
#' @export
plot.BMORSCV <- function(x, select = 'Pearson', ...){
  results <- summary(x)
  results <- results[order(results[, select]),]

  if (select == "Pearson") {
    results$SE <- 1.96 * results$SE_Pearson
    ylab <- "Pearson's Correlation"
  } else if (select == "MAAPE") {

    results$SE <- 1.96 * results$SE_MAAPE
    ylab <- select
  }
  x.labels <- paste0(results$Trait, '_', results$Env)
  plot.x <- seq_len(length(x.labels))
  plot(plot.x, results[, select], ylim = range(c(results[, select] - results$SE, results[, select] + results$SE)),
       type = 'p', ylab = ylab, xlab = '', xaxt = "n", ...)
  axis(1, at = plot.x, labels = x.labels, las = 2)
  arrows(plot.x, results[, select] - results$SE, plot.x, results[, select] + results$SE, code = 3, length = 0.02, angle = 90)
}


#' @title barplot BMORSENV graph
#'
#' @description Creates a bar plot with vertical bars, showing the predictive capability of the model or the error rate.
#'
#' @param height an \code{BMORSENV object} for which the plot of model is meaningful.
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the BMORSENV Object, else ('MAAPE'), plot the MAAPE of the BMORSENV Object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics barplot
#' @export
barplot.BMORSENV <- function(height, select = 'Pearson', ...){
  results <- summary(height)
  results <- results[order(results[, select]),]
  results$TxE <- paste(results$Trait, results$Environment, sep = '_')
  ylab <- ifelse(select == 'Pearson', "Pearson's Correlation", 'MAAPE')
  barplot(results[, select], ylab = ylab, names.arg = results$TxE)
}


#' @title boxplot.BMECV
#'
#' @description Produce box-and-whisker plot(s) of the given BMECV object.
#'
#' @param x an \code{BMECV object} for which the plot of model is meaningful.
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the BMECV Object, else ('MAAPE'), plot the MAAPE of the BMECV Object.
#' @param ordered \code{logic} The graph should be sorted by the median? by default is \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMECV <- function(x, select = 'Pearson', ordered = TRUE, ...){
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
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = TRUE))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = TRUE))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  } else if (length(unique(results$Trait)) > 1)  {
    if (ordered && select != 'MAAPE') {
      results$Trait  <- with(results, reorder(Trait, Pearson, median, na.rm = TRUE))
    } else if (ordered && select == 'MAAPE') {
      results$Trait  <- with(results, reorder(Trait, MAAPE, median, na.rm = TRUE))
    }
    boxplot(plot.y ~ results$Trait, col = "grey", xlab = 'Traits', ylab = ylab, ...)
  }
  else {
    boxplot(plot.y, col = "grey", xlab = 'Environment', ylab = ylab, ...)
  }
}

#' @title boxplot.BMTMECV
#'
#' @description Produce box-and-whisker plot(s) of the given BMTMECV object.
#'
#' @param x an \code{BMTMECV object} for which the plot of model is meaningful.
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the BMTMECV Object, else ('MAAPE'), plot the MAAPE of the BMTMECV Object.
#' @param ordered \code{logic} The graph should be sorted by the median? by default is \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMTMECV <- function(x, select = 'Pearson', ordered = TRUE, ...){
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
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = TRUE))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = TRUE))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  }else{
    boxplot(plot.y, col = "grey", xlab = 'Environment', ylab = ylab, ...)
  }
}

#' @title boxplot.BMORSCV
#'
#' @description Produce box-and-whisker plot(s) of the given BMORSCV object.
#'
#' @param x an \code{BMORSCV object} for which the plot of model is meaningful.
#' @param select \code{character} By default ('Pearson'), plot the Pearson Correlations of the BMORSCV Object, else ('MAAPE'), plot the MAAPE of the BMORSCV Object.
#' @param ordered \code{logic} The graph should be sorted by the median? by default is \code{TRUE}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics boxplot
#' @export
boxplot.BMORSCV <- function(x, select = 'Pearson', ordered = TRUE, ...){
  results <- summary(x, 'complete')

  switch(select,
          Pearson = {
            plot.y <- results$Pearson
            ylab <- "Pearson's Correlation"
          }, MAAPE = {
            plot.y <- results$MAAPE
            ylab <- "MAAPE"
          },
          stop('Error in select parameter.', call. = )
  )

  if (length(unique(results$Env)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered && select != 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = TRUE))
    } else if (ordered && select == 'MAAPE') {
      results$TxE  <- with(results, reorder(TxE , MAAPE, median, na.rm = TRUE))
    }

    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  }else{
    boxplot(plot.y, col = "grey", ylab = ylab, ...)
  }
}


getProgressBar <- function(title = '', total = 1) {
  return(progress::progress_bar$new(format = title, total = total, clear = FALSE, show_after = 0))
}
