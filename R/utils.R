#' @title Summary.BMTMECV
#'
#' @description Solo es una prueba
#'
#' @param object \code{BMTMECV object} Objeto BMTMECV, resultado de ejecutar BMTME()
#' @param ... Further arguments passed to or from other methods.
#' @param information compact, extended, complete
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs
#'
#' @export
summary.BMTMECV <- function(object, information = 'compact', digits = 4, ...){
  if (!inherits(object, "BMTMECV")) Stop("This function only works for objects of class 'BMTMECV'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MSEP = mean((Predicted - Observed)^2, na.rm = T)) %>%
    select(Environment, Trait, Partition, Pearson, MSEP) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MSEP = sd(MSEP, na.rm = T)/sqrt(n()), MSEP = mean(MSEP, na.rm = T),
              SE_Pearson = sd(Pearson, na.rm = T)/sqrt(n()), Pearson = mean(Pearson, na.rm = T))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MSEP, SE_MSEP) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
    compact = finalSum,
    complete = presum,
    extended = {
      finalSum$Partition <- 'All'
      presum$Partition <- as.character(presum$Partition)
      presum$SE_Pearson <- NA
      presum$SE_MSEP <- NA
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
#' @param ... Further arguments passed to or from other methods.
#' @param information compact, extended, complete
#'
#' @importFrom stats cor
#' @importFrom dplyr summarise group_by select '%>%' mutate_if funs
#'
#' @export
summary.BMECV <- function(object, information = 'compact', digits = 4, ...){
  if (!inherits(object, "BMECV")) Stop("This function only works for objects of class 'BMECV'")

  object$results %>%
    group_by(Environment, Trait, Partition) %>%
    summarise(Pearson = cor(Predicted, Observed, use = 'pairwise.complete.obs'),
              MSEP = mean((Predicted - Observed)^2, na.rm = T)) %>%
    select(Environment, Trait, Partition, Pearson, MSEP) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> presum

  presum %>%  group_by(Environment, Trait) %>%
    summarise(SE_MSEP = sd(MSEP, na.rm = T)/sqrt(n()), MSEP = mean(MSEP, na.rm = T),
              SE_Pearson = sd(Pearson, na.rm = T)/sqrt(n()), Pearson = mean(Pearson, na.rm = T))  %>%
    select(Environment, Trait, Pearson, SE_Pearson, MSEP, SE_MSEP) %>%
    mutate_if(is.numeric, funs(round(., digits))) %>%
    as.data.frame() -> finalSum

  out <- switch(information,
    compact = finalSum,
    complete = presum,
    extended = {
      finalSum$Partition <- 'All'
      presum$Partition <- as.character(presum$Partition)
      presum$SE_Pearson <- NA
      presum$SE_MSEP <- NA
      rbind(presum, finalSum)
    }
  )
  return(out)
}


# summary.BMTME <- function(object,...){
#   if (!inherits(object, "BMTME")) Stop("This function only works for objects of class 'BMTME'")
# }


#' Print BMTME information object
#'
#' @param x object a
#' @param ...  more objects
#'
#' @return test
#' @export
#'
print.BMTME <- function(x, ...){
  cat('Fitted Multi-Trait Multi-Environment Model with: \n',
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
#' @return test
#' @export
#'
#' @examples
print.BME <- function(x, ...){
  cat('Multi-Environment Model Fitted with: \n',
      x$nIter, ' Iterations, burning the first ', x$burnIn, ' and thining every ', x$thin, '\n',
      'We found ', x$NAvalues, ' NA values \n\n',
      'Predicted Values: \n')

  print.default(format(x$yHat, digits = 3), print.gap = 2L, quote = FALSE)

  cat('\n Use str() function to found more datailed information.')
  invisible(x)
}

#' @title residuals.BME
#'
#' @description Solo es una prueba
#'
#' @param object \code{BME object} Objeto BME, resultado de ejecutar BME
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
#' @param x \code{BME object} Objeto BME, resultado de ejecutar BME
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BME <- function(x, ...){
  ### Check that object is compatible
  if (!inherits(x, "BME")) stop("This function only works for objects of class 'BME'")
  response <- c(x$Y)
  predictions <- c(x$yHat)
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = "BME fitted model", xlim = limits, ylim = limits, xlab = 'Response', ylab = 'Prediction', ...);
  abline(a = 0, b = 1, lty = 3)
}

#' @title plot.BMTME
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMTME object} Objeto BMTME, resultado de ejecutar BMTME
#' @param ... Further arguments passed to or from other methods.
#'
#' @importFrom graphics plot abline
#' @export
plot.BMTME <- function(x, ...){
  ### Check that object is compatible
  if (!inherits(x, "BMTME")) stop("This function only works for objects of class 'BMTME'")
  response <- c(x$Y)
  predictions <- c(x$yHat)
  limits <- range(c(response, predictions), na.rm = TRUE)
  plot(response, predictions, main = "BMTME fitted model", xlim = limits, ylim = limits, xlab = 'Response', ylab = 'Prediction', ...);
  abline(a = 0, b = 1, lty = 3)
}

#' @title boxplot.BMECV
#'
#' @description Solo es una prueba
#'
#' @param x \code{BMECV object} Objeto BMECV, resultado de ejecutar BME()
#' @param select \code{string} Pearson or MSEP
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
  } else if (select == "MSEP") {
    plot.y <- results$MSEP
    ylab <- "MSEP Average"
  }

  if (length(unique(results$Env)) > 1 && length(unique(results$Trait)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered) {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = T))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  } else if (length(unique(results$Trait)) > 1)  {
    if (ordered) {
      results$Trait  <- with(results, reorder(Trait , Pearson, median, na.rm = T))
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
#' @param select \code{string} Pearson or MSEP
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
  } else if (select == "MSEP") {
    plot.y <- results$MSEP
    ylab <- "MSEP Average"
  }

  if (length(unique(results$Env)) > 1) {
    results$TxE <- paste0(results$Trait, '_', results$Env)

    if (ordered) {
      results$TxE  <- with(results, reorder(TxE , Pearson, median, na.rm = T))
    }
    boxplot(plot.y ~ results$TxE, col = "grey", ylab = ylab, ...)
  }else{
    boxplot(plot.y, col = "grey", xlab = 'Environment', ylab = ylab, ...)
  }
}