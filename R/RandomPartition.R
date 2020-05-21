#' @title Cross-Validation with K Folds
#'
#' @description This method consists of randomly dividing the training data set and the test data set.
#'
#' @param DataSet (\code{data.frame}) The object need contain three columns in the Tidy data format:
#' \code{$Line} is the Line or genotype identifier, and the name of this column could change.
#' \code{$Env} is the name of the evaluated environment (s).
#' \code{$Response} Variable response obtained for the row corresponding to line and environment.
#' @param DataSetID (\code{string}) The ID of the lines.
#' @param K (\code{integer}) Number of groups to the cross-validation.
#' @param set_seed (\code{integer}) Seed number for reproducible research. Is \code{NULL} by default
#'
#' @importFrom stats na.omit
#' @return
#' Returns a nested list, with a positions to use as testing.
#'
#' @examples
#' \donttest{
#' data("WheatMadaToy")
#' phenoMada <- (phenoMada[order(phenoMada$GID),])
#' pheno <- data.frame(GID = phenoMada[, 1], Response = phenoMada[, 3])
#'
#' CV.KFold(pheno)
#' CV.KFold(pheno, set_seed = 123)
#' CV.KFold(pheno, DataSetID = 'GID', set_seed = 123)
#' CV.KFold(pheno, DataSetID = 'GID', K = 10, set_seed = 123)
#' }
#'
#' @export
CV.KFold <- function(DataSet, DataSetID = 'Line', K = 5, set_seed = NULL) {
  if (!is.null(set_seed)) {
    set.seed(set_seed)
  }

  if (is.null(DataSet$Env)) {
    DataSet$Env <- ''
  }

  if (is.null(DataSet$Trait)) {
    DataSet$Trait <- ''
  }

  if (length(unique(DataSet$Env)) == 1) {
    pm <- sample(dim(DataSet)[1])
    grs <- cut(seq(1, length(pm)), breaks = K, labels = FALSE)
    g_list <- vector('list', K)
    ng <- 0
    names(g_list) <- paste0('partition', 1:K)
    for (i in 1:K) {
      g_list[[paste0('partition', i)]] <- pm[grs == i]
      ng[i] <- length(g_list[[paste0('partition', i)]])
    }
    out <- list(CrossValidation_list = g_list,
                ng = ng, #Lenght in every partition
                Environments = as.character(DataSet$Env),
                Traits = as.character(DataSet$Trait)
    )
    class(out) <- 'CrossValidation'
    return(out)
  }

  UL <- unique(DataSet[, DataSetID])
  #Number of sites where each line appear
  # n_UL <- length(UL)
  # nSLA <- rep(NA, n_UL)

  nEAL <- table(DataSet[, DataSetID])#Number of Sites that appear  each line
  L_nE <- data.frame(Line = names(nEAL), nE = c(nEAL))

  #A list of Positions in data set dat_F that will conform the groups
  g_list <- vector('list', K)
  names(g_list) <- paste0('partition', 1:K)

  #Lines that will appear in all groups because
  # only appear in only one Site
  Pos1 <- which(L_nE$nE == 1)
  Pos_1_dat_F <- match(L_nE[Pos1, DataSetID], DataSet[, DataSetID])
  #dat_F[Pos_1_dat_F,]

  #Tama?o de cada partici?n sin considerar las lineas
  # que se incluir?n por defaul (las que aparecen en un solo ambiente)
  n <- dim(DataSet)[1]
  nR <- n - length(Pos1)
  ifelse(nR %% K == 0,
         ng <- rep(nR / K, K),
         ng <- rep(trunc(nR / K), K) + c(rep(1, nR - trunc(nR / K) * K), rep(0, K - (nR - trunc( nR / K ) * K))))
  #ng
  Pos_all <- 1:n
  #---------------------------------------------------------------
  #First group
  #---------------------------------------------------------------
  if (length(Pos1) == 0) {
    dat_F_k <- DataSet
  }
  else{
    dat_F_k <- DataSet[-Pos_1_dat_F,]
  }
  #Lineas ?nicas restantes
  UL_k <- unique(dat_F_k[, DataSetID])
  Pos_R_k <- rep(NA, length(UL_k))
  for (j in seq_len(length(UL_k))) {
    Pos_j_k <-  which(DataSet[, DataSetID] == UL_k[j])
    Pos_R_k[j] <- sample(Pos_j_k, 1)
  }
  Pos_R_k <- Pos_R_k

  Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_1_dat_F, Pos_R_k)], ng[1])
  g_list[[1]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)

  #---------------------------------------------------------------
  #Group 2,3, .., K
  #---------------------------------------------------------------
  for (k in 2:(K - 1))  {
    #Assigned positions
    Pos_k_a_R <- unique(unlist(g_list[1:(k - 1)]))
    dat_F_k <- DataSet[-Pos_k_a_R,]
    UL_k <- unique(dat_F_k[, DataSetID])
    #A las lineas que no aparecen en el grupo k-1, se remueve un
    # site donde aparencen para garantizar que ?stas aparezcan
    # en al menos un site
    UL_k <- UL_k[(UL_k %in% DataSet[Pos_k_a_R,DataSetID]) == FALSE]
    if (length(UL_k) > 0) {
      #Posiciones de lineas a mantener fuera del grupo k
      Pos_R_k <- rep(NA, length(UL_k))

      for (j in seq_len(length(UL_k))) {
        Pos_j_k <-  which((DataSet[, DataSetID] == UL_k[j]))
        if (length(Pos_j_k) > 1) {
          Pos_R_k[j] <- sample(Pos_j_k, 1)
        }
      }
      Pos_R_k <- na.omit(Pos_R_k)
      Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R, Pos_R_k)], ng[k])
      g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)
    }
    else{
      Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R)], ng[k])
      g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)
    }
  }

  k <- K
  Pos_k_a_R <- unique(unlist(g_list[1:(k - 1)]))
  Pos_k_2_dat_F <- sample(Pos_all[-c(Pos_k_a_R)], ng[k])
  g_list[[k]] <- c(Pos_1_dat_F, Pos_k_2_dat_F)

  n_CL <- length(Pos_1_dat_F)

  out <- list(CrossValidation_list = g_list,
              ng = ng + n_CL, #Lenght in every partition
              n_CL =  n_CL,   # Number of common lines
              Environments = as.character(DataSet$Env),
              Traits = as.character(DataSet$Trait)
  )
  class(out) <- 'CrossValidation'
  return(out)

}


#' @title Cross-Validation with Random Partitions
#'
#' @description This method consists of randomly dividing the training data set and the test data set.
#' For each division, the approximation function is adjusted from the training data and calculates the output values for the test data set.
#' The result corresponds to the arithmetic mean of the values obtained for the different divisions.
#'
#' @param DataSet \code{data.frame} The data set object is a data.frame object that contains 4 columns in the Tidy data format:
#' \code{$Line} is the Line or genotype identifier, and the name of this column could change.
#' \code{$Env} is the name of the evaluated environment (s).
#' \code{$Trait} is the name of the evaluated trait (s).
#' \code{$Response} Variable response obtained for the row corresponding to line, trait and environment.
#' @param NPartitions \code{integer} Number of Partitions for the Cross-Validation. Is 10 by default.
#' @param PTesting \code{Double} Percentage of Testing for the Cross-Validation. Is 0.35 by default.
#' @param Traits.testing \code{character} By default is null and use all the traits to fit the model, else only part of the traits specified be used to fit the model.
#' @param set_seed \code{integer} Seed number for reproducible research. Is \code{NULL} by default.
#'
#' @return \code{List} A list object with length of \code{NPartitions}, every index has a the positions to use like testing.
#'
#' @examples
#' \donttest{
#'   library(BMTME)
#'   data("WheatIranianToy")
#'   phenoIranianToy <- phenoIranianToy[order(phenoIranianToy$Env, phenoIranianToy$GID), ]
#'   pheno <- data.frame(GID = phenoIranianToy[, 1], Env = phenoIranianToy$Env,
#'                       Trait = rep(colnames(phenoIranianToy)[3:4], each = dim(phenoIranianToy)[1]),
#'                       Response = c(phenoIranianToy[, 3], phenoIranianToy[, 4]))
#'
#'   CV.RandomPart(pheno)
#'   CV.RandomPart(pheno, NPartitions = 10)
#'   CV.RandomPart(pheno, Traits.testing = 'DTM')
#'   CV.RandomPart(pheno, NPartitions = 10, PTesting = .35)
#'   CV.RandomPart(pheno, NPartitions = 10, Traits.testing = 'DTH')
#'   CV.RandomPart(pheno, NPartitions = 10, PTesting = .35, set_seed = 5)
#'   CV.RandomPart(pheno, NPartitions = 10, PTesting = .35, Traits.testing = 'DTH')
#'   CV.RandomPart(pheno, NPartitions = 10, PTesting = .35, Traits.testing = 'DTM', set_seed = 5 )
#' }
#' @export
CV.RandomPart <- function(DataSet, NPartitions = 10, PTesting = .35, Traits.testing = NULL, set_seed = NULL) {
  if (!is.null(set_seed)) {
    set.seed(set_seed)
  }

  if (length(unique(DataSet$Env)) == 0 ) {
    DataSet$Env <- ''
  }
  if (length(unique(DataSet$Trait)) == 0 ) {
    DataSet$Trait <- ''
  }
  if (length(unique(DataSet$Trait)) == 1) {
    Traits.testing <- NULL
  }

  new_Data <- tidyr::unite(DataSet, 'TraitxEnv', 'Trait', 'Env', sep = "_")
  new_Data <- tidyr::spread(new_Data, 'TraitxEnv', 'Response')

  NLine <- dim(new_Data)[1]

  if (length(unique(DataSet$Env)) == 1 ) {
    NEnv <- length(unique(DataSet$Trait))
    NTraits <- length(unique(DataSet$Env))
  } else {
    NEnv <- length(unique(DataSet$Env))
    NTraits <- length(unique(DataSet$Trait))
  }

  p_list <- vector('list', NPartitions)
  names(p_list) <- paste0('partition', seq_len(NPartitions))
  resp <- rep(1, NLine * NEnv)
  Y <- matrix(resp, ncol = NEnv, byrow = FALSE)

  for (i in seq_len(NPartitions)) {
    y <- Y[, seq_len(NEnv)]
    n <- nrow(Y)
    percTST <- PTesting
    nTST <- round(percTST*n)
    nNA <- NEnv*nTST
    if (nNA < n) {
      indexNA <- sample(1:n,nNA,replace = FALSE)
    }
    if (nNA >= n) {
      nRep <- floor(nNA/n)
      remain <- sample(1:n, nNA %% n, replace = FALSE)
      a0 <- sample(1:n,n,replace = FALSE)
      indexNA <- rep(a0,nRep)

      if (length(remain) > 0) {
        a1 <- floor(length(indexNA)/nTST)*nTST
        a2 <- nNA - a1 - length(remain)
        bb <- sample(a0[!a0 %in% remain], a2, replace = FALSE)
        noInIndexNA <- c(rep(a0, nRep - 1), a0[!a0 %in% bb])
        indexNA <- c(noInIndexNA,bb,remain)
      }
    }

    indexEnv <- rep(1:NEnv, each = nTST)
    yNA <- y

    for (j in 1:NEnv) {
      if (NEnv < 2) {
        yNA[indexNA] <- 2
      } else {
        yNA[indexNA[indexEnv == j], j] <- 2
      }
    }

    A <- matrix(rep(1, NTraits), ncol = NTraits)
    B1 <- kronecker(A, yNA)

    Names_MFormat <- colnames(new_Data[, -1])  # Remove GIDS column
    colnames(B1) <- Names_MFormat

    if (!is.null(Traits.testing)) {
      Traits_Selec_F <- c()
      for (r in seq_len(length(Traits.testing))) {
        Traits_Selec <- which(grepl(Traits.testing[r], Names_MFormat) == TRUE)
        Traits_Selec_F <- c(Traits_Selec_F,Traits_Selec)
      }

      B1[, -c(Traits_Selec_F)] <- 1
    }
    pos <- c(B1)
    p_list[[paste('partition', i, sep = '')]] <- which(pos == 2)
  }


  out <- list(
    CrossValidation_list = p_list,
    Environments = as.character(DataSet$Env),
    Traits.testing = Traits.testing,
    Traits = as.character(DataSet$Trait),
    Response = DataSet$Response,
    Observations = NLine
  )

  class(out) <- 'CrossValidation'
  return(out)
}
