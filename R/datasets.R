#' @name WheatToy
#' @docType data
#' @title Phenotypic values and Genetic values of Wheat toy dataset.
#' @description A data set based on a portion of the data used in the study
#' from a collection of 599 historical CIMMYT wheat lines.
#' The wheat data set is from CIMMYT's Global Wheat Program.
#'
#' The variables that import the dataset are as follows:
#'
#' * 'phenoWheatToy': Phenotypic values.
#' * 'genoWheatToy':  Genomic values.
#' @source International Maize and Wheat Improvement Center (CIMMYT), Mexico.
#' @usage data(WheatToy)
#' @format 2 objects are loaded (phenoWheatToy and genoWheatToy)
#' @references McLaren, C. G., R. Bruskiewich, A.M. Portugal, and A.B. Cosico. 2005. The International Rice Information System. A platform for meta-analysis of rice crop data. \emph{Plant Physiology} \bold{139}: 637-642.
#' @keywords Wheat dataset
NULL


#' @name phenoWheatToy
#' @docType data
#' @title Phenotypic values of Wheat toy dataset.
#' @description A data set based on a portion of the data used in the study
#' from a collection of 599 historical CIMMYT wheat lines.
#' The wheat data set is from CIMMYT's Global Wheat Program.
#
#' @source International Maize and Wheat Improvement Center (CIMMYT), Mexico.
#' @usage phenoWheatToy
#' @format data.frame, 90 row per 4 columns.
#' @keywords Wheat dataset
NULL

#' @name genoWheatToy
#' @docType data
#' @title Genomic values of Wheat toy dataset.
#' @description A data set based on a portion of the data used in the study
#' from a collection of 599 historical CIMMYT wheat lines.
#' The wheat data set is from CIMMYT's Global Wheat Program.
#
#' @source International Maize and Wheat Improvement Center (CIMMYT), Mexico.
#' @usage genoWheatToy
#' @format 30 x 30 data.frame
#' @keywords Wheat dataset
NULL




#' @name WheatIranianToy
#' @title Wheat Iranian Toy Data
#' @description This data set is based on the data set used in the study of (Crossa et al., 2016).
#' The original dataset was composed of 2374 wheat lines that were evaluated in field (D) and heat (H) drought experiments at the CIMMYT experimental station near Obregón City, Sonora, Mexico (27°20' N, 109°54' W, 38 meters above sea level), during the Obregón 2010-2011 cycle.
#'  Two traits were evaluated (DTM days at maturity and DTH days to heading). From a total of 40,000 markers, after quality control 39,758 markers were used.
#'   To load this dataset in the package we only use 30 lines, and we have identified this data set as WheatIranianToy. For more details, see the study of (Crossa et al., 2016).
#'
#' @docType data
#' @usage data(WheatIranianToy)
#' @format 2 objects are loaded (phenoIranianToy and genoIranianToy)
#' @references Crossa, J., Jarquín, D., Franco, J., Pérez-Rodríguez, P., Burgueño, J., Saint-Pierre, C., Singh, S. (2016). Genomic Prediction of Gene Bank Wheat Landraces. G3: Genes|Genomes|Genetics, 6(7), 1819–1834. https://doi.org/10.1534/g3.116.029637
NULL

#' @name phenoIranianToy
#' @docType data
#' @title Phenotypic values of Iranian toy dataset.
#' @description A data set based on a portion of the data used in the study of (Crossa et al., 2016).
#
#' @source International Maize and Wheat Improvement Center (CIMMYT), Mexico.
#' @usage phenoIranianToy
#' @format data.frame, 60 row per 4 columns.
#' @keywords Wheat dataset
#' @references Crossa, J., Jarquín, D., Franco, J., Pérez-Rodríguez, P., Burgueño, J., Saint-Pierre, C., Singh, S. (2016). Genomic Prediction of Gene Bank Wheat Landraces. G3: Genes|Genomes|Genetics, 6(7), 1819–1834. https://doi.org/10.1534/g3.116.029637
NULL

#' @name genoIranianToy
#' @docType data
#' @title Genomic values of Iranian toy dataset.
#' @description A data set based on a portion of the data used in the study of (Crossa et al., 2016).
#'
#' @source International Maize and Wheat Improvement Center (CIMMYT), Mexico.
#' @usage genoIranianToy
#' @format 30 x 30 matrix
#' @keywords Wheat dataset
#' @references Crossa, J., Jarquín, D., Franco, J., Pérez-Rodríguez, P., Burgueño, J., Saint-Pierre, C., Singh, S. (2016). Genomic Prediction of Gene Bank Wheat Landraces. G3: Genes|Genomes|Genetics, 6(7), 1819–1834. https://doi.org/10.1534/g3.116.029637
NULL





#' @name WheatJapa50
#' @title Wheat Japa 50 Data
#' @description This data set is also based on the data used in the study of (Ben Hassen et al., 2018). The original dataset was composed of a sample of 230 lines evaluated for three traits. Each of them was evaluated in one environment. The total numbers GBS data were 32,066 SNPs was obtained with a heterozygosity rate <5% and minor allele frequency (MAF) > 5%. To load this dataset in the package we only use 50 lines, and we have identified this data set as WheatJapa50. For more details, see the study of (Ben Hassen et al., 2018). Also, in place of markers we provide the GRM matrix.
#' @docType data
#' @usage data(WheatJapa50)
#' @format 2 objects are loaded (phenoJapa50 and genoJapa50)
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name phenoJapa50
#' @docType data
#' @title Phenotypic values of Japa dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#
#' @usage phenoJapa50
#' @format data.frame, 50 row per 4 columns.
#' @keywords Japa dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name genoJapa50
#' @docType data
#' @title Genomic values of Japa dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#'
#' @usage genoJapa50
#' @format 50 x 50 matrix
#' @keywords Japa dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL



#' @name WheatJapa30
#' @title Wheat Japa 30 Data
#' @description This data set is also based on the data used in the study of (Ben Hassen et al., 2018). The original dataset was composed of a sample of 167 lines evaluated for six traits each of them was evaluated in one environment. The total numbers of genome by sequencing (GBS) data were 32,066 SNPs and they were obtained with a heterozygosity rate <5% and minor allele frequency (MAF) > 5%. To load this dataset in the package we only use 30 lines, and we have identified this data set as WheatJapa30. For more details, see the study of (Ben Hassen et al., 2018). Also, in place of markers we provide with the GRM matrix.
#' @docType data
#' @usage data(WheatJapa30)
#' @format 2 objects are loaded (phenoJapa30 and genoJapa30)
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name phenoJapa30
#' @docType data
#' @title Phenotypic values of Japa  dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#
#' @usage phenoJapa30
#' @format data.frame, 30 row per 7 columns.
#' @keywords Japa dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name genoJapa30
#' @docType data
#' @title Genomic values of Japa dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#'
#' @usage genoJapa30
#' @format 30 x 30 matrix
#' @keywords Japa dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL




#' @name WheatMadaToy
#' @title Wheat Mada Data
#' @description This data set is based on the data used in the study of (Ben Hassen, Bartholomé, Valè, Cao, & Ahmadi, 2018). The original dataset was composed of a sample of 188 wheat lines evaluated for six traits. Each of them was evaluated in one environment. The total numbers of genome by sequencing (GBS) data were 32,066 single nucleotide polymorphisms (SNPs) that was obtained with a heterozygosity rate < 5% and minor allele frequency (MAF) > 5%. To load this dataset in the package we only use 30 lines, and we have identified this data set as WheatMadaToy. For more details, see the study of (Ben Hassen et al., 2018).
#' @docType data
#' @usage data(WheatMadaToy)
#' @format 2 objects are loaded (phenoMada and genoMada)
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name phenoMada
#' @docType data
#' @title Phenotypic values of Mada dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#
#' @usage phenoMada
#' @format data.frame, 50 row per 7 columns.
#' @keywords Mada dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL

#' @name genoMada
#' @docType data
#' @title Genomic values of Mada dataset.
#' @description A data set based on a portion of the data used in the study of (Ben Hassen et al., 2018).
#'
#' @usage genoMada
#' @format 50 x 50 matrix
#' @keywords Mada dataset
#' @references Ben Hassen, M., Bartholomé, J., Valè, G., Cao, T.-V., & Ahmadi, N. (2018). Genomic Prediction Accounting for Genotype by Environment Interaction Offers an Effective Framework for Breeding Simultaneously for Adaptation to an Abiotic Stress and Performance Under Normal Cropping Conditions in Rice. G3: Genes|Genomes|Genetics, 8(July), 2319–2332. https://doi.org/10.1534/g3.118.200098
NULL



#' @name MaizeToy
#' @title Maize Data
#' @description This data set is based on the data set  used in the study of (Montesinos-López et al., 2017). The original dataset is composed of a sample of 309 maize lines evaluated for three traits: anthesis-silking interval (ASI), plant height (PH), grain yield (GY), each of them was evaluated in three optimal environments (Env1, Env2 and Env3). The total numbers of GBS data were 681,257 SNPs, and, after filtering for missing values and minor allele frequency, were used 158,281 SNPs for the analyses. To load this dataset in the package we only use 30 lines, and we have identified this data set as MaizeToy. For more details, see the study of (Montesinos-López et al., 2017).
#' @docType data
#' @usage data(MaizeToy)
#' @format 2 objects are loaded (phenoMaizeToy and genoMaizeToy)
#' @references Montesinos-López, O. A., Montesinos-López, A., Crossa, J., Montesinos-López, J. C., Luna-Vázquez, F. J., Salinas, J., … Buenrostro-Mariscal, R. (2017). A Variational Bayes Genomic-Enabled Prediction Model with Genotype × Environment Interaction. G3: Genes|Genomes|Genetics, 7(8), g3.117.041202. https://doi.org/10.1534/g3.117.041202
NULL

#' @name phenoMaizeToy
#' @docType data
#' @title Phenotypic values of Maize dataset.
#' @description A data set based on a portion of the data used in the study of (Montesinos-López et al., 2017).
#
#' @usage phenoMaizeToy
#' @format data.frame, 90 row per 5 columns.
#' @keywords Maize dataset
#' @references Montesinos-López, O. A., Montesinos-López, A., Crossa, J., Montesinos-López, J. C., Luna-Vázquez, F. J., Salinas, J., … Buenrostro-Mariscal, R. (2017). A Variational Bayes Genomic-Enabled Prediction Model with Genotype × Environment Interaction. G3: Genes|Genomes|Genetics, 7(8), g3.117.041202. https://doi.org/10.1534/g3.117.041202
NULL

#' @name genoMaizeToy
#' @docType data
#' @title Genomic values of Maize dataset.
#' @description A data set based on a portion of the data used in the study of (Montesinos-López et al., 2017).
#'
#' @usage genoMaizeToy
#' @format 30 x 30 matrix
#' @keywords Maize dataset
#' @references Montesinos-López, O. A., Montesinos-López, A., Crossa, J., Montesinos-López, J. C., Luna-Vázquez, F. J., Salinas, J., … Buenrostro-Mariscal, R. (2017). A Variational Bayes Genomic-Enabled Prediction Model with Genotype × Environment Interaction. G3: Genes|Genomes|Genetics, 7(8), g3.117.041202. https://doi.org/10.1534/g3.117.041202
NULL
