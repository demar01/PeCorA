#' COVID-19 plasma proteomics
#'
#' Large-Scale Multi-omic Analysis of COVID-19 Severity processed with MaxQuant
#' from Overmyer et al 2020.
#' The dataset contains the peptide output file from
#' \href{http://www.maxquant.org}{MaxQuant}.
#' @format A dataframe with 10645 rows and 169 variables:
#' \describe{
#'   \item{Sequence}{Peptide sequence}
#'   \item{Leading.razor.protein}{Name of protein}
#'   \item{LFQ.intensity columns (136)}{LFQ normalized mass spectrometry
#'   intensity, A.U.}
#' }
#' @return A data.frame
#' @source Overmyer et al 2020.
#' Large-Scale Multi-omic Analysis of COVID-19 Severity
#' https://doi.org/10.1016/j.cels.2020.10.003
#' \href{https://www.sciencedirect.com/science/article/pii/S2405471220303719}
#' {doi: 10.1016/j.cels.2020.10.003}.
#'
"peptides_data_filtered"
