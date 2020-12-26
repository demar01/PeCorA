#' @title import_LFQ_PeCorA
#' @description reads peptide.txt MaxQuant output and process it for PeCorA.
#' @param peptides_data imported peptide.txt file output of MaxQuant
#' @param protein Leading.razor.protein column
#' @param sequence Sequence column (peptide sequence)
#' @param condition1 experimental condition 1
#' @param condition2 experimental condition 2
#' @param condition3 experimental condition 3
#' @author Maria Dermit <maria.dermit@qmul.ac.uk>
#' @return data frame ready for PeCorA analysis
#' @details output files containing columns: Peptide.Modified.Sequence',
#'  Protein, variable,value,Condition,BioReplicate
#' @examples
#' data(peptides_data_filtered)
#' pecora_format <- import_LFQ_PeCorA(peptides_data_filtered,
#'   protein = "Leading.razor.protein",
#'   sequence = "Sequence", condition1 = "control", condition2 = "_COVID",
#'   condition3 = "NON.COVID"
#' )
#' @rdname import_LFQ_PeCorA
#' @importFrom reshape melt
#' @export
import_LFQ_PeCorA <- function(peptides_data,
                              protein = "Leading.razor.protein",
                              sequence = "Sequence",
                              condition1, condition2, condition3) {
  # Show error if inputs are not the required classes
  assertthat::assert_that(
    is.data.frame(peptides_data),
    is.character(protein),
    length(protein) == 1,
    is.character(sequence),
    length(sequence) == 1,
    is.character(condition1),
    is.character(condition2),
    is.character(condition3)
  )

  # Show error if there are no LFQ columns
  if (length(grep(colnames(peptides_data), pattern = "LFQ")) < 1) {
    stop("There are no LFQ columns")
  }

  # Show error if there is no variable razor protein
  if (!"Leading.razor.protein" %in% colnames(peptides_data)) {
    stop("There is no Leading.razor.protein column")
  }

  # Show error if there is no variable Sequence
  if (!"Sequence" %in% colnames(peptides_data)) {
    stop("There is no Sequence column")
  }

  message("Data is in the right format")


  idcol <- c("Sequence", "Leading.razor.protein")
  pmelt <- reshape::melt(peptides_data,
    id.vars = idcol,
    measure.vars = grep(colnames(peptides_data),
      pattern = "LFQ"
    )
  )

  pmelt$Condition <- rep(0, nrow(pmelt))

  pmelt$Condition[grep(pmelt$variable, pattern = condition1)] <-
    gsub("[^0-9A-Za-z///' ]", "", condition1, ignore.case = TRUE)
  pmelt$Condition[grep(pmelt$variable, pattern = condition2)] <-
    gsub("[^0-9A-Za-z///' ]", "", condition2, ignore.case = TRUE)
  pmelt$Condition[grep(pmelt$variable, pattern = condition3)] <-
    gsub("[^0-9A-Za-z///' ]", "", condition3, ignore.case = TRUE)

  message("Renamed")

  charnames <- as.character(pmelt$variable)
  pmelt$BioReplicate <- lapply(FUN = PeCorA::splt_last, charnames)
  message("Bioreplicates added")
  df <- apply(pmelt, 2, as.character)
  colnames(df)[1] <- "Peptide.Modified.Sequence"
  colnames(df)[2] <- "Protein"
  df <- as.data.frame(df)
  return(df)
}
