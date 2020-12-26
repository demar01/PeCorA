#' @title PeCorA_preprocessing
#' @description steps to pre-process data for PeCorA
#' @param pecora_format dataframe in PeCorA format
#' @param area_column_name numeric column number with peak areas
#' @param threshold_to_filter numeric threshold for peak areas to filter
#' @param control_name character control reference
#' @author Maria Dermit <maria.dermit@qmul.ac.uk>
#' @return dataframe output of pre-processed data ready for PeCorA analysis
#' @details Peak areas are log transformed and scaled to center.
#' After global scaling, each peptide scaling was performed to center
#' each peptide relative to the mean of the control group’s peak area.
#' @examples
#' if(interactive()){
#' scaled_peptides <- PeCorA_preprocessing(pecora_format,
#'                                          area_column_name=4,
#'                                          threshold_to_filter=100,
#'                                          control_name="control")
#' }
#' @rdname PeCorA_preprocessing
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats p.adjust
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @export
PeCorA_preprocessing <- function (pecora_format,
                                  area_column_name,
                                  threshold_to_filter,
                                  control_name){

  assertthat::assert_that(is.data.frame(pecora_format),
                          is.numeric(area_column_name),
                          length(area_column_name) == 1,
                          is.numeric(threshold_to_filter),
                          length(threshold_to_filter) == 1,
                          is.character(control_name))

  # Show error if control_name is not in  Condition
  if(!control_name %in% pecora_format$Condition) {
    stop("Control_name not within conditions")
  }



  pecora_format["modpep_z"] = paste(pecora_format$Peptide.Modified.Sequence, "all", sep = "_")
  # make column that combines condition and replicate
  pecora_format["condition_rep"] = paste(pecora_format$Condition, pecora_format$BioReplicate, sep = "_")
  n_reps_total <- length(unique(pecora_format$condition_rep))
  ## filter the data to remove NA, <100 area, and then reps without all measures
  if(suppressWarnings(length(which(is.na(as.numeric(pecora_format[,area_column_name]))==TRUE))>0)){
    pecora_format <- pecora_format[-suppressWarnings(which(is.na(as.numeric(pecora_format[,area_column_name]))==TRUE)),]
  }
  pecora_format <- pecora_format[-which(as.numeric(pecora_format[,area_column_name])<=threshold_to_filter),]
  pecora_format <- pecora_format[pecora_format$modpep_z %in% names(which(table(pecora_format$modpep_z)==n_reps_total)),]

  print("Data has been pre-filtered")
  ## scale the data and then plot
  pecora_format$ms1log2<-log2(as.numeric(pecora_format[,area_column_name]))
  pecora_format$ms1scaled <-scale_by(ms1log2 ~ Condition*BioReplicate, pecora_format)

  print("Data has been scaled")

  ms1adj <- rep(0, nrow(pecora_format))

  t_cntrl<-pecora_format[pecora_format$Condition==control_name,]
  ms1scaled_cntrl<-t_cntrl[,"ms1scaled"]
  ms1scaled_full<-pecora_format[,"ms1scaled"]

  for(x in unique(pecora_format$modpep_z) ){
    ms1adj[which(pecora_format$modpep_z==x)] <- suppressWarnings(ms1scaled_full[which(pecora_format$modpep_z==x)] - mean(ms1scaled_cntrl[which(t_cntrl$modpep_z==x)]))  # subtract the ave of control

  }

  pecora_format$ms1adj<-ms1adj
  pecora_format
}

