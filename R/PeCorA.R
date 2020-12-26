#' @title PeCorA
#' @description core function of PeCorA workflow
#' @param scaled_peptides scaled data
#' @author Maria Dermit <maria.dermit@qmul.ac.uk>
#' @return dataframe output of PeCorA analysis
#' @details PeCorA function relies on linear model
#' to assess whether the slope of a peptide’s change
#' across treatment groups differs from the slope of
#' all other peptides assigned to the same protein.
#' @examples
#' if (interactive()) {
#'   disagree_peptides <- PeCorA(scaled_peptides)
#' }
#' @rdname PeCorA
#' @importFrom stats lm
#' @importFrom stats anova
#' @import dplyr
#' @importFrom dplyr pull
#' @export
PeCorA <- function(scaled_peptides) {
  # Show error if inputs are not the required classes
  assertthat::assert_that(is.data.frame(scaled_peptides))

  # Show error if there is no variable protein
  if (!"Protein" %in% colnames(scaled_peptides)) {
    stop("There is no Protein column")
  }

  # Show error if there is no modpep_z
  if (!"modpep_z" %in% colnames(scaled_peptides)) {
    stop("There is no modpep_z column")
  }

  # Show error if there is no Condition
  if (!"Condition" %in% colnames(scaled_peptides)) {
    stop("There is no Condition column")
  }

  # Show error if there is no ms1adj
  if (!"ms1adj" %in% colnames(scaled_peptides)) {
    stop("There is no ms1adj column")
  }

  print("Scaled peptides are ready for PeCorA")
  t <- scaled_peptides
  # get all unique protein groups
  # get all unique protein groups
  print("checking which proteins still have at least 2 peptides")
  pgs <- levels(as.factor(t$Protein))
  pgs_morethan2 <- c()
  for (x in pgs) {
    if (length(unique(t[t$Protein %in% x, "modpep_z"])) > 1) {
      pgs_morethan2 <- c(pgs_morethan2, x)
    }
  }

  ### loop through proteins with >2 pep, get subset df
  ###### look through peptide precursors in that protein
  ######### check linear model, record p value
  ############ adjust pvals within each protein ### could do that differently?
  allp <- list() # empty list to store pvalues for each protein
  print("computing the interaction p-values")

  for (x in pgs_morethan2) { # loop through protein groups
    tmpdf <- t[t$Protein == x, ] ## get the subset dataframe
    tmpdf["allothers"] <- rep("allothers", times = nrow(tmpdf))
    pvalues <- c(rep(0, length(unique(tmpdf$modpep_z))))
    i <- 1
    for (y in unique(tmpdf$modpep_z)) {
      subtmpdf <- tmpdf
      subtmpdf[which(tmpdf$modpep_z == y), "allothers"] <- y
      tmplm <- lm(subtmpdf$ms1adj ~ subtmpdf$Condition * subtmpdf$allothers)
      tmpanova <- anova(tmplm)
      pvalues[i] <- tmpanova$`Pr(>F)`[3]
      i <- i + 1
    }
    allp[[x]] <- pvalues # record p-values
  }


  ### post testing analysis
  message(paste("number of proteins tested =", length(allp), sep = " "))
  message(paste("number of peptides tested =", length(unlist(allp)), sep = " "))



  ######## make table of the peptides that disagree   ##########
  ### dataframe with all values
  message("started making data table")
  alldf <- data.frame()
  x <- names(allp)[1]
  for (x in names(allp)) {
    # print(x)
    tmpdf <- t[t$Protein == x, ]
    # tmpdf["allothers"]<-rep("allothers", times=nrow(tmpdf))
    tmp_peps <- unique(tmpdf$modpep_z)
    if (length(tmp_peps) > 0) {
      tmp_pval <- allp[[x]]
      tmpout <- cbind.data.frame(protein = rep(x, length(allp[[x]])), tmp_peps, tmp_pval = as.numeric(tmp_pval))
      alldf <- rbind(alldf, tmpout)
    }
  }
  message("correcting p-values")
  alldf$adj_pval <- p.adjust(alldf$tmp_pval, method = "BH") ## adjust p-values
  alldf_ordered <- alldf[order(alldf$adj_pval), ] ## sort

  ## print summary results
  message(paste("number of uncorrelated peptides =", nrow(alldf[alldf$adj_pval <= 0.01, ]), sep = " "))
  message(paste("number of proteins with uncorrelated peptides =",
    length(unique(alldf[alldf$adj_pval <= 0.01, ]$protein)),
    sep = " "
  ))

  colnames(alldf_ordered)[2] <- "peptide"
  colnames(alldf_ordered)[3] <- "pvalue"
  alldf_ordered
}
