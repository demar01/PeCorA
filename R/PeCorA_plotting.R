#' @title PeCorA_plotting
#' @description generates boxplot using PeCorA output
#' @param disagree_peptides dataframe object with disagree peptides
#' @param disagree_peptides_selection dataframe object selection of disagree peptides
#' @param scaled_peptides scaled peptides
#' @author Maria Dermit <maria.dermit@qmul.ac.uk>
#' @return PeCorA plot
#' @details This function generates boxplots for a given protein representing
#' the peptides that are statistically different from the quantities of all the
#' other peptides in green and all other peptides in grey.
#' @examples
#' if (interactive()) {
#'   PeCorA_plotting_plot <- PeCorA_plotting(
#'     disagree_peptides,
#'     disagree_peptides[12, ], scaled_peptides
#'   )
#' }
#' @rdname PeCorA_plotting
#' @import ggplot2
#' @export

PeCorA_plotting <- function(disagree_peptides, disagree_peptides_selection, scaled_peptides) {
  w <- disagree_peptides
  u <- disagree_peptides_selection
  v <- scaled_peptides
  sign_prots <- as.character(unique(u[u$adj_pval <= 0.01, ]$protein))

  for (x in sign_prots) {
    tmpdf <- v[v$Protein == x, ]
    tmpdf["allothers"] <- rep("allothers", times = nrow(tmpdf))
    tmpalldf <- w[w$protein %in% x, ]
    tmp_peps <- unique(tmpalldf$peptide)[which(tmpalldf$adj_pval < 0.01)]
    if (length(tmp_peps) > 0) {
      plots <- list()
      for (y in tmp_peps) {
        subtmpdf <- tmpdf
        subtmpdf[which(tmpdf$modpep_z == y), "allothers"] <- y
        # boxplot(ms1adj ~ allothers*Condition, subtmpdf)
        plots[[y]] <- ggplot(subtmpdf, aes(
          x = .data$Condition,
          y = .data$ms1adj,
          fill = .data$allothers
        )) +
          geom_boxplot(size = 1.5) +
          geom_point(pch = 21, position = position_jitterdodge(), size = 3) +
          theme(text = element_text(size = 20)) +
          ylab("Log2(intensity)-Log2(control)") +
          xlab("Group") +
          ggtitle(x) +
          scale_fill_manual(values = c("grey", "#00BA38")) +
          theme(legend.position = "bottom")
      }
    }
  }
  plots
}
