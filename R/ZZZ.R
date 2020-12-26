.onLoad <- function(libname = find.package("PeCorA"), pkgname = "PeCorA") {
  if (getRversion() >= "4.0.0") {
    utils::globalVariables(
      c( # data
        "peptides_data_filtered",

        # import_LFQ_PeCorA.R globalVariables
        "pmelt", "Condition", "charnames", "BioReplicate", "df",

        # PeCorA_preprocessing.R globalVariables
        "n_reps_total", "ms1log2", "ms1adj", "pecora_format_cntrl",

        # PeCorA.R globalVariables
        "pgs", "t", "adj_pval", "alldf_ordered", "pgs_morethan2",

        # PeCorA_plotting.R globalVariables
        "tmpdf", "tmp_peps", "sign_prots",

        #Scale_by globalVariables
        "x", "f", "byrow", "drop"
      )
    )
  }
  invisible()
}
