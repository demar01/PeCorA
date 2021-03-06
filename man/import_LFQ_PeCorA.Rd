% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_LFQ_PeCorA.R
\name{import_LFQ_PeCorA}
\alias{import_LFQ_PeCorA}
\title{import_LFQ_PeCorA}
\usage{
import_LFQ_PeCorA(
  peptides_data,
  protein = "Leading.razor.protein",
  sequence = "Sequence",
  condition1,
  condition2,
  condition3
)
}
\arguments{
\item{peptides_data}{imported peptide.txt file output of MaxQuant}

\item{protein}{Leading.razor.protein column}

\item{sequence}{Sequence column (peptide sequence)}

\item{condition1}{experimental condition 1}

\item{condition2}{experimental condition 2}

\item{condition3}{experimental condition 3}
}
\value{
data frame ready for PeCorA analysis
}
\description{
reads peptide.txt MaxQuant output and process it for PeCorA.
}
\details{
output files containing columns: Peptide.Modified.Sequence',
Protein, variable,value,Condition,BioReplicate
}
\examples{
data(peptides_data_filtered)
pecora_format <- import_LFQ_PeCorA(peptides_data_filtered,
  protein = "Leading.razor.protein",
  sequence = "Sequence", condition1 = "control", condition2 = "_COVID",
  condition3 = "NON.COVID"
)
}
\author{
Maria Dermit \href{mailto:maria.dermit@qmul.ac.uk}{maria.dermit@qmul.ac.uk}
}
