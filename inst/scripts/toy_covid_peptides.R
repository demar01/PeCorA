library(tidyverse)
library(here)
peptides_data_gitub <- read.delim("https://raw.githubusercontent.com/jessegmeyerlab/PeCorA/master/inst/extdata/peptides.txt", stringsAsFactors = FALSE)
peptides_data_filtered <- peptides_data_gitub %>%
  select(!colnames(peptides_data_gitub)[grep(colnames(peptides_data_gitub), pattern="Identification.type")]) %>%
  select(!colnames(peptides_data_gitub)[grep(colnames(peptides_data_gitub), pattern=".Count")]) %>%
  select(!colnames(peptides_data_gitub)[grep(colnames(peptides_data_gitub), pattern="Experiment.")]) %>%
  select(!colnames(peptides_data_gitub)[grep(colnames(peptides_data_gitub), pattern="Intensity.")])

save(peptides_data_filtered, file = here("data","peptides_data_filtered.rda"), compress = "xz", compression_level = 9)


