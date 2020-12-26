# Peptide correlation analysis (PeCorA) <img src="man/figures/PECORA_hex.png" align="right" height="150"/>

`PeCorA` is a package that contains a number of functions to detect
discordant peptide quantities in shotgun proteomics sata by Peptide
Correlation Analysis. The package also contains published
proteomics dataset processed with processing tools such as [MaxQuant](http://www.nature.com/nbt/journal/v26/n12/full/nbt.1511.html).

### Install

Once installed, load the package by writing in the console

``` r
library(PeCorA)
```

### Available datasets

Currently, there are three datasets available in `PeCorA`.

| Data              | Description                                                                                                                                              |
| :---------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Covid\_peptides   | Large-scale proteomic Analysis of COVID-19 Severity                                                                                                      |

### Loading data

Data available in the package is loaded into the `R` session using the
`load` function; for instance, to get a recent large-scale analysis of COVID19 severity from [Overmyer et al 2020](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7388490/):
 
``` r
data(peptides_data_filtered)
```

To get more information about a dataset, see its manual page.

``` r
?peptides_data_filtered
```

### How to use

PeCorA requires a filename.csv file containing table in long format of
peptides, their quantities, and the proteins they belong to. This file
must at least contain the following columns (check spelling and letter
case):

“Condition” - group labels of the conditions. Can be more than 2 but
must be at least 2. “Peptide.Modified.Sequence” - peptide sequence
including any modifications “BioReplicate” - numbering for biological
replicates “Protein” - protein membership for each peptide

You may need to transform your data into PeCorA-ready format. For
example ransform peptides.txt output of MaxQuant into t use function
`import_LFQ_PeCorA`.

### Functions

The main function of the package is called `PeCorA`, which fits a linear
model with interaction between peptides and biological treatment groups.

### Contact

If you have any questions or suggestions please contact us:

Maria Dermit : maria.dermit at qmul.ac.uk

Jesse Meyer: jesmeyer at mcw.edu

### Additional information 

Please see the original [paper](https://pubs.acs.org/doi/abs/10.1021/acs.jproteome.0c00602) 
