#' Este script acompanha o "Exercício 2" da disciplina Ciência de Dados 
#' Aplicada à Saúde (Código: IAA019)
#' 
#' Para executar este script, primeiro fazer download do arquivo ".RData"
#' denominado "tcgaLIHCdata_preprocessed.RData", disponível em 
#' "https://github.com/csgroen/RTN_example_TCGA_LIHC"

################################################################################
### Packages required to access a SummarizedExperiment object
################################################################################
library(SummarizedExperiment)

################################################################################
### Loading data
################################################################################
load(file = "./tcgaLIHCdata_preprocessed.RData")
class(tcgaLIHCdata)
# [1] "RangedSummarizedExperiment"
# attr(,"package")
# [1] "SummarizedExperiment"

#--- Check data dimention
dim(tcgaLIHCdata)
# [1] 29885   371

#--- Extract data matrix and metadata
gexp <- assay(tcgaLIHCdata)
rowAnnotation <- rowData(tcgaLIHCdata)
colAnnotation <- colData(tcgaLIHCdata)

#--- Check data objects
class(gexp)
# [1] "matrix"
class(rowAnnotation)
# [1] "DFrame"
class(colAnnotation)
# [1] "DFrame"

#--- Other checks
gexp[1:3,1:4]
#                 TCGA-DD-A3A3-01A-11R-A22L-07 TCGA-DD-A1EF-01A-11R-A131-07 TCGA-ED-A627-01A-12R-A311-07
# ENSG00000000003                  22.37358577                  27.57565722                   18.8409124
# ENSG00000000005                   0.04209836                   0.01943315                    0.0246897
# ENSG00000000419                  13.36474791                  32.92042747                   20.6326769  

#--- Other checks
rowAnnotation
# DataFrame with 29885 rows and 3 columns
# ENSEMBL      SYMBOL         OG_ENSEMBL
# <character> <character>        <character>
#   ENSG00000000003 ENSG00000000003      TSPAN6 ENSG00000000003.13
# ENSG00000000005 ENSG00000000005        TNMD  ENSG00000000005.5
# ENSG00000000419 ENSG00000000419        DPM1 ENSG00000000419.11
# ENSG00000000457 ENSG00000000457       SCYL3 ENSG00000000457.12
