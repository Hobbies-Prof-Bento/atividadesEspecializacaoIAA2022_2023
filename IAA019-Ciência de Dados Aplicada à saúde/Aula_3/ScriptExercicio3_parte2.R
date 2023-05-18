#' Este script acompanha o "Exercício 2" da disciplina Ciência de Dados 
#' Aplicada à Saúde (Código: IAA019)
#' 
#' Para executar este script, primeiro fazer download do arquivo ".RData"
#' denominado "tcgaLIHCdata_preprocessed.RData", disponível em 
#' "https://github.com/csgroen/RTN_example_TCGA_LIHC"

################################################################################
### Packages required to run the analysis
################################################################################
library(ComplexHeatmap)
library(SummarizedExperiment)
library(circlize)
library(RColorBrewer)

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

################################################################################
### Update row names with rowAnnotation
################################################################################
all(rownames(rowAnnotation)==rownames(gexp))
# [1] TRUE
rownames(gexp) <- rowAnnotation$SYMBOL

################################################################################
### Remove genes with low counts
################################################################################
idx <- rowSums(gexp!=0)/ncol(gexp)
gexp <- gexp[idx>0.3,]
dim(gexp)
# [1] 21547   371

################################################################################
### Filter the 'gexp' matrix using correlation with "Tumor_Stage" variable
################################################################################
idx <- cor(t(gexp), colAnnotation$Tumor_Stage, method = "spearman", 
           use="complete.obs")
idx <- sort.list(abs(idx), decreasing = T)[1:100]
gexp_filt <- gexp[idx,]
dim(gexp_filt)
# [1] 100  371

################################################################################
### Filter the 'gexp' matrix using feature abundance
################################################################################
# idx <- sort.list(apply(gexp, 1, mean), decreasing = T)[1:100]
# gexp_filt <- gexp[idx,]
# dim(gexp_filt)
# [1] 100  371

################################################################################
### Filter the 'colAnnotation', removing NAs
################################################################################
#--- Get sample annotations, and remove NAs
colAnnotation_filt <- colAnnotation[,c("Tumor_Stage"), drop=F]
colAnnotation_filt <- colAnnotation_filt[complete.cases(colAnnotation_filt),, drop=F]
dim(colAnnotation_filt)
# [1] 347   1
gexp_filt <- gexp_filt[ ,rownames(colAnnotation_filt)]
dim(gexp_filt)
# [1] 100 347

################################################################################
### Re-scale the data using a columnwise rank transformation
################################################################################
x <- gexp_filt
x <- t(apply(x, 1, rank));x <- x/max(x)
x <- t(scale(t(x), center = TRUE, scale = F))
dim(x)
# [1] 100 347

################################################################################
### Run semi- or unsupervised clustering analysis 
################################################################################

#--- Set col annotations
colAnnotation_filt$Tumor_Stage <- as.factor(colAnnotation_filt$Tumor_Stage)
levels(colAnnotation_filt$Tumor_Stage)
# [1] "1" "2" "3" "4"
pal1 <- brewer.pal(4,"Set1")
names(pal1) <- levels(colAnnotation_filt$Tumor_Stage)
top_annotation <- columnAnnotation(df=colAnnotation_filt, 
                                   col=list('Tumor_Stage'=pal1))

#--- Set a color scheme
pal2 <- rev(brewer.pal(7,"RdYlBu"))
bks <- quantile(as.numeric(x), probs = seq(0,1, length.out = length(pal2)))
colors <- colorRamp2(breaks = bks, colors = pal2)

#--- Run clustering analysis and plot a large heatmap with ComplexHeatmap
Heatmap(x, col = colors, name = "RNA-seq", 
        column_split = colAnnotation_filt$Tumor_Stage,
        show_row_names = F, show_column_names = F, 
        top_annotation=top_annotation,
        clustering_method_rows = "ward.D2", 
        clustering_distance_rows="spearman",
        clustering_method_columns = "ward.D2", 
        clustering_distance_columns = "spearman")
