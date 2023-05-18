#' Este script acompanha o "Exercício 3" da disciplina Ciência de Dados 
#' Aplicada à Saúde (Código: IAA019)
#' 
#' Para executar este script, primeiro fazer download do arquivo ".RData"
#' denominado "tcgaLIHCdata_preprocessed.RData", disponível em 
#' "https://github.com/csgroen/RTN_example_TCGA_LIHC"

################################################################################
### Packages required to run the analysis
################################################################################
library(SummarizedExperiment)
library(survival)
library(survminer)

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

################################################################################
### Data preprocessing for survival analysis 
################################################################################

#--- Check survival-related varibles 
#--- "OS" = Overall Survival
#--- "PFI" = Progression Free Interval
#--- "OS" is the event, "OS.time" is time in days, and  "OS.time.months" in time in months.
names(colAnnotation)
# [1] "bcr_patient_barcode"         "gender"                      "Age"                         "ajcc_pathologic_tumor_stage"
# [5] "OS"                          "OS.time"                     "PFI"                         "PFI.time"                   
# [9] "OS.time.months"              "PFI.time.months"             "Tumor_Stage"                 "Stage"                      
# [13] "Stage_I"                     "Stage_II"                    "Stage_III"                   "Stage_IV"                   
# [17] "mRNA"                        "mRNA1"                       "mRNA2"                       "mRNA3"                      
# [21] "mRNA4"                       "mRNA5

#--- Now check the "ajcc_pathologic_tumor_stage" varible, which we will assess 
#--- in the survival analysis 
table(colAnnotation$ajcc_pathologic_tumor_stage, useNA="ifany")
# [Discrepancy]       Stage I      Stage II     Stage III    Stage IIIA    Stage IIIB    Stage IIIC      Stage IV 
#             2           171            86             3            65             8             9             2 
# Stage IVA     Stage IVB 
# 1             2

#--- Re-assign small groups in a new "ajcc_stage" varible
colAnnotation$ajcc_stage <- colAnnotation$ajcc_pathologic_tumor_stage
idx <- colAnnotation$ajcc_stage%in%c("Stage IIIA","Stage IIIB","Stage IIIC")
colAnnotation$ajcc_stage[idx] <- "Stage III"
idx <- colAnnotation$ajcc_stage%in%c("Stage IVA","Stage IVB")
colAnnotation$ajcc_stage[idx] <- "Stage IV"
idx <- colAnnotation$ajcc_stage%in%c("[Discrepancy]")
colAnnotation$ajcc_stage[idx] <- NA
table(colAnnotation$ajcc_stage, useNA="ifany")
# Stage I  Stage II Stage III  Stage IV 
#     171        86        85         5

################################################################################
### Run Kaplan-Meie survival analysis with "survfit" function
################################################################################
# fm <- formula("Surv(PFI.time.months, OS) ~ ajcc_stage")
# fit <- do.call(survfit, args= list(formula=fm, data=colAnnotation))
fit <- survfit(Surv(PFI.time.months, OS) ~ ajcc_stage, data = colAnnotation)
ggsurvplot(fit, data=colAnnotation, legend="right")

################################################################################
### Run Cox regression analysis with "coxph" function
################################################################################
fit <- coxph(Surv(OS.time, OS) ~ ajcc_stage, data = colAnnotation)
fit
# Call:
#         coxph(formula = Surv(OS.time, OS) ~ ajcc_stage, 
#               data = colAnnotation)
# 
#                                        coef exp(coef) se(coef)     z        p
# ajcc_pathologic_tumor_stageStage II  0.3527    1.4229   0.2500 1.411  0.15832
# ajcc_pathologic_tumor_stageStage III 0.9844    2.6763   0.2155 4.568 4.92e-06
# ajcc_pathologic_tumor_stageStage IV  1.7040    5.4961   0.6002 2.839  0.00452
# 
# Likelihood ratio test=23.78  on 3 df, p=2.774e-05
# n= 346, number of events= 116 
# (25 observations deleted due to missingness)

#-------------------------------------------------------------------------------
# Generate a summary of the Cox Model with "ggforest"
ggforest(fit)


