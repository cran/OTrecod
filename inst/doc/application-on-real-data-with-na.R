## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("OTrecod")

## ----results='hide',message=FALSE,warning=FALSE-------------------------------
library(OTrecod)

## ----eval=FALSE---------------------------------------------------------------
#  # Install development version from GitHub
#  devtools::install_github("otrecoding/OTrecod")

## ----results='hide',message=FALSE,warning=FALSE-------------------------------
# load("C:\\Users\\secr\\Desktop\\newpack\\datas_vignet2.RData")
data(ncds_14); data(ncds_5)

## ----summa--------------------------------------------------------------------
summary(ncds_14); summary(ncds_5)

## ----merg---------------------------------------------------------------------
merged_tab <- merge_dbs(ncds_14, ncds_5,
                        row_ID1 = 1, row_ID2 = 1,
                        NAME_Y = "GO90", NAME_Z = "RG91",
                        ordinal_DB1 = 3, ordinal_DB2 = 4,
                        impute = "MICE", R_MICE = 2,
                        seed_choice = 3023)

## ----summa2-------------------------------------------------------------------
head(merged_tab$DB_READY); dim(merged_tab$DB_READY)

## ----selec1-------------------------------------------------------------------
# ncds_14
sel_pred_GO90 <- select_pred(merged_tab$DB_READY,
                             Y = "Y", Z = "Z",
                             ID = 1 , OUT = "Y",
                             nominal = c(1:5,7), ordinal = 6,
                             RF = FALSE)


## ----selec2-------------------------------------------------------------------
# ncds_5
sel_pred_RG91 <- select_pred(merged_tab$DB_READY, Y = "Y", Z = "Z",
                             ID = 1, OUT = "Z",
                             nominal = c(1:5,7), ordinal = 6,
                             RF = FALSE)


## ----resel1-------------------------------------------------------------------
## ncds_14
sel_pred_GO90$collinear_PB$VCRAM

## ncds_5
sel_pred_RG91$collinear_PB$VCRAM


## ----resel2-------------------------------------------------------------------
## ncds_14
sel_pred_GO90$vcrm_OUTC_cat

## ncds_5
sel_pred_RG91$vcrm_OUTC_cat


## ----resel3-------------------------------------------------------------------
## ncds_14
sel_pred_GO90$vcrm_X_cat

## ncds_5
sel_pred_RG91$vcrm_X_cat


## ----readydata----------------------------------------------------------------
merged_fin = merged_tab$DB_READY[, -4]
head(merged_fin, 3)


## ----outc11-------------------------------------------------------------------
outc1 = OT_outcome(merged_fin, nominal = c(1:4), ordinal = 5:6,
                   dist.choice = "E", indiv.method = "sequential",
                   which.DB = "B")

## ----joint1-------------------------------------------------------------------
outj1 = OT_joint(merged_fin, nominal = 1:4, ordinal = 5:6,
                 dist.choice = "E", which.DB = "B")


## ----verif1-------------------------------------------------------------------

### For the model outc1
verif_outc1   = verif_OT(outc1, ordinal = FALSE, 
                         stab.prob = TRUE, min.neigb = 5)

### For the model outj1
verif_outj1   = verif_OT(outj1, ordinal = FALSE, 
                         stab.prob = TRUE, min.neigb = 5)


## ----verif2-------------------------------------------------------------------

### For the model outc1: Hellinger distance
verif_outc1$hell

### For the model outj1: Hellinger distance
verif_outj1$hell


## ----verif3-------------------------------------------------------------------

### For the model outc1: Hellinger distance
verif_outc1$res.prox

### For the model outj1: Hellinger distance
verif_outj1$res.prox


## ----verif4-------------------------------------------------------------------

### For the model outc1: Hellinger distance
verif_outc1$res.stab

### For the model outj1: Hellinger distance
verif_outj1$res.stab


