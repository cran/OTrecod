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
library(StatMatch)
data(samp.A)

## ----eval=TRUE----------------------------------------------------------------
dim(samp.A)
head(samp.A)
table(samp.A$c.neti)   # Repartition of c.neti in the sample

## ----eval=TRUE----------------------------------------------------------------
c.neti            = as.numeric(samp.A$c.neti)
samp.A$c.neti.bis = as.factor(ifelse(c.neti %in% c(1,2),1, 
                              ifelse(c.neti %in% c(3,4),2, 
                              ifelse(c.neti %in% c(5,6),3,4)))) 
data1 = samp.A[1:200,c(2:3,5,7:9,12:13)]
colnames(data1)[4] = "age" 
head(data1)
data2 = samp.A[201:350,c(3,5:6,8:11,13:14)]
head(data2)

## ----eval=TRUE----------------------------------------------------------------
table(data1$c.neti)        # 7 levels in data1

table(data2$c.neti.bis)    # 4 levels in data2

colnames(data1)

colnames(data2)

intersect(colnames(data1), colnames(data2))   # the susbet of a priori shared variables

## ----eval=TRUE----------------------------------------------------------------
db_test  = merge_dbs(data1, data2, NAME_Y = "c.neti", NAME_Z = "c.neti.bis",
                     ordinal_DB1 = c(2,3,4,7), ordinal_DB2 = c(1:2,6,9))

summary(db_test)

db_test$REMAINING_VAR

db_test$REMOVE1

db_test$REMOVE2

db_test$ID1_drop; db_test$ID2_drop

db_test$DB_READY[c(1:5,201:205),]   # The 5 1st subjects of the two databases 

## ---- eval=TRUE---------------------------------------------------------------
# for data1
test_DB1 = select_pred(db_test$DB_READY,Y = "Y", Z = "Z", ID = 1, OUT = "Y",
                       quanti = 8, nominal = c(1,5:6,7), ordinal = c(2:4),
                       convert_num = 8, convert_clss = 4,
                       thresh_cat = 0.30, thresh_num = 0.70, thresh_Y = 0.10,
                       RF = TRUE, RF_SEED = 3017)

# for data2
test_DB2 = select_pred(db_test$DB_READY,Y = "Y", Z = "Z", ID = 1, OUT = "Z",
                       quanti = 8, nominal = c(1,5:6,7), ordinal = c(2:4),
                       convert_num = 8, convert_clss = 4,
                       thresh_cat = 0.30, thresh_num = 0.70, thresh_Y = 0.10,
                       RF = TRUE, RF_SEED = 3017)

## ----eval=TRUE----------------------------------------------------------------
summary(test_DB1)

test_DB1$vcrm_OUTC_cat

test_DB1$collinear_PB

# Results from RF
test_DB1$drop_var

test_DB1$RF_PRED

## -----------------------------------------------------------------------------
summary(test_DB2)

test_DB2$vcrm_OUTC_cat

test_DB2$collinear_PB

# Results from RF
test_DB2$drop_var

test_DB2$RF_PRED

## -----------------------------------------------------------------------------
match_var = db_test$DB_READY[,-c(5,8)]
match_var[c(1:5,201:205),]

## -----------------------------------------------------------------------------
# sequential algorithm
mod1_seq = OT_outcome(match_var, nominal = c(1,5:6), ordinal = 2:4, dist.choice = "E",
                      maxrelax = 0 , indiv.method = "sequential", which.DB = "A")
summary(mod1_seq)

# optimal algorithm with no relaxation term
mod2_opt = OT_outcome(match_var, nominal = c(1,5:6), ordinal = 2:4, dist.choice = "E",
                      maxrelax = 0, indiv.method = "optimal", which.DB = "A")
head(mod2_opt$profile)

dim(mod2_opt$profile)

mod2_opt$gamma_A

head(mod2_opt$DATA1_OT)


## -----------------------------------------------------------------------------
# Algorithms with no enrichments
mod3_joint = OT_joint(match_var, nominal = c(1,5), ordinal = c(2:4,6), dist.choice = "E", which.DB = "A")
summary(mod3_joint)

## -----------------------------------------------------------------------------
# Validation of the mod1_seq model
verif_out1 = verif_OT(mod1_seq, stab.prob = TRUE, min.neigb = 3)
verif_out1$conf.mat

verif_out1$res.prox

verif_out1$res.stab


# Validation of the mod2_seq model
verif_out2 = verif_OT(mod2_opt, stab.prob = TRUE, min.neigb = 3)
verif_out2$conf.mat
rate_good_pred = (37+40+31+45+18+13+9)/200
rate_good_pred

verif_out2$res.prox

verif_out2$res.stab


# Validation of the mod3_opt model
verif_jt   = verif_OT(mod3_joint, stab.prob = TRUE, min.neigb = 3)
verif_jt$conf.mat

verif_jt$res.prox

verif_jt$res.stab

