# install npdr, npdro if not installed
#install.packages("devtools")
library(devtools)
#install_github("insilico/npdr")  
#install_github("insilico/npdro")  
library(npdr)
library(npdro)
library(dplyr)
library(ranger)
library(caret)
library(doParallel)
library(reshape2)
library(ggplot2)
library(vioplot)
# setwd to dir that this file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# load functions
source("local_npdr_functions.R")
###########################
#set up parallel processing 
#registerDoParallel(cores=40)
###########################


############################
# 1. simulated data 
###########################
## this is how you simulate new data
# num.samples <- 300 #500, sim85: changed main.bias to 0.8
# num.variables <- 100
# # use npdro to simulate data with similar dimensions
# datasets <- npdro::createSimulation2(num.samples=num.samples,
#                                      num.variables=num.variables,
#                                      pct.imbalance=0.6, 
#                                      pct.signals=0.2,
#                                      main.bias=1.5, 
#                                      interaction.bias=1.5, 
#                                      hi.cor=0.95,
#                                      lo.cor=0.2,
#                                      mix.type="main-interactionScalefree",
#                                      label="class",
#                                      sim.type="mixed",
#                                      graph.structure=NULL,
#                                      pct.mixed=0.5, 
#                                      pct.train=0.8,
#                                      pct.holdout=0.2,
#                                      pct.validation=0,
#                                      plot.graph=T,
#                                      verbose=T)
# 
# sim_train <- datasets$train  
# dim(sim_train) #[1] 240 101
# sim_test <- datasets$holdout 
# dim(sim_test) #[1]  60 101
# datasets$signal.names # known variables with effects
# # [1] "mainvar1"  "mainvar2"  "mainvar3"  "mainvar4"  "mainvar5"  "mainvar6"  "mainvar7" 
# # [8] "mainvar8"  "mainvar9"  "mainvar10" "intvar1"   "intvar2"   "intvar3"   "intvar4"  
# # [15] "intvar5"   "intvar6"   "intvar7"   "intvar8"   "intvar9"   "intvar10"
# 
# # change class to 0/1 for glmnet local npdr analysis
# sim_train01 <- class01(sim_train, which(colnames(sim_train)=="class"))
# sim_test01 <- class01(sim_test, which(colnames(sim_test)=="class"))
# head(sim_test01)
# 
# # write simulated data to file
# write.table(sim_train01,"./ssNPDR/sim_train_fullVar_rfacc.79_imbal.6_effects1.5.csv",row.names=F,quote=F,sep=",")
# write.table(sim_test01,"./ssNPDR/sim_test_fullVar_rfacc.79_imbal.6_effects1.5.csv",row.names=F,quote=F,sep=",")

# read in two simulated datasets created using above code
sim77_train <- read.csv("./data/sim77/sim_train_fullVar_rfacc.77_imbal.6_effects1.5.csv")
sim77_test <- read.csv("./data/sim77/sim_test_fullVar_rfacc.77_imbal.6_effects1.5.csv")
table(sim77_train$class)
# 0   1 
# 96 144 
table(sim77_test$class)
#  0  1 
# 24 36
sim79_train <- read.csv("./data/sim79/sim_train_fullVar_rfacc.79_imbal.6_effects1.5.csv")
sim79_test <- read.csv("./data/sim79/sim_test_fullVar_rfacc.79_imbal.6_effects1.5.csv")
table(sim79_train$class)
# 0   1 
# 96 144 
table(sim79_test$class)
#  0  1 
# 24 36

sim85_train <- read.csv("./data/sim85/sim_train_fullvar_rfacc.85_bal.5_maineff.8_inteff1.5.csv")
#dim(sim85_train)
# [1] 400 101
sim85_test <- read.csv("./data/sim85/sim_test_fullvar_rfacc.85_bal.5_maineff.8_inteff1.5.csv")

table(sim85_train$class)
#   0   1 
# 200 200 
table(sim85_test$class)
#   0  1 
#  50 50 

##########################################
# 2. sim data: ranger RF global importance
#############################################
# set up parallel calculation to tune RF model

# 
# sim_train <- sim_train01
# sim_test <- sim_test01

# tune the model
# tune_sim_full <- tuneRFclass(train.dat=sim_train, test.dat=sim_test, rm.ind=101, 
#                              outcome.ind=101, cv.folds=5, tuneTrees=T, 
#                              maxTrees=10000, parallel=T, nthreads=4)
### takes several hours; tuning performed on astro-srvr and result read in here
# tuning took 25 minutes on the astro-srvr
sim77_tuned <- readRDS("./data/sim77/tune_sim77_fullVar.rds")
sim79_tuned <- readRDS("./data/sim79/tune_sim79_fullVar.rds")
sim85_tuned <- readRDS("./data/sim85/tune_sim85_fullVar.rds")

# sim77
sim77_tuned[[1]] # tune.df
#    mtry  splitrule min.node.size Accuracy ntrees
#294   26 extratrees             6   0.8125   3000

sim77_fullRF_tune_imp <- data.frame(rf_score=sim77_tuned[[2]]$variable.importance) 
sim77_fullRF_tune_sort <- dplyr::arrange(sim77_fullRF_tune_imp,-rf_score)
sim77_fullRF_tune_sort$variable <- rownames(sim77_fullRF_tune_sort)
sim77_fullRF_tune_sort[1:20,]
#           rf_score variable
# mainvar9 52.511112 mainvar9
# mainvar4 35.384783 mainvar4
# mainvar1 31.210569 mainvar1
# mainvar8 26.894914 mainvar8
# mainvar3 18.259153 mainvar3
# mainvar2 14.826026 mainvar2
# mainvar6 14.707222 mainvar6
# var1      6.492836     var1
# mainvar5  6.442226 mainvar5
# intvar1   6.352060  intvar1
# var35     5.841595    var35
# var58     4.919025    var58
# intvar6   4.622707  intvar6
# var29     4.114056    var29
# intvar3   3.643230  intvar3
# var21     3.494116    var21
# intvar5   3.442016  intvar5
# var45     3.310723    var45
# intvar8   3.258455  intvar8
# var8      3.023448     var8

# save to file
#write.table(sim77_fullRF_tune_sort,"./sim77/sim77_rfImp_tune_fullVar.csv",row.names=F,quote=F,sep=",")

sim77_tuned[[2]]$confusion.matrix
#    predicted
# true   0   1
#    0  91   5
#    1  43 101
sim77_tuned[[3]] # test CM
#    0  1
# 0 22  2
# 1 13 23
sim77_tuned[[4]] # test Acc
# [1] 0.75
head(sim77_tuned[[5]]) # training data with final predictions
head(sim77_tuned[[6]]) # test data with final predictions

# write to file
write.table(sim77_tuned[[1]],"./sim77/sim77_tune_fullVar.csv",row.names=F,quote=F,sep=",")
saveRDS(sim77_tuned[[2]], "./sim77/sim77_tunedRF_fullVar.rds")
write.table(sim77_tuned[[5]],"./sim77/sim77_trainPred_fullVar.csv",row.names=F,quote=F,sep=",")
write.table(sim77_tuned[[6]],"./sim77/sim77_testPred_fullVar.csv",row.names=F,quote=F,sep=",")


## tuned results form 79% accuracy simulated data
sim79_tuned[[1]] # tune.df
#     mtry  splitrule min.node.size  Accuracy ntrees
# 549   47 extratrees             9 0.8333333   6000

sim79_fullRF_tune_imp <- data.frame(rf_score=sim79_tuned[[2]]$variable.importance) 
sim79_fullRF_tune_sort <- dplyr::arrange(sim79_fullRF_tune_imp,-rf_score)
sim79_fullRF_tune_sort$variable <- rownames(sim79_fullRF_tune_sort)
sim79_fullRF_tune_sort[1:20,]
#            rf_score  variable
# mainvar8  119.238672  mainvar8
# mainvar1   44.997780  mainvar1
# mainvar7   42.075239  mainvar7
# var45      14.507803     var45
# intvar3    12.051584   intvar3
# intvar7    10.048590   intvar7
# mainvar9    9.500462  mainvar9
# mainvar10   9.465122 mainvar10
# var6        9.390178      var6
# var79       9.162502     var79
# intvar1     8.873944   intvar1
# var80       8.602850     var80
# var71       8.323203     var71
# var77       8.147791     var77
# mainvar6    8.095342  mainvar6
# var13       7.749374     var13
# var64       6.696071     var64
# var42       6.448148     var42
# var75       6.051235     var75
# intvar8     5.495408   intvar8

# save to file
#write.table(sim79_fullRF_tune_sort,"./sim79/sim79_rfImp_tune_fullVar.csv",row.names=F,quote=F,sep=",")

sim79_tuned[[2]]$confusion.matrix
#    predicted
# true   0   1
#    0  92   4
#    1  34 110
sim79_tuned[[3]] # test CM
#     0  1
#  0 23  1
#  1 12 24
sim79_tuned[[4]] # test Acc
# [1] 0.7833333
head(sim79_tuned[[5]]) # training data with final predictions
head(sim79_tuned[[6]]) # test data with final predictions

# write to file
write.table(sim79_tuned[[1]],"./sim79/sim79_tune_fullVar.csv",row.names=F,quote=F,sep=",")
saveRDS(sim79_tuned[[2]], "./sim79/sim79_tunedRF_fullVar.rds")
write.table(sim79_tuned[[5]],"./sim79/sim79_trainPred_fullVar.csv",row.names=F,quote=F,sep=",")
write.table(sim79_tuned[[6]],"./sim79/sim79_testPred_fullVar.csv",row.names=F,quote=F,sep=",")


## tuning results form 85% accuracy simulated data
sim85_tuned[[1]] # tune.df
#     mtry splitrule min.node.size Accuracy ntrees
#  82    5      gini             2   0.8475   4000

sim85_fullRF_tune_imp <- data.frame(rf_score=sim85_tuned[[2]]$variable.importance) 
sim85_fullRF_tune_sort <- dplyr::arrange(sim85_fullRF_tune_imp,-rf_score)
sim85_fullRF_tune_sort$variable <- rownames(sim85_fullRF_tune_sort)
sim85_fullRF_tune_sort[1:20,]
#            rf_score  variable
# mainvar5  38.824318  mainvar5
# mainvar9  38.701959  mainvar9
# mainvar4  30.886550  mainvar4
# mainvar1  28.922372  mainvar1
# mainvar7  28.354641  mainvar7
# mainvar3  23.882235  mainvar3
# mainvar10 18.932184 mainvar10
# mainvar8  11.253756  mainvar8
# var40      6.391787     var40
# intvar4    6.373530   intvar4
# mainvar6   6.233008  mainvar6
# var37      6.203901     var37
# intvar9    5.703492   intvar9
# var47      5.668778     var47
# var33      5.173478     var33
# var12      5.168908     var12
# var51      5.054717     var51
# intvar6    4.556424   intvar6
# mainvar2   4.538655  mainvar2
# var74      3.987857     var74    

# save to file
#write.table(sim85_fullRF_tune_sort,"./sim85/sim85_rfImp_tune_fullVar.csv",row.names=F,quote=F,sep=",")

sim85_tuned[[2]]$confusion.matrix
#  predicted
# true   0   1
#    0 174  26
#    1  33 167
sim85_tuned[[3]] # test CM
#    0  1
# 0 43  7
# 1 13 37
sim85_tuned[[4]] # test Acc
# [1] 0.8
head(sim85_tuned[[5]]) # training data with final predictions
head(sim85_tuned[[6]]) # test data with final predictions

# write to file
#write.table(sim85_tuned[[1]],"./sim85/sim85_tune_fullVar.csv",row.names=F,quote=F,sep=",")
#saveRDS(sim85_tuned[[2]], "./sim85/sim85_tunedRF_fullVar.rds")
# test CM
# test Acc - combine?
#write.table(sim85_tuned[[5]],"./sim85/sim85_trainPred_fullVar.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_tuned[[6]],"./sim85/sim85_testPred_fullVar.csv",row.names=F,quote=F,sep=",")

##### read in fullVar data with predictions
sim77_trainPred_fullVar <- read.csv("./data/sim77/sim77_trainPred_fullVar.csv")
sim77_testPred_fullVar <- read.csv("./data/sim77/sim77_testPred_fullVar.csv")

sim79_trainPred_fullVar <- read.csv("./data/sim79/sim79_trainPred_fullVar.csv")
sim79_testPred_fullVar <- read.csv("./data/sim79/sim79_testPred_fullVar.csv")

sim85_trainPred_fullVar <- read.csv("./data/sim85/sim85_trainPred_fullVar.csv")
sim85_testPred_fullVar <- read.csv("./data/sim85/sim85_trainPred_fullVar.csv")


#######################################################################
# 3. URFP distance matrices for NPDR-LURF global and local importance
####################################################################

# create the URFP distance matrices
# combine train and test and find the full distance matrix - for local NPDR
sim77_all_dat <- rbind.data.frame(sim77_train,sim77_test)
dim(sim77_all_dat)
# [1] 300 101

sim79_all_dat <- rbind.data.frame(sim79_train,sim79_test)
dim(sim79_all_dat)
# [1] 300 101

sim85_all_dat <- rbind.data.frame(sim85_train,sim85_test)
dim(sim85_all_dat)
# [1] 500 101

# training data dist mats
sim77_train_urfp_dist <- urfp_dist(sim77_train,rm.ind=101,nthreads=4)
dim(sim77_train_urfp_dist)
# [1] 240 240

sim79_train_urfp_dist <- urfp_dist(sim79_train,rm.ind=101,nthreads=4)
dim(sim79_train_urfp_dist)
# [1] 240 240

# full dataset dist mats
sim77_all_urfp_dist <- urfp_dist(sim77_all_dat,rm.ind=101,nthreads=4)
dim(sim77_all_urfp_dist)
# [1] 300 300

sim79_all_urfp_dist <- urfp_dist(sim79_all_dat,rm.ind=101,nthreads=4)
dim(sim79_all_urfp_dist)
# [1] 300 300

sim85_train_urfp_dist <- urfp_dist(sim85_train,rm.ind=101,nthreads=4)
dim(sim85_train_urfp_dist)
# [1] 400 400

sim85_all_urfp_dist <- urfp_dist(sim85_all_dat,rm.ind=101,nthreads=4)
dim(sim85_all_urfp_dist)
# [1] 500 500

# save distance matrices to file
#write.table(sim77_train_urfp_dist,"sim77_train_urfp_dist.csv",row.names=F,quote=F,sep=",")
#write.table(sim77_all_urfp_dist,"sim77_allSamples_urfp_dist.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_train_urfp_dist,"sim79_train_urfp_dist.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_all_urfp_dist,"sim79_allSamples_urfp_dist.csv",row.names=F,quote=F,sep=",")

#write.table(sim85_train_urfp_dist,"sim85_train_urfp_dist.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_all_urfp_dist,"sim85_allSamples_urfp_dist.csv",row.names=F,quote=F,sep=",")

### read in
sim77_train_urfp_dist <- read.csv("./data/sim77/sim77_train_urfp_dist.csv")
sim77_allSamples_urfp_dist  <- read.csv("./data/sim77/sim77_allSamples_urfp_dist.csv")

sim79_train_urfp_dist  <- read.csv("./data/sim79/sim79_train_urfp_dist.csv")
sim79_allSamples_urfp_dist  <- read.csv("./data/sim79/sim79_allSamples_urfp_dist.csv")

sim85_train_urfp_dist <- read.csv("./data/sim85/sim85_train_urfp_dist.csv")
sim85_allSamples_urfp_dist <- read.csv("./data/sim85/sim85_allSamples_urfp_dist.csv")

#######################################################
# 4. global NPDR-LURF: sim data + new distance matrix
######################################################

sim77_global_lurf <- npdr::npdr("class", sim77_train,
                      regression.type="binomial",
                      attr.diff.type="numeric-abs",
                      nbd.method="relieff",
                      nbd.metric = "precomputed",
                      external.dist= sim77_train_urfp_dist,
                      knn=knnSURF.balanced(sim77_train$class, 
                                           sd.frac = .5),
                      use.glmnet = T, glmnet.alpha = 1, 
                      glmnet.lower = 0, 
                      glmnet.lam=0.02,
                      #glmnet.lam="lambda.min", #
                      #glmnet.lam="lambda.1se",
                      neighbor.sampling="none", dopar.nn = T, dopar.reg=T,
                      verbose=T)
# k=58
#lambda.min:  0.0002202201 
# lambda.1se:  0.002979683  * lams too small, using lam - 0.02
sim77_global_lurf$variable <- rownames(sim77_global_lurf)
sim77_global_lurf <- sim77_global_lurf[rownames(sim77_global_lurf)!="intercept",]
sim77_global_lurf <- sim77_global_lurf[abs(sim77_global_lurf$scores)>0,]
sim77_global_lurf
#             scores  variable
# mainvar9 0.30588697 mainvar9
# mainvar4 0.08253520 mainvar4
# mainvar1 0.06874237 mainvar1
# intvar8  0.05694785  intvar8
# intvar3  0.03505172  intvar3
# mainvar8 0.03311265 mainvar8
# mainvar2 0.01989410 mainvar2
# var64    0.01344564    var64
# var35    0.00952210    var35

# write these scores to file
#write.table(sim77_global_lurf,"sim77_global_lurf_lam_0_02.csv",row.names=F,quote=F,sep=",")

## other simulated dataset - sim79
sim79_global_lurf <- npdr::npdr("class", sim79_train,
                                regression.type="binomial",
                                attr.diff.type="numeric-abs",
                                nbd.method="relieff",
                                nbd.metric = "precomputed",
                                external.dist= sim79_train_urfp_dist,
                                knn=knnSURF.balanced(sim79_train$class, 
                                                     sd.frac = .5),
                                use.glmnet = T, glmnet.alpha = 1, 
                                glmnet.lower = 0, 
                                #glmnet.lam=0.02,
                                #glmnet.lam="lambda.min", #
                                glmnet.lam="lambda.1se",
                                neighbor.sampling="none", dopar.nn = T, dopar.reg=T,
                                verbose=T)
# k=58
# lambda.min:  0.008078372 
# lambda.1se:  0.01286305* 
sim79_global_lurf$variable <- rownames(sim79_global_lurf)
sim79_global_lurf <- sim79_global_lurf[rownames(sim79_global_lurf)!="intercept",]
sim79_global_lurf <- sim79_global_lurf[abs(sim79_global_lurf$scores)>0,]
sim79_global_lurf
#             scores  variable
# mainvar8 0.326657544 mainvar8
# intvar8  0.084071506  intvar8
# mainvar9 0.048994996 mainvar9
# mainvar7 0.012942465 mainvar7
# var14    0.011650062    var14
# var64    0.008440765    var64
# intvar7  0.008108018  intvar7
# var67    0.005857460    var67

# write these scores to file
#write.table(sim79_global_lurf,"sim79_global_lurf_lam_1se.csv",row.names=F,quote=F,sep=",")


## sim85 global NPDR-LURF
sim85_global_lurf <- npdr::npdr("class", sim85_train,
                                regression.type="binomial",
                                attr.diff.type="numeric-abs",
                                nbd.method="relieff",
                                nbd.metric = "precomputed",
                                external.dist= sim85_train_urfp_dist,
                                knn=knnSURF.balanced(sim85_train$class, 
                                                     sd.frac = .5),
                                use.glmnet = T, glmnet.alpha = 1, 
                                glmnet.lower = 0, 
                                glmnet.lam=0.01,
                                #glmnet.lam="lambda.min", #
                                #glmnet.lam="lambda.1se",
                                neighbor.sampling="none", dopar.nn = T, dopar.reg=T,
                                verbose=T)
# lam = 0.01
# k=122
sim85_global_lurf$variable <- rownames(sim85_global_lurf)
sim85_global_lurf <- sim85_global_lurf[rownames(sim85_global_lurf)!="intercept",]
sim85_global_lurf <- sim85_global_lurf[abs(sim85_global_lurf$scores)>0,]
sim85_global_lurf
#               scores  variable
# mainvar5  0.241626161  mainvar5
# mainvar9  0.241115472  mainvar9
# mainvar1  0.159325765  mainvar1
# mainvar4  0.138147076  mainvar4
# mainvar7  0.123955944  mainvar7
# mainvar10 0.094926907 mainvar10
# mainvar3  0.077601348  mainvar3
# mainvar8  0.026020463  mainvar8
# intvar4   0.013121732   intvar4
# mainvar6  0.004026367  mainvar6 

# write these scores to file
#write.table(sim85_global_lurf,"sim85_global_lurf_lam_0_01.csv",row.names=F,quote=F,sep=",")

### read scores in
sim77_global_lurf <- read.csv("./data/sim77/sim77_global_lurf_lam_0_02.csv")
sim79_global_lurf <- read.csv("./data/sim79/sim79_global_lurf_lam_1se.csv")
sim85_global_lurf <- read.csv("./data/sim85/sim85_global_lurf_lam_0_01.csv")


#######################################
# 5. sim data: tune NPDR-LURF RF model
#######################################

# extract NPDR-LURF selected features and combine train, test samples
sim77_lurf_feat <- sim77_global_lurf$variable
sim77_lurf_feat_idx <- which(colnames(sim77_train) %in% c(sim77_lurf_feat,"class"))
sim77_lurf_train <- sim77_train[,sim77_lurf_feat_idx]
colnames(sim77_lurf_train)
# [1] "intvar3"  "var35"    "var64"    "intvar8"  "mainvar1" "mainvar2" "mainvar4" "mainvar8" "mainvar9"
# [10] "class"  
# need to change class back to factor after data read-in
sim77_lurf_train$class <- as.factor(sim77_lurf_train$class)
# same for test
sim77_lurf_test <- sim77_test[,sim77_lurf_feat_idx]
sim77_lurf_test$class <- as.factor(sim77_lurf_test$class)

# combine for local npdr later
sim77_lurf_all <- rbind.data.frame(sim77_lurf_train, sim77_lurf_test)
sim77_lurf_all$class <- as.factor(sim77_lurf_all$class)
dim(sim77_lurf_all)
# [1] 300  10

# other simulated dataset-sim79
sim79_lurf_feat <- sim79_global_lurf$variable
sim79_lurf_feat_idx <- which(colnames(sim79_train) %in% c(sim79_lurf_feat,"class"))
sim79_lurf_train <- sim79_train[,sim79_lurf_feat_idx]
colnames(sim79_lurf_train)
# [1] "intvar8"  "var14"    "intvar7"  "var64"    "var67"    "mainvar7" "mainvar8" "mainvar9" "class"  
dim(sim79_lurf_train)
# [1] 240  9
# change class col to factor
sim79_lurf_train$class <- as.factor(sim79_lurf_train$class)

sim79_lurf_test <- sim79_test[,sim79_lurf_feat_idx]
sim79_lurf_test$class <- as.factor (sim79_lurf_test$class)

# combine for local npdr later
sim79_lurf_all <- rbind.data.frame(sim79_lurf_train, sim79_lurf_test)
sim79_lurf_all$class <- as.factor(sim79_lurf_all$class)

# sim85
sim85_lurf_feat <- sim85_global_lurf$variable
sim85_lurf_feat_idx <- which(colnames(sim85_train) %in% c(sim85_lurf_feat,"class"))
sim85_lurf_train <- sim85_train[,sim85_lurf_feat_idx]
colnames(sim85_lurf_train)
# [1] "intvar4"   "mainvar1"  "mainvar3"  "mainvar4"  "mainvar5"  "mainvar6"  "mainvar7"  "mainvar8" 
# [9] "mainvar9"  "mainvar10" "class"    
dim(sim85_lurf_train)
# [1] 400  11
# change class col to factor
sim85_lurf_train$class <- as.factor(sim85_lurf_train$class)

sim85_lurf_test <- sim85_test[,sim85_lurf_feat_idx]
sim85_lurf_test$class <- as.factor(sim85_lurf_test$class)

# combine for local npdr later
sim85_lurf_all <- rbind.data.frame(sim85_lurf_train, sim85_lurf_test)
sim85_lurf_all$class <- as.factor(sim85_lurf_all$class)


### tune the models
class77_ind <- which(colnames(sim77_lurf_train)=="class")
class79_ind <- which(colnames(sim79_lurf_train)=="class")
class85_ind <- which(colnames(sim85_lurf_train)=="class")

registerDoParallel(cores=4)
tune77_sim_lurf <- tuneRFclass(train.dat=sim77_lurf_train, 
                             test.dat=sim77_lurf_test, 
                             rm.ind=class77_ind, # the only col to rm is class 
                             outcome.ind=class77_ind, cv.folds=5, tuneTrees=T, 
                             maxTrees=10000, parallel=T, nthreads=4)
# Time difference of 3.711004 mins
tune77_sim_lurf[[1]] # tune.df
#    mtry splitrule min.node.size Accuracy ntrees
# 60    5      gini            12    0.775   5000
tune77_sim_lurf[[2]] # tuned RF model
# Type:                             Classification 
# Number of trees:                  5000 
# Sample size:                      240 
# Number of independent variables:  9 
# Mtry:                             5 
# Target node size:                 12 
# Variable importance mode:         permutation 
# Splitrule:                        2 
# OOB prediction error:             22.08 %
sim77_lurfRF_tune_imp <- data.frame(rf_score=tune77_sim_lurf[[2]]$variable.importance) 
sim77_lurfRF_tune_sort <- dplyr::arrange(sim77_lurfRF_tune_imp,-rf_score)
sim77_lurfRF_tune_sort$variable <- rownames(sim77_lurfRF_tune_sort)
sim77_lurfRF_tune_sort
#            rf_score variable
# mainvar9 100.809081 mainvar9
# mainvar1  46.378084 mainvar1
# mainvar4  41.208939 mainvar4
# mainvar8  24.756412 mainvar8
# var35     22.933987    var35
# mainvar2  16.256758 mainvar2
# intvar3    9.831754  intvar3
# var64      6.520918    var64
# intvar8   -2.621373  intvar8

# save to file
#write.table(sim77_lurfRF_tune_sort,"sim77_lurfRFimp_tune.csv",row.names=F,quote=F,sep=",")
# train acc = 77.92%
tune77_sim_lurf[[2]]$confusion.matrix
#     predicted
# true   0   1
#    0  78  18
#    1  35 109
tune77_sim_lurf[[3]] # test CM - 
#    0  1
# 0 20  4
# 1 13 23 
tune77_sim_lurf[[4]] # test acc
# [1] 0.7166667
head(tune77_sim_lurf[[5]]) # training data with final predictions
head(tune77_sim_lurf[[6]]) # test data with final predictions

# write to file
#write.table(tune77_sim_lurf[[1]],"sim77_tune_lurfRF.csv",row.names=F,quote=F,sep=",")
#saveRDS(tune77_sim_lurf[[2]], "sim77_lurfRF_tuned.rds")
#write.table(tune77_sim_lurf[[5]],"sim77_trainPred_lurfRF.csv",row.names=F,quote=F,sep=",")
#write.table(tune77_sim_lurf[[6]],"sim77_testPred_lurfRF.csv",row.names=F,quote=F,sep=",")

# other simulated dataset-sim79
tune79_sim_lurf <- tuneRFclass(train.dat=sim79_lurf_train, 
                               test.dat=sim79_lurf_test, 
                               rm.ind=class79_ind, # the only col to rm is class 
                               outcome.ind=class79_ind, cv.folds=5, tuneTrees=T, 
                               maxTrees=10000, parallel=T, nthreads=4)
# Time difference of 3.758346 mins
tune79_sim_lurf[[1]] # tune.df
#  mtry  splitrule min.node.size  Accuracy ntrees
#75    8 extratrees             3 0.8291667   6000
tune79_sim_lurf[[2]] # tuned RF model
# Type:                             Classification 
# Number of trees:                  6000 
# Sample size:                      240 
# Number of independent variables:  8 
# Mtry:                             8 
# Target node size:                 3 
# Variable importance mode:         permutation 
# Splitrule:                        1 
# Number of random splits:          1 
# OOB prediction error:             17.08 % 
sim79_lurfRF_tune_imp <- data.frame(rf_score=tune79_sim_lurf[[2]]$variable.importance) 
sim79_lurfRF_tune_sort <- dplyr::arrange(sim79_lurfRF_tune_imp,-rf_score)
sim79_lurfRF_tune_sort$variable <- rownames(sim79_lurfRF_tune_sort)
sim79_lurfRF_tune_sort
#           rf_score variable
# mainvar8 196.115232 mainvar8
# intvar7   44.008754  intvar7
# var67     43.762701    var67
# mainvar7  28.098648 mainvar7
# var64      8.400611    var64
# mainvar9   3.441670 mainvar9
# var14     -3.853706    var14
# intvar8   -6.483734  intvar8
# save to file
#write.table(sim79_lurfRF_tune_sort,"sim79_lurfRFimp_tune.csv",row.names=F,quote=F,sep=",")
table(sim79_train$class)
#  0   1 
# 96 144

# train acc = 
tune79_sim_lurf[[2]]$confusion.matrix
#     predicted
# true   0   1
#    0  94   2
#    1  39 105
tune79_sim_lurf[[3]]# test CM
#    0  1
# 0 23  1
# 1 12 24 
tune79_sim_lurf[[4]] # test acc
# [1] 0.7833333
head(tune79_sim_lurf[[5]]) # training data with final predictions
head(tune79_sim_lurf[[6]]) # test data with final predictions

# write to file
#write.table(tune79_sim_lurf[[1]],"sim79_tune_lurfRF.csv",row.names=F,quote=F,sep=",")
#saveRDS(tune79_sim_lurf[[2]], "sim79_lurfRF_tuned.rds")
#write.table(tune79_sim_lurf[[5]],"sim79_trainPred_lurfRF.csv",row.names=F,quote=F,sep=",")
#write.table(tune79_sim_lurf[[6]],"sim79_testPred_lurfRF.csv",row.names=F,quote=F,sep=",")


## sim85
tune85_sim_lurf <- tuneRFclass(train.dat=sim85_lurf_train, 
                               test.dat=sim85_lurf_test, 
                               rm.ind=class85_ind, # the only col to rm is class 
                               outcome.ind=class85_ind, cv.folds=5, tuneTrees=T, 
                               maxTrees=10000, parallel=T, nthreads=4)
# Time difference of 3.758346 mins - 300 samples
# Time difference of 14.01678 mins - 400 samples
tune85_sim_lurf[[1]] # tune.df
#   mtry  splitrule min.node.size Accuracy ntrees
# 7    2 extratrees             7   0.8425   6000
tune85_sim_lurf[[2]] # tuned RF model
# Type:                             Classification 
# Number of trees:                  6000 
# Sample size:                      400 
# Number of independent variables:  10 
# Mtry:                             2 
# Target node size:                 7 
# Variable importance mode:         permutation 
# Splitrule:                        1 
# Number of random splits:          1 
# OOB prediction error:             15.75 % 
sim85_lurfRF_tune_imp <- data.frame(rf_score=tune85_sim_lurf[[2]]$variable.importance) 
sim85_lurfRF_tune_sort <- dplyr::arrange(sim85_lurfRF_tune_imp,-rf_score)
sim85_lurfRF_tune_sort$variable <- rownames(sim85_lurfRF_tune_sort)
sim85_lurfRF_tune_sort
#           rf_score  variable
# mainvar5  89.844435  mainvar5
# mainvar9  77.500408  mainvar9
# mainvar1  62.061707  mainvar1
# mainvar4  56.716966  mainvar4
# mainvar7  52.653267  mainvar7
# mainvar3  48.376468  mainvar3
# mainvar10 35.425831 mainvar10
# mainvar8  26.959445  mainvar8
# mainvar6   5.198352  mainvar6
# intvar4    2.060468   intvar4  

# save to file
#write.table(sim85_lurfRF_tune_sort,"sim85_lurfRFimp_tune.csv",row.names=F,quote=F,sep=",")

# train acc = 84.3%
tune85_sim_lurf[[2]]$confusion.matrix
#    predicted
# true   0   1
#   0 169  31
#   1  32 168
tune85_sim_lurf[[3]] # test CM 
#    0  1
# 0 41  9
# 1 11 39
tune85_sim_lurf[[4]] # test Acc 
# 0.8
head(tune85_sim_lurf[[5]]) # training data with final predictions
head(tune85_sim_lurf[[6]]) # test data with final predictions

# write to file
#write.table(tune85_sim_lurf[[1]],"sim85_tune_lurfRF.csv",row.names=F,quote=F,sep=",")
#saveRDS(tune85_sim_lurf[[2]], "sim85_lurfRF_tuned.rds")
#write.table(tune85_sim_lurf[[5]],"sim85_trainPred_lurfRF.csv",row.names=F,quote=F,sep=",")
#write.table(tune85_sim_lurf[[6]],"sim85_testPred_lurfRF.csv",row.names=F,quote=F,sep=",")

# get NPDR-LURF train/test data with predictions from tuned model
# sim77_train_lurfRF_pred <- tune77_sim_lurf[[5]]
# sim77_test_lurfRF_pred <- tune77_sim_lurf[[6]]
# 
# sim79_train_lurfRF_pred <- tune79_sim_lurf[[5]]
# sim79_test_lurfRF_pred <- tune79_sim_lurf[[6]]

# new session: get from file
sim77_trainPred_lurfRF <- read.csv("./data/sim77/sim77_trainPred_lurfRF.csv")
sim77_testPred_lurfRF <- read.csv("./data/sim77/sim77_testPred_lurfRF.csv")
sim77_lurfRF <- readRDS("./data/sim77/sim77_lurfRF_tuned.rds") 

sim79_trainPred_lurfRF <- read.csv("./data/sim79/sim79_trainPred_lurfRF.csv")
sim79_testPred_lurfRF <- read.csv("./data/sim79/sim79_testPred_lurfRF.csv")
sim79_lurfRF <- readRDS("./data/sim79/sim79_lurfRF_tuned.rds")

sim85_trainPred_lurfRF <- read.csv("./data/sim85/sim85_trainPred_lurfRF.csv")
sim85_testPred_lurfRF <- read.csv("./data/sim85/sim85_testPred_lurfRF.csv")
sim85_lurfRF <- readRDS("./data/sim85/sim85_lurfRF_tuned.rds")

############################################################
# 6. sim data: RF probability forest for NPDR-LURF features
############################################################
# assess model confidence using probability forest
colnames(sim77_lurf_train)
#[1] "intvar3"  "var35"    "var64"    "intvar8"  "mainvar1" "mainvar2" "mainvar4" "mainvar8"
#[9] "mainvar9" "class" 
colnames(sim79_lurf_train)
# [1] "intvar8"  "var14"    "intvar7"  "var64"    "var67"    "mainvar7" "mainvar8" "mainvar9"
# [9] "class"  
colnames(sim85_lurf_train)
# [1] "intvar4"   "mainvar1"  "mainvar3"  "mainvar4"  "mainvar5"  "mainvar6"  "mainvar7"  "mainvar8" 
# [9] "mainvar9"  "mainvar10" "class"  

# probability forest for NPDR-LURF features
sim77_lurf_prob_rf <- ranger(class~., sim77_lurf_train, 
                           keep.inbag = TRUE,
                           num.trees = tune77_sim_lurf[[1]]$ntrees,
                           mtry = tune77_sim_lurf[[1]]$mtry, 
                           importance = "permutation", 
                           splitrule = tune77_sim_lurf[[1]]$splitrule,
                           min.node.size = tune77_sim_lurf[[1]]$min.node.size,
                           class.weights = as.numeric(c(1/table(sim77_lurf_train$class))),
                           scale.permutation.importance = T, 
                           probability = T,
                           local.importance = F, 
                           num.threads = 4)
sim77_lurf_prob_imp <- sim77_lurf_prob_rf$variable.importance
sort(sim77_lurf_prob_imp,decreasing = T)
# mainvar9   mainvar1   mainvar4      var35   mainvar8   mainvar2    intvar3      var64 
# 106.466417  44.820371  39.604801  23.052834  22.876649  14.877670  12.458731   7.984106 
# intvar8 
# -4.750172

sim77_prob_train_pred <- sim77_lurf_prob_rf$predictions
class(sim77_prob_train_pred) #matrix, convert to df
sim77_prob_train_pred_df <- as.data.frame(sim77_prob_train_pred)
head(sim77_prob_train_pred_df)
colnames(sim77_prob_train_pred_df) <- c("prob_0","prob_1")

sim77_lurf_prob_rf$prediction.error
# [1] 0.138903

# add to copy of training data
sim77_lurf_train_prob <- sim77_lurf_train
sim77_lurf_train_prob$prob_0 <- sim77_prob_train_pred_df$prob_0
sim77_lurf_train_prob$prob_1 <- sim77_prob_train_pred_df$prob_1
head(sim77_lurf_train_prob)
#     intvar3      var35      var64     intvar8   mainvar1   mainvar2   mainvar4   mainvar8
# 1 -0.3375445 -0.3263857  0.3789533  0.42934330  1.3569962  1.2749388  0.1203776  3.1130645
# 2  0.6408931 -1.3947580 -0.4174560 -0.01675236 -0.8779517  0.2719729 -0.7692282 -1.2891825
# 3 -1.7047642 -1.0142690 -0.4363001 -0.54736778 -0.8335569 -0.5584615  1.0412128 -1.0074721
# 4 -1.3438325 -1.1866532 -0.5933077 -0.30962939  0.4463967 -0.2005929  3.0355534 -1.6562930
# 5 -0.9807091  2.1324712 -0.9455821 -0.77551908 -1.0455826  0.9129539 -1.4924557  0.3652548
# 6  0.8018771 -1.8089660  0.2327144  0.55160231  1.1051407 -0.2037404 -0.2130715 -0.6816977
#      mainvar9 class    prob_0    prob_1
# 1 -0.13781246     1 0.8104465 0.1895535
# 2  0.48090626     1 0.4419990 0.5580010
# 3  0.44091190     1 0.3515389 0.6484611
# 4  0.24419503     1 0.4443039 0.5556961
# 5 -0.56052302     1 0.6286009 0.3713991
# 6  0.01643714     1 0.5153647 0.4846353

# add pred class back
sim77_lurf_prob_df <- cbind.data.frame(sim77_lurf_train_prob,
                                        sim77_train_lurfRF_pred$pred)
colnames(sim77_lurf_prob_df)[dim(sim77_lurf_prob_df)[2]] <- "pred"
head(sim77_lurf_prob_df)
#      intvar3      var35      var64     intvar8   mainvar1   mainvar2   mainvar4   mainvar8
# 1 -0.3375445 -0.3263857  0.3789533  0.42934330  1.3569962  1.2749388  0.1203776  3.1130645
# 2  0.6408931 -1.3947580 -0.4174560 -0.01675236 -0.8779517  0.2719729 -0.7692282 -1.2891825
# 3 -1.7047642 -1.0142690 -0.4363001 -0.54736778 -0.8335569 -0.5584615  1.0412128 -1.0074721
# 4 -1.3438325 -1.1866532 -0.5933077 -0.30962939  0.4463967 -0.2005929  3.0355534 -1.6562930
# 5 -0.9807091  2.1324712 -0.9455821 -0.77551908 -1.0455826  0.9129539 -1.4924557  0.3652548
# 6  0.8018771 -1.8089660  0.2327144  0.55160231  1.1051407 -0.2037404 -0.2130715 -0.6816977
#      mainvar9 class    prob_0    prob_1 pred
# 1 -0.13781246     1 0.8104465 0.1895535    0
# 2  0.48090626     1 0.4419990 0.5580010    1
# 3  0.44091190     1 0.3515389 0.6484611    1
# 4  0.24419503     1 0.4443039 0.5556961    1
# 5 -0.56052302     1 0.6286009 0.3713991    0
# 6  0.01643714     1 0.5153647 0.4846353    0

# get predicted probabilities for test data
colnames(sim77_lurf_test)
rm77_ind <- 10 
outcome77_ind <- 10 

sim77_lurf_predProb_test<-predict(sim77_lurf_prob_rf,data=sim77_test[,-rm77_ind])
head(sim77_lurf_predProb_test$predictions)
#              0         1
# [1,] 0.6561694 0.3438306
# [2,] 0.5472089 0.4527911
# [3,] 0.7615705 0.2384295
# [4,] 0.5582670 0.4417330
# [5,] 0.6850814 0.3149186
# [6,] 0.4305586 0.5694414
class(sim77_lurf_predProb_test$predictions) #matrix, convert to df
sim77_prob_test_pred_df <- as.data.frame(sim77_lurf_predProb_test$predictions)
colnames(sim77_prob_test_pred_df) <- c("prob_0","prob_1")
head(sim77_prob_test_pred_df)
#      prob_0    prob_1
# 1 0.6561694 0.3438306
# 2 0.5472089 0.4527911
# 3 0.7615705 0.2384295
# 4 0.5582670 0.4417330
# 5 0.6850814 0.3149186
# 6 0.4305586 0.5694414

# add to copy of test data
sim77_lurf_test_prob <- sim77_lurf_test
sim77_lurf_test_prob$prob_0 <- sim77_prob_test_pred_df$prob_0
sim77_lurf_test_prob$prob_1 <- sim77_prob_test_pred_df$prob_1
head(sim77_lurf_test_prob)
#      intvar3      var35       var64     intvar8   mainvar1   mainvar2    mainvar4   mainvar8
# 1  1.0272703 -1.2158357 -0.05972453 -0.75864064  1.0912238  1.5780790  1.61176689  0.1027399
# 2 -0.7639726 -1.3578192 -0.07546258  1.21220867  0.1924724 -0.6284312  1.06629179 -0.2235473
# 3 -0.4599568 -0.4687442 -0.25149547 -1.44828846 -0.4119003 -0.3893421 -1.04843019  0.8772137
# 4 -0.2234593  1.6640654 -0.16874654 -0.13080320  1.7612002 -0.7866653 -1.09838070  1.4147274
# 5  0.5797351 -0.9366242  0.30820732  1.07307738  0.9745559  0.2905377 -0.03079317  0.3240980
# 6  1.1071810 -0.8218041  0.46998019 -0.05555037 -0.2500002  0.9569221 -0.63341434 -1.2473973
#      mainvar9 class    prob_0    prob_1
# 1 -0.52979254     1 0.6561694 0.3438306
# 2 -0.31101584     1 0.5472089 0.4527911
# 3  0.04202181     1 0.7615705 0.2384295
# 4 -0.36757990     1 0.5582670 0.4417330
# 5  0.59975662     1 0.6850814 0.3149186
# 6 -0.97224973     1 0.4305586 0.5694414

# add pred class back
sim77_lurf_prob_test_df <- cbind.data.frame(sim77_lurf_test_prob,
                                     sim77_test_lurfRF_pred$pred)
colnames(sim77_lurf_prob_test_df)[dim(sim77_lurf_prob_test_df)[2]] <- "pred"
head(sim77_lurf_prob_test_df)
#      intvar3      var35       var64     intvar8   mainvar1   mainvar2    mainvar4   mainvar8
# 1  1.0272703 -1.2158357 -0.05972453 -0.75864064  1.0912238  1.5780790  1.61176689  0.1027399
# 2 -0.7639726 -1.3578192 -0.07546258  1.21220867  0.1924724 -0.6284312  1.06629179 -0.2235473
# 3 -0.4599568 -0.4687442 -0.25149547 -1.44828846 -0.4119003 -0.3893421 -1.04843019  0.8772137
# 4 -0.2234593  1.6640654 -0.16874654 -0.13080320  1.7612002 -0.7866653 -1.09838070  1.4147274
# 5  0.5797351 -0.9366242  0.30820732  1.07307738  0.9745559  0.2905377 -0.03079317  0.3240980
# 6  1.1071810 -0.8218041  0.46998019 -0.05555037 -0.2500002  0.9569221 -0.63341434 -1.2473973
#      mainvar9 class    prob_0    prob_1 pred
# 1 -0.52979254     1 0.6561694 0.3438306    0
# 2 -0.31101584     1 0.5472089 0.4527911    0
# 3  0.04202181     1 0.7615705 0.2384295    0
# 4 -0.36757990     1 0.5582670 0.4417330    0
# 5  0.59975662     1 0.6850814 0.3149186    0
# 6 -0.97224973     1 0.4305586 0.5694414    1

# save model and data with pred to file
#write.table(sim77_lurf_prob_df,"sim77_lurf_train_prob.csv",quote=F,row.names=F,sep=",")
#saveRDS(sim77_lurf_prob_rf,"sim77_lurf_probRF.rds")
#write.table(sim77_lurf_prob_test_df,"sim77_lurf_test_prob.csv",quote=F,row.names=F,sep=",")
sim77_lurf_prob_all <- rbind.data.frame(sim77_lurf_prob_df,sim77_lurf_prob_test_df)
#write.table(sim77_lurf_prob_all,"sim77_lurf_prob_allSamples.csv",quote=F,row.names=F,sep=",")


### do for other simulated dataset -  sim79
# probability forest for NPDR-LURF features
sim79_lurf_prob_rf <- ranger(class~., sim79_lurf_train, 
                             keep.inbag = TRUE,
                             num.trees = tune79_sim_lurf[[1]]$ntrees,
                             mtry = tune79_sim_lurf[[1]]$mtry, 
                             importance = "permutation", 
                             splitrule = tune79_sim_lurf[[1]]$splitrule,
                             min.node.size = tune79_sim_lurf[[1]]$min.node.size,
                             class.weights = as.numeric(c(1/table(sim79_lurf_train$class))),
                             scale.permutation.importance = T, 
                             probability = T,
                             local.importance = F, 
                             num.threads = 4)
sim79_lurf_prob_imp <- sim79_lurf_prob_rf$variable.importance
sort(sim79_lurf_prob_imp,decreasing = T)
#   mainvar8    intvar7      var67   mainvar7      var64   mainvar9      var14    intvar8 
# 196.336920  45.644636  44.930275  29.173325   9.451309   3.153345  -1.737355  -4.755002 

sim79_prob_train_pred <- sim79_lurf_prob_rf$predictions
class(sim79_prob_train_pred) #matrix, convert to df
sim79_prob_train_pred_df <- as.data.frame(sim79_prob_train_pred)
colnames(sim79_prob_train_pred_df) <- c("prob_0","prob_1")
head(sim79_prob_train_pred_df)
#      prob_0    prob_1
# 1 0.7164948 0.2835052
# 2 0.5761372 0.4238628
# 3 0.5217755 0.4782245
# 4 0.7922300 0.2077700
# 5 0.7452381 0.2547619
# 6 0.5038673 0.4961327
sim79_lurf_prob_rf$prediction.error
# [1] 0.12288

# add to copy of training data
sim79_lurf_train_prob <- sim79_lurf_train
sim79_lurf_train_prob$prob_0 <- sim79_prob_train_pred_df$prob_0
sim79_lurf_train_prob$prob_1 <- sim79_prob_train_pred_df$prob_1
head(sim79_lurf_train_prob)

# add pred class back
sim79_lurf_prob_df <- cbind.data.frame(sim79_lurf_train_prob,
                                       sim79_train_lurfRF_pred$pred)
colnames(sim79_lurf_prob_df)[dim(sim79_lurf_prob_df)[2]] <- "pred"
head(sim79_lurf_prob_df)

# get predicted probabilities for test data
colnames(sim79_lurf_test)
rm79_ind <- 9 
outcome79_ind <- 9 

sim79_lurf_predProb_test<-predict(sim79_lurf_prob_rf,data=sim79_test[,-rm79_ind])
head(sim79_lurf_predProb_test$predictions)

class(sim79_lurf_predProb_test$predictions) #matrix, convert to df
sim79_prob_test_pred_df <- as.data.frame(sim79_lurf_predProb_test$predictions)
colnames(sim79_prob_test_pred_df) <- c("prob_0","prob_1")
head(sim79_prob_test_pred_df)

# add to copy of test data
sim79_lurf_test_prob <- sim79_lurf_test
sim79_lurf_test_prob$prob_0 <- sim79_prob_test_pred_df$prob_0
sim79_lurf_test_prob$prob_1 <- sim79_prob_test_pred_df$prob_1
head(sim79_lurf_test_prob)

# add pred class back
sim79_lurf_prob_test_df <- cbind.data.frame(sim79_lurf_test_prob,
                                            sim79_test_lurfRF_pred$pred)
colnames(sim79_lurf_prob_test_df)[dim(sim79_lurf_prob_test_df)[2]] <- "pred"
head(sim79_lurf_prob_test_df)
#        intvar8       var14    intvar7      var64       var67   mainvar7   mainvar8    mainvar9
# 1  0.816731747 -0.73670526  0.9944979  0.1441196  0.22629213  0.5164339  1.9751240  0.38690516
# 2 -1.602771723  0.97809304  0.2904781  0.9209390 -0.11049816  1.1877225  0.7581714  0.20928900
# 3  0.001080858 -0.01536307 -0.6121599  1.5388981 -0.08762549 -0.8000804 -0.6237390 -0.09813807
# 4  0.886350544 -0.69914605 -0.5393866 -1.4984767 -0.43194158  0.6766454  1.2702301 -3.27436285
# 5  0.523253199 -1.64647128 -0.9336884 -1.1883063 -1.30715623  0.9978209  2.0382304  0.22834040
# 6 -0.235001993  0.34247853 -0.7225504 -0.5960336 -1.06733439  0.9197879  0.2673161 -0.06532411
#   class    prob_0    prob_1 pred
# 1     1 0.3856111 0.6143889    1
# 2     1 0.6500556 0.3499444    0
# 3     1 0.5873056 0.4126944    0
# 4     1 0.6947222 0.3052778    0
# 5     1 0.3392500 0.6607500    1
# 6     1 0.6943333 0.3056667    0

# save model and data with pred to file
#write.table(sim79_lurf_prob_df,"sim79_lurf_train_prob.csv",quote=F,row.names=F,sep=",")
#saveRDS(sim79_lurf_prob_rf,"sim79_lurf_probRF.rds")
#write.table(sim79_lurf_prob_test_df,"sim79_lurf_test_prob.csv",quote=F,row.names=F,sep=",")
sim79_lurf_prob_all <- rbind.data.frame(sim79_lurf_prob_df,sim79_lurf_prob_test_df)
#write.table(sim79_lurf_prob_all,"sim79_lurf_prob_allSamples.csv",quote=F,row.names=F,sep=",")


## sim85, use probability_rf function
which(colnames(sim85_lurf_train)=="class")
sim85_prob_rf <- probability_rf(train_dat=sim85_lurf_train, test_dat=sim85_lurf_test, 
                                class_idx=11, num_trees=tune85_sim_lurf[[1]]$ntrees, 
                                mtry=tune85_sim_lurf[[1]]$mtry, 
                                splitrule=tune85_sim_lurf[[1]]$splitrule,
                                min_node_size=tune85_sim_lurf[[1]]$min.node.size, nthreads=4)
names(sim85_prob_rf)
# [1] "prob_rf"    "var_imp"    "train_acc"  "train_pred" "test_pred" 

sim85_prob_rf$prob_rf
# Type:                             Probability estimation 
# Number of trees:                  6000 
# Sample size:                      400 
# Number of independent variables:  10 
# Mtry:                             2 
# Target node size:                 7 
# Variable importance mode:         permutation 
# Splitrule:                        1 
# Number of random splits:          1 
# OOB prediction error (Brier s.):  0.1308387 

sim85_prob_rf$var_imp
#   mainvar5   mainvar9   mainvar1   mainvar4   mainvar7   mainvar3  mainvar10   mainvar8   mainvar6 
# 95.2392449 81.2764076 68.3219235 61.2811540 56.0548105 50.7900333 37.9105948 28.4539878  7.8184230 
#    intvar4 
# -0.2787282

sim85_prob_rf$train_acc
# [1] 0.8691613

sim85_prob_lurf_train_df <- sim85_prob_rf$train_pred
sim85_prob_lurf_test_df <- sim85_prob_rf$test_pred

# add predictions back to this data 
sim85_prob_lurf_train_df$pred <- sim85_train_lurfRF_pred$pred
sim85_prob_lurf_test_df$pred <- sim85_test_lurfRF_pred$pred

# save model and data with pred to file
#write.table(sim85_prob_lurf_train_df,"sim85_lurf_train_prob.csv",quote=F,row.names=F,sep=",")
#write.table(sim85_prob_lurf_test_df,"sim85_lurf_test_prob.csv",quote=F,row.names=F,sep=",")
sim85_lurf_prob_all <- rbind.data.frame(sim85_prob_lurf_train_df,sim85_prob_lurf_test_df)
#write.table(sim85_lurf_prob_all,"sim85_lurf_prob_allSamples.csv",quote=F,row.names=F,sep=",")

# read in for new session: 
#sim77_lurf_prob_rf <- readRDS("sim77_lurf_probRF.rds")
sim77_lurfProb_allSamples <- read.csv("./data/sim77/sim77_lurf_prob_allSamples.csv")
colnames(sim77_lurfProb_allSamples)
# [1] "intvar3"  "var35"    "var64"    "intvar8"  "mainvar1" "mainvar2" "mainvar4" "mainvar8" "mainvar9"
# [10] "class"    "prob_0"   "prob_1"   "pred"   

sim79_lurfProb_allSamples <- read.csv("./data/sim79/sim79_lurf_prob_allSamples.csv")
colnames(sim79_lurfProb_allSamples)
# [1] "intvar8"  "var14"    "intvar7"  "var64"    "var67"    "mainvar7" "mainvar8" "mainvar9" "class"   
# [10] "prob_0"   "prob_1"   "pred" 

sim85_lurfProb_allSamples <- read.csv("./data/sim85/sim85_lurf_prob_allSamples.csv")
colnames(sim85_lurfProb_allSamples)
# [1] "intvar4"   "mainvar1"  "mainvar3"  "mainvar4"  "mainvar5"  "mainvar6"  "mainvar7" 
# [8] "mainvar8"  "mainvar9"  "mainvar10" "class"     "prob_0"    "prob_1"    "pred"  

#####################################################
# 7. sim data: local NPDR analysis on one sample
#####################################################

# need distance matrices for NPDR-LURF features - use all samples
colnames(sim77_lurf_all)
sim77_lurf_urfp_dist <- urfp_dist(sim77_lurf_all,rm.ind=10)
dim(sim77_lurf_urfp_dist)
# [1] 300 300

colnames(sim79_lurf_all)
sim79_lurf_urfp_dist <- urfp_dist(sim79_lurf_all,rm.ind=9)
dim(sim79_lurf_urfp_dist)
# [1] 300 300

colnames(sim85_lurf_all)
sim85_lurf_urfp_dist <- urfp_dist(sim85_lurf_all,rm.ind=11)
dim(sim85_lurf_urfp_dist)
# [1] 500 500

# save these new distance matrices to file
#write.table(sim77_lurf_urfp_dist,"sim77_lurf_urfp_dist_allSamples.csv",
#            row.names=F,quote=F,sep=",")
#write.table(sim79_lurf_urfp_dist,"sim79_lurf_urfp_dist_allSamples.csv",
#            row.names=F,quote=F,sep=",")
#write.table(sim85_lurf_urfp_dist,"sim85_lurf_urfp_dist_allSamples.csv",
#            row.names=F,quote=F,sep=",")


## read in 
sim77_lurf_urfp_allSamples <- read.csv("./data/sim77/sim77_lurf_urfp_dist_allSamples.csv")
sim79_lurf_urfp_allSamples <- read.csv("./data/sim79/sim79_lurf_urfp_dist_allSamples.csv")
sim85_lurf_urfp_allSamples <- read.csv("./data/sim85/sim85_lurf_urfp_dist_allSamples.csv")

# get NPDR-LURF train/test data with predictions from tuned model
head(sim77_trainPred_lurfRF)
head(sim79_trainPred_lurfRF)
head(sim85_trainPred_lurfRF)

# label each type of prediction in the train and test data
sim77_train_lurfRF_local <- sim77_trainPred_lurfRF
sim77_test_lurfRF_local <- sim77_testPred_lurfRF

sim79_train_lurfRF_local <- sim79_trainPred_lurfRF
sim79_test_lurfRF_local <- sim79_testPred_lurfRF

sim85_train_lurfRF_local <- sim85_trainPred_lurfRF
sim85_test_lurfRF_local  <- sim85_testPred_lurfRF

# label TP, TN, FP, FN. make positive class the minority like biosig data
sim77_test_lurf_tf <- true_false_data(data=sim77_test_lurfRF_local,pos_class=0)
head(sim77_test_lurf_tf)
#      intvar3      var35       var64     intvar8   mainvar1   mainvar2    mainvar4   mainvar8
# 1  1.0272703 -1.2158357 -0.05972453 -0.75864064  1.0912238  1.5780790  1.61176689  0.1027399
# 2 -0.7639726 -1.3578192 -0.07546258  1.21220867  0.1924724 -0.6284312  1.06629179 -0.2235473
# 3 -0.4599568 -0.4687442 -0.25149547 -1.44828846 -0.4119003 -0.3893421 -1.04843019  0.8772137
# 4 -0.2234593  1.6640654 -0.16874654 -0.13080320  1.7612002 -0.7866653 -1.09838070  1.4147274
# 5  0.5797351 -0.9366242  0.30820732  1.07307738  0.9745559  0.2905377 -0.03079317  0.3240980
# 6  1.1071810 -0.8218041  0.46998019 -0.05555037 -0.2500002  0.9569221 -0.63341434 -1.2473973
#      mainvar9 class pred pred_type
# 1 -0.52979254     1    0        FP
# 2 -0.31101584     1    0        FP
# 3  0.04202181     1    0        FP
# 4 -0.36757990     1    0        FP
# 5  0.59975662     1    0        FP
# 6 -0.97224973     1    1        TN
tail(sim77_test_lurf_tf)

sim79_test_lurf_tf<- true_false_data(data=sim79_test_lurfRF_local,pos_class=0)

sim85_test_lurf_tf <- true_false_data(sim85_test_lurfRF_local,pos_class = 0)

# same for training data for future analysis
sim77_train_lurf_tf <- true_false_data(sim77_train_lurfRF_local,pos_class=0)
sim79_train_lurf_tf <- true_false_data(sim79_train_lurfRF_local,pos_class=0)
sim85_train_lurf_tf <- true_false_data(sim85_train_lurfRF_local,pos_class=0)

# write these dfs to file
#write.table(sim77_train_lurf_tf,"sim77_tf_train_lurf.csv",row.names=F,quote=F,sep=",")
#write.table(sim77_test_lurf_tf,"sim77_tf_test_lurf.csv",row.names=F,quote=F,sep=",")

#write.table(sim79_train_lurf_tf,"sim79_tf_train_lurf.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_test_lurf_tf,"sim79_tf_test_lurf.csv",row.names=F,quote=F,sep=",")

#write.table(sim85_train_lurf_tf,"sim85_tf_train_lurf.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_test_lurf_tf,"sim85_tf_test_lurf.csv",row.names=F,quote=F,sep=",")


# read in 
sim77_train_lurf_tf <- read.csv("./data/sim77/sim77_tf_train_lurf.csv")
sim77_test_lurf_tf <- read.csv("./data/sim77/sim77_tf_test_lurf.csv")

sim79_train_lurf_tf <- read.csv("./data/sim79/sim79_tf_train_lurf.csv")
sim79_test_lurf_tf <- read.csv("./data/sim79/sim79_tf_test_lurf.csv")

sim85_train_lurf_tf <- read.csv("./data/sim85/sim85_tf_train_lurf.csv")
sim85_test_lurf_tf <- read.csv("./data/sim85/sim85_tf_test_lurf.csv")

head(sim77_train_lurf_tf)


# combine
sim77_lurf_tf_all <- rbind.data.frame(sim77_train_lurf_tf, sim77_test_lurf_tf)
sim79_lurf_tf_all <- rbind.data.frame(sim79_train_lurf_tf, sim79_test_lurf_tf)
sim85_lurf_tf_all <- rbind.data.frame(sim85_train_lurf_tf, sim85_test_lurf_tf)


# choose a test index - 297 is a false negative
tail(sim77_lurf_tf_all)
#         intvar3       var35      var64    intvar8    mainvar1    mainvar2   mainvar4      mainvar8
# 295  0.31583603  0.21954375  0.3568560 -1.4668335  0.59165147 -0.07505094 -0.8591221 -2.6063843896
# 296  0.20558634 -1.84038359  0.3349610  0.7434300  1.14505955 -1.11383311  2.0956066 -0.7880354108
# 297 -1.66854408 -2.11551383 -0.1460695 -0.7897351  0.79334062 -1.49801372  0.3585102  0.2373595146
# 298 -0.11092676  0.07586266  0.3574503  1.3147154 -1.08526672  1.19613187  0.9445567 -0.7898927328
# 299  0.06425202  0.93271716 -1.4425363 -1.1191919  0.01831337 -1.53827202  0.9444972  0.0216402282
# 300  0.29253658  0.49421048 -0.1988638 -0.8377030  0.70879400 -0.07860425 -1.4677275 -0.0003686254
#       mainvar9 class pred pred_type
# 295  0.8156379     0    0        TP
# 296  0.8891642     0    1        FN
# 297 -1.2430330     0    1        FN
# 298  0.2953270     0    0        TP
# 299  0.9497018     0    0        TP
# 300  0.1949572     0    0        TP

test77_fn_idx <- 297 # FN
test77_tp_idx <- 295 # TP

(sim79_lurf_tf_all$pred_type[250:300])# 250 is a FP
test79_fp_idx <- 254 # FP
test79_tp_idx <- 300 # TP

tail(sim85_lurf_tf_all) # 495 is TP, 498 is FN
test85_tp_idx <- 495
test85_fn_idx <- 498

### local NPDR using the URFP distance matrix (NPDR-LURF variables) with all samples
# test on one sample - then loop through others (next section)

# replace class vec with training class and predicted test class
sim77_lurf_local <- local_truePred_df(trainPred_tf_df=sim77_train_lurf_tf, testPred_tf_df = sim77_test_lurf_tf,
                   class_colname = "class", pred_colname = "pred")
head(sim77_lurf_local)
#   intvar3      var35      var64     intvar8   mainvar1   mainvar2   mainvar4
# 1 -0.3375445 -0.3263857  0.3789533  0.42934330  1.3569962  1.2749388  0.1203776
# 2  0.6408931 -1.3947580 -0.4174560 -0.01675236 -0.8779517  0.2719729 -0.7692282
# 3 -1.7047642 -1.0142690 -0.4363001 -0.54736778 -0.8335569 -0.5584615  1.0412128
# 4 -1.3438325 -1.1866532 -0.5933077 -0.30962939  0.4463967 -0.2005929  3.0355534
# 5 -0.9807091  2.1324712 -0.9455821 -0.77551908 -1.0455826  0.9129539 -1.4924557
# 6  0.8018771 -1.8089660  0.2327144  0.55160231  1.1051407 -0.2037404 -0.2130715
#     mainvar8    mainvar9 class pred pred_type
# 1  3.1130645 -0.13781246     1    0     train
# 2 -1.2891825  0.48090626     1    1     train
# 3 -1.0074721  0.44091190     1    1     train
# 4 -1.6562930  0.24419503     1    1     train
# 5  0.3652548 -0.56052302     1    0     train
# 6 -0.6816977  0.01643714     1    0     train
tail(sim77_lurf_local)
#         intvar3       var35      var64    intvar8    mainvar1    mainvar2   mainvar4
# 295  0.31583603  0.21954375  0.3568560 -1.4668335  0.59165147 -0.07505094 -0.8591221
# 296  0.20558634 -1.84038359  0.3349610  0.7434300  1.14505955 -1.11383311  2.0956066
# 297 -1.66854408 -2.11551383 -0.1460695 -0.7897351  0.79334062 -1.49801372  0.3585102
# 298 -0.11092676  0.07586266  0.3574503  1.3147154 -1.08526672  1.19613187  0.9445567
# 299  0.06425202  0.93271716 -1.4425363 -1.1191919  0.01831337 -1.53827202  0.9444972
# 300  0.29253658  0.49421048 -0.1988638 -0.8377030  0.70879400 -0.07860425 -1.4677275
#          mainvar8   mainvar9 class pred pred_type
# 295 -2.6063843896  0.8156379     0    0        TP
# 296 -0.7880354108  0.8891642     0    1        FN
# 297  0.2373595146 -1.2430330     0    1        FN
# 298 -0.7898927328  0.2953270     0    0        TP
# 299  0.0216402282  0.9497018     0    0        TP
# 300 -0.0003686254  0.1949572     0    0        TP

sim79_lurf_local <- local_truePred_df(trainPred_tf_df=sim79_train_lurf_tf, 
                                     testPred_tf_df = sim79_test_lurf_tf,
                                     class_colname = "class", pred_colname = "pred")

sim85_lurf_local <- local_truePred_df(trainPred_tf_df=sim85_train_lurf_tf, 
                                       testPred_tf_df = sim85_test_lurf_tf,
                                       class_colname = "class", pred_colname = "pred")
# 
## write these to file
#write.table(sim77_lurf_local,"sim77_lurf_predType_allSamples.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_lurf_local,"sim79_lurf_predType_allSamples.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_lurf_local,"sim85_lurf_predType_allSamples.csv",row.names=F,quote=F,sep=",")

#sim77_lurf_local <- sim77_lurf_tf_all
#sim79_lurf_local <- sim79_lurf_tf_all
#sim85_lurf_local <- sim85_lurf_tf_all

# compare correct (give actual class) / incorrect prediction for same sample
pred77_idx <- which(colnames(sim77_lurf_local)=="pred")
actual77_idx <- which(colnames(sim77_lurf_local)=="class")
pred77_rm_cols <- which(colnames(sim77_lurf_local) %in% c("class","pred_type"))
actual77_rm_cols <- which(colnames(sim77_lurf_local) %in% c("pred","pred_type"))

pred79_idx <- which(colnames(sim79_lurf_local)=="pred")
actual79_idx <- which(colnames(sim79_lurf_local)=="class")
pred79_rm_cols <- which(colnames(sim79_lurf_local) %in% c("class","pred_type"))
actual79_rm_cols <- which(colnames(sim79_lurf_local) %in% c("pred","pred_type"))

pred85_idx <- which(colnames(sim85_lurf_local)=="pred")
actual85_idx <- which(colnames(sim85_lurf_local)=="class")
pred85_rm_cols <- which(colnames(sim85_lurf_local) %in% c("class","pred_type"))
actual85_rm_cols <- which(colnames(sim85_lurf_local) %in% c("pred","pred_type"))


# local NPDR analysis
sim77_fn_localNPDR <- local_npdr(data=sim77_lurf_local, class_idx=pred77_idx, 
                                  dist_mat = sim77_lurf_urfp_dist,
                                  nbd_metric = "precomputed", 
                                  ss_idx= test77_fn_idx, lambda="lambda.1se", 
                                  rm_cols = pred77_rm_cols, knn=NULL)
sim77_fn_localNPDR$knn
# [1] 78
sim77_fn_localNPDR$nbd_balance 
#  0.6153846
sim77_fn_localNPDR$lambdas
# Measure: Misclassification Error 
#     Lambda Index Measure      SE Nonzero
# min 0.06831    85  0.3062 0.05240       9
# 1se 0.30267    69  0.3500 0.06596       9

# local NPDR variable importance scores
sim77_fn_localNPDR$coeffs
#       att         s1 abs_scores
# 1 mainvar2 -0.3570192  0.3570192
# 2 mainvar4 -0.2396558  0.2396558
# 3 mainvar9 -0.1917312  0.1917312
# 4  intvar8 -0.1817466  0.1817466
# 5    var35  0.1778872  0.1778872
# 6 mainvar8  0.1555998  0.1555998
# 7 mainvar1 -0.1515932  0.1515932
# 8  intvar3  0.1014924  0.1014924
# 9    var64  0.0401729  0.0401729

## calculate the total local score
sim77_fn_total <- sum(sim77_fn_localNPDR$coeffs$s1)
sim77_fn_total
# [1] -0.6465937

# now use the class as the class column - let test_idx be "correct"
sim77_fnActual_localNPDR <- local_npdr(data=sim77_lurf_local, class_idx=actual77_idx, 
                              dist_mat = sim77_lurf_urfp_dist,
                              nbd_metric = "precomputed", 
                              ss_idx= test77_fn_idx, lambda="lambda.1se", 
                              rm_cols = actual77_rm_cols,knn=NULL)

sim77_fnActual_localNPDR$knn
# [1] 73
sim77_fnActual_localNPDR$nbd_balance 
# [1] 0.4246575
sim77_fnActual_localNPDR$lambdas
#      Lambda Index Measure      SE Nonzero
# min 0.0291    93  0.3483 0.06017       9
# 1se 0.8297    57  0.4078 0.05747       9

sim77_fnActual_localNPDR$coeffs # local NPDR variable importance scores for this sample
#        att          s1 abs_scores
# 1 mainvar2  0.16348378 0.16348378
# 2    var64 -0.13628190 0.13628190
# 3 mainvar4  0.11565831 0.11565831
# 4  intvar8  0.10182585 0.10182585
# 5 mainvar8 -0.08691525 0.08691525
# 6 mainvar9  0.07513194 0.07513194
# 7    var35 -0.06892415 0.06892415
# 8 mainvar1  0.06153156 0.06153156
# 9  intvar3 -0.01660582 0.01660582

## calculate the total scores in each case
sim77_fnActual_total <- sum(sim77_fnActual_localNPDR$coeffs$s1)
sim77_fnActual_total
# [1] 0.2089043

## try a true prediction: tn
sim77_tp_localNPDR <- local_npdr(data=sim77_lurf_local, class_idx=actual77_idx, 
                                   dist_mat = sim77_lurf_urfp_dist,
                                   nbd_metric = "precomputed", 
                                   ss_idx= test77_tp_idx, lambda="lambda.1se", 
                                   rm_cols = actual77_rm_cols, knn=NULL)
sim77_tp_localNPDR$knn
# [1] 73
sim77_tp_localNPDR$nbd_balance
# [1] 0.4246575
sim77_tp_localNPDR$lambdas
# Measure: Misclassification Error
# Lambda Index Measure      SE Nonzero
# min 0.5516    63  0.3602 0.04162       9
# 1se 0.8783    58  0.3882 0.04103       9

# local NPDR variable importance scores
sim77_tp_localNPDR$coeffs
#        att           s1  abs_scores
# 1    var64 -0.255940772 0.255940772
# 2 mainvar9  0.113974082 0.113974082
# 3  intvar3  0.101964536 0.101964536
# 4    var35  0.088082948 0.088082948
# 5 mainvar1  0.073764919 0.073764919
# 6 mainvar8 -0.037021270 0.037021270
# 7 mainvar4  0.033488983 0.033488983
# 8 mainvar2 -0.023915049 0.023915049
# 9  intvar8  0.002180057 0.002180057
## calculate the total scores in each case
sim77_tp_total <- sum(sim77_tp_localNPDR$coeffs$s1)
sim77_tp_total
# [1] 0.09657843 # positive

# same for sim79
# local NPDR analysis for a false positive
sim79_fp_localNPDR <- local_npdr(data=sim79_lurf_local, class_idx=pred79_idx, 
                                   dist_mat = sim79_lurf_urfp_dist,
                                   nbd_metric = "precomputed", 
                                   ss_idx= test79_fp_idx, lambda="lambda.1se", 
                                   rm_cols = pred79_rm_cols, knn=NULL)
sim79_fp_localNPDR$knn
# [1] 80
sim79_fp_localNPDR$nbd_balance 
# [1] 0.5375
sim79_fp_localNPDR$lambdas
# Measure: Misclassification Error 
# Lambda Index Measure      SE Nonzero
# min 0.2425    79  0.1310 0.05521       8
# 1se 1.7106    58  0.1821 0.05513       8
sim79_fp_localNPDR$coeffs
#    att          s1 abs_scores
# 1    var67 -0.10306455 0.10306455
# 2 mainvar8  0.09009028 0.09009028
# 3 mainvar9  0.08931509 0.08931509
# 4 mainvar7 -0.07570247 0.07570247
# 5  intvar8 -0.06875193 0.06875193
# 6    var14 -0.06788654 0.06788654
# 7  intvar7 -0.06129721 0.06129721
# 8    var64  0.05561943 0.05561943

## calculate the total scores in each case
sim79_fp_total <- sum(sim79_fp_localNPDR$coeffs$s1)
sim79_fp_total
# [1] -0.1416779

# now use the class as the class column - let test_idx be "correct"
sim79_fpActual_localNPDR <- local_npdr(data=sim79_lurf_local, class_idx=actual79_idx, 
                                     dist_mat = sim79_lurf_urfp_dist,
                                     nbd_metric = "precomputed", 
                                     ss_idx= test79_fp_idx, lambda="lambda.1se", 
                                     rm_cols = actual79_rm_cols,knn=NULL)

sim79_fpActual_localNPDR$knn
# [1] 73
sim79_fpActual_localNPDR$nbd_balance 
# [1] 0.4109589
sim79_fpActual_localNPDR$lambdas
# Measure: Misclassification Error 
#     Lambda Index Measure      SE Nonzero
# min 0.9924    63  0.1663 0.04418       8
# 1se 2.7615    52  0.1946 0.03546       8

sim79_fpActual_localNPDR$coeffs # local NPDR variable importance scores for this sample
#        att          s1 abs_scores
# 1    var67  0.06801651 0.06801651
# 2  intvar7  0.06418728 0.06418728
# 3 mainvar8 -0.05619989 0.05619989
# 4 mainvar9 -0.05568912 0.05568912
# 5    var14  0.05553291 0.05553291
# 6 mainvar7  0.05205647 0.05205647
# 7  intvar8  0.04166422 0.04166422
# 8    var64 -0.03004105 0.03004105

## calculate the total scores in each case
sim79_fpActual_total <- sum(sim79_fpActual_localNPDR$coeffs$s1)
sim79_fpActual_total
# [1] 0.1395273

## try for a correct prediction
# local NPDR analysis
sim79_tp_localNPDR <- local_npdr(data=sim79_lurf_local, class_idx=pred79_idx, 
                                   dist_mat = sim79_lurf_urfp_dist,
                                   nbd_metric = "precomputed", 
                                   ss_idx= test79_tp_idx, lambda="lambda.1se", 
                                   rm_cols = pred79_rm_cols, knn=NULL)
sim79_tp_localNPDR$knn
# [1] 80
sim79_tp_localNPDR$nbd_balance 
# [1] 0.2875
sim79_tp_localNPDR$lambdas
# Measure: Misclassification Error 
#  Lambda Index Measure     SE Nonzero
# min 0.09402    81  0.2750 0.0327       8
# 1se 0.23836    71  0.2967 0.0390       8

# local NPDR variable importance scores
sim79_tp_localNPDR$coeffs
#        att            s1   abs_scores
# 1    var64  0.5529777912 0.5529777912
# 2    var67  0.3406084370 0.3406084370
# 3 mainvar8  0.2506723137 0.2506723137
# 4 mainvar9  0.1052858847 0.1052858847
# 5  intvar8 -0.0755232347 0.0755232347
# 6  intvar7  0.0586076719 0.0586076719
# 7 mainvar7  0.0222117297 0.0222117297
# 8    var14  0.0002307583 0.0002307583

## calculate the total scores in each case
sim79_tp_total <- sum(sim79_tp_localNPDR$coeffs$s1)
sim79_tp_total
# [1] 1.255071 # very positive


# same for sim85
# local NPDR analysis for a false negative
sim85_fn_localNPDR <- local_npdr(data=sim85_lurf_local, class_idx=pred85_idx, 
                                 dist_mat = sim85_lurf_urfp_dist,
                                 nbd_metric = "precomputed", 
                                 ss_idx= test85_fn_idx, lambda="lambda.1se", 
                                 rm_cols = pred85_rm_cols, knn=NULL)
sim85_fn_localNPDR$knn
# [1] 152
sim85_fn_localNPDR$nbd_balance 
#[1] 0.4342105
sim85_fn_localNPDR$lambdas
#     Lambda Index Measure      SE Nonzero
# min 0.1423    77  0.2089 0.02782      10
# 1se 0.3958    66  0.2246 0.03327      10

# local NPDR variable importance scores
sim85_fn_localNPDR$coeffs
#          att          s1 abs_scores
# 1  mainvar10 -0.31265841 0.31265841
# 2   mainvar8 -0.28017853 0.28017853
# 3   mainvar4  0.26656280 0.26656280
# 4   mainvar3  0.26603202 0.26603202
# 5   mainvar5  0.25064157 0.25064157
# 6   mainvar7  0.24915638 0.24915638
# 7   mainvar9 -0.20473497 0.20473497
# 8   mainvar6 -0.14567716 0.14567716
# 9   mainvar1 -0.09795941 0.09795941
# 10   intvar4 -0.02231711 0.02231711

## calculate the total scores in each case
sim85_fn_total <- sum(sim85_fn_localNPDR$coeffs$s1)
sim85_fn_total
# [1] -0.03113283 # slightly negative

# now use the class as the class column - let test_idx be "correct"
sim85_fnActual_localNPDR <- local_npdr(data=sim85_lurf_local, class_idx=actual85_idx, 
                                       dist_mat = sim85_lurf_urfp_dist,
                                       nbd_metric = "precomputed", 
                                       ss_idx= test85_fn_idx, lambda="lambda.1se", 
                                       rm_cols = actual85_rm_cols,knn=NULL)

sim85_fnActual_localNPDR$knn
# [1] 153
sim85_fnActual_localNPDR$nbd_balance 
# [1] 0.5816993
sim85_fnActual_localNPDR$lambdas
# Measure: Misclassification Error 
#    Lambda Index Measure      SE Nonzero
# min 0.4084    65  0.2542 0.03906      10
# 1se 2.1798    47  0.2867 0.04228      10

sim85_fnActual_localNPDR$coeffs # local NPDR variable importance scores for this sample
#          att          s1 abs_scores
# 1  mainvar10  0.08629189 0.08629189
# 2   mainvar5 -0.07448950 0.07448950
# 3   mainvar3 -0.07159108 0.07159108
# 4   mainvar7 -0.07156112 0.07156112
# 5   mainvar8  0.06806390 0.06806390
# 6   mainvar4 -0.06440286 0.06440286
# 7   mainvar9  0.06163278 0.06163278
# 8   mainvar6  0.04199053 0.04199053
# 9   mainvar1  0.03061888 0.03061888
# 10   intvar4  0.01399750 0.01399750

## calculate the total scores in each case
sim85_fnActual_total <- sum(sim85_fnActual_localNPDR$coeffs$s1)
sim85_fnActual_total
# [1] 0.02055091 # now positive

## try for a correct prediction
sim85_tp_localNPDR <- local_npdr(data=sim85_lurf_local, class_idx=pred85_idx, 
                                 dist_mat = sim85_lurf_urfp_dist,
                                 nbd_metric = "precomputed", 
                                 ss_idx= test85_tp_idx, lambda="lambda.1se", 
                                 rm_cols = pred85_rm_cols, knn=NULL)
sim85_tp_localNPDR$knn
# [1] 152
sim85_tp_localNPDR$nbd_balance
# [1] 0.3684211
sim85_tp_localNPDR$lambdas
# Measure: Misclassification Error 
#  Lambda Index Measure      SE Nonzero
# min 0.3425    71  0.1935 0.03340      10
# 1se 2.4160    50  0.2232 0.03876      10

# local NPDR variable importance scores
sim85_tp_localNPDR$coeffs
#          att           s1  abs_scores
# 1   mainvar9 -0.097154592 0.097154592
# 2   mainvar5  0.072195173 0.072195173
# 3   mainvar4  0.059510773 0.059510773
# 4   mainvar1  0.050291791 0.050291791
# 5   mainvar8  0.042690694 0.042690694
# 6    intvar4 -0.034188880 0.034188880
# 7   mainvar3  0.032136661 0.032136661
# 8  mainvar10 -0.022075823 0.022075823
# 9   mainvar7 -0.011264701 0.011264701
# 10  mainvar6 -0.006157785 0.006157785

## calculate the total scores in each case
sim85_tp_total <- sum(sim85_tp_localNPDR$coeffs$s1)
sim85_tp_total
# [1] 0.08598331 # positive


# make dfs for plotting using function and selected test_idx
sim77_test_vec <- c(test77_fn_idx,test77_tp_idx)
sim79_test_vec <- c(test79_fp_idx,test79_tp_idx)
sim85_test_vec <- c(test85_fn_idx, test85_tp_idx)

prob_colnames <- c("prob_0","prob_1")
head(sim77_lurfProb_allSamples) # probability df
head(sim79_lurfProb_allSamples)
head(sim85_lurfProb_allSamples)


### test my loop function that does pred and actual
sim77_test_localNPDR <- localNPDR_actualPred_scores(data=sim77_lurf_local, 
                           test_idx_vec=sim77_test_vec, pred_colname="pred", 
                           actual_colname="class", pred_rm_cols=c("class","pred_type"),
                           actual_rm_cols=c("pred","pred_type"),
                           nbd_metric="precomputed", dist_mat=sim77_lurf_urfp_allSamples,
                           lambda="lambda.1se", probability_data=T,
                           prob_cols_vec=prob_colnames, prob_df=sim77_lurfProb_allSamples)
names(sim77_test_localNPDR)
# [1] "pred"   "actual"
length(sim77_test_localNPDR$pred)
# [1] 2
names(sim77_test_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"           
# [5] "total_local_score" "melted_df" 
head(sim77_test_localNPDR$pred[[1]]$melted_df)
#    class pred    prob_0    prob_1 total_local_score variable local_score        sign
# 1     0    1 0.4368877 0.5631123       -0.08002948 mainvar1 -0.01961457 Contradicts
# 2     0    1 0.4368877 0.5631123       -0.08002948    var64  0.03063766    Supports
# 3     0    1 0.4368877 0.5631123       -0.08002948 mainvar2 -0.03925677 Contradicts
# 4     0    1 0.4368877 0.5631123       -0.08002948    var35  0.04453058    Supports
# 5     0    1 0.4368877 0.5631123       -0.08002948  intvar8  0.04518817    Supports
# 6     0    1 0.4368877 0.5631123       -0.08002948  intvar3  0.05725564    Supports
sim77_test_localNPDR$pred[[1]]$total_local_score
# [1] -0.08002948

# compare to score with actual class label
sim77_test_localNPDR$actual[[1]]$total_local_score
# [1] 0.282336

sim77_test_localNPDR$pred[[2]]$total_local_score
# [1] 0.004907221

# compare to score with actual class label - should be similar
sim77_test_localNPDR$actual[[2]]$total_local_score # why are these scores so different?
# [1] 0.1164985


## other simualted dataset: sim79
sim79_test_localNPDR <- localNPDR_actualPred_scores(data=sim79_lurf_local, 
                                                   test_idx_vec=sim79_test_vec, 
                                                   pred_colname="pred", 
                              actual_colname="class", pred_rm_cols=c("class","pred_type"),
                              actual_rm_cols=c("pred","pred_type"),
                              nbd_metric="precomputed", dist_mat=sim79_lurf_urfp_allSamples,
                              lambda="lambda.1se", probability_data=T,
                              prob_cols_vec=prob_colnames, prob_df=sim79_lurfProb_allSamples)
head(sim79_test_localNPDR$pred[[1]]$melted_df)
#   class pred prob_0 prob_1 total_local_score variable  local_score        sign
# 1     1    0   0.54   0.46        -0.1709285    var64  0.002485843    Supports
# 2     1    0   0.54   0.46        -0.1709285  intvar7  0.071142366    Supports
# 3     1    0   0.54   0.46        -0.1709285    var14 -0.097658806 Contradicts
# 4     1    0   0.54   0.46        -0.1709285    var67 -0.173719756 Contradicts
# 5     1    0   0.54   0.46        -0.1709285  intvar8 -0.192108763 Contradicts
# 6     1    0   0.54   0.46        -0.1709285 mainvar9  0.214356195    Supports

sim79_test_localNPDR$pred[[1]]$total_local_score
# [1] -0.1709285

# compare to score with actual class label
sim79_test_localNPDR$actual[[1]]$total_local_score
# [1] 0.2993855

sim79_test_localNPDR$pred[[2]]$total_local_score # why are these so different?
# [1] 1.598612

# compare to score with actual class label
sim79_test_localNPDR$actual[[2]]$total_local_score
#[1] 0.2765511

## sim85 simualted data
sim85_test_localNPDR <- localNPDR_actualPred_scores(data=sim85_lurf_local, 
                                  test_idx_vec=sim85_test_vec, 
                                  pred_colname="pred", 
                                  actual_colname="class", pred_rm_cols=c("class","pred_type"),
                                  actual_rm_cols=c("pred","pred_type"),
                                  nbd_metric="precomputed", dist_mat=sim85_lurf_urfp_allSamples,
                                  lambda="lambda.1se", probability_data=T,
                                  prob_cols_vec=prob_colnames, prob_df=sim85_lurfProb_allSamples)
head(sim85_test_localNPDR$pred[[1]]$melted_df)
#   class pred    prob_0    prob_1 total_local_score variable local_score        sign
# 1     0    1 0.3671282 0.6328718       -0.04414775 mainvar6 -0.03842917 Contradicts
# 2     0    1 0.3671282 0.6328718       -0.04414775 mainvar1 -0.04243754 Contradicts
# 3     0    1 0.3671282 0.6328718       -0.04414775  intvar4 -0.07858087 Contradicts
# 4     0    1 0.3671282 0.6328718       -0.04414775 mainvar7  0.08334786    Supports
# 5     0    1 0.3671282 0.6328718       -0.04414775 mainvar8 -0.09243856 Contradicts
# 6     0    1 0.3671282 0.6328718       -0.04414775 mainvar5  0.12115931    Supports

sim85_test_localNPDR$pred[[1]]$total_local_score
#[1] -0.04414775

# compare to score with actual class label
sim85_test_localNPDR$actual[[1]]$total_local_score
# [1] 0.02312084

sim85_test_localNPDR$pred[[2]]$total_local_score # finally, close?
# [1] 0.07543779

# compare to score with actual class label
sim85_test_localNPDR$actual[[2]]$total_local_score
# [1] 0.2331296


##############################################
# 8. Plot local NPDR results for test samples
#############################################
## plot 
sim77_pred1_p <- local_importance_plot(melted_df=sim77_test_localNPDR$pred[[1]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim77_test_vec[1], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim77_lurf_feat)
sim77_pred1_p

sim77_actual1_p <- local_importance_plot(melted_df=sim77_test_localNPDR$actual[[1]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim77_test_vec[1], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim77_lurf_feat)
sim77_actual1_p


sim77_pred2_p <- local_importance_plot(melted_df=sim77_test_localNPDR$pred[[2]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim77_test_vec[2], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim77_lurf_feat)
sim77_pred2_p       


sim77_actual2_p <- local_importance_plot(melted_df=sim77_test_localNPDR$actual[[2]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim77_test_vec[2], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim77_lurf_feat)
sim77_actual2_p      


## sim79
sim79_pred1_p <- local_importance_plot(melted_df=sim79_test_localNPDR$pred[[1]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim79_test_vec[1], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim79_lurf_feat)
sim79_pred1_p

sim79_actual1_p <- local_importance_plot(melted_df=sim79_test_localNPDR$actual[[1]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim79_test_vec[1], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim79_lurf_feat)
sim79_actual1_p

sim79_pred2_p <- local_importance_plot(melted_df=sim79_test_localNPDR$pred[[2]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim79_test_vec[2], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim79_lurf_feat)
sim79_pred2_p       


sim79_actual2_p <- local_importance_plot(melted_df=sim79_test_localNPDR$actual[[2]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim79_test_vec[2], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim79_lurf_feat)
sim79_actual2_p      

## sim85
sim85_pred1_p <- local_importance_plot(melted_df=sim85_test_localNPDR$pred[[1]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim85_test_vec[1], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim85_lurf_feat)
sim85_pred1_p

sim85_actual1_p <- local_importance_plot(melted_df=sim85_test_localNPDR$actual[[1]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim85_test_vec[1], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim85_lurf_feat)
sim85_actual1_p

sim85_pred2_p <- local_importance_plot(melted_df=sim85_test_localNPDR$pred[[2]]$melted_df, 
                                       actual_colname="class",
                                       class_names = c("0","1"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=sim85_test_vec[2], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                       var_order=sim85_lurf_feat)
sim85_pred2_p       


sim85_actual2_p <- local_importance_plot(melted_df=sim85_test_localNPDR$actual[[2]]$melted_df, 
                                         actual_colname="class",
                                         class_names = c("0","1"),
                                         pred_colname="pred",
                                         main_title="Single-sample variable importance for NPDR-LURF selected features",
                                         analysis=sim85_test_vec[2], 
                                         caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnSURF",
                                         var_order=sim85_lurf_feat)
sim85_actual2_p      

# save these example dfs
#write.table(sim77_test_localNPDR$pred[[1]]$melted_df,
#            "sim77_localNPDR_predFN_sample297.csv",quote=F,row.names=F,sep=",")
#write.table(sim77_test_localNPDR$pred[[2]]$melted_df,
#            "sim77_localNPDR_predTP_sample295.csv",quote=F,row.names=F,sep=",")

#write.table(sim79_test_localNPDR$pred[[1]]$melted_df,
#            "sim79_localNPDR_predFP_sample254.csv",quote=F,row.names=F,sep=",")
#write.table(sim79_test_localNPDR$pred[[2]]$melted_df,
#            "sim79_localNPDR_predTP_sample300.csv",quote=F,row.names=F,sep=",")

#write.table(sim85_test_localNPDR$pred[[1]]$melted_df,
#            "sim85_localNPDR_predFP_sample498.csv",quote=F,row.names=F,sep=",")
#write.table(sim85_test_localNPDR$pred[[2]]$melted_df,
#            "sim85_localNPDR_predTP_sample495.csv",quote=F,row.names=F,sep=",")


##################################################################
# 9. sim data: Local NPDR for all test samples - box/violin plots 
##################################################################
# loop over all test samples - do local NPDR, find total scores for actual and pred labels
# use modified form of Brett's function with error checking 

#write.table(sim77_lurf_local,"sim77_lurf_local.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_lurf_local,"sim79_lurf_local.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_lurf_local,"sim85_lurf_local.csv",row.names=F,quote=F,sep=",")

# read in 
sim77_lurf_local <- read.csv("./data/sim77/sim77_lurf_local.csv")
sim79_lurf_local <- read.csv("./data/sim79/sim79_lurf_local.csv")
sim85_lurf_local <- read.csv("./data/sim85/sim85_lurf_local.csv")
head(sim77_lurf_local)
tail(sim77_lurf_local)


sim77_test_idx <- which(sim77_lurf_local$pred_type != "train")
sim79_test_idx <- which(sim79_lurf_local$pred_type != "train")
sim85_test_idx <- which(sim85_lurf_local$pred_type != "train")

sim77_train_idx <- which(sim77_lurf_local$pred_type == "train")
sim79_train_idx <- which(sim79_lurf_local$pred_type == "train")
sim85_train_idx <- which(sim85_lurf_local$pred_type == "train")

## scores for true/false predictions
sim77_tf_localNPDR <- localNPDR_predTF_scores(pred_data=sim77_lurf_local, 
                                              test_idx_vec=sim77_test_idx,
                                              pred_colname="pred", class_colname="class",
                                              dist_mat=sim77_lurf_urfp_allSamples, 
                                              nbd_metric="precomputed", knn="kmax", 
                                              rm_cols = c("pred_type"), verbose=T)
# no warnings
sim77_test_true_vec = sim77_tf_localNPDR$true_vec
length(sim77_test_true_vec)
# [1] 43
head(sim77_test_true_vec)
# [1] 0.5964652 1.4894058 1.3837894 0.7353271 2.9761305 2.0545964
# [1] 0.05799454 0.07914204 1.64966471 0.28520973 0.75907294 0.15266508
sim77_test_false_vec = sim77_tf_localNPDR$false_vec
length(sim77_test_false_vec)
#[1] 17
head(sim77_test_false_vec)
# [1] -1.0992604 -0.6390171 -0.5174496 -0.4793478 -0.9944400 -0.6968559
# [1] -0.02311657  0.06804536  0.03308016 -0.02276269  0.03972726  0.03540686

t.test(sim77_test_true_vec, sim77_test_false_vec)
# Welch Two Sample t-test
# 
# data:  sim77_test_true_vec and sim77_test_false_vec
# t = 5.1055, df = 52.936, p-value = 4.587e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2429281 0.5573234
# sample estimates:
#   mean of x   mean of y 
# 0.37187413 -0.02825165 

# save object to file
#saveRDS(sim77_tf_localNPDR,"sim77_tf_localNPDR.rds")

# read in
sim77_tf_localNPDR <- readRDS("./data/sim77/sim77_tf_localNPDR.rds")
sim77_test_true_vec <- sim77_tf_localNPDR$true_vec
sim77_test_false_vec <- sim77_tf_localNPDR$false_vec



# sim79 data
sim79_tf_localNPDR <- localNPDR_predTF_scores(pred_data=sim79_lurf_local, 
                                              test_idx_vec=sim79_test_idx,
                                              pred_colname="pred", class_colname="class",
                                              dist_mat=sim79_lurf_urfp_allSamples, 
                                              nbd_metric="precomputed", knn="kmax", verbose=T)

sim79_test_true_vec = sim79_tf_localNPDR$true_vec
length(sim79_test_true_vec)
# [1] 47
head(sim79_test_true_vec)
# [1] -0.09558737 -0.46614569 -0.14243011 -0.07110983  0.39011619  0.26377922
# [1] 0.32107303 0.03886930 0.03914459 0.55881754 0.55440013 0.30043170
sim79_test_false_vec = sim79_tf_localNPDR$false_vec
length(sim79_test_false_vec)
# [1] 13
head(sim79_test_false_vec)
# [1] 0.07938967 0.09057283 0.16933943 0.26431990 0.03041589 0.26472865
# [1] -0.036417419 -0.211263470  0.011731788 -0.012971941 -0.085610325 -0.003456623
t.test(sim79_test_true_vec, sim79_test_false_vec)
# Welch Two Sample t-test
# 
# data:  sim79_test_true_vec and sim79_test_false_vec
# t = 8.6467, df = 54.957, p-value = 7.85e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4306657 0.6905290
# sample estimates:
#   mean of x   mean of y 
# 0.48206695 -0.07853039 
# now significant, probably an error before

# save object to file
#saveRDS(sim79_tf_localNPDR,"sim79_tf_localNPDR.rds")

# read in
sim79_tf_localNPDR <- readRDS("./data/sim79/sim79_tf_localNPDR.rds")
sim79_test_true_vec <- sim79_tf_localNPDR$true_vec
sim79_test_false_vec <- sim79_tf_localNPDR$false_vec

# sim85 data
sim85_tf_localNPDR <- localNPDR_predTF_scores(pred_data=sim85_lurf_local, 
                                              test_idx_vec=sim85_test_idx,
                                              pred_colname="pred", class_colname="class",
                                              dist_mat=sim85_lurf_urfp_allSamples, 
                                              nbd_metric="precomputed", knn="kmax", verbose=T)
sim85_test_true_vec = sim85_tf_localNPDR$true_vec
length(sim85_test_true_vec)
# [1] 80
head(sim85_test_true_vec)
# [1] 0.6344624 0.1143621 0.1948678 0.1896219 0.2966088 0.2150418
# [1] 0.5099260 0.1664656 1.4815209 3.7531243 0.2339454 0.9952128
sim85_test_false_vec = sim85_tf_localNPDR$false_vec
length(sim85_test_false_vec)
# [1] 20
head(sim85_test_false_vec)
# [1]  0.143792426 -0.002509162 -0.024853842  0.222702559 -0.023111211  0.033358070
# [1]  0.003594027 -0.217350523 -0.141514055 -0.044189746 -0.116099395 -0.030021238
t.test(sim85_test_true_vec, sim85_test_false_vec)
#	Welch Two Sample t-test
# 
# data:  sim85_test_true_vec and sim85_test_false_vec
# t = 8.1366, df = 90.327, p-value = 2.085e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.3342476 0.5501831
# sample estimates:
#   mean of x   mean of y 
# 0.34286855 -0.09934678

# save object to file
#saveRDS(sim85_tf_localNPDR,"sim85_tf_localNPDR.rds")


# read in
sim85_tf_localNPDR <- readRDS("./data/sim85/sim85_tf_localNPDR.rds")
sim85_test_true_vec <- sim85_tf_localNPDR$true_vec
sim85_test_false_vec <- sim85_tf_localNPDR$false_vec



# read in 
sim77_tfTest_localNPDR <- readRDS("./data/sim77/sim77_tf_localNPDR.rds")
sim79_tfTest_localNPDR <- readRDS("./data/sim79/sim79_tf_localNPDR.rds")
sim85_tfTest_localNPDR <- readRDS("./data/sim85/sim85_tf_localNPDR.rds")

# dataframes for ggplot
# sim77
sim77_totalTest_local_df = data.frame(
  classification=c(rep("True",length(sim77_tfTest_localNPDR$true_vec)),
                   rep("False",length(sim77_tfTest_localNPDR$false_vec))), 
  value=c(sim77_tfTest_localNPDR$true_vec,sim77_tfTest_localNPDR$false_vec))
sim77_totalTest_local_df$classification <- factor(sim77_totalTest_local_df$classification,
                                              levels = c('True','False'),ordered = TRUE)
head(sim77_totalTest_local_df)
# classification      value
# 1           True 0.05799454
# 2           True 0.07914204
# 3           True 1.64966471
# 4           True 0.28520973
# 5           True 0.75907294
# 6           True 0.15266508

# sim79
sim79_totalTest_local_df = data.frame(
  classification=c(rep("True",length(sim79_tfTest_localNPDR$true_vec)),
                   rep("False",length(sim79_tfTest_localNPDR$false_vec))), 
  value=c(sim79_tfTest_localNPDR$true_vec,sim79_tfTest_localNPDR$false_vec))
sim79_totalTest_local_df$classification <- factor(sim79_totalTest_local_df$classification,
                                              levels = c('True','False'),ordered = TRUE)

# sim85
sim85_totalTest_local_df = data.frame(
  classification=c(rep("True",length(sim85_tfTest_localNPDR$true_vec)),
                   rep("False",length(sim85_tfTest_localNPDR$false_vec))), 
  value=c(sim85_tfTest_localNPDR$true_vec,sim85_tfTest_localNPDR$false_vec))
sim85_totalTest_local_df$classification <- factor(sim85_totalTest_local_df$classification,
                                              levels = c('True','False'),ordered = TRUE)
head(sim85_totalTest_local_df)
# classification     value
# 1           True 0.5099260
# 2           True 0.1664656
# 3           True 1.4815209
# 4           True 3.7531243
# 5           True 0.2339454
# 6           True 0.9952128

# save dfs to file
#write.table(sim77_totalTest_local_df,"sim77_total_local_test_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_totalTest_local_df,"sim79_total_local_test_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_totalTest_local_df,"sim85_total_local_test_df.csv",row.names=F,quote=F,sep=",")

# read in 
sim77_totalTest_local_df <- read.csv("./data/sim77/sim77_total_local_test_df.csv")
sim79_totalTest_local_df <- read.csv("./data/sim79/sim79_total_local_test_df.csv")
sim85_totalTest_local_df <- read.csv("./data/sim85/sim85_total_local_test_df.csv")

#sim77_totalTest_local_df$classification <- as.factor(sim77_totalTest_local_df$classification)
# note have to rerun making dfs above to get True, False releveled so True appears first

### box plots
# sim77 test data
sim77_box = ggplot(sim77_totalTest_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim77 total local scores: test data")
print(sim77_box)

# sim79 test data
sim79_box = ggplot(sim79_totalTest_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim79 total local scores: test data")
print(sim79_box)

# sim85 test data
sim85_box = ggplot(sim85_totalTest_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim85 total local scores: test data")
print(sim85_box)

#### violin plots
# sim77 test data
sim77_localVio_df <- data.frame(local_scores = c(sim77_test_true_vec,sim77_test_false_vec))
group_factor = factor(c(rep("True",length(sim77_test_true_vec)),
                        rep("False",length(sim77_test_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim77_localVio_df$group = group_factor
vioplot(local_scores~group,data=sim77_localVio_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim77 local scores: test data")

# sim79 test data
sim79_localVio_df <- data.frame(local_scores = c(sim79_test_true_vec,sim79_test_false_vec))
group_factor = factor(c(rep("True",length(sim79_test_true_vec)),
                        rep("False",length(sim79_test_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim79_localVio_df$group = sim79_localVio_df$group = group_factor
vioplot(local_scores~group,data=sim79_localVio_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim79 local scores: test data")

# sim85 test data
sim85_localVio_df <- data.frame(local_scores = c(sim85_test_true_vec,sim85_test_false_vec))
group_factor = factor(c(rep("True",length(sim85_test_true_vec)),
                        rep("False",length(sim85_test_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim85_localVio_df$group = sim85_localVio_df$group = group_factor
vioplot(local_scores~group,data=sim85_localVio_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim85 local scores: test data")

# save violin plot dfs to file
#write.table(sim77_localVio_df,"sim77_total_localVio_test_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_localVio_df,"sim79_total_localVio_test_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_localVio_df,"sim85_total_localVio_test_df.csv",row.names=F,quote=F,sep=",")



################# do same analysis for train data
sim77_tfTrain_localNPDR <- localNPDR_predTF_scores(pred_data=sim77_lurf_local, 
                                                   test_idx_vec=sim77_train_idx,
                                                   pred_colname="pred", class_colname="class",
                                                   dist_mat=sim77_lurf_urfp_allSamples, 
                                                   nbd_metric="precomputed", knn="kmax", 
                                                   rm_cols = c("pred_type"), verbose=T)
# no warnings
sim77_train_true_vec = sim77_tfTrain_localNPDR$true_vec
length(sim77_train_true_vec)
# 187
head(sim77_train_true_vec)
# [1]  0.01145496 -0.02413419  0.02925515  0.05555679  0.04541746 -0.03632944
sim77_train_false_vec = sim77_tfTrain_localNPDR$false_vec
head(sim77_train_false_vec)
# [1] -0.03108203 -0.03892299  0.06206135  0.01139847  0.02571076  0.07150521
length(sim77_train_false_vec)
#[1] 53
t.test(sim77_train_true_vec, sim77_train_false_vec)
# Welch Two Sample t-test
# 
# data:  sim77_train_true_vec and sim77_train_false_vec
# t = 7.7962, df = 93.003, p-value = 9.013e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2986885 0.5028531
# sample estimates:
#   mean of x   mean of y 
# 0.31602768 -0.08474312

# save object to file
#saveRDS(sim77_tf_localNPDR,"sim77_tfTrain_localNPDR.rds")

# read in
sim77_tfTrain_localNPDR <- readRDS("./data/sim77/sim77_tfTrain_localNPDR.rds")
sim77_train_true_vec <- sim77_tfTrain_localNPDR$true_vec
sim77_train_false_vec <- sim77_tfTrain_localNPDR$false_vec


# sim79
sim79_tfTrain_localNPDR <- localNPDR_predTF_scores(pred_data=sim79_lurf_local, 
                                                   test_idx_vec=sim79_train_idx,
                                                   pred_colname="pred", class_colname="class",
                                                   dist_mat=sim79_lurf_urfp_allSamples, 
                                                   nbd_metric="precomputed", knn="kmax", 
                                                   rm_cols = c("pred_type"), verbose=T)
# no warnings
sim79_train_true_vec = sim79_tfTrain_localNPDR$true_vec
head(sim79_train_true_vec)
# [1] 0.11962872 0.05221590 0.02555693 0.09194296 0.06959152 2.01690710
length(sim79_train_true_vec)
# [1] 199
sim79_train_false_vec = sim79_tfTrain_localNPDR$false_vec
head(sim79_train_false_vec)
#[1] -0.01972073 -0.18901938 -0.14998844 -0.25484286 -0.11386052 -0.03899921
length(sim79_train_false_vec)
#[1] 41
t.test(sim79_train_true_vec, sim79_train_false_vec)
#Welch Two Sample t-test
# 
# data:  sim79_train_true_vec and sim79_train_false_vec
# t = 11.905, df = 85.784, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.5369300 0.7521916
# sample estimates:
#   mean of x  mean of y 
# 0.4545763 -0.1899845


# save object to file
#saveRDS(sim79_tf_localNPDR,"sim79_tfTrain_localNPDR.rds")

# read in
sim79_tfTrain_localNPDR <- readRDS("./data/sim79/sim79_tfTrain_localNPDR.rds")
sim79_train_true_vec <- sim79_tfTrain_localNPDR$true_vec
sim79_train_false_vec <- sim79_tfTrain_localNPDR$false_vec


# sim85
sim85_tfTrain_localNPDR <- localNPDR_predTF_scores(pred_data=sim85_lurf_local, 
                                                   test_idx_vec=sim85_train_idx,
                                                   pred_colname="pred", class_colname="class",
                                                   dist_mat=sim85_lurf_urfp_allSamples, 
                                                   nbd_metric="precomputed", knn="kmax", 
                                                   rm_cols = c("pred_type"), verbose=T)
# no warnings
sim85_train_true_vec = sim85_tfTrain_localNPDR$true_vec
head(sim85_train_true_vec)
# [1] 0.1076500 0.1917818 0.1570751 0.3522268 0.1789994 0.1961562
length(sim85_train_true_vec)
#[1] 337
sim85_train_false_vec = sim85_tfTrain_localNPDR$false_vec
head(sim85_train_false_vec)
#[1] -0.003469413 -0.190475258  0.015671410 -0.075876268 -0.054715410 -0.146430853
length(sim85_train_false_vec)
#[1] 63
t.test(sim85_train_true_vec, sim85_train_false_vec)
# Welch Two Sample t-test
# 
# data:  sim85_train_true_vec and sim85_train_false_vec
# t = 13.945, df = 379.54, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.4544788 0.6036835
# sample estimates:
#   mean of x  mean of y 
# 0.4051665 -0.1239147

# save object to file
#saveRDS(sim85_tf_localNPDR,"sim85_tfTrain_localNPDR.rds")

# read in
sim85_tfTrain_localNPDR <- readRDS("./data/sim85/sim85_tfTrain_localNPDR.rds")
sim85_train_true_vec <- sim85_tfTrain_localNPDR$true_vec
sim85_train_false_vec <- sim85_tfTrain_localNPDR$false_vec



# dataframes for ggplot
# sim77
sim77_totalTrain_local_df = data.frame(
  classification=c(rep("True",length(sim77_tfTrain_localNPDR$true_vec)),
                   rep("False",length(sim77_tfTrain_localNPDR$false_vec))), 
  value=c(sim77_tfTrain_localNPDR$true_vec,sim77_tfTrain_localNPDR$false_vec))
sim77_totalTrain_local_df$classification <- factor(sim77_totalTrain_local_df$classification,
                                                   levels = c('True','False'),ordered = TRUE)
# sim79
sim79_totalTrain_local_df = data.frame(
  classification=c(rep("True",length(sim79_tfTrain_localNPDR$true_vec)),
                   rep("False",length(sim79_tfTrain_localNPDR$false_vec))), 
  value=c(sim79_tfTrain_localNPDR$true_vec,sim79_tfTrain_localNPDR$false_vec))
sim79_totalTrain_local_df$classification <- factor(sim79_totalTrain_local_df$classification,
                                                   levels = c('True','False'),ordered = TRUE)

# sim85
sim85_totalTrain_local_df = data.frame(
  classification=c(rep("True",length(sim85_tfTrain_localNPDR$true_vec)),
                   rep("False",length(sim85_tfTrain_localNPDR$false_vec))), 
  value=c(sim85_tfTrain_localNPDR$true_vec,sim85_tfTrain_localNPDR$false_vec))
sim85_totalTrain_local_df$classification <- factor(sim85_totalTrain_local_df$classification,
                                                   levels = c('True','False'),ordered = TRUE)

### box plots
sim77_train_box = ggplot(sim77_totalTrain_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim77 total local scores: training data")
print(sim77_train_box)

sim79_train_box = ggplot(sim79_totalTrain_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim79 total local scores: training data")
print(sim79_train_box)

sim85_train_box = ggplot(sim85_totalTrain_local_df) + 
  geom_boxplot(aes(x=classification, y=value, fill=classification)) +
  scale_fill_manual(values=c("blue", "red")) +
  theme(legend.position = "none") + theme_bw() + labs(title="sim85 total local scores: training data")
print(sim85_train_box)

# violin plot
sim77_localTrain_df <- data.frame(local_scores = c(sim77_train_true_vec,sim77_train_false_vec))
group_factor = factor(c(rep("True",length(sim77_train_true_vec)),
                        rep("False",length(sim77_train_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim77_localTrain_df$group = sim77_localTrain_df$group = group_factor
vioplot(local_scores~group,data=sim77_localTrain_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim77 local scores: training data")

sim79_localTrain_df <- data.frame(local_scores = c(sim79_train_true_vec,sim79_train_false_vec))
group_factor = factor(c(rep("True",length(sim79_train_true_vec)),
                        rep("False",length(sim79_train_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim79_localTrain_df$group = sim79_localTrain_df$group = group_factor
vioplot(local_scores~group,data=sim79_localTrain_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim79 local scores: training data")

# sim85 training
sim85_localTrain_df <- data.frame(local_scores = c(sim85_train_true_vec,sim85_train_false_vec))
group_factor = factor(c(rep("True",length(sim85_train_true_vec)),
                        rep("False",length(sim85_train_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
sim85_localTrain_df$group = sim85_localTrain_df$group = group_factor
vioplot(local_scores~group,data=sim85_localTrain_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="sim85 local scores: training data")


## save dfs to file
# box plots
#write.table(sim77_totalTrain_local_df,"sim77_total_local_train_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_totalTrain_local_df,"sim79_total_local_train_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_totalTrain_local_df,"sim85_total_local_train_df.csv",row.names=F,quote=F,sep=",")

# violin plots
#write.table(sim77_localTrain_df,"sim77_total_localVio_train_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_localTrain_df,"sim79_total_localVio_train_df.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_localTrain_df,"sim85_total_localVio_train_df.csv",row.names=F,quote=F,sep=",")




## calc scores for predicted class + melted df for test samples + opposite class pred
prob_colnames <- c("prob_0","prob_1")

# sim77
sim77_testSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim77_lurf_local, 
                                        test_idx_vec=sim77_test_idx, 
                                        pred_colname="pred", 
                                        actual_colname="class", 
                                        pred_rm_cols=c("class","pred_type"),
                                        actual_rm_cols=c("pred","pred_type"),
                                        nbd_metric="precomputed", 
                                        dist_mat=sim77_lurf_urfp_allSamples,
                                        lambda="lambda.1se", 
                                        probability_data=T, 
                                        knn="kmax",
                                        prob_cols_vec=prob_colnames, 
                                        prob_df=sim77_lurfProb_allSamples)
# info stored for each sample
names(sim77_testSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 
sim77_testSamples_localNPDR$pred[[1]]$melted_df # uses pred for localNPDR
#   class pred    prob_0    prob_1 total_local_score variable  local_score        sign
# 1     1    0 0.6561694 0.3438306        0.09806477    var64 -0.006641950 Contradicts
# 2     1    0 0.6561694 0.3438306        0.09806477 mainvar1  0.009732447    Supports
# 3     1    0 0.6561694 0.3438306        0.09806477  intvar3  0.014536423    Supports
# 4     1    0 0.6561694 0.3438306        0.09806477  intvar8 -0.016500062 Contradicts
# 5     1    0 0.6561694 0.3438306        0.09806477    var35 -0.019344585 Contradicts
# 6     1    0 0.6561694 0.3438306        0.09806477 mainvar2 -0.038319118 Contradicts
# 7     1    0 0.6561694 0.3438306        0.09806477 mainvar4  0.046281246    Supports
# 8     1    0 0.6561694 0.3438306        0.09806477 mainvar9  0.053310053    Supports
# 9     1    0 0.6561694 0.3438306        0.09806477 mainvar8  0.055010322    Supports
sim77_testSamples_localNPDR$pred[[1]]$total_local_score
# [1] 0.09806477

# compare to score with actual class label
sim77_testSamples_localNPDR$opposite[[1]]$melted_df #uses pseudo_class for localNPDR (opposite pred)
#   pseudo_class pred    prob_0    prob_1 total_local_score variable local_score        sign
# 1            1    0 0.6561694 0.3438306         0.2691566    var64 -0.01481102 Contradicts
# 2            1    0 0.6561694 0.3438306         0.2691566 mainvar1  0.01938402    Supports
# 3            1    0 0.6561694 0.3438306         0.2691566  intvar3  0.04464692    Supports
# 4            1    0 0.6561694 0.3438306         0.2691566  intvar8 -0.05016406 Contradicts
# 5            1    0 0.6561694 0.3438306         0.2691566    var35 -0.05829313 Contradicts
# 6            1    0 0.6561694 0.3438306         0.2691566 mainvar2 -0.10188184 Contradicts
# 7            1    0 0.6561694 0.3438306         0.2691566 mainvar4  0.12874086    Supports
# 8            1    0 0.6561694 0.3438306         0.2691566 mainvar8  0.14981005    Supports
# 9            1    0 0.6561694 0.3438306         0.2691566 mainvar9  0.15172479    Supports
sim77_testSamples_localNPDR$opposite[[1]]$total_local_score
# [1] 0.2691566 # increases in this case

# write these lists to file
#saveRDS(sim77_testSamples_localNPDR$pred,"sim77_localNPDR_pred_testSamples.rds")
#saveRDS(sim77_testSamples_localNPDR$opposite,"sim77_localNPDR_opposite_testSamples.rds")

# sim79
sim79_testSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim79_lurf_local, 
                                      test_idx_vec=sim79_test_idx, 
                                      pred_colname="pred", 
                                      actual_colname="class", 
                                      pred_rm_cols=c("class","pred_type"),
                                      actual_rm_cols=c("pred","pred_type"),
                                      nbd_metric="precomputed", 
                                      dist_mat=sim79_lurf_urfp_allSamples,
                                      lambda="lambda.1se", probability_data=T,
                                      knn="kmax",
                                      prob_cols_vec=prob_colnames, 
                                      prob_df=sim79_lurfProb_allSamples)
# info stored for each sample
names(sim79_testSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 
 
sim79_testSamples_localNPDR$pred[[1]]$melted_df
#   class pred    prob_0    prob_1 total_local_score variable  local_score        sign
# 1     1    1 0.3856111 0.6143889        -0.1711503    var67 -0.004235721 Contradicts
# 2     1    1 0.3856111 0.6143889        -0.1711503    var14  0.006671372    Supports
# 3     1    1 0.3856111 0.6143889        -0.1711503  intvar8  0.007771316    Supports
# 4     1    1 0.3856111 0.6143889        -0.1711503    var64  0.008430727    Supports
# 5     1    1 0.3856111 0.6143889        -0.1711503  intvar7  0.023055346    Supports
# 6     1    1 0.3856111 0.6143889        -0.1711503 mainvar9  0.027409602    Supports
# 7     1    1 0.3856111 0.6143889        -0.1711503 mainvar8 -0.075283222 Contradicts
# 8     1    1 0.3856111 0.6143889        -0.1711503 mainvar7 -0.164969696 Contradicts
sim79_testSamples_localNPDR$pred[[1]]$total_local_score
# [1] -0.1711503

# compare to score with actual class label
sim79_testSamples_localNPDR$opposite[[1]]$melted_df
#   pseudo_class pred    prob_0    prob_1 total_local_score variable  local_score        sign
# 1            0    1 0.3856111 0.6143889        -0.1050991    var67 -0.002451114 Contradicts
# 2            0    1 0.3856111 0.6143889        -0.1050991    var14  0.004013088    Supports
# 3            0    1 0.3856111 0.6143889        -0.1050991  intvar8  0.004567492    Supports
# 4            0    1 0.3856111 0.6143889        -0.1050991    var64  0.006166380    Supports
# 5            0    1 0.3856111 0.6143889        -0.1050991  intvar7  0.013802502    Supports
# 6            0    1 0.3856111 0.6143889        -0.1050991 mainvar9  0.016939372    Supports
# 7            0    1 0.3856111 0.6143889        -0.1050991 mainvar8 -0.047155296 Contradicts
# 8            0    1 0.3856111 0.6143889        -0.1050991 mainvar7 -0.100981541 Contradicts
sim79_testSamples_localNPDR$opposite[[1]]$total_local_score
# [1] -0.1050991 # slightly less negative but about the same

# write these lists to file
#saveRDS(sim79_testSamples_localNPDR$pred,"sim79_localNPDR_pred_testSamples.rds")
#saveRDS(sim79_testSamples_localNPDR$opposite,"sim79_localNPDR_opposite_testSamples.rds")


# sim85
sim85_testSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim85_lurf_local, 
                                                    test_idx_vec=sim85_test_idx, 
                                                    pred_colname="pred", 
                                                    actual_colname="class", pred_rm_cols=c("class","pred_type"),
                                                    actual_rm_cols=c("pred","pred_type"),
                                                    nbd_metric="precomputed", dist_mat=sim85_lurf_urfp_allSamples,
                                                    lambda="lambda.1se", probability_data=T,
                                                    prob_cols_vec=prob_colnames, prob_df=sim85_lurfProb_allSamples)
sim85_testSamples_localNPDR$pred[[1]]$melted_df
#    class pred   prob_0   prob_1 total_local_score  variable local_score        sign
# 1      1    1 0.161594 0.838406         0.3405985   intvar4 -0.00222114 Contradicts
# 2      1    1 0.161594 0.838406         0.3405985  mainvar6 -0.03157080 Contradicts
# 3      1    1 0.161594 0.838406         0.3405985  mainvar8 -0.04234571 Contradicts
# 4      1    1 0.161594 0.838406         0.3405985  mainvar3  0.04976781    Supports
# 5      1    1 0.161594 0.838406         0.3405985  mainvar9  0.05241669    Supports
# 6      1    1 0.161594 0.838406         0.3405985 mainvar10  0.05620852    Supports
# 7      1    1 0.161594 0.838406         0.3405985  mainvar7  0.05663776    Supports
# 8      1    1 0.161594 0.838406         0.3405985  mainvar1  0.05853253    Supports
# 9      1    1 0.161594 0.838406         0.3405985  mainvar4  0.06882939    Supports
# 10     1    1 0.161594 0.838406         0.3405985  mainvar5  0.07434347    Supports
sim85_testSamples_localNPDR$pred[[1]]$total_local_score
# [1] 0.3405985

# compare to score with actual class label
sim85_testSamples_localNPDR$opposite[[1]]$melted_df
#     pseudo_class pred   prob_0   prob_1 total_local_score  variable  local_score        sign
# 1             0    1 0.161594 0.838406         0.5053735   intvar4 -0.003453451 Contradicts
# 2             0    1 0.161594 0.838406         0.5053735  mainvar6 -0.046888044 Contradicts
# 3             0    1 0.161594 0.838406         0.5053735  mainvar8 -0.062792390 Contradicts
# 4             0    1 0.161594 0.838406         0.5053735  mainvar3  0.074024860    Supports
# 5             0    1 0.161594 0.838406         0.5053735  mainvar9  0.077236584    Supports
# 6             0    1 0.161594 0.838406         0.5053735 mainvar10  0.083329506    Supports
# 7             0    1 0.161594 0.838406         0.5053735  mainvar7  0.084089762    Supports
# 8             0    1 0.161594 0.838406         0.5053735  mainvar1  0.086844049    Supports
# 9             0    1 0.161594 0.838406         0.5053735  mainvar4  0.102339930    Supports
# 10            0    1 0.161594 0.838406         0.5053735  mainvar5  0.110642685    Supports
sim85_testSamples_localNPDR$opposite[[1]]$total_local_score
# [1] 0.5053735 # increases even more!

# write these lists to file
#saveRDS(sim85_testSamples_localNPDR$pred,"sim85_localNPDR_pred_testSamples.rds")
#saveRDS(sim85_testSamples_localNPDR$opposite,"sim85_localNPDR_opposite_testSamples.rds")


#### do the same for the training data
# sim77
sim77_trainSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim77_lurf_local, 
                                                             test_idx_vec=sim77_train_idx, 
                                                             pred_colname="pred", 
                                                             actual_colname="class", pred_rm_cols=c("class","pred_type"),
                                                             actual_rm_cols=c("pred","pred_type"),
                                                             nbd_metric="precomputed", dist_mat=sim77_lurf_urfp_allSamples,
                                                             lambda="lambda.1se", probability_data=T, knn="kmax",
                                                             prob_cols_vec=prob_colnames, 
                                                             prob_df=sim77_lurfProb_allSamples)
# info stored for each sample
names(sim77_trainSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 
sim77_trainSamples_localNPDR$pred[[1]]$melted_df
#
sim77_trainSamples_localNPDR$pred[[1]]$total_local_score
# 

# compare to score with actual class label
sim77_trainSamples_localNPDR$opposite[[1]]$melted_df
#
sim77_trainSamples_localNPDR$opposite[[1]]$total_local_score
#

# write these lists to file
#saveRDS(sim77_trainSamples_localNPDR$pred,"sim77_localNPDR_pred_trainSamples.rds")
#saveRDS(sim77_trainSamples_localNPDR$opposite,"sim77_localNPDR_opposite_trainSamples.rds")

# sim79
sim79_trainSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim79_lurf_local, 
                                                              test_idx_vec=sim79_train_idx, 
                                                              pred_colname="pred", 
                                                              actual_colname="class", pred_rm_cols=c("class","pred_type"),
                                                              actual_rm_cols=c("pred","pred_type"),
                                                              nbd_metric="precomputed", 
                                                              dist_mat=sim79_lurf_urfp_allSamples,
                                                              lambda="lambda.1se", 
                                                              probability_data=T, knn="kmax",
                                                              prob_cols_vec=prob_colnames, 
                                                              prob_df=sim79_lurfProb_allSamples)
# info stored for each sample
names(sim79_trainSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 
sim79_trainSamples_localNPDR$pred[[1]]$melted_df
#
sim79_trainSamples_localNPDR$pred[[1]]$total_local_score
#

# compare to score with actual class label
sim79_trainSamples_localNPDR$opposite[[1]]$melted_df
# 
sim79_trainSamples_localNPDR$opposite[[1]]$total_local_score
#

# write these lists to file
#saveRDS(sim79_trainSamples_localNPDR$pred,"sim79_localNPDR_pred_trainSamples.rds")
#saveRDS(sim79_trainSamples_localNPDR$opposite,"sim79_localNPDR_opposite_trainSamples.rds")


# sim85
sim85_trainSamples_localNPDR <- localNPDR_predOpposite_scores(data=sim85_lurf_local, 
                                                              test_idx_vec=sim85_train_idx, 
                                                              pred_colname="pred", 
                                                              actual_colname="class", pred_rm_cols=c("class","pred_type"),
                                                              actual_rm_cols=c("pred","pred_type"),
                                                              nbd_metric="precomputed", 
                                                              dist_mat=sim85_lurf_urfp_allSamples,
                                                              lambda="lambda.1se", 
                                                              probability_data=T, knn="kmax",
                                                              prob_cols_vec=prob_colnames, 
                                                              prob_df=sim85_lurfProb_allSamples)
# info stored for each sample
names(sim85_trainSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 

sim85_trainSamples_localNPDR$pred[[1]]$melted_df
#
sim85_trainSamples_localNPDR$pred[[1]]$total_local_score
# 

# compare to score with actual class label
sim85_trainSamples_localNPDR$opposite[[1]]$melted_df
sim85_trainSamples_localNPDR$opposite[[1]]$total_local_score
# 

# write these lists to file
#saveRDS(sim85_trainSamples_localNPDR$pred,"sim85_localNPDR_pred_trainSamples.rds")
#saveRDS(sim85_trainSamples_localNPDR$opposite,"sim85_localNPDR_opposite_trainSamples.rds")

sim85_trainSamples_localNPDR_pred <- readRDS("./data/sim85/sim85_localNPDR_pred_trainSamples.rds") 
sim85_testSamples_localNPDR_pred <- readRDS("./data/sim85/sim85_localNPDR_pred_testSamples.rds")

sim77_trainSamples_localNPDR_pred <- readRDS("./data/sim77/sim77_localNPDR_pred_trainSamples.rds") 
sim77_testSamples_localNPDR_pred <- readRDS("./data/sim77/sim77_localNPDR_pred_testSamples.rds")

sim79_trainSamples_localNPDR_pred <- readRDS("./data/sim79/sim79_localNPDR_pred_trainSamples.rds") 
sim79_testSamples_localNPDR_pred <- readRDS("./data/sim79/sim79_localNPDR_pred_testSamples.rds")


sim77_trainSamples_localNPDR_opposite <- readRDS("./data/sim77/sim77_localNPDR_opposite_trainSamples.rds")
sim77_testSamples_localNPDR_opposite <- readRDS("./data/sim77/sim77_localNPDR_opposite_testSamples.rds")

sim79_trainSamples_localNPDR_opposite <- readRDS("./data/sim79/sim79_localNPDR_opposite_trainSamples.rds")
sim79_testSamples_localNPDR_opposite <- readRDS("./data/sim79/sim79_localNPDR_opposite_testSamples.rds")


sim85_trainSamples_localNPDR_opposite <- readRDS("./data/sim85/sim85_localNPDR_opposite_trainSamples.rds")
sim85_testSamples_localNPDR_opposite <- readRDS("./data/sim85/sim85_localNPDR_opposite_testSamples.rds")


### add results to final df for analysis
# sim77
sim77_final_local_df <- final_local_data(local_train_list=sim77_trainSamples_localNPDR_pred,
                                         local_test_list = sim77_testSamples_localNPDR_pred,
                                         opposite_local_train_list = sim77_trainSamples_localNPDR_opposite,
                                         opposite_local_test_list = sim77_testSamples_localNPDR_opposite, 
                                         positive_class=0, 
                                         class_colname = "class", 
                                         class_names = c("1","0"),
                                         opp_class_colname = "pseudo_class",
                                         pred_colname = "pred",
                                         prob_colnames = c("prob_0","prob_1"),
                                         train_idx_vec = sim77_train_idx, 
                                         test_idx_vec = sim77_test_idx,
                                         lambda = "lambda.1se",
                                         id_vec = NULL)
head(sim77_final_local_df)[,1:11]
# id train_test pred_type class pred prob_0 prob_1 knn nbd_bal lambda.1se       intvar3
# 1  1      train        FP     1    0 0.8104 0.1896 299  0.5151  4.3731109 -1.358925e-03
# 2  2      train        TN     1    1 0.4420 0.5580 299  0.4883  3.7219617  1.069035e-02
# 3  3      train        TN     1    1 0.3515 0.6485 299  0.4883  3.7225509 -2.250416e-03
# 4  4      train        TN     1    1 0.4443 0.5557 299  0.4883  4.4821241  9.259487e-03
# 5  5      train        FP     1    0 0.6286 0.3714 299  0.5151  0.3276956 -7.038122e-02
# 6  6      train        FP     1    0 0.5154 0.4846 299  0.5151  2.5314426  9.171402e-05

colnames(sim77_final_local_df)
# [1] "id"                    "train_test"            "pred_type"             "class"                
# [5] "pred"                  "prob_0"                "prob_1"                "knn"                  
# [9] "nbd_bal"               "lambda.1se"            "intvar3"               "var64"                
# [13] "intvar8"               "var35"                 "mainvar1"              "mainvar2"             
# [17] "mainvar8"              "mainvar9"              "mainvar4"              "total_local_score"    
# [21] "opp_class"             "opp_lambda.1se"        "opp_intvar3"           "opp_var64"            
# [25] "opp_intvar8"           "opp_var35"             "opp_mainvar1"          "opp_mainvar2"         
# [29] "opp_mainvar8"          "opp_mainvar9"          "opp_mainvar4"          "opp_total_local_score"

# sim79
sim79_final_local_df <- final_local_data(local_train_list=sim79_trainSamples_localNPDR_pred,
                                         local_test_list = sim79_testSamples_localNPDR_pred,
                                         opposite_local_train_list = sim79_trainSamples_localNPDR_opposite,
                                         opposite_local_test_list = sim79_testSamples_localNPDR_opposite, 
                                         positive_class="0", 
                                         class_colname = "class", 
                                         class_names = c("1","0"),
                                         opp_class_colname = "pseudo_class",
                                         pred_colname = "pred",
                                         prob_colnames = c("prob_0","prob_1"),
                                         train_idx_vec = sim79_train_idx, 
                                         test_idx_vec = sim79_test_idx,
                                         lambda = "lambda.1se",
                                         id_vec = NULL)
head(sim79_final_local_df)[,1:11]
#id train_test pred_type class pred prob_0 prob_1 knn nbd_bal lambda.1se        var14
# 1  1      train        FP     1    0 0.7165 0.2835 299  0.4415  3.0903344 -0.004171403
# 2  2      train        FP     1    0 0.5761 0.4239 299  0.4415  0.9246775 -0.006081897
# 3  3      train        FP     1    0 0.5218 0.4782 299  0.4415  0.3938788 -0.015340110
# 4  4      train        FP     1    0 0.7922 0.2078 299  0.4415  1.0916318  0.002004532
# 5  5      train        FP     1    0 0.7452 0.2548 299  0.4415  0.3297412  0.012521761
# 6  6      train        FP     1    0 0.5039 0.4961 299  0.4415  0.6909560 -0.018723878

# sim85
sim85_final_local_df <- final_local_data(local_train_list=sim85_trainSamples_localNPDR_pred,
                                         local_test_list = sim85_testSamples_localNPDR_pred,
                        opposite_local_train_list = sim85_trainSamples_localNPDR_opposite,
                        opposite_local_test_list = sim85_testSamples_localNPDR_opposite, 
                        positive_class="0", 
                        class_colname = "class", 
                        class_names = c("1","0"),
                        opp_class_colname = "pseudo_class",
                        pred_colname = "pred",
                        prob_colnames = c("prob_0","prob_1"),
                        train_idx_vec = sim85_train_idx, 
                        test_idx_vec = sim85_test_idx,
                        lambda = "lambda.1se",
                        id_vec = NULL)
head(sim85_final_local_df)[,1:11]
#id train_test pred_type class pred prob_0 prob_1 knn nbd_bal lambda.1se       intvar4
# 1  1      train        TN     1    1 0.3166 0.6834 499   0.507  2.8360566  1.173569e-03
# 2  2      train        TN     1    1 0.2430 0.7570 499   0.507  2.8284170  2.758388e-05
# 3  3      train        TN     1    1 0.2592 0.7408 499   0.507  4.3882358  6.372980e-03
# 4  4      train        TN     1    1 0.4343 0.5657 499   0.507  0.9705588 -5.753545e-03
# 5  5      train        FP     1    0 0.7264 0.2736 499   0.495  5.5267241 -8.192016e-03
# 6  6      train        TN     1    1 0.4086 0.5914 499   0.507  0.7977002  1.625936e-02

# save to csv
#write.table(sim77_final_local_df,"sim77_final_local_data2.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_final_local_df,"sim79_final_local_data2.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_final_local_df,"sim85_final_local_data2.csv",row.names=F,quote=F,sep=",")


