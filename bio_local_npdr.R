# install npdr, npdro if not installed
#library(devtools)
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
library(viridis)
library(hrbrthemes)
# setwd to dir that this file is in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# load functions
source("local_npdr_functions.R")
###########################
#set up parallel processing 
registerDoParallel(cores=4)
###########################

#####################
# 1. read in IRMS data
#####################

# try biosignature data
bio_train_lurfRF_pred <- read.csv("./data/bio/bio_trainPred_lurf0.csv")
bio_test_lurfRF_pred <- read.csv("./data/bio/bio_testPred_lurf0.csv")
# combine
bio_lurf_pred_allSamples <- rbind.data.frame(bio_train_lurfRF_pred, bio_test_lurfRF_pred)

# probability data
bio_lurfProb_allSamples <- read.csv("./data/bio/bio_lurf_prob_allSamples.csv")
head(bio_lurfProb_allSamples)

# need distance matrix in lurf space
# cols to remove for urfp dist
rm_bio_cols <- which(colnames(bio_lurf_pred_allSamples) %in% c("Analysis","biotic","pred"))

bio_lurf_urfp <- urfp_dist(bio_lurf_pred_allSamples,rm.ind = rm_bio_cols)
dim(bio_lurf_urfp)
# [1] 174 174

# write to file
#write.table(bio_lurf_urfp,"bio_lurf_urfp_dist.csv",row.names=F,quote=F,sep=",")
bio_lurf_urfp <- read.csv("./data/bio/bio_lurf_urfp_dist.csv")

######################################
# 2. label prediction types: FP/FN/TP/TN
######################################
bio_lurf_tf_train <- true_false_data(bio_train_lurfRF_pred,pos_class="biotic",
                               class_colname="biotic", pred_colname="pred")

bio_lurf_tf_test <- true_false_data(bio_test_lurfRF_pred,pos_class="biotic",
                                     class_colname="biotic",pred_colname="pred")

# label training data
bio_predType_df <- local_truePred_df(trainPred_tf_df = bio_lurf_tf_train,
                                           testPred_tf_df = bio_lurf_tf_test, 
                                           class_colname="biotic",
                                           pred_colname="pred")
head(bio_predType_df)
#   Analysis diff2_acf1 time_kl_shift fluctanal_prop_r1 avg_rR45CO244CO2  sd_d18O13C biotic   pred pred_type
# 1     2961  0.7343175           133         0.4081633         1.178875 0.003179166 biotic biotic     train
# 2     2962  0.7091780           133         0.4693878         1.180019 0.009839738 biotic biotic     train
# 3     2963  0.7348665           133         0.4081633         1.178937 0.002773175 biotic biotic     train
# 4     2965  0.7241026           133         0.4081633         1.176910 0.003006303 biotic biotic     train
# 5     2966  0.7023151           133         0.4897959         1.179907 0.007840798 biotic biotic     train
# 6     2968  0.7374716           133         0.4081633         1.179628 0.004168266 biotic biotic     train
tail(bio_predType_df)
#     Analysis diff2_acf1 time_kl_shift fluctanal_prop_r1 avg_rR45CO244CO2  sd_d18O13C  biotic    pred pred_type
# 169     5929  0.7410574           133         0.4285714         1.179414 0.003302264  biotic  biotic        TP
# 170     5939  0.7405218           133         0.4285714         1.180668 0.003091583  biotic  biotic        TP
# 171     5944  0.7318990           323         0.4285714         1.180432 0.002238492  biotic  biotic        TP
# 172     5950  0.7510807           133         0.4285714         1.181049 0.004603391 abiotic abiotic        TN
# 173     5956  0.7474498           323         0.4285714         1.180162 0.003348692 abiotic  biotic        FP
# 174     5959  0.7377971           133         0.4285714         1.179716 0.002888099  biotic  biotic        TP


# write to file
#write.table(bio_predType_df,"bio_predType_data.csv",row.names=F,quote=F,sep=",")
dim(bio_predType_df)

bio_predType_df <- read.csv("./data/bio/bio_predType_data.csv")

bio_train_idx <- seq(1,length(bio_train_lurfRF_pred$biotic))
bio_test_idx <- seq(length(bio_train_lurfRF_pred$biotic)+1,length(bio_predType_df$biotic))
#######################################################
# 3. local-NPDR loop to analyze TLS for T/F predictions
########################################################
colnames(bio_predType_df)
# [1] "Analysis"          "diff2_acf1"        "time_kl_shift"     "fluctanal_prop_r1" "avg_rR45CO244CO2" 
# [6] "sd_d18O13C"        "biotic"            "pred"              "pred_type"   
analysis_vec <- bio_predType_df$Analysis

# Brett's function for violin plots and t-test 
# training data
bio_localNPDR_train_tf <- localNPDR_predTF_scores(pred_data=bio_predType_df, 
                                                  test_idx_vec=bio_train_idx,
                                                  pred_colname="pred", class_colname="biotic",
                                                  dist_mat=bio_lurf_urfp, lambda="lambda.1se",
                                                  nbd_metric="precomputed", knn="kmax", 
                                                  rm_cols = c("pred_type","Analysis","biotic"), verbose=T)
# no warnings
bio_train_true_vec = bio_localNPDR_train_tf$true_vec
length(bio_train_true_vec)
# [1] 127
head(bio_train_true_vec)
#  [1] 119.15395 -57.23000 103.19299 286.05470 -54.45366 -24.06097
bio_train_false_vec = bio_localNPDR_train_tf$false_vec
head(bio_train_false_vec)
# [1]  101.70499   68.78833  388.36659  -31.37521 -131.49982  253.85295
length(bio_train_false_vec)
# [1] 13

t.test(bio_train_true_vec, bio_train_false_vec) 
# # Welch Two Sample t-test
# 
# data:  bio_train_true_vec and bio_train_false_vec
# t = -1.2498, df = 13.819, p-value = 0.2321
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -196.8067   51.9995
# sample estimates:
#   mean of x mean of y 
# 17.55600  89.95959



#### violin plots
# bio_train data
bio_train_local_df <- data.frame(local_scores = c(bio_train_true_vec,bio_train_false_vec))
group_factor = factor(c(rep("True",length(bio_train_true_vec)),
                        rep("False",length(bio_train_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
bio_train_local_df$group = group_factor
vioplot(local_scores~group,data=bio_train_local_df,col=c("blue","red"),ylab="local scores", xlab="", 
        main="biosignature data local scores: train data")


# bio_test data
bio_test_local_df <- data.frame(local_scores = c(bio_test_true_vec,bio_test_false_vec))
group_factor = factor(c(rep("True",length(bio_test_true_vec)),
                         rep("False",length(bio_test_false_vec))))
group_factor=factor(group_factor, levels=c("True", "False"))
bio_test_local_df$group = group_factor
vioplot(local_scores~group,data=bio_test_local_df,col=c("blue","red"),ylab="local scores", xlab="", 
         main="biosignature data local scores: test data")



#### look at some individual samples
#bio_FN_idx <- 166
#bio_TP_idx <- 169
#bio_TN_idx <- 172
#bio_FP_idx <- 173


#bio_test_idx_vec <- c(bio_FN_idx,bio_TP_idx,bio_TN_idx,bio_FP_idx)
prob_colnames <- c("prob_abiotic","prob_biotic")



bio_test_localNPDR <- localNPDR_actualPred_scores(data=bio_predType_df, 
                                                    test_idx_vec=bio_test_idx, 
                                                    pred_colname="pred", 
                                                    actual_colname="biotic", 
                                                    pred_rm_cols=c("Analysis","biotic","pred_type"),
                                                    actual_rm_cols=c("Analysis","pred","pred_type"),
                                                    nbd_metric="precomputed", 
                                                    dist_mat=bio_lurf_urfp,
                                                    lambda="lambda.1se", probability_data=T,
                                                    knn="kmax",
                                                    prob_cols_vec=prob_colnames, 
                                                    prob_df=bio_lurfProb_allSamples)
bio_test_localNPDR$pred[[1]]$melted_df
#   biotic    pred prob_abiotic prob_biotic total_local_score          variable   local_score        sign
# 1 biotic abiotic     0.632872    0.367128         -67.56235     time_kl_shift  2.435979e-05    Supports
# 2 biotic abiotic     0.632872    0.367128         -67.56235        diff2_acf1 -3.575741e+00 Contradicts
# 3 biotic abiotic     0.632872    0.367128         -67.56235        sd_d18O13C  4.089409e+00    Supports
# 4 biotic abiotic     0.632872    0.367128         -67.56235 fluctanal_prop_r1 -4.386212e+00 Contradicts
# 5 biotic abiotic     0.632872    0.367128         -67.56235  avg_rR45CO244CO2 -6.368983e+01 Contradicts

bio_test_localNPDR$pred[[1]]$total_local_score
#[1] -67.56235

# compare to score with actual class label
bio_test_localNPDR$actual[[1]]$total_local_score
# [1] 5.599781






## plot 
# var order
bio_lurf_feat <- c("avg_rR45CO244CO2","sd_d18O13C","diff2_acf1","fluctanal_prop_r1","time_kl_shift")
# FN
bio_FN_p <- local_importance_plot(melted_df=bio_test_localNPDR$pred[[1]]$melted_df, 
                                       actual_colname="biotic",
                                       class_names = c("abiotic","biotic"),
                                       pred_colname="pred",
                                       main_title="Single-sample variable importance for NPDR-LURF selected features",
                                       analysis=bio_predType_df$Analysis[bio_test_idx_vec][1], 
                                       caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnMax",
                                       var_order=bio_lurf_feat)
bio_FN_p

# TP
bio_TP_p <- local_importance_plot(melted_df=bio_test_localNPDR$pred[[2]]$melted_df, 
                                  actual_colname="biotic",
                                  class_names = c("abiotic","biotic"),
                                  pred_colname="pred",
                                  main_title="Single-sample variable importance for NPDR-LURF selected features",
                                  analysis=bio_predType_df$Analysis[bio_test_idx_vec][2], 
                                  caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnMax",
                                  var_order=bio_lurf_feat)
bio_TP_p

# TN
bio_TN_p <- local_importance_plot(melted_df=bio_test_localNPDR$pred[[3]]$melted_df, 
                                  actual_colname="biotic",
                                  class_names = c("abiotic","biotic"),
                                  pred_colname="pred",
                                  main_title="Single-sample variable importance for NPDR-LURF selected features",
                                  analysis=bio_predType_df$Analysis[bio_test_idx_vec][3], 
                                  caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnMax",
                                  var_order=bio_lurf_feat)
bio_TN_p

# FP
bio_FP_p <- local_importance_plot(melted_df=bio_test_localNPDR$pred[[4]]$melted_df, 
                                  actual_colname="biotic",
                                  class_names = c("abiotic","biotic"),
                                  pred_colname="pred",
                                  main_title="Single-sample variable importance for NPDR-LURF selected features",
                                  analysis=bio_predType_df$Analysis[bio_test_idx_vec][4], 
                                  caption="local importance calculated using NPDR-Ridge (lambda.1se) with knnMax",
                                  var_order=bio_lurf_feat)
bio_FP_p


###############################################################
# 4. local-NPDR with individual variable scores for TP/TN/FP/FN
###############################################################
bio_trainSamples_localNPDR <- localNPDR_predOpposite_scores(bio_predType_df, 
                              test_idx_vec=bio_train_idx, 
                              pred_colname="pred", 
                              actual_colname="biotic", pred_rm_cols=c("Analysis","biotic","pred_type"),
                              actual_rm_cols=c("Analysis","pred","pred_type"),
                              nbd_metric="precomputed", dist_mat=bio_lurf_urfp,
                              lambda="lambda.1se", probability_data=T, knn="kmax",
                              prob_cols_vec=prob_colnames, 
                              prob_df=bio_lurfProb_allSamples)
# info stored for each sample
names(bio_trainSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 

bio_trainSamples_localNPDR$pred[[1]]$melted_df
#   biotic   pred prob_abiotic prob_biotic total_local_score          variable   local_score        sign
# 1 biotic biotic   0.03295293   0.9670471          237.9183     time_kl_shift -1.241192e-05 Contradicts
# 2 biotic biotic   0.03295293   0.9670471          237.9183 fluctanal_prop_r1  6.408400e+00    Supports
# 3 biotic biotic   0.03295293   0.9670471          237.9183        diff2_acf1  1.590219e+01    Supports
# 4 biotic biotic   0.03295293   0.9670471          237.9183        sd_d18O13C -2.969981e+01 Contradicts
# 5 biotic biotic   0.03295293   0.9670471          237.9183  avg_rR45CO244CO2  2.453075e+02    Supports
bio_trainSamples_localNPDR$pred[[1]]$total_local_score
#[1] 237.9183
# compare to score with actual class label
bio_trainSamples_localNPDR$opposite[[1]]$melted_df
#pseudo_biotic   pred prob_abiotic prob_biotic total_local_score          variable   local_score        sign
# 1       abiotic biotic   0.03295293   0.9670471          177.3371     time_kl_shift -1.374366e-05 Contradicts
# 2       abiotic biotic   0.03295293   0.9670471          177.3371 fluctanal_prop_r1  5.090617e+00    Supports
# 3       abiotic biotic   0.03295293   0.9670471          177.3371        diff2_acf1  1.231224e+01    Supports
# 4       abiotic biotic   0.03295293   0.9670471          177.3371        sd_d18O13C -1.862253e+01 Contradicts
# 5       abiotic biotic   0.03295293   0.9670471          177.3371  avg_rR45CO244CO2  1.785568e+02    Supports
bio_trainSamples_localNPDR$opposite[[1]]$total_local_score
# [1] 177.3371

# write these lists to file
#saveRDS(bio_trainSamples_localNPDR$pred,"bioLURF_localNPDR_pred_trainSamples.rds")
#saveRDS(bio_trainSamples_localNPDR$opposite,"bioLURF_localNPDR_opposite_trainSamples.rds")

## do for test data
bio_testSamples_localNPDR <- localNPDR_predOpposite_scores(bio_predType_df, 
                                                            test_idx_vec=bio_test_idx, 
                                                            pred_colname="pred", 
                                                            actual_colname="biotic", pred_rm_cols=c("Analysis","biotic","pred_type"),
                                                            actual_rm_cols=c("Analysis","pred","pred_type"),
                                                            nbd_metric="precomputed", dist_mat=bio_lurf_urfp,
                                                            lambda="lambda.1se", probability_data=T, knn="kmax",
                                                            prob_cols_vec=prob_colnames, 
                                                            prob_df=bio_lurfProb_allSamples)
# info stored for each sample
names(bio_testSamples_localNPDR$pred[[1]])
# [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
# [6] "melted_df" 

bio_testSamples_localNPDR$pred[[1]]$melted_df
# biotic   pred prob_abiotic prob_biotic total_local_score          variable   local_score        sign
# 1 biotic biotic   0.01231535   0.9876847          402.1765     time_kl_shift  1.192324e-04    Supports
# 2 biotic biotic   0.01231535   0.9876847          402.1765 fluctanal_prop_r1  1.617201e+01    Supports
# 3 biotic biotic   0.01231535   0.9876847          402.1765        diff2_acf1  2.446221e+01    Supports
# 4 biotic biotic   0.01231535   0.9876847          402.1765        sd_d18O13C -1.288315e+02 Contradicts
# 5 biotic biotic   0.01231535   0.9876847          402.1765  avg_rR45CO244CO2  4.903736e+02    Supports
bio_testSamples_localNPDR$pred[[1]]$total_local_score
# [1] 402.1765
# compare to score with actual class label
bio_testSamples_localNPDR$opposite[[1]]$melted_df
#  pseudo_biotic   pred prob_abiotic prob_biotic total_local_score          variable   local_score        sign
# 1       abiotic biotic   0.01231535   0.9876847          39.89078     time_kl_shift -5.332548e-06 Contradicts
# 2       abiotic biotic   0.01231535   0.9876847          39.89078 fluctanal_prop_r1  1.351302e+00    Supports
# 3       abiotic biotic   0.01231535   0.9876847          39.89078        sd_d18O13C -1.714158e+00 Contradicts
# 4       abiotic biotic   0.01231535   0.9876847          39.89078        diff2_acf1  2.206429e+00    Supports
# 5       abiotic biotic   0.01231535   0.9876847          39.89078  avg_rR45CO244CO2  3.804721e+01    Supports
bio_testSamples_localNPDR$opposite[[1]]$total_local_score
# [1] 39.89078

# write these lists to file
#saveRDS(bio_testSamples_localNPDR$pred,"bioLURF_localNPDR_pred_testSamples.rds")
#saveRDS(bio_testSamples_localNPDR$opposite,"bioLURF_localNPDR_opposite_testSamples.rds")

### final local data
bio_final_local_df <- final_local_data(local_train_list=bio_trainSamples_localNPDR$pred,
                                         local_test_list = bio_testSamples_localNPDR$pred,
                                         opposite_local_train_list = bio_trainSamples_localNPDR$opposite,
                                         opposite_local_test_list = bio_testSamples_localNPDR$opposite, 
                                         positive_class="biotic", 
                                         class_colname = "biotic", 
                                         class_names = c("biotic","abiotic"),
                                         opp_class_colname = "pseudo_biotic",
                                         pred_colname = "pred",
                                         prob_colnames = c("prob_abiotic","prob_biotic"),
                                         train_idx_vec = bio_train_idx, 
                                         test_idx_vec = bio_test_idx,
                                         lambda = "lambda.1se",
                                         id_vec = analysis_vec)
head(bio_final_local_df)[,1:11]
#     id train_test pred_type  class   pred prob_abiotic prob_biotic knn nbd_bal        lambda.1se         time_kl_shift
# 1 2961      train        TP biotic biotic        0.033       0.967 173  0.6301 0.349291045908087 -1.24119214575224e-05
# 2 2962      train        TP biotic biotic       0.1816      0.8184 173  0.6301 0.811383984446425 -1.98801864310437e-05
# 3 2963      train        TP biotic biotic        0.028       0.972 173  0.6301 0.813648090328585 -1.30877514128821e-05
# 4 2965      train        TP biotic biotic       0.0124      0.9876 173  0.6301  1.30941743960493 -7.24851915772585e-06
# 5 2966      train        TP biotic biotic       0.3101      0.6899 173  0.6301  1.87905067854891 -9.40405969105189e-06
# 6 2968      train        TP biotic biotic       0.1353      0.8647 173  0.6301  2.56249521360733 -6.62965821836362e-06
colnames(bio_final_local_df)
# [1] "id"                    "train_test"            "pred_type"             "class"                 "pred"                 
# [6] "prob_abiotic"          "prob_biotic"           "knn"                   "nbd_bal"               "lambda.1se"           
# [11] "time_kl_shift"         "fluctanal_prop_r1"     "diff2_acf1"            "sd_d18O13C"            "avg_rR45CO244CO2"     
# [16] "total_local_score"     "opp_class"             "opp_lambda.1se"        "opp_time_kl_shift"     "opp_fluctanal_prop_r1"
# [21] "opp_diff2_acf1"        "opp_sd_d18O13C"        "opp_avg_rR45CO244CO2"  "opp_total_local_score"

# save to csv
#write.table(bio_final_local_df,"bio_final_local_data.csv",row.names=F,quote=F,sep=",")

bio_final_local_df <- read.csv("./data/bio/bio_final_local_data.csv")

###### bio FPs
bio_FP_local_vars <- bio_final_local_df %>%  
  filter(pred_type=="FP") %>% select(id, train_test, pred_type,pred,
                                     prob_abiotic, prob_biotic,
                                     all_of(bio_lurf_feat))
head(bio_FP_local_vars)
#  id train_test pred_type   pred prob_abiotic prob_biotic avg_rR45CO244CO2 sd_d18O13C
# 1 3240      train        FP biotic       0.3377      0.6623        191.68273  10.008847
# 2 5900      train        FP biotic       0.3343      0.6657        -24.70915   6.296869
# 3 5924      train        FP biotic       0.0205      0.9795        173.64715 -15.162373
# 4 5928      train        FP biotic       0.4068      0.5932        148.57896 -14.270253
# 5 5937      train        FP biotic       0.4238      0.5762        119.31609   7.009326
# 6 5945      train        FP biotic       0.5524      0.4476        -73.15845   7.512943
# diff2_acf1 fluctanal_prop_r1 time_kl_shift
# 1  -9.281445          6.798809 -2.288023e-05
# 2   4.057285         -3.573303 -1.837385e-05
# 3  14.258207          8.964108 -3.480919e-05
# 4  10.185302         -8.673627 -3.663145e-05
# 5  -5.403819          4.756924 -3.391613e-05
# 6  -7.088951          4.183350 -2.724686e-05

dim(bio_FP_local_vars)
# [1] 9  11

#write.table(bio_FP_local_vars,"bio_FP_localVarImp.csv",row.names=F,quote=F,sep=",")

colnames(bio_FP_local_vars)
# [1] "id"                "train_test"        "pred_type"         "pred"             
# [5] "prob_abiotic"      "prob_biotic"       "avg_rR45CO244CO2"  "sd_d18O13C"       
# [9] "diff2_acf1"        "fluctanal_prop_r1" "time_kl_shift"  

bio_FP_mean_local_vars <- sapply(bio_FP_local_vars[,7:11],function(x){mean(x)})
bio_FP_mean_local_vars
# avg_rR45CO244CO2        sd_d18O13C        diff2_acf1 fluctanal_prop_r1     time_kl_shift 
#     9.602639e+00     -1.083752e+01      2.156424e+00      4.026362e+00     -2.973212e-05

bio_FP_mean_local_df <- as.data.frame(bio_FP_mean_local_vars)
colnames(bio_FP_mean_local_df)<-c("FP_mean_local_score")
bio_FP_mean_local_df
#          FP_mean_local_score
# avg_rR45CO244CO2         9.602639e+00
# sd_d18O13C              -1.083752e+01
# diff2_acf1               2.156424e+00
# fluctanal_prop_r1        4.026362e+00
# time_kl_shift           -2.973212e-05


#### bio FNs
bio_FN_local_vars <- bio_final_local_df %>%  
  filter(pred_type=="FN") %>% select(id, train_test, pred_type,pred,
                                     prob_abiotic, prob_biotic,
                                     all_of(bio_lurf_feat))
head(bio_FN_local_vars)
dim(bio_FN_local_vars)
# [1] 7  11

#write.table(bio_FN_local_vars,"bio_FN_localVarImp.csv",row.names=F,quote=F,sep=",")

bio_FN_mean_local_vars <- sapply(bio_FN_local_vars[,7:11],function(x){mean(x)})
bio_FN_mean_local_vars
#avg_rR45CO244CO2        sd_d18O13C        diff2_acf1 fluctanal_prop_r1     time_kl_shift 
#    9.378983e+01      2.590770e+01     -1.294224e+01      1.072136e-01      4.289323e-05

bio_FN_mean_local_df <- as.data.frame(bio_FN_mean_local_vars)
colnames(bio_FN_mean_local_df)<-c("FN_mean_local_score")
bio_FN_mean_local_df
#    FN_mean_local_score
# avg_rR45CO244CO2         9.378983e+01
# sd_d18O13C               2.590770e+01
# diff2_acf1              -1.294224e+01
# fluctanal_prop_r1        1.072136e-01
# time_kl_shift            4.289323e-05

#### bio TPs
bio_TP_local_vars <- bio_final_local_df %>%  
  filter(pred_type=="TP") %>% select(id, train_test, pred_type,pred,
                                     prob_abiotic, prob_biotic,
                                     all_of(bio_lurf_feat))
head(bio_TP_local_vars)
dim(bio_TP_local_vars)
# [1] 56  11

#write.table(bio_TP_local_vars,"bio_TP_localVarImp.csv",row.names=F,quote=F,sep=",")

bio_TP_mean_local_vars <- sapply(bio_TP_local_vars[,7:11],function(x){mean(x)})
bio_TP_mean_local_vars
# avg_rR45CO244CO2        sd_d18O13C        diff2_acf1 fluctanal_prop_r1     time_kl_shift 
#     8.319624e+01     -2.359554e+01      8.493124e+00      3.597923e+00     -1.629227e-05 

bio_TP_mean_local_df <- as.data.frame(bio_TP_mean_local_vars)
colnames(bio_TP_mean_local_df)<-c("TP_mean_local_score")
bio_TP_mean_local_df
# TP_mean_local_score
# avg_rR45CO244CO2         8.319624e+01
# sd_d18O13C              -2.359554e+01
# diff2_acf1               8.493124e+00
# fluctanal_prop_r1        3.597923e+00
# time_kl_shift           -1.629227e-05


#### bio TNs
bio_TN_local_vars <- bio_final_local_df %>%  
  filter(pred_type=="TN") %>% select(id, train_test, pred_type,pred,
                                     prob_abiotic, prob_biotic,
                                     all_of(bio_lurf_feat))
head(bio_TN_local_vars)
dim(bio_TN_local_vars)
# [1] 102  11

#write.table(bio_TN_local_vars,"bio_TN_localVarImp.csv",row.names=F,quote=F,sep=",")

bio_TN_mean_local_vars <- sapply(bio_TN_local_vars[,7:11],function(x){mean(x)})
bio_TN_mean_local_vars
# avg_rR45CO244CO2        sd_d18O13C        diff2_acf1 fluctanal_prop_r1     time_kl_shift 
# -4.970760e+01      1.953133e+01      6.044101e-01     -1.840707e+00      2.618938e-05  

bio_TN_mean_local_df <- as.data.frame(bio_TN_mean_local_vars)
colnames(bio_TN_mean_local_df)<-c("TN_mean_local_score")
bio_TN_mean_local_df
#          TN_mean_local_score
# avg_rR45CO244CO2        -4.970760e+01
# sd_d18O13C               1.953133e+01
# diff2_acf1               6.044101e-01
# fluctanal_prop_r1       -1.840707e+00
# time_kl_shift            2.618938e-05

## combine bio mean var scores
bio_pred_local_mean_df <- cbind.data.frame(
  bio_TP_mean_local_df,
  bio_TN_mean_local_df,
  bio_FP_mean_local_df,
  bio_FN_mean_local_df)
bio_pred_local_mean_df$variable <- rownames(bio_pred_local_mean_df)
bio_pred_local_mean_df
#   TP_mean_local_score TN_mean_local_score FP_mean_local_score FN_mean_local_score
# avg_rR45CO244CO2         8.319624e+01       -4.970760e+01        9.602639e+00        9.378983e+01
# sd_d18O13C              -2.359554e+01        1.953133e+01       -1.083752e+01        2.590770e+01
# diff2_acf1               8.493124e+00        6.044101e-01        2.156424e+00       -1.294224e+01
# fluctanal_prop_r1        3.597923e+00       -1.840707e+00        4.026362e+00        1.072136e-01
# time_kl_shift           -1.629227e-05        2.618938e-05       -2.973212e-05        4.289323e-05
#                            variable
# avg_rR45CO244CO2   avg_rR45CO244CO2
# sd_d18O13C               sd_d18O13C
# diff2_acf1               diff2_acf1
# fluctanal_prop_r1 fluctanal_prop_r1
# time_kl_shift         time_kl_shift

# save to file
#write.table(bio_pred_local_mean_df,"bio_mean_localVarImp.csv",row.names=F,quote=F,sep=",")

###############################################################
# 5. bar plot of local-NPDR variable averages for TP/TN/FP/FN
#############################################################

bio_bar_dat <- melt(bio_pred_local_mean_df,id="variable")
head(bio_bar_dat)
colnames(bio_bar_dat)<-c("variable","condition","value")
head(bio_bar_dat)
#            variable           condition         value
# 1  avg_rR45CO244CO2 TP_mean_local_score  8.319624e+01
# 2        sd_d18O13C TP_mean_local_score -2.359554e+01
# 3        diff2_acf1 TP_mean_local_score  8.493124e+00
# 4 fluctanal_prop_r1 TP_mean_local_score  3.597923e+00
# 5     time_kl_shift TP_mean_local_score -1.629227e-05
# 6  avg_rR45CO244CO2 TN_mean_local_score -4.970760e+01


# Graph
ggplot(bio_bar_dat, aes(fill=variable, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Average total local importance by prediction type: biosignature data") +
  facet_wrap(~condition) +
  #theme_ipsum() +
  theme(legend.position="none") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  xlab("")
