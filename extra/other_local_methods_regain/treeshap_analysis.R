library(npdr)
library(igraph)
library(umap)
library(ggplot2)
library(dplyr)
library(treeshap)
library(ranger)

make_pred_TF_vec <- function(pred_vec, class_vec, positive_class){
  # pred: vector of predictions
  # class: vector of true classes
  # positive_class: the class that is considered positive
  # returns a vector of prediction types: TP, TN, FP, FN
  pred_type <- rep(NA, length(pred_vec))
  for(i in seq_along(pred_vec)){
    curr_pred <- pred_vec[i]
    curr_class <- class_vec[i]
    correct_pred <- curr_class == curr_pred
    if(correct_pred){ # is it true positive or true negative
      TP <- curr_class == positive_class
      if(TP){ # true positive
        pred_type[i] <- "TP"
      }else{ # true negative
        pred_type[i] <- "TN"
      }
    }else{ # not a correct prediction
      FP <- curr_pred == positive_class
      if(FP){ # false positive
        pred_type[i] <- "FP"
      }else{ # false negative
        pred_type[i] <- "FN"
      }
    }
  }
  return(data.frame(id=seq(1,length(pred_vec)),pred_type=pred_type))
}

setwd("~/Documents/Papers/npdr_umap/single_sample/local_knn_regain")
#sim="bio"
sim="bio"
switch(sim,
       "77"={
      traindat = read.csv("./sim77/sim77_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim77/sim77_testPred_lurfRF.csv", header = TRUE)
      #dist_lurf = read.csv("./sim77/sim77_lurf_urfp_dist_allSamples.csv", header = TRUE)
    #wide_summary = read.csv("./sim77/sim77_final_local_data.csv", header = TRUE)
    #mtry,splitrule,min.node.size,Accuracy,ntrees
    #   5,gini,12,0.775,5000
    mtry=5; splitrule="gini"; min.node.size=12; ntrees=5000
       },
    "79"={
      traindat = read.csv("./sim79/sim79_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim79/sim79_testPred_lurfRF.csv", header = TRUE)
      #dist_lurf = read.csv("./sim77/sim77_lurf_urfp_dist_allSamples.csv", header = TRUE)
      #wide_summary = read.csv("./sim79/sim79_final_local_data.csv", header = TRUE)
      #wide_summary = NULL
      #mtry,splitrule,min.node.size,Accuracy,ntrees
      #8,extratrees,3,0.829166666666667,6000
      mtry=8; splitrule="extratrees"; min.node.size=3; ntrees=6000
       },
       "80"={
    },
       "85"={
      traindat = read.csv("./sim85/sim85_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim85/sim85_testPred_lurfRF.csv", header = TRUE)
      #dist_lurf = read.csv("./sim85/sim85_lurf_urfp_dist_allSamples.csv", header = TRUE)
      #wide_summary = read.csv("./sim85/sim85_final_local_data.csv", header = TRUE)
      #mtry,splitrule,min.node.size,Accuracy,ntrees
      #2,extratrees,7,0.8425,6000
      mtry=2; splitrule="extratrees"; min.node.size=7; ntrees=6000
       },
    "bio"={
      traindat = read.csv("./bio/bio_trainPred_lurf0.csv", header = TRUE) 
      testdat = read.csv("./bio/bio_testPred_lurf0.csv", header = TRUE)
      dist_lurf = read.csv("./bio/bio_lurf_urfp_dist.csv", header = TRUE)
      #wide_summary = read.csv("./bio/bio_final_local_data.csv", header = TRUE)
      traindat = traindat %>% mutate(class = biotic) %>% select(-biotic)
      testdat = testdat %>% mutate(class = biotic) %>% select(-biotic)
      flagged = read.csv("./bio/bio_flagged_samples.csv", header = TRUE)
      # mtry	splitrule	min.node.size	Accuracy	ntrees
      #    5	extratrees	7	0.914285714	3000
      mtry=5; splitrule="extratrees"; min.node.size=7; ntrees=3000
      #flagged
      #wide_summary %>% filter(train_test=="test") %>% select(id, pred_type) %>% filter(pred_type=="FP" | pred_type=="FN")
    }
)

if (sim!="bio"){
  trainclass <- traindat$class
  trainpred <- traindat$pred
  traindat <- traindat %>% select(-pred)

  testclass <- testdat$class
  testpred <- testdat$pred
  testdat <- testdat %>% select(-pred)
} else{
  trainclass <- traindat$class
  traindat <- traindat %>% select(-Analysis,-pred)
  traindat <- traindat %>% mutate(class = ifelse(class=="biotic", 0, 1))

  testclass <- testdat$class
  testdat <- testdat %>% select(-Analysis,-pred)
  testdat <- testdat %>% mutate(class = ifelse(class=="biotic", 0, 1))
}
class_idx = which(colnames(traindat)=="class")
### TREESHAP DOES NOT LIKE OUTCOME VARIABLE AS FACTOR
rf_fit = ranger(x=traindat[,-class_idx], y = traindat[,class_idx], 
                keep.inbag = TRUE,
                mtry=mtry, splitrule=splitrule, 
                min.node.size=min.node.size, num.trees=ntrees,
                #mtry=tuned_mtry, 
                importance="permutation", 
                #splitrule = tuned_splitRule,
                #min.node.size=tuned_minNodeSize,
                #class.weights = as.numeric(c(1/table(sim_train[,class_idx]))),
                scale.permutation.importance = T,
                local.importance = T, # do outside this function
                classification=T
)
#num.threads=nthreads)
#sort(rf_fit$variable.importance.local[2,],decreasing = T)[1:10]
sorted_imp<-sort(rf_fit$variable.importance,decreasing=TRUE)
sorted_imp
acc2 <- 1-rf_fit$prediction.error
print(paste("Final tuned OOB Accuracy: ",acc2,sep=""))
train_cf <- rf_fit$confusion.matrix 
print("Final tuned confusion matrix: ")
print(train_cf)
# misclassifications
#data.frame(false=which(rf_fit$predictions!=traindat$class))
#which(rf_fit$predictions!=traindat$class)

###### treeshap on NPDR selected features on train data, which rf already seen
model_unified_treeshap <- unify(rf_fit, traindat)
treeshap_res <- treeshap(model_unified_treeshap, traindat)
dim(treeshap_res$shaps)
#plot_contribution(treeshap_res, obs = 1, max_vars=ncol(traindat)-1, title="False")
#plot_contribution(treeshap_res, obs = 2, max_vars=ncol(traindat)-1, title="True")

# wide_summary is a bad variable name
wide_summary <- make_pred_TF_vec(rf_fit$predictions, traindat$class, 
                                 positive_class = 0)
#wide_summary %>% filter(train_test=="train") %>% 
#  filter(pred_type=="FP" | pred_type=="FN") %>% select(id, pred_type)
# True vs False Predictions
false_train_ids <- wide_summary %>% 
  filter(pred_type=="FP" | pred_type=="FN") 
true_train_ids <- wide_summary %>% 
  filter(pred_type=="TP" | pred_type=="TN") 

tshap_false <- rowSums(treeshap_res$shaps[false_train_ids$id,])
tshap_true <- rowSums(treeshap_res$shaps[true_train_ids$id,])
t.test(tshap_true,tshap_false)
# SIM77
#Welch Two Sample t-test
#data:  tshap_true and tshap_false
#t = 0.93666, df = 146.21, p-value = 0.3505
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.04875428  0.13660293
#sample estimates:
#  mean of x    mean of y 
#0.009699955 -0.034224371 
mean(tshap_true)   
length(tshap_true) 
mean(tshap_false)  
length(tshap_false) 

TF_df = data.frame(tshap=c(tshap_true,tshap_false),
                   pred_type=c(rep("TP/TN",length(tshap_true)),
                               rep("FP/FN",length(tshap_false))))
vioplot::vioplot(tshap_true, tshap_false, 
          names=c("TP/TN","FP/FN"), col=c("blue","red"),
          main="Total Tree Shap Scores, Training Data")

## Further breakdown of True vs False Predictions
FP_train_ids <- wide_summary %>% filter(pred_type=="FP")  
FN_train_ids <- wide_summary %>% filter(pred_type=="FN") 
TP_train_ids <- wide_summary %>% filter(pred_type=="TP") 
TN_train_ids <- wide_summary %>% filter(pred_type=="TN") 
tshap_FP <- rowSums(treeshap_res$shaps[FP_train_ids$id,])
tshap_FN <- rowSums(treeshap_res$shaps[FN_train_ids$id,])
tshap_TP <- rowSums(treeshap_res$shaps[TP_train_ids$id,])
tshap_TN <- rowSums(treeshap_res$shaps[TN_train_ids$id,])
mean(tshap_TP) 
mean(tshap_TN) 
mean(tshap_FP) 
mean(tshap_FN) 
length(tshap_TP) 
length(tshap_TN) 
length(tshap_FP) 
length(tshap_FN) 

tshap_TF_subgroups <- c(rep("TP", length(tshap_TP)),  rep("TN", length(tshap_TN)), 
                        rep("FP", length(tshap_FP)), rep("FN", length(tshap_FN)))
tshap_TF_subvalues <- c(tshap_TP, tshap_TN, tshap_FP, tshap_FN)
tshap_TF_df <- data.frame(tshap_TF_subgroups, tshap_TF_subvalues)

# 4 group vioplot
with(tshap_TF_df, vioplot::vioplot( 
  tshap_TF_subvalues[tshap_TF_subgroups=="TP"], 
  tshap_TF_subvalues[tshap_TF_subgroups=="TN"], 
  tshap_TF_subvalues[tshap_TF_subgroups=="FP"],
  tshap_TF_subvalues[tshap_TF_subgroups=="FN"],
  col=c("blue","blue","red","red") , names=c("TP","TN","FP","FN"),
  main="Total Tree SHAP, Training"))

############################################################
###### treeshap on NPDR selected features on TEST DATA
model_unified_treeshap <- unify(rf_fit, testdat)
treeshap_res <- treeshap(model_unified_treeshap, testdat)
dim(treeshap_res$shaps)
#plot_contribution(treeshap_res, obs = 1, max_vars=ncol(testndat)-1, title="False")
#plot_contribution(treeshap_res, obs = 2, max_vars=ncol(testndat)-1, title="True")

#check
#wide_summary %>% filter(train_test=="test") %>% 
#  filter(pred_type=="FP" | pred_type=="FN") %>% select(id, pred_type)
# True vs False Predictions
test_predictions <- predict(rf_fit, 
                            data=testdat[,-class_idx])$predictions
wide_summary <- make_pred_TF_vec(test_predictions, testdat$class, 
                                 positive_class = 0)
false_test_ids <- wide_summary %>% 
  filter(pred_type=="FP" | pred_type=="FN") 
true_test_ids <- wide_summary %>% 
  filter(pred_type=="TP" | pred_type=="TN") 
# have to shift ids by nrow(traindat) since test data starts after train data
tshap_false_test <- rowSums(treeshap_res$shaps[false_test_ids$id,])
tshap_true_test <- rowSums(treeshap_res$shaps[true_test_ids$id,])
t.test(tshap_true_test,tshap_false_test)
wilcox.test(tshap_true_test,tshap_false_test)
mean(tshap_true_test)   
mean(tshap_false_test)
length(tshap_true_test) 
length(tshap_false_test) 

#TF_test_df = data.frame(tshap=c(tshap_true_test,tshap_false_test),
#                   pred_type=c(rep("TP/TN",length(tshap_true_test)),
#                               rep("FP/FN",length(tshap_false_test))))
vioplot::vioplot(tshap_true_test, tshap_false_test, 
                 names=c("TP/TN","FP/FN"), col=c("blue","red"),
                 main="Total Tree SHAP, Test Data")

## Further breakdown of True vs False Predictions
FP_test_ids <- wide_summary %>% filter(pred_type=="FP")  
FN_test_ids <- wide_summary %>% filter(pred_type=="FN") 
TP_test_ids <- wide_summary %>% filter(pred_type=="TP")  
TN_test_ids <- wide_summary %>% filter(pred_type=="TN") 
tshap_FP_test <- rowSums(treeshap_res$shaps[FP_test_ids$id,])
tshap_FN_test <- rowSums(treeshap_res$shaps[FN_test_ids$id,])
tshap_TP_test <- rowSums(treeshap_res$shaps[TP_test_ids$id,])
tshap_TN_test <- rowSums(treeshap_res$shaps[TN_test_ids$id,])
#mean(c(tshap_TP_test,tshap_TN_test))
#mean(c(tshap_FP_test,tshap_FN_test))
#t.test(c(tshap_TP_test,tshap_TN_test),
#       c(tshap_FP_test,tshap_FN_test))
mean(tshap_TP_test) 
mean(tshap_TN_test) 
mean(tshap_FP_test) 
mean(tshap_FN_test) 
length(tshap_TP_test) 
length(tshap_TN_test) 
length(tshap_FP_test) 
length(tshap_FN_test) 

tshap_TF_subgroups_test <- c(rep("TP", length(tshap_TP_test)),  
                             rep("TN", length(tshap_TN_test)), 
                             rep("FP", length(tshap_FP_test)), 
                             rep("FN", length(tshap_FN_test)))
tshap_TF_subvalues_test <- c(tshap_TP_test, tshap_TN_test, 
                             tshap_FP_test, tshap_FN_test)
tshap_TF_df_test <- data.frame(tshap_TF_subgroups_test, tshap_TF_subvalues_test)

# 4 group vioplot
with(tshap_TF_df_test, vioplot::vioplot( 
  tshap_TF_subvalues_test[tshap_TF_subgroups_test=="TP"], 
  tshap_TF_subvalues_test[tshap_TF_subgroups_test=="TN"], 
  tshap_TF_subvalues_test[tshap_TF_subgroups_test=="FP"],
  tshap_TF_subvalues_test[tshap_TF_subgroups_test=="FN"],
  col=c("blue","blue","red","red") , names=c("TP","TN","FP","FN"),
  main="Total Tree SHAP, Test Data"))
