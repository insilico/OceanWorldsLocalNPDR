library(npdr)
#library(igraph)
#library(umap)
library(ggplot2)
library(dplyr)
library(treeshap)
library(ranger)
library(lime)

setwd("~/Documents/Papers/npdr_umap/single_sample/local_knn_regain")

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


make_lime_matrix <- function(lime_df){
  # lime_df: data frame with columns: case, feature, feature_value, feature_weight
  # returns a matrix with cases as rows and lime features scores as columns
  # each cell is the weight of the feature for that case
  #unique_features = data.frame(features=unique(lime_df$feature)) %>% filter(features!="class")
  lime_df <- lime_df %>% filter(feature!="class") # remove class from lime features
  case_id_vec <- as.numeric(unique(lime_df$case))
  score_mat <- matrix(0, nrow = length(case_id_vec), 
                      ncol=length(unique(lime_df$feature)))
  row_idx <- 1
  for (curr_case in case_id_vec){
    #cat(curr_case, "\n")
    curr_case_df <- lime_df %>% filter(case==curr_case) %>%
      select(feature,feature_weight)
    feature_names <- curr_case_df$feature
    #cat(length(curr_case_df$feature_weight),"\n")
    #cat(curr_case_df$feature_weight, "\n")
    score_mat[row_idx,] <- curr_case_df$feature_weight
    row_idx <- row_idx + 1
  }
  colnames(score_mat) <- feature_names 
  return(score_mat)
}

#sim="bio"
sim="bio"
switch(sim,
       "77"={
      traindat = read.csv("./sim77/sim77_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim77/sim77_testPred_lurfRF.csv", header = TRUE)
      dist_lurf = read.csv("./sim77/sim77_lurf_urfp_dist_allSamples.csv", header = TRUE)
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
      dist_lurf = read.csv("./sim85/sim85_lurf_urfp_dist_allSamples.csv", header = TRUE)
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
  traindat <- traindat %>% mutate_at(vars(class), factor)

  testclass <- testdat$class
  testpred <- testdat$pred
  testdat <- testdat %>% select(-pred)
  testdat <- testdat %>% mutate_at(vars(class), factor)
} else{
  trainclass <- traindat$class
  traindat <- traindat %>% select(-Analysis,-pred)
  traindat <- traindat %>% mutate(class = ifelse(class=="biotic", 0, 1)) %>%
                mutate_at(vars(class), factor)

  testclass <- testdat$class
  testdat <- testdat %>% select(-Analysis,-pred)
  testdat <- testdat %>% mutate(class = ifelse(class=="biotic", 0, 1)) %>%
    mutate_at(vars(class), factor)
}
class_idx = which(colnames(traindat)=="class")
### TREESHAP DOES NOT LIKE OUTCOME VARIABLE AS FACTOR
# LIME wants factor
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

###### lime on NPDR selected features on test data, which rf already seen
# explainer - one model for all samples
lime_explainer <- lime(traindat,#training data
                          rf_fit, # tuned RF model
                          bin_continuous = TRUE, # bin continuous variables
                          quantile_bins = FALSE) # spread over range of training data

#explain_test <- lime::explain(testdat, lime_explainer,
#                    n_labels = 1, 
#                    #feature_select = "none", # use all features
#                    n_features = ncol(traindat)-1) # number of features to explain

#lime_score_mat <- make_lime_matrix(explain_test)

#plot_contribution(treeshap_res, obs = 1, max_vars=ncol(traindat)-1, title="False")
#plot_contribution(treeshap_res, obs = 2, max_vars=ncol(traindat)-1, title="True")

test_predictions <- predict(rf_fit, 
                            data=testdat[,-class_idx])$predictions
wide_summary <- make_pred_TF_vec(test_predictions, testdat$class, 
                                 positive_class = 0)
#false_test_ids <- wide_summary %>% 
#  filter(pred_type=="FP" | pred_type=="FN") 
#true_test_ids <- wide_summary %>% 
#  filter(pred_type=="TP" | pred_type=="TN") 
# have to shift ids by nrow(traindat) since test data starts after train data
#explain_test_false <- lime::explain(traindat[false_test_ids$id,], lime_explainer,
#                              n_labels = 1, 
#                              #feature_select = "none", # use all features
#                              n_features = ncol(traindat)-1) # number of features to explain
#lime_score_mat_false <- make_lime_matrix(explain_test_false)
#lime_false_test <- rowSums(lime_score_mat_false)

#explain_test_true <- lime::explain(testdat[true_test_ids$id,], lime_explainer,
#                                    n_labels = 1, 
                                    #feature_select = "none", # use all features
#                                    n_features = ncol(traindat)-1) # number of features to explain
#lime_score_mat_true <- make_lime_matrix(explain_test_true)
#lime_true_test <- rowSums(lime_score_mat_true)

#t.test(lime_true_test,lime_false_test)
#mean(lime_true_test)   
#mean(lime_false_test)
#length(lime_true_test) 
#length(lime_false_test) 

#TF_test_df = data.frame(tshap=c(tshap_true_test,tshap_false_test),
#                   pred_type=c(rep("TP/TN",length(tshap_true_test)),
#                               rep("FP/FN",length(tshap_false_test))))
#vioplot::vioplot(lime_true_test, lime_false_test, 
#                 names=c("TP/TN","FP/FN"), col=c("blue","red"),
#                 main="Total LIME, Test Data")

####################### Further breakdown of True vs False Predictions
FP_test_ids <- wide_summary %>% filter(pred_type=="FP")  
FN_test_ids <- wide_summary %>% filter(pred_type=="FN") 
TP_test_ids <- wide_summary %>% filter(pred_type=="TP")  
TN_test_ids <- wide_summary %>% filter(pred_type=="TN") 
length(FP_test_ids$id) 
length(FN_test_ids$id) 
length(TP_test_ids$id) 
length(TN_test_ids$id) 

explain_test_FP <- lime::explain(testdat[FP_test_ids$id,], lime_explainer,
                                    n_labels = 1, 
                                    #feature_select = "none", # use all features
                              n_features = ncol(traindat)) # number of features to explain
explain_test_FN <- lime::explain(testdat[FN_test_ids$id,], lime_explainer,
                                 n_labels = 1, 
                                 #feature_select = "none", # use all features
                                 n_features = ncol(traindat)) # number of features to explain
explain_test_TP <- lime::explain(testdat[TP_test_ids$id,], lime_explainer,
                                 n_labels = 1, 
                                 #feature_select = "none", # use all features
                             n_features = ncol(traindat)) # number of features to explain
explain_test_TN <- lime::explain(testdat[TN_test_ids$id,], lime_explainer,
                                 n_labels = 1, 
                                 #feature_select = "none", # use all features
                             n_features = ncol(traindat)) # number of features to explain

lime_score_mat_FP <- make_lime_matrix(explain_test_FP)
lime_score_mat_FN <- make_lime_matrix(explain_test_FN)
lime_score_mat_TP <- make_lime_matrix(explain_test_TP)
lime_score_mat_TN <- make_lime_matrix(explain_test_TN)

##
# Actual Classes for True's
# 9 samples
testdat$class[TP_test_ids$id] # postive class is 0: all 0's
# 5 features but class is included, so 6
# long lime data
# 9 * 6 * 2 = 108, the 2 comes from the probability for each class, repeated 
# prediction is the "original prediction from the model"
# I think "original" means the ranger prediction
unlist(explain_test_TP$prediction)
# these predictions are all 0's as expected
names(unlist(explain_test_TP$prediction))[unlist(explain_test_TP$prediction)>0.5]
# model_prediction, I think this means the LIME prediciton based on perturbation of sample
length(explain_test_TP$model_prediction) # 9 samples times 6 features
explain_test_TP$model_prediction>0.5
explain_test_TP$label
explain_test_TN$label
explain_test_FN$label 
explain_test_FP$label

testdat$class[TN_test_ids$id] # negative class is 1: all 1's

## first LIME TN plot
testdat$class[TN_test_ids$id[1]] # true class label is 1
# In the plot, I believe label is the data class label, class is the RF prediction
explain1_test_TN <- lime::explain(testdat[TN_test_ids$id[1],], lime_explainer,
                                 n_labels = 1, 
                                 #feature_select = "none", # use all features
                                 n_features = ncol(traindat)) 
plot_features(explain1_test_TN, ncol = ncol(traindat))#, 
#              title="LIME Features for False Positives",
#              xlab="Features", ylab="LIME Score")
## first LIME FN plot
explain1_test_FN <- lime::explain(testdat[FN_test_ids$id[1],], lime_explainer,
                                  n_labels = 1, 
                                  #feature_select = "none", # use all features
                                  n_features = ncol(traindat)) 
plot_features(explain1_test_FN, ncol = ncol(traindat))#, 
#              title="LIME Features for False Positives",
#              xlab="Features", ylab="LIME Score")

lime_FP_test <- rowSums(lime_score_mat_FP)
lime_FN_test <- rowSums(lime_score_mat_FN)
lime_TP_test <- rowSums(lime_score_mat_TP)
lime_TN_test <- rowSums(lime_score_mat_TN)

mean(c(lime_TP_test,lime_TN_test))
mean(c(lime_FP_test,lime_FN_test))
t.test(c(lime_TP_test,lime_TN_test),
       c(lime_FP_test,lime_FN_test))

vioplot::vioplot(c(lime_TP_test,lime_TN_test), c(lime_FP_test,lime_FN_test), 
                 names=c("TP/TN","FP/FN"), col=c("blue","red"),
                 main="Total LIME, Test Data")

mean(lime_TP_test) 
mean(lime_TN_test) 
mean(lime_FP_test) 
mean(lime_FN_test) 
length(lime_TP_test) 
length(lime_TN_test) 
length(lime_FP_test) 
length(lime_FN_test) 

lime_TF_subgroups_test <- c(rep("TP", length(lime_TP_test)),  
                             rep("TN", length(lime_TN_test)), 
                             rep("FP", length(lime_FP_test)), 
                             rep("FN", length(lime_FN_test)))
lime_TF_subvalues_test <- c(lime_TP_test, lime_TN_test, 
                             lime_FP_test, lime_FN_test)
lime_TF_df_test <- data.frame(lime_TF_subgroups_test, lime_TF_subvalues_test)

# 4 group vioplot
with(lime_TF_df_test, vioplot::vioplot( 
  lime_TF_subvalues_test[lime_TF_subgroups_test=="TP"], 
  lime_TF_subvalues_test[lime_TF_subgroups_test=="TN"], 
  lime_TF_subvalues_test[lime_TF_subgroups_test=="FP"],
  lime_TF_subvalues_test[lime_TF_subgroups_test=="FN"],
  col=c("blue","blue","red","red") , names=c("TP","TN","FP","FN"),
  main="Total LIME, Test Data"))

