#######################
# Function to tune RF
#######################
tuneRFclass <- function(train.dat, test.dat, rm.ind, outcome.ind, cv.folds=5, tuneTrees=T, 
                        maxTrees=10000, parallel=T, nthreads=4){
  # requires ranger, doParallel, caret
  ret.list <- list()
  list.ind <- 1
  
  # RF default hyperparameters for classification
  # min.node.size = 1 - trees are fully grown (most tree depth)
  #                     bigger min.node.size = smaller trees
  #                     deeper trees = more variance (risk overfitting); 
  #                     shallower trees = more bias (risk of not capturing relationships in data)
  # mtry = floor(sqrt(ncol(x))) - number of variables to randomly sample at each split
  #                     mtry = p --> bagging; mtry = 1: split variable completely random, could bias results
  #                     tune over 2 to p
  # splitrule - how trees are split
  #                     exratrees
  #                     gini
  #                     hellinger
  tuneGrid<-expand.grid(
    .mtry=seq(2,(dim(train.dat[,-rm.ind])[2]),by=3),
    .splitrule=c("extratrees","gini","hellinger"),
    .min.node.size=seq(1,floor(dim(train.dat)[1]/20)))#10
  
  # set up train control
  if(parallel){#recommended
    trControl<-trainControl(method = "cv", 
                            number = cv.folds,
                            verboseIter = T, 
                            search="random",
                            allowParallel=T)
  }else{#slow, very slow
    trControl<-trainControl(method = "cv", 
                            number = cv.folds,
                            verboseIter = T, 
                            search="random",
                            allowParallel=F)
  }
  # tune these 3 hyperparams
  start<-Sys.time()
  rf1<-train(x=train.dat[,-rm.ind], 
             y=train.dat[,outcome.ind],
             method="ranger",
             importance="none",
             metric="Accuracy",
             trControl=trControl, 
             tuneGrid=tuneGrid,
             num.trees=5000, # could make an arg for the func?
             class.weights = as.numeric(c(1/table(train.dat[,outcome.ind]))),
             verbose=T)
  end<-Sys.time()
  rf.time<-end-start
  
  print("tune time for mtry, splitrule, and min.node.size: ")
  print(rf.time)
  #print("tuned hyperparameters:")
  #print(rf1$bestTune)
  # best accuracy
  #print("OOB Accuracy with tuned hyperparameters:")
  acc1 <- 1-rf1$finalModel$prediction.error
  #print(acc1)
  
  # saved tuned hyperparameters to df for output list
  tune.df <- rf1$bestTune
  tune.df$Accuracy <- acc1
  # hyperparameters to tune ntrees
  tuned_mtry<-rf1$bestTune$mtry
  tuned_minNodeSize<-rf1$bestTune$min.node.size
  tuned_splitRule<-rf1$bestTune$splitrule
  
  print("tuned hyperparameters:")
  print(tune.df)
  
  # tune trees if specified
  if(tuneTrees){
    # tune the number of trees using best params from previous run
    fileName<-"maxtrees_out_tmp.csv" # for parallel output
    
    tuneGrid<-expand.grid(.mtry=tuned_mtry,
                          .splitrule=tuned_splitRule,
                          .min.node.size=tuned_minNodeSize)
    
    trControl<-trainControl(method = "cv",
                            number = cv.folds,
                            verboseIter = T, 
                            savePredictions=TRUE,
                            search="random",
                            allowParallel=F # parallelize across number of trees
    )
    
    start<-Sys.time()
    foreach(max_tree=seq(3000,maxTrees,by=1000)) %dopar% {
      set.seed(1234)
      rf<-train(x=train.dat[,-rm.ind], 
                y=train.dat[,outcome.ind],
                method="ranger",
                importance="none", 
                metric="Accuracy", 
                trControl=trControl, 
                tuneGrid=tuneGrid,
                num.trees=max_tree,
                class.weights = as.numeric(c(1/table(train.dat[,outcome.ind]))),
                verbose=T)
      curr_acc<-cbind.data.frame(max_tree,rf$results$Accuracy)
      write.table(curr_acc,fileName,quote=F,append=T,col.names=F,row.names=F,sep=",")
    }
    end<-Sys.time()
    rf.time<-end-start
    print("tune time for ntrees: ")
    print(rf.time)
    
    # read in file, get max accuracy ntrees
    tree.results<-read.csv("maxtrees_out_tmp.csv",header=F)
    colnames(tree.results)<-c("maxtrees","Accuracy")
    max_acc<-tree.results[which.max(tree.results$Accuracy),]
    print("tuned ntrees OOB Accuracy: ")
    print(max_acc) 
    tuned_maxTree <- max_acc$maxtrees
    tune.df$ntrees <- tuned_maxTree
    
    # add tuned df to output list
    ret.list[[list.ind]] <- tune.df
    list.ind <- list.ind+1
    
    # Delete temp file 
    file.remove(fileName)
    
  }else{#use default
    tuned_maxTree <- 5000
    tune.df$ntrees <- tuned_maxTree
    ret.list[[list.ind]] <- tune.df
    list.ind <- list.ind+1
  }
  ### run a final model
  # change outcome to factor, might not be - for ranger not called from caret
  train.dat[,outcome.ind] <- as.factor(train.dat[,outcome.ind])
  test.dat[,outcome.ind] <- as.factor(test.dat[,outcome.ind])
  
  # rf final model, return local importance
  rfFinal.fit <- ranger(x = train.dat[,-rm.ind], 
                        y = train.dat[,outcome.ind], 
                        keep.inbag = TRUE,
                        num.trees=tuned_maxTree, 
                        mtry=tuned_mtry, 
                        importance="permutation", 
                        splitrule = tuned_splitRule,
                        min.node.size=tuned_minNodeSize,
                        class.weights = as.numeric(c(1/table(train.dat[,outcome.ind]))),
                        scale.permutation.importance = T,
                        local.importance = F, # do outside this function
                        num.threads=nthreads)
  sorted.imp<-sort(rfFinal.fit$variable.importance,decreasing=TRUE)
  train_cf <- rfFinal.fit$confusion.matrix 
  print("Final tuned confusion matrix: ")
  print(train_cf)
  #             predicted
  # true      abiotic biotic
  # abiotic      89      0
  # biotic        0     51
  acc2 <- 1-rfFinal.fit$prediction.error
  print(paste("Final tuned OOB Accuracy: ",acc2,sep=""))
  
  # add model to output list
  ret.list[[list.ind]] <- rfFinal.fit
  list.ind <- list.ind + 1 
  
  # use test data to validate model
  predFinal.test<-predict(rfFinal.fit,data=test.dat[,-rm.ind])
  test_cm <- table(test.dat[,outcome.ind],predFinal.test$predictions)
  print("Test confusion matrix: ")
  print(test_cm)
  # predicted
  # T       abiotic biotic
  # abiotic      22      0
  # biotic        1     11
  test_acc <- sum(test.dat[,outcome.ind]==predFinal.test$predictions)/length(test.dat[,outcome.ind])
  # add to output list
  ret.list[[list.ind]] <- test_cm
  list.ind <- list.ind+1
  ret.list[[list.ind]] <- test_acc
  list.ind <- list.ind+1
  
  # add final model predictions to train and test data
  train.dat$pred <- rfFinal.fit$predictions # I think?
  test.dat$pred <- predFinal.test$predictions
  # add to output list
  ret.list[[list.ind]] <- train.dat 
  list.ind<-list.ind+1
  ret.list[[list.ind]] <- test.dat
  list.ind<-list.ind+1
  
  return(ret.list)
}

###########################
# Functions for local NPDR
############################
ss_npdr_ridge <- function(ss_diff_mat, ss_class_diff, weights, lambda="lambda.1se"){
  #ret.list <- list()
  ss_ridge <- glmnet::cv.glmnet(ss_diff_mat, ss_class_diff,
                                alpha = 0, family = "binomial", 
                                lower.limits = -Inf, # min val for coeffs
                                type.measure = "class",
                                weights=weights)
  if(lambda=="lambda.1se"){
    lambda <- ss_ridge$lambda.1se
  }else if(lambda=="lambda.min"){
    lambda <- ss_ridge$lambda.min
  } # else use specified lam
  
  ss_coeffs <- data.frame(as.matrix(predict(ss_ridge, 
                                            type = "coefficients",
                                            weights=weights,
                                            s=lambda)))
  ss_coeffs <- tibble::rownames_to_column(ss_coeffs, "att") 
  ss_coeffs<- ss_coeffs %>% filter(att!="(Intercept)") # remove Intercept
  ss_coeffs$abs_scores <- abs(ss_coeffs$s1)
  ss_coeffs <- ss_coeffs %>% filter(abs_scores>0) %>% arrange(-abs_scores)
  ss_coeffs
  #                 att            s1   abs_scores
  # 1  avg_rR45CO244CO2  1.506174e+02 1.506174e+02
  # 2        sd_d18O13C  3.630152e+01 3.630152e+01
  # 3        diff2_acf1  2.933570e+01 2.933570e+01
  # 4 fluctanal_prop_r1  6.413905e+00 6.413905e+00
  # 5     time_kl_shift -2.788801e-05 2.788801e-05
  
  return(ss_coeffs)
}

calculate_diff_mat <- function(X, neighbor_pairs_idx){
  # X is predictors data matrix (no class column)
  # X is the matrix of neighbor pairs from npdr::nearestNeighbors
  num_attr <- ncol(X)
  diff_mat <- matrix(0, nrow = nrow(neighbor_pairs_idx), ncol = num_attr)
  for (attr_idx in seq(1, num_attr)) {
    attr_vec <- X[, attr_idx]
    # difference between neighbors for the current attribute vector
    Ri_attr_vals <- attr_vec[neighbor_pairs_idx[, 1]] # first column of neighbor pairs
    NN_attr_vals <- attr_vec[neighbor_pairs_idx[, 2]] # second column of neighbor pairs
    # diff vector for current attribute
    attr_diff_vec <- abs(Ri_attr_vals - NN_attr_vals)
    diff_mat[, attr_idx] <- attr_diff_vec # append to diff matrix
  } # end for
  colnames(diff_mat) <- colnames(X)
  return(diff_mat)
}

ss_npdr_ridge2 <- function(ss_diff_mat, ss_class_diff, weights, lambda="lambda.1se"){
  ret.list <- list()
  ss_ridge <- glmnet::cv.glmnet(ss_diff_mat, ss_class_diff,
                                alpha = 0, family = "binomial", 
                                lower.limits = -Inf, # min val for coeffs
                                type.measure = "class",
                                weights=weights)
  if(lambda=="lambda.1se"){
    lambda <- ss_ridge$lambda.1se
  }else if(lambda=="lambda.min"){
    lambda <- ss_ridge$lambda.min
  } # else use specified lam
  ret.list[[1]] <- ss_ridge
  
  ss_coeffs <- data.frame(as.matrix(predict(ss_ridge, 
                                            type = "coefficients",
                                            weights=weights,
                                            s=lambda)))
  ss_coeffs <- tibble::rownames_to_column(ss_coeffs, "att") 
  ss_coeffs<- ss_coeffs %>% filter(att!="(Intercept)") # remove Intercept
  ss_coeffs$abs_scores <- abs(ss_coeffs$s1)
  ss_coeffs <- ss_coeffs %>% filter(abs_scores>0) %>% arrange(-abs_scores)
  ss_coeffs
  #                 att            s1   abs_scores
  # 1  avg_rR45CO244CO2  1.506174e+02 1.506174e+02
  # 2        sd_d18O13C  3.630152e+01 3.630152e+01
  # 3        diff2_acf1  2.933570e+01 2.933570e+01
  # 4 fluctanal_prop_r1  6.413905e+00 6.413905e+00
  # 5     time_kl_shift -2.788801e-05 2.788801e-05
  ret.list[[2]] <- ss_coeffs
  return(ret.list)
}

calculate_class_diff <- function(class_vec_numeric, neighbor_pairs_idx){
  # create diff vector for class vector
  #(class_vec_numeric[neighbor_pairs_idx[, 1]] ==
  #             class_vec_numeric[neighbor_pairs_idx[, 2]]) + 0
  # create diff vector for class vector 
  ifelse(class_vec_numeric[neighbor_pairs_idx[, 1]] == class_vec_numeric[neighbor_pairs_idx[, 2]], 0, 1) 
}

ss_diffs_weights <- function(data, neighbor_pairs_idx, ss_idx, knn, class01){
  ret.list <- list()
  #train_X <- data
  # get neighbors of current single sample
  # from the neighbor_pairs_idx matrix from npdr::nearestNeighbors
  ss_neighbor_rows <- which(neighbor_pairs_idx[,1] %in% ss_idx)
  ss_neighbor_pairs <- neighbor_pairs_idx[ss_neighbor_rows,]
  
  # compute npdr diff matrix and class diff for the ss neighbors
  ss_class_diff <- calculate_class_diff(class01,ss_neighbor_pairs)
  ss_diff_mat <- calculate_diff_mat(data,ss_neighbor_pairs)
  ret.list[[1]] <- ss_class_diff 
  ret.list[[2]] <- ss_diff_mat
  #head(ss_class_diff)
  #head(ss_diff_mat)
  
  # interesting quantity
  # does a more balanced neighborhood have better scores?
  # percentage of neighbors of this sample that are in the same class/hits
  table(ss_class_diff)
  neighborhood_balance <- sum(ss_class_diff)/knn
  #neighborhood_balance
  ret.list[[3]] <- neighborhood_balance
  
  # calculate weights
  # account for imbalance in glmnet
  # https://stats.stackexchange.com/questions/357407/glmnet-weights-and-imbalanced-data
  # 1 - num_in_class/num_samples
  fraction_0 <- rep(1 - sum(ss_class_diff == 0) / length(ss_class_diff), sum(ss_class_diff == 0))
  fraction_1 <- rep(1 - sum(ss_class_diff == 1) / length(ss_class_diff), sum(ss_class_diff == 1))
  
  # assign that value to a "weights" vector
  weights <- numeric(length(ss_class_diff))
  weights[ss_class_diff == 0] <- fraction_0
  weights[ss_class_diff == 1] <- fraction_1
  ret.list[[4]] <- weights
  names(ret.list)<-c("ss_class_diff","ss_diff_mat","neighborhood_balance","weights")
  
  return(ret.list)
}

true_false_data <- function(data, pos_class=0,class_colname="class",pred_colname="pred"){
  pred_type <- c()
  class_col_idx <- which(colnames(data)==class_colname)
  pred_col_idx <- which(colnames(data)==pred_colname)
  for(i in seq(1,dim(data)[1])){
    #true_class <- data[i,class_col_idx]
    #pred_class <- data[i,pred_col_idx]
    
    correct_pred <- data[i,class_col_idx] == data[i,pred_col_idx]
    if(correct_pred){ # is it true positive or true negative
      TP <- data[i,class_col_idx] == pos_class
      if(TP){ # true positive
        pred_type <- c(pred_type,"TP")
      }else{ # true negative
        pred_type <- c(pred_type,"TN")
      }
    } else{ # not a correct prediction
      FP <- data[i,pred_col_idx] == pos_class
      if(FP){ # false positive
        pred_type <- c(pred_type,"FP")
      }else{ # false negative
        pred_type <- c(pred_type,"FN")
      }
    }
  }
  data$pred_type <- pred_type
  return(data)
}

class01 <- function(data, class_idx){
  data_classes <- table(data[,class_idx])
  class_names <- names(data_classes)
  head(class_names)
  #class_names <- as.integer(class_names)
  
  class_vec <- as.character(data[,class_idx])
  #head(class_vec)
  #class_vec <- as.integer(class_vec)
  data[,class_idx] <- class_vec
  
  contains_zero <- "0" %in% class_names
  contains_one <- "1" %in% class_names
  
  if(!contains_zero | !contains_one){
    # adjust class vec
    print("Adjusting class vector to 0/1")
    class1_idx <- which(data[,class_idx] == class_names[1])
    class2_idx <- which(data[,class_idx] == class_names[2])
    
    data[class1_idx,class_idx] <- rep(1,length(class1_idx))
    data[class2_idx,class_idx] <- rep(0,length(class2_idx))
    
    #table(data$class)
  }
  data[,class_idx] <- as.factor(data[,class_idx])
  #head(data)
  return(data)
}

local_truePred_df <- function(trainPred_tf_df, testPred_tf_df, class_colname,
                              pred_colname="pred"){
  # combine train/test data to make local dataframe
  local_df <- rbind.data.frame(trainPred_tf_df,testPred_tf_df)
  # grab actual class label for training data
  train_class_vec <- trainPred_tf_df[,which(colnames(trainPred_tf_df)==class_colname)]
  train_pred_vec <- trainPred_tf_df[,which(colnames(trainPred_tf_df)==pred_colname)]
  # grab predicted value for test data
  test_pred_vec <- testPred_tf_df[,which(colnames(testPred_tf_df)==pred_colname)]
  # label training predicitons "train"
  train_predType <- rep("train",length(train_pred_vec))
  # for test data, add the pred_type (TP, TN, FP, FN)
  test_predType <- testPred_tf_df$pred_type
  # combine test, train labels into vectors
  hybrid_predType_vec <- c(train_predType,test_predType)
  hybrid_pred_vec <-  c(train_pred_vec,test_pred_vec)
  # add to local dataframe
  local_df$pred <- hybrid_pred_vec
  local_df$pred_type <- hybrid_predType_vec
  return(local_df)
}

local_npdr <- function(data, class_idx, dist_mat = NULL, nbd_metric = "manhattan", 
                       ss_idx, lambda="lambda.1se", rm_cols = NULL, knn=NULL){
  # data : class col needs to be 0 or 1
  ret_list <- list()
  
  # rm_cols: cols other than class label that need to be removed 
  class_names <- names(table(data[,class_idx]))
  #class_names
  
  # change to 0/1
  data01<-class01(data,class_idx)
  #head(data01)
  
  # process out class label and remove any other cols specified
  if(is.null(rm_cols)){ # just process out class label
    data_X <- data[,-class_idx]  
  }else{# process out other columns
    rm_cols_idx <- which(colnames(data) %in% rm_cols)
    rm_tot <- c(class_idx,rm_cols_idx) # lac
    data_X <- data[,-rm_tot]
  }
  
  # use kmax by default to avoid warnings
  if(is.null(knn)){
    class01 <-  data01[,class_idx]
    knn <- knnSURF.balanced(class01,sd.frac=.5)
    #knn = nrow(data)-1
    ret_list[[1]] <- knn
  }else{
    ret_list[[1]] <- knn
  }
  
  # calculate NN pairs - check if distance matrix precomputed or not
  if(nbd_metric == "precomputed"){
    neighbor_pairs_idx <- npdr::nearestNeighbors(dist_mat,
                                                 nbd.method = "relieff", # for fixed-k
                                                 nbd.metric= nbd_metric, 
                                                 k=knn,dopar.nn=F)
  } else{ # need to specify either manhattan, euclidean or mahalanobis 
    neighbor_pairs_idx <- npdr::nearestNeighbors(data_X, #lac
                                                 nbd.method = "relieff", # for fixed-k
                                                 nbd.metric= nbd_metric, 
                                                 k=knn,dopar.nn=F)
  }
  ret_list[[2]] <- neighbor_pairs_idx
  ## calculate class diffs and class diff matrix
  # get 01 class vector
  class01_vec <- data01[,class_idx]
  #head(class01_vec)
  #class_diff <- calculate_class_diff(class01_vec,neighbor_pairs_idx)
  #class_diff_mat <- calculate_diff_mat(data_X,neighbor_pairs_idx)
  
  ## now for the single-sample analysis
  #ss_idx <- 2 # for debugging
  ## get neighbors of current single sample
  # from the neighbr_pairs_idx matrix from npdr::nearestNeighbors
  ss_neighbor_rows <- which(neighbor_pairs_idx[,1]==ss_idx)
  ss_neighbor_pairs <- neighbor_pairs_idx[ss_neighbor_rows,]
  
  # compute npdr diff matrix and class diff for the ss neighbors
  ss_class_diff <- calculate_class_diff(class01_vec, ss_neighbor_pairs)
  ss_diff_mat <- calculate_diff_mat(data_X, ss_neighbor_pairs)
  ret_list[[3]] <- ss_class_diff
  ret_list[[4]] <- ss_diff_mat
  
  # interesting quantity: does a more balanced neighborhood have better scores?
  # percentage of neighbors of this sample that are in the same class/hits
  same_class_ratio_local <- sum(ss_class_diff)/knn  
  ret_list[[5]] <- same_class_ratio_local
  
  # account for imbalance in glment: https://stats.stackexchange.com/questions/357407/glmnet-weights-and-imbalanced-data
  # 1 - num_in_class/num_samples
  fraction_0 <- rep(1 - sum(ss_class_diff == 0) / length(ss_class_diff), sum(ss_class_diff == 0))
  fraction_1 <- rep(1 - sum(ss_class_diff == 1) / length(ss_class_diff), sum(ss_class_diff == 1))
  
  # assign that value to a "weights" vector
  weights <- numeric(length(ss_class_diff))
  weights[ss_class_diff == 0] <- fraction_0
  weights[ss_class_diff == 1] <- fraction_1
  ret_list[[6]] <- weights
  
  # bam
  if (sum(ss_class_diff)==0){
    stop("All neighbors are misses. Increase knn.\n", call.=F)
  }
  
  npdr_ridge <- ss_npdr_ridge2(ss_diff_mat, ss_class_diff, weights, lambda=lambda)
  
  ret_list[[7]] <- npdr_ridge[[1]]
  ret_list[[8]] <- npdr_ridge[[2]]
  
  names(ret_list) <- c("knn","neighbor_pairs_idx","ss_class_diff","ss_diff_mat",
                       "nbd_balance","weights","lambdas","coeffs")
  
  return(ret_list)
}

localNPDR_actualPred_scores <- function(data, test_idx_vec, pred_colname="pred", actual_colname="class",
                                       pred_rm_cols=c("class","pred_type"),actual_rm_cols=c("pred","pred_type"),
                                       #local npdr args
                                       nbd_metric="precomputed",dist_mat,lambda="lambda.1se",
                                       probability_data = F, knn=NULL, prob_cols_vec=NULL, prob_df=NULL){
  # return lists for local NPDR results for predicted class and actual class
  # these may be the same in the case of correct predictions
  ret_pred_list <- list()
  ret_actual_list <- list()
  # get pred, class col idx 
  pred_col_idx <- which(colnames(data)==pred_colname)
  actual_col_idx <- which(colnames(data)==actual_colname)
  
  pred_rm_idx <- which(colnames(data) %in% pred_rm_cols)
  actual_rm_idx <- which(colnames(data) %in% actual_rm_cols)
  
  # get vector for actual class and predicted class labels
  actual_lab_vec <- data[test_idx_vec, actual_col_idx]
  pred_lab_vec <- data[test_idx_vec, pred_col_idx]
  
  if (!is.null(knn)){
    if (knn=="kmax"){
      knn = nrow(data)-1
    }else{
      knn = knnSURF.balanced(data[,actual_col_idx])
    }
  }
  
  for(i in seq(1,length(test_idx_vec))){
    # i=1 
    test_idx <- test_idx_vec[i]
    # local npdr for predicted class label
    pred_list <- list()
    localNPDR_pred <- local_npdr(data=data, class_idx=pred_col_idx, 
                                 dist_mat = dist_mat,
                                 nbd_metric = "precomputed", 
                                 ss_idx= test_idx, lambda=lambda, 
                                 rm_cols = pred_rm_cols,knn=knn)
    #names(localNPDR_pred)
    # [1] "knn"                "neighbor_pairs_idx" "ss_class_diff"      "ss_diff_mat"       
    # [5] "nbd_balance"        "weights"            "lambdas"            "coeffs" 
    pred_list[[1]] <- localNPDR_pred$knn
    #pred_list[[2]] <- localNPDR_pred$neighbor_pairs_idx
    #pred_list[[3]] <- localNPDR_pred$ss_class_diff
    #pred_list[[4]] <- localNPDR_pred$ss_diff_mat
    pred_list[[2]] <- localNPDR_pred$nbd_balance
    #pred_list[[6]] <- localNPDR_pred$weights
    pred_list[[3]] <- localNPDR_pred$lambdas
    pred_list[[4]] <- localNPDR_pred$coeffs
    
    
    # calculate total score
    total_pred_score <- sum(localNPDR_pred$coeffs$s1)
    total_pred_score
    # [1] 0.5608593
    pred_list[[5]] <- total_pred_score
    
    # now use the class as the class column - let test_idx be "correct"
    actual_list <- list()
    localNPDR_actual <- local_npdr(data=data, class_idx=actual_col_idx, 
                                   dist_mat = dist_mat,
                                   nbd_metric = nbd_metric, 
                                   ss_idx = test_idx, lambda=lambda, 
                                   rm_cols = actual_rm_cols,knn=knn)
    actual_list[[1]] <- localNPDR_actual$knn
    #actual_list[[2]] <- localNPDR_actual$neighbor_pairs_idx
    #actual_list[[3]] <- localNPDR_actual$ss_class_diff
    #actual_list[[4]] <- localNPDR_actual$ss_diff_mat
    actual_list[[2]] <- localNPDR_actual$nbd_balance
    #actual_list[[6]] <- localNPDR_actual$weights
    actual_list[[3]] <- localNPDR_actual$lambdas
    actual_list[[4]] <- localNPDR_actual$coeffs
    
    # total score
    total_actual_score <- sum(localNPDR_actual$coeffs$s1)
    actual_list[[5]] <- total_actual_score
    
    # make dfs for plotting, add to pred_list and actual_list
    localNPDR_pred_df <- as.data.frame(t(localNPDR_pred$coeffs$s1))
    localNPDR_actual_df <- as.data.frame(t(localNPDR_actual$coeffs$s1))
    colnames(localNPDR_pred_df) <- localNPDR_pred$coeffs$att
    colnames(localNPDR_actual_df) <- localNPDR_actual$coeffs$att
    
    # add actual and predicted classes, probabilities and pred_type
    localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- data[test_idx, actual_col_idx]
    colnames(localNPDR_pred_df)[dim(localNPDR_pred_df)[2]] <- actual_colname
    localNPDR_actual_df[,dim(localNPDR_actual_df)[2]+1] <- data[test_idx, actual_col_idx]
    colnames(localNPDR_actual_df)[dim(localNPDR_actual_df)[2]] <- actual_colname
    if(probability_data){
      prob_idx_vec <- which(colnames(prob_df) %in% prob_cols_vec)
      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[1]]
      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[2]]
      colnames(localNPDR_pred_df)[c(dim(localNPDR_pred_df)[2]-1,dim(localNPDR_pred_df)[2])] <- prob_cols_vec
      
      localNPDR_actual_df[,dim(localNPDR_actual_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[1]]
      localNPDR_actual_df[,dim(localNPDR_actual_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[2]]
      colnames(localNPDR_actual_df)[c(dim(localNPDR_actual_df)[2]-1,dim(localNPDR_actual_df)[2])] <- prob_cols_vec
    }
    localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- data[test_idx,pred_col_idx]
    colnames(localNPDR_pred_df)[dim(localNPDR_pred_df)[2]] <- pred_colname
    #localNPDR_pred_df
    #     mainvar9      var38   mainvar3    mainvar5      var23        var24        var54      intvar6
    # 1 0.02204378 0.02151776 0.02014423 -0.01788702 0.01639048 -0.004402344 -0.002505678 -0.000989372
    #   class    prob_0    prob_1 pred
    # 1     1 0.2675697 0.7324303    1
    
    localNPDR_actual_df[,dim(localNPDR_actual_df)[2]+1] <- data[test_idx,pred_col_idx]
    colnames(localNPDR_actual_df)[dim(localNPDR_actual_df)[2]] <- pred_colname
    #localNPDR_actual_df
    
    localNPDR_pred_df$total_local_score <- total_pred_score
    localNPDR_actual_df$total_local_score <- total_actual_score
    
    # created melted dfs for plotting 
    if(probability_data){
      pred_melted <- melt(rev(localNPDR_pred_df),
                          id=c(actual_colname,pred_colname,prob_cols_vec,"total_local_score")) 
      actual_melted <- melt(rev(localNPDR_actual_df),
                            id=c(actual_colname,pred_colname,prob_cols_vec,"total_local_score")) 
    }else{
      pred_melted <- melt(rev(localNPDR_pred_df),
                          id=c(actual_colname,pred_colname,"total_local_score")) 
      actual_melted <- melt(rev(localNPDR_actual_df),
                            id=c(actual_colname,pred_colname,"total_local_score")) 
    }
    #head(pred_melted)
    # get sign of importance for plot and add to melted dfs
    pred_sign_vec<-rep(0,dim(pred_melted)[1])
    pred_sign_vec[which(pred_melted$value<=0)]<-"Contradicts"
    pred_sign_vec[which(pred_melted$value>0)]<-"Supports"
    pred_melted$sign <- pred_sign_vec
    colnames(pred_melted)[which(colnames(pred_melted)=="value")]<-"local_score"
    #head(pred_melted)
    #  class pred    prob_0    prob_1 variable  local_score        sign
    # 1     1    1 0.3250172 0.6749828  intvar6 -0.008855199 Contradicts
    # 2     1    1 0.3250172 0.6749828    var54 -0.013461589 Contradicts
    # 3     1    1 0.3250172 0.6749828    var24 -0.046561488 Contradicts
    # 4     1    1 0.3250172 0.6749828    var23  0.155782600    Supports
    # 5     1    1 0.3250172 0.6749828 mainvar5 -0.170309330 Contradicts
    # 6     1    1 0.3250172 0.6749828 mainvar3  0.204901707    Supports
    actual_sign_vec<-rep(0,dim(actual_melted)[1])
    actual_sign_vec[which(actual_melted$value<=0)]<-"Contradicts"
    actual_sign_vec[which(actual_melted$value>0)]<-"Supports"
    actual_melted$sign <- actual_sign_vec
    colnames(actual_melted)[which(colnames(actual_melted)=="value")]<-"local_score"
    #head(actual_melted)
    
    pred_list[[6]] <- pred_melted
    actual_list[[6]] <- actual_melted
    
    names(pred_list) <- c("knn","nbd_balance","lambdas","coeffs","total_local_score","melted_df")
    names(actual_list) <- c("knn","nbd_balance","lambdas","coeffs","total_local_score","melted_df")
    
    ret_pred_list[[i]] <- pred_list
    ret_actual_list[[i]] <- actual_list
  }
  ret_list <- list()
  ret_list[[1]] <- ret_pred_list
  ret_list[[2]] <- ret_actual_list
  names(ret_list) <- c("pred","actual")
  return(ret_list)
}

opposite_vector <- function(x,class_names=c("1","0")){
  opp_vec <- c()
  if(x==class_names[1]){
    opp_vec <- c(opp_vec,class_names[2])
  }else{
    opp_vec <- c(opp_vec,class_names[1])
  }
  return(opp_vec)
}

localNPDR_predOpposite_scores <- function(data, 
                                            test_idx_vec, 
                                            pred_colname="pred", 
                                            actual_colname="class",
                                            pred_rm_cols=c("class","pred_type"),
                                            actual_rm_cols=c("pred","pred_type"),
                                            #local npdr args
                                            nbd_metric="precomputed",
                                            dist_mat,
                                            lambda="lambda.1se",
                                            probability_data = F,
                                            knn="kmax",
                                            prob_cols_vec=NULL, 
                                            prob_df=NULL){
  # return lists for local NPDR results for predicted class and the opposite class
  ret_pred_list <- list()
  ret_opp_list <- list()
  # get pred, class col idx 
  pred_col_idx <- which(colnames(data)==pred_colname)
  actual_col_idx <- which(colnames(data)==actual_colname)
  
  pred_rm_idx <- which(colnames(data) %in% pred_rm_cols)
  actual_rm_idx <- which(colnames(data) %in% actual_rm_cols)
  class_names <- names(table(data[,actual_col_idx]))
  # get vector for actual class and predicted class labels
  actual_full_lab_vec <- data[,actual_col_idx]
  pred_full_lab_vec <- data[,pred_col_idx]
  #head(pred_full_lab_vec)
  #[1] 0 1 1 1 0 0
  #actual_lab_vec <- data[test_idx_vec, actual_col_idx]
  #head(actual_lab_vec)
  # [1] 1 1 1 1 1 1
  #pred_lab_vec <- data[test_idx_vec, pred_col_idx]
  #head(pred_lab_vec)
  # [1] 0 0 0 0 0 1
  # make a vector of opposite predictions than pred_lab_vec
  # function not worrking, use class names, will only work for 2 classes right now
  #opp_vec <- as.factor(sapply(pred_lab_vec,opposite_vector,class_names=class_names))
  #opp_full_vec <- as.factor(sapply(pred_full_lab_vec,opposite_vector,class_names=class_names))
  #head(opp_full_vec)
  # [1] 1 0 0 0 1 1
  
  opp_full_vec <- pred_full_lab_vec # copy pred vec first
  first_class_idx <- which(pred_full_lab_vec==class_names[1])
  second_class_idx <- which(pred_full_lab_vec==class_names[2])
  opp_full_vec[first_class_idx] <- class_names[2] # give first class pred second class name
  opp_full_vec[second_class_idx] <- class_names[1] # vice versa
  opp_full_vec <- as.factor(opp_full_vec)
  #head(opp_full_vec)
  # [1] 1 0 0 0 1 1
  #head(as.factor(opp_full_vec))
  # [1] 1 1 1 1 1 0
  #length(opp_vec)
  data_opp <- data#[test_idx_vec,]
  data_opp[,actual_col_idx] <- opp_full_vec # now class col of this df is the opposite of pred
  #head(opp_full_vec)
  #head(data_opp)
  #colnames(data_opp)[actual_col_idx]<-paste("pseudo_",actual_colname,sep="")
  #dim(data_opp)
  #data_opp$class[test_idx_vec] <- opp_vec # now class col of this df is the opposite of pred
  
  if(!is.null(knn)){
    if(knn=="kmax"){
      knn = nrow(data)-1
    }else{
      knn = knnSURF.balanced(data[,actual_col_idx])
    }
  }
  
  for(i in seq(1,length(test_idx_vec))){
    test_idx <- test_idx_vec[i]
    print(paste("Sample ",i,"of ",length(test_idx_vec),"..."),sep="")
    # local npdr for predicted class label
    pred_list <- list()
    tryCatch({
      localNPDR_pred <- local_npdr(data=data, class_idx=pred_col_idx, 
                                 dist_mat = dist_mat,
                                 nbd_metric = "precomputed", 
                                 ss_idx= test_idx, 
                                 lambda=lambda, 
                                 rm_cols = pred_rm_cols, knn=knn)
      # [1] "knn"                "neighbor_pairs_idx" "ss_class_diff"      "ss_diff_mat"       
      # [5] "nbd_balance"        "weights"            "lambdas"            "coeffs" 
      pred_list[[1]] <- localNPDR_pred$knn
      pred_list[[2]] <- localNPDR_pred$nbd_balance
      pred_list[[3]] <- localNPDR_pred$lambdas
      pred_list[[4]] <- localNPDR_pred$coeffs
    
      # calculate total score
      total_pred_score <- sum(localNPDR_pred$coeffs$s1)
      pred_list[[5]] <- total_pred_score
    
      # make dfs for plotting, add to pred_list and actual_list
      localNPDR_pred_df <- as.data.frame(t(localNPDR_pred$coeffs$s1))
      colnames(localNPDR_pred_df) <- localNPDR_pred$coeffs$att
    
      # add actual and predicted classes, probabilities and pred_type
      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- data[test_idx, actual_col_idx]
      colnames(localNPDR_pred_df)[dim(localNPDR_pred_df)[2]] <- actual_colname
    
      }, error = function(e){message(paste("An error occurred for sample", i,":\n"), e)
        return(localNPDR_pred=NA)})
    
    # now use the opposite class as the class column
    opp_list <- list()
    tryCatch({
      localNPDR_opp <- local_npdr(data=data_opp, class_idx=actual_col_idx, 
                                   dist_mat = dist_mat,
                                   nbd_metric = nbd_metric, 
                                   ss_idx = test_idx, 
                                   lambda=lambda, 
                                   rm_cols = actual_rm_cols, knn=knn)
      opp_list[[1]] <- localNPDR_opp$knn
      opp_list[[2]] <- localNPDR_opp$nbd_balance
      opp_list[[3]] <- localNPDR_opp$lambdas
      opp_list[[4]] <- localNPDR_opp$coeffs
    
      # total score
      total_opp_score <- sum(localNPDR_opp$coeffs$s1)
      opp_list[[5]] <- total_opp_score
    
      localNPDR_opp_df <- as.data.frame(t(localNPDR_opp$coeffs$s1))
      colnames(localNPDR_opp_df) <- localNPDR_opp$coeffs$att
    
      localNPDR_opp_df[,dim(localNPDR_opp_df)[2]+1] <- data_opp[test_idx, actual_col_idx]
      colnames(localNPDR_opp_df)[dim(localNPDR_opp_df)[2]] <- paste("pseudo_",actual_colname,sep="")
    
      }, error = function(e){message(paste("An error occurred for sample", i,":\n"), e)
        return(localNPDR_opp=NA)})
    
    pred_na <- !is.na(localNPDR_pred)[1] # false if NA
    opp_na <- !is.na(localNPDR_opp)[1] # false if NA
    data_na <- pred_na | opp_na # true if both have data
    
    prob_idx_vec <- which(colnames(prob_df) %in% prob_cols_vec)
    
    # probability data for predicted data
    if(probability_data && pred_na){
      
      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[1]]
      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[2]]
      colnames(localNPDR_pred_df)[c(dim(localNPDR_pred_df)[2]-1,dim(localNPDR_pred_df)[2])] <- prob_cols_vec

      localNPDR_pred_df[,dim(localNPDR_pred_df)[2]+1] <- data[test_idx,pred_col_idx]
      colnames(localNPDR_pred_df)[dim(localNPDR_pred_df)[2]] <- pred_colname
      #localNPDR_pred_df
      #     mainvar9      var38   mainvar3    mainvar5      var23        var24        var54      intvar6
      # 1 0.02204378 0.02151776 0.02014423 -0.01788702 0.01639048 -0.004402344 -0.002505678 -0.000989372
      #   class    prob_0    prob_1 pred
      # 1     1 0.2675697 0.7324303    1
      localNPDR_pred_df$total_local_score <- total_pred_score
      
      # created melted dfs for plotting 
      if(probability_data){
        pred_melted <- melt(rev(localNPDR_pred_df),
                            id=c(actual_colname,pred_colname,prob_cols_vec,"total_local_score")) 
      }else{
        pred_melted <- melt(rev(localNPDR_pred_df),
                            id=c(actual_colname,pred_colname,"total_local_score")) 
      }
      # get sign of importance for plot and add to melted dfs
      pred_sign_vec<-rep(0,dim(pred_melted)[1])
      pred_sign_vec[which(pred_melted$value<=0)]<-"Contradicts"
      pred_sign_vec[which(pred_melted$value>0)]<-"Supports"
      pred_melted$sign <- pred_sign_vec
      colnames(pred_melted)[which(colnames(pred_melted)=="value")]<-"local_score"
      #head(pred_melted)
      #  class pred    prob_0    prob_1 variable  local_score        sign
      # 1     1    1 0.3250172 0.6749828  intvar6 -0.008855199 Contradicts
      # 2     1    1 0.3250172 0.6749828    var54 -0.013461589 Contradicts
      # 3     1    1 0.3250172 0.6749828    var24 -0.046561488 Contradicts
      # 4     1    1 0.3250172 0.6749828    var23  0.155782600    Supports
      # 5     1    1 0.3250172 0.6749828 mainvar5 -0.170309330 Contradicts
      # 6     1    1 0.3250172 0.6749828 mainvar3  0.204901707    Supports
      
      pred_list[[6]] <- pred_melted
      names(pred_list) <- c("knn","nbd_balance","lambdas","coeffs","total_local_score","melted_df")
      }else{ # end if no NAs
        pred_list <- NA
      }
    
    # opposite of prediction
    if(probability_data && opp_na){
      
      localNPDR_opp_df[,dim(localNPDR_opp_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[1]]
      localNPDR_opp_df[,dim(localNPDR_opp_df)[2]+1] <- prob_df[test_idx,prob_idx_vec[2]]
      colnames(localNPDR_opp_df)[c(dim(localNPDR_opp_df)[2]-1,dim(localNPDR_opp_df)[2])] <- prob_cols_vec
      
      localNPDR_opp_df[,dim(localNPDR_opp_df)[2]+1] <- data[test_idx,pred_col_idx]
      colnames(localNPDR_opp_df)[dim(localNPDR_opp_df)[2]] <- pred_colname
      #localNPDR_actual_df
      localNPDR_opp_df$total_local_score <- total_opp_score
      if(probability_data){
        opp_melted <- melt(rev(localNPDR_opp_df),
        id=c(paste("pseudo_",actual_colname,sep=""),pred_colname,prob_cols_vec,"total_local_score")) 
      }else{
        opp_melted <- melt(rev(localNPDR_actual_df),
        id=c(paste("pseudo_",actual_colname,sep=""),pred_colname,"total_local_score")) 
      }
      
      opp_sign_vec<-rep(0,dim(opp_melted)[1])
      opp_sign_vec[which(opp_melted$value<=0)]<-"Contradicts"
      opp_sign_vec[which(opp_melted$value>0)]<-"Supports"
      opp_melted$sign <- opp_sign_vec
      colnames(opp_melted)[which(colnames(opp_melted)=="value")]<-"local_score"
      
      opp_list[[6]] <- opp_melted
      names(opp_list) <- c("knn","nbd_balance","lambdas","coeffs","total_local_score","melted_df")
    }else{
      opp_list <- NA
    }
    ret_pred_list[[i]] <- pred_list
    ret_opp_list[[i]] <- opp_list
  }
  ret_list <- list()
  ret_list[[1]] <- ret_pred_list
  ret_list[[2]] <- ret_opp_list
  names(ret_list) <- c("pred","opposite")
  return(ret_list)
}


local_importance_plot <- function(melted_df, actual_colname="class",
                                  class_names = c("0","1"),
                                  pred_colname="pred",
                                  main_title="Single-sample variable importance for NPDR-LURF selected features",
                                  analysis, 
                                  caption="local importance calculated using NPDR-Ridge (lambda.1se) with knn-max",
                                  var_order=NULL){
  # this function only works for two classes right now
  # assumes probability data provided
  
  # if var order specified, reorder rows of melted_df
  if(!is.null(var_order)){
    #reorder according to var_order
    reordered_df <- melted_df %>% mutate(variable=factor(variable,levels=rev(var_order))) %>% 
      arrange(variable)
    melted_df <- reordered_df
  }
  
  #classes <- names(table(melted_df[,which(colnames(melted_df)==actual_colname)]))
  classes <- class_names
  prob_cols <- colnames(melted_df)[grep("prob",colnames(melted_df))]
  # prob_className must be how prob column appears
  first_class_name <- strsplit(prob_cols[grep(classes[1],prob_cols)],"_")[[1]][2]
  second_class_name <- strsplit(prob_cols[grep(classes[2],prob_cols)],"_")[[1]][2]
  first_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[1])]
  second_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[2])]
  
  p <- ggplot(melted_df,  aes(x=variable,y=local_score,fill=sign)) +  
    geom_bar(position="stack",stat="identity") +
    coord_flip() + 
    scale_fill_manual(values=c("red3","dodgerblue3")) +
    geom_hline(yintercept=0)  + theme_bw() +
    labs(title=main_title,
         subtitle=paste("Analysis: ", analysis, "\t total_local_score = ", round(melted_df$total_local_score[1],4),
                        "\n","Class: ","\n", "\t"," actual = ",melted_df[1,which(colnames(melted_df)==actual_colname)],
                        "\t \t predicted = ", melted_df[1,which(colnames(melted_df)==pred_colname)],
                        "\nProbability (percent votes): ", "\n","\t ",first_class_name, " = ",
                        round(first_class_prob,4),
                        "\t \t ",second_class_name, " = ", round(second_class_prob,4),
                        sep=""), caption=caption)
  p
  return(p)
}

local_importance_plot_allSupporting <- function(melted_df, actual_colname="class",
                                  class_names = c("0","1"),
                                  pred_colname="pred",
                                  main_title="Single-sample variable importance for NPDR-LURF selected features",
                                  analysis, 
                                  caption="local importance calculated using NPDR-Ridge (lambda.1se) with knn-max",
                                  var_order=NULL){
  # this function only works for two classes right now
  # assumes probability data provided
  
  # if var order specified, reorder rows of melted_df
  if(!is.null(var_order)){
    #reorder according to var_order
    reordered_df <- melted_df %>% mutate(variable=factor(variable,levels=rev(var_order))) %>% 
      arrange(variable)
    melted_df <- reordered_df
  }
  
  #classes <- names(table(melted_df[,which(colnames(melted_df)==actual_colname)]))
  classes <- class_names
  prob_cols <- colnames(melted_df)[grep("prob",colnames(melted_df))]
  # prob_className must be how prob column appears
  first_class_name <- strsplit(prob_cols[grep(classes[1],prob_cols)],"_")[[1]][2]
  second_class_name <- strsplit(prob_cols[grep(classes[2],prob_cols)],"_")[[1]][2]
  first_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[1])]
  second_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[2])]
  
  p <- ggplot(melted_df,  aes(x=variable,y=local_score,fill=sign)) +  
    geom_bar(position="stack",stat="identity") +
    coord_flip() + 
    scale_fill_manual(values=c("dodgerblue3")) +
    geom_hline(yintercept=0)  + theme_bw() +
    labs(title=main_title,
         subtitle=paste("Analysis: ", analysis, "\t total_local_score = ", round(melted_df$total_local_score[1],4),
                        "\n","Class: ","\n", "\t"," actual = ",melted_df[1,which(colnames(melted_df)==actual_colname)],
                        "\t \t predicted = ", melted_df[1,which(colnames(melted_df)==pred_colname)],
                        "\nProbability (percent votes): ", "\n","\t ",first_class_name, " = ",
                        round(first_class_prob,4),
                        "\t \t ",second_class_name, " = ", round(second_class_prob,4),
                        sep=""), caption=caption)
  p
  return(p)
}


local_importance_plot_allContradicting <- function(melted_df, actual_colname="class",
                                                class_names = c("0","1"),
                                                pred_colname="pred",
                                                main_title="Single-sample variable importance for NPDR-LURF selected features",
                                                analysis, 
                                                caption="local importance calculated using NPDR-Ridge (lambda.1se) with knn-max",
                                                var_order=NULL){
  # this function only works for two classes right now
  # assumes probability data provided
  
  # if var order specified, reorder rows of melted_df
  if(!is.null(var_order)){
    #reorder according to var_order
    reordered_df <- melted_df %>% mutate(variable=factor(variable,levels=rev(var_order))) %>% 
      arrange(variable)
    melted_df <- reordered_df
  }
  
  #classes <- names(table(melted_df[,which(colnames(melted_df)==actual_colname)]))
  classes <- class_names
  prob_cols <- colnames(melted_df)[grep("prob",colnames(melted_df))]
  # prob_className must be how prob column appears
  first_class_name <- strsplit(prob_cols[grep(classes[1],prob_cols)],"_")[[1]][2]
  second_class_name <- strsplit(prob_cols[grep(classes[2],prob_cols)],"_")[[1]][2]
  first_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[1])]
  second_class_prob <- melted_df[1,which(colnames(melted_df)==prob_cols[2])]
  
  p <- ggplot(melted_df,  aes(x=variable,y=local_score,fill=sign)) +  
    geom_bar(position="stack",stat="identity") +
    coord_flip() + 
    scale_fill_manual(values=c("red3")) +
    geom_hline(yintercept=0)  + theme_bw() +
    labs(title=main_title,
         subtitle=paste("Analysis: ", analysis, "\t total_local_score = ", round(melted_df$total_local_score[1],4),
                        "\n","Class: ","\n", "\t"," actual = ",melted_df[1,which(colnames(melted_df)==actual_colname)],
                        "\t \t predicted = ", melted_df[1,which(colnames(melted_df)==pred_colname)],
                        "\nProbability (percent votes): ", "\n","\t ",first_class_name, " = ",
                        round(first_class_prob,4),
                        "\t \t ",second_class_name, " = ", round(second_class_prob,4),
                        sep=""), caption=caption)
  p
  return(p)
}

# add Supports/Contradicts col to melted dfs for plotting
sign_col <- function(melted_df,score_colname="local_score"){
  sign_vec <- c()
  score_ind <- which(colnames(melted_df)==score_colname)
  for(i in seq(1,dim(melted_df)[1])){
    curr_score <- melted_df[i,score_ind]
    if(curr_score <= 0){
      sign_vec <- c(sign_vec,"Contradicts")
    }else{
      sign_vec <- c(sign_vec,"Supports")
    }
  }
  melted_df$sign <- sign_vec
  return(melted_df)
}



localNPDR_predTF_scores <- function(pred_data, test_idx_vec,
                                      pred_colname="pred", class_colname="class",
                                      dist_mat, nbd_metric, lambda="lambda.1se", knn=NULL, 
                                      rm_cols = c("pred_type"), verbose=F){
  # get pred col index and class col index
  pred_idx <- which(colnames(pred_data)==pred_colname)
  class_idx <- which(colnames(pred_data)==class_colname)
  rm_col_idx <- which(colnames(pred_data) %in% rm_cols)
  # knn="kmax" option should not be default
  if (!is.null(knn)){
    if (knn=="kmax"){
      knn = nrow(pred_data)-1
    }else{
      knn = knnSURF.balanced(pred_data[,class_idx])
    }
  }
  
  # need actual class info from model (random forest rf_fit)
  misclass_idx = which(pred_data[,pred_idx] != pred_data[,class_idx])
  false_vec = c()
  true_vec = c()
  # loop over pred label
  #filtered_pred_data <- pred_data[,-rm_col_idx]
  #filt_pred_idx <- which(colnames(filtered_pred_data)==pred_colname)
  for (SSidx in seq(1,length(pred_data[,pred_idx]))){
    if (verbose){
      cat("sample index: ",SSidx," of ",length(pred_data[,pred_idx]), "\n")
    }
    if (SSidx %in% test_idx_vec){
      #sim_train_select = data %>% select(c(npdr_global$variable,"class"))
      tryCatch({
        scores_local <- local_npdr(data=pred_data, 
                                   #class_idx=(length(npdr_global$variable)+1), 
                                   class_idx = pred_idx, # use predicted label
                                   dist_mat = dist_mat,
                                   nbd_metric = nbd_metric, 
                                   ss_idx= SSidx, 
                                   lambda=lambda, 
                                   rm_cols = rm_cols, 
                                   knn=knn)
        # add up the local scores for the global variables
        total_local = sum(scores_local$coeffs %>% 
                            #filter(att %in% npdr_global$variable) %>%
                            arrange(-s1) %>% select(s1))
        # if misclassified, add to false_vec
        if (SSidx %in% misclass_idx){
          false_vec = c(false_vec, total_local)
        } else{
          true_vec = c(true_vec, total_local)
        }
      },
      error = function(e){message(paste("An error occurred for sample", SSidx,":\n"), e)})
   
    } # end if idx in test_idx_vec
  } #end for
  return(list(true_vec=true_vec, false_vec=false_vec))
} # end fn

# loop over the results and add to a final results dataframe
final_local_data <- function(local_train_list, local_test_list,
                             opposite_local_train_list, opposite_local_test_list,
                             positive_class=0, class_colname = "class", 
                             class_names = c("1","0"),
                             opp_class_colname = "pseudo_class",
                             pred_colname = "pred",
                             prob_colnames = c("prob_0","prob_1"),
                             train_idx_vec, test_idx_vec,
                             lambda = "lambda.1se",
                             id_vec = NULL){ #input: localNPDR_list$pred or localNPDR_list$opposite
  # info for each sample
  # names(local_list[[1]])
  # [1] "knn"               "nbd_balance"       "lambdas"           "coeffs"            "total_local_score"
  # [6] "melted_df"   
  
  # make train/test label vec
  train_lab_vec <- rep("train",length(train_idx_vec))
  test_lab_vec <- rep("test",length(test_idx_vec))
  
  train_test_lab_vec <- c(train_lab_vec,test_lab_vec)
  
  # intialize df with dims of list and data
  numRows <- length(local_train_list) + length(local_test_list) # total number of samples
  numVars <- length(local_train_list[[1]]$coeffs$att)
  
  vars <- as.character(local_train_list[[1]]$melted_df$variable)
  opp_vars <- sapply(vars,function(x){paste("opp_",x,sep="")})
  
  ret_df <- as.data.frame(matrix(rep(NA,numRows*(numVars*2 + 14)),nrow=numRows)) # 14 = number of label/meta cols
  # dim(ret_df) # [1] 500  34
  colnames(ret_df) <- c("id","train_test","pred_type","class","pred", prob_colnames[1],prob_colnames[2],
                        "knn","nbd_bal",lambda, vars,"total_local_score",
                        "opp_class",paste("opp_",lambda,sep=""),opp_vars,"opp_total_local_score")
  #head(ret_df)
  if(is.null(id_vec)){
    # use row nums
    id_vec <- seq(1,numRows)
  }
  
  local_list <- c(local_train_list, local_test_list)
  opposite_local_list <- c(opposite_local_train_list, opposite_local_test_list)
  
  for(i in seq(1,numRows)){
    curr_id <- id_vec[i]
    curr_train_test <- train_test_lab_vec[i]
    # local npdr data
    curr_knn <- local_list[[i]]$knn
    curr_nbd_bal <- local_list[[i]]$nbd_balance
    curr_lambda <- local_list[[i]]$lambdas[lambda]
    # coeffs and labels
    curr_melted_df <- local_list[[i]]$melted_df
    curr_local_scores <- curr_melted_df$local_score
    # total local NPDR score
    curr_total_local_score <- local_list[[i]]$total_local_score
    # class and predictedc class
    curr_class <- curr_melted_df[,which(colnames(curr_melted_df)==class_colname)][1]
    curr_pred <- curr_melted_df[,which(colnames(curr_melted_df)==pred_colname)][1]
    # probabilities
    prob_class1 <- curr_melted_df[,which(colnames(curr_melted_df)==prob_colnames[1])][1]
    prob_class2 <- curr_melted_df[,which(colnames(curr_melted_df)==prob_colnames[2])][1]
    
    # local npdr for opposite of predicted class
    opp_melted_df <- opposite_local_list[[i]]$melted_df
    opp_lambdas <- opposite_local_list[[i]]$lambdas[lambda]
    opp_total_local_score <- opposite_local_list[[i]]$total_local_score
    opp_local_scores <- opp_melted_df$local_score
    opp_class <- opp_melted_df[,which(colnames(opp_melted_df)==opp_class_colname)][1]
    
    # add predType back
    correct_pred <- curr_class == curr_pred
    if(correct_pred){ # is it true positive or true negative
      TP <- curr_class == positive_class
      if(TP){ # true positive
        pred_type <- "TP"
      }else{ # true negative
        pred_type <- "TN"
      }
    }else{ # not a correct prediction
      FP <- curr_pred == positive_class
      if(FP){ # false positive
        pred_type <- "FP"
      }else{ # false negative
        pred_type <- "FN"
      }
    }
    # make row of these vals
    # colnames(ret_df)
    # [1] "id"                    "train_test"            "pred_type"             "class"                
    # [5] "pred"                  "prob_0"                "prob_1"                "knn"                  
    # [9] "nbd_bal"               "lambda.1se"            "intvar4"               "mainvar6"             
    # [13] "mainvar8"              "mainvar3"              "mainvar9"              "mainvar10"            
    # [17] "mainvar7"              "mainvar1"              "mainvar4"              "mainvar5"             
    # [21] "total_local_score"     "opp_class"             "opp_lambda.1se"        "opp_intvar4"          
    # [25] "opp_mainvar6"          "opp_mainvar8"          "opp_mainvar3"          "opp_mainvar9"         
    # [29] "opp_mainvar10"         "opp_mainvar7"          "opp_mainvar1"          "opp_mainvar4"         
    # [33] "opp_mainvar5"          "opp_total_local_score"
    curr_row <- unlist(c(curr_id, curr_train_test, pred_type, curr_class, curr_pred,
                         round(prob_class1,4), round(prob_class2,4), curr_knn, round(curr_nbd_bal,4), curr_lambda,
                         curr_local_scores, curr_total_local_score, as.character(opp_class), opp_lambdas,
                         opp_local_scores, opp_total_local_score))
    # add to df
    ret_df[i,]<-curr_row
    head(ret_df)
  }# end for
  return(ret_df)
}

####################
# Functions for URFP
####################
extract_proximity_oob = function(fit, olddata) {
  pred = predict(fit, olddata, type = "terminalNodes")$predictions
  prox = matrix(NA, nrow(pred), nrow(pred))
  ntree = ncol(pred)
  n = nrow(prox)
  
  if (is.null(fit$inbag.counts)) {
    stop("call ranger with keep.inbag = TRUE")
  }
  
  # Get inbag counts
  inbag = simplify2array(fit$inbag.counts)
  
  for(i in seq(1,n-1)){
    for(j in seq(i+1,n)){
      # Use only trees where both obs are OOB
      tree_idx = inbag[i, ] == 0 & inbag[j, ] == 0
      prox[i, j] = sum(pred[i, tree_idx] == pred[j, tree_idx]) / sum(tree_idx)
    }
  }
  
  prox
}

urfp_dist <- function(train.dat, rm.ind,nthreads=4){
  
  synth <- as.data.frame(lapply(train.dat[,-rm.ind], 
                                function(x) {sample(x, length(x), replace = TRUE)}))
  
  synth.dat<-rbind(data.frame(y="real",train.dat[,-rm.ind]),
                   data.frame(y="synth",synth))
  synth.dat$y<-as.factor(synth.dat$y)
  # URF
  urf.fit <- ranger(y~., synth.dat, keep.inbag = TRUE,
                    num.trees=5000, #mtry=2, 
                    importance="none",
                    local.importance = F, 
                    num.threads=nthreads)
  prox <- extract_proximity_oob(urf.fit, synth.dat)[1:nrow(train.dat), 
                                                    1:nrow(train.dat)]
  ## process this upper tri matrix to a full matrix
  # copy matrix?
  #full_mat <- sim_train_urfp_dist
  prox[lower.tri(prox)] <- 0
  #tail(full_mat)
  # add transpose
  prox <- prox + t(prox)
  #tail(full_mat)
  #isSymmetric(full_mat)
  ###
  urfp.dist<-sqrt(1-prox)
  # make sure diag is 0's or whatever needed
  diag(urfp.dist) <- 0
  return(urfp.dist)
}

#####################
# probability RF
#####################
# only for two classes right now
probability_rf <- function(train_dat, test_dat, class_idx, num_trees, mtry, 
                           splitrule, min_node_size, nthreads=4){
  class_names <- names(table(train_dat[,class_idx]))
  ret_list <- list()
  # probability forest 
  prob_rf <- ranger(x=train_dat[,-class_idx],
                               y=train_dat[,class_idx],
                               keep.inbag = TRUE,
                               num.trees = num_trees,
                               mtry = mtry, 
                               importance = "permutation", 
                               splitrule = splitrule,
                               min.node.size = min_node_size,
                               class.weights = as.numeric(c(1/table(train_dat[,class_idx]))),
                               scale.permutation.importance = T, 
                               probability = T,
                               local.importance = F, 
                               num.threads = nthreads)
  prob_imp <- prob_rf$variable.importance
  prob_imp <- sort(prob_imp,decreasing = T)
  ret_list[[1]] <- prob_rf
  ret_list[[2]] <- prob_imp
  
  prob_train_pred <- prob_rf$predictions
  #class(prob_train_pred) #matrix, convert to df
  prob_train_pred_df <- as.data.frame(prob_train_pred)
  # TODO: lapply for nclasses?
  colnames(prob_train_pred_df) <- c(paste("prob_",class_names[1],sep=""),
                                    paste("prob_",class_names[2],sep=""))
  #head(prob_train_pred_df)
  prob_train_acc <- 1-prob_rf$prediction.error
  ret_list[[3]] <- prob_train_acc

  # add predictions to copy of training data
  train_prob <- train_dat
  # TODO: will need to adjust for more than two classes
  train_prob$prob1 <- prob_train_pred_df[,1]
  train_prob$prob2 <- prob_train_pred_df[,2]
  replace_colnames_idx <- which(colnames(train_prob) %in% c("prob1","prob2"))
  colnames(train_prob)[replace_colnames_idx] <- colnames(prob_train_pred_df)
  #head(train_prob)
  ret_list[[4]] <- train_prob
  
  # get predicted probabilities for test data
  predProb_test<-predict(prob_rf,data = test_dat[,-class_idx])
  prob_test_pred_df <- as.data.frame(predProb_test$predictions)
  colnames(prob_test_pred_df) <- c(paste("prob_",class_names[1],sep=""),
                                   paste("prob_",class_names[2],sep=""))
  #head(prob_test_pred_df)
  # add to copy of test data
  test_prob <- test_dat
  test_prob$prob1 <- prob_test_pred_df[,1]
  test_prob$prob2 <- prob_test_pred_df[,2]
  replace_colnames_idx <- which(colnames(test_prob) %in% c("prob1","prob2"))
  colnames(test_prob)[replace_colnames_idx] <- colnames(prob_test_pred_df)
  #head(test_prob)
  ret_list[[5]] <- test_prob
  
  names(ret_list) <- c("prob_rf","var_imp","train_acc","train_pred","test_pred")
  return(ret_list)
}


############################
# summary analysis functions
#########################

# function for flagging samples based on thresholds - prob AND local_score
flag_predictions <- function(test_samples_df, prob_thresh, total_local_score_thresh,id_name="id"){
  flagged_id <- c()
  flagged_pred_type <- c()
  
  #prob_thresh <- 0.75
  #score_thresh <- 0.25 
  id_ind <- which(colnames(test_samples_df)==id_name)
  for(i in seq(1,length(test_samples_df[,id_ind]))){
    prob <- test_samples_df$prob_predicted[i]
    score <- test_samples_df$total_local_score[i]
    flag <- (prob <= prob_thresh) & (score <= total_local_score_thresh)
    if(flag){
      flagged_id <- c(flagged_id, test_samples_df[i,id_ind])
      flagged_pred_type <- c(flagged_pred_type, test_samples_df$pred_type[i])
    }
  }
  flagged_df <- cbind.data.frame(flagged_id,flagged_pred_type)
  colnames(flagged_df) <- c(id_name,"pred_type")
  return(flagged_df)
}


# function for flagging samples based on thresholds - prob OR local_score (flags more samples)
flag_predictions2 <- function(test_samples_df, prob_thresh, total_local_score_thresh){
  flagged_id <- c()
  flagged_pred_type <- c()
  
  #prob_thresh <- 0.75
  #score_thresh <- 0.25  
  for(i in seq(1,length(test_samples_df$id))){
    prob <- test_samples_df$prob_predicted[i]
    score <- test_samples_df$total_local_score[i]
    flag <- (prob <= prob_thresh) | (score <= total_local_score_thresh)
    if(flag){
      flagged_id <- c(flagged_id, test_samples_df$id[i])
      flagged_pred_type <- c(flagged_pred_type, test_samples_df$pred_type[i])
    }
  }
  flagged_df <- cbind.data.frame(flagged_id,flagged_pred_type)
  colnames(flagged_df) <- c("id","pred_type")
  return(flagged_df)
}

# take RF local importance scores and add them for each sample: total_local_RF_imp
total_local_RF_imp <- function(localRF_dat,id_col="id",class_col="class",prob_cols=c("prob_0","prob_1")){
  num_rows <- nrow(localRF_dat)
  rm_cols <- which(colnames(localRF_dat) %in% c(id_col,class_col,prob_cols))
  #local_dat <- localRF_dat[,-rm_cols]
  # loop over rows, add scores
  total_RF_vec <- c()
  for(i in seq(1,num_rows)){
    total_score <- sum(localRF_dat[i,-rm_cols])
    total_RF_vec <- c(total_RF_vec,total_score)
  }
  # add to df
  localRF_dat$total_local_score <- total_RF_vec
  return(localRF_dat)
}

