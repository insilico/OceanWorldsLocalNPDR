library(dplyr)
library(ggplot2)
library(viridis)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
###################
# 1. read in data
#################
sim77_final_local_df <- read.csv("./data/sim77/sim77_final_local_data.csv")
head(sim77_final_local_df)[,1:11]
#   id train_test pred_type class pred prob_0 prob_1 knn nbd_bal lambda.1se       intvar3
# 1  1      train        FP     1    0 0.8104 0.1896 299  0.5151  4.3731109 -1.358925e-03
# 2  2      train        TN     1    1 0.4420 0.5580 299  0.4883  3.7219617  1.069035e-02
# 3  3      train        TN     1    1 0.3515 0.6485 299  0.4883  3.7225509 -2.250416e-03
# 4  4      train        TN     1    1 0.4443 0.5557 299  0.4883  4.4821241  9.259487e-03
# 5  5      train        FP     1    0 0.6286 0.3714 299  0.5151  0.3276956 -7.038122e-02
# 6  6      train        FP     1    0 0.5154 0.4846 299  0.5151  2.5314426  9.171402e-05



sim77_train_FPs <- sim77_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FP")
sim77_train_FNs <- sim77_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FN")
sim77_train_TNs <- sim77_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TN")
sim77_train_TPs <- sim77_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TP")

dim(sim77_train_FPs)[1]
# [1] 35
dim(sim77_train_FNs)[1]
# [1] 18
dim(sim77_train_TNs)[1]
# [1] 109
dim(sim77_train_TPs)[1]
# [1] 78

sim77_test_FPs <- sim77_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FP")
sim77_test_FNs <- sim77_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FN")
sim77_test_TNs <- sim77_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TN")
sim77_test_TPs <- sim77_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TP")

dim(sim77_test_FPs)[1]
# [1] 13
dim(sim77_test_FNs)[1]
# [1] 4
dim(sim77_test_TNs)[1]
# [1] 23
dim(sim77_test_TPs)[1]
# [1] 20



sim79_final_local_df <- read.csv("./data/sim79/sim79_final_local_data.csv")
head(sim79_final_local_df)[,1:11]
#   id train_test pred_type class pred prob_0 prob_1 knn nbd_bal        lambda.1se                var14
# 1  1      train        FP     1    0 0.7165 0.2835 299  0.4415  3.09033439353415 -0.00417140313880278
# 2  2      train        FP     1    0 0.5761 0.4239 299  0.4415 0.924677488816821 -0.00608189698839856
# 3  3      train        FP     1    0 0.5218 0.4782 299  0.4415 0.393878818477726  -0.0153401098853608
# 4  4      train        FP     1    0 0.7922 0.2078 299  0.4415  1.09163182981353  0.00200453246831573
# 5  5      train        FP     1    0 0.7452 0.2548 299  0.4415  0.32974120155005   0.0125217613576204
# 6  6      train        FP     1    0 0.5039 0.4961 299  0.4415 0.690955991886808  -0.0187238779528601

sim79_train_FPs <- sim79_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FP")
sim79_train_FNs <- sim79_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FN")
sim79_train_TNs <- sim79_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TN")
sim79_train_TPs <- sim79_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TP")

dim(sim79_train_FPs)[1]
# [1] 39
dim(sim79_train_FNs)[1]
# [1] 2
dim(sim79_train_TNs)[1]
# [1] 105
dim(sim79_train_TPs)[1]
# [1] 94

sim79_test_FPs <- sim79_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FP")
sim79_test_FNs <- sim79_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FN")
sim79_test_TNs <- sim79_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TN")
sim79_test_TPs <- sim79_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TP")

dim(sim79_test_FPs)[1]
# [1] 12
dim(sim79_test_FNs)[1]
# [1] 1
dim(sim79_test_TNs)[1]
# [1] 24
dim(sim79_test_TPs)[1]
# [1] 23


sim85_final_local_df <- read.csv("./data/sim85/sim85_final_local_data.csv")
head(sim85_final_local_df)[,1:11]
#  id train_test pred_type class pred prob_0 prob_1 knn nbd_bal lambda.1se       intvar4
# 1  1      train        TN     1    1 0.3166 0.6834 499   0.507  2.8360566  1.173569e-03
# 2  2      train        TN     1    1 0.2430 0.7570 499   0.507  2.8284170  2.758388e-05
# 3  3      train        TN     1    1 0.2592 0.7408 499   0.507  4.3882358  6.372980e-03
# 4  4      train        TN     1    1 0.4343 0.5657 499   0.507  0.9705588 -5.753545e-03
# 5  5      train        FP     1    0 0.7264 0.2736 499   0.495  5.5267241 -8.192016e-03
# 6  6      train        TN     1    1 0.4086 0.5914 499   0.507  0.7977002  1.625936e-02


sim85_train_FPs <- sim85_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FP")
sim85_train_FNs <- sim85_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FN")
sim85_train_TNs <- sim85_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TN")
sim85_train_TPs <- sim85_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TP")

dim(sim85_train_FPs)[1]
# [1] 32
dim(sim85_train_FNs)[1]
# [1] 31
dim(sim85_train_TNs)[1]
# [1] 168
dim(sim85_train_TPs)[1]
# [1] 169

sim85_test_FPs <- sim85_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FP")
sim85_test_FNs <- sim85_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FN")
sim85_test_TNs <- sim85_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TN")
sim85_test_TPs <- sim85_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TP")

dim(sim85_test_FPs)[1]
# [1] 11
dim(sim85_test_FNs)[1]
# [1] 9
dim(sim85_test_TNs)[1]
# [1] 39
dim(sim85_test_TPs)[1]
# [1] 41





bio_final_local_df <- read.csv("./data/bio/bio_final_local_data.csv")
head(bio_final_local_df)[,1:12]
#     id train_test pred_type  class   pred prob_abiotic prob_biotic knn nbd_bal lambda.1se time_kl_shift fluctanal_prop_r1
# 1 2961      train        TP biotic biotic       0.0330      0.9670 173  0.6301  0.3492910 -1.241192e-05         6.4083995
# 2 2962      train        TP biotic biotic       0.1816      0.8184 173  0.6301  0.8113840 -1.988019e-05        -0.3485142
# 3 2963      train        TP biotic biotic       0.0280      0.9720 173  0.6301  0.8136481 -1.308775e-05         3.6372007
# 4 2965      train        TP biotic biotic       0.0124      0.9876 173  0.6301  1.3094174 -7.248519e-06         2.6123529
# 5 2966      train        TP biotic biotic       0.3101      0.6899 173  0.6301  1.8790507 -9.404060e-06         2.4809398
# 6 2968      train        TP biotic biotic       0.1353      0.8647 173  0.6301  2.5624952 -6.629658e-06         1.3918056 




bio_train_FPs <- bio_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FP")
bio_train_FNs <- bio_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="FN")
bio_train_TNs <- bio_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TN")
bio_train_TPs <- bio_final_local_df %>% filter(train_test=="train") %>% filter(pred_type=="TP")

dim(bio_train_FPs)[1]
# [1] 8
dim(bio_train_FNs)[1]
# [1] 5
dim(bio_train_TNs)[1]
# [1] 81
dim(bio_train_TPs)[1]
# [1] 46

bio_test_FPs <- bio_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FP")
bio_test_FNs <- bio_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="FN")
bio_test_TNs <- bio_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TN")
bio_test_TPs <- bio_final_local_df %>% filter(train_test=="test") %>% filter(pred_type=="TP")

dim(bio_test_FPs)[1]
# [1] 1
dim(bio_test_FNs)[1]
# [1] 2
dim(bio_test_TNs)[1]
# [1] 21
dim(bio_test_TPs)[1]
# [1] 10



### NPDR LURF feature importance 
sim77_global_lurf <- read.csv("./data/sim77/sim77_global_lurf_lam_0_02.csv")
sim79_global_lurf <- read.csv("./data/sim79/sim79_global_lurf_lam_1se.csv")
sim85_global_lurf <- read.csv("./data/sim85/sim85_global_lurf_lam_0_01.csv")
sim77_lurf_feat <- sim77_global_lurf$variable
sim79_lurf_feat <- sim79_global_lurf$variable
sim85_lurf_feat <- sim85_global_lurf$variable
# order of bio lurf features in run0 from ESS paper
bio_lurf_feat <- c("avg_rR45CO244CO2","sd_d18O13C","diff2_acf1","fluctanal_prop_r1","time_kl_shift")

########################################################
# 2. create summary dfs: average scores per prediction type
#########################################################
# sim77 
sim77_summ=sim77_final_local_df %>% group_by(train_test,pred_type,pred) %>% 
  reframe(mean_total_local=mean(total_local_score), mean_prob0=mean(prob_0), 
          mean_prob1=mean(prob_1)) 
sim77_summ_df <- as.data.frame(sim77_summ)
sim77_summ_df
#   train_test pred_type pred mean_total_local mean_prob0 mean_prob1
# 1       test        FN    1       -0.3808570 0.41900000  0.5810000
# 2       test        FP    0        0.2661110 0.66113846  0.3388615
# 3       test        TN    1        0.1557080 0.07175217  0.9282478
# 4       test        TP    0        0.3082902 0.70692000  0.2930800
# 5      train        FN    1       -0.2683580 0.38698889  0.6130111
# 6      train        FP    0        0.3008754 0.69554857  0.3044514
# 7      train        TN    1        0.1769557 0.05898073  0.9410193
# 8      train        TP    0        0.3764564 0.71977051  0.2802295

# sim79
sim79_summ=sim79_final_local_df %>% group_by(train_test,pred_type,pred) %>% 
  reframe(mean_total_local=mean(total_local_score), mean_prob0=mean(prob_0), 
          mean_prob1=mean(prob_1)) 
sim79_summ_df <- as.data.frame(sim79_summ)
sim79_summ_df
#  train_test pred_type pred mean_total_local mean_prob0 mean_prob1
# 1       test        FN    1      -0.18402326 0.33010000  0.6699000
# 2       test        FP    0       0.37106671 0.63990000  0.3601000
# 3       test        TN    1       0.31790620 0.07634167  0.9236542
# 4       test        TP    0       0.30507712 0.69529130  0.3047087
# 5      train        FN    1      -0.07165207 0.47070000  0.5293000
# 6      train        FP    0       0.30905669 0.66693077  0.3330692
# 7      train        TN    1       0.27980575 0.03635524  0.9636448
# 8      train        TP    0       0.35998785 0.67762766  0.3223723

# sim85
sim85_summ=sim85_final_local_df %>% group_by(train_test,pred_type,pred) %>% 
  reframe(mean_total_local=mean(total_local_score), mean_prob0=mean(prob_0), 
          mean_prob1=mean(prob_1)) 
sim85_summ_df <- as.data.frame(sim85_summ)
sim85_summ_df
#   train_test pred_type pred mean_total_local mean_prob0 mean_prob1
# 1       test        FN    1       0.07329656  0.3914556  0.6085444
# 2       test        FP    0       0.11352803  0.6492818  0.3507182
# 3       test        TN    1       0.20937634  0.2551231  0.7448769
# 4       test        TP    0       0.25651445  0.7479415  0.2520561
# 5      train        FN    1       0.08013086  0.4045484  0.5954516
# 6      train        FP    0       0.03039437  0.5883781  0.4116219
# 7      train        TN    1       0.23494723  0.2712536  0.7287464
# 8      train        TP    0       0.22703120  0.7210189  0.2789811


# bio
bio_summ=bio_final_local_df %>% group_by(train_test,pred_type,pred) %>% 
  reframe(mean_total_local=mean(total_local_score), mean_prob_abiotic=mean(prob_abiotic), 
          mean_prob_biotic=mean(prob_biotic)) 
bio_summ_df <- as.data.frame(bio_summ)
bio_summ_df
#   train_test pred_type    pred mean_total_local mean_prob_abiotic mean_prob_biotic
# 1       test        FN abiotic        -23.79130         0.8124000       0.18760000
# 2       test        FP  biotic        -52.76159         0.4450000       0.55500000
# 3       test        TN abiotic        -91.48976         0.9456714       0.05432857
# 4       test        TP  biotic         26.49100         0.2069500       0.79305000
# 5      train        FN abiotic        159.12409         0.6654800       0.33452000
# 6      train        FP  biotic         12.16156         0.3797250       0.62027500
# 7      train        TN abiotic        -15.83697         0.8897309       0.11026914
# 8      train        TP  biotic         81.51798         0.2078130       0.79218696

# save these summary dfs to file
#write.table(sim77_summ_df,"sim77_localNPDR_trainTest_summary2.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_summ_df,"sim79_localNPDR_trainTest_summary2.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_summ_df,"sim85_localNPDR_trainTest_summary2.csv",row.names=F,quote=F,sep=",")
#write.table(bio_summ_df,"bio_localNPDR_trainTest_summary.csv",row.names=F,quote=F,sep=",")

# bio_summ_df <- read.csv("./data/bio/bio_localNPDR_trainTest_summary.csv")

#########################################
# 3. mean_total_local vs pred_type plots
########################################

# sim77 test data
sim77_test_summ <- sim77_summ_df %>% filter(train_test=="test") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim77_test_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim77 test data: local importance vs. prediction type",
          subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 71.7%") + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")

# sim77 train data 
sim77_train_summ <- sim77_summ_df %>% filter(train_test=="train") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim77_train_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim77 train data: local importance vs. prediction type",
          subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF train accuracy = 77.9%")  + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")


# sim79 test data
sim79_test_summ <- sim79_summ_df %>% filter(train_test=="test") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim79_test_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim79 test data: local importance vs. prediction type",
          subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 78.3%") + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")

# sim79 train data 
sim79_train_summ <- sim79_summ_df %>% filter(train_test=="train") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim79_train_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim79 train data: local importance vs. prediction type",
          subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF train accuracy = 82.9%")  + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")


# sim85 test data
sim85_test_summ <- sim85_summ_df %>% filter(train_test=="test") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim85_test_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim85 test data: local importance vs. prediction type",
          subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
          \n RF test accuracy = 80.0%") + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")

# sim85 train data 
sim85_train_summ <- sim85_summ_df %>% filter(train_test=="train") %>% select(c(mean_total_local,pred_type))
ggplot(data=sim85_train_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("sim85 train data: local importance vs. prediction type",
          subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
          \n RF train accuracy = 84.3%")  + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")


# biosignature test data
bio_test_summ <- bio_summ_df %>% filter(train_test=="test") %>% select(c(mean_total_local,pred_type))
ggplot(data=bio_test_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("biosignature test data: local importance vs. prediction type",
          subtitle = "imbalance = 0.64;
          \n RF test accuracy = 91.2%") + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")

# biosignature train data 
bio_train_summ <- bio_summ_df %>% filter(train_test=="train") %>% select(c(mean_total_local,pred_type))
ggplot(data=bio_train_summ, aes(x=pred_type, y=mean_total_local)) + 
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  ggtitle("biosignature train data: local importance vs. prediction type",
          subtitle = "imbalance = 0.64;
          \n RF train accuracy = 90.7%")  + 
  theme_bw() + labs(x="prediction type",y="mean total local importance score")



#######################################################
# 4. probability of predicted class vs pred_type plots
#######################################################

# sim77 test data
sim77_test_prob = sim77_summ_df %>% filter(train_test=="test") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim77_test_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim77 test data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 71.7%", x="prediction type", 
       y="probability of predicted class")

# sim77 train data
sim77_train_prob = sim77_summ_df %>% filter(train_test=="train") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim77_train_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim77 train data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 77.9%", x="prediction type", 
       y="probability of predicted class")


# sim79 test data
sim79_test_prob = sim79_summ_df %>% filter(train_test=="test") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim79_test_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim79 test data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 78.3%", x="prediction type", 
       y="probability of predicted class")

# sim79 train data
sim79_train_prob = sim79_summ_df %>% filter(train_test=="train") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim79_train_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim79 train data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          \n RF test accuracy = 82.9%", x="prediction type", 
       y="probability of predicted class")


# sim85 test data
sim85_test_prob = sim85_summ_df %>% filter(train_test=="test") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim85_test_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim85 test data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
          \n RF test accuracy = 80.0%", x="prediction type", 
       y="probability of predicted class")

# sim85 train data
sim85_train_prob = sim85_summ_df %>% filter(train_test=="train") %>% 
  mutate(prob_predicted=ifelse(pred==1, mean_prob1, mean_prob0)) %>%
  select(pred_type, prob_predicted)

ggplot(data=sim85_train_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("sim85 train data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
          \n RF test accuracy = 84.3%", x="prediction type", 
       y="probability of predicted class")

# biosignature test data
bio_test_prob = bio_summ_df %>% filter(train_test=="test") %>% 
  mutate(prob_predicted=ifelse(pred=="abiotic", mean_prob_abiotic, mean_prob_biotic)) %>%
  select(pred_type, prob_predicted)

ggplot(data=bio_test_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("biosignature test data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.64;
          \n RF test accuracy = 91.2%", x="prediction type", 
       y="probability of predicted class")

# biosignature train data
bio_train_prob = bio_summ_df %>% filter(train_test=="train") %>% 
  mutate(prob_predicted=ifelse(pred=="abiotic", mean_prob_abiotic, mean_prob_biotic)) %>%
  select(pred_type, prob_predicted)

ggplot(data=bio_train_prob, aes(x=pred_type, y=prob_predicted)) + geom_point(size=5) +
  theme_bw() + ggtitle("biosignature train data: 
  predicted class probability vs prediction type") +
  geom_point(size=5,colour=c("red","red","blue","blue")) +
  labs(subtitle = "imbalance = 0.64;
          \n RF test accuracy = 90.7%", x="prediction type", 
       y="probability of predicted class")




############################################################
# 5. mean total local importance vs prediction type boxplots
############################################################

# sim77 true positive
sim77_tp_scores=sim77_final_local_df %>% filter(pred_type=="TP") %>% select(seq(11,18))
sim77_stacked_tp_scores <- stack(sim77_tp_scores)
colnames(sim77_stacked_tp_scores)<-c("mean_total_local_scores","variable")
sim77_reordered_stacked_tp <- sim77_stacked_tp_scores %>% 
  mutate(variable=factor(variable,levels=sim77_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim77_reordered_stacked_tp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim77 true positives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 77.9% / 71.7%",
       x="",y="mean total local importance score")

# sim77 true negative
sim77_tn_scores=sim77_final_local_df %>% filter(pred_type=="TN") %>% select(seq(11,18))
sim77_stacked_tn_scores <- stack(sim77_tn_scores)
colnames(sim77_stacked_tn_scores)<-c("mean_total_local_scores","variable")
sim77_reordered_stacked_tn <- sim77_stacked_tn_scores %>% mutate(variable=factor(variable,levels=sim77_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim77_reordered_stacked_tn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim77 true negatives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          RF train/test accuracy = 77.9% / 71.7%",
       x="",y="mean total local importance score")

# sim77 false positive
sim77_fp_scores=sim77_final_local_df %>% filter(pred_type=="FP") %>% select(seq(11,18))
sim77_stacked_fp_scores <- stack(sim77_fp_scores)
colnames(sim77_stacked_fp_scores) <- c("mean_total_local_scores","variable")
sim77_reordered_stacked_fp <- sim77_stacked_fp_scores %>% mutate(variable=factor(variable,levels=sim77_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim77_reordered_stacked_fp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim77 false positives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
          RF train/test accuracy = 77.9% / 71.7%",
       x="",y="mean total local importance score")


# sim77 false negative
sim77_fn_scores=sim77_final_local_df %>% filter(pred_type=="FN") %>% select(seq(11,18))
sim77_stacked_fn_scores <- stack(sim77_fn_scores)
colnames(sim77_stacked_fn_scores)<-c("mean_total_local_scores","variable")
sim77_reordered_stacked_fn <- sim77_stacked_fn_scores %>% mutate(variable=factor(variable,levels=sim77_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim77_reordered_stacked_fn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim77 false negatives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 77.9% / 71.7%",
       x="",y="mean total local importance score")



#### sim 79
colnames(sim79_final_local_df)
# [1] "id"                    "train_test"            "pred_type"            
# [4] "class"                 "pred"                  "prob_0"               
# [7] "prob_1"                "knn"                   "nbd_bal"              
# [10] "lambda.1se"            "var14"                 "var64"                
# [13] "var67"                 "intvar7"               "intvar8"              
# [16] "mainvar9"              "mainvar8"              "mainvar7"             
# [19] "total_local_score"     "opp_class"             "opp_lambda.1se"       
# [22] "opp_var14"             "opp_var64"             "opp_var67"            
# [25] "opp_intvar7"           "opp_intvar8"           "opp_mainvar9"         
# [28] "opp_mainvar8"          "opp_mainvar7"          "opp_total_local_score"

# sim79 true positive
sim79_tp_scores=sim79_final_local_df %>% filter(pred_type=="TP") %>% select(seq(11,18))
sim79_stacked_tp_scores <- stack(sim79_tp_scores)
colnames(sim79_stacked_tp_scores)<-c("mean_total_local_scores","variable")
sim79_reordered_stacked_tp <- sim79_stacked_tp_scores %>% mutate(variable=factor(variable,levels=sim79_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim79_reordered_stacked_tp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim79 true positives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 82.9% / 78.3%",
       x="",y="mean total local importance score")


# sim79 true negative
sim79_tn_scores=sim79_final_local_df %>% filter(pred_type=="TN") %>% select(seq(11,18))
sim79_stacked_tn_scores <- stack(sim79_tn_scores)
colnames(sim79_stacked_tn_scores)<-c("mean_total_local_scores","variable")
sim79_reordered_stacked_tn <- sim79_stacked_tn_scores %>% mutate(variable=factor(variable,levels=sim79_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim79_reordered_stacked_tn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim79 true negatives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 82.9% / 78.3%",
       x="",y="mean total local importance score")


# sim79 false positive
sim79_fp_scores=sim79_final_local_df %>% filter(pred_type=="FP") %>% select(seq(11,18))
sim79_stacked_fp_scores <- stack(sim79_fp_scores)
colnames(sim79_stacked_fp_scores) <- c("mean_total_local_scores","variable")
sim79_reordered_stacked_fp <- sim79_stacked_fp_scores %>% mutate(variable=factor(variable,levels=sim79_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim79_reordered_stacked_fp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim79 false positives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 82.9% / 78.3%",
       x="",y="mean total local importance score")


# sim79 false negative
sim79_fn_scores=sim79_final_local_df %>% filter(pred_type=="FN") %>% select(seq(11,18))
sim79_stacked_fn_scores <- stack(sim79_fn_scores)
colnames(sim79_stacked_fn_scores)<-c("mean_total_local_scores","variable")
sim79_reordered_stacked_fn <- sim79_stacked_fn_scores %>% mutate(variable=factor(variable,levels=sim79_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim79_reordered_stacked_fn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim79 false negatives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.6; main effects = 1.5; interaction effects = 1.5
           RF train/test accuracy = 82.9% / 78.3%",
       x="",y="mean total local importance score")


#### sim85
# sim85 true positive
colnames(sim85_final_local_df)
# [1] "id"                    "train_test"            "pred_type"            
# [4] "class"                 "pred"                  "prob_0"               
# [7] "prob_1"                "knn"                   "nbd_bal"              
# [10] "lambda.1se"            "intvar4"               "mainvar8"             
# [13] "mainvar7"              "mainvar6"              "mainvar10"            
# [16] "mainvar1"              "mainvar3"              "mainvar4"             
# [19] "mainvar5"              "mainvar9"              "total_local_score"    
# [22] "opp_class"             "opp_lambda.1se"        "opp_intvar4"          
# [25] "opp_mainvar8"          "opp_mainvar7"          "opp_mainvar6"         
# [28] "opp_mainvar10"         "opp_mainvar1"          "opp_mainvar3"         
# [31] "opp_mainvar4"          "opp_mainvar5"          "opp_mainvar9"         
# [34] "opp_total_local_score"


# sim85 true positives
sim85_tp_scores=sim85_final_local_df %>% filter(pred_type=="TP") %>% select(seq(11,20))
sim85_stacked_tp_scores <- stack(sim85_tp_scores)
colnames(sim85_stacked_tp_scores)<-c("mean_total_local_scores","variable")
sim85_reordered_stacked_tp <- sim85_stacked_tp_scores %>% mutate(variable=factor(variable,levels=sim85_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim85_reordered_stacked_tp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim85 true positives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
           RF train/test accuracy = 84.3% / 80.0%",
       x="",y="mean total local importance score")


# sim85 true negative
sim85_tn_scores=sim85_final_local_df %>% filter(pred_type=="TN") %>% select(seq(11,20))
sim85_stacked_tn_scores <- stack(sim85_tn_scores)
colnames(sim85_stacked_tn_scores)<-c("mean_total_local_scores","variable")
sim85_reordered_stacked_tn <- sim85_stacked_tn_scores %>% mutate(variable=factor(variable,levels=sim85_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim85_reordered_stacked_tn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim85 true negatives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
           RF train/test accuracy = 84.3% / 80.0%",
       x="",y="mean total local importance score")

# sim85 false positives
sim85_fp_scores=sim85_final_local_df %>% filter(pred_type=="FP") %>% select(seq(11,20))
sim85_stacked_fp_scores <- stack(sim85_fp_scores)
colnames(sim85_stacked_fp_scores) <- c("mean_total_local_scores","variable")
sim85_reordered_stacked_fp <- sim85_stacked_fp_scores %>% mutate(variable=factor(variable,levels=sim85_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim85_reordered_stacked_fp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim85 false positives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
           RF train/test accuracy = 84.3% / 80.0%",
       x="",y="mean total local importance score")

# sim85 false negatives
sim85_fn_scores=sim85_final_local_df %>% filter(pred_type=="FN") %>% select(seq(11,20))
sim85_stacked_fn_scores <- stack(sim85_fn_scores)
colnames(sim85_stacked_fn_scores)<-c("mean_total_local_scores","variable")
sim85_reordered_stacked_fn <- sim85_stacked_fn_scores %>% mutate(variable=factor(variable,levels=sim85_lurf_feat)) %>% 
  arrange(variable)

ggplot(sim85_reordered_stacked_fn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("sim85 false negatives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.5; main effects = 0.8; interaction effects = 1.5
           RF train/test accuracy = 84.3% / 80.0%",
       x="",y="mean total local importance score")


#### bio
# bio true positive
colnames(bio_final_local_df)
# [1] "id"                    "train_test"            "pred_type"            
# [4] "class"                 "pred"                  "prob_abiotic"         
# [7] "prob_biotic"           "knn"                   "nbd_bal"              
# [10] "lambda.1se"            "time_kl_shift"         "fluctanal_prop_r1"    
# [13] "diff2_acf1"            "sd_d18O13C"            "avg_rR45CO244CO2"     
# [16] "total_local_score"     "opp_class"             "opp_lambda.1se"       
# [19] "opp_time_kl_shift"     "opp_fluctanal_prop_r1" "opp_diff2_acf1"       
# [22] "opp_sd_d18O13C"        "opp_avg_rR45CO244CO2"  "opp_total_local_score"

# bio true positives
bio_tp_scores=bio_final_local_df %>% filter(pred_type=="TP") %>% select(seq(11,15))
bio_stacked_tp_scores <- stack(bio_tp_scores)
colnames(bio_stacked_tp_scores)<-c("mean_total_local_scores","variable")
bio_reordered_stacked_tp <- bio_stacked_tp_scores %>% mutate(variable=factor(variable,levels=bio_lurf_feat)) %>% 
  arrange(variable)

ggplot(bio_reordered_stacked_tp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("biosignature true positives (all samples): 
          mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.64;
           RF train/test accuracy = 90.7% / 91.2%",
       x="",y="mean total local importance score")

# bio true negative
bio_tn_scores=bio_final_local_df %>% filter(pred_type=="TN") %>% select(seq(11,15))
bio_stacked_tn_scores <- stack(bio_tn_scores)
colnames(bio_stacked_tn_scores)<-c("mean_total_local_scores","variable")
bio_reordered_stacked_tn <- bio_stacked_tn_scores %>% mutate(variable=factor(variable,levels=bio_lurf_feat)) %>% 
  arrange(variable)

ggplot(bio_reordered_stacked_tn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("biosignature true negatives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.64;
           RF train/test accuracy = 90.7% / 91.2%",
       x="",y="mean total local importance score")

# bio false positive
bio_fp_scores=bio_final_local_df %>% filter(pred_type=="FP") %>% select(seq(11,15))
bio_stacked_fp_scores <- stack(bio_fp_scores)
colnames(bio_stacked_fp_scores) <- c("mean_total_local_scores","variable")
bio_reordered_stacked_fp <- bio_stacked_fp_scores %>% mutate(variable=factor(variable,levels=bio_lurf_feat)) %>% 
  arrange(variable)

ggplot(bio_reordered_stacked_fp, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("biosignature false positives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.64;
           RF train/test accuracy = 90.7% / 91.2%",
       x="",y="mean total local importance score")

# bio false negative
bio_fn_scores=bio_final_local_df %>% filter(pred_type=="FN") %>% select(seq(11,15))
bio_stacked_fn_scores <- stack(bio_fn_scores)
colnames(bio_stacked_fn_scores)<-c("mean_total_local_scores","variable")
bio_reordered_stacked_fn <- bio_stacked_fn_scores %>% mutate(variable=factor(variable,levels=bio_lurf_feat)) %>% 
  arrange(variable)

ggplot(bio_reordered_stacked_fn, aes(x = variable, y = mean_total_local_scores)) +
  geom_boxplot(aes(colour=variable),show.legend = F) +
  ggtitle("biosignature false negatives (all samples): 
           mean total local importance score by feature") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  labs(subtitle = "imbalance = 0.64;
           RF train/test accuracy = 90.7% / 91.2%",
       x="",y="mean total local importance score")




###################################################################
# 6. analysis: local variable importance by prediction type
##################################################################
# compare raw values for each prediction; in which cases do the scores
# increase or decrease?

colnames(sim77_final_local_df)
# [1] "id"                    "train_test"            "pred_type"            
# [4] "class"                 "pred"                  "prob_0"               
# [7] "prob_1"                "knn"                   "nbd_bal"              
# [10] "lambda.1se"            "intvar3"               "var64"                
# [13] "intvar8"               "var35"                 "mainvar1"             
# [16] "mainvar2"              "mainvar8"              "mainvar9"             
# [19] "mainvar4"              "total_local_score"     "opp_class"            
# [22] "opp_lambda.1se"        "opp_intvar3"           "opp_var64"            
# [25] "opp_intvar8"           "opp_var35"             "opp_mainvar1"         
# [28] "opp_mainvar2"          "opp_mainvar8"          "opp_mainvar9"         
# [31] "opp_mainvar4"          "opp_total_local_score"

dim(sim77_final_local_df)
# [1] 300  32


#### find the means for each of FPs, FNs, TPs, TNs

######################### sim77 FPs
sim77_FP_local_vars <- sim77_final_local_df %>%  
  filter(pred_type=="FP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim77_lurf_feat))
head(sim77_FP_local_vars)
# id train_test pred_type pred prob_0 prob_1   mainvar9   mainvar4    mainvar1
# 1  1      train        FP    0 0.8104 0.1896 0.04738750 0.05462306 -0.01255232
# 2  5      train        FP    0 0.6286 0.3714 0.30348620 0.31076650 -0.13726773
# 3  6      train        FP    0 0.5154 0.4846 0.07738681 0.08630738 -0.02784230
# 4  7      train        FP    0 0.7089 0.2911 0.05573508 0.05597645 -0.02129960
# 5  8      train        FP    0 0.8707 0.1293 0.16685947 0.17844193  0.07356866
# 6  9      train        FP    0 0.8133 0.1867 0.03274713 0.03321430  0.02615540
# intvar8       intvar3   mainvar8    mainvar2        var64        var35
# 1 -0.009591616 -1.358925e-03 0.04478179 -0.03375190  0.001991114 -0.011205627
# 2 -0.110742080 -7.038122e-02 0.27791319 -0.17069025 -0.076116617  0.125534721
# 3  0.013328375  9.171402e-05 0.05687840  0.04341070 -0.010404089  0.020751995
# 4 -0.015719334  8.498413e-03 0.04625206  0.04411517 -0.012589992 -0.018032038
# 5  0.054224939  2.506195e-02 0.15723599  0.08251588 -0.036970652 -0.061001531
# 6 -0.007474916 -1.648637e-04 0.03238400  0.02617818 -0.006960766  0.008161196
dim(sim77_FP_local_vars)
# [1] 48 15

#write.table(sim77_FP_local_vars,"sim77_FP_localVarImp2.csv",row.names=F,quote=F,sep=",")

colnames(sim77_FP_local_vars)
# [1] "id"         "train_test" "pred_type"  "pred"       "prob_0"     "prob_1"    
# [7] "mainvar9"   "mainvar4"   "mainvar1"   "intvar8"    "intvar3"    "mainvar8"  
# [13] "mainvar2"   "var64"      "var35"

sim77_FP_mean_local_vars <- sapply(sim77_FP_local_vars[,7:15],function(x){mean(x)})
sim77_FP_mean_local_vars
# mainvar9     mainvar4     mainvar1      intvar8      intvar3     mainvar8 
# 0.093179717  0.110109295  0.004736015 -0.006308635  0.003528627  0.069942730 
# mainvar2        var64        var35 
# 0.021428470 -0.006524000  0.001367812 

sim77_FP_mean_local_df <- as.data.frame(sim77_FP_mean_local_vars)
colnames(sim77_FP_mean_local_df)<-c("FP_mean_local_score")
sim77_FP_mean_local_df
#    FP_mean_local_score
# mainvar9         0.093179717
# mainvar4         0.110109295
# mainvar1         0.004736015
# intvar8         -0.006308635
# intvar3          0.003528627
# mainvar8         0.069942730
# mainvar2         0.021428470
# var64           -0.006524000
# var35            0.001367812


## sim77 FNs
sim77_FN_local_vars <- sim77_final_local_df %>%  
  filter(pred_type=="FN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim77_lurf_feat))
head(sim77_FN_local_vars)
# id train_test pred_type pred prob_0 prob_1    mainvar9    mainvar4     mainvar1
# 1 148      train        FN    1 0.3423 0.6577 -0.02702731 -0.03175952  0.009768753
# 2 160      train        FN    1 0.3967 0.6033 -0.04162625 -0.04674428 -0.033044880
# 3 161      train        FN    1 0.4690 0.5310 -0.05727226 -0.07399961  0.023419506
# 4 163      train        FN    1 0.4314 0.5686 -0.04233604 -0.04614658  0.012559983
# 5 164      train        FN    1 0.2971 0.7029 -0.08769587  0.08973838  0.032691122
# 6 165      train        FN    1 0.4268 0.5732 -0.39581264 -0.46286277 -0.256914861
# intvar8      intvar3    mainvar8    mainvar2        var64        var35
# 1  0.008437163  0.005822350  0.02248647  0.01588134 -0.006542943  0.009687627
# 2 -0.007125302 -0.001281447 -0.04073522  0.03386304  0.002570508  0.013815881
# 3  0.017060971 -0.005283848 -0.05297141 -0.03213911  0.012491876  0.017585911
# 4 -0.011993239 -0.005943838  0.04119375 -0.03924346  0.007871212 -0.012171469
# 5  0.015605085 -0.004935203 -0.08712645 -0.05397479  0.013295114  0.025731373
# 6  0.139299569 -0.078600670 -0.35064454 -0.28397488  0.122522643  0.244694520
dim(sim77_FN_local_vars)
# [1] 22 15

#write.table(sim77_FN_local_vars,"sim77_FN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim77_FN_mean_local_vars <- sapply(sim77_FN_local_vars[,7:15],function(x){mean(x)})
sim77_FN_mean_local_vars
#  mainvar9     mainvar4     mainvar1      intvar8      intvar3     mainvar8 
# -0.076701707 -0.118749995 -0.033677648  0.012732791 -0.008759162 -0.079442721 
# mainvar2        var64        var35 
# -0.024191339  0.016791507  0.023185939 

sim77_FN_mean_local_df <- as.data.frame(sim77_FN_mean_local_vars)
colnames(sim77_FN_mean_local_df)<-c("FN_mean_local_score")
sim77_FN_mean_local_df
#          FN_mean_local_score
# mainvar9        -0.076701707
# mainvar4        -0.118749995
# mainvar1        -0.033677648
# intvar8          0.012732791
# intvar3         -0.008759162
# mainvar8        -0.079442721
# mainvar2        -0.024191339
# var64            0.016791507
# var35            0.023185939


## sim77 TPs
sim77_TP_local_vars <- sim77_final_local_df %>%  
  filter(pred_type=="TP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim77_lurf_feat))
head(sim77_TP_local_vars)
# id train_test pred_type pred prob_0 prob_1   mainvar9   mainvar4    mainvar1
# 1 145      train        TP    0 0.8817 0.1183 0.33704836 0.34459339  0.11846640
# 2 146      train        TP    0 0.5428 0.4572 0.11928891 0.12060654  0.08840663
# 3 147      train        TP    0 0.9079 0.0921 0.06596792 0.06661069 -0.02525009
# 4 149      train        TP    0 0.6807 0.3193 0.03528086 0.03753983 -0.01265797
# 5 150      train        TP    0 0.8629 0.1371 0.14072087 0.15385400  0.05222528
# 6 151      train        TP    0 0.7803 0.2197 0.07950693 0.08580352 -0.02002179
# intvar8       intvar3    mainvar8   mainvar2        var64        var35
# 1  0.059256681  0.0254289145 -0.21997038 0.21782192  0.026325365  0.100359371
# 2 -0.018281717  0.0107235709 -0.09178780 0.09128254 -0.013984338 -0.043251523
# 3  0.012291318 -0.0086625235  0.05700976 0.05359511  0.009165334 -0.024877178
# 4 -0.007095282 -0.0005451557  0.03450025 0.02699147 -0.002353281 -0.007099695
# 5 -0.001936348 -0.0009046684  0.10117787 0.05365505  0.001075234  0.023466605
# 6  0.011820614 -0.0012526052  0.07891727 0.07533254 -0.007136563 -0.018454368
dim(sim77_TP_local_vars)
# [1] 98 15

#write.table(sim77_TP_local_vars,"sim77_TP_localVarImp2.csv",row.names=F,quote=F,sep=",")



sim77_TP_mean_local_vars <- sapply(sim77_TP_local_vars[,7:15],function(x){mean(x)})
sim77_TP_mean_local_vars
#     mainvar9     mainvar4     mainvar1      intvar8      intvar3     mainvar8 
# 0.101946584  0.113683557  0.022791860 -0.004340588  0.001721777  0.071710548 
# mainvar2        var64        var35 
# 0.046473561  0.004525124  0.004032499 

sim77_TP_mean_local_df <- as.data.frame(sim77_TP_mean_local_vars)
colnames(sim77_TP_mean_local_df)<-c("TP_mean_local_score")
sim77_TP_mean_local_df
#  TP_mean_local_score
# mainvar9         0.101946584
# mainvar4         0.113683557
# mainvar1         0.022791860
# intvar8         -0.004340588
# intvar3          0.001721777
# mainvar8         0.071710548
# mainvar2         0.046473561
# var64            0.004525124
# var35            0.004032499

# sim77 TNs
sim77_TN_local_vars <- sim77_final_local_df %>%  
  filter(pred_type=="TN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim77_lurf_feat))
head(sim77_TN_local_vars)
# id train_test pred_type pred prob_0 prob_1    mainvar9    mainvar4    mainvar1
# 1  2      train        TN    1 0.4420 0.5580 -0.05011796 -0.05689858  0.01911812
# 2  3      train        TN    1 0.3515 0.6485 -0.05400155 -0.05509229  0.02006841
# 3  4      train        TN    1 0.4443 0.5557 -0.04326346 -0.04691664 -0.03516512
# 4 21      train        TN    1 0.4924 0.5076 -0.04896167 -0.05099808  0.01425393
# 5 24      train        TN    1 0.4005 0.5995 -0.04373326 -0.04513071  0.02358929
# 6 28      train        TN    1 0.4927 0.5073 -0.09055497 -0.10058237 -0.02898723
# intvar8      intvar3    mainvar8    mainvar2       var64       var35
# 1 -0.017675105  0.010690353 -0.03382562  0.02392199 -0.01519912  0.01868535
# 2  0.015562015 -0.002250416 -0.04858681 -0.04596047  0.01032987  0.01795126
# 3  0.016090671  0.009259487  0.04168314 -0.04085791  0.01259538  0.01661350
# 4  0.010152927  0.009152437 -0.04891935  0.02554701  0.00995775  0.01223083
# 5  0.009013624  0.004810724 -0.03964797  0.03359938  0.00627875  0.01137106
# 6  0.025515211 -0.004722474 -0.07188797  0.03245708  0.01059869 -0.02860616
dim(sim77_TN_local_vars)
# [1] 132 15

#write.table(sim77_TN_local_vars,"sim77_TN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim77_TN_mean_local_vars <- sapply(sim77_TN_local_vars[,7:15],function(x){mean(x)})
sim77_TN_mean_local_vars
#   mainvar9     mainvar4     mainvar1      intvar8      intvar3     mainvar8 
# 0.0427728009 0.0450128421 0.0122089622 0.0006162847 0.0016968446 0.0390485639 
# mainvar2        var64        var35 
# 0.0252921173 0.0037341196 0.0028709137 

sim77_TN_mean_local_df <- as.data.frame(sim77_TN_mean_local_vars)
colnames(sim77_TN_mean_local_df)<-c("TN_mean_local_score")
sim77_TN_mean_local_df
#  TN_mean_local_score
# mainvar9        0.0427728009
# mainvar4        0.0450128421
# mainvar1        0.0122089622
# intvar8         0.0006162847
# intvar3         0.0016968446
# mainvar8        0.0390485639
# mainvar2        0.0252921173
# var64           0.0037341196
# var35           0.0028709137

## combine sim77 mean var scores
sim77_pred_local_mean_df <- cbind.data.frame(
      sim77_TP_mean_local_df,
      sim77_TN_mean_local_df,
      sim77_FP_mean_local_df,
      sim77_FN_mean_local_df)
sim77_pred_local_mean_df$variable <- rownames(sim77_pred_local_mean_df)
sim77_pred_local_mean_df
#       TP_mean_local_score TN_mean_local_score FP_mean_local_score FN_mean_local_score variable
# mainvar9         0.101946584        0.0427728009         0.093179717        -0.076701707 mainvar9
# mainvar4         0.113683557        0.0450128421         0.110109295        -0.118749995 mainvar4
# mainvar1         0.022791860        0.0122089622         0.004736015        -0.033677648 mainvar1
# intvar8         -0.004340588        0.0006162847        -0.006308635         0.012732791  intvar8
# intvar3          0.001721777        0.0016968446         0.003528627        -0.008759162  intvar3
# mainvar8         0.071710548        0.0390485639         0.069942730        -0.079442721 mainvar8
# mainvar2         0.046473561        0.0252921173         0.021428470        -0.024191339 mainvar2
# var64            0.004525124        0.0037341196        -0.006524000         0.016791507    var64
# var35            0.004032499        0.0028709137         0.001367812         0.023185939    var35

#save to file
#write.table(sim77_pred_local_mean_df,"sim77_mean_localVarImp2.csv",row.names=F,quote=F,sep=",")



########### sim79 FPs
sim79_FP_local_vars <- sim79_final_local_df %>%  
  filter(pred_type=="FP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim79_lurf_feat))
head(sim79_FP_local_vars)
#   id train_test pred_type pred prob_0 prob_1   mainvar8     intvar8    mainvar9   mainvar7        var14
# 1  1      train        FP    0 0.7165 0.2835 0.05929236 -0.01075392  0.04534942 0.06506282 -0.004171403
# 2  2      train        FP    0 0.5761 0.4239 0.10512637  0.03669345 -0.05173533 0.22803622 -0.006081897
# 3  3      train        FP    0 0.5218 0.4782 0.25947125 -0.07694237 -0.09213315 0.35171515 -0.015340110
# 4  4      train        FP    0 0.7922 0.2078 0.12556601 -0.03629234  0.04829236 0.15342334  0.002004532
# 5  5      train        FP    0 0.7452 0.2548 0.31815210  0.12359987 -0.22191094 0.36733365  0.012521761
# 6  6      train        FP    0 0.5039 0.4961 0.19049297  0.07219429  0.11743049 0.21250949 -0.018723878
#         var64      intvar7        var67
# 1  0.005494129 -0.009924498  0.006161337
# 2  0.013254341 -0.024326723 -0.018058299
# 3 -0.038967741 -0.044323104 -0.040155510
# 4  0.004662641 -0.019904497 -0.011501224
# 5 -0.060707003 -0.078014336  0.060707757
# 6  0.020000942 -0.040977101 -0.030509553

dim(sim79_FP_local_vars)
# [1] 51  14

#write.table(sim79_FP_local_vars,"sim79_FP_localVarImp2.csv",row.names=F,quote=F,sep=",")

colnames(sim79_FP_local_vars)
# [1] "id"         "train_test" "pred_type"  "pred"       "prob_0"     "prob_1"    
# [7] "mainvar8"   "intvar8"    "mainvar9"   "mainvar7"   "var14"      "var64"     
# [13] "intvar7"    "var67" 

sim79_FP_mean_local_vars <- sapply(sim79_FP_local_vars[,7:14],function(x){mean(x)})
sim79_FP_mean_local_vars
#     mainvar8       intvar8      mainvar9      mainvar7         var14         var64       intvar7 
# 0.1247842170  0.0192285766  0.0163033766  0.1861307727  0.0004178495 -0.0033858638 -0.0104817039 
#         var67 
# -0.0093499413

sim79_FP_mean_local_df <- as.data.frame(sim79_FP_mean_local_vars)
colnames(sim79_FP_mean_local_df)<-c("FP_mean_local_score")
sim79_FP_mean_local_df
#          FP_mean_local_score
# mainvar8        0.1247842170
# intvar8         0.0192285766
# mainvar9        0.0163033766
# mainvar7        0.1861307727
# var14           0.0004178495
# var64          -0.0033858638
# intvar7        -0.0104817039
# var67          -0.0093499413


## sim79 FNs
sim79_FN_local_vars <- sim79_final_local_df %>%  
  filter(pred_type=="FN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim79_lurf_feat))
head(sim79_FN_local_vars)
# id train_test pred_type pred prob_0 prob_1    mainvar8     intvar8    mainvar9    mainvar7
# 1 145      train        FN    1 0.4805 0.5195 -0.04500381 -0.01864244  0.03940335 -0.05997377
# 2 190      train        FN    1 0.4609 0.5391  0.39510457 -0.08126035  0.13540574 -0.48509421
# 3 281       test        FN    1 0.3301 0.6699 -0.07752501  0.01805602 -0.05641351 -0.09384993
# var14        var64      intvar7       var67
# 1  0.003465466  0.007055098 -0.009530394 0.007331671
# 2 -0.000770796 -0.001965178 -0.071402503 0.042573419
# 3 -0.001514007  0.005592258  0.012130332 0.009500589
dim(sim79_FN_local_vars)
# [1] 3  14

#write.table(sim79_FN_local_vars,"sim79_FN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim79_FN_mean_local_vars <- sapply(sim79_FN_local_vars[,7:14],function(x){mean(x)})
sim79_FN_mean_local_vars
#  mainvar8       intvar8      mainvar9      mainvar7         var14         var64       intvar7 
# 0.0908585848 -0.0272822558  0.0394651920 -0.2129726387  0.0003935545  0.0035607261 -0.0229341881 
# var67 
# 0.0198018928

sim79_FN_mean_local_df <- as.data.frame(sim79_FN_mean_local_vars)
colnames(sim79_FN_mean_local_df)<-c("FN_mean_local_score")
sim79_FN_mean_local_df
#         FN_mean_local_score
# mainvar8        0.0908585848
# intvar8        -0.0272822558
# mainvar9        0.0394651920
# mainvar7       -0.2129726387
# var14           0.0003935545
# var64           0.0035607261
# intvar7        -0.0229341881
# var67           0.0198018928


## sim79 TPs
sim79_TP_local_vars <- sim79_final_local_df %>%  
  filter(pred_type=="TP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim79_lurf_feat))
head(sim79_TP_local_vars)
# id train_test pred_type pred prob_0 prob_1   mainvar8     intvar8    mainvar9   mainvar7        var14
# 1 146      train        TP    0 0.5297 0.4703 0.07618745 -0.01379053 -0.05214031 0.09369028 -0.003184421
# 2 147      train        TP    0 0.6858 0.3142 0.04515050  0.01399575 -0.03143602 0.05121415 -0.001747764
# 3 148      train        TP    0 0.7490 0.2510 0.32628527  0.08714113  0.11633594 0.36553092 -0.027046707
# 4 149      train        TP    0 0.7539 0.2461 0.20968163  0.07389494  0.07663598 0.26267571 -0.016237488
# 5 150      train        TP    0 0.7681 0.2319 0.16116021 -0.04165934 -0.04884056 0.20773799  0.014172734
# 6 151      train        TP    0 0.5975 0.4025 0.16760873 -0.05027954  0.05493945 0.18106653 -0.006873008
# var64      intvar7        var67
# 1 -0.004090632 -0.011007890 -0.009544059
# 2  0.003037084 -0.005996367  0.004389564
# 3 -0.033084287  0.061594197  0.056774998
# 4 -0.022655383 -0.040877355  0.026979634
# 5 -0.018312803 -0.038860909 -0.018749304
# 6  0.007948140 -0.017591346  0.007998817
dim(sim79_TP_local_vars)
# [1] 117  14

#write.table(sim79_TP_local_vars,"sim79_TP_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim79_TP_mean_local_vars <- sapply(sim79_TP_local_vars[,7:14],function(x){mean(x)})
sim79_TP_mean_local_vars
# mainvar8      intvar8     mainvar9     mainvar7        var14        var64      intvar7        var67 
# 0.144602484  0.011772242  0.010644413  0.204369316 -0.003016475 -0.007759913 -0.002939445 -0.008479192

sim79_TP_mean_local_df <- as.data.frame(sim79_TP_mean_local_vars)
colnames(sim79_TP_mean_local_df)<-c("TP_mean_local_score")
sim79_TP_mean_local_df
#  TP_mean_local_score
# mainvar8         0.144602484
# intvar8          0.011772242
# mainvar9         0.010644413
# mainvar7         0.204369316
# var14           -0.003016475
# var64           -0.007759913
# intvar7         -0.002939445
# var67           -0.008479192


# sim79 TNs
sim79_TN_local_vars <- sim79_final_local_df %>%  
  filter(pred_type=="TN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim79_lurf_feat))
head(sim79_TN_local_vars)
# id train_test pred_type pred prob_0 prob_1    mainvar8     intvar8    mainvar9    mainvar7
# 1 13      train        TN    1 0.3234 0.6766 -0.43094498 0.077567346  0.12645624 -0.49116696
# 2 14      train        TN    1 0.4294 0.5706 -0.30003644 0.069795445 -0.15407048 -0.30921317
# 3 25      train        TN    1 0.4854 0.5146 -0.11414200 0.018480922 -0.04266768 -0.12203476
# 4 39      train        TN    1 0.3437 0.6563 -0.40573936 0.082871102  0.14223938 -0.52535413
# 5 40      train        TN    1 0.3716 0.6284 -0.05685699 0.016935297 -0.03510784 -0.08376578
# 6 45      train        TN    1 0.0148 0.9852  0.02537189 0.004768368 -0.01282807  0.02633658
# var14        var64      intvar7       var67
# 1 -0.0046492989  0.017741276  0.066589344 0.061118017
# 2  0.0224328130  0.048318455 -0.057560662 0.051283236
# 3  0.0035267397 -0.004040217 -0.011691674 0.010439843
# 4 -0.0350378764  0.044390061 -0.076611434 0.068200508
# 5  0.0031850933  0.005824139  0.011591808 0.010426569
# 6  0.0004017582 -0.001723624 -0.004711967 0.001914182
dim(sim79_TN_local_vars)
# [1] 129  14

#write.table(sim79_TN_local_vars,"sim79_TN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim79_TN_mean_local_vars <- sapply(sim79_TN_local_vars[,7:14],function(x){mean(x)})
sim79_TN_mean_local_vars
#   mainvar8       intvar8      mainvar9      mainvar7         var14         var64       intvar7 
# 0.1046066312 -0.0086471571  0.0293237176  0.1465212986  0.0035128156  0.0056799598  0.0000225101 
# var67 
# 0.0058744319 

sim79_TN_mean_local_df <- as.data.frame(sim79_TN_mean_local_vars)
colnames(sim79_TN_mean_local_df)<-c("TN_mean_local_score")
sim79_TN_mean_local_df
#       TN_mean_local_score
# mainvar8        0.1046066312
# intvar8        -0.0086471571
# mainvar9        0.0293237176
# mainvar7        0.1465212986
# var14           0.0035128156
# var64           0.0056799598
# intvar7         0.0000225101
# var67           0.0058744319

## combine sim79 mean var scores
sim79_pred_local_mean_df <- cbind.data.frame(
  sim79_TP_mean_local_df,
  sim79_TN_mean_local_df,
  sim79_FP_mean_local_df,
  sim79_FN_mean_local_df)
sim79_pred_local_mean_df$variable <- rownames(sim79_pred_local_mean_df)
sim79_pred_local_mean_df
#          TP_mean_local_score TN_mean_local_score FP_mean_local_score FN_mean_local_score variable
# mainvar8         0.144602484        0.1046066312        0.1247842170        0.0908585848 mainvar8
# intvar8          0.011772242       -0.0086471571        0.0192285766       -0.0272822558  intvar8
# mainvar9         0.010644413        0.0293237176        0.0163033766        0.0394651920 mainvar9
# mainvar7         0.204369316        0.1465212986        0.1861307727       -0.2129726387 mainvar7
# var14           -0.003016475        0.0035128156        0.0004178495        0.0003935545    var14
# var64           -0.007759913        0.0056799598       -0.0033858638        0.0035607261    var64
# intvar7         -0.002939445        0.0000225101       -0.0104817039       -0.0229341881  intvar7
# var67           -0.008479192        0.0058744319       -0.0093499413        0.0198018928    var67

#save to file
#write.table(sim79_pred_local_mean_df,"sim79_mean_localVarImp2.csv",row.names=F,quote=F,sep=",")


############ sim85 FPs
sim85_FP_local_vars <- sim85_final_local_df %>%  
  filter(pred_type=="FP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim85_lurf_feat))
head(sim85_FP_local_vars)
#  id train_test pred_type pred prob_0 prob_1    mainvar5    mainvar9    mainvar1    mainvar4     mainvar7
# 1  5      train        FP    0 0.7264 0.2736  0.04228502 -0.04288178  0.02858537  0.03554342  0.020267417
# 2 21      train        FP    0 0.5778 0.4222  0.03827453 -0.05190494  0.02289085  0.02754273  0.007695353
# 3 24      train        FP    0 0.7827 0.2173  0.07363863  0.08785723  0.05240687  0.05763748  0.021009423
# 4 30      train        FP    0 0.5437 0.4563  0.10874597 -0.10991659 -0.10001913 -0.10426331  0.038308772
# 5 40      train        FP    0 0.7055 0.2945  0.03279079  0.04493766  0.02347458 -0.03128485 -0.008784901
# 6 46      train        FP    0 0.6101 0.3899 -0.06833652 -0.08460211 -0.05033017  0.05956975  0.016187208
# mainvar10    mainvar3     mainvar8      intvar4     mainvar6
# 1  0.02727444 -0.03226548  0.011563162 -0.008192016  0.024894380
# 2 -0.01279599  0.02308898  0.006135751  0.002404549 -0.012531513
# 3  0.03882116  0.05736931 -0.016923167 -0.003970782 -0.029329222
# 4  0.07138834 -0.10300896  0.035547224  0.019290487  0.044463252
# 5  0.01888023 -0.02608888 -0.002707355  0.002568385  0.009164705
# 6  0.03304807  0.05127919 -0.014939419 -0.011013682  0.020103847

dim(sim85_FP_local_vars)
# [1] 43  16

#write.table(sim85_FP_local_vars,"sim85_FP_localVarImp2.csv",row.names=F,quote=F,sep=",")

colnames(sim85_FP_local_vars)
# [1] "id"         "train_test" "pred_type"  "pred"       "prob_0"     "prob_1"     "mainvar5"  
# [8] "mainvar9"   "mainvar1"   "mainvar4"   "mainvar7"   "mainvar10"  "mainvar3"   "mainvar8"  
# [15] "intvar4"    "mainvar6"

sim85_FP_mean_local_vars <- sapply(sim85_FP_local_vars[,7:16],function(x){mean(x)})
sim85_FP_mean_local_vars
#   mainvar5      mainvar9      mainvar1      mainvar4      mainvar7     mainvar10      mainvar3 
# -0.0081759447  0.0099468669  0.0004467833  0.0066540127  0.0053649166  0.0135743035  0.0039218068 
# mainvar8       intvar4      mainvar6 
# 0.0094021240  0.0003921815  0.0101340681 

sim85_FP_mean_local_df <- as.data.frame(sim85_FP_mean_local_vars)
colnames(sim85_FP_mean_local_df)<-c("FP_mean_local_score")
sim85_FP_mean_local_df
#     FP_mean_local_score
# mainvar5        -0.0081759447
# mainvar9         0.0099468669
# mainvar1         0.0004467833
# mainvar4         0.0066540127
# mainvar7         0.0053649166
# mainvar10        0.0135743035
# mainvar3         0.0039218068
# mainvar8         0.0094021240
# intvar4          0.0003921815
# mainvar6         0.0101340681


## sim85 FNs
sim85_FN_local_vars <- sim85_final_local_df %>%  
  filter(pred_type=="FN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim85_lurf_feat))
head(sim85_FN_local_vars)
# id train_test pred_type pred prob_0 prob_1    mainvar5    mainvar9    mainvar1    mainvar4    mainvar7
# 1 204      train        FN    1 0.4515 0.5485  0.03534076 -0.04091218 -0.02661965 -0.03194868 -0.00935546
# 2 211      train        FN    1 0.3447 0.6553  0.14437139 -0.18774618  0.10641346  0.14111925 -0.04889117
# 3 216      train        FN    1 0.4483 0.5517  0.04560128  0.04691341 -0.03568082 -0.04150726 -0.01623149
# 4 217      train        FN    1 0.4809 0.5191 -0.09060908  0.09069952 -0.06710701  0.07763745 -0.02965167
# 5 236      train        FN    1 0.3561 0.6439  0.16693957  0.19008563 -0.13169455  0.15632225  0.04025659
# 6 270      train        FN    1 0.4246 0.5754  0.05399675 -0.05615594 -0.03858699  0.04493987 -0.01456158
# mainvar10    mainvar3     mainvar8       intvar4     mainvar6
# 1 -0.01632124  0.02724788  0.007112213  0.0027486411  0.009420513
# 2 -0.09017257  0.12706119  0.040799128 -0.0398294667  0.065054909
# 3  0.02723367 -0.03603640 -0.011187945  0.0070233518 -0.024396208
# 4  0.06144854  0.07099283 -0.024691518 -0.0004670394 -0.048603161
# 5 -0.11195757  0.13652416  0.012773239 -0.0039749778 -0.062750754
# 6  0.02947274  0.04154169  0.011936032  0.0035430765 -0.017432585
dim(sim85_FN_local_vars)
# [1] 40  16

#write.table(sim85_FN_local_vars,"sim85_FN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim85_FN_mean_local_vars <- sapply(sim85_FN_local_vars[,7:16],function(x){mean(x)})
sim85_FN_mean_local_vars
#  mainvar5     mainvar9     mainvar1     mainvar4     mainvar7    mainvar10     mainvar3     mainvar8 
# 0.047011578  0.062880126  0.010876176  0.001750982 -0.014989692 -0.022739844  0.013588923 -0.003313290 
# intvar4     mainvar6 
# -0.001071314 -0.015400504 

sim85_FN_mean_local_df <- as.data.frame(sim85_FN_mean_local_vars)
colnames(sim85_FN_mean_local_df)<-c("FN_mean_local_score")
sim85_FN_mean_local_df
#    FN_mean_local_score
# mainvar5          0.047011578
# mainvar9          0.062880126
# mainvar1          0.010876176
# mainvar4          0.001750982
# mainvar7         -0.014989692
# mainvar10        -0.022739844
# mainvar3          0.013588923
# mainvar8         -0.003313290
# intvar4          -0.001071314
# mainvar6         -0.015400504


## sim85 TPs
sim85_TP_local_vars <- sim85_final_local_df %>%  
  filter(pred_type=="TP") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim85_lurf_feat))
head(sim85_TP_local_vars)
#    id train_test pred_type pred prob_0 prob_1   mainvar5    mainvar9    mainvar1   mainvar4    mainvar7
# 1 201      train        TP    0 0.8513 0.1487 0.05464595  0.05851982 -0.03058229 0.05191035  0.01829263
# 2 202      train        TP    0 0.7846 0.2154 0.06861845  0.09796618 -0.03710317 0.05366905  0.03482841
# 3 203      train        TP    0 0.7802 0.2198 0.14406844  0.18515274 -0.11623571 0.13553392  0.04949642
# 4 205      train        TP    0 0.8398 0.1602 0.12338609  0.12510934  0.05534898 0.09290789 -0.02827832
# 5 206      train        TP    0 0.5537 0.4463 0.10851685 -0.13136958  0.07061344 0.08390162  0.02883397
# 6 207      train        TP    0 0.7701 0.2299 0.06836496  0.07093107 -0.04829768 0.05104072 -0.01552272
#    mainvar10    mainvar3    mainvar8      intvar4    mainvar6
# 1 0.02328768 -0.03857610  0.01461996 -0.002085622 -0.02245931
# 2 0.03703412  0.05011137  0.02881789  0.028237848 -0.03593951
# 3 0.08302420  0.12363389  0.03329893  0.030842850 -0.05354066
# 4 0.03809051 -0.07666339  0.02383375  0.001794653  0.03570042
# 5 0.06475343  0.08158832 -0.02703212 -0.025548423  0.03034303
# 6 0.03751388 -0.04901688 -0.01473198 -0.010649437  0.02533895

dim(sim85_TP_local_vars)
# [1] 210  16

#write.table(sim85_TP_local_vars,"sim85_TP_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim85_TP_mean_local_vars <- sapply(sim85_TP_local_vars[,7:16],function(x){mean(x)})
sim85_TP_mean_local_vars
#   mainvar5     mainvar9     mainvar1     mainvar4     mainvar7    mainvar10     mainvar3     mainvar8 
# 0.0442874622 0.0593643342 0.0243385251 0.0310699052 0.0187290234 0.0229450423 0.0169993677 0.0050074372 
# intvar4     mainvar6 
# 0.0002429986 0.0098033613 

sim85_TP_mean_local_df <- as.data.frame(sim85_TP_mean_local_vars)
colnames(sim85_TP_mean_local_df)<-c("TP_mean_local_score")
sim85_TP_mean_local_df
#      TP_mean_local_score
# mainvar5         0.0442874622
# mainvar9         0.0593643342
# mainvar1         0.0243385251
# mainvar4         0.0310699052
# mainvar7         0.0187290234
# mainvar10        0.0229450423
# mainvar3         0.0169993677
# mainvar8         0.0050074372
# intvar4          0.0002429986
# mainvar6         0.0098033613
# mainvar6         -0.002145438


# sim85 TNs
sim85_TN_local_vars <- sim85_final_local_df %>%  
  filter(pred_type=="TN") %>% select(id, train_test, pred_type,pred,
                                     prob_0, prob_1,
                                     all_of(sim85_lurf_feat))
head(sim85_TN_local_vars)
#  id train_test pred_type pred prob_0 prob_1    mainvar5    mainvar9    mainvar1    mainvar4     mainvar7
# 1  1      train        TN    1 0.3166 0.6834  0.07490072  0.07882531 -0.02268643  0.05033968  0.005180496
# 2  2      train        TN    1 0.2430 0.7570 -0.06163211  0.07392640  0.05339926  0.06096450 -0.018535769
# 3  3      train        TN    1 0.2592 0.7408  0.04325009 -0.04498334  0.03333745  0.03935665 -0.016751664
# 4  4      train        TN    1 0.4343 0.5657 -0.15622843  0.18645768  0.12396715  0.14176060  0.035161637
# 5  6      train        TN    1 0.4086 0.5914  0.16630359 -0.22209528  0.11954969  0.16552537  0.037615129
# 6  7      train        TN    1 0.5049 0.4951  0.09427706 -0.12794902  0.07116121 -0.09211343  0.023177546
# mainvar10    mainvar3     mainvar8       intvar4    mainvar6
# 1 -0.01863795 -0.04678929  0.002048561  1.173569e-03  0.01124338
# 2  0.04312216  0.05902337  0.012891101  2.758388e-05 -0.03486974
# 3 -0.03159168  0.03624583 -0.009996409  6.372980e-03  0.02254903
# 4  0.10966970  0.12656725  0.009448578 -5.753545e-03 -0.09693668
# 5 -0.11758519  0.12428216 -0.020582012  1.625936e-02 -0.05497646
# 6 -0.05349881  0.08338612  0.022774331  1.900086e-02  0.04777125
dim(sim85_TN_local_vars)
# [1] 207  16

#write.table(sim85_TN_local_vars,"sim85_TN_localVarImp2.csv",row.names=F,quote=F,sep=",")

sim85_TN_mean_local_vars <- sapply(sim85_TN_local_vars[,7:16],function(x){mean(x)})
sim85_TN_mean_local_vars
#  mainvar5     mainvar9     mainvar1     mainvar4     mainvar7    mainvar10     mainvar3     mainvar8 
# 0.051297245  0.070033083  0.030620002  0.035144897 -0.001940921  0.003393684  0.047799728 -0.003292899 
# intvar4     mainvar6 
# 0.001533057 -0.004458354

sim85_TN_mean_local_df <- as.data.frame(sim85_TN_mean_local_vars)
colnames(sim85_TN_mean_local_df)<-c("TN_mean_local_score")
sim85_TN_mean_local_df
#    TN_mean_local_score
# mainvar5          0.051297245
# mainvar9          0.070033083
# mainvar1          0.030620002
# mainvar4          0.035144897
# mainvar7         -0.001940921
# mainvar10         0.003393684
# mainvar3          0.047799728
# mainvar8         -0.003292899
# intvar4           0.001533057
# mainvar6         -0.004458354

## combine sim85 mean var scores
sim85_pred_local_mean_df <- cbind.data.frame(
  sim85_TP_mean_local_df,
  sim85_TN_mean_local_df,
  sim85_FP_mean_local_df,
  sim85_FN_mean_local_df)
sim85_pred_local_mean_df$variable <- rownames(sim85_pred_local_mean_df)
sim85_pred_local_mean_df
#  TP_mean_local_score TN_mean_local_score FP_mean_local_score FN_mean_local_score  variable
# mainvar5         0.0442874622         0.051297245       -0.0081759447         0.047011578  mainvar5
# mainvar9         0.0593643342         0.070033083        0.0099468669         0.062880126  mainvar9
# mainvar1         0.0243385251         0.030620002        0.0004467833         0.010876176  mainvar1
# mainvar4         0.0310699052         0.035144897        0.0066540127         0.001750982  mainvar4
# mainvar7         0.0187290234        -0.001940921        0.0053649166        -0.014989692  mainvar7
# mainvar10        0.0229450423         0.003393684        0.0135743035        -0.022739844 mainvar10
# mainvar3         0.0169993677         0.047799728        0.0039218068         0.013588923  mainvar3
# mainvar8         0.0050074372        -0.003292899        0.0094021240        -0.003313290  mainvar8
# intvar4          0.0002429986         0.001533057        0.0003921815        -0.001071314   intvar4
# mainvar6         0.0098033613        -0.004458354        0.0101340681        -0.015400504  mainvar6

# save to file
#write.table(sim85_pred_local_mean_df,"sim85_mean_localVarImp2.csv",row.names=F,quote=F,sep=",")



###############################################################
# 7. bar plot: mean total local variable importance per prediction type
##############################################################

sim77_pred_local_mean_df
sim79_pred_local_mean_df
sim85_pred_local_mean_df

sim77_bar_dat <- melt(sim77_pred_local_mean_df,id="variable")
head(sim77_bar_dat)
colnames(sim77_bar_dat)<-c("variable","condition","value")
head(sim77_bar_dat)
#   variable           condition        value
# 1 mainvar9 TP_mean_local_score  0.101946584
# 2 mainvar4 TP_mean_local_score  0.113683557
# 3 mainvar1 TP_mean_local_score  0.022791860
# 4  intvar8 TP_mean_local_score -0.004340588
# 5  intvar3 TP_mean_local_score  0.001721777
# 6 mainvar8 TP_mean_local_score  0.071710548

sim79_bar_dat <- melt(sim79_pred_local_mean_df,id="variable")
colnames(sim79_bar_dat)<-c("variable","condition","value")
head(sim79_bar_dat)
#   variable           condition        value
# 1 mainvar8 TP_mean_local_score  0.144602484
# 2  intvar8 TP_mean_local_score  0.011772242
# 3 mainvar9 TP_mean_local_score  0.010644413
# 4 mainvar7 TP_mean_local_score  0.204369316
# 5    var14 TP_mean_local_score -0.003016475
# 6    var64 TP_mean_local_score -0.007759913

sim85_bar_dat <- melt(sim85_pred_local_mean_df,id="variable")
colnames(sim85_bar_dat)<-c("variable","condition","value")
head(sim85_bar_dat)
#   variable           condition      value
# 1  mainvar5 TP_mean_local_score 0.04428746
# 2  mainvar9 TP_mean_local_score 0.05936433
# 3  mainvar1 TP_mean_local_score 0.02433853
# 4  mainvar4 TP_mean_local_score 0.03106991
# 5  mainvar7 TP_mean_local_score 0.01872902
# 6 mainvar10 TP_mean_local_score 0.02294504


# Graph
ggplot(sim77_bar_dat, aes(fill=variable, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Average total local importance by prediction type: sim77 data") +
  facet_wrap(~condition) +
  #theme_ipsum() +
  theme(legend.position="none") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  xlab("")

ggplot(sim79_bar_dat, aes(fill=variable, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Average total local importance by prediction type: sim79 data") +
  facet_wrap(~condition) +
  #theme_ipsum() +
  theme(legend.position="none") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  xlab("")

ggplot(sim85_bar_dat, aes(fill=variable, y=value, x=variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Average total local importance by prediction type: sim85 data") +
  facet_wrap(~condition) +
  #theme_ipsum() +
  theme(legend.position="none") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(yintercept=0, linetype="dashed", color = "darkgray",linewidth=0.75)+
  xlab("")


##########################################################
# 8. how many falsely predicted samples can be diagnosed?
######################################################

sim77_fp_id <- sim77_FP_local_vars$id
sim77_fn_id <- sim77_FN_local_vars$id
sim77_false_id <- c(sim77_fp_id,sim77_fn_id)

sim77_false_samples = sim77_final_local_df %>% filter(train_test=="test") %>%
  filter(id %in% sim77_false_id) %>% 
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim77_lurf_feat),total_local_score)
head(sim77_false_samples)
dim(sim77_false_samples)
# [1] 17 13


sim77_test_samples = sim77_final_local_df %>% filter(train_test=="test") %>%
  #filter(id %in% sim77_false_id) %>% 
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim77_lurf_feat),total_local_score)
head(sim77_test_samples)
dim(sim77_test_samples)
# [1] 60 13

colnames(sim77_false_samples)
# [1] "id"                "pred_type"         "prob_predicted"    "mainvar9"         
# [5] "mainvar4"          "mainvar1"          "intvar8"           "intvar3"          
# [9] "mainvar8"          "mainvar2"          "var64"             "var35"            
# [13] "total_local_score"

sum(sim77_false_samples$prob_predicted < 0.75)
# 14 

sum(sim77_test_samples$prob_predicted < 0.75)
# [1] 30

sum(sim77_test_samples$total_local_score < 0.25)
# [1] 48

sum(sim77_false_samples$total_local_score < 0.25)
#[1] 14

sum(sim77_false_samples$total_local_score < 0.1)
# 7

sum(sim77_false_samples$total_local_score < 0)
#[1] 4

hist(sim77_final_local_df$total_local_score,breaks = 20,
     main="sim77 total local importance scores",xlab="total local importance score")

hist(sim77_false_samples$total_local_score,breaks=20,
    main="sim77 false prediction total local importance scores",
    xlab="false prediction total local importance score")


flagged_id <- c()
flagged_pred_type <- c()

prob_thresh <- 0.75
score_thresh <- 0.25

for(i in seq(1,length(sim77_test_samples$id))){
  prob <- sim77_test_samples$prob_predicted[i]
  score <- sim77_test_samples$total_local_score[i]
  flag <- (prob <= prob_thresh) & (score <= score_thresh)
  if(flag){
    flagged_id <- c(flagged_id, sim77_test_samples$id[i])
    flagged_pred_type <- c(flagged_pred_type, sim77_test_samples$pred_type[i])
  }
}
sim77_flagged_df <- cbind.data.frame(flagged_id,flagged_pred_type)
colnames(sim77_flagged_df) <- c("id","pred_type")
sim77_flagged_df
#     id pred_type
# 1  241        FP
# 2  244        FP
# 3  245        FP
# 4  246        TN
# 5  247        FP
# 6  249        TN
# 7  250        FP
# 8  252        FP
# 9  253        FP
# 10 254        FP
# 11 260        TN
# 12 278        TP
# 13 279        TP
# 14 280        TP
# 15 282        TP
# 16 285        FN
# 17 286        FN
# 18 288        TP
# 19 289        TP
# 20 290        TP
# 21 291        TP
# 22 292        TP
# 23 293        TP
# 24 296        FN
# 25 297        FN
# 26 299        TP

#write.table(sim77_flagged_df,"sim77_flagged_samples2.csv",row.names=F,quote=F,sep=",")

sim77_flagged_false <- sim77_flagged_df %>% filter(pred_type=="FP" | pred_type=="FN")
sim77_flagged_false
#     id pred_type
# 1  241        FP
# 2  244        FP
# 3  245        FP
# 4  247        FP
# 5  250        FP
# 6  252        FP
# 7  253        FP
# 8  254        FP
# 9  285        FN
# 10 286        FN
# 11 296        FN
# 12 297        FN

#write.table(sim77_flagged_false,"sim77_flagged_false_samples2.csv",row.names=F,quote=F,sep=",")


sim77_actual_false <- sim77_test_samples %>% filter(pred_type == "FP" | pred_type == "FN") 
head(sim77_actual_false)
#    id pred_type prob_predicted   mainvar9   mainvar4    mainvar1      intvar8
# 1 241        FP         0.6562 0.05331005 0.05501032 -0.01934459  0.014536423
# 2 242        FP         0.5472 0.15459561 0.15686735  0.11763409 -0.019552476
# 3 243        FP         0.7616 0.04879970 0.05102219 -0.01403457 -0.009022740
# 4 244        FP         0.5583 0.04189608 0.04333365 -0.01611294 -0.007316877
# 5 245        FP         0.6851 0.07113532 0.07441676  0.02503753  0.013823077
# 6 247        FP         0.7009 0.04824810 0.05025749  0.01988033  0.012488539
#         intvar3   mainvar8    mainvar2         var64        var35 total_local_score
# 1 -0.0066419503 0.04628125 -0.03831912  0.0097324466 -0.016500062        0.09806477
# 2  0.0113414819 0.14392528  0.13530139 -0.0148737348 -0.062833219        0.62240577
# 3 -0.0022673004 0.04512890  0.03778255  0.0034280623 -0.009922201        0.15091460
# 4 -0.0007612782 0.03548631 -0.03429668 -0.0009114302  0.013416651        0.07473349
# 5  0.0004965147 0.05678308 -0.02608076  0.0018841517  0.016704415        0.23420009
# 6  0.0017713036 0.04113771  0.03663271 -0.0069674168 -0.017246481        0.18620230
dim(sim77_actual_false)
# [1] 17 13

sim77_notFlagged <- sim77_actual_false %>% filter(!(id %in% sim77_flagged_false$id))
dim(sim77_notFlagged)
# [1]  5 13
sim77_notFlagged
#    id pred_type prob_predicted   mainvar9   mainvar4    mainvar1     intvar8
# 1 242        FP         0.5472 0.15459561 0.15686735  0.11763409 -0.01955248
# 2 243        FP         0.7616 0.04879970 0.05102219 -0.01403457 -0.00902274
# 3 248        FP         0.7534 0.04668172 0.04936418 -0.02640841  0.01253612
# 4 251        FP         0.6747 0.10665977 0.11687619  0.04665754  0.03542659
# 5 255        FP         0.8567 0.28263086 0.29178368 -0.14419075 -0.12460437
#         intvar3   mainvar8    mainvar2        var64        var35 total_local_score
# 1  1.134148e-02 0.14392528  0.13530139 -0.014873735 -0.062833219        0.62240577
# 2 -2.267300e-03 0.04512890  0.03778255  0.003428062 -0.009922201        0.15091460
# 3  6.491884e-05 0.03719133 -0.02766848  0.007184486 -0.013456915        0.08548895
# 4  6.163901e-03 0.10402622  0.07469159 -0.021853471 -0.037295162        0.43135317
# 5  2.935432e-03 0.26792518  0.25968528  0.026446011  0.143981952        1.00659327

dim(sim77_flagged_false)[1]/dim(sim77_actual_false)[1]
# [1] 0.7058824

#write.table(sim77_actual_false,"sim77_test_allFalse_pred.csv",row.names=F,quote=F,sep=",")
#write.table(sim77_notFlagged,"sim77_test_false_notFlagged.csv",row.names=F,quote=F,sep=",")


# test function on sim79 and sim85 data
sim79_fp_id <- sim79_FP_local_vars$id
sim79_fn_id <- sim79_FN_local_vars$id
sim79_false_id <- c(sim79_fp_id,sim79_fn_id)

sim85_fp_id <- sim85_FP_local_vars$id
sim85_fn_id <- sim85_FN_local_vars$id
sim85_false_id <- c(sim85_fp_id,sim85_fn_id)

bio_FP_local_vars<- read.csv("./bio/bio_FP_localVarImp.csv")
bio_FN_local_vars<-read.csv("./bio/bio_FN_localVarImp.csv")

bio_fp_id <- bio_FP_local_vars$id
bio_fn_id <- bio_FN_local_vars$id
bio_false_id <- c(bio_fp_id,bio_fn_id)


sim79_false_samples = sim79_final_local_df %>% filter(train_test=="test") %>%
  filter(id %in% sim79_false_id) %>% 
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim79_lurf_feat),total_local_score)
head(sim79_false_samples)
#    id pred_type prob_predicted   mainvar8     intvar8    mainvar9   mainvar7         var14        var64
# 1 242        FP         0.6501 0.06049848  0.01091123 -0.01521056 0.06768964  5.169613e-05 -0.001329321
# 2 243        FP         0.5873 0.14208986  0.05688607  0.05838391 0.25454611  3.106560e-02 -0.036353569
# 3 244        FP         0.6947 0.15803339  0.04314422  0.09081508 0.16528965 -1.225460e-02 -0.013211648
# 4 246        FP         0.6943 0.12882795  0.02934004  0.04048401 0.13061671  1.185309e-02 -0.015226776
# 5 247        FP         0.6482 0.09960238 -0.01993778 -0.08567115 0.13192669 -5.585350e-04 -0.004763122
# 6 249        FP         0.6547 0.09059648  0.03485644  0.07218777 0.13397771  8.781118e-03 -0.010610261
# intvar7        var67 total_local_score
# 1 -0.00425700  0.001831637         0.1201858
# 2 -0.05676918 -0.049841117         0.4000077
# 3  0.01834590  0.015838469         0.4660005
# 4 -0.02129116  0.016181680         0.3207856
# 5 -0.01058544  0.006658528         0.1166716
# 6 -0.01607162  0.011467150         0.3251848
dim(sim79_false_samples)
# [1] 13 12


sim79_test_samples = sim79_final_local_df %>% filter(train_test=="test") %>%
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim79_lurf_feat),total_local_score)
head(sim79_test_samples)
# id pred_type prob_predicted    mainvar8     intvar8    mainvar9    mainvar7         var14        var64
# 1 241        TN         0.6144 -0.07528322  0.02305535  0.02740960 -0.16496970 -4.235721e-03  0.006671372
# 2 242        FP         0.6501  0.06049848  0.01091123 -0.01521056  0.06768964  5.169613e-05 -0.001329321
# 3 243        FP         0.5873  0.14208986  0.05688607  0.05838391  0.25454611  3.106560e-02 -0.036353569
# 4 244        FP         0.6947  0.15803339  0.04314422  0.09081508  0.16528965 -1.225460e-02 -0.013211648
# 5 245        TN         0.6607 -0.03654420 -0.01343548 -0.02645692 -0.08971094  1.499608e-03 -0.001516740
# 6 246        FP         0.6943  0.12882795  0.02934004  0.04048401  0.13061671  1.185309e-02 -0.015226776
# intvar7        var67 total_local_score
# 1  0.008430727  0.007771316        -0.1711503
# 2 -0.004257000  0.001831637         0.1201858
# 3 -0.056769182 -0.049841117         0.4000077
# 4  0.018345896  0.015838469         0.4660005
# 5  0.007479196 -0.006780989        -0.1654665
# 6 -0.021291158  0.016181680         0.3207856
dim(sim79_test_samples)
# [1] 60 12


sim85_false_samples = sim85_final_local_df %>% filter(train_test=="test") %>%
  filter(id %in% sim85_false_id) %>% 
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim85_lurf_feat),total_local_score)
head(sim85_false_samples)
# id pred_type prob_predicted    mainvar5    mainvar9    mainvar1    mainvar4    mainvar7   mainvar10
# 1 405        FP         0.8360  0.04254728  0.05696592  0.03029665  0.03760645  0.01878431  0.02491966
# 2 408        FP         0.5794  0.10176882 -0.11800248 -0.08018334 -0.09071201  0.06051827  0.08015163
# 3 411        FP         0.5083 -0.04075626  0.04510972  0.02391537 -0.04028793 -0.01122805 -0.01598300
# 4 417        FP         0.6629  0.21812298 -0.26508847  0.13406778  0.19463870 -0.03094397  0.13344144
# 5 432        FP         0.5975  0.06500397  0.07168970 -0.05218249 -0.06260800  0.02609795 -0.04290217
# 6 438        FP         0.6511  0.06324296 -0.07826380  0.05434736 -0.05999095  0.03025926  0.04707679
# mainvar3     mainvar8      intvar4    mainvar6 total_local_score
# 1 -0.03588126  0.007070624  0.004329177 -0.02345576        0.16318305
# 2  0.08321785  0.025281351 -0.012931351 -0.06503336       -0.01592462
# 3  0.03257707  0.007337838  0.006820921  0.01592808        0.02343374
# 4  0.15939252 -0.019103470  0.017120876 -0.03955344        0.50209495
# 5 -0.05281829  0.017945774 -0.011132887  0.03039085       -0.01051560
# 6 -0.05801780  0.029251662 -0.010266805  0.03999992        0.05763860
dim(sim85_false_samples)
# [1] 20 14


sim85_test_samples = sim85_final_local_df %>% filter(train_test=="test") %>%
  mutate(prob_predicted=ifelse(pred==1, prob_1, prob_0)) %>%
  select(id, pred_type, prob_predicted,all_of(sim85_lurf_feat),total_local_score)
head(sim85_test_samples)
# id pred_type prob_predicted    mainvar5   mainvar9   mainvar1    mainvar4     mainvar7   mainvar10
# 1 401        TN         0.8384  0.06882939 0.07434347 0.05620852  0.05853253 -0.042345709  0.05241669
# 2 402        TN         0.6411 -0.06038397 0.07499926 0.05594973  0.05867190 -0.042481533 -0.05331091
# 3 403        TN         0.6608 -0.07234697 0.08362638 0.06270593 -0.06562932 -0.029201726  0.05972078
# 4 404        TN         0.8956  0.11783011 0.12389336 0.09236116  0.09724535  0.063968966  0.08051913
# 5 405        FP         0.8360  0.04254728 0.05696592 0.03029665  0.03760645  0.018784312  0.02491966
# 6 406        TN         0.8206  0.04891347 0.04975496 0.03386247 -0.04016397 -0.005465392  0.02824698
# mainvar3     mainvar8      intvar4    mainvar6 total_local_score
# 1  0.05663776 -0.031570797 -0.002221140  0.04976781        0.34059853
# 2  0.05650843 -0.031351109  0.003913942  0.04617024        0.10868598
# 3  0.06378033 -0.022888172  0.004262658 -0.04139874        0.04263115
# 4  0.09441642  0.021865124  0.004559366 -0.06941997        0.62723902
# 5 -0.03588126  0.007070624  0.004329177 -0.02345576        0.16318305
# 6  0.03760383  0.005446770 -0.004813472 -0.02442797        0.12895767
dim(sim85_test_samples)
# [1] 100 14

bio_false_samples = bio_final_local_df %>% filter(train_test=="test") %>%
  filter(id %in% bio_false_id) %>% 
  mutate(prob_predicted=ifelse(pred=="biotic", prob_biotic, prob_abiotic)) %>%
  select(id, pred_type, prob_predicted,all_of(bio_lurf_feat),total_local_score)
head(bio_false_samples)
#     id pred_type prob_predicted avg_rR45CO244CO2 sd_d18O13C diff2_acf1
# 1 5911        FN         0.6329        -63.68983  -4.386212   4.089409
# 2 5913        FN         0.9919         12.16319   9.637173  -8.085310
# 3 5956        FP         0.5550        -59.18707   7.565676  -5.359420
#   fluctanal_prop_r1 time_kl_shift total_local_score
# 1         -3.575741  2.435979e-05         -67.56235
# 2          6.264666  2.363434e-05          19.97975
# 3          4.219251 -2.771710e-05         -52.76159
dim(bio_false_samples)
# [1] 3 9


bio_test_samples = bio_final_local_df %>% filter(train_test=="test") %>%
  mutate(prob_predicted=ifelse(pred=="biotic", prob_biotic, prob_abiotic)) %>%
  select(id, pred_type, prob_predicted,all_of(bio_lurf_feat),total_local_score)
head(bio_test_samples)
#  id pred_type prob_predicted avg_rR45CO244CO2  sd_d18O13C diff2_acf1 fluctanal_prop_r1 time_kl_shift
# 1 2960        TP         0.9877        490.37365 -128.831469  24.462211         16.172007  1.192324e-04
# 2 2992        TP         0.9489        147.20635  -12.182084   8.844626          4.199424 -1.211280e-05
# 3 2996        TP         0.9507         53.17841   -8.692205   4.255948          1.878912 -8.219488e-06
# 4 3231        TN         0.9655         39.43314   -6.098220   5.093769          2.315945  1.151089e-05
# 5 3232        TN         0.9836       -403.28555   23.920641  18.457613        -17.230575  6.310621e-06
# 6 3255        TN         0.9885       -343.71967   20.294786 -13.090456         11.842958 -3.216816e-05
# total_local_score
# 1         402.17652
# 2         148.06831
# 3          50.62106
# 4          40.74464
# 5        -378.13787
# 6        -324.67241
dim(bio_test_samples)
# [1] 34 9



## sim79 
hist(sim79_final_local_df$total_local_score,breaks = 20,
     main="sim79 total local importance scores",xlab="total local importance score")

hist(sim79_false_samples$total_local_score,breaks=20,
     main="sim77 false prediction total local importance scores",
     xlab="false prediction total local importance score")

sum(sim79_test_samples$total_local_score<0.35)
# 42
sum(sim79_false_samples$total_local_score<0.35)
# 8

sim79_flagged_df <- flag_predictions(sim79_test_samples,
                                     prob_thresh=0.75,
                                     total_local_score_thresh=0.35)
sim79_flagged_df
#   id pred_type
# 1  241        TN
# 2  242        FP
# 3  245        TN
# 4  246        FP
# 5  247        FP
# 6  248        TN
# 7  249        FP
# 8  252        FP
# 9  253        TN
# 10 254        FP
# 11 255        FP
# 12 277        TP
# 13 279        TP
# 14 281        FN
# 15 282        TP
# 16 286        TP
# 17 289        TP
# 18 290        TP
# 19 291        TP
# 20 293        TP
# 21 295        TP

#write.table(sim79_flagged_df,"sim79_flagged_samples2.csv",row.names=F,quote=F,sep=",")


sim79_flagged_false <- sim79_flagged_df %>% filter(pred_type=="FP" | pred_type=="FN")
sim79_flagged_false
#    id pred_type
# 1 242        FP
# 2 246        FP
# 3 247        FP
# 4 249        FP
# 5 252        FP
# 6 254        FP
# 7 255        FP
# 8 281        FN
# 8 281        FP

#write.table(sim79_flagged_false,"sim79_flagged_false_samples2.csv",row.names=F,quote=F,sep=",")


sim79_actual_false <- sim79_test_samples %>% filter(pred_type == "FP" | pred_type == "FN") 
dim(sim79_actual_false)
# [1] 13 12


sim79_notFlagged <- sim79_actual_false %>% filter(!(id %in% sim79_flagged_false$id))
dim(sim79_notFlagged)
# [1]  5 12
sim79_notFlagged
# sim79_notFlagged
#    id pred_type prob_predicted  mainvar8     intvar8   mainvar9  mainvar7        var14
# 1 243        FP         0.5873 0.1420899  0.05688607 0.05838391 0.2545461  0.031065602
# 2 244        FP         0.6947 0.1580334  0.04314422 0.09081508 0.1652896 -0.012254595
# 3 250        FP         0.5091 0.2367815 -0.05573126 0.15556740 0.2879151 -0.003650495
# 4 251        FP         0.5188 0.4853785  0.19496785 0.21240120 0.5265563 -0.003872553
# 5 256        FP         0.8397 0.1068074  0.03676030 0.07204996 0.2379265  0.003438944
#          var64     intvar7       var67 total_local_score
# 1 -0.036353569 -0.05676918 -0.04984112         0.4000077
# 2 -0.013211648  0.01834590  0.01583847         0.4660005
# 3 -0.024239783 -0.04968404 -0.03735821         0.5096002
# 4  0.064228508  0.13192762 -0.09213802         1.5194495
# 5 -0.005308543 -0.01575820 -0.01000184         0.4259146


#write.table(sim79_actual_false,"sim79_test_allFalse_pred.csv",row.names=F,quote=F,sep=",")
#write.table(sim79_notFlagged,"sim79_test_false_notFlagged.csv",row.names=F,quote=F,sep=",")


dim(sim79_flagged_false)[1]/dim(sim79_actual_false)[1]
# [1] 0.6153846



## sim85 data
hist(sim85_final_local_df$total_local_score,breaks = 20,
     main="sim85 total local importance scores",xlab="total local importance score")

hist(sim85_false_samples$total_local_score,breaks=20,
     main="sim85 false prediction total local importance scores",
     xlab="false prediction total local importance score")

sum(sim85_test_samples$total_local_score<0.35)
# 80
sum(sim85_false_samples$total_local_score<0.35)
# 19

sim85_flagged_df <- flag_predictions(sim85_test_samples,
                                     prob_thresh=0.75,
                                     total_local_score_thresh=0.35)
sim85_flagged_df
#     id pred_type
# 1  402        TN
# 2  403        TN
# 3  407        TN
# 4  408        FP
# 5  409        TN
# 6  411        FP
# 7  412        TN
# 8  416        TN
# 9  420        TN
# 10 423        TN
# 11 424        TN
# 12 426        TN
# 13 427        TN
# 14 429        TN
# 15 430        TN
# 16 432        FP
# 17 433        TN
# 18 434        TN
# 19 437        TN
# 20 438        FP
# 21 440        FP
# 22 441        FP
# 23 444        FP
# 24 445        FP
# 25 446        TN
# 26 447        TN
# 27 448        FP
# 28 450        TN
# 29 451        TP
# 30 452        FN
# 31 454        TP
# 32 458        TP
# 33 459        TP
# 34 460        TP
# 35 462        FN
# 36 464        FN
# 37 465        TP
# 38 466        TP
# 39 469        TP
# 40 470        TP
# 41 474        FN
# 42 475        TP
# 43 476        TP
# 44 477        TP
# 45 479        FN
# 46 481        FN
# 47 483        TP
# 48 485        TP
# 49 486        TP
# 50 489        TP
# 51 490        FN
# 52 498        FN
# 53 500        FN

#write.table(sim85_flagged_df,"sim85_flagged_samples2.csv",row.names=F,quote=F,sep=",")



sim85_flagged_false <- sim85_flagged_df %>% filter(pred_type=="FP" | pred_type=="FN")
sim85_flagged_false
#     id pred_type
# 1  408        FP
# 2  411        FP
# 3  432        FP
# 4  438        FP
# 5  440        FP
# 6  441        FP
# 7  444        FP
# 8  445        FP
# 9  448        FP
# 10 452        FN
# 11 462        FN
# 12 464        FN
# 13 474        FN
# 14 479        FN
# 15 481        FN
# 16 490        FN
# 17 498        FN
# 18 500        FN

#write.table(sim85_flagged_false,"sim85_flagged_false_samples2.csv",row.names=F,quote=F,sep=",")

sim85_actual_false <- sim85_test_samples %>% filter(pred_type == "FP" | pred_type == "FN") 
dim(sim85_actual_false)
# [1] 20 14



sim85_notFlagged <- sim85_actual_false %>% filter(!(id %in% sim85_flagged_false$id))
dim(sim85_notFlagged)
# [1]  2 14
sim85_notFlagged
#    id pred_type prob_predicted   mainvar5    mainvar9   mainvar1   mainvar4
# 1 405        FP         0.8360 0.04254728  0.05696592 0.03029665 0.03760645
# 2 417        FP         0.6629 0.21812298 -0.26508847 0.13406778 0.19463870
#      mainvar7  mainvar10    mainvar3     mainvar8     intvar4    mainvar6
# 1  0.01878431 0.02491966 -0.03588126  0.007070624 0.004329177 -0.02345576
# 2 -0.03094397 0.13344144  0.15939252 -0.019103470 0.017120876 -0.03955344
#   total_local_score
# 1         0.1631830
# 2         0.5020949

#write.table(sim85_actual_false,"sim85_test_allFalse_pred.csv",row.names=F,quote=F,sep=",")
#write.table(sim85_notFlagged,"sim85_test_false_notFlagged.csv",row.names=F,quote=F,sep=",")




dim(sim85_flagged_false)[1]/dim(sim85_actual_false)[1]
# [1] 0.9


#### biotic samples
hist(bio_final_local_df$total_local_score,breaks = 20,
     main="biosignature data total local importance scores",xlab="total local importance score")

hist(bio_false_samples$total_local_score,breaks=20,
     main="biosignature false prediction total local importance scores",
     xlab="false prediction total local importance score")

sum(bio_test_samples$total_local_score<25)
# 22
sum(bio_false_samples$total_local_score<25)
# 3

bio_flagged_df <- flag_predictions(bio_test_samples,
                                     prob_thresh=0.75,
                                     total_local_score_thresh=25)
bio_flagged_df
#     id pred_type
# 1 5881        TP
# 2 5911        FN
# 3 5939        TP
# 4 5944        TP
# 5 5956        FP

#write.table(bio_flagged_df,"bio_flagged_samples.csv",row.names=F,quote=F,sep=",")
bio_flagged_df<-read.csv("./data/bio/bio_flagged_samples.csv")

bio_flagged_false <- bio_flagged_df %>% filter(pred_type=="FP" | pred_type=="FN")
bio_flagged_false
#  id pred_type
# 1 5911        FN
# 2 5956        FP

#write.table(bio_flagged_false,"bio_flagged_false_samples.csv",row.names=F,quote=F,sep=",")


bio_actual_false <- bio_test_samples %>% filter(pred_type == "FP" | pred_type == "FN") 
dim(bio_actual_false)
# [1] 3 9

bio_notFlagged <- bio_actual_false %>% filter(!(id %in% bio_flagged_false$id))
dim(bio_notFlagged)
# [1]  1 9
bio_notFlagged
#     id pred_type prob_predicted avg_rR45CO244CO2 sd_d18O13C diff2_acf1
# 1 5913        FN         0.9919         12.16319   9.637173   -8.08531
#   fluctanal_prop_r1 time_kl_shift total_local_score
# 1          6.264666  2.363434e-05          19.97975

dim(bio_flagged_false)[1]/dim(bio_actual_false)[1]
# [1] 0.6666667


