library(npdr)
library(igraph)
library(umap)
library(ggplot2)
library(dplyr)
setwd("~/Documents/Papers/npdr_umap/single_sample/local_knn_regain")
sim="bio"
switch(sim,
       "77"={
      traindat = read.csv("./sim77/sim77_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim77/sim77_testPred_lurfRF.csv", header = TRUE)
      dist_lurf = read.csv("./sim77/sim77_lurf_urfp_dist_allSamples.csv", header = TRUE)
    wide_summary = read.csv("./sim77/sim77_final_local_data.csv", header = TRUE)
       },
       "85"={
      traindat = read.csv("./sim85/sim85_trainPred_lurfRF.csv", header = TRUE) 
      testdat = read.csv("./sim85/sim85_testPred_lurfRF.csv", header = TRUE)
      dist_lurf = read.csv("./sim85/sim85_lurf_urfp_dist_allSamples.csv", header = TRUE)
      wide_summary = read.csv("./sim85/sim85_final_local_data.csv", header = TRUE)
       },
    "bio"={
      traindat = read.csv("./bio/bio_trainPred_lurf0.csv", header = TRUE) 
      testdat = read.csv("./bio/bio_testPred_lurf0.csv", header = TRUE)
      dist_lurf = read.csv("./bio/bio_lurf_urfp_dist.csv", header = TRUE)
      wide_summary = read.csv("./bio/bio_final_local_data.csv", header = TRUE)
      traindat = traindat %>% mutate(class = biotic) %>% select(-biotic)
      testdat = testdat %>% mutate(class = biotic) %>% select(-biotic)
      flagged = read.csv("./bio/bio_flagged_samples.csv", header = TRUE)
      #flagged
      #wide_summary %>% filter(train_test=="test") %>% select(id, pred_type) %>% filter(pred_type=="FP" | pred_type=="FN")
    }
)

pred_type = wide_summary$pred_type
false_idx = ifelse(pred_type=="FP" | pred_type=="FN", 1, 2)

alldat = rbind(traindat, testdat)
alldat = alldat %>% select(-Analysis)
alldat_predonly = alldat[,seq(1,ncol(alldat)-2)]

# Make the local knn graph
npdr_nbpairs_idx = nearestNeighbors(dist_lurf, 
                                    nbd.metric = "precomputed", 
                                    #k=15,
                                    # knnSURF.balanced is 73
                                    k=knnSURF.balanced(alldat$class,.5)
                                    )
knn_graph = graph_from_edgelist(as.matrix(npdr_nbpairs_idx),directed=F)
knn_graph = simplify(knn_graph)  

# greedy and louvaine, default params, 2 clusters
louvain = cluster_louvain(knn_graph)
greedy =cluster_fast_greedy(knn_graph)
table(louvain$membership)  # has more clusters than greedy
table(greedy$membership)
table(louvain$membership,alldat$pred)
table(greedy$membership,alldat$pred)
table(greedy$membership,pred_type)
table(greedy$membership,alldat$class)

# leiden using parameter, 4 clusters
r = quantile(strength(knn_graph))[2] / (gorder(knn_graph) - 1)
# Set seed for sake of reproducibility
ldc = cluster_leiden(knn_graph, resolution_parameter=r)
table(ldc$membership)
table(ldc$membership,alldat$pred)
table(ldc$membership,alldat$class)
table(ldc$membership, pred_type)
plot(ldc, knn_graph, vertex.label=pred_type)

# shape by greedy cluster, color by class
clrs = c("red", "green")
shps = c("circle", "square")
V(knn_graph)$color = clrs[alldat$class+1]
V(knn_graph)$shape = shps[greedy$membership]
plot(knn_graph, mark.groups=greedy$membership, vertex.label=pred_type,
     edge.color = "grey50", vertex.shapes=V(knn_graph)$shape, vertex.size=8,
     main="shape by greedy cluster, color by class, label pred type")

# shape by greedy cluster, color by false(red)
clrs = c("red", "green")
shps = c("circle", "square")
V(knn_graph)$color = clrs[false_idx]
V(knn_graph)$shape = shps[greedy$membership]
plot(knn_graph, mark.groups=greedy$membership, vertex.label=pred_type,
     edge.color = "grey50", vertex.shapes=V(knn_graph)$shape, vertex.size=8,
     main="shape by greedy cluster, color by false(red), label pred type")

# umap with nbpdr lurf nearest neighbors
# change umap config parameters
custom.config = umap.defaults
custom.config$random_state = 123
custom.config$n_epochs = 500
custom.config$n_neighbors = knnSURF.balanced(alldat$class,.5)  
# could experiment 
# umap to cluster observations
# it computes distance in its own way, which you can modify
obs_umap = umap(alldat_predonly, config=custom.config, knn=npdr_nbpairs_idx)
# $layout is the matrix of x-y umap coordinates
obs_umap_df = data.frame(dim1=obs_umap$layout[,1], dim2=obs_umap$layout[,2])

umap_gg_df = obs_umap_df
umap_gg_df$class = as.factor(alldat$class)
umap_gg_df$knn_cluster = as.factor(greedy$membership)
umap_gg_df$false_idx = as.factor(false_idx)
umap_gg_df$pred_type = as.factor(pred_type)
ugg = ggplot(data=umap_gg_df, aes(y=dim2))
ugg = ugg + geom_point(aes(x=dim1, color=pred_type, shape=class), size=5)
print(ugg)

if (sim=="bio"){
  flagged_ids = which(wide_summary$id %in% c(5911,5913,5956))
  pred_type2 = pred_type
  #pred_type2[flagged_ids] = "flagged"
  pred_type2[flagged_ids] = c(5911,5913,5956)
  umap_gg_df = obs_umap_df
  umap_gg_df$class = as.factor(alldat$class)
  umap_gg_df$knn_cluster = as.factor(greedy$membership)
  umap_gg_df$false_idx = as.factor(false_idx)
  umap_gg_df$pred_type = as.factor(pred_type)
  umap_gg_df$pred_type2 = as.factor(pred_type2)
  sizes = rep(5,nrow(umap_gg_df))
  sizes[flagged_ids] = 15
  ugg = ggplot(data=umap_gg_df, aes(y=dim2))
  ugg = ugg + geom_point(aes(x=dim1, color=pred_type2, shape=class), 
                         size=sizes)
  print(ugg)
}

########### biotic regain ###############

alldat$class = as.factor(alldat$class)
regain <- npdr::regain(alldat,
                       indVarNames=colnames(alldat_predonly),
                        depVarName="class",
                        reg.fam="binomial",
                        nCovs=0,
                        excludeMainEffects=F)

#### preliminary look at regain results
betas <- as.matrix(regain$stdbetas)
regain.nodiag <- betas
diag(regain.nodiag) <- 0

### Cumulative interactions of each variable IGNORING SIGN
# degree of regain matrix without main effects.
# some interaction regression might fail
regain.nodiag.deg <- rowSums(abs(regain.nodiag))

# Ranking here is cumulative interactions a variable has
regain.nodiag.deg[order(regain.nodiag.deg, decreasing = TRUE)]/length(regain.nodiag.deg)
#diff2_acf1  avg_rR45CO244CO2 fluctanal_prop_r1     time_kl_shift 
#0.9788699         0.9644128         0.8298084         0.6429069 
#sd_d18O13C 
#0.3898518 

### Main effects
main.effs.abs <- abs(diag(betas))
# Main effects of each variable
diag(betas)[order(main.effs.abs, decreasing = TRUE)]
#fluctanal_prop_r1        diff2_acf1  avg_rR45CO244CO2        sd_d18O13C 
#4.0779108        -3.1756680        -2.8308687         2.4025313 
#time_kl_shift 
#0.7247328 

### filter regain matrix for plotting
# Threshold regain-nodiag matrix for plot
# Interactions can be negative, so do abs
# if you only want to see positive interactions, remove abs
THRESH=0 # 1.2
hist(abs(regain.nodiag),breaks=20)
regain.nodiag.adj <- as.matrix(abs(regain.nodiag)>THRESH)+0  # remove edges below threshold
regain.nodiag.weight <- regain.nodiag.adj*regain.nodiag

## the following will remove unconnected nodes in the network
# you might want to keep them in case they have a main effect of interest
# keep non-zero degree
rsums<-rowSums(regain.nodiag.adj)
na_mask <- !is.na(rsums)
regain.nodiag.adj <- regain.nodiag.adj[na_mask,na_mask]
regain.nodiag.weight <- regain.nodiag.weight[na_mask,na_mask]
beta_diag <- diag(betas)[na_mask]
rsums2 <- rowSums(regain.nodiag.adj)
degree_mask <- rsums2>0 
regain.nodiag.adj <- regain.nodiag.adj[degree_mask,degree_mask]
regain.nodiag.weight <- regain.nodiag.weight[degree_mask,degree_mask]
beta_diag <- beta_diag[degree_mask]

### adjacency matrix
A.adj <- graph_from_adjacency_matrix(regain.nodiag.adj, mode="undirected")
A.weight <- graph_from_adjacency_matrix(regain.nodiag.weight, mode="undirected", weight=T)
my.layout <- layout_with_fr(A.adj, niter = 10000)

# make green a postive effect
# biotic has larger mean and these are green
t.test(formula=sd_d18O13C~class,data=alldat)
t.test(formula=time_kl_shift~class,data=alldat)
t.test(formula=fluctanal_prop_r1~class,data=alldat)
# verify negative
t.test(formula=diff2_acf1~class,data=alldat)
t.test(formula=avg_rR45CO244CO2~class,data=alldat)
glm(class~sd_d18O13C,data=alldat, family=binomial)
# Coefficients:
# (Inntercept)   sd_d18O13C  
# -1.404      231.103      # positive coefficient
# abiotic is the reference group
# positive beta means increased chance of being biotic
vertices.colors <- rep("limegreen", rep(length(V(A.adj))))
vertices.colors[beta_diag<0] <- "gray" 

# make negative interactions green as  well
E(A.adj)$color <- 'limegreen'
E(A.adj)$color[E(A.weight)$weight < 0] <- 'gray'
E(A.adj)$lty <- 1
E(A.adj)$lty[E(A.weight)$weight < 0] <- 2  # line type  

# epistasis-rank centrality
er.ranks <- npdr::EpistasisRank(betas,Gamma_vec = .85, magnitude.sort=T)
er.ranks
#gene         ER
#2     time_kl_shift -5.4514216
#4  avg_rR45CO244CO2  5.1095990
#5        sd_d18O13C  1.9806025
#1        diff2_acf1 -1.5587236
#3 fluctanal_prop_r1  0.9199438
## old
#  gene          ER
# 6        sd_d18O13C  0.54083111
# 5   avg_R45CO244CO2  0.43958932
# 2     time_kl_shift  0.24583652
# 4  avg_rR45CO244CO2 -0.17361776
# 1        diff2_acf1 -0.07122914
# 3 fluctanal_prop_r1  0.01858995             

plot.igraph(A.adj,layout=my.layout, 
            vertex.color=vertices.colors,
            edge.width=abs(E(A.weight)$weight)*1.5, # make edges easier to see
            vertex.label.dist=1.5, 
            vertex.label.color="black",
            #vertex.size = regain.filter.degree # could size by degree
            vertex.size=abs(beta_diag)*5  # size by main effect
            #vertex.size=abs(er.ranks[order(as.numeric(row.names(er.ranks))), 2])*75 # size by ER rank
)

# lets you manually edit the layout and save as a high quality image
tkplot(A.adj,layout=my.layout, 
       vertex.color=vertices.colors,     
       edge.width=abs(E(A.weight)$weight)*1, 
       vertex.label.dist=1.5, vertex.label.color="black",
       #vertex.size = regain.filter.degree # size by degree
       vertex.size=abs(beta_diag)*5  # size by main effect
       #vertex.size=abs(er.ranks[order(as.numeric(row.names(er.ranks))), 2])*75 # size by ER
)
tk_close() # will generate warning