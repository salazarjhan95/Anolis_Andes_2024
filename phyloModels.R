library(ape)
library(geiger)
library(nlme)
library(phytools)

setwd("")

# Load tree
anolistree <- read.nexus("mcc_thinned_allruns.trees")
plot(anolistree)
is.rooted(anolistree)
ladderize(anolistree)


##########################################################################
######                                                              ######
######         phyloModel for CTmax                                 ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######                                                              ######
##########################################################################
# Load the data
CTmax_data <- read.csv("New_PGLS_CTmax_Sept24.csv", header=TRUE)
rownames(CTmax_data) <- CTmax_data$Sp

# Prune the tree to match the species in the data
anolespecies_ctmax <- row.names(CTmax_data)
pruned_anolis_ctmax <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies_ctmax))
ladderize(pruned_anolis_ctmax)
plotTree(pruned_anolis_ctmax)

# Order data to match the tree tip labels
CTmax_data <- CTmax_data[pruned_anolis_ctmax$tip.label, ]
ctmax <- setNames(CTmax_data$ctmax, rownames(CTmax_data))

#Using the package geiger
fitmax_BM<-fitContinuous(pruned_anolis_ctmax, ctmax, model = c("BM"))
fitmax_BM
fitmax_OU<-fitContinuous(pruned_anolis_ctmax, ctmax, model = c("OU"))
fitmax_OU
fitmax_EB<-fitContinuous(pruned_anolis_ctmax, ctmax, model = c("EB"))
fitmax_EB
fitmax_delta<-fitContinuous(pruned_anolis_ctmax, ctmax, model = c("delta"))
fitmax_delta
fitmax_WN<-fitContinuous(pruned_anolis_ctmax, ctmax, model = c("white"))
fitmax_WN

## compare model fit using Akaike weights:
# Extracting AIC values
aic.vals<-setNames(c(fitmax_BM$opt$aic,  fitmax_OU$opt$aic, fitmax_EB$opt$aic,
                     fitmax_delta$opt$aic, fitmax_WN$opt$aic),
                   c("BM","OU","EB", "delta", "white"))

# Extracting AIC weight values
aic.w <- aic.w(aic.vals)

# Extracting liklog values
lnL<-setNames(c(fitmax_BM$opt$lnL,  fitmax_OU$opt$lnL, fitmax_EB$opt$lnL,
                fitmax_delta$opt$lnL, fitmax_WN$opt$lnL),
              c("BM","OU","EB", "delta", "white"))

# Extracting AICc  values
aicc <-setNames(c(fitmax_BM$opt$aicc,  fitmax_OU$opt$aicc, fitmax_EB$opt$aicc,
                  fitmax_delta$opt$aicc, fitmax_WN$opt$aicc),
                c("BM","OU","EB", "delta", "white"))

# Merging everything together
EvoModel <- cbind(aic.vals, aicc, aic.w, lnL)
# Naming each row
rownames(EvoModel)<-c("BM", "OU", "EB", "Delta", "White Noise")
# Names each column
colnames(EvoModel)<-c("AIC", "AICc", "AICw", "Lik")
# Making what we just did into a data frame
EvoModel <- as.data.frame(EvoModel)

# Saving everything into a .csv
write.csv(EvoModel, file = "EvoModel_CTmax.csv")



##########################################################################
######                                                              ######
######         phyloModel for CTmin                                 ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######                                                              ######
##########################################################################
# Load the data
CTmin_data <- read.csv("New_PGLS_CTmin_Sept24.csv", header=TRUE)
rownames(CTmin_data) <- CTmin_data$Sp

# Prune the tree to match the species in the data
anolespecies_ctmin <- row.names(CTmin_data)
pruned_anolis_ctmin <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies_ctmin))
ladderize(pruned_anolis_ctmin)
plotTree(pruned_anolis_ctmin)

# Order data to match the tree tip labels
CTmin_data <- CTmin_data[pruned_anolis_ctmin$tip.label, ]
ctmin <- setNames(CTmin_data$ctmin, rownames(CTmin_data))

#Using the package geiger
fitmin_BM<-fitContinuous(pruned_anolis_ctmin, ctmin, model = c("BM"))
fitmin_BM
fitmin_OU<-fitContinuous(pruned_anolis_ctmin, ctmin, model = c("OU"))
fitmin_OU
fitmin_EB<-fitContinuous(pruned_anolis_ctmin, ctmin, model = c("EB"))
fitmin_EB
fitmin_delta<-fitContinuous(pruned_anolis_ctmin, ctmin, model = c("delta"))
fitmin_delta
fitmin_WN<-fitContinuous(pruned_anolis_ctmin, ctmin, model = c("white"))
fitmin_WN

## compare model fit using Akaike weights:
# Extracting AIC values
aic.vals<-setNames(c(fitmin_BM$opt$aic,  fitmin_OU$opt$aic, fitmin_EB$opt$aic,
                     fitmin_delta$opt$aic, fitmin_WN$opt$aic),
                   c("BM","OU","EB", "delta", "white"))

# Extracting AIC weight values
aic.w <- aic.w(aic.vals)

# Extracting liklog values
lnL<-setNames(c(fitmin_BM$opt$lnL,  fitmin_OU$opt$lnL, fitmin_EB$opt$lnL,
                fitmin_delta$opt$lnL, fitmin_WN$opt$lnL),
              c("BM","OU","EB", "delta", "white"))

# Extracting AICc  values
aicc <-setNames(c(fitmin_BM$opt$aicc,  fitmin_OU$opt$aicc, fitmin_EB$opt$aicc,
                  fitmin_delta$opt$aicc, fitmin_WN$opt$aicc),
                c("BM","OU","EB", "delta", "white"))

# Merging everything together
EvoModel <- cbind(aic.vals, aicc, aic.w, lnL)
# Naming each row
rownames(EvoModel)<-c("BM", "OU", "EB", "Delta", "White Noise")
# Names each column
colnames(EvoModel)<-c("AIC", "AICc", "AICw", "Lik")
# Making what we just did into a data frame
EvoModel <- as.data.frame(EvoModel)

# Saving everything into a .csv
write.csv(EvoModel, file = "EvoModel_CTmin.csv")



##########################################################################
######                                                              ######
######         phyloModel for Tb                                    ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_Tb_Sept24.csv"                        ######
######                                                              ######
##########################################################################
# Load the data
Tb_data <- read.csv("New_PGLS_Tb_Sept24.csv", header=TRUE)
rownames(Tb_data) <- Tb_data$species

# Prune the tree to match the species in the data
anolespecies_Tb <- row.names(Tb_data)
pruned_anolis_Tb <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies_Tb))
ladderize(pruned_anolis_Tb)
plotTree(pruned_anolis_Tb)

# Order data to match the tree tip labels
Tb_data <- Tb_data[pruned_anolis_Tb$tip.label, ]
Tb <- setNames(Tb_data$breadth, rownames(Tb_data))

#Using the package geiger
fitTb_BM<-fitContinuous(pruned_anolis_Tb, Tb, model = c("BM"))
fitTb_BM
fitTb_OU<-fitContinuous(pruned_anolis_Tb, Tb, model = c("OU"))
fitTb_OU
fitTb_EB<-fitContinuous(pruned_anolis_Tb, Tb, model = c("EB"))
fitTb_EB
fitTb_delta<-fitContinuous(pruned_anolis_Tb, Tb, model = c("delta"))
fitTb_delta
fitTb_WN<-fitContinuous(pruned_anolis_Tb, Tb, model = c("white"))
fitTb_WN

## compare model fit using Akaike weights:
# Extracting AIC values
aic.vals<-setNames(c(fitTb_BM$opt$aic,  fitTb_OU$opt$aic, fitTb_EB$opt$aic,
                     fitTb_delta$opt$aic, fitTb_WN$opt$aic),
                   c("BM","OU","EB", "delta", "white"))

# Extracting AIC weight values
aic.w <- aic.w(aic.vals)

# Extracting liklog values
lnL<-setNames(c(fitTb_BM$opt$lnL,  fitTb_OU$opt$lnL, fitTb_EB$opt$lnL,
                fitTb_delta$opt$lnL, fitTb_WN$opt$lnL),
              c("BM","OU","EB", "delta", "white"))

# Extracting AICc  values
aicc <-setNames(c(fitTb_BM$opt$aicc,  fitTb_OU$opt$aicc, fitTb_EB$opt$aicc,
                  fitTb_delta$opt$aicc, fitTb_WN$opt$aicc),
                c("BM","OU","EB", "delta", "white"))

# Merging everything together
EvoModel <- cbind(aic.vals, aicc, aic.w, lnL)
# Naming each row
rownames(EvoModel)<-c("BM", "OU", "EB", "Delta", "White Noise")
# Names each column
colnames(EvoModel)<-c("AIC", "AICc", "AICw", "Lik")
# Making what we just did into a data frame
EvoModel <- as.data.frame(EvoModel)

# Saving everything into a .csv
write.csv(EvoModel, file = "EvoModel_Tb.csv")
