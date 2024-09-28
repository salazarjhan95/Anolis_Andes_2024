##########################################################################
######                                                              ######
######         PGLS.SEy_CTmax                                       ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######                                                              ######
##########################################################################

setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Anolis/CT/Last_analysis/NewTree")

library(phytools)
library(ape)
library(caper)

###Load tree
anolistree <- read.nexus("mcc_thinned_allruns.trees")
plot(anolistree)
is.rooted(anolistree)
  
## Yes, tree is rooted
anolistree$tip.label
  
anolisdata_ctmax <- read.csv("New_PGLS_CTmax_Sept24.csv", header=TRUE) ### this is your data file
head(anolisdata_ctmax)
attach(anolisdata_ctmax)
rownames(anolisdata_ctmax)<-anolisdata_ctmax$Sp
  
### PRUNE THE TREE to just include your species data
anolespecies<-row.names(anolisdata_ctmax)
pruned_anolis_ctmax <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies))
plotTree(pruned_anolis_ctmax)
  
### Order Data
ordered_anolis_data_ctmax<-anolisdata_ctmax[pruned_anolis_ctmax$tip.label,] # this puts your data into the same order as the tips on the tree
  
### Create vectors - basically make all your continuous variables into separate vectors.
names(ordered_anolis_data_ctmax)


sp<-ordered_anolis_data_ctmax[,1]
names(sp)<-row.names(ordered_anolis_data_ctmax)
    
ctmax<-ordered_anolis_data_ctmax[,2]
names(ctmax)<-row.names(ordered_anolis_data_ctmax) 
    
SE_ctmax<-ordered_anolis_data_ctmax[,3]
names(SE_ctmax)<-row.names(ordered_anolis_data_ctmax) 
    
mid.ele<-ordered_anolis_data_ctmax[,4]
names(mid.ele)<-row.names(ordered_anolis_data_ctmax)
    
bio1<-ordered_anolis_data_ctmax[,5]
names(bio1)<-row.names(ordered_anolis_data_ctmax) 
    
bio5<-ordered_anolis_data_ctmax[,6]
names(bio5)<-row.names(ordered_anolis_data_ctmax) 
    
bio6<-ordered_anolis_data_ctmax[,7]
names(bio6)<-row.names(ordered_anolis_data_ctmax)

Te_max<-ordered_anolis_data_ctmax[,8]
names(Te_max)<-row.names(ordered_anolis_data_ctmax) 

Te_min<-ordered_anolis_data_ctmax[,9]
names(Te_min)<-row.names(ordered_anolis_data_ctmax) 

Te_ave<-ordered_anolis_data_ctmax[,10]
names(Te_ave)<-row.names(ordered_anolis_data_ctmax)  

clade<-ordered_anolis_data_ctmax[,11]
names(clade)<-row.names(ordered_anolis_data_ctmax)  
  
## pgls considering the Standard Error for CTmax
# CTmax vs. Mean elevation 
fit_CTmax_mean_elev <- pgls.SEy(ctmax ~ mid.ele, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                                se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_mean_elev
summary(fit_CTmax_mean_elev) 
res_ctmax_elev <- residuals(fit_CTmax_mean_elev)
res_ctmax_elev
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_elev, method = "lambda")  # Pagel's λ
lambda_res # ##logarythm phyloSig = 0.108955  logL = 17.1256 

# CTmax vs. Bio1   
fit_CTmax_bio1 <-pgls.SEy(ctmax ~ bio1, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_bio1
summary(fit_CTmax_bio1) 
res_ctmax_bio1 <- residuals(fit_CTmax_bio1)
res_ctmax_bio1
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_bio1, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -19.0357

# CTmax vs. Bio5   
fit_CTmax_bio5 <-pgls.SEy(ctmax ~ bio5, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_bio5
summary(fit_CTmax_bio5) 
res_ctmax_bio5 <- residuals(fit_CTmax_bio5)
res_ctmax_bio5
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_bio5, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -19.4478

# CTmax vs. Bio5    
fit_CTmax_bio6 <-pgls.SEy(ctmax ~ bio6, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_bio6
summary(fit_CTmax_bio6) 
res_ctmax_bio6 <- residuals(fit_CTmax_bio6)
res_ctmax_bio6
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_bio6, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -18.7865


## Operative temperature
# CTmax vs. Te_max   
fit_CTmax_Te_max <-pgls.SEy(ctmax ~ Te_max, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_Te_max
summary(fit_CTmax_Te_max) 
res_ctmax_Te_max <- residuals(fit_CTmax_Te_max)
res_ctmax_Te_max
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_Te_max, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -20.5628 

# CTmax vs. Te_min   
fit_CTmax_Te_min <-pgls.SEy(ctmax ~ Te_min, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_Te_min
summary(fit_CTmax_Te_min) 
res_ctmax_Te_min <- residuals(fit_CTmax_Te_min)
res_ctmax_Te_min
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_Te_min, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -17.3808

# CTmax vs. Te_ave   
fit_CTmax_Te_ave <-pgls.SEy(ctmax ~ Te_ave, data = ordered_anolis_data_ctmax, corClass=corBrownian, 
                          se=SE_ctmax, tree = pruned_anolis_ctmax, method="ML")
fit_CTmax_Te_ave
summary(fit_CTmax_Te_ave) 
res_ctmax_Te_av <- residuals(fit_CTmax_Te_ave)
res_ctmax_Te_av
lambda_res <- phylosig(pruned_anolis_ctmax, res_ctmax_Te_av, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05  logL = -18.1568



##########################################################################
######                                                              ######
######         PGLS.SEy_CTmin                                       ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######                                                              ######
##########################################################################

anolisdata_ctmin <- read.csv("New_PGLS_CTmin_Sept24.csv", header=TRUE) ### this is your data file
head(anolisdata_ctmin)
attach(anolisdata_ctmin)
rownames(anolisdata_ctmin)<-anolisdata_ctmin$Sp

### PRUNE THE TREE to just include your species data
anolespecies<-row.names(anolisdata_ctmin)
pruned_anolis_ctmin <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies))
plotTree(pruned_anolis_ctmin)

### Order Data
ordered_anolis_data_ctmin<-anolisdata_ctmin[pruned_anolis_ctmin$tip.label,] # this puts your data into the same order as the tips on the tree

### Create vectors - basically make all your continuous variables into separate vectors.
names(ordered_anolis_data_ctmin)


sp<-ordered_anolis_data_ctmin[,1]
names(sp)<-row.names(ordered_anolis_data_ctmin)

ctmin<-ordered_anolis_data_ctmin[,2]
names(ctmin)<-row.names(ordered_anolis_data_ctmin) 

SE_ctmin<-ordered_anolis_data_ctmin[,3]
names(SE_ctmin)<-row.names(ordered_anolis_data_ctmin) 

mid.ele<-ordered_anolis_data_ctmin[,4]
names(mid.ele)<-row.names(ordered_anolis_data_ctmin)

bio1<-ordered_anolis_data_ctmin[,5]
names(bio1)<-row.names(ordered_anolis_data_ctmin) 

bio5<-ordered_anolis_data_ctmin[,6]
names(bio5)<-row.names(ordered_anolis_data_ctmin) 

bio6<-ordered_anolis_data_ctmin[,7]
names(bio6)<-row.names(ordered_anolis_data_ctmin)

Te_max<-ordered_anolis_data_ctmin[,8]
names(Te_max)<-row.names(ordered_anolis_data_ctmin) 

Te_min<-ordered_anolis_data_ctmin[,9]
names(Te_min)<-row.names(ordered_anolis_data_ctmin) 

Te_ave<-ordered_anolis_data_ctmin[,10]
names(Te_ave)<-row.names(ordered_anolis_data_ctmin)  

clade<-ordered_anolis_data_ctmin[,11]
names(clade)<-row.names(ordered_anolis_data_ctmin) 

## pgls considering the Standard Error for CTmax
# CTmin vs. Mean elevation 
fit_CTmin_mean_elev <- pgls.SEy(ctmin ~ mid.ele, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                                se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_mean_elev
summary(fit_CTmin_mean_elev) 
res_ctmin_elev <- residuals(fit_CTmin_mean_elev)
res_ctmin_elev 
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_elev, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = 2.65652 

# CTmin vs. Bio1   
fit_CTmin_bio1 <-pgls.SEy(ctmin ~ bio1, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                          se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_bio1
summary(fit_CTmin_bio1) 
res_ctmin_bio1 <- residuals(fit_CTmax_bio1)
res_ctmin_bio1
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_bio1, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -17.5787

# CTmin vs. Bio5   
fit_CTmin_bio5 <-pgls.SEy(ctmin ~ bio5, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                          se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_bio5
summary(fit_CTmin_bio5) 
res_ctmin_bio5 <- residuals(fit_CTmin_bio5)
res_ctmin_bio5 
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_bio5, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -23.9771 

# CTmin vs. Bio5    
fit_CTmin_bio6 <-pgls.SEy(ctmin ~ bio6, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                          se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_bio6
summary(fit_CTmin_bio6) 
res_ctmin_bio6 <- residuals(fit_CTmin_bio6)
res_ctmin_bio6
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_bio6, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -23.3845


## Operative temperature
# CTmin vs. Te_max   
fit_CTmin_Te_max <-pgls.SEy(ctmin ~ Te_max, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                            se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_Te_max
summary(fit_CTmin_Te_max) 
res_ctmin_Te_max <- residuals(fit_CTmin_Te_max)
res_ctmin_Te_max
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_Te_max, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -23.3146

# CTmin vs. Te_min   
fit_CTmin_Te_min <-pgls.SEy(ctmin ~ Te_min, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                            se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_Te_min
summary(fit_CTmin_Te_min) 
res_ctmin_Te_min <- residuals(fit_CTmin_Te_min)
res_ctmin_Te_min
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_Te_min, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -23.2387 

# CTmin vs. Te_ave   
fit_CTmin_Te_ave <-pgls.SEy(ctmin ~ Te_ave, data = ordered_anolis_data_ctmin, corClass=corBrownian, 
                            se=SE_ctmin, tree = pruned_anolis_ctmin, method="ML")
fit_CTmin_Te_ave
summary(fit_CTmin_Te_ave) 
res_ctmin_Te_ave <- residuals(fit_CTmin_Te_ave)
res_ctmin_Te_ave 
lambda_res <- phylosig(pruned_anolis_ctmin, res_ctmin_Te_ave, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -22.8814 




##########################################################################
######                                                              ######
######         PGLS_Tb                                              ######
######         uses "mcc_thinned_allruns.trees"                     ######
######         uses "New_PGLS_Tb_Sept24.csv"                        ######
######                                                              ######
##########################################################################

anolisdata_tb <- read.csv("New_PGLS_Tb_Sept24.csv", header=TRUE) ### this is your data file
head(anolisdata_tb)
attach(anolisdata_tb)
rownames(anolisdata_tb)<-anolisdata_tb$species

### PRUNE THE TREE to just include your species data
anolespecies<-row.names(anolisdata_tb)
pruned_anolis_tb <- drop.tip(anolistree, setdiff(anolistree$tip.label, anolespecies))
plotTree(pruned_anolis_tb)

### Order Data
ordered_anolis_data_tb<-anolisdata_tb[pruned_anolis_tb$tip.label,] # this puts your data into the same order as the tips on the tree

### Create vectors - basically make all your continuous variables into separate vectors.
names(ordered_anolis_data_tb)

## Since I only have one measure of Tb per species, I ran a standard pgls
comp.data<-comparative.data(pruned_anolis_tb, ordered_anolis_data_tb, names.col="species", 
                            vcv.dim=2, warn.dropped=TRUE)


### Bios and elevation
# Tb vs. Elevation 
modelo1 <- pgls(breadth ~ mid.ele, data=comp.data, lambda="ML")
summary(modelo1)
res_modelo1 <- residuals(modelo1)   
lambda_res <- phylosig(pruned_anolis_tb, res_modelo1, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = 2.8854 
# phyloSig = 5.98695e-05 logL = 2.8854 

# Tb vs. Bio1 
modelo2 <- pgls(breadth ~ bio1, data=comp.data, lambda="ML")
summary(modelo2)
res_modelo2 <- residuals(modelo2)    
lambda_res <- phylosig(pruned_anolis_tb, res_modelo2, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.3165 

# Tb vs. Bio5
modelo3 <- pgls(breadth ~ bio5, data=comp.data, lambda="ML")
summary(modelo3)
res_modelo3 <- residuals(modelo3)     
lambda_res <- phylosig(pruned_anolis_tb, res_modelo3, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.3264 

# Tb vs. Bio6
modelo4 <- pgls(breadth ~ bio6, data=comp.data, lambda="ML")
summary(modelo4)
res_modelo4 <- residuals(modelo4)     
lambda_res <- phylosig(pruned_anolis_tb, res_modelo4, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.3044  


## Operative temperature
# Tb vs. Te_min
modelo6 <- pgls(breadth ~ te_min, data=comp.data, lambda="ML")
summary(modelo6)
res_modelo6 <- residuals(modelo6)      
lambda_res <- phylosig(pruned_anolis_tb, res_modelo6, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.3343

# Tb vs. Te_ave
modelo7 <- pgls(breadth ~ te_ave, data=comp.data, lambda="ML")
summary(modelo7)
res_modelo7 <- residuals(modelo7)      
lambda_res <- phylosig(pruned_anolis_tb, res_modelo7, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.2699 

# Tb vs. Te_max
modelo8 <- pgls(breadth ~ te_max, data=comp.data, lambda="ML")
summary(modelo8)
res_modelo8 <- residuals(modelo8)      
lambda_res <- phylosig(pruned_anolis_tb, res_modelo8, method = "lambda")  # Pagel's λ
lambda_res # phyloSig = 5.98695e-05 logL = -25.1907 
