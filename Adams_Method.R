#rate comparison
require(ape)
library(phytools)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)

setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Anolis/CT/Adams method")

#load D. Adams' Function

source("CompareRatesAmongTraits.R")
source("findCI.R")

### ^ that's the name of the file containing the function, which I stored to the same directory in which 
### I was working. You can also just cut and paste the function into R. Works either way.



################################
## Analysis for lowland species
################################

# Load bio data for lowland species
ct <- read.csv("data_adams_method.csv", header=1)
ct
rownames(ct)<-ct$specie

## Load standard error for each species
thermalbio_SE <- read.csv("pgls_se.csv", header=1)
thermalbio_SE
rownames(thermalbio_SE)<-thermalbio_SE$specie


###load mcc tree and prune to species from Lowlands
ct_tree <- read.nexus("mcc_thinned_allruns.trees") 
ct_sp <- row.names(ct)
pruned_ct_tree<- drop.tip(ct_tree, setdiff(ct_tree$tip.label, ct_sp))
plotTree(pruned_ct_tree)


### Order data
ordered_data_ct <- ct[pruned_ct_tree$tip.label,] # this puts your data into the same order as the tips on the tree
ordered_data_ct ## note the column numbers associated with your trait data.


### compare rates - CTmin vs CTmax; uncorrected
CTmin_CTmax_uncorrected <- CompareRates.multTrait(pruned_ct_tree, ordered_data_ct[,4:5], TraitCov = T)
CTmin_CTmax_uncorrected

### compare rates - CTmin vs CTmax; Corrected
CTmin_CTmax_corrected <- CompareRates.multTrait(pruned_ct_tree, ordered_data_ct[,4:5], TraitCov = T, ms.err=thermalbio_SE[,c(2:3)])
CTmin_CTmax_corrected



### Confidence interval for CTmax
ctmax_r <-ordered_data_ct[,4]
names(ctmax_r)<-row.names(ordered_data_ct)
ctmax_r
findCI(pruned_ct_tree, ctmax_r)

### Confidence interval for CTmin
ctmin_r<-ordered_data_ct[,5]
names(ctmin_r)<-row.names(ordered_data_ct)
ctmin_r
findCI(pruned_ct_tree, ctmin_r)