library(phytools)
library(mapdata)

setwd("")

# Read tree
tree <- read.nexus("mcc_thinned_allruns.trees")

# Load a file with cordinates
geo_data <- read.csv("")

# Load a file with the species names
species_data <- read.csv("Species_forMap.csv")
rownames(species_data) <- species_data$Species

# Match tree with data
TreeOnly <- setdiff(tree$tip.label, rownames(species_data))
TreeOnly # Enter the name of the object we just created to see what's in it.
DataOnly <- setdiff(rownames(species_data), tree$tip.label)
DataOnly # Enter to see what species are in the data set but not the tree

# Tree pruned
pruned_anolis <- drop.tip(tree, tip = TreeOnly)
plotTree(pruned_anolis)

## Because we have repeating row names, we need to provide the data as a matrix instead of a data frame
geo_data2<-cbind(latitude=geo_data$Latitude,
                 longitude=geo_data$Longitude)

rownames(geo_data2)<-geo_data$Species


geo_data2[,2]<--abs(geo_data2[,2])

ColMap <- phylo.to.map(pruned_anolis, geo_data2, database = "worldHires",
                       regions = "Colombia", plot = FALSE, direction="rightwards")


## The dimensions of the plot are based on the latitudinal and longitudinal
## limits of the map. If we don't change this the limts are going to default
## to the rage of the data 

# Increase resolution with width, height, and res
png("topoMap_2017_for_EJLS.png", width = 2000, height = 1500, res = 600)  


plot(ColMap, direction="rightwards", xlim=c(-82,-66.7), ylim=c(-4.3, 13.5),
     ftype="i", cex.points=c(0, 1.5), colors = "#8b8c89", map.bg = "#a3e0a3",
     fsize = 0.4) 

dev.off()
