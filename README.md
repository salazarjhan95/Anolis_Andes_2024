# The Andes are a driver of physiological diversity in Anolis lizards

Jhan C. Salazar, Gustavo A. Londoño, Martha M. Muñoz, Donald B. Miles, María del Rosario Castañeda

For our manuscript, we ran several phylogenetic analyses and created some cool figures. Here are the scripts and data we used. I prefer to separate each analysis and figure into its own .R file, so I have multiple .R files for each.

## Description
We have two main datasets containing the raw data. For each analysis, we had to separate the components because CTmin, Tb, and CTmax have different numbers of species. The easiest approach was to separate each thermal trait (using the means) into its own file to run the phylogenetic analysis.

## Phylogenetic tree
This is the tree we used for all the analysis:


  mcc_thinned_allruns.trees.

## Adams. method
To compare the evolutionary rate of CTmin and CTmax we used the Adams' method.
  
Adams_Method.R: This is the code we used, and the following two .R files are the functions you need to run the analysis.
    
  CompareRatesAmongTraits.R
  
  findCI.R

## pgls.SEy
To estimate how our variables (Elevation, Bio 1, Bio 5, Bio 6, and Te minimum, Te average, and Te maximum) affect the evolution of CTmin, CTmax, and Tb, we ran a pgls.SEy. We also performed a phylogenetic signal analysis for the residuals of each regression, which is included in the script. Additionally, we ran the same analysis for each clade to produce separate regression lines in both Figure 1 and Figure S4.
  
  pgls_SEy_final.R

## Evolutionary models for each physiological trait
To find the best evolutionary model that explain the evolution of thermalphysiology in Andean anoles we fitted five different models of evolution – Brownian motion (BM), Ornstein–Uhlenbeck (OU), early burst (EB), Delta, and White Noise (WN).

  phyloModel.R

## Figures
To do Figure 1, Figure S4, and the map in Figure S1 we used these two scripts:
  
  Figure1_Boxplot.R
  
  Figure S4_Boxplot.R
  
  topo_Map_for_EJLS.R
  
