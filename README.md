The Andes are a driver of physiological diversity in Anolis lizards

Jhan C. Salazar, Gustavo A. Londoño, Martha M. Muñoz, Donald B. Miles, María del Rosario Castañeda

For our manuscript, we ran several phylogenetic analysis and make some cool figure. Here are the scripts and data we used. I prefer to separate each analysis and figure in their own .R file, so I have multiple .R for each of those.

We have to main datasets where we have the raw data, but for each analysis we had to separate each component, because CTmin, Tb, and CTmax have different number of species, so the easier things to do
was to separate each thermal trait (we used the means) in its own file to run the phylogenetic analysis

This is the tree we used for all the analysis:
  mcc_thinned_allruns.trees

To compare the evolutioanry rate of CTmin and CTmax we used the Adams' method
  Adams_Method: This is the code we use, and the following to .R are the function you need to run the analysis
    CompareRatesAmongTraits
    findCI

To estimate how our different variables (Elevation, Bio 1, Bio 5, Bio 6, and Te minimum, Te average and Te maximum) affect the evolution of CTmin, CTmax and Tb, we used ran a pgls.SEy. 
We also ran a phylogenetic signal analysis for the residuals of each regression, that part is included in that script.
  pgls_SEy_final

To do Figure 1 and Figure S4, we used these two scripts:
  Figure1_Boxplot
  Figure S4_Boxplot
