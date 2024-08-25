install.packages("MendelianRandomization")
install.packages("ivreg")
if (!requireNamespace("qqman", quietly = TRUE)) {
  install.packages("qqman")
}

library("MendelianRandomization")
library("ivreg")
library(qqman)

gwas_resultsDrinks <- read.table("/Users/chenmaige/Desktop/plink_mac_20231211/DrinksRegression.assoc.logistic", header = TRUE)
gwas_resultssmk <- read.table("/Users/chenmaige/Desktop/plink_mac_20231211/SmokesRegression.assoc.logistic", header = TRUE)


manhattan(gwas_resultsDrinks, col = c("blue4", "orange3"), main = "Manhattan Plot for Drink GWAS", ylim = c(0, 10), 
          suggestiveline = FALSE, genomewideline = -log10(5e-04))
manhattan(gwas_resultssmk, col = c("blue4", "orange3"), main = "Manhattan Plot for Smoke GWAS", ylim = c(0, 10),
          suggestiveline = FALSE, genomewideline = -log10(5e-04))


index_dr <-  which(gwas_resultsDrinks$P<0.0005)
index_smk <- which(gwas_resultssmk$P<0.0005)

object1 <- mr_input(
  bx = gwas_resultsDrinks[index_dr,7] ,
  bxse = gwas_resultsDrinks[index_dr,8] ,
  by = gwas_resultssmk[index_dr,7] ,
  byse = gwas_resultssmk[index_dr,8] 
)
object1

IVWObject1 <- mr_ivw( mr_input(
  bx = gwas_resultsDrinks[index_dr,7] ,
  bxse = gwas_resultsDrinks[index_dr,8] ,
  by = gwas_resultssmk[index_dr,7] ,
  byse = gwas_resultssmk[index_dr,8] 
))
IVWObject1

EggerObject1 <- mr_egger(mr_input(
  bx = gwas_resultsDrinks[index_dr,7] ,
  bxse = gwas_resultsDrinks[index_dr,8] ,
  by = gwas_resultssmk[index_dr,7] ,
  byse = gwas_resultssmk[index_dr,8] 
))
EggerObject1

MBEObject1 <- mr_mbe(mr_input(
  bx = gwas_resultsDrinks[index_dr,7] ,
  bxse = gwas_resultsDrinks[index_dr,8] ,
  by = gwas_resultssmk[index_dr,7] ,
  byse = gwas_resultssmk[index_dr,8] 
))
MBEObject1

WeightedMedianObject1 <- mr_median(mr_input(
  bx = gwas_resultsDrinks[index_dr,7] ,
  bxse = gwas_resultsDrinks[index_dr,8] ,
  by = gwas_resultssmk[index_dr,7] ,
  byse = gwas_resultssmk[index_dr,8] 
))
WeightedMedianObject1


mr_forest(object1,
          methods = c("ivw", "median", "wmedian", "egger", "maxlik", "mbe", "conmix"),
          snp_estimates = FALSE)

mr_forest(object1, ordered=TRUE)



mr_plot(object1,
        error = TRUE, orientate = FALSE, line = "egger", interactive = FALSE, labels = TRUE)



