library(SLIDEHelper)
library(tidyverse)
library(doParallel)
library(ggplot2)

cores <-  as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK', unset=NA))
if(is.na(cores)) cores <- detectCores()
registerDoParallel(cores)
cat('number of cores using', cores, '. . .\n')



er_path <- "final_delta_0.001_lambda_0.5.rds"
x_path <- "x.csv"
out_path <- "output/"
Z_matrix <- CalcZMatrix(x_path, er_path, out_path)



y_path <- "y.csv"
SLIDE_res <- runSLIDE(y_path = y_path,
                      z_path = NULL,
                      z_matrix = Z_matrix,
                      er_path = er_path,
                      do_interacts = TRUE,
                      spec = 0.2,
                      niter = 100)

num_top_feats <- 10
condition <- "auc"
SLIDE_res <- GetTopFeatures(x_path, y_path, er_path, out_path, SLIDE_res, num_top_feats = 10, condition)

plotSigGenes(SLIDE_res, out_path, plot_interaction = TRUE)

#the SLIDE_res has to be the output from GetTopFeatures
CalcControlPerformance(z_matrix = Z_matrix, y_path, SLIDE_res, niter = 1000, condition, out_path)

