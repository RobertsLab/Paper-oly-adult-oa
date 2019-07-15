
rm(list=ls())         #start script by deleting all objects - clean slate 

# Before running scripts, load all needed packages 
list.of.packages <- c("ggplot2", "car", "mvoutlier", "dplyr", "reshape2", "stringr", "plotly", "ggpubr", "ggridges", "HH", "pwr", "reshape2", "rcompanion", "lme4", "plotrix", "grid", "gridExtra", "scales", "MASS", "plyr", "PBSmapping", "betareg", "gplots", "corrplot", "cowplot", "tidyr", "colorspace", "tidyverse", "FSA", "ggpmisc") #add new libraries here 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load all libraries 
lapply(list.of.packages, FUN = function(X) {
  do.call("require", list(X)) 
})
sessionInfo()

source("Analyses/panelcor.R")
