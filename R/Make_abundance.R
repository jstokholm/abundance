library(devtools)
library(roxygen2)

### Create, only first time
#create("abundance")

### Update the DOCUMENTATION file
# If you need external packages put them like this (without the #):
# Imports: ggplot2, lme4, foreach, phyloseq

# If the external packages are from BioConductor add this:
# Remotes: bioc::release/phyloseq

### Add your R functions as R scripts in the R/ directory

### Never edit NAMESPACE file
### Never edit or add anything to the man/ directory

setwd("~/Dropbox/Documents/ABC Jakob/postdoc/R/git/")
### Update man pages and namespace
# This should be run everytime you update your scripts or the DOCUMENTATION
document("abundance")

### Install locally
install("abundance")

### Test it out
library(abundance)
?abundance

load("/Volumes//UserFolders/Jakob/Projects/AB_bak_asthma/xf_2k_1w_6y_dada2_meta.Rdata")

rabuplot(phylo_ob=xf_2k_1w_6y_dada2,predictor="j45_5yr_cross",type="Genus",order=TRUE, no_other_type=FALSE, legend_title="Asthma at \n5 years",legend_names=c("No","Yes"), reverse=FALSE,N_taxa=30, Timepoint="1y", Strata="asthma_mother",Strata_val="1",By_median=TRUE,Only_sig=FALSE,stats="parametric")


library(phyloseq)
library(dplyr)
library(RColorBrewer)
library(tidyr)

library(ggplot2)
######### First you have to initialize your git repository.
# We only initialize the very first time
# We change directory into your code repository (R package):
cd /some/folder/mypackage
cd "/Users/jakobstokholm/Dropbox/Documents/ABC Jakob/postdoc/R/git/abundance/"
# We initialize a git repo:
git init

######### We then add our files to the staging area, which means that git will keep track of them
# The . means all files:
git add .
git commit -m "Updated XX"
git push -u origin master

# You can also only add specific files:
git add DOCUMENTATION

######### We then have to commit our changes.
# Only when a change is commited it will be saved!
git commit -m "here you write what you have changed in your code"
git push -u origin master
######### Check the status
# All the way through this workflow you can check the status
# git will tell which changes are untracked, which are ready to commited, or whether everything is up to date. Try to run this command at different stages in the workflow to check the output.
git status

######### Summary of local git
# You always have to change directory to where your code repository is located
# You only run git init first time
# Every time you make a change you want to save in git history, you run git add [. or filename] and git commit -m "message"

# If git complains when you run git commit, maybe you have to run git config first.
# Adding --global will apply this to your entire computer
# Removing --global will make it specific for your repository
git config --global user.name "FIRST_NAME LAST_NAME"
git config --global user.email "MY_NAME@example.com"

######### Github
# Go to github and create a new repository
# Do not add README, LICENSE, or .gitignore
# Push the local repository to github (first time):
git remote add origin remote https://github.com/USERNAME/REPO_NAME.git
git push -u origin master

# Push changes in the local repository to github (after first time):
git push

# If you change something on github, for example add a README, you should pull it to your local repository
# If you also made local changes, often you have to pull before you can push.
git pull

