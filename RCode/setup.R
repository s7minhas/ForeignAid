# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829"
){pathMain="~/Dropbox/Research/ButheProjects/ForeignAid";
  pathGraphics="~/Dropbox/Research/ButheProjects/ForeignAid/Graphics";
  pathData="~/Dropbox/Research/ButheProjects/ForeignAid/Data";
  pathCode="~/Desktop/Research/ButheProjects/ForeignAid/RCode";
  pathFunctions="~/Desktop/Prog Notes/R Functions"}

# Loading libraries and functions
library(foreign)
library(WDI)
library(cshapes)
library(plm)
library(MASS)
library(mvtnorm)
library(reshape)
library(gtools)
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(gridExtra)
library(plyr)
library(tikzDevice)
setwd(pathFunctions)
source("rwish.R")
source("theme_border.R")
library(lme4)
library(arm)
# source("hdr_2d.R")
# source("scatterHist.R")

# Setting seed
set.seed(6886)
setwd(pathMain)