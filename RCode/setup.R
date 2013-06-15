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
require(countrycode)
setwd(pathFunctions)
require(xlsx)
require(gdata)
require(reshape)
require(plyr)
require(WDI)

require(lme4)

# Setting seed
set.seed(6886)
setwd(pathMain)