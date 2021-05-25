# === clear the screen === #
cat("\014")

# === clear the environment === #
rm(list = ls())

# === reset graphics === #
graphics.off()

# === set working directory === #
setwd("~/MA")

# === unload all packages except for base === #
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# === install and load required packages via pacman === #
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, dplyr, broom, tidyr, plm, xtable, gtools, stringr, stats)

# === update current packages === #
#update.packages()

# === AGENDA === #
# After loading long form df covering GDP per capita data for the period 1980-2021
# Design the six model fits and perform regressions for each Panel-Fit combination
# Concluding each fit-section, create a tidy selection of regression output
# Combine the intermediate per-fit results into one result table
# Reshape result to long form and save as LaTeX-ready .txt to directory

# === load data === #
df <- read.csv('MA_nuts3_data.csv')

pdata <- pdata.frame(df, index = c("nuts_3", "year"))

# === FIT 1 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4

# PANEL A: trade center dummy
A_fit1 <- plm(lnGDPpc ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude + trade_center*year1980 + trade_center*year1985 + trade_center*year1990 + trade_center*year1995 + trade_center*year2000 + trade_center*year2005 + trade_center*year2010 + trade_center*year2015,
              data = pdata, model = 'within')
summary(A_fit1)
