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
pacman::p_load(plyr, dplyr, broom, tidyr, xtable, gtools, stringr)

# === update current packages === #
#update.packages()

# === AGENDA === #
# After loading long form df covering GDP per capita data for the period 1980-2021
# Design DID for the subset of regions post Fall of the iron curtain.

# === load data === #
df <- read.csv('MA_nuts3_data.csv')
  
dt1 <- df %>%
  filter(year == 1990 | year == 2020,
         east_germany == 1 | country %in% c('Poland', 'Hungary', 'Lithuania')) %>%
  mutate(mining_trade = mining*trade_center,
         time = ifelse(year > 1990, 1, 0),
         treat = mining_trade,
         did = time*treat)

didreg1 = lm(lnGDPpc ~ 
               treat + 
               time + 
               did, data = dt1)
summary(didreg1)


dt2 <- df %>%
  filter(Year == 1980 | Year == 2010) %>%
  mutate(time = ifelse(Year > 1980, 1, 0)) %>%
  mutate(treat = mining*trade_center) %>%
  mutate(did = time*treat)

didreg2 = lm(lnGDPpc ~ 
               treat + 
               time + 
               did, data = dt2)
summary(didreg2)

check2 <- df %>%
  filter(Year == 1950 | Year == 1980) %>%
  mutate(time = ifelse(Year > 1950, 1, 0)) %>%
  mutate(treat = mining*trade_center) %>%
  mutate(did = time*treat)

didregcheck2 = lm(lnGDPpc ~ 
                    treat + 
                    time + 
                    did, data = check2)
summary(didregcheck2)

# === Fit tidy selection of regression output === #
# list of Panels
DID <- list(reg1=didreg1, che1=didregcheck1, reg2=didreg2, che2=didregcheck2)
# tidy selection
res <- ldply(DID, tidy, .id = "run") %>%
  mutate(signif = stars.pval(p.value))

is.num <- sapply(res, is.numeric)
res[is.num] <- lapply(res[is.num], round, 3)