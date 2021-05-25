# === clear the screen === #
cat("\014")

# === clear the environment === #
rm(list = ls())

# === reset graphics === #
graphics.off()

# === set working directory === #
setwd("~/MA")

# === update current packages === #
#update.packages()

# === install and load packages via pacman === #
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plyr, dplyr)

# === load data === #
df <- read.csv('MA_nuts2_data.csv')

# https://stackoverflow.com/questions/22337394/dplyr-mutate-with-conditional-values

dt1 <- df %>%
  filter(Year == 1925 | Year == 1950) %>%
  mutate(time = ifelse(Year > 1925, 1, 0)) %>%
  mutate(treat = mining*trade_center) %>%
  mutate(did = time*treat)

didreg1 = lm(lnGDPpc ~ 
               treat + 
               time + 
               did, data = dt1)
summary(didreg1)

check1 <- df %>%
  filter(Year == 1900 | Year == 1925) %>%
  mutate(time = ifelse(Year > 1900, 1, 0)) %>%
  mutate(treat = mining*trade_center) %>%
  mutate(did = time*treat)

didregcheck1 = lm(lnGDPpc ~ 
                    treat + 
                    time + 
                    did, data = check1)
summary(didregcheck1)


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