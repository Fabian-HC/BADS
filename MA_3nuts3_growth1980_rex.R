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
# Design the six model fits and perform regressions on subset of years after 1990 for each Panel-Fit combination
# Concluding each fit-section, create a tidy selection of regression output
# Combine the intermediate per-fit results into one result table
# Reshape result to long form and save as LaTeX-ready .txt to directory

# === load data === #
df <- read.csv('MA_nuts3_data.csv')

# === FIT 1 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4

# PANEL A: trade center dummy
A_fit1 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
                       data = data)))
tidyA1 <- A_fit1 %>% summarise(tidy(mod))
glanA1 <- A_fit1 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit1 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
                       data = data)))
tidyB1 <- B_fit1 %>% summarise(tidy(mod))
glanB1 <- B_fit1 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit1 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
                       data = data)))
tidyC1 <- C_fit1 %>% summarise(tidy(mod))
glanC1 <- C_fit1 %>% summarise(glance(mod))

# === Fit (1) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy1 <- rbind(A=tidyA1, B=tidyB1, C=tidyC1)
glan1 <- rbind(A=glanA1, B=glanB1, C=glanC1)

# prepare intermediate per-fit result df
res1 <- tidy1 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res1$adj.r.squared <- glan1$adj.r.squared


# === FIT 2 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + lnarea + lnagri

# PANEL A: trade center dummy
A_fit2 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude +
                         capital + mountain + mining + lnarea + lnagri,
                       data = data)))
tidyA2 <- A_fit2 %>% summarise(tidy(mod))
glanA2 <- A_fit2 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit2 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_1 +
                         latitude + longitude + altitude +
                         capital + mountain + mining + lnarea + lnagri,
                       data = data)))
tidyB2 <- B_fit2 %>% summarise(tidy(mod))
glanB2 <- B_fit2 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit2 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_1 +
                         latitude + longitude + altitude +
                         capital + mountain + mining + lnarea + lnagri,
                       data = data)))
tidyC2 <- C_fit2 %>% summarise(tidy(mod))
glanC2 <- C_fit2 %>% summarise(glance(mod))

# === Fit (2) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy2 <- rbind(A=tidyA2, B=tidyB2, C=tidyC2)
glan2 <- rbind(A=glanA2, B=glanB2, C=glanC2)

# prepare intermediate per-fit result df
res2 <- tidy2 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res2$adj.r.squared <- glan2$adj.r.squared


# === FIT 3 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman

# PANEL A: trade center dummy
A_fit3 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyA3 <- A_fit3 %>% summarise(tidy(mod))
glanA3 <- A_fit3 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit3 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_1 +
                         latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyB3 <- B_fit3 %>% summarise(tidy(mod))
glanB3 <- B_fit3 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit3 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_1 +
                         latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyC3 <- C_fit3 %>% summarise(tidy(mod))
glanC3 <- C_fit3 %>% summarise(glance(mod))

# === Fit (3) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy3 <- rbind(A=tidyA3, B=tidyB3, C=tidyC3)
glan3 <- rbind(A=glanA3, B=glanB3, C=glanC3)

# prepare intermediate per-fit result df
res3 <- tidy3 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res3$adj.r.squared <- glan3$adj.r.squared


# === FIT 4 === #
# Controls:
# NUTS dummies: nuts_1
# Basic geographic controls: latitude + longitude + altitude
# Growth covariates: eqi100 + inequality2 + lnemp_comp

# PANEL A: trade center dummy
A_fit4 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyA4 <- A_fit4 %>% summarise(tidy(mod))
glanA4 <- A_fit4 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit4 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_1 +
                         latitude + longitude + altitude +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyB4 <- B_fit4 %>% summarise(tidy(mod))
glanB4 <- B_fit4 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit4 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_1 +
                         latitude + longitude + altitude +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyC4 <- C_fit4 %>% summarise(tidy(mod))
glanC4 <- C_fit4 %>% summarise(glance(mod))

# === Fit (4) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy4 <- rbind(A=tidyA4, B=tidyB4, C=tidyC4)
glan4 <- rbind(A=glanA4, B=glanB4, C=glanC4)

# prepare intermediate per-fit result df
res4 <- tidy4 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res4$adj.r.squared <- glan4$adj.r.squared


# === FIT 5 === #
# Controls:
# NUTS dummies: nuts_1
# Basic geographic controls: latitude + longitude + altitude
# Regional environment and location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4
# Region characteristics: capital + mountain + mining + lnarea + lnagri
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman
# Growth covariates: eqi100 + inequality2 + lnemp_comp

# PANEL A: trade center dummy
A_fit5 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyA5 <- A_fit5 %>% summarise(tidy(mod))
glanA5 <- A_fit5 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit5 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyB5 <- B_fit5 %>% summarise(tidy(mod))
glanB5 <- B_fit5 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit5 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_1 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman +
                         eqi100 + inequality2 + lnemp_comp,
                       data = data)))
tidyC5 <- C_fit5 %>% summarise(tidy(mod))
glanC5 <- C_fit5 %>% summarise(glance(mod))

# === Fit (5) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy5 <- rbind(A=tidyA5, B=tidyB5, C=tidyC5)
glan5 <- rbind(A=glanA5, B=glanB5, C=glanC5)

# prepare intermediate per-fit result df
res5 <- tidy5 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res5$adj.r.squared <- glan5$adj.r.squared


# === FIT 6 === #
# NUTS dummies: nuts_2
# Basic geographic controls: latitude + longitude + altitude
# Regional environment and location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4
# Region characteristics: capital + mountain + mining + lnarea + lnagri
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman

# PANEL A: trade center dummy
A_fit6 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_center +
                         nuts_2 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyA6 <- A_fit6 %>% summarise(tidy(mod))
glanA6 <- A_fit6 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit6 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ ln_tc_dist +
                         nuts_2 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyB6 <- B_fit6 %>% summarise(tidy(mod))
glanB6 <- B_fit6 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit6 <- df %>% filter(year>1980) %>% nest_by(year) %>%
  mutate(mod = list(lm(GDPpc_delta1980 ~ trade_years +
                         nuts_2 +
                         latitude + longitude + altitude +
                         lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
                         capital + mountain + mining + lnarea + lnagri +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence + imperialroad + lndist_roman,
                       data = data)))
tidyC6 <- C_fit6 %>% summarise(tidy(mod))
glanC6 <- C_fit6 %>% summarise(glance(mod))

# mining coefficient is not defined because of singularities in all Fit6 regressions, where the regional control is passed to nuts_2
# example check for Panel A
# alias(A_fit6)
# shows linear dependence of some nuts_2 region controls with mining
# further investigation shows that mining data is collected on NUTS2 level so the result is not surprising
# --> mining dropped from Fit6

# === Fit (6) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy6 <- rbind(A=tidyA6, B=tidyB6, C=tidyC6)
glan6 <- rbind(A=glanA6, B=glanB6, C=glanC6)

# prepare intermediate per-fit result df
res6 <- tidy6 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res6$adj.r.squared <- glan6$adj.r.squared


# === Combine intermediate results for all model fits (1-6) with for-loop into one wide-form result df === #
list.df <- list(res1, res2, res3, res4, res5, res6)
sfx <- c(".fit1", ".fit2", ".fit3", ".fit4", ".fit5", ".fit6")
res <- list.df[[1]]
for(i in head(seq_along(list.df), -1)) {
  
  res <- merge(res, list.df[[i+1]], all = TRUE, 
               suffixes = sfx[i:(i+1)], by = c('term', 'year'))
}
is.num <- sapply(res, is.numeric)
res[is.num] <- lapply(res[is.num], round, 3)

# wide form result df with numeric entries rounded to three decimals
res

res1985 <- res %>%
  filter(year==1985)

res1985_long <- reshape(res1985, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res1990 <- res %>%
  filter(year==1990)

res1990_long <- reshape(res1990, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res1995 <- res %>%
  filter(year==1995)

res1995_long <- reshape(res1995, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res2000 <- res %>%
  filter(year==2000)

res2000_long <- reshape(res2000, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res2005 <- res %>%
  filter(year==2005)

res2005_long <- reshape(res2005, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res2010 <- res %>%
  filter(year==2010)

res2010_long <- reshape(res2010, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res2015 <- res %>%
  filter(year==2015)

res2015_long <- reshape(res2015, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

res2020 <- res %>%
  filter(year==2020)

res2020_long <- reshape(res2020, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')

# === export and save LaTeX-ready .txt file of res_long table to directory === #
print.xtable(xtable(res_long),
             type = "latex", 
             file = "./res_Wahl_repl_rex.txt", 
             size = "tiny")