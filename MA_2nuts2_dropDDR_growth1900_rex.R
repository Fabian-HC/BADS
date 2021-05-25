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
pacman::p_load(dplyr, broom, tidyr, xtable, gtools, stringr)

# === load data === #
df <- read.csv('MA_nuts2_data.csv')

# === PRE-SCRIPT === #
# an earlier version of this script displayed regression output using a "group_by() %>% do()" chain:
# https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr?noredirect=1&lq=1
# This grammar is superseded replaced by updated grammar using nest_by() %>% mutate(list()):
# https://dplyr.tidyverse.org/reference/do.html
# Both approaches eliminate repetition over observation periods as compared to base R [c.p. replication script], like:
# A_fit1_1900 <- lm(GDPpc_delta1900 ~ trade_center + ...,
#                   data = subset(df, Year==1900))
# summary(A_fit1_1900)
# ...per Panel, Fit, and Year

# === FIT 1 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: dist_border_ln + dist_coast_ln

# PANEL A: trade center dummy
A_fit1 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyA1 <- A_fit1 %>% summarise(tidy(mod))
glanA1 <- A_fit1 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit1 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyB1 <- B_fit1 %>% summarise(tidy(mod))
glanB1 <- B_fit1 %>% summarise(glance(mod))

# PANEL C: centuries of trade 
C_fit1 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyC1 <- C_fit1 %>% summarise(tidy(mod))
glanC1 <- C_fit1 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit1 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyD1 <- D_fit1 %>% summarise(tidy(mod))
glanD1 <- D_fit1 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit1 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyE1 <- E_fit1 %>% summarise(tidy(mod))
glanE1 <- E_fit1 %>% summarise(glance(mod))

# === Fit (1) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy1 <- rbind(A=tidyA1, B=tidyB1, C=tidyC1, D=tidyD1, E=tidyE1)
glan1 <- rbind(A=glanA1, B=glanB1, C=glanC1, D=glanD1, E=glanE1)

# prepare intermediate per-fit result df
res1 <- tidy1 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res1$adj.r.squared <- glan1$adj.r.squared

# === FIT 2 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + AREA_ln + agri_ln

# PANEL A: trade center dummy
A_fit2 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln,
                       data = data)))
tidyA2 <- A_fit2 %>% summarise(tidy(mod))
glanA2 <- A_fit2 %>% summarise(glance(mod))

# PANEL B: distance to trade
B_fit2 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln,
                       data = data)))
tidyB2 <- B_fit2 %>% summarise(tidy(mod))
glanB2 <- B_fit2 %>% summarise(glance(mod))

# PANEL C: centuries of trade 
C_fit2 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln,
                       data = data)))
tidyC2 <- C_fit2 %>% summarise(tidy(mod))
glanC2 <- C_fit2 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit2 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln,
                       data = data)))
tidyD2 <- D_fit2 %>% summarise(tidy(mod))
glanD2 <- D_fit2 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit2 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyE2 <- E_fit2 %>% summarise(tidy(mod))
glanE2 <- E_fit2 %>% summarise(glance(mod))

# === Fit (2) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy2 <- rbind(A=tidyA2, B=tidyB2, C=tidyC2, D=tidyD2, E=tidyE2)
glan2 <- rbind(A=glanA2, B=glanB2, C=glanC2, D=glanD2, E=glanE2)

# prepare intermediate per-fit result df
res2 <- tidy2 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res2$adj.r.squared <- glan2$adj.r.squared

# === FIT 3 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy
A_fit3 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         country + latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyA3 <- A_fit3 %>% summarise(tidy(mod))
glanA3 <- A_fit3 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit3 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         country + latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyB3 <- B_fit3 %>% summarise(tidy(mod))
glanB3 <- B_fit3 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit3 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         country + latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyC3 <- C_fit3 %>% summarise(tidy(mod))
glanC3 <- C_fit3 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit3 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country + latitude + longitude + altitude +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyD3 <- D_fit3 %>% summarise(tidy(mod))
glanD3 <- D_fit3 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit3 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyE3 <- E_fit3 %>% summarise(tidy(mod))
glanE3 <- E_fit3 %>% summarise(glance(mod))

# === Fit (3) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy3 <- rbind(A=tidyA3, B=tidyB3, C=tidyC3, D=tidyD3, E=tidyE3)
glan3 <- rbind(A=glanA3, B=glanB3, C=glanC3, D=glanD3, E=glanE3)

# prepare intermediate per-fit result df
res3 <- tidy3 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res3$adj.r.squared <- glan3$adj.r.squared

# === FIT 4 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy
A_fit4 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyA4 <- A_fit4 %>% summarise(tidy(mod))
glanA4 <- A_fit4 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit4 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyB4 <- B_fit4 %>% summarise(tidy(mod))
glanB4 <- B_fit4 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit4 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         country + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyC4 <- C_fit4 %>% summarise(tidy(mod))
glanC4 <- C_fit4 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit4 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyD4 <- D_fit4 %>% summarise(tidy(mod))
glanD4 <- D_fit4 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit4 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyE4 <- E_fit4 %>% summarise(tidy(mod))
glanE4 <- E_fit4 %>% summarise(glance(mod))

# === Fit (4) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy4 <- rbind(A=tidyA4, B=tidyB4, C=tidyC4, D=tidyD4, E=tidyE4)
glan4 <- rbind(A=glanA4, B=glanB4, C=glanC4, D=glanD4, E=glanE4)

# prepare intermediate per-fit result df
res4 <- tidy4 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res4$adj.r.squared <- glan4$adj.r.squared


# === FIT 5 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy
A_fit5 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         nuts_1 + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyA5 <- A_fit5 %>% summarise(tidy(mod))
glanA5 <- A_fit5 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit5 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         nuts_1 + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyB5 <- B_fit5 %>% summarise(tidy(mod))
glanB5 <- B_fit5 %>% summarise(glance(mod))

# PANEL C: centuries of trade
C_fit5 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         nuts_1 + latitude + longitude + altitude +
                         capital + mountain + mining + AREA_ln + agri_ln +
                         university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                       data = data)))
tidyC5 <- C_fit5 %>% summarise(tidy(mod))
glanC5 <- C_fit5 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit5 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyD5 <- D_fit5 %>% summarise(tidy(mod))
glanD5 <- D_fit5 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit5 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country + latitude + longitude + altitude +
                         dist_border_ln + dist_coast_ln,
                       data = data)))
tidyE5 <- E_fit5 %>% summarise(tidy(mod))
glanE5 <- E_fit5 %>% summarise(glance(mod))

# === Fit (5) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy5 <- rbind(A=tidyA5, B=tidyB5, C=tidyC5, D=tidyD5, E=tidyE5)
glan5 <- rbind(A=glanA5, B=glanB5, C=glanC5, D=glanD5, E=glanE5)

# prepare intermediate per-fit result df
res5 <- tidy5 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res5$adj.r.squared <- glan5$adj.r.squared

# === FIT 6 === #
# Reduced Fit
# Control: country

# PANEL A: trade center dummy
A_fit6 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_center +
                         country,
                       data = data)))
tidyA6 <- A_fit6 %>% summarise(tidy(mod))
glanA6 <- A_fit6 %>% summarise(glance(mod))

# PANEL B: distance to trade 
B_fit6 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ ln_tc_dist +
                         country,
                       data = data)))
tidyB6 <- B_fit6 %>% summarise(tidy(mod))
glanB6 <- B_fit6 %>% summarise(glance(mod))

# PANEL C: centuries of trade 
C_fit6 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ trade_years +
                         country,
                       data = data)))
tidyC6 <- C_fit6 %>% summarise(tidy(mod))
glanC6 <- C_fit6 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit6 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_ratio +
                         country,
                       data = data)))
tidyD6 <- D_fit6 %>% summarise(tidy(mod))
glanD6 <- D_fit6 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit6 <- df %>% filter(east_germany!=1) %>% nest_by(Year) %>%
  mutate(mod = list(lm(GDPpc_delta1900 ~ tc_dens +
                         country,
                       data = data)))
tidyE6 <- E_fit6 %>% summarise(tidy(mod))
glanE6 <- E_fit6 %>% summarise(glance(mod))

# === Fit (6) tidy selection of regression output === #
# row-wise stack panels of broom tables required
tidy6 <- rbind(A=tidyA6, B=tidyB6, C=tidyC6, D=tidyD6, E=tidyE6)
glan6 <- rbind(A=glanA6, B=glanB6, C=glanC6, D=glanD6, E=glanE6)

# prepare intermediate per-fit result df
res6 <- tidy6 %>%
  filter(term %in% c('trade_center', 'ln_tc_dist', 'trade_years', 'tc_ratio', 'tc_dens'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res6$adj.r.squared <- glan6$adj.r.squared


# === Combine intermediate results for all model fits (1-6) with for-loop into one wide-form result df === #
list.df <- list(res1, res2, res3, res4, res5, res6)
sfx <- c(".fit1", ".fit2", ".fit3", ".fit4", ".fit5", ".fit6")
res <- list.df[[1]]
for(i in head(seq_along(list.df), -1)) {
  
  res <- merge(res, list.df[[i+1]], all = TRUE, 
               suffixes = sfx[i:(i+1)], by = c('term', 'Year'))
}
is.num <- sapply(res, is.numeric)
res[is.num] <- lapply(res[is.num], round, 3)

# wide form result df with numeric entries rounded to three decimals
res

# === reshape selection of Year subsets to long form result df === #
res1900 <- res %>%
  filter(Year==1900)

v <- c('estimate', 'std.error', 'signif', 'adj.r.squared')

res1900_long <- reshape(res1900, 
                        direction = 'long',
                        varying = lapply(v, grep, names(res)),
                        v.names = v,
                        idvar = 'term',
                        timevar = 'fit')
# .
# ..
# ...
# ..
# .

# === export and save LaTeX-ready .txt file of res_long table to directory === #
print.xtable(xtable(res_long),
             type = "latex", 
             file = "./res_Wahl_repl_rex.txt", 
             size = "tiny")