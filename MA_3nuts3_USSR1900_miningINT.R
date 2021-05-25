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
df <- read.csv('MA_nuts3_data.csv')

# Mining Interaction
# Controls: country + mining + trade_center*mining

# PANEL A: trade center dummy
A_fit1 <- df %>% filter(year>1990, east_germany == 1 | country %in% c('Poland','Hungary','Lithuania')) %>% nest_by(year) %>%
  mutate(mod = list(lm(lnGDPpc ~ trade_center +
                         country + mining + trade_center*mining,
                       data = data)))
tidyA1 <- A_fit1 %>% summarise(tidy(mod))
glanA1 <- A_fit1 %>% summarise(glance(mod))

# PANEL B: Centuries of Trade 
B_fit1 <- df %>% filter(year>1990, east_germany == 1 | country %in% c('Poland','Hungary','Lithuania')) %>% nest_by(year) %>%
  mutate(mod = list(lm(lnGDPpc ~ ln_tc_dist +
                         country + mining + ln_tc_dist*mining,
                       data = data)))
tidyB1 <- B_fit1 %>% summarise(tidy(mod))
glanB1 <- B_fit1 %>% summarise(glance(mod))

# PANEL C: trade center density
C_fit1 <- df %>% filter(year>1990, east_germany == 1 | country %in% c('Poland','Hungary','Lithuania')) %>% nest_by(year) %>%
  mutate(mod = list(lm(lnGDPpc ~ trade_years +
                         country + mining + trade_years*mining,
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

res2 <- tidy1 %>%
  filter(term %in% c('trade_center:mining', 'ln_tc_dist:mining', 'trade_years:mining'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res2$adj.r.squared <- glan1$adj.r.squared

res <- rbind(res1, res2)
