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

# Mining Interaction
# Controls: country + mining + mining*trade_center

# PANEL A: trade center dummy
A_fit1 <- df %>% nest_by(Year) %>%
  mutate(mod = list(lm(lnGDPpc ~ trade_center +
                         country + mining + trade_center*mining,
                       data = data)))
tidyA1 <- A_fit1 %>% summarise(tidy(mod))
glanA1 <- A_fit1 %>% summarise(glance(mod))

# PANEL B: Centuries of Trade 
B_fit1 <- df %>% nest_by(Year) %>%
  mutate(mod = list(lm(lnGDPpc ~ ln_tc_dist +
                         country + mining + ln_tc_dist*mining,
                       data = data)))
tidyB1 <- B_fit1 %>% summarise(tidy(mod))
glanB1 <- B_fit1 %>% summarise(glance(mod))

# PANEL C: trade center density
C_fit1 <- df %>% nest_by(Year) %>%
  mutate(mod = list(lm(lnGDPpc ~ trade_years +
                         country + mining + trade_years*mining,
                       data = data)))
tidyC1 <- C_fit1 %>% summarise(tidy(mod))
glanC1 <- C_fit1 %>% summarise(glance(mod))

# PANEL D: share NUTS3 trade regions per NUTS2 region
D_fit1 <- df %>% nest_by(Year) %>%
  mutate(mod = list(lm(lnGDPpc ~ tc_ratio +
                         country + mining + tc_ratio*mining,
                       data = data)))
tidyD1 <- D_fit1 %>% summarise(tidy(mod))
glanD1 <- D_fit1 %>% summarise(glance(mod))

# PANEL E: NUTS3 trade regions per NUTS2 area
E_fit1 <- df %>% nest_by(Year) %>%
  mutate(mod = list(lm(lnGDPpc ~ tc_dens +
                         country + mining + tc_dens*mining,
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

res2 <- tidy1 %>%
  filter(term %in% c('trade_center:mining', 'ln_tc_dist:mining', 'trade_years:mining', 'tc_ratio:mining', 'tc_dens:mining'),) %>%
  mutate(signif = stars.pval(p.value)) %>%
  select(-(statistic:p.value))
res2$adj.r.squared <- glan1$adj.r.squared

res <- rbind(res1, res2)
