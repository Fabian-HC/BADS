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
pacman::p_load(plyr, dplyr, broom, tidyr, xtable, gtools, stringr)

# This Script codes tight replications of the regression model fits published as Table 2 in F. Wahl.
# According to F. Wahl on 09.04.21 via telephone, naming of some incl. the dependent variable 'lngdp' is confusing because it in fact represents the ln of regional GDP per capita
# === AGENDA === #
# After loading data as shared for replication by F. Wahl
# Design the six model fits and perform regressions for each Panel-Fit combination
# Concluding each fit-section, create a tidy selection of regression output
# Combine the intermediate per-fit results into one result table
# Reshape result to long form and save as LaTeX-ready .txt to directory

# === load data === #
df <- read.csv('data_wahl.csv')

wahl <- c(
  "lngdp",
  "trade_city_final",
  "lndist_trade_city1",
  "trade_year",
  "country",
  "nuts_1",
  "nuts_2",
  "latitude",
  "longitude",
  "altitude",
  "lndist_border",
  "lndist_coast",
  "lndist_river",
  "neighborgdp",
  "tradeneighbor",
  "tradeneighbor2",
  "tradeneighbor3",
  "tradeneighbor4",
  "capital",
  "mountain_region",
  "mining",
  "lnarea",
  "lnagri",
  "university_1500",
  "printingpress_pre1500",
  "bishop",
  "imperial_city",
  "hanse_city",
  "residence",
  "imperialroad",
  "lndist_roman",
  "education",
  "eqi100",
  "inequality2",
  "patents",
  "unemployment",
  "lnemp_comp",
  "lnfixed_cap")
df_wahl <- df[wahl]
summary(df_wahl)
stargazer(df_wahl, title="Descriptive Statistics, c.p. Wahl (2016)", summary.stat = c("n", "mean", "sd", "min", "max"), digits=3, out="MA_1nuts3_DSfull")


# === FIT 1 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4

# PANEL A: trade center dummy
A_fit1 <- lm(lngdp ~ trade_city_final +
               nuts_1 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
             data=df)
summary(A_fit1)

# PANEL B: Centuries of Trade 
B_fit1 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_1 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
             data=df)
summary(B_fit1)

# PANEL C: trade center density
C_fit1 <- lm(lngdp ~ trade_year +
               nuts_1 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4,
             data=df)
summary(C_fit1)

# === Fit (1) tidy selection of regression output === #
# list of Panels
p1 <- list(A=A_fit1, B=B_fit1, C=C_fit1)
# prepare adj.r.squared
gla1 <- ldply(p1, glance) %>%
  select(adj.r.squared)
# tidy selection
res1 <- ldply(p1, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla1$adj.r.squared) %>%
  select(-(statistic:p.value))


# === FIT 2 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain_region + mining + lnarea + lnagri

# PANEL A: Trade Center Dummy
A_fit2 <- lm(lngdp ~ trade_city_final +
               nuts_1 +
               latitude + longitude + altitude +
               capital + mountain_region + mining + lnarea + lnagri,
             data = df)
summary(A_fit2)

# PANEL B: Distance to Trade 
B_fit2 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_1 +
               latitude + longitude + altitude +
               capital + mountain_region + mining + lnarea + lnagri,
             data = df)
summary(B_fit2)

# PANEL C: Centuries of Trade
C_fit2 <- lm(lngdp ~ trade_year +
               nuts_1 +
               latitude + longitude + altitude +
               capital + mountain_region + mining + lnarea + lnagri,
             data = df)
summary(C_fit2)

# === Fit (2) tidy selection of regression output === #
# list of Panels
p2 <- list(A=A_fit2, B=B_fit2, C=C_fit2)
# prepare adj.r.squared
gla2 <- ldply(p2, glance) %>%
  select(adj.r.squared)
# tidy selection
res2 <- ldply(p2, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla2$adj.r.squared) %>%
  select(-(statistic:p.value))


# === FIT 3 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman

# PANEL A: Trade Center Dummy
A_fit3 <-lm(lngdp ~ trade_city_final +
              nuts_1 +
              latitude + longitude + altitude +
              university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
            data = df)
summary(A_fit3)

# PANEL B: Distance to Trade 
B_fit3 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_1 +
               latitude + longitude + altitude +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
             data = df)
summary(B_fit3)

# PANEL C: Centuries of Trade
C_fit3 <- lm(lngdp ~ trade_year +
               nuts_1 +
               latitude + longitude + altitude +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
             data = df)
summary(C_fit3)

# === Fit (3) tidy selection of regression output === #
# list of Panels
p3 <- list(A=A_fit3, B=B_fit3, C=C_fit3)
# prepare adj.r.squared
gla3 <- ldply(p3, glance) %>%
  select(adj.r.squared)
# tidy selection
res3 <- ldply(p3, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla3$adj.r.squared) %>%
  select(-(statistic:p.value))


# === FIT 4 === #
# Controls:
# NUTS dummies: nuts_1
# Basic geographic controls: latitude + longitude + altitude
# Growth covariates: education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap

# PANEL A: Trade Center Dummy
A_fit4 <-lm(lngdp ~ trade_city_final +
              nuts_1 +
              latitude + longitude + altitude +
              education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
            data = df)
summary(A_fit4)

# PANEL B: Distance to Trade 
B_fit4 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_1 +
               latitude + longitude + altitude +
               education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
             data = df)
summary(B_fit4)

# PANEL C: Centuries of Trade
C_fit4 <- lm(lngdp ~ trade_year +
               nuts_1 +
               latitude + longitude + altitude +
               education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
             data = df)
summary(C_fit4)

# === Fit (4) tidy selection of regression output === #
# list of Panels
p4 <- list(A=A_fit4, B=B_fit4, C=C_fit4)
# prepare adj.r.squared
gla4 <- ldply(p4, glance) %>%
  select(adj.r.squared)
# tidy selection
res4 <- ldply(p4, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla4$adj.r.squared) %>%
  select(-(statistic:p.value))


# === FIT 5 === #
# Controls:
# NUTS dummies: nuts_1
# Basic geographic controls: latitude + longitude + altitude
# Regional environment and location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4
# Region characteristics: capital + mountain_region + mining + lnarea + lnagri
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman
# Growth covariates: education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap

# PANEL A: Trade Center Dummy
A_fit5 <-lm(lngdp ~ trade_city_final +
              nuts_1 +
              latitude + longitude + altitude +
              lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
              capital + mountain_region + mining + lnarea + lnagri +
              university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman +
              education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
            data = df)
summary(A_fit5)

# PANEL B: Distance to Trade 
B_fit5 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_1 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
               capital + mountain_region + mining + lnarea + lnagri +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman +
               education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
             data = df)
summary(B_fit5)

# PANEL C: Centuries of Trade
C_fit5 <- lm(lngdp ~ trade_year +
               nuts_1 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
               capital + mountain_region + mining + lnarea + lnagri +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman +
               education + eqi100 + inequality2 + patents + unemployment + lnemp_comp + lnfixed_cap,
             data = df)
summary(C_fit5)

# === Fit (5) tidy selection of regression output === #
# list of Panels
p5 <- list(A=A_fit5, B=B_fit5, C=C_fit5)
# prepare adj.r.squared
gla5 <- ldply(p5, glance) %>%
  select(adj.r.squared)
# tidy selection
res5 <- ldply(p5, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla5$adj.r.squared) %>%
  select(-(statistic:p.value))


# === FIT 6 === #
# NUTS dummies: nuts_2
# Basic geographic controls: latitude + longitude + altitude
# Regional environment and location: lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4
# Region characteristics: capital + mountain_region + mining + lnarea + lnagri
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman

# PANEL A: Trade Center Dummy
A_fit6 <-lm(lngdp ~ trade_city_final +
              nuts_2 +
              latitude + longitude + altitude +
              lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
              capital + mountain_region + mining + lnarea + lnagri +
              university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
            data = df)
summary(A_fit6)

# PANEL B: Distance to Trade 
B_fit6 <- lm(lngdp ~ lndist_trade_city1 +
               nuts_2 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
               capital + mountain_region + mining + lnarea + lnagri +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
             data = df)
summary(B_fit6)

# PANEL C: Centuries of Trade
C_fit6 <- lm(lngdp ~ trade_year +
               nuts_2 +
               latitude + longitude + altitude +
               lndist_border + lndist_coast + lndist_river + neighborgdp + tradeneighbor + tradeneighbor2 + tradeneighbor3 + tradeneighbor4 +
               capital + mountain_region + mining + lnarea + lnagri +
               university_1500 + printingpress_pre1500 + bishop + imperial_city + hanse_city + residence + imperialroad + lndist_roman,
             data = df)
summary(C_fit6)

# mining coefficient is not defined because of singularities in all Fit6 regressions, where the regional control is passed to nuts_2
# example check for Panel A
alias(A_fit6)
# shows linear dependence of some nuts_2 region controls with mining
# further investigation shows that mining data is collected on NUTS2 level so the result is not surprising
# --> mining dropped from Fit6



# === Fit (6) tidy selection of regression output === #
# list of Panels
p6 <- list(A=A_fit6, B=B_fit6, C=C_fit6)
# prepare adj.r.squared
gla6 <- ldply(p6, glance) %>%
  select(adj.r.squared)
# tidy selection
res6 <- ldply(p6, tidy, .id = "panel") %>%
  filter(term %in% c('trade_city_final', 'lndist_trade_city1', 'trade_year'),) %>%
  mutate(signif = stars.pval(p.value),
         adj.r.squared = gla6$adj.r.squared) %>%
  select(-(statistic:p.value))


# === Combine intermediate results for all (six) model fits with for-loop into one wide-form result df === #
list.df <- list(res1, res2, res3, res4, res5, res6)
sfx <- c(".fit1", ".fit2", ".fit3", ".fit4", ".fit5", ".fit6")
res <- list.df[[1]]
for(i in head(seq_along(list.df), -1)) {
  
  res <- merge(res, list.df[[i+1]], all = TRUE, 
               suffixes = sfx[i:(i+1)], by = c('panel','term'))
}
is.num <- sapply(res, is.numeric)
res[is.num] <- lapply(res[is.num], round, 3)

# wide form result df with numeric entries rounded to three decimals
res

# === reshape to long form result df === #
v <- c('estimate', 'std.error', 'signif', 'adj.r.squared')

res_long <- reshape(res, 
        direction = 'long',
        varying = lapply(v, grep, names(res)),
        v.names = v,
        idvar = 'panel',
        timevar = 'fit')

# === export and save LaTeX-ready .txt file of res_long table to directory === #
print.xtable(xtable(res_long),
             type = "latex", 
             file = "./res_Wahl_repl_rex.txt", 
             size = "tiny")
