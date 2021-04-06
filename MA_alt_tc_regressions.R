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
pacman::p_load(lme4, broom, dplyr)

# === load data === #
df <- read.csv('MA_data.csv')


# === FIT 1 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: dist_border_ln + dist_coast_ln

## PANEL A:
# trade center dummy alt 1
A_fit1_alt_tc1 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_1 +
                       country + latitude + longitude + altitude +
                       dist_border_ln + dist_coast_ln, data = .))) %>% 
  unnest(model)
A_fit1_alt_tc1 %>% print(n = Inf)

# trade center dummy alt 2
A_fit1_alt_tc2 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_2 +
                       country + latitude + longitude + altitude +
                       dist_border_ln + dist_coast_ln, data = .))) %>% 
  unnest(model)
A_fit1_alt_tc2 %>% print(n = Inf)


# === FIT 2 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + AREA_ln + agri_ln

## PANEL A:
# trade center dummy alt 1
A_fit2_alt_tc1 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_1 +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln, data = .))) %>% 
  unnest(model)
A_fit2_alt_tc1 %>% print(n = Inf)

# trade center dummy alt 2
A_fit2_alt_tc2 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_2 +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln, data = .))) %>% 
  unnest(model)
A_fit2_alt_tc2 %>% print(n = Inf)


# === FIT 3 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

## PANEL A:
# trade center dummy alt 1
A_fit3_alt_tc1 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_1 +
                       country + latitude + longitude + altitude +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit3_alt_tc1 %>% print(n = Inf)

# trade center dummy alt 2
A_fit3_alt_tc2 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_2 +
                       country + latitude + longitude + altitude +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit3_alt_tc2 %>% print(n = Inf)


# === FIT 4 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

## PANEL A:
# trade center dummy alt 1
A_fit4_alt_tc1 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_1 +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit4_alt_tc1 %>% print(n = Inf)

# trade center dummy alt 2
A_fit4_alt_tc2 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_2 +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit4_alt_tc2 %>% print(n = Inf)


# === FIT 5 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

## PANEL A:
# trade center dummy alt 1
A_fit5_alt_tc1 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_1 +
                       nuts_1 + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit5_alt_tc1 %>% print(n = Inf)

# trade center dummy alt 2
A_fit5_alt_tc2 <- df %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center_2 +
                       nuts_1 + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit5_alt_tc2 %>% print(n = Inf)
