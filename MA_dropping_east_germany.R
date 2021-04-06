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
pacman::p_load(dplyr, broom, tidyr)

# === load data === #
df <- read.csv('MA_data.csv')

# === ABOUT THIS SCRIPT === #
# R syntax to display regression output leans on:
# https://stackoverflow.com/questions/22713325/fitting-several-regression-models-with-dplyr?noredirect=1&lq=1


# === FIT 1 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: dist_border_ln + dist_coast_ln

# PANEL A: trade center dummy
A_fit1 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center +
                       country + latitude + longitude + altitude +
                       dist_border_ln + dist_coast_ln, data = .))) %>% 
  unnest(model)
A_fit1 %>% print(n = Inf)

# PANEL B: Centuries of Trade 
B_fit1 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_year +
                       country + latitude + longitude + altitude +
                       dist_border_ln + dist_coast_ln, data = .))) %>% 
  unnest(model)
B_fit1 %>% print(n = Inf)

# PANEL C: number of trade centers per NUTS-2 Region
C_fit1 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ num_trade_centers +
                       country + latitude + longitude + altitude +
                       dist_border_ln + dist_coast_ln, data = .))) %>% 
  unnest(model)
C_fit1 %>% print(n = Inf)


# === FIT 2 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + AREA_ln + agri_ln

# PANEL A: trade center dummy
A_fit2 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln, data = .))) %>% 
  unnest(model)
A_fit2 %>% print(n = Inf)

# PANEL B: Centuries of Trade 
B_fit2 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_year +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln, data = .))) %>% 
  unnest(model)
B_fit2 %>% print(n = Inf)

# PANEL C: number of trade centers per NUTS-2 Region
C_fit2 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ num_trade_centers +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln, data = .))) %>% 
  unnest(model)
C_fit2 %>% print(n = Inf)


# === FIT 3 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy
A_fit3 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center +
                       country + latitude + longitude + altitude +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit3 %>% print(n = Inf)

# PANEL B: Centuries of Trade 
B_fit3 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_year +
                       country + latitude + longitude + altitude +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
B_fit3 %>% print(n = Inf)

# PANEL C: number of trade centers per NUTS-2 Region
C_fit3 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ num_trade_centers +
                       country + latitude + longitude + altitude +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
C_fit3 %>% print(n = Inf)


# === FIT 4 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy
A_fit4 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit4 %>% print(n = Inf)

# PANEL B: Centuries of Trade 
B_fit4 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_year +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
B_fit4 %>% print(n = Inf)

# PANEL C: number of trade centers per NUTS-2 Region
C_fit4 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ num_trade_centers +
                       country + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
C_fit4 %>% print(n = Inf)


# === FIT 5 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit5 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_center +
                       nuts_1 + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
A_fit5 %>% print(n = Inf)


# PANEL B: Centuries of Trade 
B_fit5 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ trade_year +
                       nuts_1 + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
B_fit5 %>% print(n = Inf)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit5 <- subset(df, east_germany != 1) %>% group_by(Year) %>%
  do(model = tidy(lm(lnGDPpc ~ num_trade_centers +
                       nuts_1 + latitude + longitude + altitude +
                       capital + mountain + mining + AREA_ln + agri_ln +
                       university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence, data = .))) %>% 
  unnest(model)
C_fit5 %>% print(n = Inf)
