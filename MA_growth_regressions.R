# === NEW COMMMENT TEST === #
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
pacman::p_load(dplyr, broom)

# === load data === #
df <- read.csv('MA_data.csv')

# === define period subsets for regressions on growth as dependent variable === #

# === FIT 1 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: dist_border + dist_coast

# PANEL A: trade center dummy
A_fit1 <- subset(df, Year>1900) %>% group_by(Year) %>%
  do(model = tidy(lm(GDPpcGrowth ~ trade_center +
                       country + latitude + longitude + altitude +
                       dist_border + dist_coast, data = ))) %>% 
  unnest(model)
A_fit1 %>% print(n = Inf)

A_fit1_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1910)
summary(A_fit1_1910)

A_fit1_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1925)
summary(A_fit1_1925)$coefficients[c('trade_center'),]

A_fit1_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1938)
summary(A_fit1_1938)$coefficients[c('trade_center'),]

A_fit1_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1950)
summary(A_fit1_1950)$coefficients[c('trade_center'),]

A_fit1_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1960)
summary(A_fit1_1960)$coefficients[c('trade_center'),]

A_fit1_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1970)
summary(A_fit1_1970)$coefficients[c('trade_center'),]

A_fit1_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1980)
summary(A_fit1_1980)$coefficients[c('trade_center'),]

A_fit1_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1990)
summary(A_fit1_1990)$coefficients[c('trade_center'),]

A_fit1_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2000)
summary(A_fit1_2000)$coefficients[c('trade_center'),]

A_fit1_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2010)
summary(A_fit1_2010)$coefficients[c('trade_center'),]

A_fit1_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2015)
summary(A_fit1_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit1_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1910)
summary(B_fit1_1910)$coefficients[c('trade_year'),]

B_fit1_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1925)
summary(B_fit1_1925)$coefficients[c('trade_year'),]

B_fit1_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1938)
summary(B_fit1_1938)$coefficients[c('trade_year'),]

B_fit1_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1950)
summary(B_fit1_1950)$coefficients[c('trade_year'),]

B_fit1_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1960)
summary(B_fit1_1960)$coefficients[c('trade_year'),]

B_fit1_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1970)
summary(B_fit1_1970)$coefficients[c('trade_year'),]

B_fit1_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1980)
summary(B_fit1_1980)$coefficients[c('trade_year'),]

B_fit1_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1990)
summary(B_fit1_1990)$coefficients[c('trade_year'),]

B_fit1_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2000)
summary(B_fit1_2000)$coefficients[c('trade_year'),]

B_fit1_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2010)
summary(B_fit1_2010)$coefficients[c('trade_year'),]

B_fit1_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2015)
summary(B_fit1_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit1_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1910)
summary(C_fit1_1910)$coefficients[c('num_trade_centers'),]

C_fit1_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1925)
summary(C_fit1_1925)$coefficients[c('num_trade_centers'),]

C_fit1_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1938)
summary(C_fit1_1938)$coefficients[c('num_trade_centers'),]

C_fit1_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1950)
summary(C_fit1_1950)$coefficients[c('num_trade_centers'),]

C_fit1_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1960)
summary(C_fit1_1960)$coefficients[c('num_trade_centers'),]

C_fit1_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1970)
summary(C_fit1_1970)$coefficients[c('num_trade_centers'),]

C_fit1_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1980)
summary(C_fit1_1980)$coefficients[c('num_trade_centers'),]

C_fit1_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat1990)
summary(C_fit1_1990)$coefficients[c('num_trade_centers'),]

C_fit1_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2000)
summary(C_fit1_2000)$coefficients[c('num_trade_centers'),]

C_fit1_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2010)
summary(C_fit1_2010)$coefficients[c('num_trade_centers'),]

C_fit1_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    dist_border_ln + dist_coast_ln,
                  data = dat2015)
summary(C_fit1_2015)$coefficients[c('num_trade_centers'),]



# === FIT 2 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + AREA_ln + agri_ln

# PANEL A: trade center dummy

A_fit2_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(A_fit2_1910)$coefficients[c('trade_center'),]

A_fit2_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(A_fit2_1925)$coefficients[c('trade_center'),]

A_fit2_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(A_fit2_1938)$coefficients[c('trade_center'),]

A_fit2_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(A_fit2_1950)$coefficients[c('trade_center'),]

A_fit2_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(A_fit2_1960)$coefficients[c('trade_center'),]

A_fit2_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(A_fit2_1970)$coefficients[c('trade_center'),]

A_fit2_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(A_fit2_1980)$coefficients[c('trade_center'),]

A_fit2_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(A_fit2_1990)$coefficients[c('trade_center'),]

A_fit2_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(A_fit2_2000)$coefficients[c('trade_center'),]

A_fit2_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(A_fit2_2010)$coefficients[c('trade_center'),]

A_fit2_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(A_fit2_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit2_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(B_fit2_1910)$coefficients[c('trade_year'),]

B_fit2_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(B_fit2_1925)$coefficients[c('trade_year'),]

B_fit2_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(B_fit2_1938)$coefficients[c('trade_year'),]

B_fit2_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(B_fit2_1950)$coefficients[c('trade_year'),]

B_fit2_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(B_fit2_1960)$coefficients[c('trade_year'),]

B_fit2_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(B_fit2_1970)$coefficients[c('trade_year'),]

B_fit2_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(B_fit2_1980)$coefficients[c('trade_year'),]

B_fit2_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(B_fit2_1990)$coefficients[c('trade_year'),]

B_fit2_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(B_fit2_2000)$coefficients[c('trade_year'),]

B_fit2_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(B_fit2_2010)$coefficients[c('trade_year'),]

B_fit2_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(B_fit2_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit2_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(C_fit2_1910)$coefficients[c('num_trade_centers'),]

C_fit2_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(C_fit2_1925)$coefficients[c('num_trade_centers'),]

C_fit2_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(C_fit2_1938)$coefficients[c('num_trade_centers'),]

C_fit2_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(C_fit2_1950)$coefficients[c('num_trade_centers'),]

C_fit2_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(C_fit2_1960)$coefficients[c('num_trade_centers'),]

C_fit2_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(C_fit2_1970)$coefficients[c('num_trade_centers'),]

C_fit2_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(C_fit2_1980)$coefficients[c('num_trade_centers'),]

C_fit2_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(C_fit2_1990)$coefficients[c('num_trade_centers'),]

C_fit2_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(C_fit2_2000)$coefficients[c('num_trade_centers'),]

C_fit2_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(C_fit2_2010)$coefficients[c('num_trade_centers'),]

C_fit2_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(C_fit2_2015)$coefficients[c('num_trade_centers'),]



# === FIT 3 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit3_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit3_1910)$coefficients[c('trade_center'),]

A_fit3_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit3_1925)$coefficients[c('trade_center'),]

A_fit3_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit3_1938)$coefficients[c('trade_center'),]

A_fit3_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit3_1950)$coefficients[c('trade_center'),]

A_fit3_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit3_1960)$coefficients[c('trade_center'),]

A_fit3_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit3_1970)$coefficients[c('trade_center'),]

A_fit3_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit3_1980)$coefficients[c('trade_center'),]

A_fit3_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit3_1990)$coefficients[c('trade_center'),]

A_fit3_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit3_2000)$coefficients[c('trade_center'),]

A_fit3_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit3_2010)$coefficients[c('trade_center'),]

A_fit3_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit3_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit3_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit3_1910)$coefficients[c('trade_year'),]

B_fit3_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit3_1925)$coefficients[c('trade_year'),]

B_fit3_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit3_1938)$coefficients[c('trade_year'),]

B_fit3_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit3_1950)$coefficients[c('trade_year'),]

B_fit3_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit3_1960)$coefficients[c('trade_year'),]

B_fit3_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit3_1970)$coefficients[c('trade_year'),]

B_fit3_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit3_1980)$coefficients[c('trade_year'),]

B_fit3_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit3_1990)$coefficients[c('trade_year'),]

B_fit3_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit3_2000)$coefficients[c('trade_year'),]

B_fit3_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit3_2010)$coefficients[c('trade_year'),]

B_fit3_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit3_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit3_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit3_1910)$coefficients[c('num_trade_centers'),]

C_fit3_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit3_1925)$coefficients[c('num_trade_centers'),]

C_fit3_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit3_1938)$coefficients[c('num_trade_centers'),]

C_fit3_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit3_1950)$coefficients[c('num_trade_centers'),]

C_fit3_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit3_1960)$coefficients[c('num_trade_centers'),]

C_fit3_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit3_1970)$coefficients[c('num_trade_centers'),]

C_fit3_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit3_1980)$coefficients[c('num_trade_centers'),]

C_fit3_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit3_1990)$coefficients[c('num_trade_centers'),]

C_fit3_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit3_2000)$coefficients[c('num_trade_centers'),]

C_fit3_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit3_2010)$coefficients[c('num_trade_centers'),]

C_fit3_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit3_2015)$coefficients[c('num_trade_centers'),]



# === FIT 4 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit4_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit4_1910)$coefficients[c('trade_center'),]

A_fit4_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit4_1925)$coefficients[c('trade_center'),]

A_fit4_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit4_1938)$coefficients[c('trade_center'),]

A_fit4_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit4_1950)$coefficients[c('trade_center'),]

A_fit4_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit4_1960)$coefficients[c('trade_center'),]

A_fit4_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit4_1970)$coefficients[c('trade_center'),]

A_fit4_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit4_1980)$coefficients[c('trade_center'),]

A_fit4_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit4_1990)$coefficients[c('trade_center'),]

A_fit4_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit4_2000)$coefficients[c('trade_center'),]

A_fit4_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit4_2010)$coefficients[c('trade_center'),]

A_fit4_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit4_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit4_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit4_1910)$coefficients[c('trade_year'),]

B_fit4_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit4_1925)$coefficients[c('trade_year'),]

B_fit4_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit4_1938)$coefficients[c('trade_year'),]

B_fit4_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit4_1950)$coefficients[c('trade_year'),]

B_fit4_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit4_1960)$coefficients[c('trade_year'),]

B_fit4_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit4_1970)$coefficients[c('trade_year'),]

B_fit4_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit4_1980)$coefficients[c('trade_year'),]

B_fit4_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit4_1990)$coefficients[c('trade_year'),]

B_fit4_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit4_2000)$coefficients[c('trade_year'),]

B_fit4_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit4_2010)$coefficients[c('trade_year'),]

B_fit4_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit4_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit4_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit4_1910)$coefficients[c('num_trade_centers'),]

C_fit4_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit4_1925)$coefficients[c('num_trade_centers'),]

C_fit4_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit4_1938)$coefficients[c('num_trade_centers'),]

C_fit4_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit4_1950)$coefficients[c('num_trade_centers'),]

C_fit4_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit4_1960)$coefficients[c('num_trade_centers'),]

C_fit4_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit4_1970)$coefficients[c('num_trade_centers'),]

C_fit4_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit4_1980)$coefficients[c('num_trade_centers'),]

C_fit4_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit4_1990)$coefficients[c('num_trade_centers'),]

C_fit4_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit4_2000)$coefficients[c('num_trade_centers'),]

C_fit4_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit4_2010)$coefficients[c('num_trade_centers'),]

C_fit4_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit4_2015)$coefficients[c('num_trade_centers'),]



# === FIT 4 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit5_1910 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit5_1910)$coefficients[c('trade_center'),]

A_fit5_1925 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit5_1925)$coefficients[c('trade_center'),]

A_fit5_1938 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit5_1938)$coefficients[c('trade_center'),]

A_fit5_1950 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit5_1950)$coefficients[c('trade_center'),]

A_fit5_1960 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit5_1960)$coefficients[c('trade_center'),]

A_fit5_1970 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit5_1970)$coefficients[c('trade_center'),]

A_fit5_1980 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit5_1980)$coefficients[c('trade_center'),]

A_fit5_1990 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit5_1990)$coefficients[c('trade_center'),]

A_fit5_2000 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit5_2000)$coefficients[c('trade_center'),]

A_fit5_2010 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit5_2010)$coefficients[c('trade_center'),]

A_fit5_2015 <- lm(GDPpcGrowth ~ trade_center +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit5_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit5_1910 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit5_1910)$coefficients[c('trade_year'),]

B_fit5_1925 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit5_1925)$coefficients[c('trade_year'),]

B_fit5_1938 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit5_1938)$coefficients[c('trade_year'),]

B_fit5_1950 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit5_1950)$coefficients[c('trade_year'),]

B_fit5_1960 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit5_1960)$coefficients[c('trade_year'),]

B_fit5_1970 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit5_1970)$coefficients[c('trade_year'),]

B_fit5_1980 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit5_1980)$coefficients[c('trade_year'),]

B_fit5_1990 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit5_1990)$coefficients[c('trade_year'),]

B_fit5_2000 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit5_2000)$coefficients[c('trade_year'),]

B_fit5_2010 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit5_2010)$coefficients[c('trade_year'),]

B_fit5_2015 <- lm(GDPpcGrowth ~ trade_year +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit5_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit5_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit5_1910)$coefficients[c('num_trade_centers'),]

C_fit5_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit5_1925)$coefficients[c('num_trade_centers'),]

C_fit5_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit5_1938)$coefficients[c('num_trade_centers'),]

C_fit5_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit5_1950)$coefficients[c('num_trade_centers'),]

C_fit5_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit5_1960)$coefficients[c('num_trade_centers'),]

C_fit5_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit5_1970)$coefficients[c('num_trade_centers'),]

C_fit5_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit5_1980)$coefficients[c('num_trade_centers'),]

C_fit5_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit5_1990)$coefficients[c('num_trade_centers'),]

C_fit5_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit5_2000)$coefficients[c('num_trade_centers'),]

C_fit5_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit5_2010)$coefficients[c('num_trade_centers'),]

C_fit5_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    nuts_1 +
                    latitude + longitude + altitude +
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit5_2015)$coefficients[c('num_trade_centers'),]
