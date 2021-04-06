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
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load()

# === load data === #
df <- read.csv('MA_data.csv')

# === define period subsets for regressions on growth as dependent variable === #
dat1910 <- df[df$Year == 1910,]
dat1925 <- df[df$Year == 1925,]
dat1938 <- df[df$Year == 1938,]
dat1950 <- df[df$Year == 1950,]
dat1960 <- df[df$Year == 1960,]
dat1970 <- df[df$Year == 1970,]
dat1980 <- df[df$Year == 1980,]
dat1990 <- df[df$Year == 1990,]
dat2000 <- df[df$Year == 2000,]
dat2010 <- df[df$Year == 2010,]
dat2015 <- df[df$Year == 2015,]

# === FIT 1 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: 

# PANEL A: trade center dummy

A_fit1_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1910)
summary(A_fit1_1910)$coefficients[c('trade_center'),]

A_fit1_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1925)
summary(A_fit1_1925)$coefficients[c('trade_center'),]

A_fit1_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1938)
summary(A_fit1_1938)$coefficients[c('trade_center'),]

A_fit1_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1950)
summary(A_fit1_1950)$coefficients[c('trade_center'),]

A_fit1_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1960)
summary(A_fit1_1960)$coefficients[c('trade_center'),]

A_fit1_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1970)
summary(A_fit1_1970)$coefficients[c('trade_center'),]

A_fit1_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1980)
summary(A_fit1_1980)$coefficients[c('trade_center'),]

A_fit1_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat1990)
summary(A_fit1_1990)$coefficients[c('trade_center'),]

A_fit1_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat2000)
summary(A_fit1_2000)$coefficients[c('trade_center'),]

A_fit1_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat2010)
summary(A_fit1_2010)$coefficients[c('trade_center'),]

A_fit1_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country,
                  data = dat2015)
summary(A_fit1_2015)$coefficients[c('trade_center'),]


# PANEL B: Centuries of Trade 

B_fit1_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1910)
summary(B_fit1_1910)$coefficients[c('trade_year'),]

B_fit1_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1925)
summary(B_fit1_1925)$coefficients[c('trade_year'),]

B_fit1_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1938)
summary(B_fit1_1938)$coefficients[c('trade_year'),]

B_fit1_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1950)
summary(B_fit1_1950)$coefficients[c('trade_year'),]

B_fit1_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1960)
summary(B_fit1_1960)$coefficients[c('trade_year'),]

B_fit1_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1970)
summary(B_fit1_1970)$coefficients[c('trade_year'),]

B_fit1_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1980)
summary(B_fit1_1980)$coefficients[c('trade_year'),]

B_fit1_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat1990)
summary(B_fit1_1990)$coefficients[c('trade_year'),]

B_fit1_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat2000)
summary(B_fit1_2000)$coefficients[c('trade_year'),]

B_fit1_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat2010)
summary(B_fit1_2010)$coefficients[c('trade_year'),]

B_fit1_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country,
                  data = dat2015)
summary(B_fit1_2015)$coefficients[c('trade_year'),]


# PANEL C: number of trade centers per NUTS-2 Region

C_fit1_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1910)
summary(C_fit1_1910)$coefficients[c('num_trade_centers'),]

C_fit1_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1925)
summary(C_fit1_1925)$coefficients[c('num_trade_centers'),]

C_fit1_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1938)
summary(C_fit1_1938)$coefficients[c('num_trade_centers'),]

C_fit1_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1950)
summary(C_fit1_1950)$coefficients[c('num_trade_centers'),]

C_fit1_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1960)
summary(C_fit1_1960)$coefficients[c('num_trade_centers'),]

C_fit1_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1970)
summary(C_fit1_1970)$coefficients[c('num_trade_centers'),]

C_fit1_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1980)
summary(C_fit1_1980)$coefficients[c('num_trade_centers'),]

C_fit1_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat1990)
summary(C_fit1_1990)$coefficients[c('num_trade_centers'),]

C_fit1_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat2000)
summary(C_fit1_2000)$coefficients[c('num_trade_centers'),]

C_fit1_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat2010)
summary(C_fit1_2010)$coefficients[c('num_trade_centers'),]

C_fit1_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country,
                  data = dat2015)
summary(C_fit1_2015)$coefficients[c('num_trade_centers'),]