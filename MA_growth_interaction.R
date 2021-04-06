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
# NUTS dummies: country + mining + mining*trade_center
# Basic Geographic Controls: latitude + longitude + altitude
# Regional Environment and Location: 

# PANEL A: trade center dummy

A_fit1_1910 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1910)
summary(A_fit1_1910)

A_fit1_1925 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1925)
summary(A_fit1_1925)

A_fit1_1938 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1938)
summary(A_fit1_1938)

A_fit1_1950 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1950)
summary(A_fit1_1950)

A_fit1_1960 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1960)
summary(A_fit1_1960)

A_fit1_1970 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1970)
summary(A_fit1_1970)

A_fit1_1980 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1980)
summary(A_fit1_1980)

A_fit1_1990 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat1990)
summary(A_fit1_1990)

A_fit1_2000 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat2000)
summary(A_fit1_2000)

A_fit1_2010 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat2010)
summary(A_fit1_2010)

A_fit1_2015 <- lm(GDPpcGrowth ~ trade_center +
                    country + mining + mining*trade_center,
                  data = dat2015)
summary(A_fit1_2015)


# PANEL B: Centuries of Trade 

B_fit1_1910 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1910)
summary(B_fit1_1910)

B_fit1_1925 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1925)
summary(B_fit1_1925)

B_fit1_1938 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1938)
summary(B_fit1_1938)

B_fit1_1950 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1950)
summary(B_fit1_1950)

B_fit1_1960 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1960)
summary(B_fit1_1960)

B_fit1_1970 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1970)
summary(B_fit1_1970)

B_fit1_1980 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1980)
summary(B_fit1_1980)

B_fit1_1990 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat1990)
summary(B_fit1_1990)

B_fit1_2000 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat2000)
summary(B_fit1_2000)

B_fit1_2010 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat2010)
summary(B_fit1_2010)

B_fit1_2015 <- lm(GDPpcGrowth ~ trade_year +
                    country + mining + mining*trade_center,
                  data = dat2015)
summary(B_fit1_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit1_1910 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1910)
summary(C_fit1_1910)

C_fit1_1925 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1925)
summary(C_fit1_1925)

C_fit1_1938 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1938)
summary(C_fit1_1938)

C_fit1_1950 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1950)
summary(C_fit1_1950)

C_fit1_1960 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1960)
summary(C_fit1_1960)

C_fit1_1970 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1970)
summary(C_fit1_1970)

C_fit1_1980 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1980)
summary(C_fit1_1980)

C_fit1_1990 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat1990)
summary(C_fit1_1990)

C_fit1_2000 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat2000)
summary(C_fit1_2000)

C_fit1_2010 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat2010)
summary(C_fit1_2010)

C_fit1_2015 <- lm(GDPpcGrowth ~ num_trade_centers +
                    country + mining + mining*trade_center,
                  data = dat2015)
summary(C_fit1_2015)