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

# === define period subsets for base regressions === #
dat1900 <- df[df$Year == 1900,]
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

A_fit1_1900 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center,
                  data = dat1900)
summary(A_fit1_1900)

A_fit1_1910 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1910)
summary(A_fit1_1910)

A_fit1_1925 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1925)
summary(A_fit1_1925)

A_fit1_1938 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1938)
summary(A_fit1_1938)

A_fit1_1950 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1950)
summary(A_fit1_1950)

A_fit1_1960 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1960)
summary(A_fit1_1960)

A_fit1_1970 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1970)
summary(A_fit1_1970)

A_fit1_1980 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1980)
summary(A_fit1_1980)

A_fit1_1990 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1990)
summary(A_fit1_1990)

A_fit1_2000 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2000)
summary(A_fit1_2000)

A_fit1_2010 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2010)
summary(A_fit1_2010)

A_fit1_2015 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2015)
summary(A_fit1_2015)


# PANEL B: Centuries of Trade 

B_fit1_1900 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1900)
summary(B_fit1_1900)

B_fit1_1910 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1910)
summary(B_fit1_1910)

B_fit1_1925 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1925)
summary(B_fit1_1925)

B_fit1_1938 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1938)
summary(B_fit1_1938)

B_fit1_1950 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1950)
summary(B_fit1_1950)

B_fit1_1960 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1960)
summary(B_fit1_1960)

B_fit1_1970 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1970)
summary(B_fit1_1970)

B_fit1_1980 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1980)
summary(B_fit1_1980)

B_fit1_1990 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1990)
summary(B_fit1_1990)

B_fit1_2000 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2000)
summary(B_fit1_2000)

B_fit1_2010 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2010)
summary(B_fit1_2010)

B_fit1_2015 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2015)
summary(B_fit1_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit1_1900 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1900)
summary(C_fit1_1900)

C_fit1_1910 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1910)
summary(C_fit1_1910)

C_fit1_1925 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1925)
summary(C_fit1_1925)

C_fit1_1938 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1938)
summary(C_fit1_1938)

C_fit1_1950 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1950)
summary(C_fit1_1950)

C_fit1_1960 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1960)
summary(C_fit1_1960)

C_fit1_1970 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1970)
summary(C_fit1_1970)

C_fit1_1980 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1980)
summary(C_fit1_1980)

C_fit1_1990 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat1990)
summary(C_fit1_1990)

C_fit1_2000 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2000)
summary(C_fit1_2000)

C_fit1_2010 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2010)
summary(C_fit1_2010)

C_fit1_2015 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    ,
                  data = dat2015)
summary(C_fit1_2015)



# === FIT 2 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region Characteristics: capital + mountain + mining + AREA_ln + agri_ln

# PANEL A: trade center dummy

A_fit2_1900 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1900)
summary(A_fit2_1900)

A_fit2_1910 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(A_fit2_1910)

A_fit2_1925 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(A_fit2_1925)

A_fit2_1938 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(A_fit2_1938)

A_fit2_1950 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(A_fit2_1950)

A_fit2_1960 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(A_fit2_1960)

A_fit2_1970 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(A_fit2_1970)

A_fit2_1980 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(A_fit2_1980)

A_fit2_1990 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(A_fit2_1990)

A_fit2_2000 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(A_fit2_2000)

A_fit2_2010 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(A_fit2_2010)

A_fit2_2015 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(A_fit2_2015)


# PANEL B: Centuries of Trade 

B_fit2_1900 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1900)
summary(B_fit2_1900)

B_fit2_1910 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(B_fit2_1910)

B_fit2_1925 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(B_fit2_1925)

B_fit2_1938 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(B_fit2_1938)

B_fit2_1950 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(B_fit2_1950)

B_fit2_1960 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(B_fit2_1960)

B_fit2_1970 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(B_fit2_1970)

B_fit2_1980 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(B_fit2_1980)

B_fit2_1990 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(B_fit2_1990)

B_fit2_2000 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(B_fit2_2000)

B_fit2_2010 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(B_fit2_2010)

B_fit2_2015 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(B_fit2_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit2_1900 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1900)
summary(C_fit2_1900)

C_fit2_1910 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1910)
summary(C_fit2_1910)

C_fit2_1925 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1925)
summary(C_fit2_1925)

C_fit2_1938 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1938)
summary(C_fit2_1938)

C_fit2_1950 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1950)
summary(C_fit2_1950)

C_fit2_1960 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1960)
summary(C_fit2_1960)

C_fit2_1970 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1970)
summary(C_fit2_1970)

C_fit2_1980 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1980)
summary(C_fit2_1980)

C_fit2_1990 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat1990)
summary(C_fit2_1990)

C_fit2_2000 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2000)
summary(C_fit2_2000)

C_fit2_2010 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2010)
summary(C_fit2_2010)

C_fit2_2015 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln,
                  data = dat2015)
summary(C_fit2_2015)



# === FIT 3 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit3_1900 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(A_fit3_1900)

A_fit3_1910 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit3_1910)

A_fit3_1925 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit3_1925)

A_fit3_1938 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit3_1938)

A_fit3_1950 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit3_1950)

A_fit3_1960 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit3_1960)

A_fit3_1970 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit3_1970)

A_fit3_1980 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit3_1980)

A_fit3_1990 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit3_1990)

A_fit3_2000 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit3_2000)

A_fit3_2010 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit3_2010)

A_fit3_2015 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit3_2015)


# PANEL B: Centuries of Trade 

B_fit3_1900 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(B_fit3_1900)

B_fit3_1910 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit3_1910)

B_fit3_1925 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit3_1925)

B_fit3_1938 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit3_1938)

B_fit3_1950 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit3_1950)

B_fit3_1960 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit3_1960)

B_fit3_1970 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit3_1970)

B_fit3_1980 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit3_1980)

B_fit3_1990 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit3_1990)

B_fit3_2000 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit3_2000)

B_fit3_2010 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit3_2010)

B_fit3_2015 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit3_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit3_1900 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(C_fit3_1900)

C_fit3_1910 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit3_1910)

C_fit3_1925 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit3_1925)

C_fit3_1938 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit3_1938)

C_fit3_1950 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit3_1950)

C_fit3_1960 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit3_1960)

C_fit3_1970 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit3_1970)

C_fit3_1980 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit3_1980)

C_fit3_1990 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit3_1990)

C_fit3_2000 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit3_2000)

C_fit3_2010 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit3_2010)

C_fit3_2015 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit3_2015)



# === FIT 4 === #
# Controls:
# NUTS dummies: country
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit4_1900 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(A_fit4_1900)

A_fit4_1910 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit4_1910)

A_fit4_1925 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit4_1925)

A_fit4_1938 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit4_1938)

A_fit4_1950 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit4_1950)

A_fit4_1960 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit4_1960)

A_fit4_1970 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit4_1970)

A_fit4_1980 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit4_1980)

A_fit4_1990 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit4_1990)

A_fit4_2000 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit4_2000)

A_fit4_2010 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit4_2010)

A_fit4_2015 <- lm(lnGDPpc ~ trade_center +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit4_2015)


# PANEL B: Centuries of Trade 

B_fit4_1900 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(B_fit4_1900)

B_fit4_1910 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit4_1910)

B_fit4_1925 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit4_1925)

B_fit4_1938 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit4_1938)

B_fit4_1950 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit4_1950)

B_fit4_1960 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit4_1960)

B_fit4_1970 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit4_1970)

B_fit4_1980 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit4_1980)

B_fit4_1990 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit4_1990)

B_fit4_2000 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit4_2000)

B_fit4_2010 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit4_2010)

B_fit4_2015 <- lm(lnGDPpc ~ trade_year +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit4_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit4_1900 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(C_fit4_1900)

C_fit4_1910 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit4_1910)

C_fit4_1925 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit4_1925)

C_fit4_1938 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit4_1938)

C_fit4_1950 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit4_1950)

C_fit4_1960 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit4_1960)

C_fit4_1970 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit4_1970)

C_fit4_1980 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit4_1980)

C_fit4_1990 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit4_1990)

C_fit4_2000 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit4_2000)

C_fit4_2010 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit4_2010)

C_fit4_2015 <- lm(lnGDPpc ~ num_trade_centers +
                    country +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit4_2015)



# === FIT 4 === #
# Controls:
# NUTS dummies: nuts_1
# Basic Geographic Controls: latitude + longitude + altitude
# Region characteristics: capital + mountain + mining + AREA_ln + agri_ln,
# Historical Region Characteristics: university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence

# PANEL A: trade center dummy

A_fit5_1900 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(A_fit5_1900)

A_fit5_1910 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(A_fit5_1910)

A_fit5_1925 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(A_fit5_1925)

A_fit5_1938 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(A_fit5_1938)

A_fit5_1950 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(A_fit5_1950)

A_fit5_1960 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(A_fit5_1960)

A_fit5_1970 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(A_fit5_1970)

A_fit5_1980 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(A_fit5_1980)

A_fit5_1990 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(A_fit5_1990)

A_fit5_2000 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(A_fit5_2000)

A_fit5_2010 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(A_fit5_2010)

A_fit5_2015 <- lm(lnGDPpc ~ trade_center +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(A_fit5_2015)


# PANEL B: Centuries of Trade 

B_fit5_1900 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(B_fit5_1900)

B_fit5_1910 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(B_fit5_1910)

B_fit5_1925 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(B_fit5_1925)

B_fit5_1938 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(B_fit5_1938)

B_fit5_1950 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(B_fit5_1950)

B_fit5_1960 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(B_fit5_1960)

B_fit5_1970 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(B_fit5_1970)

B_fit5_1980 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(B_fit5_1980)

B_fit5_1990 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(B_fit5_1990)

B_fit5_2000 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(B_fit5_2000)

B_fit5_2010 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(B_fit5_2010)

B_fit5_2015 <- lm(lnGDPpc ~ trade_year +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(B_fit5_2015)


# PANEL C: number of trade centers per NUTS-2 Region

C_fit5_1900 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1900)
summary(C_fit5_1900)

C_fit5_1910 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1910)
summary(C_fit5_1910)

C_fit5_1925 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1925)
summary(C_fit5_1925)

C_fit5_1938 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1938)
summary(C_fit5_1938)

C_fit5_1950 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1950)
summary(C_fit5_1950)

C_fit5_1960 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1960)
summary(C_fit5_1960)

C_fit5_1970 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1970)
summary(C_fit5_1970)

C_fit5_1980 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1980)
summary(C_fit5_1980)

C_fit5_1990 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat1990)
summary(C_fit5_1990)

C_fit5_2000 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2000)
summary(C_fit5_2000)

C_fit5_2010 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2010)
summary(C_fit5_2010)

C_fit5_2015 <- lm(lnGDPpc ~ num_trade_centers +
                    nuts_1 +
                    mining + mining*trade_center
                    capital + mountain + mining + AREA_ln + agri_ln +
                    university_1500 + printingpress_pre1500 + bishop + imperial + hanse + residence,
                  data = dat2015)
summary(C_fit5_2015)