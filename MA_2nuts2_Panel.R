# === clear the screen === #
cat("\014")

# === clear the environment === #
rm(list = ls())

# === reset graphics === #
graphics.off()

# === set working directory === #
setwd("~/MA")

# === unload all packages except for base === #
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

# === install and load required packages via pacman === #
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, plm)

# === update current packages === #
#update.packages()

# === AGENDA === #
# Panel regression modification. Prof. Wolf requests one model to be used per fit. With long data, apply a fixed effects (balanced Panel) Model that allows inference on the development of the estimate for the trade indicators over time.
# 

# === load data === #
df <- read.csv('MA_nuts2_data.csv')
df <- df %>% filter(nuts_2 != 'NL23')
df$nuts_2 <- as.factor(df$nuts_2)
df$Year <- as.factor(df$Year)
dt <- model.matrix(~ Year + 0 , data = df)
df <- cbind(df,dt)
rm(dt)
as.numeric(levels(df$Year))[df$Year]

lsdvA_fit1 <- lm(lnGDPpc ~ trade_center*Year1900 + trade_center*Year1910 + trade_center*Year1925+
                    trade_center*Year1938 + trade_center*Year1950 + trade_center*Year1960+ 
                    trade_center*Year1970 + trade_center*Year1980 + trade_center*Year1990+
                    trade_center*Year2000 + trade_center*Year2010 + trade_center*Year2015,
               data = df)
summary(lsdvA_fit1)

test <- lm(lnGDPpc ~ (trade_center*Year),
                  data = df)

test2 <- plm(formula = lnGDPpc ~ (trade_center * Year), data = df, effect = "twoways", 
    model = "within")

summary(test2)

alias(lsdvA_fit1)

unique(df$Year)
