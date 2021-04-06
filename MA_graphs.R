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
pacman::p_load(shiny, ExPanDaR, ggplot2, dineq, kableExtra, dplyr)

# === load data === #
df <- read.csv('MA_data.csv')

ggplot(aes(x=Year,y=GDPpc), data = df) + geom_point(alpha = 1/5, color = "orange") +
  geom_line(stat = "summary", fun = mean) +
  geom_line(stat = "summary", fun = quantile,
            fun.args = list(probs = .1), linetype = 2, color = 'blue') +
  geom_line(stat = "summary", fun = quantile,
            fun.args = list(probs = .5), color = 'blue') +
  geom_line(stat = "summary", fun = quantile,
            fun.args = list(probs = .9), linetype = 2, color = 'blue') +
  coord_trans(y = "sqrt") +
  facet_wrap(~trade_center, scales='free_y')

ggplot(df, aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(df, aes(as.character(trade_center), SERSH)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(df, aes(as.character(trade_center), AGSH)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(df, aes(as.character(trade_center), INDSH)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="Germany"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="France"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="Austria"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="Netherlands"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="Belgium"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y')

ggplot(subset(df,country=="Italy"), aes(as.character(trade_center), GDPpc)) + 
  geom_boxplot() +
  facet_wrap(~Year, scales='free_y') +
  stat_summary(fun=mean, colour="blue", geom="point", size=1, show.legend = FALSE) +
  stat_summary(fun=mean, colour="blue", geom="text", show.legend = FALSE, 
               vjust=-.3, aes(label=round(..y.., digits=0)))

