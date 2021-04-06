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
pacman::p_load(dplyr, stringr)

# === load wide panel data === #
df <- read.csv('data_panel_long_nona.csv')

# === variable computation and transformation === #
## relative GDP density as measure of spatial industry agglomeration ##
df.countryyear <- aggregate(list(df$GDP,df$AREA), list(df$country,df$Year), FUN=sum)
colnames(df.countryyear)[1] <- "country"
colnames(df.countryyear)[2] <- "Year"
colnames(df.countryyear)[3] <- "sumGDP"
colnames(df.countryyear)[4] <- "sumAREA"

base1 <- merge(df, df.countryyear, by=c("country","Year"))

df["relden.GDP"] <- (base1$GDP/base1$sumGDP)/(base1$AREA/base1$sumAREA)

# housekeeping
rm(base1,df.countryyear)

## alternative agglomeration measure
df["popDensity"] <- df$POP/df$AREA

## GDP per capita
df["GDPpc"] <- df$GDP*1000/df$POP
df["GDP_11pc"] <- df$GDP_11*1000/df$POP

## growth of GDPpc
pct <- function(x) {x/lag(x)}

dt <- df %>% group_by(nuts_2) %>% mutate_each(funs(pct), c(GDPpc, GDP_11pc)) %>% mutate(delta_Year = Year - lag(Year, default = first(Year)))

df["delta_Year"] <- dt$delta_Year
df["GDPpcGrowth"] <- dt$GDPpc/dt$delta_Year
df["GDP_11pcGrowth"] <- dt$GDP_11pc/dt$delta_Year
df[c("GDPpcGrowth", "GDP_11pcGrowth")][is.na(df[c("GDPpcGrowth", "GDP_11pcGrowth")])] <- 1

# housekeeping
rm(pct,dt)

# growth since 1900

fct <- function(x) {x/x[1]}

dt <- df %>% group_by(nuts_2) %>% mutate_each(funs(fct), c(GDPpc, GDP_11pc))
df["GDPpc_delta1900"] <- dt$GDPpc
df["GDP_11pc_delta1900"] <- dt$GDP_11pc

# housekeeping
rm(fct,dt)

# === logarithmic transformations === #
df["GDP_ln"] <- log(df$GDP)
df["GDP_11_ln"] <- log(df$GDP_11)
df["lnGDPpc"] <- log(df$GDPpc)
df["lnGDP_11pc"] <- log(df$GDP_11pc)
df["lnGDPpcGrowth"] <- log(df$GDPpcGrowth)
df["lnGDP_11pcGrowth"] <- log(df$GDP_11pcGrowth)
df["POP_ln"] <- log(df$POP)
df["AREA_ln"] <- log(df$AREA)
df["ln.relden.GDP"] <- log(df$relden.GDP)
df["ln.popDensity"] <- log(df$popDensity)
df["education_ln"] <- log(df$education)
df["catholics_ln"] <- log(df$catholics)
df["dist_border_ln"] <- log(df$dist_border)
df["dist_coast_ln"] <- log(df$dist_coast)
df["dist_trade_ln"] <- log(df$dist_trade)
df["eqi100_ln"] <- log(df$eqi100)
df["agri_ln"] <- log(df$agri)
df["AGSH_ln"] <- log(df$AGSH)
df["SERSH_ln"] <- log(df$SERSH)
df["INDSH_ln"] <- log(df$INDSH)

# === save panel data frames to directory === #
write.csv(df, file="MA_data.csv")