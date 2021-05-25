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
pacman::p_load(dplyr)

# === load long-form data === #
df <- read.csv('data_nuts3_long.csv')

## growth of GDPpc (rolling annual and base period 1980)
df <- df %>% group_by(nuts_3) %>%
  mutate(delta_year = year - lag(year, default = first(year)),
         GDPpcGrowth = ((GDPpc/lag(GDPpc))/delta_year),
         GDPpc_delta1980 = GDPpc/GDPpc[1])

## NUTS3_longdata is initially cleaned from NAs. Re
df["GDPpcGrowth"][is.na(df["GDPpcGrowth"])] <- 1


## growth for subset of years after base period 1990
dt <- df %>%
  select(year, nuts_3, GDPpc) %>%
  filter(year>=1990)

dt <- dt %>% group_by(nuts_3) %>%
  mutate(GDPpc_delta1990 = (GDPpc/GDPpc[1]))


df <- left_join(df, dt, by = c('nuts_3', 'year'))

df <- df %>%
  select(-'GDPpc.y') %>%
  dplyr::rename(GDPpc = GDPpc.x)

# housekeeping
rm(dt)

# === logarithmic transformations === #
df["lnGDPpc"] <- log(df$GDPpc)
df["lnGDPpcGrowth"] <- log(df$GDPpcGrowth)
df["lncatholics"] <- log(df$catholics)
df["lndist_border"] <- log(df$dist_border)
df["lndist_coast"] <- log(df$dist_coast)
df["lndist_trade"] <- log(df$dist_trade)
df["lndist_roman"] <- log(df$dist_roman)
df["lndist_river"] <- log(df$dist_river)
df["lnagri"] <- log(df$agri)
df["lnarea"] <- log(df$area)
df["lnemp_comp"] <- log(df$emp_comp)

# === save extended long form nuts3 df to directory === #
write.csv(df, file="MA_nuts3_data.csv")
