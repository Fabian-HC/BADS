#set working directory
setwd("~/MA")

#clear workspace
rm(list=ls())

#update current packages
#update.packages()

#install and load packages via pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, eurostat, sf, maps, tmap, compare, ggplot2)

# === Load data === #

# Shapefile
RW_sf <- read_sf('~/MA/RosesWolf_RegionalGDP_shapefile/regions_nuts2.shp')
#alternate from GISCO [commented out]: a sf at resolution 1:60  from year  2010
#geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "2", year = 2010)
RW_sf$NUTS_CODE[RW_sf$NUTS_CODE == "AT123"] <- "AT12+AT13"
RW_sf$NUTS_CODE[RW_sf$NUTS_CODE == "DE912"] <- "DE91+DE92"
RW_sf$NUTS_CODE[RW_sf$NUTS_CODE == "DE712"] <- "DE71+DE72"
# Attribute Data
df <- read.csv('MA_nuts2_data.csv')

# === Applicability === #
# Check overlap of attribute data with shapefile
intersect(df$nuts_2,RW_sf$NUTS_CODE)
setdiff(df$nuts_2,RW_sf$NUTS_CODE)
setdiff(RW_sf$NUTS_CODE,df$nuts_2)

# === Specification of attribute data for the plot === #
dat <- df %>% 
  select(nuts_2, GDP_11pc, Year) %>%
  filter(Year == 2015) %>%
  mutate(cat = cut_to_classes(GDP_11pc, style = "quantile"))

# merge attribute data with geodata
map_data <- inner_join(RW_sf, dat, by = c("NUTS_CODE" = "nuts_2"))

# === Plot Map === #
ggplot(map_data) + geom_sf(aes(fill=cat),color="dim grey", size=.1) + 
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "International Dollars")) +
  labs(title="GDPpc",
       caption="Map of regions in Quantiles by GDPpc") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-10,30), ylim=c(35,60))


dat2 <- df %>% 
  select(nuts_2, trade_center, Year) %>%
  filter(Year == 2015) %>%
  mutate(cat2=as.factor(trade_center))
# merge attribute data with geodata
map_data2 <- inner_join(RW_sf, dat2, by = c("NUTS_CODE" = "nuts_2"))

# === Plot Map === #
ggplot(map_data2) + geom_sf(aes(fill=cat2),color="dim grey", size=.1) + 
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "trade_center")) +
  labs(title="Trade Center Regions",
       caption="Map of regions by trade_center") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-10,30), ylim=c(35,60))

dat3 <- df %>% 
  select(nuts_2, trade_center_2, Year) %>%
  filter(Year == 2015) %>%
  mutate(cat2=as.factor(trade_center_2))
# merge attribute data with geodata
map_data3 <- inner_join(RW_sf, dat3, by = c("NUTS_CODE" = "nuts_2"))

# === Plot Map === #
ggplot(map_data3) + geom_sf(aes(fill=cat2),color="dim grey", size=.1) + 
  scale_fill_brewer(palette = "Greens") +
  guides(fill = guide_legend(reverse=T, title = "trade_center_2")) +
  labs(title="Trade Center 2 Regions",
       caption="Map of regions by trade_center") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-10,30), ylim=c(35,60))

dat4 <- df %>% 
  select(nuts_2, trade_center_1, Year) %>%
  filter(Year == 2015) %>%
  mutate(cat2=as.factor(trade_center_1))
# merge attribute data with geodata
map_data4 <- inner_join(RW_sf, dat4, by = c("NUTS_CODE" = "nuts_2"))

# === Plot Map === #
ggplot(map_data4) + geom_sf(aes(fill=cat2),color="dim grey", size=.1) + 
  scale_fill_brewer(palette = "Blues") +
  guides(fill = guide_legend(reverse=T, title = "trade_center_1")) +
  labs(title="Trade Center 1 Regions",
       caption="Map of regions by trade_center") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-10,30), ylim=c(35,60))

# For all interactions between mining & trade_center including DiffDiff
dat5 <- df %>% 
  select(nuts_2, mining, trade_center, Year) %>%
  filter(Year == 2010) %>%
  mutate(mining_trade = mining*trade_center)
# merge attribute data with geodata
map_data5 <- inner_join(RW_sf, dat5, by = c("NUTS_CODE" = "nuts_2"))

# === Plot Map === #
ggplot(map_data5) + geom_sf(aes(fill=factor(mining_trade)),color="dim grey", size=.1) + 
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "mining_trade")) +
  labs(title="Mining Regions",
       caption="Map of regions by mining") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-10,30), ylim=c(35,60))