# === clear the screen === #
cat("\014")

# === clear the environment === #
rm(list = ls())

# === reset graphics === #
graphics.off()

#set working directory
setwd("~/MA")

#update current packages
#update.packages()

#install and load packages via pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr)

#load raw data
wolfV6 <- read.csv2('RosesWolf_RegionalGDP_CSV_v6.csv', header = TRUE)
wahl <- read.csv('wahldata.csv')

#rename variables
names(wolfV6)[names(wolfV6) == 'Country..current.borders.'] <- 'Country'

##Wahl uses NUTS 2006, Roses-WolfV6 uses NUTS 2010
#manual changes according to NUTS-1 correspondence table
wahl$nuts_1[wahl$nuts_1 == "ITD"] <- "ITH"
wahl$nuts_1[wahl$nuts_1 == "ITE"] <- "ITI"
wahl$nuts_1[wahl$nuts_1 == "BE1"] <- "BRU"
wahl$nuts_1[wahl$nuts_1 == "BE2"] <- "BRU"
wahl$nuts_1[wahl$nuts_1 == "BE3"] <- "BRU"

#manual changes according to NUTS-2 correspondence table
wahl$nuts_2[wahl$nuts_2 == "DE41"] <- "DE40"
wahl$nuts_2[wahl$nuts_2 == "DE42"] <- "DE40"
wahl$nuts_2[wahl$nuts_2 == "DED1"] <- "DED4"
wahl$nuts_2[wahl$nuts_2 == "DED3"] <- "DED5"

wahl$nuts_2[wahl$nuts_2 == "ITD1"] <- "ITH1"
wahl$nuts_2[wahl$nuts_2 == "ITD2"] <- "ITH2"
wahl$nuts_2[wahl$nuts_2 == "ITD3"] <- "ITH3"
wahl$nuts_2[wahl$nuts_2 == "ITD4"] <- "ITH4"
wahl$nuts_2[wahl$nuts_2 == "ITD5"] <- "ITH5"
wahl$nuts_2[wahl$nuts_2 == "ITE1"] <- "ITI1"
wahl$nuts_2[wahl$nuts_2 == "ITE2"] <- "ITI2"
wahl$nuts_2[wahl$nuts_2 == "ITE3"] <- "ITI3"
wahl$nuts_2[wahl$nuts_2 == "ITE4"] <- "ITI4"

##Roses-WolfV6 additionally combines  specific regions, which prior to aggregation are manually renamed in Wahl
wahl$nuts_2[wahl$nuts_2 == "AT12"] <- "AT12+AT13"
wahl$nuts_2[wahl$nuts_2 == "AT13"] <- "AT12+AT13"
wahl$nuts_2[wahl$nuts_2 == "BE10"] <- "BE10+BE24+BE31"
wahl$nuts_2[wahl$nuts_2 == "BE24"] <- "BE10+BE24+BE31"
wahl$nuts_2[wahl$nuts_2 == "BE31"] <- "BE10+BE24+BE31"
wahl$nuts_2[wahl$nuts_2 == "DE71"] <- "DE71+DE72"
wahl$nuts_2[wahl$nuts_2 == "DE72"] <- "DE71+DE72"
wahl$nuts_2[wahl$nuts_2 == "DE91"] <- "DE91+DE92"
wahl$nuts_2[wahl$nuts_2 == "DE92"] <- "DE91+DE92"
wahl$nuts_2[wahl$nuts_2 == "ITH1"] <- "ITH1+ITH2"
wahl$nuts_2[wahl$nuts_2 == "ITH2"] <- "ITH1+ITH2"

#adjust the region code in Roses-WolfV6 (looks like a date) to the corresponding NUTS code 
wolfV6$NUTS.Codes[wolfV6$NUTS.Codes == "Dec 00"] <- "DEC0"

#list variables to keep from wahl
wahl_keep <- c(
  "nuts_1",
  "nuts_2",
  "country",
  "trade_city_final",
  "trade_city_final_4",
  "trade_city_alt",
  "capital",
  "education",
  "catholics",
  "latitude",
  "longitude",
  "mountain_region",
  "border_region",
  "university_1500",
  "coast_region",
  "printingpress_pre1500",
  "hanse_city",
  "imperial_city",
  "lnagri",
  "altitude",
  "dist_to_border",
  "dist_coast",
  "dist_trade",
  "east_germany",
  "bishop",
  "mining",
  "eqi100",
  "residence",
  "trade_year")

df_wahl_keep <- wahl[wahl_keep]

#reduce rows by overlapping countries
countries_to_keep <- Reduce(intersect,  list(v1 = unique(wolfV6$Country),  v2 = unique(wahl$country)))
df_wahl <- df_wahl_keep[ df_wahl_keep$country %in% countries_to_keep, ]
df_wolf <- wolfV6[ wolfV6$Country %in% countries_to_keep, ]

#Delete island regions Corsica, Sardinia, and Sicily not present in Wahl
df_wolf<-df_wolf[!(df_wolf$NUTS.Codes=="FR83" |
                     df_wolf$NUTS.Codes=="ITG1" |
                     df_wolf$NUTS.Codes=="ITG2"),]

# === Aggregation of NUTS3-wahl data to target level NUTS2 === #
##make custom aggregation functions available
#for binary vars
cust_bin <- function(col){
  min(sum(col), 1)
}
#for distance variables
cust_dist <- function(col){
  ifelse(min(col)==0, 0, mean(col))
}
#for mountain region variable
cust_mount <- function(col){
  ifelse(max(col)==3, 1, 0)
}
#for unique values that are already aggregated on NUTS2 or higher
cust_uni <- function(col){
  unique(col)
}
#Applied aggregation function using group by:
#aggregate df_wahl from NUTS3 to NUTS2
wahl_agg<-group_by(df_wahl, nuts_2) %>%
  summarise(
    nuts_1 = cust_uni(nuts_1),
    nuts_2 = cust_uni(nuts_2),
    country = cust_uni(country),
    trade_center = cust_bin(trade_city_final),
    trade_center_1 = cust_bin(trade_city_final_4),
    trade_center_2 = cust_bin(trade_city_alt),
    num_trade_centers = sum(trade_city_final)/n(),
    num_trade_centers_1 = sum(trade_city_final_4)/n(),
    num_trade_centers_2 = sum(trade_city_alt)/n(),
    capital = cust_bin(capital),
    education = mean(education),
    catholics = mean(catholics),
    latitude = mean(latitude),
    longitude = mean(longitude),
    mountain = cust_mount(mountain_region),
    border_region = cust_bin(border_region),
    university_1500 = cust_bin(university_1500),
    coast_region = cust_bin(coast_region),
    printingpress_pre1500 = cust_bin(printingpress_pre1500),
    hanse = cust_bin(hanse_city),
    imperial = cust_bin(imperial_city),
    agri = mean(exp(lnagri)),
    altitude = mean(altitude),
    dist_border = cust_dist(dist_to_border),
    dist_coast = cust_dist(dist_coast),
    dist_trade = cust_dist(dist_trade),
    east_germany = cust_bin(east_germany),
    bishop = cust_bin(bishop),
    mining = cust_bin(mining),
    eqi100 = mean(eqi100),
    residence = cust_bin(residence),
    trade_year = max(trade_year)
    )

#combine data from wahl and wolf into one df
df_merge<-merge(x = wahl_agg, y = df_wolf, by.x = "nuts_2", by.y = "NUTS.Codes")

#drop duplicate "Country" variable
df_merge$Country <- NULL

#make sure that vars "trade_center" and "trade_year" (i.e. centuries of trade) have the same number of non-zero entries
sum(df_merge$trade_center != 0)
sum(df_merge$trade_year != 0)

#check that vars "trade_center" and "trade_year" are available in pairs. [expect 104 TRUE statements]
for(i in 1:nrow(df_merge)){
  a=df_merge[i,which(colnames(df_merge)=="trade_center")]
  b=df_merge[i,which(colnames(df_merge)=="trade_year")]
  if(a!=0 & b==0 | a==0 & b!=0){ 
    cat(i,"FALSE","\n")}
  else {
    cat(i,"TRUE","\n")
  }
}

#save merged df to directory
write.csv(df_merge, file="data_comb.csv")
