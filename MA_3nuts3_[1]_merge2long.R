# README: THIS SCRIPT MERGES CONTEMPORARY EUROSTAT PER CAPITA GDP DATA [1980-2021] WITH F.WAHL'S TRADE DATA CONTROLS.
# === clear the screen === #
cat("\014")

# === clear the environment === #
rm(list = ls())

# === reset graphics === #
graphics.off()

# === set working directory === #
setwd("~/MA")

# === packages/libraries === #
# update.packages()

# install and load packages via pacman
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr, ExPanDaR)

# === load input data === #
eustat <- read.csv2('data_nuts3_eustat.csv', header = TRUE)
wahl <- read.csv('data_wahl.csv')

# === combine data from wahl and eustat into one wide-form df === #
df_1 <- merge(x = wahl, y = eustat, by.x = "nuts_3", by.y = "NUTS_ID")

# --> 586 NUTS3 regions match following this first df_1 merge attempt.

c1 <- eustat %>%
  filter(STAT_LEVL_CODE == 0) %>%
  mutate(Country = NUTS_NAME) %>%
  select(NUTS_ID, Country)

eustat$title <- gsub("^([A-Z]{2}).+$", "\\1", eustat$NUTS_ID)

c2 <- merge(x = eustat, y = c1, by.x='title', by = 'NUTS_ID')

# === NUTS correspondence tables === #
## Wahl uses NUTS 2006, eustat data is based on NUTS 2016
## Most adjustments are 1:1 (ID changes) and thereby inconsequential
## Else, i.e..
# i. boundry-shifts:
# recalculations of region borders are assumed to be marginal and abstracted from.
# ii. region mergers:
# Aggregation applied on NUTS3 level
# iii. region splits:
# least frequent case when regions are dropped

# NUTS2006 --> NUTS2010
wahl$nuts_3[wahl$nuts_3 == "DE411"] <- "DE403"
wahl$nuts_3[wahl$nuts_3 == "DE412"] <- "DE405"
wahl$nuts_3[wahl$nuts_3 == "DE413"] <- "DE409"
wahl$nuts_3[wahl$nuts_3 == "DE414"] <- "DE40A"
wahl$nuts_3[wahl$nuts_3 == "DE415"] <- "DE40C"
wahl$nuts_3[wahl$nuts_3 == "DE416"] <- "DE40D"
wahl$nuts_3[wahl$nuts_3 == "DE417"] <- "DE40F"
wahl$nuts_3[wahl$nuts_3 == "DE418"] <- "DE40I"
wahl$nuts_3[wahl$nuts_3 == "DE421"] <- "DE401"
wahl$nuts_3[wahl$nuts_3 == "DE422"] <- "DE402"
wahl$nuts_3[wahl$nuts_3 == "DE423"] <- "DE404"
wahl$nuts_3[wahl$nuts_3 == "DE424"] <- "DE406"
wahl$nuts_3[wahl$nuts_3 == "DE425"] <- "DE407"
wahl$nuts_3[wahl$nuts_3 == "DE426"] <- "DE408"
wahl$nuts_3[wahl$nuts_3 == "DE427"] <- "DE40B"
wahl$nuts_3[wahl$nuts_3 == "DE428"] <- "DE40E"
wahl$nuts_3[wahl$nuts_3 == "DE429"] <- "DE40G"
wahl$nuts_3[wahl$nuts_3 == "DE42A"] <- "DE40H"
wahl$nuts_3[wahl$nuts_3 == "DED11"] <- "DED41"
wahl$nuts_3[wahl$nuts_3 == "DED31"] <- "DED51"
wahl$nuts_3[wahl$nuts_3 == "DEA21"] <- "DEA2D"
wahl$nuts_3[wahl$nuts_3 == "DEA25"] <- "DEA2D"
wahl$nuts_3[wahl$nuts_3 == "DED23"] <- "DED2C"
wahl$nuts_3[wahl$nuts_3 == "DED24"] <- "DED2C"
wahl$nuts_3[wahl$nuts_3 == "DED2B"] <- "DED2C"
wahl$nuts_3[wahl$nuts_3 == "DED22"] <- "DED2D"
wahl$nuts_3[wahl$nuts_3 == "DED26"] <- "DED2D"
wahl$nuts_3[wahl$nuts_3 == "DED28"] <- "DED2D"
wahl$nuts_3[wahl$nuts_3 == "DED25"] <- "DED2E"
wahl$nuts_3[wahl$nuts_3 == "DED27"] <- "DED2E"
wahl$nuts_3[wahl$nuts_3 == "DED29"] <- "DED2F"
wahl$nuts_3[wahl$nuts_3 == "DED2A"] <- "DED2F"
wahl$nuts_3[wahl$nuts_3 == "DED14"] <- "DED42"
wahl$nuts_3[wahl$nuts_3 == "DED18"] <- "DED42"
wahl$nuts_3[wahl$nuts_3 == "DED1A"] <- "DED42"
wahl$nuts_3[wahl$nuts_3 == "DED1B"] <- "DED42"
wahl$nuts_3[wahl$nuts_3 == "DED16"] <- "DED43"
wahl$nuts_3[wahl$nuts_3 == "DED19"] <- "DED43"
wahl$nuts_3[wahl$nuts_3 == "DED33"] <- "DED43"

wahl$nuts_2[wahl$nuts_3 == "DED43" & wahl$nuts_2 == "DED3"] <- "DED1"

wahl$nuts_3[wahl$nuts_3 == "DED12"] <- "DED44"
wahl$nuts_3[wahl$nuts_3 == "DED17"] <- "DED44"
wahl$nuts_3[wahl$nuts_3 == "DED13"] <- "DED45"
wahl$nuts_3[wahl$nuts_3 == "DED15"] <- "DED45"
wahl$nuts_3[wahl$nuts_3 == "DED1C"] <- "DED45"
wahl$nuts_3[wahl$nuts_3 == "DED34"] <- "DED52"
wahl$nuts_3[wahl$nuts_3 == "DED35"] <- "DED52"
wahl$nuts_3[wahl$nuts_3 == "DED32"] <- "DED53"
wahl$nuts_3[wahl$nuts_3 == "DED36"] <- "DED53"

wahl$nuts_3[wahl$nuts_3 == "ITD10"] <- "ITH10"
wahl$nuts_3[wahl$nuts_3 == "ITD20"] <- "ITH20"
wahl$nuts_3[wahl$nuts_3 == "ITD31"] <- "ITH31"
wahl$nuts_3[wahl$nuts_3 == "ITD32"] <- "ITH32"
wahl$nuts_3[wahl$nuts_3 == "ITD33"] <- "ITH33"
wahl$nuts_3[wahl$nuts_3 == "ITD34"] <- "ITH34"
wahl$nuts_3[wahl$nuts_3 == "ITD35"] <- "ITH35"
wahl$nuts_3[wahl$nuts_3 == "ITD36"] <- "ITH36"
wahl$nuts_3[wahl$nuts_3 == "ITD37"] <- "ITH37"
wahl$nuts_3[wahl$nuts_3 == "ITD41"] <- "ITH41"
wahl$nuts_3[wahl$nuts_3 == "ITD42"] <- "ITH42"
wahl$nuts_3[wahl$nuts_3 == "ITD43"] <- "ITH43"
wahl$nuts_3[wahl$nuts_3 == "ITD44"] <- "ITH44"
wahl$nuts_3[wahl$nuts_3 == "ITD51"] <- "ITH51"
wahl$nuts_3[wahl$nuts_3 == "ITD52"] <- "ITH52"
wahl$nuts_3[wahl$nuts_3 == "ITD53"] <- "ITH53"
wahl$nuts_3[wahl$nuts_3 == "ITD54"] <- "ITH54"
wahl$nuts_3[wahl$nuts_3 == "ITD55"] <- "ITH55"
wahl$nuts_3[wahl$nuts_3 == "ITD56"] <- "ITH56"
wahl$nuts_3[wahl$nuts_3 == "ITD57"] <- "ITH57"
wahl$nuts_3[wahl$nuts_3 == "ITD58"] <- "ITH58"
wahl$nuts_3[wahl$nuts_3 == "ITE11"] <- "ITI11"
wahl$nuts_3[wahl$nuts_3 == "ITE12"] <- "ITI12"
wahl$nuts_3[wahl$nuts_3 == "ITE13"] <- "ITI13"
wahl$nuts_3[wahl$nuts_3 == "ITE14"] <- "ITI14"
wahl$nuts_3[wahl$nuts_3 == "ITE15"] <- "ITI15"
wahl$nuts_3[wahl$nuts_3 == "ITE16"] <- "ITI16"
wahl$nuts_3[wahl$nuts_3 == "ITE17"] <- "ITI17"
wahl$nuts_3[wahl$nuts_3 == "ITE18"] <- "ITI18"
wahl$nuts_3[wahl$nuts_3 == "ITE19"] <- "ITI19"
wahl$nuts_3[wahl$nuts_3 == "ITE1A"] <- "ITI1A"
wahl$nuts_3[wahl$nuts_3 == "ITE21"] <- "ITI21"
wahl$nuts_3[wahl$nuts_3 == "ITE22"] <- "ITI22"
wahl$nuts_3[wahl$nuts_3 == "ITE32"] <- "ITI32"
wahl$nuts_3[wahl$nuts_3 == "ITE33"] <- "ITI33"
wahl$nuts_3[wahl$nuts_3 == "ITE41"] <- "ITI41"
wahl$nuts_3[wahl$nuts_3 == "ITE42"] <- "ITI42"
wahl$nuts_3[wahl$nuts_3 == "ITE43"] <- "ITI43"
wahl$nuts_3[wahl$nuts_3 == "ITE44"] <- "ITI44"
wahl$nuts_3[wahl$nuts_3 == "ITE45"] <- "ITI45"

## Recalculations involving data loss
# Milano split from ITC45 to ITC4C "Milano" (kept) + ITC4D "Monza e della Brianza" (dropped)
wahl$nuts_3[wahl$nuts_3 == "ITC45"] <- "ITC4C"

wahl$nuts_3[wahl$nuts_3 == "ITD59"] <- "ITH59"

wahl$nuts_3[wahl$nuts_3 == "ITE31"] <- "ITI31"

# Ascoli Piceno split from ITE34 to ITI34 "Ascoli Piceno" (kept) + ITI35 "Fermo" (dropped)
wahl$nuts_3[wahl$nuts_3 == "ITE34"] <- "ITI34"

# ITF41, ITF42 terminated. Converted to imperfect matches, dropping ITF48 "Barletta-Andria-Trani"
wahl$nuts_3[wahl$nuts_3 == "ITF41"] <- "ITF46"
wahl$nuts_3[wahl$nuts_3 == "ITF42"] <- "ITF47"

wahl$nuts_3[wahl$nuts_3 == "NL331"] <- "NL337"
wahl$nuts_3[wahl$nuts_3 == "NL334"] <- "NL338"
wahl$nuts_3[wahl$nuts_3 == "NL335"] <- "NL339"
wahl$nuts_3[wahl$nuts_3 == "NL336"] <- "NL33A"

# NUTS2010 --> NUTS2013 [ data loss below follows above logic - see Correspondence Tables for full descriptions ]

wahl$nuts_3[wahl$nuts_3 == "DE801"] <- "DE80N"
wahl$nuts_3[wahl$nuts_3 == "DE808"] <- "DE80J"
wahl$nuts_3[wahl$nuts_3 == "DE80B"] <- "DE80J"
wahl$nuts_3[wahl$nuts_3 == "DE80C"] <- "DE80J"
wahl$nuts_3[wahl$nuts_3 == "DE80F"] <- "DE80N"
wahl$nuts_3[wahl$nuts_3 == "DE80I"] <- "DE80N"
wahl$nuts_3[wahl$nuts_3 == "DE802"] <- "DE80J"
wahl$nuts_3[wahl$nuts_3 == "DE807"] <- "DE80K"
wahl$nuts_3[wahl$nuts_3 == "DE809"] <- "DE80K"
wahl$nuts_3[wahl$nuts_3 == "DE805"] <- "DE80L"
wahl$nuts_3[wahl$nuts_3 == "DE80D"] <- "DE80L"
wahl$nuts_3[wahl$nuts_3 == "DE80H"] <- "DE80L"
wahl$nuts_3[wahl$nuts_3 == "DE806"] <- "DE80M"
wahl$nuts_3[wahl$nuts_3 == "DE80E"] <- "DE80M"
wahl$nuts_3[wahl$nuts_3 == "DE80A"] <- "DE80O"
wahl$nuts_3[wahl$nuts_3 == "DE80G"] <- "DE80O"

wahl$nuts_3[wahl$nuts_3 == "PL121"] <- "PL12B"
wahl$nuts_3[wahl$nuts_3 == "PL122"] <- "PL12D"
wahl$nuts_3[wahl$nuts_3 == "PL215"] <- "PL218"
wahl$nuts_3[wahl$nuts_3 == "PL216"] <- "PL21A"
wahl$nuts_3[wahl$nuts_3 == "PL422"] <- "PL426"
wahl$nuts_3[wahl$nuts_3 == "PL423"] <- "PL427"
wahl$nuts_3[wahl$nuts_3 == "PL425"] <- "PL428"
wahl$nuts_3[wahl$nuts_3 == "PL521"] <- "PL523"
wahl$nuts_3[wahl$nuts_3 == "PL522"] <- "PL524"
wahl$nuts_3[wahl$nuts_3 == "PL614"] <- "PL616"
wahl$nuts_3[wahl$nuts_3 == "PL615"] <- "PL619"
wahl$nuts_3[wahl$nuts_3 == "PL631"] <- "PL636"
wahl$nuts_3[wahl$nuts_3 == "PL635"] <- "PL638"

# NUTS2013 --> NUTS2016

wahl$nuts_3[wahl$nuts_3 == "DE915"] <- "DE91C"
wahl$nuts_3[wahl$nuts_3 == "DE919"] <- "DE91C"
wahl$nuts_3[wahl$nuts_3 == "DEB16"] <- "DEB1C"
wahl$nuts_3[wahl$nuts_3 == "DEB19"] <- "DEB1D"
wahl$nuts_3[wahl$nuts_3 == "FR241"] <- "FRB01"
wahl$nuts_3[wahl$nuts_3 == "FR242"] <- "FRB02"
wahl$nuts_3[wahl$nuts_3 == "FR243"] <- "FRB03"
wahl$nuts_3[wahl$nuts_3 == "FR244"] <- "FRB04"
wahl$nuts_3[wahl$nuts_3 == "FR245"] <- "FRB05"
wahl$nuts_3[wahl$nuts_3 == "FR246"] <- "FRB06"
wahl$nuts_3[wahl$nuts_3 == "FR261"] <- "FRC11"
wahl$nuts_3[wahl$nuts_3 == "FR262"] <- "FRC12"
wahl$nuts_3[wahl$nuts_3 == "FR263"] <- "FRC13"
wahl$nuts_3[wahl$nuts_3 == "FR264"] <- "FRC14"
wahl$nuts_3[wahl$nuts_3 == "FR431"] <- "FRC21"
wahl$nuts_3[wahl$nuts_3 == "FR432"] <- "FRC22"
wahl$nuts_3[wahl$nuts_3 == "FR433"] <- "FRC23"
wahl$nuts_3[wahl$nuts_3 == "FR434"] <- "FRC24"
wahl$nuts_3[wahl$nuts_3 == "FR251"] <- "FRD11"
wahl$nuts_3[wahl$nuts_3 == "FR252"] <- "FRD12"
wahl$nuts_3[wahl$nuts_3 == "FR253"] <- "FRD13"
wahl$nuts_3[wahl$nuts_3 == "FR231"] <- "FRD21"
wahl$nuts_3[wahl$nuts_3 == "FR232"] <- "FRD22"
wahl$nuts_3[wahl$nuts_3 == "FR301"] <- "FRE11"
wahl$nuts_3[wahl$nuts_3 == "FR302"] <- "FRE12"
wahl$nuts_3[wahl$nuts_3 == "FR221"] <- "FRE21"
wahl$nuts_3[wahl$nuts_3 == "FR222"] <- "FRE22"
wahl$nuts_3[wahl$nuts_3 == "FR223"] <- "FRE23"
wahl$nuts_3[wahl$nuts_3 == "FR421"] <- "FRF11"
wahl$nuts_3[wahl$nuts_3 == "FR422"] <- "FRF12"
wahl$nuts_3[wahl$nuts_3 == "FR211"] <- "FRF21"
wahl$nuts_3[wahl$nuts_3 == "FR212"] <- "FRF22"
wahl$nuts_3[wahl$nuts_3 == "FR213"] <- "FRF23"
wahl$nuts_3[wahl$nuts_3 == "FR214"] <- "FRF24"
wahl$nuts_3[wahl$nuts_3 == "FR411"] <- "FRF31"
wahl$nuts_3[wahl$nuts_3 == "FR412"] <- "FRF32"
wahl$nuts_3[wahl$nuts_3 == "FR413"] <- "FRF33"
wahl$nuts_3[wahl$nuts_3 == "FR414"] <- "FRF34"
wahl$nuts_3[wahl$nuts_3 == "FR511"] <- "FRG01"
wahl$nuts_3[wahl$nuts_3 == "FR512"] <- "FRG02"
wahl$nuts_3[wahl$nuts_3 == "FR513"] <- "FRG03"
wahl$nuts_3[wahl$nuts_3 == "FR514"] <- "FRG04"
wahl$nuts_3[wahl$nuts_3 == "FR515"] <- "FRG05"
wahl$nuts_3[wahl$nuts_3 == "FR521"] <- "FRH01"
wahl$nuts_3[wahl$nuts_3 == "FR522"] <- "FRH02"
wahl$nuts_3[wahl$nuts_3 == "FR523"] <- "FRH03"
wahl$nuts_3[wahl$nuts_3 == "FR524"] <- "FRH04"
wahl$nuts_3[wahl$nuts_3 == "FR611"] <- "FRI11"
wahl$nuts_3[wahl$nuts_3 == "FR612"] <- "FRI12"
wahl$nuts_3[wahl$nuts_3 == "FR613"] <- "FRI13"
wahl$nuts_3[wahl$nuts_3 == "FR614"] <- "FRI14"
wahl$nuts_3[wahl$nuts_3 == "FR615"] <- "FRI15"
wahl$nuts_3[wahl$nuts_3 == "FR631"] <- "FRI21"
wahl$nuts_3[wahl$nuts_3 == "FR632"] <- "FRI22"
wahl$nuts_3[wahl$nuts_3 == "FR633"] <- "FRI23"
wahl$nuts_3[wahl$nuts_3 == "FR531"] <- "FRI31"
wahl$nuts_3[wahl$nuts_3 == "FR532"] <- "FRI32"
wahl$nuts_3[wahl$nuts_3 == "FR533"] <- "FRI33"
wahl$nuts_3[wahl$nuts_3 == "FR534"] <- "FRI34"
wahl$nuts_3[wahl$nuts_3 == "FR811"] <- "FRJ11"
wahl$nuts_3[wahl$nuts_3 == "FR812"] <- "FRJ12"
wahl$nuts_3[wahl$nuts_3 == "FR813"] <- "FRJ13"
wahl$nuts_3[wahl$nuts_3 == "FR814"] <- "FRJ14"
wahl$nuts_3[wahl$nuts_3 == "FR815"] <- "FRJ15"
wahl$nuts_3[wahl$nuts_3 == "FR621"] <- "FRJ21"
wahl$nuts_3[wahl$nuts_3 == "FR622"] <- "FRJ22"
wahl$nuts_3[wahl$nuts_3 == "FR623"] <- "FRJ23"
wahl$nuts_3[wahl$nuts_3 == "FR624"] <- "FRJ24"
wahl$nuts_3[wahl$nuts_3 == "FR625"] <- "FRJ25"
wahl$nuts_3[wahl$nuts_3 == "FR626"] <- "FRJ26"
wahl$nuts_3[wahl$nuts_3 == "FR627"] <- "FRJ27"
wahl$nuts_3[wahl$nuts_3 == "FR628"] <- "FRJ28"
wahl$nuts_3[wahl$nuts_3 == "FR721"] <- "FRK11"
wahl$nuts_3[wahl$nuts_3 == "FR722"] <- "FRK12"
wahl$nuts_3[wahl$nuts_3 == "FR723"] <- "FRK13"
wahl$nuts_3[wahl$nuts_3 == "FR724"] <- "FRK14"
wahl$nuts_3[wahl$nuts_3 == "FR711"] <- "FRK21"
wahl$nuts_3[wahl$nuts_3 == "FR712"] <- "FRK22"
wahl$nuts_3[wahl$nuts_3 == "FR713"] <- "FRK23"
wahl$nuts_3[wahl$nuts_3 == "FR714"] <- "FRK24"
wahl$nuts_3[wahl$nuts_3 == "FR715"] <- "FRK25"
wahl$nuts_3[wahl$nuts_3 == "FR716"] <- "FRK26"
wahl$nuts_3[wahl$nuts_3 == "FR717"] <- "FRK27"
wahl$nuts_3[wahl$nuts_3 == "FR718"] <- "FRK28"
wahl$nuts_3[wahl$nuts_3 == "FR821"] <- "FRL01"
wahl$nuts_3[wahl$nuts_3 == "FR822"] <- "FRL02"
wahl$nuts_3[wahl$nuts_3 == "FR823"] <- "FRL03"
wahl$nuts_3[wahl$nuts_3 == "FR824"] <- "FRL04"
wahl$nuts_3[wahl$nuts_3 == "FR825"] <- "FRL05"
wahl$nuts_3[wahl$nuts_3 == "FR826"] <- "FRL06"

wahl$nuts_3[wahl$nuts_3 == "LT00A"] <- "LT011"
wahl$nuts_3[wahl$nuts_3 == "LT001"] <- "LT021"
wahl$nuts_3[wahl$nuts_3 == "LT002"] <- "LT022"
wahl$nuts_3[wahl$nuts_3 == "LT003"] <- "LT023"
wahl$nuts_3[wahl$nuts_3 == "LT004"] <- "LT024"
wahl$nuts_3[wahl$nuts_3 == "LT005"] <- "LT025"
wahl$nuts_3[wahl$nuts_3 == "LT006"] <- "LT026"
wahl$nuts_3[wahl$nuts_3 == "LT007"] <- "LT027"
wahl$nuts_3[wahl$nuts_3 == "LT008"] <- "LT028"
wahl$nuts_3[wahl$nuts_3 == "LT009"] <- "LT029"

wahl$nuts_3[wahl$nuts_3 == "HU101"] <- "HU110"
wahl$nuts_3[wahl$nuts_3 == "HU102"] <- "HU120"

wahl$nuts_3[wahl$nuts_3 == "NL121"] <- "NL124"
wahl$nuts_3[wahl$nuts_3 == "NL122"] <- "NL125"
wahl$nuts_3[wahl$nuts_3 == "NL123"] <- "NL126"
wahl$nuts_3[wahl$nuts_3 == "NL322"] <- "NL328"
wahl$nuts_3[wahl$nuts_3 == "NL326"] <- "NL329"
wahl$nuts_3[wahl$nuts_3 == "NL338"] <- "NL33B"
wahl$nuts_3[wahl$nuts_3 == "NL339"] <- "NL33C"

wahl$nuts_3[wahl$nuts_3 == "PL113"] <- "PL711"
wahl$nuts_3[wahl$nuts_3 == "PL114"] <- "PL712"
wahl$nuts_3[wahl$nuts_3 == "PL115"] <- "PL713"
wahl$nuts_3[wahl$nuts_3 == "PL116"] <- "PL714"
wahl$nuts_3[wahl$nuts_3 == "PL117"] <- "PL715"
wahl$nuts_3[wahl$nuts_3 == "PL331"] <- "PL721"
wahl$nuts_3[wahl$nuts_3 == "PL332"] <- "PL722"
wahl$nuts_3[wahl$nuts_3 == "PL311"] <- "PL811"
wahl$nuts_3[wahl$nuts_3 == "PL312"] <- "PL812"
wahl$nuts_3[wahl$nuts_3 == "PL313"] <- "PL813"
wahl$nuts_3[wahl$nuts_3 == "PL314"] <- "PL814"
wahl$nuts_3[wahl$nuts_3 == "PL315"] <- "PL815"
wahl$nuts_3[wahl$nuts_3 == "PL323"] <- "PL821"
wahl$nuts_3[wahl$nuts_3 == "PL324"] <- "PL822"
wahl$nuts_3[wahl$nuts_3 == "PL325"] <- "PL823"
wahl$nuts_3[wahl$nuts_3 == "PL326"] <- "PL824"
wahl$nuts_3[wahl$nuts_3 == "PL343"] <- "PL841"
wahl$nuts_3[wahl$nuts_3 == "PL344"] <- "PL842"
wahl$nuts_3[wahl$nuts_3 == "PL345"] <- "PL843"
wahl$nuts_3[wahl$nuts_3 == "PL127"] <- "PL911"
wahl$nuts_3[wahl$nuts_3 == "PL129"] <- "PL912"
wahl$nuts_3[wahl$nuts_3 == "PL12A"] <- "PL913"
wahl$nuts_3[wahl$nuts_3 == "PL128"] <- "PL921"
wahl$nuts_3[wahl$nuts_3 == "PL12B"] <- "PL922"
wahl$nuts_3[wahl$nuts_3 == "PL12C"] <- "PL923"
wahl$nuts_3[wahl$nuts_3 == "PL12D"] <- "PL924"
wahl$nuts_3[wahl$nuts_3 == "PL12E"] <- "PL925"

# === Reduce data by countries available in both datasets === #
countries_to_keep <- Reduce(intersect,  list(v1 = unique(c2$Country),  v2 = unique(wahl$country)))
df_wahl <- wahl[ wahl$country %in% countries_to_keep, ]
df_eustat <- c2[ c2$Country %in% countries_to_keep, ]
df_eustat <- filter(df_eustat, STAT_LEVL_CODE == 3)

# === Aggregate Wahl data to accommodate merged NUTS3 regions === #
## make custom aggregation functions available
#for binary vars
cust_bin <- function(col){
  min(sum(col), 1)
}
#for distance variables
cust_dist <- function(col){
  ifelse(min(col)==0, 0, mean(col))
}
#for mountain-region variable
cust_mount <- function(col){
  ifelse(max(col)==3, 1, 0)
}
#for unique values that are already aggregated on NUTS2 or higher
cust_uni <- function(col){
  unique(col)
}

wahl_agg<-group_by(df_wahl, nuts_3) %>%
  summarise(
    nuts_1 = cust_uni(nuts_1),
    nuts_2 = cust_uni(nuts_2),
    nuts_3 = cust_uni(nuts_3),
    country = cust_uni(country),
    trade_center = cust_bin(trade_city_final),
    trade_center_1 = cust_bin(trade_city_final_4),
    trade_center_2 = cust_bin(trade_city_alt),
    num_trade_centers = sum(trade_city_final)/n(),
    num_trade_centers_1 = sum(trade_city_final_4)/n(),
    num_trade_centers_2 = sum(trade_city_alt)/n(),
    trade_centers_abs = sum(trade_city_final),
    trade_centers_abs1 = sum(trade_city_final_4),
    trade_centers_abs2 = sum(trade_city_alt),
    neighborgdp = mean(neighborgdp),
    tradeneighbor = sum(tradeneighbor),
    tradeneighbor2 = sum(tradeneighbor2),
    tradeneighbor3 = sum(tradeneighbor3),
    tradeneighbor4 = sum(tradeneighbor4),
    capital = cust_bin(capital),
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
    imperialroad = cust_bin(imperialroad),
    agri = mean(exp(lnagri)),
    area = mean(exp(lnarea)),
    altitude = mean(altitude),
    dist_border = cust_dist(dist_to_border),
    dist_coast = cust_dist(dist_coast),
    dist_river = mean(exp(lndist_river)),
    dist_trade = cust_dist(dist_trade),
    dist_roman = mean(exp(lndist_roman)),
    east_germany = cust_bin(east_germany),
    bishop = cust_bin(bishop),
    mining = cust_bin(mining),
    eqi100 = mean(eqi100),
    residence = cust_bin(residence),
    inequality2 = mean(inequality2),
    emp_comp = mean(exp(lnemp_comp)),
    ln_tc_dist = log(mean(exp(lndist_trade_city1))),
    trade_years = max(trade_year)
  )

# === combine data from wahl and eustat into one wide-form df === #
df_merge <- merge(x = wahl_agg, y = df_eustat, by.x = "nuts_3", by.y = "NUTS_ID")

## Check that there remain no NUTS3 duplicates [success.] 
anyDuplicated(df_merge$nuts_3)

# Without correspondence adjustments and aggregation, the merged file corresponds in 586/839 (or just under 70% of total) observations.
# Following manual adjustments from NUTS2006 --> NUTS2010 --> NUTS2013 --> NUTS2016 and above described aggregation procedure, the match ratio is improved to 797/839 (or 95%).

# === make wide to long form panel data set === #
vars_vary<-names(df_merge)[str_detect(names(df_merge), "^X\\d{4}$")]
years <- unique(str_extract_all(vars_vary, "\\d{4}"))
var_names <- 'GDPpc'
sorted_vars_vary <- vars_vary[order(as.numeric(gsub("^X", "", vars_vary)))]

df_long<- reshape(df_merge, direction = 'long', 
                  varying = sorted_vars_vary, 
                  timevar = 'year',
                  times = unlist(years),
                  v.names = var_names,
                  idvar = 'nuts_3')

# === PCA expected output: ["FALSE"] === #
any(duplicated(df_long[,c("nuts_3", "year")]))

# === Drop no-use variables === #
drops <- c('Country', 'STAT_LEVL_CODE', 'NUTS_NAME')
df_long <- df_long[ , !(names(df_long) %in% drops)]

prepare_missing_values_graph(df_long, ts_id = 'year', no_factors = FALSE, binary = TRUE)
# count number of NA in the data 
sum(is.na(df_long))

# === inspect data types === #
str(df_long)

# === omitting NAs from long form for use === #
df_nona <- na.omit(df_long)

#save merged df to directory
write.csv(df_nona, file="data_nuts3_long.csv")
