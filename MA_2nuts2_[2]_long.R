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
df <- read.csv('data_nuts2_wide.csv')

# === make wide to long form panel data set === #
vars_vary<-names(df)[str_detect(names(df), "_\\d{1}[90]\\d{2}")]
years <- unique(str_extract_all(vars_vary, "\\d{4}"))
var_names <- unique(str_extract_all(vars_vary, "[A-Z]+(_11)?(?=_\\d{4})"))
sorted_vars_vary_names <- vars_vary[order(gsub("\\d{4}$", "", vars_vary))]
sorted_vars_vary <- sorted_vars_vary_names[order(as.numeric(gsub("[A-Z]+(_11)?_", "", sorted_vars_vary_names)))]

df_long<- reshape(df, direction="long", 
        varying = sorted_vars_vary, 
        timevar = 'Year',
        times = unlist(years),
        v.names = unlist(var_names)[order(unlist(var_names))],
        idvar='nuts_2')

# === PCA expected output: ["FALSE"] === #
any(duplicated(df_long[,c("nuts_2", "Year")]))

# === Flevoland ('nuts_2' == "NL23") adjustments prior to 1960, 1970 respectively === #
# Flevoland has "." as missing value rep in some years. Adjust to NA prior to conversion of variables from chr to num
df_long[df_long == "."] <- NA

# check replacement of "." with NA worked by counting the number of NA in the data [expected 33]
sum(is.na(df_long))

# === inspect and adjust data types === #
str(df_long)
df_long$GDP <- as.numeric(df_long$GDP)
df_long$GDP_11 <- as.numeric(df_long$GDP_11)
df_long$AGSH <- as.numeric(gsub(",", ".", df_long$AGSH))
df_long$SERSH <- as.numeric(gsub(",", ".", df_long$SERSH))
df_long$INDSH <- as.numeric(gsub(",", ".", df_long$INDSH))
df_long$POP <- as.numeric(gsub("\\.","",df_long$POP))

# === omitting NAs from long form for use === #
df_nona <- na.omit(df_long)

# === save panel data frame to directory === #
write.csv(df_nona, file="data_nuts2_long.csv")
