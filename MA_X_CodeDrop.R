# PACMAN ALTERNATIVE TO INSTALL AND LOAD PACKAGES
# === Install packages if not installed === #
libraries = c("readODS", "dineq")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
# Load packages
lapply(libraries, 
       library, 
       quietly        = TRUE, 
       character.only = TRUE)

Since Stargazer can't do this directly, you can create your own summary table as a data frame and output that using pander, xtable, or any other package. For example, here's how you can use dplyr and tidyr to create a summary table:
  
  library(dplyr)
library(tidyr)

# CREATE CUSTOM SUMMARY STATISTICS TABLE
# https://stackoverflow.com/questions/26912957/output-each-factor-level-as-dummy-variable-in-stargazer-summary-statistics-table
fancy.summary <- Blackmoor %>%
  select(-subject) %>%  # Remove the subject column
  group_by(group) %>%  # Group by patient and control
  summarise_each(funs(mean, sd, min, max, length)) %>%  # Calculate summary statistics for each group
  mutate(prop = age_length / sum(age_length)) %>%  # Calculate proportion
  gather(variable, value, -group, -prop) %>%  # Convert to long
  separate(variable, c("variable", "statistic")) %>%  # Split variable column
  mutate(statistic = ifelse(statistic == "length", "n", statistic)) %>%
  spread(statistic, value) %>%  # Make the statistics be actual columns
  select(group, variable, n, mean, sd, min, max, prop)  # Reorder columns
#Which results in this custom summary table if you use pander:
library(pander)

pandoc.table(fancy.summary)

------------------------------------------------------
  group   variable   n   mean   sd    min   max   prop 
------- ---------- --- ------ ----- ----- ----- ------
  control    age     359 11.26  2.698   8   17.92 0.3799

control  exercise  359 1.641  1.813   0   11.54 0.3799

patient    age     586 11.55  2.802   8   17.92 0.6201

patient  exercise  586 3.076  4.113   0   29.96 0.6201
------------------------------------------------------
  
  

# Q: How to save a plot in R to a sub-directory of the working directory
File <- "./img/name.png"
if (file.exists(File)) stop(File, " already exists")
dir.create(dirname(File), showWarnings = FALSE)
png(File)
dev.off()

# remove rows that have NAs in one of the addressed columns
final[complete.cases(df[ , 5:6]),]

# DOUBLE CHECK GITHUB SPL-OIL CODE FOR USE FOR THE FUNCTION RESULTLATEX1, RESULTLATEX2 RESPECITVELY
D_2008 = ResultLatex1(a = DumyReg)
print.xtable(xtable(D_2008),
             type = "latex", 
             file = "./D_2008.txt", 
             size = "tiny")

R2_2008 = ResultLatex2(
  a = repre2008, 
  b = rePost2008)
print.xtable(xtable(R2_2008),
             type = "latex", 
             file = "./2R2008.txt", 
             size = "tiny")

# === Theil Index === #
# requires package 'dineq'
# must check for following that subset is correctly specified
# theil.wtd(df$year == 1900)
subset.dfGDPpc <- subset.data.frame(dfGDPpc,STAT_LEVL_CODE==0,select=c(NUTS_ID, NUTS_NAME,41:43))

vector.subset.columnTheil <- apply(subset.dfGDPpc[,3:ncol(subset.dfGDPpc)], c(2), theil.wtd)

plot(vector.subset.columnTheil,xaxt="n")
axis(1,at=1:3,labels=names(vector.subset.columnTheil))