# === Theil Index === #
# requires package 'dineq'
# must check for following that subset is correctly specified
# theil.wtd(df$year == 1900)
subset.dfGDPpc <- subset.data.frame(dfGDPpc,STAT_LEVL_CODE==0,select=c(NUTS_ID, NUTS_NAME,41:43))

vector.subset.columnTheil <- apply(subset.dfGDPpc[,3:ncol(subset.dfGDPpc)], c(2), theil.wtd)

plot(vector.subset.columnTheil,xaxt="n")
axis(1,at=1:3,labels=names(vector.subset.columnTheil))

#