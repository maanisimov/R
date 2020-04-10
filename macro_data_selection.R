library(dplyr)
library(gdata)

df_1 <- read.xls('/Users/maxim_anisimov/Desktop/VARL_project/macro_ts_1.xlsx')
df_2 <- read.xls('/Users/maxim_anisimov/Desktop/VARL_project/macro_ts_2.xlsx')
pmi <- read.xls('/Users/maxim_anisimov/Desktop/VARL_project/pmi.xlsx')

# df_1
# EHUPUS - US Unemployment Rate (%)
# GDP.CURY.Index - U.S. GDP in Nominal Dollars year-over-year
# CPI.INDX.Index - US CPI Index
# INJCJC - US Initial Jobless Claims SA
# TMNOCONS - US Manufacturers New orders consumer goods SA
# MTSL - US Manuf and trade sales in nominal dollars SA
# MTSLRL - US Manuf and trade sales in millions of chained (2009) dollars SA
# CPTICHNG - US Capacity Utilization % of Total Capacity SA
# NHCHATCH - Private Total Housing Authorized by Building Permits MoM SA
# PPI - US PPI Finished Goods SA MoM
# PPF.TOT - US PPI finished goods SA
# CPI.CHNG - US CPI Urban Consumers MoM SA
# CPURNSA - US CPI Urban Consumers NSA
# CONCCONF - Conference Board Consumer Confidence SA 1985=100
# M2.YOY - Fed money supply M2 YoY % change
# LEI.IRTE - Conference board US leading index interest spread 10 year Treasury less fed funds rate
# FDTR - Federal Funds Target Rate - Upper Bound
# USTBTOT - US Trade Balance of Goods and Services SA
# USTBIMP - US IMPORT
# DXY.CURNCY - DOLLAR INDEX SPOT
# GDP.CUR - GDP US nominal dollars SAAR
# AWH.MANU - US average weekly hours all employees manufacturing SA
# TMNOCHNG - US Manufacturers New Orders Total MoM SA
# LEICDIFF - Conference board US leading net contributions vendor performance diffusion
# MTIBCHNG - US Manufacturing & Trade Inventories Total MoM SA
# MTSLCHNG - US manuf and trade sales MoM SA
# CPMFTOT - US capacity utilization manuf total SIC SA
# LEI.BP - Conference board US leading index building permits
# PPI.CHNG - US PPI Finished Goods SA MoM%
# PPI.YOY - US PPI Finished Goods NSA YoY%
# COI.PI - Conference board Coincident personal income less transfer payments 
# M2 - Fed US money supply M2 SA
# ECORUSN - US money supply M2 (billion USD) NSA
# GT10.GOVT - generic US 10 year government note
# FEDL01 - Bloomberg Dollar Total Return Index
# USTBEXP - US EXPORTS
# USTBGOOD - US trade balance of goods SA

# df_2
# CPFTTOT Index	-	Corporate Profits
# CPFTATAX - Corporate Profits After Tax
# EHGDUSY - 
# EHGDUS - USA Real GDP
# GDP.PIQQ - 
# GDP.CQOQ - 
# PCE.CRCH - 
# PCE.DEFM -
# GDPCPCEC -
# RREFKEYR -
# RUFRON -

glimpse(df_1)
glimpse(df_2)
glimpse(pmi)
pmi <- rename(pmi, 'Dates' = 'Date')
df <- merge(df_1, df_2, by='Dates')
pmi$Dates <- as.Date(pmi$Dates)
df$PMI <- dplyr::filter(pmi[1:nrow(pmi)-1,], Dates >= '1968-02-01')$Confidence # take only common data ranges
glimpse(df)
length(df)

df <- data.frame(lapply(df, function(x) {gsub("#N/A N/A", NaN, x)})) # missed values in approp. format
df[, 2:length(df)] <- format(df[, 2:length(df)], decimal.mark=".") # change decimal mark from , to .
df$Dates <- as.Date(df$Dates) # to date format
df[, 2:length(df)] <- lapply(df[, 2:length(df)], as.numeric) # all var. to numeric (except dates)
glimpse(df)
df <- dplyr::filter(df, Dates >= '1968-03-29') 
glimpse(df)
df_selected <- dplyr::select(df, -FDTR.Index, -USTBTOT.Index, -AWH.MANU.Index,
                             -EHGDUSY.Index, -RREFKEYR.Index, -RUFRON.Index, -PMI) # remove ts with NaNs

write.csv(df_selected, file = "macro_times_series.csv")