library(quantmod)

getSymbols("YNDX")
tail(YNDX)

# Import QQQ data from Alpha Vantage
getSymbols("QQQ", src="av")

# Look at the structure of QQQ
str(QQQ)

# Import GDP data from FRED
getSymbols("GDP", src="FRED")

# Look at the structure of GDP
str(GDP)

# Assign SPY data to 'spy' using auto.assign argument
spy <- getSymbols('SPY', auto.assign=F)

# Look at the structure of the 'spy' object
str(spy)

# Assign JNJ data to 'jnj' using env argument
jnj <- getSymbols("JNJ", env=NULL)

# Look at the structure of the 'jnj' object
str(jnj)


# Currency #### only for last 180 days :\

# Create a currency_pair object
currency_pair <- "GBP/CAD"

# Load British Pound to Canadian Dollar exchange rate data
getSymbols(currency_pair, src='oanda')

# Examine object using str()
str(GBPCAD)

# Try to load data from 190 days ago
getSymbols(currency_pair, from = Sys.Date() - 190, to = Sys.Date(), src = "oanda")

# OHLC ####
help("OHLC.Transformations")

# CME ####
# Download CME data for CL and BZ as an xts object
oil_data <- Quandl(code = c("CME/CLH2016", "CME/BZH2016"), type = "xts")

# Look at the column names of the oil_data object
colnames(oil_data)

# Extract the Open price for CLH2016
cl_open <- getPrice(oil_data, symbol = "CLH2016", prefer = "Open$")

# Look at January, 2016 using xts' ISO-8601 subsetting
cl_open["2016-01"]