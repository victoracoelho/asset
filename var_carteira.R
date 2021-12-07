
tickers <- c('TASA4.SA', 'BMOB3.SA', 'PETZ3.SA', 'ASAI3.SA', 'RANI3.SA', 'SUZB3.SA',
             'ETER3.SA', 'RRRP3.SA', 'RECV3.SA', 'PCAR3.SA', 'ESPA3.SA', 'BOVV11.SA',
             'MELI34.SA')

pesos <- c(0.11, 0.085, 0.075, 0.075, 0.065, 0.065, 0.06, 0.06, 0.06, 0.055,
           0.055, 0.055, 0.08)

prices <- NULL
for (ticker in tickers){
  prices <- cbind(prices, getSymbols(ticker, from = '2021-1-1', periodicity = 'daily', auto.assign = F)[,4])
}

getSymbols(tickers, from='2021-1-1')


port.prices <- na.omit(merge(Ad(TASA4.SA), Ad(BMOB3.SA), Ad(PETZ3.SA), Ad(ASAI3.SA),
                             Ad(RANI3.SA), Ad(SUZB3.SA), Ad(ETER3.SA), Ad(RRRP3.SA), 
                             Ad(RECV3.SA), Ad(PCAR3.SA), Ad(ESPA3.SA), Ad(BOVV11.SA), 
                             Ad(MELI34.SA)))
portf.ret <- ROC(port.prices, type = "continuous")[-1,]
colnames(portf.ret) <- tickers

VaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")
CVaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")
