library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)


tickers <- c('TASA4.SA', 'RRRP3.SA', 'vale3.SA', 'JBSS3.SA', 'RECV3.SA', 'GGBR4.SA',
             'SEQL3.SA', 'ETER3.SA', 'SUZB3.SA', 'PETZ3.SA')

aivale <- c('VALE3.sa', 'RRRP3.SA', 'JBSS3.SA', 'GGBR4.sa', 'BPAC11.sa', 'BBDC4.sa', 
            'ITUB4.sa', 'WEGE3.sa', 'SUZB3.sa', 'TASA4.sa', 'SEQL3.sa', 'IVVB11.sa', 
            'ETER3.sa', 'PETZ3.sa')

tickers_clean <- c('TASA', 'RRRP', 'SUZB', 'BMOB', 'ESPA', 'PETZ',
                   'ETER', 'JBSS', 'RECV', 'RANI', 'ASAI', 'SEQL',
                   'MELI')

pesos <- c(0.125, 0.090, 0.085, 0.085, 0.075, 0.065, 0.06, 0.55, 0.55, 0.05)

aipesos <- c(0.11, 0.08, 0.075, 0.075, 0.065, 0.06, 0.06, 0.055, 0.055, 0.05, 0.05, 0.04,
             0.03, 0.03)



prices <- NULL
for (ticker in tickers){
  prices <- cbind(prices, getSymbols(ticker, from = '2021-1-1', periodicity = 'daily', auto.assign = F)[,4])
}

aiprices <- NULL
for (ai in aivale){
  aiprices <- cbind(aiprices, getSymbols(ai, from = '2021-1-1', periodicity = 'daily', auto.assign = F)[,4])
}

benchmark <- getSymbols("^BVSP", from='2021-05-07', periodicity = 'daily', auto.assign = F)[,4]



portf.ret <- na.omit(ROC(prices, type = "continuous"))[-1,]
airet <- na.omit(ROC(aiprices, type = "continuous"))[-1,]
colnames(portf.ret) <- tickers_clean


VaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")
CVaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")

VaR(R=airet, p=0.99, method="historical", weights = aipesos ,portfolio_method = "component")
CVaR(R=airet, p=0.99, method="historical", weights = aipesos ,portfolio_method = "component")


benchmark <- ROC(benchmark, type = 'continuous')[-1]


chart.RelativePerformance(portf.ret, benchmark, colorset = rainbow12equal, legend.loc = "topright", 
                          cex.legend = 0.48, main = "Performance Relativa x IBOV")
chart.RiskReturnScatter(portf.ret)
chart.RiskReturnScatter(airet)

charts.PerformanceSummary(portf.ret)
charts.PerformanceSummary(airet)

annualReturn(portf.ret)
table.AnnualizedReturns(portf.ret)
table.CalendarReturns(portf.ret)
table.Autocorrelation(portf.ret)


chart.Correlation(portf.ret)
chart.Correlation(airet)

table.Correlation(portf.ret, Rb = benchmark)
table.Correlation(airet, Rb = benchmark)