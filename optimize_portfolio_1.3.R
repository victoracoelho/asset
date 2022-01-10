library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)


tickers <- c('TASA4.SA', 'RRRP3.SA', 'SUZB3.SA', 'BMOB3.SA', 'ESPA3.SA', 'PETZ3.SA',
             'ETER3.SA', 'JBSS3.SA', 'RECV3.SA', 'RANI3.SA', 'ASAI3.SA', 'SEQL3.SA',
             'MELI34.SA')

tickers_clean <- c('TASA', 'RRRP', 'SUZB', 'BMOB', 'ESPA', 'PETZ',
                   'ETER', 'JBSS', 'RECV', 'RANI', 'ASAI', 'SEQL',
                   'MELI')

pesos <- c(0.11, 0.080, 0.070, 0.060, 0.060, 0.060, 0.06, 0.05, 0.05, 0.05,
           0.05, 0.03, 0.04)

prices <- NULL
for (ticker in tickers){
  prices <- cbind(prices, getSymbols(ticker, from = '2021-1-1', periodicity = 'daily', auto.assign = F)[,4])
}

benchmark <- getSymbols("^BVSP", from='2021-05-07', periodicity = 'daily', auto.assign = F)[,4]


#port.prices <- na.omit(merge(Ad(TASA4.SA), Ad(BMOB3.SA), Ad(PETZ3.SA), Ad(ASAI3.SA),
                             #Ad(RANI3.SA), Ad(SUZB3.SA), Ad(ETER3.SA), Ad(RRRP3.SA), 
                             #Ad(RECV3.SA), Ad(PCAR3.SA), Ad(ESPA3.SA), Ad(BOVV11.SA), 
                             #Ad(MELI34.SA)))


portf.ret <- na.omit(ROC(prices, type = "continuous"))[-1,]
colnames(portf.ret) <- tickers_clean


VaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")
CVaR(R=portf.ret, p=0.99, method="historical", weights = pesos ,portfolio_method = "component")


# assets = portf.ret, weight_seq = pesos
portf <- portfolio.spec(colnames(portf.ret))
#portf <- portfolio.spec(assets = portf.ret, weight_seq = pesos)


portf <- add.constraint(portf, type = 'weight_sum', min_sum =0.99, max_sum =1.01)
portf <- add.constraint(portf, type = "transaction_cost", ptc=0.001)
portf <- add.constraint(portf, type = 'box', min = .05, max = .35)
portf <- add.objective(portf, type = 'return', name="mean", target=0.05)
portf <- add.objective(portf, type = 'risk', name="StdDev", target=0.005)



## optPort <- optimize.portfolio(portf.ret, portf, optimize_method = "ROI", trace = TRUE)


rp <- random_portfolios(portfolio = portf, permutations = 1000, rp_method = "sample")

optReb <- optimize.portfolio.rebalancing(portf.ret, portf, optimize_method = "random",
                                         rp=rp, rebalance_on = "months", training_period = 50,
                                         rolling_window = NULL)

optRet <- optimize.portfolio(portf.ret, portfolio = portf, optimize_method = "DEoptim", 
                             search_size = 20000, rp = rp, trace = FALSE)

benchmark <- ROC(benchmark, type = 'continuous')[-1]


chart.Weights(optReb)

rebal_weights <- extractWeights(optRet)
rebal_ret <- Return.portfolio(portf.ret, weights = rebal_weights)
real_ret <- Return.portfolio(portf.ret, weights = pesos)
rets_df <- na.omit(cbind(rebal_ret, real_ret, benchmark))
df <- rets_df

charts.PerformanceSummary(df)



chart.RelativePerformance(portf.ret, benchmark, colorset = rainbow12equal, legend.loc = "topright", 
                          cex.legend = 0.48, main = "Performance Relativa x IBOV")
chart.RiskReturnScatter(portf.ret)
charts.PerformanceSummary(portf.ret)

annualReturn(portf.ret)
table.AnnualizedReturns(portf.ret)
table.CalendarReturns(portf.ret)




# TESTE
jbs <- getSymbols("JBSS3.SA", from='2021-05-07', periodicity = 'daily', auto.assign = F)[,4]
jbs <- ROC(jbs, type = 'continuous')[-1]

