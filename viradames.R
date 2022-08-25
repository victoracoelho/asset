library(dplyr)
library(openxlsx)


## CRIANDO DATAFRAME
data <- data.frame(VGR)
df <- cbind(data$CD_COTISTA, data$CD_FUNDO, data$SALDO_BRUTO, data$QT_COTAS, data$QT_COTAS_CARTEIRA)


## LIMPANDO DATAFRAME, RETIRANDO COTISTAS SEM POSICAO
row_sub = apply(df, 1, function(row) all(row !=0 ))
df_cle <- df[row_sub,]
new_df <- data.frame(df_cle)


## CRIANDO NOVOS DATAFRAMES, AGRUPANDO POR CLUBE
cd_5224 <- subset(new_df, new_df$X2==5224)
cd_5224$X6 <- (cd_5224$X4 / cd_5224$X5)
cd_5224 <- data.frame(cbind(cd_5224$X1, cd_5224$X3, cd_5224$X4, cd_5224$X6))
colnames(cd_5224) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5214 <- subset(new_df, new_df$X2==5214)
cd_5214$X6 <- (cd_5214$X4 / cd_5214$X5)
cd_5214 <- data.frame(cbind(cd_5214$X1, cd_5214$X3, cd_5214$X4, cd_5214$X6))
colnames(cd_5214) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5288 <- subset(new_df, new_df$X2==5288)
cd_5288$X6 <- (cd_5288$X4 / cd_5288$X5)
cd_5288 <- data.frame(cbind(cd_5288$X1, cd_5288$X3, cd_5288$X4, cd_5288$X6))
colnames(cd_5288) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5338 <- subset(new_df, new_df$X2==5338)
cd_5338$X6 <- (cd_5338$X4 / cd_5338$X5)
cd_5338 <- data.frame(cbind(cd_5338$X1, cd_5338$X3, cd_5338$X4, cd_5338$X6))
colnames(cd_5338) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5340 <- subset(new_df, new_df$X2==5340)
cd_5340$X6 <- (cd_5340$X4 / cd_5340$X5)
cd_5340 <- data.frame(cbind(cd_5340$X1, cd_5340$X3, cd_5340$X4, cd_5340$X6))
colnames(cd_5340) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5360 <- subset(new_df, new_df$X2==5360)
cd_5360$X6 <- (cd_5360$X4 / cd_5360$X5)
cd_5360 <- data.frame(cbind(cd_5360$X1, cd_5360$X3, cd_5360$X4, cd_5360$X6))
colnames(cd_5360) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5361 <- subset(new_df, new_df$X2==5361)
cd_5361$X6 <- (cd_5361$X4 / cd_5361$X5)
cd_5361 <- data.frame(cbind(cd_5361$X1, cd_5361$X3, cd_5361$X4, cd_5361$X6))
colnames(cd_5361) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5362 <- subset(new_df, new_df$X2==5362)
cd_5362$X6 <- (cd_5362$X4 / cd_5362$X5)
cd_5362 <- data.frame(cbind(cd_5362$X1, cd_5362$X3, cd_5362$X4, cd_5362$X6))
colnames(cd_5362) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5363 <- subset(new_df, new_df$X2==5363)
cd_5363$X6 <- (cd_5363$X4 / cd_5363$X5)
cd_5363 <- data.frame(cbind(cd_5363$X1, cd_5363$X3, cd_5363$X4, cd_5363$X6))
colnames(cd_5363) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5430 <- subset(new_df, new_df$X2==5430)
cd_5430$X6 <- (cd_5430$X4 / cd_5430$X5)
cd_5430 <- data.frame(cbind(cd_5430$X1, cd_5430$X3, cd_5430$X4, cd_5430$X6))
colnames(cd_5430) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5431 <- subset(new_df, new_df$X2==5431)
cd_5431$X6 <- (cd_5431$X4 / cd_5431$X5)
cd_5431 <- data.frame(cbind(cd_5431$X1, cd_5431$X3, cd_5431$X4, cd_5431$X6))
colnames(cd_5431) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5452 <- subset(new_df, new_df$X2==5452)
cd_5452$X6 <- (cd_5452$X4 / cd_5452$X5)
cd_5452 <- data.frame(cbind(cd_5452$X1, cd_5452$X3, cd_5452$X4, cd_5452$X6))
colnames(cd_5452) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5453 <- subset(new_df, new_df$X2==5453)
cd_5453$X6 <- (cd_5453$X4 / cd_5453$X5)
cd_5453 <- data.frame(cbind(cd_5453$X1, cd_5453$X3, cd_5453$X4, cd_5453$X6))
colnames(cd_5453) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5490 <- subset(new_df, new_df$X2==5490)
cd_5490$X6 <- (cd_5490$X4 / cd_5490$X5)
cd_5490 <- data.frame(cbind(cd_5490$X1, cd_5490$X3, cd_5490$X4, cd_5490$X6))
colnames(cd_5490) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_5492 <- subset(new_df, new_df$X2==5492)
cd_5492$X6 <- (cd_5492$X4 / cd_5492$X5)
cd_5492 <- data.frame(cbind(cd_5492$X1, cd_5492$X3, cd_5492$X4, cd_5492$X6))
colnames(cd_5492) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9413 <- subset(new_df, new_df$X2==9413)
cd_9413$X6 <- (cd_9413$X4 / cd_9413$X5)
cd_9413 <- data.frame(cbind(cd_9413$X1, cd_9413$X3, cd_9413$X4, cd_9413$X6))
colnames(cd_9413) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9463 <- subset(new_df, new_df$X2==9463)
cd_9463$X6 <- (cd_9463$X4 / cd_9463$X5)
cd_9463 <- data.frame(cbind(cd_9463$X1, cd_9463$X3, cd_9463$X4, cd_9463$X6))
colnames(cd_9463) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9641 <- subset(new_df, new_df$X2==9641)
cd_9641$X6 <- (cd_9641$X4 / cd_9641$X5)
cd_9641 <- data.frame(cbind(cd_9641$X1, cd_9641$X3, cd_9641$X4, cd_9641$X6))
colnames(cd_9641) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9676 <- subset(new_df, new_df$X2==9676)
cd_9676$X6 <- (cd_9676$X4 / cd_9676$X5)
cd_9676 <- data.frame(cbind(cd_9676$X1, cd_9676$X3, cd_9676$X4, cd_9676$X6))
colnames(cd_9676) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9705 <- subset(new_df, new_df$X2==9705)
cd_9705$X6 <- (cd_9705$X4 / cd_9705$X5)
cd_9705 <- data.frame(cbind(cd_9705$X1, cd_9705$X3, cd_9705$X4, cd_9705$X6))
colnames(cd_9705) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")

cd_9785 <- subset(new_df, new_df$X2==9785)
cd_9785$X6 <- (cd_9785$X4 / cd_9785$X5)
cd_9785 <- data.frame(cbind(cd_9785$X1, cd_9785$X3, cd_9785$X4, cd_9785$X6))
colnames(cd_9785) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS", "POSICAO")


## EXPORTANDO DATAFRAMES PARA EXCEL
xl_list <- list('5224' = cd_5224, '5214' = cd_5214, '5288' = cd_5288,
                '5338' = cd_5338, '5340' = cd_5340, '5360' = cd_5360,
                '5361' = cd_5361, '5362' = cd_5362, '5363' = cd_5363,
                '5430' = cd_5430, '5431' = cd_5431, '5452' = cd_5452,
                '5453' = cd_5453, '5490' = cd_5490, '5492' = cd_5492,
                '9413' = cd_9413, '9463' = cd_9463, '9641' = cd_9641,
                '9676' = cd_9676, '9705' = cd_9705, '9785' = cd_9785)

write.xlsx(xl_list, file = "posicao.xlsx")
