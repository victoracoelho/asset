library(dplyr)
library(openxlsx)


## CRIANDO DATAFRAME
data <- data.frame(VGR)
df <- cbind(data$CD_COTISTA, data$CD_FUNDO, data$SALDO_BRUTO, data$QT_COTAS)


## LIMPANDO DATAFRAME, RETIRANDO COTISTAS SEM POSICAO
row_sub = apply(df, 1, function(row) all(row !=0 ))
df_cle <- df[row_sub,]
new_df <- data.frame(df_cle)


## CRIANDO NOVOS DATAFRAMES, AGRUPANDO POR CLUBE
cd_5224 <- subset(new_df, new_df$X2==5224)
cd_5224 <- data.frame(cbind(cd_5224$X1, cd_5224$X3, cd_5224$X4))
colnames(cd_5224) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS")

cd_5214 <- subset(new_df, new_df$X2==5214)
cd_5214 <- data.frame(cbind(cd_5214$X1, cd_5214$X3, cd_5214$X4))
colnames(cd_5214) <- c("CD_COTISTA", "SALDO_BRUTO", "QTD_COTAS")


## EXPORTANDO DATAFRAMES PARA EXCEL
xl_list <- list('5224' = cd_5224, '5214' = cd_5214)
write.xlsx(xl_list, file = "posicao.xlsx")
