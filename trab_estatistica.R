library(lubridate)
library(tidyverse)
library(forecast)
library(dplyr)
library(psych)
library(ggplot2)
library(zoo)

# Criando a tabela de dados meteorologicos acumlados por dia
dados_meteorologicos <- read.csv("tabelas/precipitacao/precipitacao.csv")


dados_meteorologicos$Data <- as.Date(dados_meteorologicos$Data)

print(str(dados_meteorologicos))

# verificando valores discrepantes

plot(dados_meteorologicos$Data, dados_meteorologicos$PRECIPITAÇÃO.TOTAL..HORÁRIO..mm.)
print(describe(dados_meteorologicos))

# Excluindo valores menores que zero para NA

dados_met = dados_meteorologicos
dados_met <- subset(dados_met, (PRECIPITAÇÃO.TOTAL..HORÁRIO..mm. >= 0) & (TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C. >= 0) & (UMIDADE.RELATIVA.DO.AR..HORARIA.... >= 0))

print(str(dados_met))
print(describe(dados_met))

# Agregando os dados meteorologicos por dia

dados_met_por_dia <- dados_met %>% 

  group_by(Data) %>%
  summarise(acumulado_chuva_dia = sum(PRECIPITAÇÃO.TOTAL..HORÁRIO..mm.),
            media_temp_dia = mean(TEMPERATURA.DO.AR...BULBO.SECO..HORARIA...C.),
            media_umidade_dia = mean(UMIDADE.RELATIVA.DO.AR..HORARIA....))

# Plots das variaveis meteorologicas ao longo do tempo 
ggplot(dados_met_por_dia) + geom_line(aes(x=Data,y=acumulado_chuva_dia)) +
  scale_x_date(date_breaks = "12 months")

ggplot(dados_met_por_dia) + geom_line(aes(x=Data,y=media_temp_dia)) +
  scale_x_date(date_breaks = "12 months")

ggplot(dados_met_por_dia) + geom_line(aes(x=Data,y=media_umidade_dia)) +
  scale_x_date(date_breaks = "12 months")


# Criando a tabela notificações acumuladas somente dos casos confirmados por dia (CLASSI_FIN = 1 entre 2010 e 2013)
# CLASSI_FIN = 10, 11 e 12 a partir de 2014)
dados_notificacoes <- read.csv("tabelas/SINAN/tab_SINAN__florianopolis.csv")
print(str(dados_notificacoes))
dados_notificacoes$DT_NOTIFIC = as.Date(dados_notificacoes$DT_NOTIFIC)

# Filtrando a tabela para ter somente as notificações comfirmadas

dados_not_filt = subset(dados_notificacoes, (CLASSI_FIN == 1) | (CLASSI_FIN == 10) | (CLASSI_FIN == 11) |(CLASSI_FIN == 12))

dados_not_acumlados_por_dia <- dados_not_filt %>% 
  
  group_by(DT_NOTIFIC) %>%
  summarise(acumulado_not_dia = n())

dados_not_acumlados_por_dia

ggplot(dados_not_acumlados_por_dia) + geom_line(aes(x=DT_NOTIFIC,y=acumulado_not_dia)) +
  scale_x_date(date_breaks = "12 months")

colnames(dados_not_acumlados_por_dia)[which(names(dados_not_acumlados_por_dia) == "DT_NOTIFIC")] <- "Data"

dados_not_acumlados_por_dia

# criando o data set final
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2024-12-31")
datas_periodo <- seq(from = start_date, to = end_date, by = "day")

df_pre <- data.frame(Data = datas_periodo)
print(str(df_pre))

#Unindo as tabelas usando dia como chave, dias que não estão nas outras tabelas por não
# ter medição ou caso da variável ficaram como NA
df_pre <- left_join(df_pre, dados_not_acumlados_por_dia, by="Data")
df_pre <- left_join(df_pre, dados_met_por_dia, by="Data")
df_pre$semana  <- week(df_pre$Data)
df_pre$ano  <- year(df_pre$Data)
df_pre$mes  <- month(df_pre$Data)

# Substituindo os valores NA porm 0 nas variáveis de acumulo
df_pre$acumulado_not_dia[is.na(df_pre$acumulado_not_dia)] <- 0
df_pre$acumulado_chuva_dia[is.na(df_pre$acumulado_chuva_dia)] <- 0

describe(df_pre)


# Substituindo valores NA de media por a média do dia anterior

df_pre$media_temp_dia <- na.locf(df_pre$media_temp_dia, fromLast = TRUE, na.rm = FALSE)
df_pre$media_umidade_dia <- na.locf(df_pre$media_umidade_dia, fromLast = TRUE, na.rm = FALSE)

# Agrupando por mês
df_mensal <- df_pre %>%
  group_by(Data = floor_date(Data, "1 month")) %>% 
  summarise(contagem = sum(acumulado_not_dia),
            temp_media_semana = mean(media_temp_dia),
            umid_media_semana = mean(media_umidade_dia),
            chuva_acum = sum(acumulado_chuva_dia),
            ano = median(ano),
            semana = median(semana),
            mes = median(mes))

ggplot(df_mensal) + geom_line(aes(x=Data,y=contagem)) +
  scale_x_date(date_breaks = "12 months")

ggplot(df_mensal) + geom_line(aes(x=Data,y=temp_media_semana)) +
  scale_x_date(date_breaks = "12 months")

ggplot(df_mensal) + geom_line(aes(x=Data,y=umid_media_semana)) +
  scale_x_date(date_breaks = "12 months")

ggplot(df_mensal) + geom_line(aes(x=Data,y=chuva_acum)) +
  scale_x_date(date_breaks = "12 months")

# Separação dos dados em 2 grupos treino/teste, 1 com os dados somente entre 2010 e 
# e 2020 com o ltimo ano de teste e o outro com todos os dados usando o ultimo ano de teste

dados_1 = df_mensal[df_semanal$ano < 2020,]
dados_2 = df_mensal


treino1 = dados_1[dados_1$ano < 2018,]
teste1 = dados_1[dados_1$ano >= 2018,]

treino2 = dados_2[dados_2$ano < 2023,]
teste2 = dados_2[dados_2$ano >= 2023,]

ts1_treino= ts(treino1$contagem, frequency = 12)
plot(ts1_treino)
ts1_test= ts(teste1$contagem, start=end(ts1_treino), frequency = 12)
plot(ts1_test)
ts2_treino= ts(treino2$contagem, frequency = 12)
plot(ts2_treino)
ts2_test= ts(teste2$contagem, start=end(ts2_treino), frequency = 12)
plot(ts2_test)


treino3 = dados_2[(dados_2$ano <= 2022) & (dados_2$ano >= 2020),]
teste3 = dados_2[dados_2$ano > 2022,]
ts3_treino= ts(treino3$contagem, frequency = 12)
plot(ts3_treino)
ts3_test= ts(teste3$contagem, start=end(ts3_treino), frequency = 12)
plot(ts3_test)

# Predição usando modelo de regressão linear Para dados_1

modelo_linear11 = lm(data = treino1, contagem ~ umid_media_semana + temp_media_semana + chuva_acum + ano + mes)
summary(modelo_linear11)
forecast::checkresiduals(modelo_linear11)

prev_modelo_linear11 <- predict(modelo_linear11, teste1)
graph_model11 <- cbind(teste1,prev_modelo_linear11)

ggplot(graph_model11, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear11, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))

# Retirando as variaveis com p-valor maior que 0.05

modelo_linear12 = lm(data = treino1, contagem ~ + mes)
summary(modelo_linear12)
forecast::checkresiduals(modelo_linear12)

prev_modelo_linear12 <- predict(modelo_linear12, teste1)
graph_model12 <- cbind(teste1,prev_modelo_linear12)

ggplot(graph_model12, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear12, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))

# Predição usando modelo de regressão linear Para dados_2

modelo_linear21 = lm(data = treino2, contagem ~ umid_media_semana + temp_media_semana + chuva_acum + ano + mes)
summary(modelo_linear21)
forecast::checkresiduals(modelo_linear21)

prev_modelo_linear21 <- predict(modelo_linear21, teste2)
graph_model21 <- cbind(teste2,prev_modelo_linear21)

ggplot(graph_model21, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear21, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))

# Retirando as variaveis com p-valor maior que 0.05

modelo_linear22 = lm(data = treino2, contagem ~ ano)
summary(modelo_linear22)
forecast::checkresiduals(modelo_linear22)

prev_modelo_linear22 <- predict(modelo_linear22, teste2)
graph_model22 <- cbind(teste2,prev_modelo_linear22)

ggplot(graph_model22, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear22, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))


# Predição usando modelo de regressão linear Para dados_2

modelo_linear31 = lm(data = treino3, contagem ~ umid_media_semana + temp_media_semana + chuva_acum + ano + mes)
summary(modelo_linear31)
forecast::checkresiduals(modelo_linear31)

prev_modelo_linear31 <- predict(modelo_linear31, teste3)
graph_model31 <- cbind(teste3,prev_modelo_linear31)

ggplot(graph_model31, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear31, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))

# Retirando as variaveis com p-valor maior que 0.05

modelo_linear32 = lm(data = treino3, contagem ~ ano)
summary(modelo_linear32)
forecast::checkresiduals(modelo_linear32)

prev_modelo_linear32 <- predict(modelo_linear32, teste3)
graph_model32 <- cbind(teste3,prev_modelo_linear32)

ggplot(graph_model32, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = contagem, color = "Real"),
            linetype = "dashed") +
  geom_line(aes(y = prev_modelo_linear32, color = "Regressao Linear"),
            linetype = "dashed") +
  scale_color_manual(name = NULL, values = c("blue", "red"))

# Previsão usando o modelo ARIMA


# Usando dados entre 2010 e 2020 arima com Seasonal = false
modelo_arima = auto.arima(ts1_treino, seasonal = FALSE)
checkresiduals((modelo_arima))
summary(modelo_arima)
prev_arima <- forecast(modelo_arima, h = 24)
prev_arima
teste = cbind(teste1,prev_arima$mean)



teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_arima$mean, color = "ARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")


# Usando arima com Seasonal = True
modelo_sarima = auto.arima(ts1_treino, seasonal = TRUE, D = 1  )
summary(modelo_sarima)
checkresiduals((modelo_sarima))
prev_sarima <- forecast(modelo_sarima, h=24)
teste = cbind(teste1,prev_sarima$mean)

teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_sarima$mean, color = "SARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")

# Usando dados entre 2010 e 2024 arima com Seasonal = false
modelo_arima = auto.arima(ts2_treino, seasonal = FALSE)
checkresiduals((modelo_arima))
summary(modelo_arima)
prev_arima <- forecast(modelo_arima, h = 24)
teste = cbind(teste2,prev_arima$mean)


teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_arima$mean, color = "ARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")


# Usando arima com Seasonal = True
modelo_sarima = auto.arima(ts2_treino, seasonal = TRUE, D = 1   )
summary(modelo_sarima)
checkresiduals((modelo_sarima))
prev_sarima <- forecast(modelo_sarima, h=24)
teste = cbind(teste2,prev_sarima$mean)



teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_sarima$mean, color = "ARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")



# Usando dados entre 2020 e 2024 arima com Seasonal = false
modelo_arima = auto.arima(ts3_treino, seasonal = FALSE)
checkresiduals((modelo_arima))
summary(modelo_arima)
prev_arima <- forecast(modelo_arima, h = 24)
teste = cbind(teste3,prev_arima$mean)

teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_arima$mean, color = "ARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")


# Usando arima com Seasonal = True
modelo_sarima = auto.arima(ts3_treino, seasonal = TRUE, D = 1  )
summary(modelo_sarima)
checkresiduals((modelo_sarima))
prev_sarima <- forecast(modelo_sarima, h=24)
teste = cbind(teste3,prev_sarima$mean)


teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = prev_sarima$mean, color = "ARIMA"),
            linetype = "dashed") +
  theme(legend.position = "bottom")


# Usando suavizção exponencial em dados1

SES = ses(ts1_treino, h=24)
summary(SES)
HOLT = holt(ts1_treino, h=24)
summary(HOLT)
HW_ad = hw(ts1_treino, seasonal = "additive", h=24)
summary(HW_ad)

SES.predito = forecast(SES,h = 24)
HOLT.predito = forecast(HOLT,h = 24)
HWad.predito = forecast(HW_ad,h = 24)
teste = cbind(teste1,SES.predito$mean,HOLT.predito$mean,HWad.predito$mean)



teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = SES.predito$mean, color = "SES"),
            linetype = "dashed") +
  geom_line(aes(y = HOLT.predito$mean, color = "HOLT"),
            linetype = "dashed") +
  geom_line(aes(y = HWad.predito$mean, color = "HW.ad"),
            linetype = "dashed") +
  theme(legend.position = "bottom")


# Usando suavizção exponencial em dados2

SES = ses(ts2_treino, h=24)
summary(SES)
HOLT = holt(ts2_treino, h=24)
summary(HOLT)
HW_ad = hw(ts2_treino, seasonal = "additive", h=24)
summary(HW_ad)

SES.predito = forecast(SES,h=24)
HOLT.predito = forecast(HOLT,h=24)
HWad.predito = forecast(HW_ad,h=24)
teste = cbind(teste2,SES.predito$mean,HOLT.predito$mean,HWad.predito$mean)



teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = SES.predito$mean, color = "SES"),
            linetype = "dashed") +
  geom_line(aes(y = HOLT.predito$mean, color = "HOLT"),
            linetype = "dashed") +
  geom_line(aes(y = HWad.predito$mean, color = "HW.ad"),
            linetype = "dashed") +
  theme(legend.position = "bottom")

# Usando suavizção exponencial em dados3

SES = ses(ts3_treino, h=24)
summary(SES)
HOLT = holt(ts3_treino, h=24)
summary(HOLT)
HW_ad = hw(ts3_treino, seasonal = "additive", h=24)
summary(HW_ad)

SES.predito = forecast(SES,h=24)
HOLT.predito = forecast(HOLT,h=24)
HWad.predito = forecast(HW_ad,h=24)
teste = cbind(teste3,SES.predito$mean,HOLT.predito$mean,HWad.predito$mean)



teste$date <- as.Date(teste$Data)
ggplot(teste, aes(x=Data, y=contagem)) +
  geom_line() +
  geom_line(aes(y = SES.predito$mean, color = "SES"),
            linetype = "dashed") +
  geom_line(aes(y = HOLT.predito$mean, color = "HOLT"),
            linetype = "dashed") +
  geom_line(aes(y = HWad.predito$mean, color = "HW.ad"),
            linetype = "dashed") +
  theme(legend.position = "bottom")

