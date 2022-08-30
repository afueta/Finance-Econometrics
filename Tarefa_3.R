
rm(list=ls())

## InflaÃ§Ã£o Agregada ##

library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(tidyr)
library(naniar)
library(xts)
library(tseries)
library(forecast)
library(cents)
library(zoo)

setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Dados")

BRA <- read_excel("Valls_Trabalho_Dados_certo.xlsx")

summary(BRA)

BRA <- BRA%>%
  select(Date, IBXX_Vol, IBOVC, IBOVABC, LIBOVC, RCPIBOV, RSPIBOV, PETR4C, Petro4ABC, 
         LPetro4, RCPetro4, RSPetro4, RCPIBOV_SQ, RCPetro4_SQ,
         Selic252_SB)


BRA <- BRA %>% 
  replace_with_na_all(condition = ~.x %in% "#N/A")

library(lubridate)
BRA<- BRA %>% 
  mutate_if(is.character,as.numeric) %>%
  mutate(Date= ymd(Date))


ggplot(data = BRA, aes(x = Date, y = IBOVABC)) +
  geom_line(color="#09557f") +
  scale_y_continuous(limits = c(0,150000))+
  labs(x = "Data",
       y="",
       title = "IBOV",
       subtitle = "Período jan/2000-Jul/2022")
setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Tarefas\\Tarefa 03")
ggsave("IBOV.png",width = 18, height = 9)



ggplot(data = BRA, aes(x = Date, y = Selic252_SB)) +
  geom_line(color="#09557f") +
  scale_y_continuous(limits = c(0,30))+
  labs(x = "Data",
       y="",
       title = "SELIC",
       subtitle = "Período jan/2000-Jul/2022")
setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Tarefas\\Tarefa 03")
ggsave("selic.png",width = 18, height = 9)



ggplot(data = BRA, aes(x = Date, y = Petro4ABC)) +
  geom_line(color="#09557f") +
  scale_y_continuous(limits = c(0,40))+
  labs(x = "Data",
       y="",
       title = "PETR4",
       subtitle = "Período jan/2000-Jul/2022")
setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Tarefas\\Tarefa 03")
ggsave("petr4.png",width = 18, height = 9)

#test unit root for log(dollar)

BRA <- BRA %>%
  mutate(rc_selic = c(NA, diff(log(BRA$Selic252_SB),1)*100),
    exc_ativo = diff(log(Petro4ABC),1)*100 -rc_selic,
    exc_mercado = diff(log(IBOVABC),1)*100-rc_selic)



### Estacionário 
BRA <- BRA %>%
  mutate(IBOVC_e=c(NA,diff(BRA$LIBOVC)),
         PETROC_e=c(NA,diff(BRA$LPetro4)))

library(forecast)
library(tseries)


# ibov
par(mfrow=c(1,2))
Acf(ts(BRA$IBOVC_e[2:5880]), lag.max=20,main='FAC - IBOV')
Pacf((BRA$IBOVC_e[2:5880]), lag.max=20,main='FACP - IBOV')

# Petro
par(mfrow=c(1,2))
Acf(ts(BRA$PETROC_e[2:5880]), lag.max=20,main='FAC - PETR4')
Pacf((BRA$PETROC_e[2:5880]), lag.max=20,main='FACP - PETR4')



summary(lm(exc_ativo ~ exc_mercado, BRA))




##########3 Recursive ###########
# recursive estimation - IBOV
df <- data.frame(BRA$exc_ativo[1:5880], BRA$exc_ativo[2:5881], BRA$exc_mercado[1:5880],BRA$exc_mercado[2:5881])
colnames(df) <- c("dlativo", "dlativo_1","dlmercado", "dlmercado_1")
eq1r_capm <- lapply(seq(10, 5880), 
                    function(x) lm(dlativo ~ dlmercado, data = df[1:x, ]))

# first regression with 10 observations
summary(eq1r_capm[[1]])
# first regression with all observations
summary(eq1r_capm[[5871]])


##3 IBOV
# number of linear regressions performed, starting from 10 observations until it reached 5871
length(eq1r_capm)

# storing all coefficients
all_intercepts <- unlist(sapply(1:length(eq1r_capm),function(j) eq1r_capm[[j]]$coefficients[1]))
all_slopes<-unlist(sapply(1:length(eq1r_capm),function(j) eq1r_capm[[j]]$coefficients[2]))
all_slopes[1]  # slope from the first regression

# storing sd
sd_intercepts <- unlist(sapply(1:length(eq1r_capm),function(j) summary(eq1r_capm[[j]])$coefficients[3]))
sd_slopes<-unlist(sapply(1:length(eq1r_capm),function(j) summary(eq1r_capm[[j]])$coefficients[4]))
sd_slopes[1]  # slope sd from the first regression 


# plotting the coefficients
par(mfrow = c(2,1))

plot.zoo(cbind(all_intercepts, 
               all_intercepts+ sd_intercepts*1.96, 
               all_intercepts-sd_intercepts*1.96), 
         plot.type = "single", 
         main = "Recursive estimation - intercept - CAPM", 
         ylab = "", xlab = "",
         col = c(1,2,2))




plot.zoo(cbind(all_slopes, 
               all_slopes+ sd_intercepts*1.96, 
               all_slopes-sd_intercepts*1.96), 
         plot.type = "single", 
         main = "Recursive estimation - phi - CAPM", 
         ylab = "", xlab = "",
         col = c(1,2,2))




##Simulate a linear process with time-varying coefficient
##as functions of rescaled time period.
library(tvReg)
tau <- seq(1:5879)/5879
#beta <- data.frame(beta1 = sin(2 * pi * tau), beta2 = 2 * tau)
X1 <- BRA$exc_mercado
y <- BRA$exc_ativo
data <-data.frame(y = y, X1 = X1)

##Estimate coefficients with lm and tvLM for comparison
coef.lm <- stats::lm(y ~ X1, data = data)$coef
model.tvLM <- tvLM(y ~ X1, data = data)

tau_2 <- seq(1:5879)
## Calculating regression bandwidth... bw =  0.1047012
##Plot the estimates of beta1
par(mfrow = c(2,1))
plot(tau_2,model.tvLM$coefficients[, 1], 
     type = "l", main = "Alpha", 
     ylab = "",
     xlab = expression(tau), 
     ylim = range(model.tvLM$coefficients[, 1]))
lines(tau_2, rep(coef.lm[1],5879), col=4)
lines(tau_2, c(rep(NA,8),all_intercepts), col=2)
legend("topright", c("tvlm", "MQO","Recursivo"), col = c(1, 4,2), 
       bty = "n", lty = 1, cex = 0.8)

plot(tau_2,model.tvLM$coefficients[, 2], 
     type = "l", main = "Beta", 
     ylab = "",
     xlab = expression(tau), 
     ylim = range(model.tvLM$coefficients[, 2]))
lines(tau_2, rep(coef.lm[2],5879), col=4)
lines(tau_2, c(rep(NA,8),all_slopes), col=2)
legend("topright", c("tvlm", "MQO","Recursivo"), col = c(1, 4,2), 
       bty = "n", lty = 1, cex = 0.8)




####### Questão 02 ###########

BRA[, LastDayInMonth := Date == max(Date), by = .(year(Date), month(Date))]

BRA <- BRA %>%
  mutate(LastDayInMonth = ifelse(LastDayInMonth == TRUE & is.na(IBXX_Vol)==TRUE, 
                                  "AFTER", LastDayInMonth))

BRA_end <- BRA %>% 
  filter(LastDayInMonth == "TRUE")

inds <- which(BRA$LastDayInMonth == TRUE | 
                # Ou ultima obs é NA então pega antes
                lead(BRA$LastDayInMonth)=="AFTER" & lead(is.na(BRA$IBXX_Vol)) == TRUE & is.na(BRA$IBXX_Vol) == FALSE|
                # Ou as duas ultima obs são NA então pega 2 antes
                lead(BRA$LastDayInMonth,2)=="AFTER" & lead(is.na(BRA$IBXX_Vol),2) == TRUE)

BRA_end<- BRA[sort(unique(c(inds))),] %>%
  #Excluir as ultimas duas 
  filter(is.na(BRA_end$IBXX_Vol)==FALSE)
# 297 obs

library(dygraphs)

par(mfrow = c(1,1))

## plots 

ggplot(data = BRA_end, aes(x = Date, y = Petro4ABC)) +
  geom_line(color="#09557f") +
  scale_y_continuous(limits = c(0,40))+
  labs(x = "Data",
       y="",
       title = "PETRO - Fim de período",
       subtitle = "Período jan/2000-Jul/2022")
setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Tarefas\\Tarefa 03")
ggsave("Petro_fim.png",width = 18, height = 9)

ggplot(data = BRA_end, aes(x = Date, y = IBOVABC)) +
  geom_line(color="#09557f") +
  scale_y_continuous(limits = c(0,150000))+
  labs(x = "Data",
       y="",
       title = "IBOVABC - Fim de período",
       subtitle = "Período jan/2000-Jul/2022")
setwd("C:\\Users\\de_il\\Desktop\\Faculdade\\Doutorado\\Financial Econometrics\\Tarefas\\Tarefa 03")
ggsave("IBOVABC_fim.png",width = 18, height = 9)



## Boxplot
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
### Com diferença
seried <- diff(BRA_end$Petro4ABC)
monthplot(seried)

boxplot(diff(BRA_end$Petro4ABC) ~ substring(BRA_end$Date,6,7)[-1],
        main = "Boxplot - diff", 
        ylab = "",
        xlab = seq(1,12))
plot(seried, type="l")


## diferenciação sazonal
petro_dif_s <- diff(BRA_end$Petro4ABC, lag = 12)
Acf(petro_dif_s, lag = 60, main = "ACF - Petr4")
Pacf(petro_dif_s, lag = 60, main = "PACF - IBOV")


ibov_dif_s <- diff(BRA_end$IBOVABC, lag = 12)
Acf(ibov_dif_s, lag = 60, main = "ACF - Petr4")
Pacf(ibov_dif_s, lag = 60, main = "PACF - Petr4")


library(forecast)
library(astsa)

auto.arima(BRA_end$IBOVABC,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')
auto.arima(BRA_end$IBOVABC)
auto.arima(diff(diff(BRA_end$IBOVABC,1,),12))

sarima(BRA_end$IBOVABC, 0,1,0,0,1,0,12)
#19.24

sarima(BRA_end$IBOVABC, 2,1,2,2,1,2,12)
# tira os sar2 e sma1 18.82 BIC 18.94

# (2,1,2)(1,1,0) AIC 19.05 BIC 19.13
sarima(BRA_end$IBOVABC, 2,1,2,1,1,0,12)

####### MELHOR MODELO!!
# (2,1,2)(1,1,1) AIC 18.81 BIC 18.902
sarima(BRA_end$IBOVABC, 2,1,2,1,1,1,12)
Pacf(sarima(BRA_end$IBOVABC, 2,1,2,1,1,1,12)$fit$residuals,lag=60)

# (2,1,2)(2,1,1) AIC 18.84 BIC 18.93
sarima(BRA_end$IBOVABC, 2,1,2,2,1,1,12)
# não melhora

# (2,1,2)(1,1,2) AIC 18.84 BIC 18.93
sarima(BRA_end$IBOVABC, 2,1,2,1,1,2,12)
#não melhora tb


####### Petro4
auto.arima(BRA_end$Petro4ABC)
auto.arima(BRA_end$Petro4ABC,max.p = 5,max.q = 5,max.P = 5,max.Q = 5,max.d = 3,seasonal = TRUE,ic = 'aicc')


# AIC e BIC : 3.77 e 3.78 -> SOBRA RESÍduo no 12
sarima(BRA_end$Petro4ABC, 0,1,0,0,1,0,12) 


# AIC e BIC : 3.54 e 3.56 -> SOBRA RESÍduo no 12 e 24
sarima(BRA_end$Petro4ABC, 0,1,0,1,1,0,12) 
Pacf(sarima(BRA_end$Petro4ABC, 0,1,0,1,1,0,12)$fit$residuals)
# sobra resíduo no 12


# AIC e BIC : 3.3059 e 3.3430 
sarima(BRA_end$Petro4ABC, 0,1,0,1,1,1,12) 
Pacf(sarima(BRA_end$Petro4ABC,  0,1,0,1,1,1,12)$fit$residuals, lag=60)
# ajusta tudo, mas sar insignificante


# AIC e BIC : 3.3001 e 3.324 
sarima(BRA_end$Petro4ABC, 0,1,0,0,1,1,12) 
Pacf(sarima(BRA_end$Petro4ABC,  0,1,0,0,1,1,12)$fit$residuals, lag=60)


sarima(BRA_end$Petro4ABC, 1,1,0,0,1,1,12) 
# ar1 insign


# AIC e BIC : 3.2967 e 3.333 
sarima(BRA_end$Petro4ABC, 0,1,1,0,1,1,12) 
# ma1 significa
Pacf(sarima(BRA_end$Petro4ABC,  0,1,1,0,1,1,12)$fit$residuals, lag=60)


# add outro ma -> não melhora
# AIC e BIC : 3.2934 e 3.343 
sarima(BRA_end$Petro4ABC, 0,1,2,0,1,1,12) 

# add outro ar -> melhora
##### Melhor (1,1,1)(0,1,1)
# AIC e BIC : 3.279 e 3.29 
sarima(BRA_end$Petro4ABC, 1,1,1,0,1,1,12) 
Pacf(sarima(BRA_end$Petro4ABC, 1,1,1,0,1,1,12)$fit$residuals,lag=60)



# São piores os abaixo:
sarima(BRA_end$Petro4ABC, 1,1,2,0,1,1,12) 
sarima(BRA_end$Petro4ABC, 0,1,2,1,1,1,12) 

