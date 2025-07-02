# ========================================================
# A household electricity consumption
# ========================================================
# last update: 20/May/2025
#
# Time series analysis using the SARIMA model
#
#
# Raul Matsushita
# ========================================================

# ========================================================
# Load fonts & packages
# update.packages(checkBuilt=TRUE, ask=FALSE)
# ========================================================

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(extrafont)){
  install.packages("extrafont")
  library(extrafont)
}
#loadfonts(device = "win")
if(!require(openxlsx)){
  install.packages("openxlsx")
  library(openxlsx)
}
if(!require(TSA)){
  install.packages("TSA")
  library(TSA)
}
if(!require(tseries)){
  install.packages("tseries")
  library(tseries)
}
if(!require(astsa)){
  install.packages("astsa")
  library(astsa)
}

# ========================================================


# ========================================================
# Set the working folder
# ========================================================
setwd("C:\\Unb\\series-temporais\\seriestemporais-t2")

# ========================================================
# Data preparation
# ========================================================================

energia           <- read.xlsx("ConsumoEnergiaEAgua_New.xlsx", colNames = TRUE)
energia           <- as.data.frame(energia)
energia$mes       <- as.Date(as.numeric(energia$mes), origin = "1899-12-30")
energia$Consumo <- energia$Energia/energia$Dias

# ========================================================================

# ========================================================================
# Figure 1
# ========================================================================
Fig.energia <- ggplot(data = energia, aes(x = mes, y = Consumo)) +
  geom_line(linewidth=1.2) +
  ggtitle("Consumo kWh/dia") +
  xlab(expression(italic(t)))+
  ylab(expression(italic(Y[t])))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

Fig.energia


# ========================================================
# Sample ACF and Partial ACF
# ========================================================================

Y.t <- energia$Consumo

fac    <- acf(Y.t, lag = 40, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp    <- pacf(Y.t, lag = 40, plot = FALSE)
Fig.3 <- with(facp, data.frame(lag, acf))
Fig.3 <- rbind(c(0,1),Fig.3)
# ========================================================================

Fig.acf <- ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FAC da série original") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(rho)[italic(h)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

Fig.acf

Fig.2

Fig.pacf <- ggplot(data = Fig.3, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FACP da série original") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(phi)[italic(hh)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

Fig.pacf

Fig.3

# ========================================================================
# Testing first order nonstationarity
# ========================================================================

adf.test(Y.t, k=1)


# ========================================================================
# Differentiation
# ========================================================================

X.t           <- diff(energia$Consumo)
energia$dif <- c(NA,X.t)

# ========================================================================

Fig.dif <- ggplot(data = energia, mapping = aes(x = mes, y = dif)) +
  geom_line(size=1.2) +
  ggtitle("Variação do consumo (kWh)") +
  xlab(expression(italic(t)))+
  ylab(expression(paste(nabla)*italic(Y[t])))+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=1)+
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
        ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
        ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
  )
Fig.dif
# ========================================================================


# ========================================================
# Sample ACF and Partial ACF
# ========================================================================


fac    <- acf(X.t, lag = 60, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp    <- pacf(X.t, lag = 60, plot = FALSE)
Fig.3 <- with(facp, data.frame(lag, acf))
Fig.3 <- rbind(c(0,1),Fig.3)

# ========================================================================

Fig.acf <- ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FAC da primeira diferença") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(rho)[italic(h)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

conf.lim <- 2/sqrt(fac$n.used)
Fig.acf + geom_hline(aes(yintercept =  conf.lim), linetype="dotted",
                     size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")

# ========================================================================

Fig.2

Fig.pacf <- ggplot(data = Fig.3, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FACP da primeira diferença") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(phi)[italic(hh)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

Fig.pacf + geom_hline(aes(yintercept =  conf.lim), linetype="dotted",
                      size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")

Fig.3


# ========================================================================



# Training
# Model identification
# (Find the order of the model)
# ------------------------------------------
n.size          <- dim(energia)[1]
t.size          <- ceiling(3*n.size/4)
X.t             <- energia$Consumo[1:t.size]
# ------------------------------------------

# Just an example
draft <- sarima(X.t, 1, 1, 1,P = 1, D= 0, Q = 1, S=12, no.constant=TRUE, details = FALSE)

# Order determination process
# (saturated models with low orders)
# -----------------------------------------------------------------------------------------
BIC_AIC       <- NULL
grid    <- 0:4
Grid    <- 0:2
for (p.grid in grid){
  for (q.grid in grid){
    for (P.grid in Grid){
      for (Q.grid in Grid){
        # Verifica se o modelo sarima converge para evitar erros
        tryCatch({
          draft <-sarima( X.t, p.grid,  1,  q.grid,
                          P=P.grid,D=0,Q=Q.grid,S=12,
                          no.constant=TRUE,details=FALSE)
          
          BIC_AIC    <- rbind( BIC_AIC, c(BIC = unlist(draft[4])[3],
                                          AIC = unlist(draft[4])[2],
                                          p = p.grid, q = q.grid, P = P.grid, Q = Q.grid))
        }, error = function(e){
          # Opcional: imprimir mensagem para modelos que não convergiram
          # cat(paste0("Modelo SARIMA(", p.grid, ",1,", q.grid, ")(", P.grid, ",0,", Q.grid, ")12 não convergiu.\n"))
        })
      }}}}


# listing the top 10 models by BIC
# ----------------------------------
BIC_results <- data.frame(BIC_AIC)
BIC_results <- BIC_results[order(BIC_results$BIC.ICs.BIC),]
cat("Top 10 modelos por BIC:\n")
print(BIC_results[1:10,])
# ----------------------------------

# listing the top 10 models by AIC
# ----------------------------------
AIC_results <- data.frame(BIC_AIC)
AIC_results <- AIC_results[order(AIC_results$AIC.ICs.AIC),]
cat("\nTop 10 modelos por AIC:\n")
print(AIC_results[1:10,])
# ----------------------------------

# Definir o melhor modelo com base no BIC
best_model_bic_params <- BIC_results[1, c("p", "q", "P", "Q")]

# Definir o melhor modelo com base no AIC
best_model_aic_params <- AIC_results[1, c("p", "q", "P", "Q")]

cat("\nMelhor modelo (parâmetros) segundo BIC: (", paste(best_model_bic_params, collapse = ", "), ")\n")
cat("Melhor modelo (parâmetros) segundo AIC: (", paste(best_model_aic_params, collapse = ", "), ")\n")


# -----------------------------------------------------------------------------------------
# Assessing the best one (using BIC selected model as default for full validation)

fit <- sarima(X.t,best_model_bic_params$p,1,best_model_bic_params$q,
              P=best_model_bic_params$P,D=0,Q=best_model_bic_params$Q,S=12,no.constant=TRUE)
fit

# Ljung-Box Test
# ------------------
LB <- NULL
for (lag in 1:30)
{
  LB[lag] <- Box.test(resid(fit$fit), lag = lag, type='Ljung-Box')$p.value
}
data.frame(p.values = LB)

# Residual ACF and PACF
# ------------------

r.t <- resid(fit$fit)
fac    <- acf(r.t, lag = 60, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp    <- pacf(r.t, lag = 60, plot = FALSE)
Fig.3 <- with(facp, data.frame(lag, acf))
Fig.3 <- rbind(c(0,1),Fig.3)


# ========================================================================

Fig.acf <- ggplot(data = Fig.2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FAC residual") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(rho)[italic(h)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

conf.lim <- 2/sqrt(fac$n.used)
Fig.acf + geom_hline(aes(yintercept =  conf.lim), linetype="dotted",
                     size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")

# ========================================================================

Fig.2

Fig.pacf <- ggplot(data = Fig.3, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), size=2)+
  ggtitle("FACP residual") +
  xlab(expression(italic(h)))+
  ylab(expression(paste(phi)[italic(hh)]))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

Fig.pacf + geom_hline(aes(yintercept =  conf.lim), linetype="dotted",
                      size = 2, color = "red") +
  geom_hline(aes(yintercept = -conf.lim), linetype="dotted",
             size = 2, color = "red")

Fig.3



# normality test
# ----------------------------------------------
#residuals <-  as.numeric(unlist(fit$fit[8]))
# ----------------------------------------------
shapiro.test(r.t)
# ----------------------------------------------

# quantis <- quantile(na.omit(residuals), c(0.025, 0.975)) # Esta linha não é usada e pode ser removida


# ------------------------------------------------------------
# Model Validation (Out of sample) and RMSE Calculation
# ------------------------------------------------------------

# Função para calcular RMSE
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

# --- Avaliando o modelo selecionado pelo BIC ---
cat("\n--- Avaliação do Modelo selecionado pelo BIC ---\n")
# Ajustar o modelo com base nos parâmetros do BIC usando os dados de treinamento
# para obter resíduos (para cálculo do RMSE "in-sample" ou para ajuste completo)
fit_bic <- sarima(energia$Consumo[1:t.size],
                  best_model_bic_params$p, 1, best_model_bic_params$q,
                  P=best_model_bic_params$P, D=0, Q=best_model_bic_params$Q, S=12,
                  no.constant=TRUE, details=FALSE)

# Gerar previsões para a parte out-of-sample usando o modelo treinado
# Aqui estamos prevendo os últimos (n.size - t.size) pontos, que são os dados de teste
n.test_size <- n.size - t.size
preds_bic <- sarima.for(as.ts(energia$Consumo[1:t.size]),
                        n.ahead = n.test_size,
                        best_model_bic_params$p, 1, best_model_bic_params$q,
                        P=best_model_bic_params$P, D=0, Q=best_model_bic_params$Q, S=12,
                        no.constant=TRUE, plot=FALSE) # plot=FALSE para não gerar gráfico intermediário

# Dados reais da parte de teste
actual_test_data <- energia$Consumo[(t.size + 1):n.size]

# Previsões para a parte de teste
predicted_test_data_bic <- as.numeric(preds_bic$pred)

# Calcular RMSE para o modelo do BIC
rmse_bic <- calculate_rmse(actual_test_data, predicted_test_data_bic)
cat("RMSE (Out-of-sample) para o modelo BIC: ", round(rmse_bic, 3), "\n")


# --- Avaliando o modelo selecionado pelo AIC ---
cat("\n--- Avaliação do Modelo selecionado pelo AIC ---\n")
# Ajustar o modelo com base nos parâmetros do AIC
fit_aic <- sarima(energia$Consumo[1:t.size],
                  best_model_aic_params$p, 1, best_model_aic_params$q,
                  P=best_model_aic_params$P, D=0, Q=best_model_aic_params$Q, S=12,
                  no.constant=TRUE, details=FALSE)

# Gerar previsões para a parte out-of-sample
preds_aic <- sarima.for(as.ts(energia$Consumo[1:t.size]),
                        n.ahead = n.test_size,
                        best_model_aic_params$p, 1, best_model_aic_params$q,
                        P=best_model_aic_params$P, D=0, Q=best_model_aic_params$Q, S=12,
                        no.constant=TRUE, plot=FALSE)

# Previsões para a parte de teste
predicted_test_data_aic <- as.numeric(preds_aic$pred)

# Calcular RMSE para o modelo do AIC
rmse_aic <- calculate_rmse(actual_test_data, predicted_test_data_aic)
cat("RMSE (Out-of-sample) para o modelo AIC: ", round(rmse_aic, 3), "\n")

# Comparação final
cat("\n--- Comparação Final ---\n")
cat("RMSE do Melhor Modelo (BIC): ", round(rmse_bic, 3), "\n")
cat("RMSE do Melhor Modelo (AIC): ", round(rmse_aic, 3), "\n")

if (rmse_bic < rmse_aic) {
  cat("O modelo selecionado pelo BIC teve um RMSE menor, indicando melhor capacidade preditiva.\n")
} else if (rmse_aic < rmse_bic) {
  cat("O modelo selecionado pelo AIC teve um RMSE menor, indicando melhor capacidade preditiva.\n")
} else {
  cat("Os modelos selecionados por BIC e AIC tiveram RMSEs iguais.\n")
}


# Continuando com o forecasting (usando o modelo BIC como no script original para a figura de previsão)
# Para a figura de previsão, vamos usar o modelo escolhido pelo BIC,
# mas você poderia facilmente trocar para o AIC se ele se mostrar melhor no RMSE.

# O bloco abaixo é o que você já tinha para o forecast final e plotagem.
# Ajustar o modelo ao conjunto de dados completo para a previsão final
fit_final_forecast <- sarima(energia$Consumo,
                             best_model_bic_params$p, 1, best_model_bic_params$q,
                             P=best_model_bic_params$P, D=0, Q=best_model_bic_params$Q, S=12,
                             no.constant=TRUE)


# ----------------------------------------------
# Forecasting for the last 12 months (actual out-of-sample forecast)
# ----------------------------------------------
# Note: This part re-runs sarima.for.
# The previous RMSE calculation used the training set to forecast the test set.
# This section uses almost the full dataset (all but last 12 months) to forecast the true future 12 months.

fit_forecast_plot <- sarima.for(as.ts(energia$Consumo[1:(n.size-12)]),n.ahead = 12,
                                best_model_bic_params$p,1,best_model_bic_params$q,
                                P=best_model_bic_params$P,D=0,Q=best_model_bic_params$Q,S=12,
                                no.constant=TRUE, plot.all = TRUE)
X.t    <- c(energia$Consumo[(n.size - 23):(n.size-12)], rep(NA,12))
X.hat <- c(rep(NA,12), as.numeric(fit_forecast_plot$pred))
se    <- c(rep(NA,12), as.numeric(fit_forecast_plot$se))
mes    <- seq(1:length(X.t))
Preds <- data.frame(mes,X.t,X.hat,se)

# ========================================================================
# Figure pred
# ========================================================================
Fig.pred <- ggplot(data = Preds, aes(x = mes, y = X.t)) +
  geom_line(linewidth=1.2) +
  ggtitle("Consumo kWh/dia (últimos 12 meses)") +
  xlab(expression(italic(t)))+
  ylab(expression(italic(Y[t])))+
  theme(text=element_text(family="Times New Roman",size=21,colour="black")
        ,axis.title.y=element_text(family="Times New Roman",angle=90,size=22,
                                   colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,
                                                                            unit = "pt")),axis.title.x = element_text(family = "Times New Roman",
                                                                                                                      vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
        ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,
                                   colour="black"))
# ========================================================================

new.points <- data.frame(x = seq(13,24), y = energia$Consumo[(n.size - 11):(n.size)])

Fig.pred +
  geom_line(data=Preds, aes(x = mes, y = X.hat), linewidth=1.2, col = "red") +
  geom_ribbon(data=Preds,aes(ymin=X.hat -2*se,ymax=X.hat +2*se), fill="grey", alpha=0.5)+
  geom_point(data=new.points,aes(x = x, y = y), col = "black", size = 2)
