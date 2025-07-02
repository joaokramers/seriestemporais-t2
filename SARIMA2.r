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
setwd("C:/Users/raulm/OneDrive/XPS/_Datasets/EnergiaEAgua")

# ========================================================
# Data preparation
# ========================================================================

energia         <- read.xlsx("ConsumoEnergiaEAgua.xlsx", colNames = TRUE)
energia         <- as.data.frame(energia)
energia$mes     <- as.Date(as.numeric(energia$mes), origin = "1899-12-30")
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

fac   <- acf(Y.t, lag = 40, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp   <- pacf(Y.t, lag = 40, plot = FALSE)
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
       ylab(expression(paste(rho)[italic(h)]))+
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

X.t         <- diff(energia$Consumo)
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


fac   <- acf(X.t, lag = 60, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp   <- pacf(X.t, lag = 60, plot = FALSE)
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
BIC     <- NULL
grid    <- 0:4
Grid    <- 0:2
for (p.grid in grid){
BIC.col <- NULL
for (q.grid in grid){
for (P.grid in Grid){
for (Q.grid in Grid){
draft <-sarima( X.t, p.grid,  1,  q.grid,
                     P=P.grid,D=0,Q=Q.grid,S=12,
                no.constant=TRUE,details=FALSE)

BIC   <- rbind( BIC, c(BIC = unlist(draft[4])[3], 
         p = p.grid, q = q.grid, P = P.grid, Q = Q.grid))
}}}}


# listing the top 10 models
# ----------------------------------
BIC <- data.frame(BIC) 
BIC <- BIC[order(BIC$BIC.ICs.BIC),]
BIC[1:10,]
# ----------------------------------



# -----------------------------------------------------------------------------------------
# Assessing the best one

fit <- sarima(X.t,BIC[1,2],1,BIC[1,3],P=BIC[1,4],D=0,Q=BIC[1,5],S=12,no.constant=TRUE)
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
fac   <- acf(r.t, lag = 60, plot = FALSE)
Fig.2 <- with(fac, data.frame(lag, acf))
Fig.2 <- rbind(c(0,1),Fig.2)

facp   <- pacf(r.t, lag = 60, plot = FALSE)
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

quantis <- quantile(na.omit(residuals), c(0.025, 0.975))

# ------------------------------------------------------------
# Model Validation (Out of sample)
# ------------------------------------------------------------
fit <- sarima(energia$Consumo,BIC[1,2],1,BIC[1,3],P=BIC[1,4],D=0,Q=BIC[1,5],S=12,no.constant=TRUE)
fit
r.t <- resid(fit$fit)
energia$X.hat     <- energia$Consumo - r.t
energia$X.res     <- r.t
energia$X.hat[1:t.size] <- NA
energia$X.res[1:t.size] <- r.t

Fig.energia + geom_line(data=energia,mapping = aes(x = mes, y = X.hat), linewidth=1.2, col = "red")

# Out of sample performance
#--------------------------
(MAPE <- mean(abs((energia$X.hat-energia$Consumo)/energia$Consumo),na.rm=T)*100)

# ----------------------------------------------
# Forecasting
# ----------------------------------------------
fit <- sarima.for(as.ts(energia$Consumo[1:(n.size-12)]),n.ahead = 12, 
BIC[1,2],1,BIC[1,3],P=BIC[1,4],D=0,Q=BIC[1,5],S=12,
no.constant=TRUE, plot.all = TRUE)
X.t   <- c(energia$Consumo[(n.size - 23):(n.size-12)], rep(NA,12))
X.hat <- c(rep(NA,12), as.numeric(fit$pred))
se    <- c(rep(NA,12), as.numeric(fit$se))
mes   <- seq(1:length(X.t))
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
#geom_line(data=Preds, aes(x = mes, y = X.hat +2*se), linetype="dotted",  color = "blue", size=1.5) +
#geom_line(data=Preds, aes(x = mes, y = X.hat -2*se), linetype="dotted",  color = "blue", size=1.5) +
geom_ribbon(data=Preds,aes(ymin=X.hat -2*se,ymax=X.hat +2*se), fill="grey", alpha=0.5)+
geom_point(data=new.points,aes(x = x, y = y), col = "black", size = 2)

