# ========================================================
# A household electricity consumption
# ========================================================
# last update: 02/Apr/2025
# 
# Time series analysis using local trends
#
# Década de 1920-30: já se usavam médias móveis (moving averages)
# de forma empírica para suavizar séries econômicas e financeiras.
# Quase 100 anos... mas quebra o galho
#
# Raul Matsushita
# ========================================================

# Parte I (Leitura dos Dados)

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

# ========================================================


# ========================================================
# Set the working folder
# ========================================================
setwd("C:/Users/raulm/OneDrive/XPS/_Datasets/EnergiaEAgua")

# ========================================================
# Load our library
# (change the path accordingly)
# ========================================================
setwd("C:/Users/raulm/OneDrive/XPS/_Datasets/EnergiaEAgua")
source("Smoothing.txt")

# ========================================================

energia <- as.data.frame(read.xlsx("ConsumoEnergiaEAgua.xlsx", colNames = TRUE))
energia$mes <- as.Date(as.numeric(energia$mes), origin = "1899-12-30")
energia$Consumo <- energia$Energia/energia$Dias

Fig.energia <- ggplot(data = energia, mapping = aes(x = mes, y = Consumo)) +
  geom_line(linewidth=1.2) +
  ggtitle("Consumo kWh/dia") +
  xlab(expression(italic(t)))+
  ylab(expression(italic(Y[t])))+
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
 ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
 ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
 ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
   ) 
Fig.energia



# ========================================================
# Parte II (Suavização dos dados por médias, q.poly <- 0,
#           ou tendências móveis, q.poly <- 1, considerando
#           erro de previsão um passo à frente, h.pred <- 1)
#
# ========================================================


Y.t    <- energia$Consumo

# busca por uma janela ótima
# --------------------------
mse    <- NULL
mape   <- NULL
q.poly <- 0
h.pred <- 1
w.grid <- 2 : 20
w.last <- 24     # avaliação nos últimos w.last meses
for (w.size in w.grid)
{
resultado   <- na.omit(PolyMovingWindow(Y.t, w.size, q.poly, h.pred))
n.max       <- dim(resultado)[1]
resultado   <- resultado[(n.max - w.last):n.max,]
error       <- resultado$error
error.r     <- error/resultado$Y.t
# performance do modelo na massa de validação ou de teste

mse[w.size] <- mean((error^2),na.rm = TRUE)
mape[w.size] <- 100*mean(abs(error.r),na.rm = TRUE)
}


figura <- data.frame(mse, mape, w.grid = 1 : max(w.grid))
ggplot(data = figura, mapping = aes(x = w.grid, y = mse)) +
  geom_line(linewidth=1.2) +
  ggtitle(paste("mse com q = ", q.poly))+
  xlab(expression(italic(m)))+
  ylab(expression(italic(mse[m])))+
scale_x_continuous(breaks = seq(2,max(w.grid),by = 2)) +
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
 ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
 ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
 ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
   ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste0("mínimo: ", round(min(figura$mse,na.rm = TRUE), 2)),
    hjust = 1.1, vjust = 1.5,
    size = 4, fontface = "italic"
  )

ggplot(data = figura, mapping = aes(x = w.grid, y = mape)) +
  geom_line(linewidth=1.2) +
  ggtitle(paste("mape com q = ", q.poly))+
  xlab(expression(italic(m)))+
  ylab(expression(italic(mape[m])))+
scale_x_continuous(breaks = seq(2,max(w.grid),by = 2)) +
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
 ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
 ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
 ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
   ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste0("mínimo: ", round(min(figura$mape,na.rm = TRUE), 2)),
    hjust = 1.1, vjust = 1.5,
    size = 4, fontface = "italic"
  )

figura
# ------------------------------------------------------------------------------------------

consumo      <- PolyMovingWindow(Y.t, 6, 0, 1)

Fig.energia <- ggplot(data = consumo, mapping = aes(x = t, y = Y.t)) +
  geom_line(linewidth=1.2) +
  ggtitle("Consumo kWh/dia") +
  xlab(expression(italic(t)))+
  ylab(expression(italic(Y[t])))+
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
 ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
 ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
 ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
 ,legend.position="none"
   ) 
Fig.energia

Fig.energia +
geom_line(data = consumo, mapping = aes(x = t, y = Y.hat), colour = "red", linewidth=1.2) +
geom_vline(xintercept = (length(Y.t) - w.last), color = "blue", linetytpe = "dotted")


ggplot(data = consumo, mapping = aes(x = t, y = error)) +
  geom_line(linewidth=1.2) +
  ggtitle("resíduo (kWh)") +
  xlab(expression(italic(t)))+
  ylab(expression(italic(a[t])))+
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size=1)+
  theme(text = element_text(family = "Times New Roman", size=21,colour="black")
 ,axis.title.y = element_text(family = "Times New Roman", angle=90, size=22, colour="black", vjust=0.5, margin=margin(t = 0, r = 15, b = 0, l = 0,  unit = "pt"))
 ,axis.title.x = element_text(family = "Times New Roman",vjust=-1.5, margin=margin(t = 0, r = 0, b = 15, l = 0,  unit = "pt"))
 ,plot.title = element_text(family = "Times New Roman", hjust = 0.5,colour="black")
   ) 

acf(na.omit(consumo$error),lag=36)

# Conclusão do Experimento:
# ---------------------------------------------------------
# Método não robusto (sensível) para diferentes tamanhos de
# de janelas móveis e escolha da ordem do polinômio móvel.
# As medidas para avaliação do erro de previsão (mse e mape)
# dependem do tamanho da massa de dados para teste (últimas
# w.last observações da serie).
# Os resíduos apresentam correlações, o que indica possibilidade
# de extração de informações ainda escondidas na série residual.
# O método se restringe a previsões locais, não apresentando 
# resultados satisfatórios para previsões de horizontes maiores do que
# 1.
# -------------------------------------------------------------
