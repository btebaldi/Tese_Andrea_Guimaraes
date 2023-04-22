#' Disertacao Andrea Guimaraes
#' 
#' Script para forecast dos items do IPCA
#' 
#' Author: Bruno Tebaldi de Queiroz Barbosa
#' 
#' Data: 2023-04-21
#' 


# Setup -------------------------------------------------------------------

# Clear All
rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

# Diretorio com as matrizes 
matrix_dir <- file.path("./Output Matrix/")



# Leitura das matrizes de coeficientes ------------------------------------

# Matrix de constantes
M_c <- readRDS(file = file.path(matrix_dir, "mGy_inv_X_mC.rds"))

# Seleciona apenas as colunas com a constante e dummies
mLag_Dm <- M_c[ ,1:12]

# Matriz com coeficientes de longo prazo
M_L <- readRDS(file = file.path(matrix_dir, "mGy_inv_X_mL.rds"))

# como nao temos modelo de longo prazo devemos ter todos os coeficientes iguais a zero
if(sum(M_L^2) == 0){
  rm(list = c("M_L"))  
} else{
  stop("Erro: A matriz deveria estar zerada")
}

# Carega as matrizes de lag-1 a lag-13
for(i in 1:13){
  
  # determina nome da variavel (a ser gravado no R) e nome do arquivo (a ser lido do diretorio)
  nome_arquivo <- sprintf("mGy_inv_X_mGyL%d.rds", i)
  nome_variavel <- sprintf("mLag_%02d", i)
  
  # apenas informa qual variavel esta sendo construida
  cat("COnstruindo a variavel:", nome_variavel, "\n")
  
  M <- readRDS(file = file.path(matrix_dir, nome_arquivo))
  
  assign(x = nome_variavel, value = M)
  
}

cat("Limpando variaveis nao utilizadas", "\n")
rm(list = c("M",  "M_c", "i", "nome_arquivo","nome_variavel", "matrix_dir"))



# Forecast dos lags de Y --------------------------------------------------

# Carregando a matrix de Y
forecast_base <- read_excel("C:/Users/bteba/Downloads/IPCA-baseOx_forecast.xlsx", na = c("#N/A"))
colnames(forecast_base)[1] <- "Period"


# convertendo para Matriz
Y <- forecast_base %>% select(-Period) %>% data.matrix()
row.names(Y) <- forecast_base$Period


# Forecast short run lag 1
FSR_01 <- Y %*% t(mLag_01)
FSR_02 <- Y %*% t(mLag_02)
FSR_03 <- Y %*% t(mLag_03)
FSR_04 <- Y %*% t(mLag_04)
FSR_05 <- Y %*% t(mLag_05)
FSR_06 <- Y %*% t(mLag_06)
FSR_07 <- Y %*% t(mLag_07)
FSR_08 <- Y %*% t(mLag_08)
FSR_09 <- Y %*% t(mLag_09)
FSR_10 <- Y %*% t(mLag_10)
FSR_11 <- Y %*% t(mLag_11)
FSR_12 <- Y %*% t(mLag_12)
FSR_13 <- Y %*% t(mLag_13)


# Ajuste dos lags.
total_rows <- nrow(Y)

idx <- which(row.names(Y) == "2021(1)")

FSR_01 <- FSR_01[(idx-1):(total_rows-1), ]
FSR_02 <- FSR_02[(idx-2):(total_rows-2), ]
FSR_03 <- FSR_03[(idx-3):(total_rows-3), ]
FSR_04 <- FSR_04[(idx-4):(total_rows-4), ]
FSR_05 <- FSR_05[(idx-5):(total_rows-5), ]
FSR_06 <- FSR_06[(idx-6):(total_rows-6), ]
FSR_07 <- FSR_07[(idx-7):(total_rows-7), ]
FSR_08 <- FSR_08[(idx-8):(total_rows-8), ]
FSR_09 <- FSR_09[(idx-9):(total_rows-9), ]
FSR_10 <- FSR_10[(idx-10):(total_rows-10), ]
FSR_11 <- FSR_11[(idx-11):(total_rows-11), ]
FSR_12 <- FSR_12[(idx-12):(total_rows-12), ]
FSR_13 <- FSR_13[(idx-13):(total_rows-13), ]

Forecast.SR = FSR_01 + FSR_02 + FSR_03 + FSR_04 +
  FSR_05 + FSR_06 + FSR_07 + FSR_08 +
  FSR_09 + FSR_10 + FSR_11 + FSR_12 + FSR_13

# Forecast.SR = FSR_01 + FSR_02 + FSR_03 + FSR_04

# regulariza o nome de colunas e linhas
colnames(Forecast.SR) <- colnames(Y)
row.names(Forecast.SR) <- row.names(Y[idx:total_rows, ])


# Forecast das variaveis de Dummies e Constante ---------------------------


# Construcao da matriz de Constante + Dummies
Dummies <- matrix(NA, nrow = 12, ncol = 1)
rownames(Dummies) <- c("CONST", "Seasonal", paste("Seasonal", 1:10, sep = ""))

# Coloca a Constante como sendo 1
Dummies[1,1] <- 1

# Matrix com o forecast das constantes + dummies
# Uma coluna para cada variavel e uma linha para cada periodo de forecast
Forecast.Dm <- matrix(NA, nrow = (total_rows - idx + 1), ncol = 379)

season <- 1 # A sazonalidade da amostra começa no periodo 5
for(i in seq_len(nrow(Forecast.Dm))) {
  
  # Determina o valor das dumies desligadas
  # Assume em um prmeiro momento que todas estao "desligadas" (ou seja season = 12)
  Dummies[2:12,1] <- 0-1/12 # Constante
  
  # Se a sazonalidade nao for no mes 12, liga o Dummie do mes correspondente
  if(season < 12){
    Dummies[season+1, 1] <- 1-1/12
  }
  
  # calcula o forecast para o periodo em questao
  Forecast.Dm[i, ] <- mLag_Dm %*% Dummies
  
  # Faz o incremento da sazonalidade
  if(season == 12){
    season <- 1
  } else{
    season <- season + 1
  }
}

# regulariza o nome de colunas e linhas
colnames(Forecast.Dm) <- colnames(Y)
row.names(Forecast.Dm) <- row.names(Y[idx:total_rows, ])


# Calcula forecast --------------------------------------------------------

Forecast <- Forecast.SR + Forecast.Dm

Actual <- Y[idx:total_rows, ]



# Avaliação ---------------------------------------------------------------

i <- 1
for(i in seq_len(ncol(Actual))){
  
  Series_name <- colnames(Actual)[i]
  dates <- rownames(Actual) %>% lubridate::ymd(truncated = 1)
  
  tbl <- tibble(date = dates,
                Actual = Actual[ , Series_name],
                Forecast = Forecast[ , Series_name])
  
  g1 <- ggplot(tbl) + 
    geom_line(aes(x = date, y = Actual, colour = "Actual")) + 
    geom_point(aes(x = date, y = Actual, colour = "Actual")) + 
    geom_line(aes(x = date, y = Forecast, colour = "Forecast")) + 
    geom_point(aes(x = date, y = Forecast, colour = "Forecast")) + 
    theme_bw() + 
    theme(legend.position = "bottom") +
    labs(title = sprintf("%s", Series_name),
         y=NULL,
         x=NULL,
         colour = NULL)
  
  
  ggsave(filename = sprintf("./Output Graphs/%s.png", Series_name),
         plot = g1,
         units = "in",
         width = 8, height = 6,
         dpi = 100)
  
}



















