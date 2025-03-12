#Rafael Ribeiro
#Cedeplar/UFMG e Made/USP

#######################################################
### IMPACTO DA POLITICA MONETARIA POR GRUPO DO IPCA ###
#######################################################


# Instalar pacotes -------------------------------------------------------------
{
  install.packages("GetBCBData")
  install.packages("ipeadatar")
  install.packages("sidrar")
  install.packages("tidyverse")
  install.packages("stringr")
  install.packages("zoo")
  install.packages("readr")
  install.packages('readxl')
  install.packages("openxlsx")
  install.packages("mFilter")
  install.packages("seasonal")
  install.packages("urca")
  install.packages("vars")
  install.packages("lpirf")
  install.packages("ggcorrplot")
  install.packages("gridExtra")
}

{
  library(GetBCBData)
  library(ipeadatar)
  library(sidrar)
  library(tidyverse)
  library(stringr)
  library(zoo)
  library(readr)
  library(openxlsx)
  library(readxl)
  library(mFilter)
  library(seasonal)
  library(urca)
  library(vars)
  library(lpirfs)
  library(ggcorrplot)
  library(gridExtra)
}

{
  #Definir diretório de trabalho
  
  #Diretório
  setwd("C:\\Users\\Admin\\Documents\\Projetos\\Made\\Projeto inflação\\1. Inflação por grupos IPCA")
  
  # Remover todos objetos do Environment
  rm(list = ls())
}

# Importar dados do IPCA -------------------------------------------------------

{
  # Importando os dados no formato rdata
  load("dados.RData")
  View(dados)
}

# Local Projections ------------------------------------------------------------

# IPCA Geral (Cod. 7169)
{
  data_7169 <- dados %>%
    dplyr::select(p7169, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7169 <- lp_lin(data_7169,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7169)
#plot_lin(modelo_7169)[[5]]

plot_7169 <- 
  data.frame(h = 0:24,
             mean = modelo_7169$irf_lin_mean[1,,5],
             low = modelo_7169$irf_lin_low[1,,5],
             up = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = mean), colour="#3366ff", linetype="solid", linewidth=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#00BCE3", fill="#00BCE3", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "Inflação geral",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7169
#ggsave("plot_7169.png", plot_7169, width=6, height=4)

# 1. Alimentação e bebidas (Cod. 7170)
{
  data_7170 <- dados %>%
    dplyr::select(p7170, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7170 <- lp_lin(data_7170,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7170)
#plot_lin(modelo_7170)[[5]]

plot_7170 <- 
  data.frame(h = 0:24,
             mean = modelo_7170$irf_lin_mean[1,,5],
             low = modelo_7170$irf_lin_low[1,,5],
             up = modelo_7170$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "1. Inflação de alimentação e bebidas",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7170


# 1.1 Alimentação no domicílio (Cod. 7171)
{
  data_7171 <- dados %>%
    dplyr::select(p7171, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7171 <- lp_lin(data_7171,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7171)
#plot_lin(modelo_7171)[[5]]

plot_7171 <- 
  data.frame(h = 0:24,
             mean = modelo_7171$irf_lin_mean[1,,5],
             low = modelo_7171$irf_lin_low[1,,5],
             up = modelo_7171$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "1.1 Inflação de alimentação no domicílio",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7171
#ggsave("plot_7171.png", plot_7171, width=6, height=4)

# 1.2 Alimentação fora do domicílio (Cod. 7432)
{
  data_7432 <- dados %>%
    dplyr::select(p7432, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7432 <- lp_lin(data_7432,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7432)
#plot_lin(modelo_7432)[[5]]

plot_7432 <- 
  data.frame(h = 0:24,
             mean = modelo_7432$irf_lin_mean[1,,5],
             low = modelo_7432$irf_lin_low[1,,5],
             up = modelo_7432$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "1.2 Inflação de alimentação fora do domicílio",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7432
#ggsave("plot_7432.png", plot_7432, width=6, height=4)

# 2. Habitação (Cod. 7445)
{
  data_7445 <- dados %>%
    dplyr::select(p7445, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7445 <- lp_lin(data_7445,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7445)
#plot_lin(modelo_7445)[[5]]

plot_7445 <- 
  data.frame(h = 0:24,
             mean = modelo_7445$irf_lin_mean[1,,5],
             low = modelo_7445$irf_lin_low[1,,5],
             up = modelo_7445$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "2. Inflação de habitação",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7445


# 2.1 Encargos e manutenção (Cod. 7446)
{
  data_7446 <- dados %>%
    dplyr::select(p7446, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7446 <- lp_lin(data_7446,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7446)
#plot_lin(modelo_7446)[[5]]

plot_7446 <- 
  data.frame(h = 0:24,
             mean = modelo_7446$irf_lin_mean[1,,5],
             low = modelo_7446$irf_lin_low[1,,5],
             up = modelo_7446$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.22,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "2.1 Inflação de encargos e manutenção",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7446


# 2.2 Combustíveis e energia (Cod. 7479)
{
  data_7479 <- dados %>%
    dplyr::select(p7479, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7479 <- lp_lin(data_7479,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7479)
#plot_lin(modelo_7479)[[5]]

plot_7479 <- 
  data.frame(h = 0:24,
             mean = modelo_7479$irf_lin_mean[1,,5],
             low = modelo_7479$irf_lin_low[1,,5],
             up = modelo_7479$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.22,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "2.2 Inflação de combustíveis e energia",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7479


# 3. Artigos de residência (Cod. 7486)
{
  data_7486 <- dados %>%
    dplyr::select(p7486, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7486 <- lp_lin(data_7486,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7486)
#plot_lin(modelo_7486)[[5]]

plot_7486 <- 
  data.frame(h = 0:24,
             mean = modelo_7486$irf_lin_mean[1,,5],
             low = modelo_7486$irf_lin_low[1,,5],
             up = modelo_7486$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "3. Inflação de artigos de residência",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7486


# 3.1 Moveis e utensilios (Cod. 7487)
{
  data_7487 <- dados %>%
    dplyr::select(p7487, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7487 <- lp_lin(data_7487,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7487)
#plot_lin(modelo_7487)[[5]]

plot_7487 <- 
  data.frame(h = 0:24,
             mean = modelo_7487$irf_lin_mean[1,,5],
             low = modelo_7487$irf_lin_low[1,,5],
             up = modelo_7487$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "3.1 Inflação de móveis e utensílios",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7487


# 3.2 aparelhos eletroeletronicos (Cod. 7521)
{
  data_7521 <- dados %>%
    dplyr::select(p7521, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7521 <- lp_lin(data_7521,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7521)
#plot_lin(modelo_7521)[[5]]

plot_7521 <- 
  data.frame(h = 0:24,
             mean = modelo_7521$irf_lin_mean[1,,5],
             low = modelo_7521$irf_lin_low[1,,5],
             up = modelo_7521$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "3.2 Inflação de aparelhos eletroeletrônicos",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7521


# 3.3 consertos e manutenção (Cod. 7548)
{
  data_7548 <- dados %>%
    dplyr::select(p7548, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7548 <- lp_lin(data_7548,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7548)
#plot_lin(modelo_7548)[[5]]

plot_7548 <- 
  data.frame(h = 0:24,
             mean = modelo_7548$irf_lin_mean[1,,5],
             low = modelo_7548$irf_lin_low[1,,5],
             up = modelo_7548$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "3.3 Inflação de consertos e manutenção",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7548


# 4. Vestuário (Cod. 7558)
{
  data_7558 <- dados %>%
    dplyr::select(p7558, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7558 <- lp_lin(data_7558,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7558)
#plot_lin(modelo_7558)[[5]]

plot_7558 <- 
  data.frame(h = 0:24,
             mean = modelo_7558$irf_lin_mean[1,,5],
             low = modelo_7558$irf_lin_low[1,,5],
             up = modelo_7558$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4. Inflação de vestuário",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7558


# 4.1 Roupas (Cod. 7559)
{
  data_7559 <- dados %>%
    dplyr::select(p7559, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7559 <- lp_lin(data_7559,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7559)
#plot_lin(modelo_7559)[[5]]

plot_7559 <- 
  data.frame(h = 0:24,
             mean = modelo_7559$irf_lin_mean[1,,5],
             low = modelo_7559$irf_lin_low[1,,5],
             up = modelo_7559$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4.1 Inflação de roupas",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7559


# 4.2 Calçados e acessórios (Cod. 7604)
{
  data_7604 <- dados %>%
    dplyr::select(p7604, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7604 <- lp_lin(data_7604,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7604)
#plot_lin(modelo_7604)[[5]]

plot_7604 <- 
  data.frame(h = 0:24,
             mean = modelo_7604$irf_lin_mean[1,,5],
             low = modelo_7604$irf_lin_low[1,,5],
             up = modelo_7604$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4.2 Inflação de calçados e acessórios",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7604


# 4.3 Joias e bijuterias (Cod. 7615)
{
  data_7615 <- dados %>%
    dplyr::select(p7615, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7615 <- lp_lin(data_7615,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7615)
#plot_lin(modelo_7615)[[5]]

plot_7615 <- 
  data.frame(h = 0:24,
             mean = modelo_7615$irf_lin_mean[1,,5],
             low = modelo_7615$irf_lin_low[1,,5],
             up = modelo_7615$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4.3 Inflação de joias e bijuterias",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7615


# 4.4 Tecidos e armarinho (Cod. 7620)
{
  data_7620 <- dados %>%
    dplyr::select(p7620, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7620 <- lp_lin(data_7620,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7620)
#plot_lin(modelo_7620)[[5]]

plot_7620 <- 
  data.frame(h = 0:24,
             mean = modelo_7620$irf_lin_mean[1,,5],
             low = modelo_7620$irf_lin_low[1,,5],
             up = modelo_7620$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4.4 Inflação de tecidos e armarinho",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7620


# 5. Transportes (Cod. 7625)
{
  data_7625 <- dados %>%
    dplyr::select(p7625, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7625 <- lp_lin(data_7625,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7625)
#plot_lin(modelo_7625)[[5]]

plot_7625 <- 
  data.frame(h = 0:24,
             mean = modelo_7625$irf_lin_mean[1,,5],
             low = modelo_7625$irf_lin_low[1,,5],
             up = modelo_7625$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "5. Inflação de transportes",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7625


# 5.1 Transportes (Cod. 7626)
{
  data_7626 <- dados %>%
    dplyr::select(p7626, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7626 <- lp_lin(data_7626,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7626)
#plot_lin(modelo_7626)[[5]]

plot_7626 <- 
  data.frame(h = 0:24,
             mean = modelo_7626$irf_lin_mean[1,,5],
             low = modelo_7626$irf_lin_low[1,,5],
             up = modelo_7626$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "5.1 Inflação de transportes",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7626


# 6. Saúde e cuidados pessoais (Cod. 7660)
{
  data_7660 <- dados %>%
    dplyr::select(p7660, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7660 <- lp_lin(data_7660,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7660)
#plot_lin(modelo_7660)[[5]]

plot_7660 <- 
  data.frame(h = 0:24,
             mean = modelo_7660$irf_lin_mean[1,,5],
             low = modelo_7660$irf_lin_low[1,,5],
             up = modelo_7660$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "6. Inflação de saúde e cuidados pessoais",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7660


# 6.1 Produtos farmacêuticos e óticos (Cod. 7661)
{
  data_7661 <- dados %>%
    dplyr::select(p7661, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7661 <- lp_lin(data_7661,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7661)
#plot_lin(modelo_7661)[[5]]

plot_7661 <- 
  data.frame(h = 0:24,
             mean = modelo_7661$irf_lin_mean[1,,5],
             low = modelo_7661$irf_lin_low[1,,5],
             up = modelo_7661$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "6.1 Inflação de produtos farmacêuticos e óticos",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7661


# 6.2 Serviços de saúde (Cod. 7683)
{
  data_7683 <- dados %>%
    dplyr::select(p7683, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7683 <- lp_lin(data_7683,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7683)
#plot_lin(modelo_7683)[[5]]

plot_7683 <- 
  data.frame(h = 0:24,
             mean = modelo_7683$irf_lin_mean[1,,5],
             low = modelo_7683$irf_lin_low[1,,5],
             up = modelo_7683$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "6.2 Inflação de serviços de saúde",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7683


# 6.3 Cuidados pessoais (Cod. 7697)
{
  data_7697 <- dados %>%
    dplyr::select(p7697, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7697 <- lp_lin(data_7697,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7697)
#plot_lin(modelo_7697)[[5]]

plot_7697 <- 
  data.frame(h = 0:24,
             mean = modelo_7697$irf_lin_mean[1,,5],
             low = modelo_7697$irf_lin_low[1,,5],
             up = modelo_7697$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "6.3 Inflação de cuidados pessoais",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7697


# 7. Despesas pessoais (Cod. 7712)
{
  data_7712 <- dados %>%
    dplyr::select(p7712, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7712 <- lp_lin(data_7712,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7712)
#plot_lin(modelo_7712)[[5]]

plot_7712 <- 
  data.frame(h = 0:24,
             mean = modelo_7712$irf_lin_mean[1,,5],
             low = modelo_7712$irf_lin_low[1,,5],
             up = modelo_7712$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "7. Inflação de despesas pessoais",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7712


# 7.1 Serviços pessoais (Cod. 7713)
{
  data_7713 <- dados %>%
    dplyr::select(p7713, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7713 <- lp_lin(data_7713,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7713)
#plot_lin(modelo_7713)[[5]]

plot_7713 <- 
  data.frame(h = 0:24,
             mean = modelo_7713$irf_lin_mean[1,,5],
             low = modelo_7713$irf_lin_low[1,,5],
             up = modelo_7713$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "7.1 Inflação de serviços pessoais",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7713

# 7.201 Recreação (Cod. 7730)
{
  data_7730 <- dados %>%
    dplyr::select(p7730, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7730 <- lp_lin(data_7730,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7730)
#plot_lin(modelo_7730)[[5]]

plot_7730 <- 
  data.frame(h = 0:24,
             mean = modelo_7730$irf_lin_mean[1,,5],
             low = modelo_7730$irf_lin_low[1,,5],
             up = modelo_7730$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "7.201 Inflação de recreação",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7730


# 8. Educação (Cod. 7766)
{
  data_7766 <- dados %>%
    dplyr::select(p7766, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7766 <- lp_lin(data_7766,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7766)
#plot_lin(modelo_7766)[[5]]

plot_7766 <- 
  data.frame(h = 0:24,
             mean = modelo_7766$irf_lin_mean[1,,5],
             low = modelo_7766$irf_lin_low[1,,5],
             up = modelo_7766$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "8. Inflação de educação",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7766


# 8.1 Cursos, leitura e papelaria (Cod. 7767)
{
  data_7767 <- dados %>%
    dplyr::select(p7767, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7767 <- lp_lin(data_7767,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7767)
#plot_lin(modelo_7767)[[5]]

plot_7767 <- 
  data.frame(h = 0:24,
             mean = modelo_7767$irf_lin_mean[1,,5],
             low = modelo_7767$irf_lin_low[1,,5],
             up = modelo_7767$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "8.1 Inflação de cursos, leitura e papelaria",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7767


# 9. Comunicação (Cod. 7786)
{
  data_7786 <- dados %>%
    dplyr::select(p7786, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7786 <- lp_lin(data_7786,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7786)
#plot_lin(modelo_7786)[[5]]

plot_7786 <- 
  data.frame(h = 0:24,
             mean = modelo_7786$irf_lin_mean[1,,5],
             low = modelo_7786$irf_lin_low[1,,5],
             up = modelo_7786$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="#FFAB4D", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "gold", fill="gold", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "9. Inflação de comunicação",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7786


# 9.1 Comunicação (Cod. 7787)
{
  data_7787 <- dados %>%
    dplyr::select(p7787, cambio, ulc, ibc.gap, selic)
  
  exog_data <- dados %>%
    dplyr::select(commodity, ipca_adm)
}

modelo_7787 <- lp_lin(data_7787,
                      lags_endog_lin = 1,
                      exog_data      = exog_data,
                      lags_exog      = 1,
                      trend          = 0,
                      shock_type     = 1,
                      use_nw         = T,
                      confint        = 1.96,
                      hor            = 24)
#plot(modelo_7787)
#plot_lin(modelo_7787)[[5]]

plot_7787 <- 
  data.frame(h = 0:24,
             mean = modelo_7787$irf_lin_mean[1,,5],
             low = modelo_7787$irf_lin_low[1,,5],
             up = modelo_7787$irf_lin_up[1,,5],
             meanb = modelo_7169$irf_lin_mean[1,,5],
             lowb = modelo_7169$irf_lin_low[1,,5],
             upb = modelo_7169$irf_lin_up[1,,5]) %>% 
  ggplot() +
  geom_line(aes(x = h, y = meanb), colour="grey", linetype="dashed", size=0.7) +
  geom_ribbon(aes(x = h, ymin = lowb, ymax = upb),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = h, y = mean), colour="darkseagreen4", linetype="solid", size=1.5) +
  geom_ribbon(aes(x = h, ymin = low, ymax = up),
              colour = "#45ff66", fill="#45ff66", alpha=0.3) +
  scale_y_continuous(limits = c(-0.2,0.2)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0,24,4)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "9.1 Inflação de comunicação",
       subtitle = "(resposta a um aumento na Selic, em p.p)") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold"),
        plot.subtitle = element_text(size = 10, hjust=0)); plot_7787

# Gerando gráficos por grupos --------------------------------------------------

# Dados anuais de inflação por grupos
dados_infl_anual <- 
  read_excel("Dados_infl_anual.xlsx", col_names = TRUE) %>%
  mutate(data = format(as.Date(paste(2001:2023, "01", "01",sep="-"), format="%Y-%m-%d"), "%Y")) %>%
  filter(data >= '2007')
View(dados_infl_anual)

# Tabela descritiva
install.packages("vtable")
library(vtable)

dados_table <- dados_infl_anual[,-c(1,3,4)]
colnames(dados_table) <- c("Meta de inflação","Inflação agregada","Alimentação e bebidas",
                           "Habitação","Artigos de residência","Vestuário","Transportes",
                           "Saúde e cuidados pessoais","Despesas pessoais",
                           "Educação","Comunicação")
st(dados_table, out = 'csv', file = 'mytable.csv')

# Visualização dos dados anuais de inflação por grupos
plot_y7170 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7170, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "1. Inflação de alimentação e bebidas") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7170

plot_y7445 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) +
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7445, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "2. Inflação de habitação") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7445

plot_y7486 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7486, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "3. Inflação de artigos de residência") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7486

plot_y7558 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7558, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "4. Inflação de vestuário") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7558

plot_y7625 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7625, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "5. Inflação de transportes") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7625

plot_y7660 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7660, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "6. Inflação de saúde e cuidados pessoais") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7660

plot_y7712 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7712, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "7. Inflação de despesas pessoais") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7712

plot_y7766 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7766, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "8. Inflação de educação") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7766

plot_y7786 <-
  dados_infl_anual %>%
  ggplot() +
  geom_line(aes(x = data, y = meta, group = 1), colour="grey", linetype="dashed", linewidth=0.7) +
  geom_ribbon(aes(x = data, ymin = inf, ymax = sup, group = 1),
              colour = "grey", fill="grey", alpha=0.4) + 
  geom_line(aes(x = data, y = y7169, group = 1), colour="grey", linetype="solid", size=1.5) +
  geom_line(aes(x = data, y = y7786, group = 1), colour="#EB52ff", linetype="solid", size=1.5) +
  scale_x_discrete(breaks = seq(2007,2023,4)) +
  scale_y_continuous(limits = c(-3,22)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
  theme_bw() +
  labs(title = "9. Inflação de comunicação") +
  ylab("")+
  xlab("") +
  theme(plot.title = element_text(size = 12, hjust=0, face = "bold")); plot_y7786

plot_grupos_fig1 <- grid.arrange(plot_y7170, plot_y7445, plot_y7486, 
                                 plot_y7558, plot_y7625, plot_y7660, 
                                 plot_y7712, plot_y7766, plot_y7786,
                                 nrow = 3, ncol = 3)
ggsave("plot_grupos_fig1.png", plot_grupos_fig1, width=15, height=15)

# Matriz de correlação
{
  corr <- round(cor(dados_infl_anual[,-c(1,3,4)]),1)
  colnames(corr) <- c("Meta de inflação","Inflação agregada","Alimentação e bebidas",
                      "Habitação","Artigos de residência","Vestuário","Transportes",
                      "Saúde e cuidados pessoais","Despesas pessoais",
                      "Educação","Comunicação")
  rownames(corr) <- c("Meta de inflação","Inflação agregada","Alimentação e bebidas",
                      "Habitação","Artigos de residência","Vestuário","Transportes",
                      "Saúde e cuidados pessoais","Despesas pessoais",
                      "Educação","Comunicação")
}
{
  p.mat <- round(cor_pmat(dados_infl_anual[,-c(1,3,4)]),1)
  colnames(p.mat) <- c("Meta de inflação","Inflação agregada","Alimentação e bebidas",
                       "Habitação","Artigos de residência","Vestuário","Transportes",
                       "Saúde e cuidados pessoais","Despesas pessoais",
                       "Educação","Comunicação")
  rownames(p.mat) <- c("Meta de inflação","Inflação agregada","Alimentação e bebidas",
                       "Habitação","Artigos de residência","Vestuário","Transportes",
                       "Saúde e cuidados pessoais","Despesas pessoais",
                       "Educação","Comunicação")
}
corr1 <- ggcorrplot(corr,
           hc.order = F,
           type = "lower",
           colors = c("#45ff66", "transparent", "#EB52ff"),
           lab = TRUE,
           lab_size = 2,
           lab_col = "black",
           outline.color = "transparent",
           ggtheme = theme_bw,
           show.legend = TRUE,
           legend.title = "Correlação",
           tl.cex = 8,
           tl.col = "black",
           tl.srt = 30) +
  theme(legend.position="top",
        legend.title.position = "left",
        legend.title = element_text(hjust = 0.5, size = 9, face = 'bold')); corr1

corr2 <- ggcorrplot(corr,
           hc.order = F,
           type = "lower",
           colors = c("#45ff66", "transparent", "#EB52ff"),
           lab = TRUE,
           lab_size = 2,
           lab_col = "black",
           outline.color = "transparent",
           ggtheme = theme_bw,
           show.legend = TRUE,
           legend.title = "Correlação",
           p.mat = p.mat,
           insig = "blank",
           sig.level = 0.05,
           tl.cex = 8,
           tl.col = "black",
           tl.srt = 30) +
  theme(legend.position="top",
        legend.title.position = "left",
        legend.title = element_text(hjust = 0.5, size = 9, face = 'bold')); corr2

corr_plot <- grid.arrange(corr1, corr2,
                          nrow = 1, ncol = 2)

ggsave("plot_corr.png", corr_plot, width=10, height=10)

# IPCA Geral
grid.arrange(NULL, NULL, NULL,
             NULL, NULL, NULL,
             NULL, plot_7169, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_geral <- grid.arrange(plot_7169, NULL,
                                NULL, NULL,
                                nrow = 2, ncol = 2)
ggsave("plot_ipca_geral.png", plot_ipca_geral, width=10, height=10)

# 1. Alimentos e Bebidas
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7170, NULL,
             NULL, plot_7171, NULL,
             NULL, plot_7432, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_1 <- grid.arrange(plot_7171, plot_7432,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_1.png", plot_ipca_1, width=10, height=10)

# 2. Habitação
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7445, NULL,
             NULL, plot_7446, NULL,
             NULL, plot_7479, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_2 <- grid.arrange(plot_7446, plot_7479,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_2.png", plot_ipca_2, width=10, height=10)

# 3. Artigos de residência
grid.arrange(NULL, plot_7486, NULL,
             NULL, plot_7487, NULL,
             NULL, plot_7521, NULL,
             NULL, plot_7548, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_3 <- grid.arrange(plot_7487, plot_7521,
                            plot_7548, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_3.png", plot_ipca_3, width=10, height=10)

# 4. Vestuário
grid.arrange(NULL, plot_7558, NULL,
             NULL, plot_7559, NULL,
             NULL, plot_7604, NULL,
             NULL, plot_7615, NULL,
             NULL, plot_7620, NULL,
             nrow = 5, ncol = 3)

plot_ipca_4 <- grid.arrange(plot_7559, plot_7604,
                            plot_7615, plot_7620,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_4.png", plot_ipca_4, width=10, height=10)

# 5. Transportes
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7625, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_5 <- grid.arrange(plot_7625, NULL,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_5.png", plot_ipca_5, width=10, height=10)

# 6. Saúde e cuidados pessoais 
grid.arrange(NULL, plot_7660,NULL,
             NULL, plot_7661,NULL,
             NULL, plot_7683,NULL,
             NULL, plot_7697,NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_6 <- grid.arrange(plot_7661, plot_7683,
                            plot_7697, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_6.png", plot_ipca_6, width=10, height=10)

# 7. Despesas pessoais 
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7712,NULL,
             NULL, plot_7713, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_7 <- grid.arrange(plot_7713, plot_7730,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_7.png", plot_ipca_7, width=10, height=10)

# 8. Educação 
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7766, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_8 <- grid.arrange(plot_7766, NULL,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_8.png", plot_ipca_8, width=10, height=10)

# 9. Comunicação
grid.arrange(NULL, NULL, NULL,
             NULL, plot_7786, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             NULL, NULL, NULL,
             nrow = 5, ncol = 3)

plot_ipca_9 <- grid.arrange(plot_7786,NULL,
                            NULL, NULL,
                            nrow = 2, ncol = 2)
ggsave("plot_ipca_9.png", plot_ipca_9, width=10, height=10)

# 11. Gráfico por grupos
plot_ipca_grupos <- grid.arrange(plot_7170, plot_7445, plot_7486, 
                                 plot_7558, plot_7625, plot_7660, 
                                 plot_7712, plot_7766, plot_7786,
                                 nrow = 3, ncol = 3)
ggsave("plot_ipca_grupos.png", plot_ipca_grupos, width=15, height=15)

# 11. Gráfico consolidado
grid.arrange(plot_7169, plot_7170, plot_7445, plot_7486, plot_7558, plot_7625, plot_7660, plot_7712, plot_7766, plot_7786,
             NULL     , plot_7171, plot_7446, plot_7487, plot_7559, NULL     , plot_7661, plot_7713, NULL     , NULL     ,
             NULL     , plot_7432, plot_7479, plot_7521, plot_7604, NULL     , plot_7683, NULL     , NULL     , NULL     ,
             NULL     , NULL     , NULL     , plot_7548, plot_7615, NULL     , plot_7697, NULL     , NULL     , NULL     ,
             NULL     , NULL     , NULL     , NULL     , plot_7620, NULL     , NULL     , NULL     , NULL     , NULL     ,
             nrow = 5, ncol = 10)

# FIM --------------------------------------------------------------------------


library(tidyverse)
library(sidrar)

pof = get_sidra(6715, 
                period = "2018", 
                variable = 1204, 
                classific = c("all"), 
                category = list(c(103539,103540,103554,
                                  103561,103569, 103574, 103585, 103599, 103606, 111442, 8026), 
                                c(47558, 47559,47560,
                                  47561,47562,47563,
                                  47564))) %>%
  mutate(`Tipos de despesa` = gsub("[[:punct:]]|[[:digit:]]", "", `Tipos de despesa`)) %>%
  rename("classes" = `Classes de rendimento total e variação patrimonial mensal familiar`)




ggplot(subset(pof, pof$`Tipos de despesa` == "Alimentação"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com alimentação por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Habitação"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com habitação por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggplot(subset(pof, pof$`Tipos de despesa` == "Vestuário"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com vestuário por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggplot(subset(pof, pof$`Tipos de despesa` == "Transporte"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com transporte por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Higiene e cuidados pessoais"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com higiene e cuidados pessoais por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Assistência à saúde"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com assistência à saúde por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Educação"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com educação por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Serviços pessoais"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com serviços pessoais por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Eletrodomésticos"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com eletrodomésticos por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(subset(pof, pof$`Tipos de despesa` == "Mobiliários e artigos do lar"),
       mapping = aes(x = factor(classes,
                                levels=c("Até 1.908 Reais", 
                                         "Mais de 1.908 a 2.862 Reais", 
                                         "Mais de 2.862 a 5.724 Reais", 
                                         "Mais de 5.724 a 9.540 Reais", 
                                         "Mais de 9.540 a 14.310 Reais", 
                                         "Mais de 14.310 a 23.850 Reais",
                                         "Mais de 23.850 Reais")),
                     y = Valor,
                     fill = factor(classes,
                                   levels=c("Até 1.908 Reais", 
                                            "Mais de 1.908 a 2.862 Reais", 
                                            "Mais de 2.862 a 5.724 Reais", 
                                            "Mais de 5.724 a 9.540 Reais", 
                                            "Mais de 9.540 a 14.310 Reais", 
                                            "Mais de 14.310 a 23.850 Reais",
                                            "Mais de 23.850 Reais")))) + 
  geom_bar(stat='identity',
           position="dodge") +
  labs(fill="Faixa de renda") +
  theme_minimal() +
  scale_fill_brewer(palette = "RdBu") +
  ylab("Porcentagem da despesa total") +
  geom_text(aes(x = classes, label=Valor), vjust=-0.5, color="black", size=5) +
  ggtitle("Despesa com mobiliários e artigos do lar por faixa de renda") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




