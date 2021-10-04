# Bibliotecas ----

library(dplyr)
library(readxl)
library(ggplot2)

# Configurando o tema dos gráficos ----

theme_new <- function(base_size = 8) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      plot.title = element_text(size = rel(3), face = "bold", margin = margin(5,1,5,1), hjust = 0, family = "mono"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = rel(2.5), face = "bold", , family = "mono"),
      axis.text = element_text(size = rel(2.25), , family = "mono"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      legend.title = element_text(size = rel(2.5), face = "bold", family = "mono"),
      legend.text = element_text(size = rel(2.25), family = "mono"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = rel(1.5), face = "bold", color = "black", margin = margin(5,1,5,1)),
      strip.placement = "outside"
    )
}
theme_set(theme_new())

# Leitura e tratamento dos dados ----

painel_total <- read_excel("Dados finais/Painel Total.xlsx")

dados_score <- data.frame("País" = unique(painel_total$ori))
dados_score <- aggregate(x = painel_total$SCO_O, by = list(Pais = painel_total$ori, Ano = painel_total$ANO), FUN = mean)

dados_score_wide <- dados_score %>% arrange(Ano) %>% tidyr::pivot_wider(id_cols = Pais, names_from = Ano, values_from = x)

set.seed(210907)

# Criação do gráfico ----

gg1 <- ggplot(data = subset(dados_score, Pais %in% c(sample(dados_score$Pais, size = 5))), aes(x = Ano, y = x, color = Pais,
                                                                                               linetype = Pais)) +
       geom_line(size = 1.60) + 
       labs(y = "Score", color = "País", linetype = "País", title = "Evolução do score ambiental: 2010 - 2014",
            subtitle = "(5 países selecionados aleatoriamente)", caption = "Fonte: Elaboração própria.")
