
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

dados <- read.csv("Dados finais/globalizacao.csv")
colnames(dados) <- c("Ano", "Comércio mundial (% do PIB)", "Série")

series <- unique(dados$Série)
labels <- character(156)
labels[7] <- series[1]
labels[8] <- series[2]
labels[50] <- series[3]
labels[142] <- series[4]

dados$labels <- labels

g <- ggplot(data = dados, aes(x = Ano, y = `Comércio mundial (% do PIB)`, colour = Série, label = labels)) + 
     geom_line(show.legend = FALSE, size = 0.80) + 
     geom_point(aes(colour = Série), show.legend = FALSE, size = 0.55) + 
     geom_label_repel(family = "Times New Roman", nudge_x = -25, nudge_y = 4, na.rm = TRUE, show.legend = FALSE,
                      max.overlaps = 105, fontface = "bold") +
     scale_color_brewer(palette = "Dark2") +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           axis.line.x = element_line(size = 0.25, colour = "black"),
           axis.line.y = element_line(size = 0.25, colour = "black"),
           axis.text = element_text(family = "Times New Roman", size = 10, face = "bold"),
           panel.background = element_rect(fill = "white"))
g
