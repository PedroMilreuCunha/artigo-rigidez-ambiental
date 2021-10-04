setwd("C:/Users/Win10/Desktop/Estudos/TCC/Novos dados/Intermediários")
wd <- getwd

library(readxl)
library(dplyr)
library(tidyr)
library(utils)
library(rio)
library(readstata13)
`%notin%` <- Negate(`%in%`)

#### Fluxos Bilaterais (NRB e RB) ####

fluxos_bilaterais <- read_excel("Exportação por país - por ano - por parceiro - por código.xlsx")


fluxos_bilaterais <-complete(fluxos_bilaterais,ReporterISO3,ProductCode,PartnerISO3,Year = 2010:2014)
fluxos_bilaterais <- subset(fluxos_bilaterais, ReporterISO3!= "ReporterISO3" & PartnerISO3 != "PartnerISO3" &
                             ProductCode != "ProductCode" )

fluxos_bilaterais$`TradeValue in 1000 USD`[is.na(fluxos_bilaterais$`TradeValue in 1000 USD`)] <- 0

fluxos_bilaterais <- fluxos_bilaterais[!(is.na(fluxos_bilaterais$ProductCode)),]

fluxos_bilaterais$ReporterISO3 <- as.character(fluxos_bilaterais$ReporterISO3)
fluxos_bilaterais$PartnerISO3 <- as.character(fluxos_bilaterais$PartnerISO3)

fluxos_bilaterais <- subset(fluxos_bilaterais, PartnerISO3 != ReporterISO3)

nrb <- c("59","661","67","69")

##### Uso de energia ####

uso_de_energia <- read_excel("Energy Use.xls")
uso_de_energia <- subset(uso_de_energia, `Country Code` %in% unique(fluxos_bilaterais$ReporterISO3))

uso_de_energia <- na.omit(uso_de_energia)

#### Retirando os ausentes de Energy Use dos Fluxos Bilaterais

fluxos_bilaterais <- subset(fluxos_bilaterais, ReporterISO3 %in% unique(uso_de_energia$`Country Code`))

#### Testando se a retirada dos ausentes deu certo ####

#ausentes <- setdiff(unique(fluxos_bilaterais$ReporterISO3), unique(uso_de_energia$`Country Code`)) 

#### Exportando os dados ####

nrb <- subset(fluxos_bilaterais, ProductCode %in% nrb)
rb <- subset(fluxos_bilaterais, ProductCode %notin% nrb)

rb_aux <- tbl_df(rb)

colnames(rb_aux) <- c("ReporterISO3","ProductCode","PartnerISO3", "Year", "Value")

somas <- rb_aux %>% 
  group_by(ReporterISO3, PartnerISO3, Year) %>%
  summarize(soma_por_par_por_ano = sum(as.numeric(Value)))

write.csv(somas, "somasrb.csv")

#write.csv(nrb, "nrb.csv")
#write.csv(rb, "rb.csv")

#write.csv(unique(fluxos_bilaterais$ReporterISO3), "iso3.csv")
#write.csv2(uso_de_energia, "uso_de_energia.csv")

#### Fluxos bilaterais totais ####

fluxos_bilaterais_totais <- read_excel("Fluxos bilaterais totais.xlsx")



fluxos_bilaterais_totais <-complete(fluxos_bilaterais_totais,ori,des, ANO = 2010:2014)
fluxos_bilaterais_totais <- subset(fluxos_bilaterais_totais, des %in% unique(ori))
fluxos_bilaterais_totais <- subset(fluxos_bilaterais_totais, ori != des)

fluxos_bilaterais_totais$EXP[is.na(fluxos_bilaterais_totais$EXP)] <- 0

fluxos_bilaterais_totais <- fluxos_bilaterais_totais[!(is.na(fluxos_bilaterais_totais$ProductCode)),]

#write.csv(fluxos_bilaterais_totais, "fluxos_bilaterais_totais.csv")
