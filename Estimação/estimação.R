## -- Pedro Milreu Cunha -- ##
## -- Mestrando em Economia Aplicada pelo PPGE/UFPB -- ##

#### BIBLIOTECAS ####

library(readxl)
library(stargazer)
library(gravity)
library(dplyr)

#### COMANDO Negate() ####

`%notin%` <- Negate(`%in%`)

#### Lendo os dados sobre PIB e agrupando os países por renda ####

gdp_ordem <- read_excel("Dados iniciais/gdp.xls", sheet = 1)
gdp_serie <- read_excel("Dados iniciais/gdp.xls", sheet = 2)
gdp_serie$Ano <- as.numeric(gdp_serie$Ano)

ricos <- gdp_ordem$`50% Mais Ricos`
pobres <- gdp_ordem$`50% Mais Pobres`

#### Lendo os dados sobre NRB ####

painel_nrb <- read_excel("Dados finais/Painel NRB.xlsx", sheet = 1)
painel_nrb <- left_join(painel_nrb, gdp_serie, by = c("ori" = "cod", "ANO" = "Ano"))
colnames(painel_nrb)[11] <- "gdp_ori"
painel_nrb <- left_join(painel_nrb, gdp_serie, by = c("des" = "cod", "ANO" = "Ano"))
colnames(painel_nrb)[12] <- "gdp_des"

painel_nrb$log_gdp_ori <- log(painel_nrb$gdp_ori)
painel_nrb$log_gdp_des <- log(painel_nrb$gdp_des)
painel_nrb$ANO <- as.factor(painel_nrb$ANO)
painel_nrb$dist_log <- log(painel_nrb$distwces)

#### ESTIMAÇÃO NRB ####

  #### 1) Agregada ####

ppml_nrb <- ppml(dependent_variable = "EXP",
                 distance = "distwces",
                 additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                           "log_gdp_ori", "log_gdp_des", "des"),
                 robust = TRUE, method = "white1",
                 data = painel_nrb)

sum_ppml_nrb <- summary(ppml_nrb)[["coefficients"]]
sum_ppml_nrb <- as.data.frame(sum_ppml_nrb)[1:8,]
sum_ppml_nrb$`Pr(>|t|)` <- c("2.38e-16***", rep("<2e-16***", 2), "0.001667***",
                             "<2e-16***", "0.033856**", rep("<2e-16***",2))
rownames(sum_ppml_nrb) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                     "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_nrb

#### 2) NRB 50% mais ricos ####

painel_nrb_ricos <- subset(painel_nrb, painel_nrb$ori %in% ricos & painel_nrb$des %in% ricos)

  #### 2.1) Estimação NRB 50% mais ricos ####

ppml_nrb_ricos <- ppml(dependent_variable = "EXP",
                 distance = "distwces",
                 additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                           "log_gdp_ori", "log_gdp_des", "des"),
                 robust = TRUE, method = "white1",
                 data = painel_nrb_ricos)

sum_ppml_nrb_ricos <- summary(ppml_nrb_ricos)[["coefficients"]]
sum_ppml_nrb_ricos <- as.data.frame(sum_ppml_nrb_ricos)[1:8,]
sum_ppml_nrb_ricos$`Pr(>|t|)` <- c("2.08e-05***", rep("<2e-16***", 2), "0.006585***", "<2e-16***",
                                   "0,174226", "<2e-16***", "9.51e-06***")
rownames(sum_ppml_nrb_ricos) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                            "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_nrb_ricos

#### 3) NRB 50% mais pobres ####

painel_nrb_pobres <- subset(painel_nrb, painel_nrb$ori %in% pobres & painel_nrb$des %in% pobres)
  
#### 3.1) Estimação NRB 50% mais pobres ####

ppml_nrb_pobres <- ppml(dependent_variable = "EXP",
                       distance = "distwces",
                       additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                                 "log_gdp_ori", "log_gdp_des", "des"),
                       robust = TRUE, method = "white1",
                       data = painel_nrb_pobres)

sum_ppml_nrb_pobres <- summary(ppml_nrb_pobres)[["coefficients"]]
sum_ppml_nrb_pobres <- as.data.frame(sum_ppml_nrb_pobres)[1:8,]
sum_ppml_nrb_pobres$`Pr(>|t|)` <- c("0.428870", "<2e-16***", "7.39e-07***", "6.97e-08 ***",
                                    "8.49e-05***", "0.497682", "< 2e-16 ***", "0.499051")
rownames(sum_ppml_nrb_pobres) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                  "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_nrb_pobres  

#### Lendo os dados sobre RB ####

painel_rb <- read_excel("Dados finais/Painel RB.xlsx", sheet = 1)
painel_rb <- left_join(painel_rb, gdp_serie, by = c("ori" = "cod", "ANO" = "Ano"))
colnames(painel_rb)[11] <- "gdp_ori"
painel_rb <- left_join(painel_rb, gdp_serie, by = c("des" = "cod", "ANO" = "Ano"))
colnames(painel_rb)[12] <- "gdp_des"

painel_rb$log_gdp_ori <- log(painel_rb$gdp_ori)
painel_rb$log_gdp_des <- log(painel_rb$gdp_des)

#### ESTIMAÇÃO RB ####

  #### 1) Agregada ####

ppml_rb <- ppml(dependent_variable = "EXP",
                 distance = "distwces",
                 additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                           "log_gdp_ori", "log_gdp_des", "des"),
                 robust = TRUE, method = "white1",
                 data = painel_rb)

sum_ppml_rb <- summary(ppml_rb)[["coefficients"]]
sum_ppml_rb <- as.data.frame(sum_ppml_rb)[1:8,]
sum_ppml_rb$`Pr(>|t|)` <- c("0.013333**", "0.568623", "0.170030", "0.031281**", "0.100211",
                            "3.14e-08***", "0.726945", "0.321808")
rownames(sum_ppml_rb) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                            "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_rb

#### 2) RB 50% mais ricos ####

painel_rb_ricos <- subset(painel_rb, painel_rb$ori %in% ricos & painel_rb$des %in% ricos)

#### 2.1) Estimação RB 50% mais ricos ####

ppml_rb_ricos <- ppml(dependent_variable = "EXP",
                       distance = "distwces",
                       additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                                 "log_gdp_ori", "log_gdp_des", "des"),
                       robust = TRUE, method = "white1",
                       data = painel_rb_ricos)

sum_ppml_rb_ricos <- summary(ppml_rb_ricos)[["coefficients"]]
sum_ppml_rb_ricos <- as.data.frame(sum_ppml_rb_ricos)[1:8,]
sum_ppml_rb_ricos$`Pr(>|t|)` <- c("0.779994", "0.000473***", "0.651871", "0.005139***",
                                  "7.70e-05***", "3.15e-06***", "3.78e-05***", "0.071669*")
rownames(sum_ppml_rb_ricos) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                  "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_rb_ricos

#### 3) RB 50% mais pobres ####

painel_rb_pobres <- subset(painel_rb, painel_rb$ori %in% pobres & painel_rb$des %in% pobres)

#### 3.1) Estimação RB 50% mais pobres ####

ppml_rb_pobres <- ppml(dependent_variable = "EXP",
                        distance = "distwces",
                        additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                                  "log_gdp_ori", "log_gdp_des", "des"),
                        robust = TRUE, method = "white1",
                        data = painel_rb_pobres)

sum_ppml_rb_pobres <- summary(ppml_rb_pobres)[["coefficients"]]
sum_ppml_rb_pobres <- as.data.frame(sum_ppml_rb_pobres)[1:8,]
sum_ppml_rb_pobres$`Pr(>|t|)` <- c("1.49e-15***", "0.016260**", "0.785446", "3.55e-07***",
                                   "1.99e-11***", "0.002535**", "0.133180", "6.75e-10***")
rownames(sum_ppml_rb_pobres) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                   "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_rb_pobres  

#### Lendo os dados sobre Total ####

painel_total <- read_excel("Dados finais/Painel Total.xlsx")
painel_total <- left_join(painel_total, gdp_serie, by = c("ori" = "cod", "ANO" = "Ano"))
colnames(painel_total)[11] <- "gdp_ori"
painel_total <- left_join(painel_total, gdp_serie, by = c("des" = "cod", "ANO" = "Ano"))
colnames(painel_total)[12] <- "gdp_des"

painel_total$log_gdp_ori <- log(painel_total$gdp_ori)
painel_total$log_gdp_des <- log(painel_total$gdp_des)

#### ESTIMAÇÃO TOTAL ####

#### 1) Agregada ####

ppml_total <- ppml(dependent_variable = "EXP",
                distance = "distwces",
                additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                          "log_gdp_ori", "log_gdp_des", "des"),
                robust = TRUE, method = "white1",
                data = painel_total)

sum_ppml_total <- summary(ppml_total)[["coefficients"]]
sum_ppml_total <- as.data.frame(sum_ppml_total)[1:8,]
sum_ppml_total$`Pr(>|t|)` <- c(rep("<2e-16***", 3), "1.29e-15***", "<2e-16***", "0.001516**",
                               rep("<2e-16***", 2))
rownames(sum_ppml_total) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                           "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_total

#### 2) Total 50% mais ricos ####

painel_total_ricos <- subset(painel_total, painel_total$ori %in% ricos & painel_total$des %in% ricos)

#### 2.1) Estimação total 50% mais ricos ####

ppml_total_ricos <- ppml(dependent_variable = "EXP",
                      distance = "distwces",
                      additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                                "log_gdp_ori", "log_gdp_des", "des"),
                      robust = TRUE, method = "white1",
                      data = painel_total_ricos)

sum_ppml_total_ricos <- summary(ppml_total_ricos)[["coefficients"]]
sum_ppml_total_ricos <- as.data.frame(sum_ppml_total_ricos)[1:8,]
sum_ppml_total_ricos$`Pr(>|t|)` <- c("4.27e-11***", rep("<2e-16***", 2), "3.70e-06***",
                                     "<2e-16***", "0.044392**", "<2e-16***", "2.08e-07***")
rownames(sum_ppml_total_ricos) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                 "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_total_ricos

#### 3) total 50% mais pobres ####

painel_total_pobres <- subset(painel_total, painel_total$ori %in% pobres & painel_total$des %in% pobres)

#### 3.1) Estimação total 50% mais pobres ####

ppml_total_pobres <- ppml(dependent_variable = "EXP",
                       distance = "distwces",
                       additional_regressors = c("contig", "comlang_off", "SCO_O", "SCO_D",
                                                 "log_gdp_ori", "log_gdp_des", "des"),
                       robust = TRUE, method = "white1",
                       data = painel_total_pobres)

sum_ppml_total_pobres <- summary(ppml_total_pobres)[["coefficients"]]
sum_ppml_total_pobres <- as.data.frame(sum_ppml_total_pobres)[1:8,]
sum_ppml_total_pobres$`Pr(>|t|)` <- c("0.029695**", "<2e-16***", "0.031077**",
                                      "<2e-16***", "0.018699**", "0.433130",
                                      "<2e-16***", "0.091475*")
rownames(sum_ppml_total_pobres) <- c("Intercepto", "log(distância)", "contiguidade", "língua comum",
                                  "score origem", "score destino", "log(pib) origem", "log(pib) destino")
sum_ppml_total_pobres  

#### ESTATÍSTICAS DESCRITIVAS ####

dados_completos_agregados <- data.frame(`Fluxo total` = painel_total$EXP, `Fluxo NRB` = painel_nrb$EXP,
                                        `Fluxo RB` = painel_rb$EXP, `Fronteira` = painel_total$contig,
                                        `Língua comum` = painel_total$comlang_off,
                                        `log(Distância)` = log(painel_total$distwces),
                                        `Score exportação`= painel_total$SCO_O, 
                                        `Score importação`= painel_total$SCO_D,
                                        `log(PIB) ` = painel_total$log_gdp_ori)
colnames(dados_completos_agregados) <- c("Fluxo total", "Fluxo NRB", "Fluxo RB", "Fronteira", 
                                         "Língua comum", "log(Distância)", "Score exportação",
                                         "Score importação", "log(PIB)")
stargazer(dados_completos_agregados, type = "text")  

dados_completos_ricos <- data.frame(`Fluxo total` = painel_total_ricos$EXP, `Fluxo NRB` = painel_nrb_ricos$EXP,
                                    `Fluxo RB` = painel_rb_ricos$EXP, `Fronteira` = painel_total_ricos$contig,
                                    `Língua comum` = painel_total_ricos$comlang_off,
                                    `log(Distância)` = log(painel_total_ricos$distwces),
                                    `Score exportação`= painel_total_ricos$SCO_O, 
                                    `Score importação`= painel_total_ricos$SCO_D,
                                    `log(PIB)` = painel_total_ricos$log_gdp_ori)
colnames(dados_completos_ricos) <- c("Fluxo total", "Fluxo NRB", "Fluxo RB", "Fronteira", 
                                         "Língua comum", "log(Distância)", "Score exportação",
                                         "Score importação", "log(PIB)")
stargazer(dados_completos_ricos, type = "text")  

dados_completos_pobres <- data.frame(`Fluxo total` = painel_total_pobres$EXP, `Fluxo NRB` = painel_nrb_pobres$EXP,
                                    `Fluxo RB` = painel_rb_pobres$EXP, `Fronteira` = painel_total_pobres$contig,
                                    `Língua comum` = painel_total_pobres$comlang_off,
                                    `log(Distância)` = log(painel_total_pobres$distwces),
                                    `Score exportação`= painel_total_pobres$SCO_O, 
                                    `Score importação`= painel_total_pobres$SCO_D,
                                    `log(PIB)` = painel_total_pobres$log_gdp_ori)
colnames(dados_completos_pobres) <- c("Fluxo total", "Fluxo NRB", "Fluxo RB", "Fronteira", 
                                     "Língua comum", "log(Distância)", "Score exportação",
                                     "Score importação", "log(PIB)")
stargazer(dados_completos_pobres, type = "text")  

