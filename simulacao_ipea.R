library(magrittr)
library(dplyr)
library(ggplot2)
library(multipanelfigure)

dados <- read.csv2("C:\\Users\\Documentos\\Documents\\Estudos\\simulacao_ipea.csv")

dados[dados == ""] <- NA; dados[dados == " "] <- NA
dados$remuneracao %<>% as.numeric


#4. Qual é a quantidade de trabalhadores do sexo masculino com idade igual ou superior a 50 anos, registrados no estado da Bahia, no ano de 2019? *
dados %>% subset(sexo == "M" & idade >= 50 & uf == "BA" & ano == "2019") %>% dim

#R: 3184


#5. Qual ano e estabelecimento apresenta a maior quantidade de registros do sexo feminino? *
aux <- data.frame(table(paste0(dados$ano, dados$orgao)[dados$sexo == "F"]))
aux[aux$Freq == max(aux$Freq), ]

#R: ANA 2017 - SBB 2017


#6. Sabendo-se que o nível de escolaridade de um trabalhador vai de 1 até 5, qual é o percentual de trabalhadores, registrados no estabelecimento Ipea, com escolaridade nível 4 no ano de 2015? *
subset(dados, orgao == "Ipea" & ano == 2015)["escolaridade"] %>% table(useNA = "always") %>% prop.table %>% multiply_by(100) %>% round(1)

#R: 40.1%
#Obs: Fiquei um pouco confuso sobre o escopo do filtro desejado, então adotei o filtro completo (além de usar os missings).


#7. Ao analisar os números de mulheres registradas no estabelecimento UFSJ, apenas para o ano de 2010, qual raça apresenta a menor quantidade de registros? Obs: desconsiderar registros sem a raça declarada. *
subset(dados, orgao == "UFSJ" & ano == 2010)["raca"] %>% table

#R: Amarelo = 80


#8. Carregar um gráfico, preferencialmente nos formatos HTML, PNG, JPEG, e PDF, contendo uma série temporal de 1985 até 2019, que trate sobre a diferença de sexo e raça. Você pode explorar as variáveis que achar mais relevantes.
dados <- dados[sample(nrow(dados), 5000), ] #Como a remuneração foi gerada utilizando uma mesma distribuição para todas as classes, os dados se mostraram extremamente homogênios, então optei por utilizar uma amostra para poder responder à questão 9.

#G1: Remuneração x Ano/Sexo
aux <- aggregate(list(remuneracao = dados$remuneracao), by = list(ano = dados$ano, sexo = dados$sexo), mean, na.rm = T)
G1 <- aux %>% 
  ggplot(aes(ano, remuneracao, color = sexo)) +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = sexo),
    data = aux %>% filter(ano == max(aux$ano)),
    color = "black",
    hjust = 0,
    size = 3,
    nudge_x = 0.5
  ) +
  guides(color = "none") +
  coord_cartesian(xlim = c(1985, 2022)) + #, ylim = c(5000, 6000)
  labs(title = "Remuneração x Ano / Gênero", x = "Ano", y = "Remuneração") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 10))


#G2: Remuneração x FaixaAno/Raça
aux <- dados[c("ano", "raca", "remuneracao")]
aux <- aggregate(list(remuneracao = dados$remuneracao), by = list(
  ano = cut(dados$ano, breaks = seq(1990, 2020, 5), include.lowest = TRUE, labels = paste0(substr(seq(1990, 2015, 5), 3, 4), "-", substr(seq(1994, 2019, 5), 3, 4))),
  raca = dados$raca), mean, na.rm = T)
G2 <- aux %>% 
  ggplot(aes(as.numeric(ano), remuneracao, color = raca)) +
  geom_line() +
  geom_point() +
  coord_cartesian() + #ylim = c(4800, 6000)
  scale_x_discrete(limits = levels(aux$ano)) +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), text = element_text(size = 10),
        legend.text = element_text(size = 7), legend.key.size = unit(0.5, 'cm')) +
  labs(title = "Remuneração x Intervalo Anual / Raça", x = "Intervalo Anual", y = "Remuneração")


#G3: Remuneração x Escolaridade/Sexo | 2019
aux <- subset(dados, ano == 2019)
aux <- aggregate(list(remuneracao = aux$remuneracao), by = list(escolaridade = aux$escolaridade, sexo = aux$sexo), mean, na.rm = T)
G3 <- aux %>% 
  ggplot(aes(escolaridade, remuneracao, color = sexo)) +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = sexo),
    data = aux %>% filter(escolaridade == 5),
    color = "black",
    hjust = 0,
    size = 3,
    nudge_x = 0.5
  ) +
  guides(color = "none") +
  coord_cartesian() + #ylim = c(4900, 6300)
  labs(title = "Remuneração x Escolaridade / Gênero\n Ano 2019", x = "Escolaridade", y = "Remuneração") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 10))


#G4: Remuneração x Raça | 2019
aux <- aggregate(list(remuneracao = dados$remuneracao), by = list(ano = dados$ano, raca = dados$raca), mean, na.rm = T)
aux %<>% merge(aggregate(list(remuneracaoTotal = aux$remuneracao), by = list(ano = aux$ano), sum), all.x = T, all.y = F)
aux$G4prop <- 100 * aux$remuneracao / aux$remuneracaoTotal
G4 <- ggplot(aux, aes(ano, G4prop, fill = raca)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribuição da Remuneração x Ano / Raça", x = "Ano", y = "Remuneração") +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(hjust = 0.5), text = element_text(size = 10),
        legend.text = element_text(size = 7), legend.key.size = unit(0.5, 'cm'))


plot <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none", row_spacing = 5, column_spacing = 5,
                           width = 250, height = 250) %>% 
  fill_panel(G1, column = 1, row = 1) %>%
  fill_panel(G2, column = 2, row = 1) %>% 
  fill_panel(G3, column = 1, row = 2) %>%
  fill_panel(G4, column = 2, row = 2) %>% print





######### LIXO #############

# > head(dados)
# ano  orgao idade sexo uf escolaridade     raca remuneracao
# 1993    CNC    79      TO           NA Indigena     6799.73
# 2007   Uesb    33    F MS           NA   Branco     9843.94
# 2011   Ufma    60    M MG            5    Preto     7416.29
# 2000   Ifad    34    M AC            3    Pardo     9416.94
# 2013 Uniube    67    F RN            5 Indigena     2769.03
# 2019   Uneb    49    M PB            3    Pardo     6157.14
# 
# 
# > str(dados)
# 'data.frame':	10000000 obs. of  8 variables:
# $ ano         : int  1993 2007 2011 2000 2013 2019 2003 1990 1995 2018 ...
# $ orgao       : chr  "CNC" "Uesb" "Ufma" "Ifad" ...
# $ idade       : int  79 33 60 34 67 49 80 45 58 77 ...
# $ sexo        : chr  "" "F" "M" "M" ...
# $ uf          : chr  "TO" "MS" "MG" "AC" ...
# $ escolaridade: int  NA NA 5 3 5 3 2 1 4 2 ...
# $ raca        : chr  "Indigena" "Branco" "Preto" "Pardo" ...
# $ remuneracao : chr  "6799.73" "9843.94" "7416.29" "9416.94" ...
# 
# 
# > sapply(dados, FUN = function(x) c(sum(is.na(x)), sum(x %in% c("", " "))))
# ano orgao idade    sexo uf escolaridade   raca remuneracao
#  0     0     0       0  0       666254      0           0
#  0     0     0 1665801  0            0 834387           0
# 
# 
# > sapply(dados[-which(names(dados) %in% c("orgao", "remuneracao"))], FUN = function(x) unique(x))
# $ano
# [1] 1993 2007 2011 2000 2013 2019 2003 1990 1995 2018 2002 2014 1991 2009 1996 2005 2010 1994 2008 2006 1992 2012 2001 2015 1998 2016 2004 1997
# [29] 2017 1999
#  
# $idade
# [1] 79 33 60 34 67 49 80 45 58 77 75 25 53 31 50 40 84 26 42 37 23 74 30 64 38 32 21 68 57 46 63 36 47 41 54 81 59 55 66 35 48 19 52 27 65 51 62
# [48] 71 61 44 39 24 29 43 18 28 82 22 76 72 56 73 70 69 20 78 83
#  
# $sexo
# [1] ""  "F" "M"
#  
# $uf
# [1] "TO" "MS" "MG" "AC" "RN" "PB" "PA" "DF" "AM" "BA" "GO" "PI" "RO" "CE" "SE" "RJ" "MA" "PE" "RR" "ES" "MT" "RS" "SC" "PR" "AL" "SP" "AP"    
#  
# $escolaridade
# [1] NA  5  3  2  1  4
#  
# $raca
# [1] "Indigena" "Branco"   "Preto"    "Pardo"    "Amarelo"  ""        
#  
#  
# > summary(dados)
#     ano          orgao               idade           sexo                uf             escolaridade        raca           remuneracao       
# Min.   :1990   Length:10000000    Min.   :18.00   Length:10000000    Length:10000000    Min.   :1.0      Length:10000000    Length:10000000   
# 1st Qu.:1999   Class :character   1st Qu.:33.00   Class :character   Class :character   1st Qu.:3.0      Class :character   Class :character  
# Median :2006   Mode  :character   Median :45.00   Mode  :character   Mode  :character   Median :4.0      Mode  :character   Mode  :character  
# Mean   :2006                      Mean   :46.54                                         Mean   :3.3                                           
# 3rd Qu.:2013                      3rd Qu.:60.00                                         3rd Qu.:4.0                                           
# Max.   :2019                      Max.   :84.00                                         Max.   :5.0                                           
# NA's   :666254                                        
# 
# 
# > sapply(dados, FUN = function(x) summary(nchar(x)))
# $ano
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       4       4       4       4       4       4 
# 
# $orgao
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2.000   4.000   4.000   4.769   6.000  11.000 
# 
# $idade
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       2       2       2       2       2       2 
# 
# $sexo
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0000  1.0000  1.0000  0.8334  1.0000  1.0000 
# 
# $uf
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       2       2       2       2       2       2 
# 
# $escolaridade
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1       1       1       1       1       1  666254 
# 
# $raca
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   5.000   5.000   5.332   6.000   8.000 
# 
# $remuneracao
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.000   7.000   7.000   6.883   7.000   8.000 


 
 
 