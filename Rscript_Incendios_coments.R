#Em primeiro lugar devemos definir o nosso working directory, 
#local no nosso computador onde estão guardados todos os ficheiros que utilizaremos para trabalhar neste script.

setwd("C:/Users/ebvillax/Desktop/Media/R")

#Antes de importar o dataset, vamos instalar já todos os packages que serão necessarios no decorrer deste projeto.

install.packages("data.table")

library(data.table)

install.packages("ggplot2")
library(ggplot2)

#De seguida, vamos importar o dataset sobre o tema escolhido para a analise: incendios em Portugal Continental em 2015.
#Neste caso, utilizamos o comando read.csv pois o ficheiro onde está inserido o dataset tem o formato CSV.
#O comando load só poderá ser utilizados com ficheiros com formato RData.

> medica <- read.csv("Lista Incendios_2015_33var.csv")

#Podemos transformar diretamente o nosso dataset em data.table para começar a trabalhar.

> media <- data.table(medica)

> names(medica)
 [1] "Ano"                                 "Codigo.SGIF"                        
 [3] "Codigo_ANPC"                         "Tipo"                               
 [5] "Distrito"                            "Concelho"                           
 [7] "Freguesia"                           "Local"                              
 [9] "INE"                                 "x"                                  
[11] "y"                                   "lat"                                
[13] "lon"                                 "DataAlerta"                         
[15] "DataExtincao"                        "Data1Intervencao"                   
[17] "FonteAlerta"                         "NUT"                                
[19] "AA_Povoamento..ha."                  "AA_Mato..ha."                       
[21] "AA_Agricola..ha."                    "AA_EspacosFlorestais..pov.mato..ha."
[23] "AA_Total..pov.mato.agric...ha."      "Reacendimentos"                     
[25] "Queimada"                            "Falso.Alarme"                       
[27] "Fogacho"                             "Incendio"                           
[29] "Agricola"                            "Perimetro"                          
[31] "APS"                                 "Causa"                              
[33] "TipoCausa"                           "Região.PROF"                        
[35] "UGF.................."              

#Podemos agora explorar o dataset. Vamos começar por identificar a classe de cada variavel. Podemos obter através do comando "sapply".

> sapply(media, class)
                                Ano                         Codigo.SGIF 
                           "factor"                            "factor" 
                        Codigo_ANPC                                Tipo 
                          "numeric"                            "factor" 
                           Distrito                            Concelho 
                           "factor"                            "factor" 
                          Freguesia                               Local 
                           "factor"                            "factor" 
                                INE                                   x 
                          "integer"                           "integer" 
                                  y                          DataAlerta 
                          "integer"                            "factor" 
                       DataExtincao                    Data1Intervencao 
                           "factor"                            "factor" 
                        FonteAlerta                                 NUT 
                           "factor"                            "factor" 
                 AA_Povoamento..ha.                        AA_Mato..ha. 
                          "numeric"                           "numeric" 
                   AA_Agricola..ha. AA_EspacosFlorestais..pov.mato..ha. 
                          "numeric"                           "numeric" 
     AA_Total..pov.mato.agric...ha.                      Reacendimentos 
                          "numeric"                           "integer" 
                           Queimada                        Falso.Alarme 
                          "integer"                           "integer" 
                            Fogacho                            Incendio 
                          "integer"                           "integer" 
                           Agricola                           Perimetro 
                          "integer"                            "factor" 
                                APS                               Causa 
                           "factor"                            "factor" 
                          TipoCausa                         Região.PROF 
                           "factor"                            "factor" 
                                UGF 
                           "factor" 




#Visto que só temos informação sobre incendios em 2015, para obter o numero total de incendios, basta correr o comando nrow no nosso dataset.

> nrow(medica)
[1] 27471

#Explorando agora a distribuição dos incendios por região e por causa, podemos construir os seguintes quadros.

> media[ , list(n_fogos=.N), by = TipoCausa]
       TipoCausa n_fogos
1:    Negligente    5955
2:  Desconhecida    5428
3: Reacendimento    1536
4:          NULL    7004
5:   Intencional    3085
6:                    17
7:       Natural     150

#Olhando para os incendios que têm causa definida, a causa mais vezes identificada foi a negligencia. Para 7004 incendios, não há informação sobre a causa.


> media[ , list(n_fogos=.N), by = Distrito]
            Distrito n_fogos
 1: Viana do Castelo    1483
 2:            Porto    4444
 3:            Braga    2231
 4:            Viseu    1528
 5:        Vila Real    1480
 6:          Coimbra     749
 7:         Santarém    1327
 8:         Bragança     733
 9:           Lisboa    2225
10:       Portalegre     363
11:             Beja     438
12:            Évora     306
13:             Faro     584
14:           Guarda     809
15:           Aveiro    1764
16:           Leiria     904
17:          Setúbal    1170
18:   Castelo Branco     544
19: Viana Do Castelo      76
20:                       17

#Os 3 distritos com mais incendios em 2015 foram Porto, Lisboa e Braga.


#De seguida, interessa fazer uma distribuição temporal dos incendios. Aqui, poderemos logo ter uma ideia da fiabilidade dos dados recolhidos: 
# tratando-se dum evento bastante sazonal e potenciado por temperaturas altas e periodos de pouca precipitação, os meses com maior incidencia
#  deveriam ser os meses de verão. 
# A variavel que nos permitirá fazer esta primeira distribuição é a "DataAlerta". Para isso, devemos primeiro ver em que formato
#  está esta variável para avaliar se este ultimo permite uma analise temporal.

> head(media$DataAlerta)
[1] 2015-03-24 00:00:00.000 2015-03-24 00:00:00.000 2015-03-24 00:00:00.000 2015-03-24 00:00:00.000
[5] 2015-03-24 00:00:00.000 2015-03-24 00:00:00.000
347 Levels:  2015-01-01 00:00:00.000 2015-01-02 00:00:00.000 2015-01-03 00:00:00.000 ... 2015-12-30 00:00:00.000

# Constatamos que o formato da data não é muito usual e impossibilita uma analise temporal. Relembramos que as data em R são por default em AAAA-MM-DD.
# Vamos então prosseguir com a criação duma variável com este formato através da variavel inicial.

> media[, Data_Inicio:=as.Date(substr(DataAlerta, 1 , 10))]

#Selecionámos os 10 primeiros caracteres da variável "DataAlerta" e demos o formato de data à nova variavel, chamada "Data_Inicio".


# Através deste grafico, construido com ajuda do package ggplot2, temos a nossa tese empirica confirmada dado que os meses
# com mais incendios são Julho e Agosto, os 2 meses mais quentes do ano estatitiscamente.

> qplot ( data = media , x = months(Data_Inicio), geom = "bar") + xlim("janeiro", "fevereiro", "março", "abril", "maio", "junho", "julho", "agosto", "setembro", "outubro", "novembro", "dezembro", NA).

#Com um grafico de identica construção, obtemos a distribuição dos incnedios por dia da semana. 
# Reparamos que há uma propensão maior para haver incendios os dias de fins-de-semana do que nos dias de semana.

> qplot ( data = media , x = weekdays(Data_Inicio), geom = "bar") + xlim("segunda-feira",
 "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado", "domingo", NA)


#Probabilidade de um incendio ocorrer por dia da semana.


> media[ , list(Probabilidade_de_incendio=.N/nrow(media)*100), by = weekdays(Data_Inicio)]
        weekdays Probabilidade_de_incendio
1:   terça-feira               13.44120820
2:  quarta-feira               12.45307443
3:  quinta-feira               13.52750809
4:   sexta-feira               14.45091694
5:        sábado               16.15965480
6: segunda-feira               13.96763754
7:       domingo               15.92664509
8:          <NA>                0.07335491


#Area ardida
#Iremos agora interessar-nos pela area ardida em hectares. 
#Temos varios indicadores, mas vamos-nos focar apenas na area ardida total de cada incendio.
#Devemos em primeiro lugar explorar a variavel em questão para ver se está no formato correto
#e se tem "missing data".

> summary(media$AA_Total..pov.mato.agric...ha.)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
   0.000    0.005    0.040    2.949    0.340 4673.000       17 

#Reparamos que há 17 linhas onde há NAs. 
#Vamos então criar uma variavel nova que transforma os NAs em zeros.

media[, AreaArdidaTotal:=ifelse(is.na(AA_Total..pov.mato.agric...ha.), 0, paste(AA_Total..pov.mato.agric...ha.))]

#Agora temos de ver se é preciso fazer alguma conversão no formato da variavel.

> class("AreaArdidaTotal")
[1] "character"

#Estando no formato character, vamos ter de transformar em numeric.
#Para isso vamos criar uma variavel numerica.

> media[, AreaArdidaTotalNA:=as.numeric(AreaArdidaTotal)]

#Podemos agora computar alguns dados sobre a area total ardida.

> media[, sum(AreaArdidaTotalNA)]
[1] 68300.9

Segundo o dataset em analise, arderam mais de 68 mil hectares em Portugal em 2015.


> mean(media[, AreaArdidaTotalNA])
[1] 2.94718

Cada incendio teve em media uma area ardida equivalente a 3 campos de futebol.


#Computando a area total ardida por dia da semana em que o incendio se inicia,
#constatamos que os incendios que se iniciaram ao sabado foram os que 
#consumiram mais terreno, em termos absolutos.
 
> media[, sum(AreaArdidaTotalNA), by = weekdays(Data_Inicio)]
        weekdays        V1
1:   terça-feira  7726.364
2:  quarta-feira  3182.056
3:  quinta-feira  9768.966
4:   sexta-feira  7317.125
5:        sábado 15635.617
6: segunda-feira 13745.389
7:       domingo 10925.380
8:          <NA>     0.000


#Em relação a area media ardida por incendio,
#os incendios mais devastadores foram os que deflagraram à segunda. 


> media[, mean(AreaArdidaTotalNA), by = weekdays(Data_Inicio)]
        weekdays       V1
1:   terça-feira 2.480374
2:  quarta-feira 1.102583
3:  quinta-feira 3.116097
4:   sexta-feira 2.184868
5:        sábado 4.175065
6: segunda-feira 4.246336
7:       domingo 2.960006
8:          <NA> 0.000000


#Criação de variavel que devolve o dia da semana do inicio do incendio em fator.

media[, DiaSemana:=ifelse(weekdays(Data_Inicio) == "segunda-feira"
, 1 , ifelse(weekdays(Data_Inicio) == "terça-feira", 2 
, ifelse(weekdays(Data_Inicio) == "quarta-feira", 3
, ifelse(weekdays(Data_Inicio) == "quinta-feira", 4
, ifelse(weekdays(Data_Inicio) == "sexta-feira", 5
, ifelse(weekdays(Data_Inicio) == "sábado", 6, 7))))))]


> lm.AA <- lm(AreaArdidaTotalNA ~ DiaSemana, data = media)
> summary(lm.AA)

Call:
lm(formula = AreaArdidaTotalNA ~ DiaSemana, data = media)

Residuals:
   Min     1Q Median     3Q    Max 
  -4.4   -3.4   -2.5   -1.7 4668.6 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.9317     0.7532   1.237   0.2161   
DiaSemana     0.4918     0.1651   2.978   0.0029 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 50.09 on 23156 degrees of freedom
  (17 observations deleted due to missingness)
Multiple R-squared:  0.0003829, Adjusted R-squared:  0.0003398 
F-statistic: 8.871 on 1 and 23156 DF,  p-value: 0.00290