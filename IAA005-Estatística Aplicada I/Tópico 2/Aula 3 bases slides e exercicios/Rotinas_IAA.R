
############################### AULA 1  ####################################

# Limpa a tela

CTRL + L


# Importar conjunto de dados

salarios <- read.csv("C:/iaa/estat_I/cps91.txt", sep="")

# Ver e alterar conjunto de Dados

View (salarios)
fix (salarios)


# Ver variáveis do conjunto de dados ativo

names (salarios)


# Salvar conjunto de dados no formato R

save ("salarios", file = "C:/iaa/estat_I/salarios.RData")


# Carregar conjunto de dados salvo no formato do R

load("C:/iaa/estat_I/salarios.RData")

# Construção de nova variável - multiplicação dos valores da 
#                               variável "age" por 1

salarios$age_new = with (salarios, age*10)


# Apagar variável

salarios <- within(salarios, {age_new<-NULL})


# Adicionar número das observações nos dados

salarios$ObsNumber <- 1:5634

# Distribuição de frequência


# install.packages("fdth")

library (fdth)

table <- fdt (salarios$husearns)

print (table)


# Histograma

# install.packages("RcmdrMisc")
library (RcmdrMisc)

X11(width = 10, height = 12)
with(salarios, Hist(age, scale="frequency", breaks="sturges", 
                    col="darkgray"))
?Hist

# Gráficos

# Boxplot

## Importar os Dados
ceo <- read.table("C:/iaa/estat_I/ceo.txt", header = TRUE, sep = "", 
                  na.strings = "NA", dec = ".", strip.white = TRUE)

save ("ceo", file = "C:/iaa/estat_I/ceo.RData")
load("C:/iaa/estat_I/ceo.RData")

View (ceo)

names (ceo)

# install.packages("car")

require (car)

X11(width = 10, height = 12)
Boxplot( ~ age, data=ceo, id=list(method="y"))

# Dispersão (scatter plot) X Y

X11(width = 10, height = 12)
attach(ceo)
plot(comten, age, main="Tempo de Empresa X Idade do Funcionário",
     xlab="Tempo de Empresa ", ylab="Idade ", pch=19)

# Gráfico de linhas

# Importanto arquivo de dados temporais

# install.packages("readxl")
# install.packages(plotly)
# install.packages(tidyverse)

library(readxl)
library(plotly)
library(tidyverse)

basepetr4 <- read_excel("C:/iaa/estat_I/basepetr4.xlsx")

ggplotly(
  basepetr4 %>%
    mutate(Data = as.Date(Data)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = Fechamento, color = "série")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)


# Distribuições Discretas de Probabilidade


# Distribuição Binomial

# Exemplo: x = 0; n = 10; p = 0.3; Resultado = 2.82%

local({
 .Table <- data.frame(Probability = dbinom(0:10, size=10, prob = 0.3))
 rownames(.Table) <- 0:10
 print (.Table)   
})


# Distribuição Poisson

# Exemplo: lambda = 5; x = 3; resultado 14.04%

local({
  .Table <- data.frame(Probability = dpois(0:14, lambda=5))
  rownames(.Table) <- 0:14
  print (.Table)   
})


# Distribuição Geométrica

# Número de fracassos até 1º sucesso

# Exemplo: Y=k=2; p = 0.4; Resultado = 14,40%

local({
  .Table <- data.frame(Probability = dgeom(0:14, prob=0.4))
  rownames(.Table) <- 0:14
  print (.Table)   
})

# Número de tentativas necessárias para se obter o 1º sucesso

# Exemplo: p = 0.10; Y = k = 34 ==> K-1 = 33 ; Resultado 0.3%

options(scipen = 999) # mostra formato numérico e não notação científica

local({
  .Table <- data.frame(Probability = dgeom(0:72, prob=0.1))
  rownames(.Table) <- 0:72
  print (.Table)   
})


# Distribuição Hipergeométrica

# Exemplo: número de sucessos na amostra = 2; 
#          número de sucessos na populaçãio = m = 3; 
#          número de fracassos na população = n = 3; 
#          tamanho da amostra = k = 4; 
#          Resultado = 60%

local({
  .Table <- data.frame(Probability = dhyper(1:3, m = 3, n = 3, k = 4))
  rownames(.Table) <- 1:3
  print (.Table)   
})


# Distribuição Binomial Negativa (inversa)

# Exemplo: x = 10; k=número de sucessos= size = 3; p = 0.02; 
#          Resposta = 0.025% 

local({
  .Table <- data.frame(Probability = dnbinom(6:20, size = 3, prob=0.02))
  rownames(.Table) <- 6:20
  print (.Table)   
})

# no resultado ler a observação "7", pois 10-3=7 , dado que a distribuição
# é negativa(inversa)


###############################  AULA 2 #####################################


######################### Distribuição Normal padronizada (Z)

# para 95.0% de confiança o valor de "Z" para lado da distribuição é 1.96 

#install.packages("RcmdrMisc")
#install.packages("tigerstats")
library(RcmdrMisc)
library(tigerstats)

# Como no R os valores da tabela Z são para uma cauda, para se obter o 
# nível de significância de 95% deve-se colocar na função o valor de
# 2,5% 

qnorm(0.025)

X11(width = 10, height = 12)
local({
  .x <- seq(-1.96, 1.96, length.out=1000)  
  plotDistr(.x, dnorm(.x, mean=0, sd=1), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("Normal Distribution (Z):  Mean=0, Standard deviation=1"))
})

X11(width = 10, height = 12)
pnormGC(c(-1.96,1.96),region="between",mean=0,
        sd=1,graph=TRUE)


############################ Distribuição t de Student

# para 95,0% de confiança e 120 graus de liberdade t= 1.98
# para a tabela t também deve-se colocar 2,5% porque os valores são para uma 
# cauda

qt(0.025, 120)

X11(width = 10, height = 12)
local({
  .x <- seq(-1.98, 1.98, length.out=1000)  
  plotDistr(.x, dt(.x, df=120), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("t Distribution: Degrees of freedom=120"))
})

X11(width = 10, height = 12)
ptGC(c(-1.98,1.98),region="between",df=120, graph = TRUE)

########################## Distribuição ChiSquare

# Graus de liberdade = 5 e 5.0% (1.145) na cauda inferior e 
# 95.0% (11.070) na cauda superior

qchisq(0.05,5)
qchisq(0.95,5)

X11(width = 10, height = 12)
local({
  .x <- seq(1.145, 11.070, length.out=1000)  
  plotDistr(.x, dchisq(.x, df=5), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("ChiSquared Distribution: Degrees of freedom=5"))
})

X11(width = 10, height = 12)
pchisqGC(c(11.070),region="below",df=5, graph = TRUE)


######################## Distribuição F de Fischer e Snedecor

# install.packages("graphics")
library(graphics)


# com 20 (numerador) e 19 graus de liberdade (denominador)

qf(0.95, 20, 19)

X11(width = 10, height = 12)
local({
  .x <- seq(0, 2.155, length.out=1000)  
  plotDistr(.x, df(.x, df1=20, df2=19), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("F Distribution: Numerator df=20, denominator df=19"))
})

# install.packages("sjPlot")
library(sjPlot)

dist_f(f = 0, deg.f1 = 20, deg.f2 = 19)

######################## Distribuição logística

qlogis(0.95)

X11(width = 10, height = 12)
local({
  .x <- seq(-2.95, 2.95, length.out=1000)  
  plotDistr(.x, plogis(.x, location=0, scale=1), cdf=TRUE, xlab="x", 
            ylab="Cumulative Probability", 
            main=paste("Logistic Distribution:  Location=0, Scale=1"))
})


########################### Estatísticas descritivas

load("C:/iaa/estat_I/salarios.RData")


#################### Média

mean(salarios$husearns)
mean(salarios$earns)

# A média do rendimento dos maridos na amostra é US$453.54
# A média do rendimento das esposas na amostra é US$232,833

453.5406/232.833

# Portanto, o rendimento médio dos maridos é quase o dobro do 
# rendimento médio das esposa

#################### Mediana

median (salarios$husearns)
median (salarios$earns)

# A mediana do rendimento dos maridos é de US$418,5
# A mediana do rendimento das esposas é de US$185,0 

418.5/185

# Em termos de mediana o rendimento dos maridos é mais que o
# dobro do rendimento das esposas

#################### Moda

# Para a idade dos maridos 
table(salarios$husage)
subset(table(salarios$husage), 
       table(salarios$husage) == max(table(salarios$husage)))

# Para a idade das esposas
table(salarios$age)
subset(table(salarios$age), 
       table(salarios$age) == max(table(salarios$age)))

# A moda da idade dos maridos é de 44 anos, com 201 pessoas 
# A moda da idade das esposa é de 37 anos, com 217 pessoas

#################### Variância

var(salarios$husearns)

var(salarios$earns)

# A variância do rendimento dos maridos é de 165638,1
# A variância do rendimento das esposas é de 69340,83

165639.1/69340.83

# Portanto a variância do rendimento do rendimento dos maridos 
# é mais que 2X a variância dos rendimentos das esposas

#################### Desvio Padrão

sd(salarios$husearns)
sd(salarios$earns)

# O desvio padrão do rendimento dos maridos é de US$406,98 
# O desvio padrão do rendimento das esposas é de US$263,32

406.9878/263.3265

# O desvio padrão dos rendimentos dos maridos é mais que 50%
# superior ao das esposas

#################### Coeficiente de Variação

meanM <- mean(salarios$husearns)
meanE <- mean(salarios$earns)
sdM <- sd(salarios$husearns)
sdE <- sd(salarios$earns)

cvM <- (sdM/meanM)*100
cvM

cvE <- (sdE/meanE)*100
cvE

# O coeficiente de variação do rendimento dos maridos é de 89,73%
# O coeficiente de variação do rendimento das esposas é de 113,09%

# Isso quer dizer que o rendimento dos maridos e esposas variam muito
# na amostra. Ainda, pode-se concluir que os rendimentos das esposas
# variam mais do que dos maridos.

# menor ou igual a 15% → baixa dispersão: dados homogêneos
# entre 15 e 30% → média dispersão
# maior que 30% → alta dispersão: dados heterogêneos


#################### Valores Minimo e Máximo

summary(salarios$husearns)
summary(salarios$earns)

# O rendimento máximo das esposas é maior do que dos maridos
# mas percebe-se que a mediana a média e o 3.Quartil é menor.

#################### Quantis

Q1 <- quantile(salarios$husearns, probs = 0.25)
Q1

# Q2 é igual a mediana
Q2 <- quantile(salarios$husearns, probs = 0.50)
Q2

Q3 <- quantile(salarios$husearns, probs = 0.75)
Q3


################## Distancia interquantílica (IQR)

husearns = salarios$husearns
earns = salarios$earns

IQR(husearns)
IQR(earns)

# A comparação entre as distâncias interquartílicas entre os rendimentos
# das esposas e maridos confirma que a variação entre os rendimentos das
# esposas é menor. Q3 - Q1

summary(salarios$husearns)
summary(salarios$earns)

################## Percentis

quantile(husearns, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 
quantile(earns, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 


################ Distribuição amostral da média (dam) - população infinita

#Exemplo do slide: Média = m = 150; Desvio padrão da amostra = s = 36; n = 36

m <- 150
s <- 36
n <- 36

dam <- s/sqrt(n)
dam

dam1 <- m + dam
dam2 <- m - dam

dam1
dam2

# Isso quer dizer que os valores vão variar entre 144 e 156 com uma
# média de 150

##### Exemplo para a variável "husearns" da base de dados salários

n <- nrow(salarios)

m <- mean(salarios$husearns)
m
s <- sd(salarios$husearns)

dam <- s/sqrt(n)
dam
dam1 <- m + dam
dam2 <- m - dam 

dam1
dam2

# Isso quer dizer que o rendimento dos maridos vão variar de US$448.11 a
# US$458.96 com uma média de US$453.54

# Para a variável "earns"

m <- mean(salarios$earns)
m
s <- sd(salarios$earns)

dam <- s/sqrt(n)
dam
dam1 <- m + dam
dam2 <- m - dam 

dam1
dam2

# Isso quer dizer que os rendimentos das esposas vão variar de US$229.32
# a US$236.34, com uma média de US$232.83

############## Distribuição amostral da média (damf) - população finita

# Exemplo: n = 16; N = 100; s = 57

n <- 16

N <- 100

s <- 57


damf <- (s/sqrt(n))*sqrt((N-n)/(N-1))
damf

# "Façamos de conta" que a amostra da base de dados "salarios" permite dizer 
# que a população é finita, com uma população de 100.000

# Para o rendimento das esposas (earns)
n <- nrow(salarios)

N <- 100000

s <- sd(salarios$earns)

m <- mean(salarios$earns)
m
damf <- (s/sqrt(n))*sqrt((N-n)/(N-1))
damf

dam1 <- m + damf
dam2 <- m - damf

dam1
dam2

mean(salarios$earns)

# Isso quer dizer que a média do rendimentos das esposas vai variar
# entre US$229.42 e US$236.24 com uma média de US$232.83

######################### Intervalo de confiança para a média (Z)
######################### Grandes amostras n>100

# install.packages("carData")
# install.packages("datasets")

library(carData)

library(datasets)

# install.packages("BSDA")

library(BSDA)

sd <- sd(salarios$husearns)

x <- salarios$husearns

options(scipen = 999)
z.test(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sd,
       sigma.y = NULL, conf.level = 0.95)

# Isso quer dizer que a média do rendimento dos maridos vai variar 
# entre US$442.91 e US$464.16, com uma média de US$453.54, com 95% de 
# confiança ou 5% de significância

# Estatística z = 83,646

# Confrontamos esse valor com o valor tabelado da estatística Z para 
# 95% de confiança ou 5% de significância (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qnorm(0.025)

# Como a estistica z calculada (83.646) é maior que a estatística tabelada
# (1.96) rejeitamos a hipótese (H0) de que o valor verdadeiro da média
# é estatisticamente igual a zero.Ressalta-se que se o valor da estatística z
# calculada fossa menor que -1.96 o resultado seria o mesmo ao interpretar 
# o teste. Mas, se a estatística t calculada se situasse entre -1.96 e 1.96
# (valor tabelado obtido pela função acima) aceitaríamos H0, portanto 
# poderíamos considerar que a média da variável seria estatisticamente igual
# a zero, com 95% de confiança ou 5% de significância.


####################### Intervalo de confiança para a média (t)
####################### Façamos de conta que a amostra é pequena n<30
# install.packages("stats")

library(stats)

x <- salarios$husearns

t.test(husearns, y = NULL,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, data=salarios)

# Isso quer dizer que a média das vendas vai variar entre US$442.91
# e US$464.17, com uma média de US$453.54, com 95% de confiança ou
# 5% de significância.

# Estatística t = 83.646, com 5633 graus de liberdade

# Confrontamos esse valor com o valor tabelado da estatística t para 
# 95% de confiança ou 5% de significância (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qt(0.025, df=5633)

# Como a estistica t calculada (83.646) é maior que a estatística tabelada
# (1.96) rejeitamos a hipótese (H0) de que o valor verdadeiro da média
# é estatisticamente igual a zero.Ressalta-se que se o valor da estatística t
# calculada fossa menor que -1.96 o resultado seria o mesmo ao interpretar 
# o teste. Mas, se a estatística t calculada se situasse entre -1.96 e 1.96
# (valor tabelado obtido pela função acima) aceitaríamos H0, portanto 
# poderíamos considerar que a média da variável seria estatisticamente igual
# a zero, com 95% de confiança ou 5% de significância. 

########## Intervalo de confiança para a variância pressupondo que a
########## "distribuição é normal"

var <- var(salarios$husearns)

n <- nrow(salarios)

# obtendo os valores de chi quadrado

chiinf <- qchisq(0.025, df=5633)
chisup <- qchisq(0.975, df=5633)

superior <- ((n-1)*var)/chiinf
inferior <- ((n-1)*var)/chisup

inferior
superior

# Isso quer dizer que a variância do rendimento dos maridos vai variar
# entre 159688.1 e 171930.7 com 95% de confiança ou 5% de significância

# Confrontamos esse valor com a variância da variável

var(salarios$husearns)

# Consideramos a estimativa do intervalo verdadeira pois o valor verdadeiro
# da variância 165639.1 se encontra no intervalo obtido (159688.1 e 171930.7)


#################### Teste para a diferença entre duas médias (z)
# Vamos usar os rendimentos dos maridos e esposas
# calculando os desvios padrão de x e y

sdx <- sd(salarios$husearns)

sdy <- sd(salarios$earns)

# Criando os objetos com as variáveis em análise

x <- salarios$husearns

y <- salarios$earns

z.test(x, y, alternative = "two.sided", mu = 0, sigma.x = sdx,
       sigma.y = sdy, conf.level = 0.95)


# Quer dizer que a diferença entre as médias estará entre US$ 208,04 e
# US$ 233,36, para as médias de US$453.54 e US$232.83 (dif. = 220.70),
# com 95% de confiança ou 5% de significância.

# Estatística z = 34,175

# Confrontamos esse valor com o valor tabelado da estatística z para 
# 95% de confiança ou 5% de significância (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qnorm(0.025)

# Como a estistica z calculada (34.175) é maior que a estatística tabelada
# (1.96) rejeitamos a hipótese (H0) de que as médias são iguais
# ou seja, de que a diferença verdadeira entre as médias é igual a zero.
# Ressalta-se que se o valor da estatística t calculada fossa menor que 
# -1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatística z calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela função acima) aceitaríamos H0, portanto poderíamos considerar
# que as médias seriam estatisticamente iguais, com 95% de confiança ou
# 5% de significância

################ Teste para a diferença entre duas médias (t)

# Façamos de conta que a amostra da base "salários" é pequena (<100)

x <- salarios$husearns

y <- salarios$earns

t.test(x, y,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

# Quer dizer que a diferença entre as médias estará entre US$ 208,04 e
# US$ 233,36, para as médias de US$453.54 e US$232.83 (dif. = 220.70),
# com 95% de confiança ou 5% de significância

# Estatistica t = 34,175, com 9646 graus de liberdade

# Confrontamos esse valor com o valor tabelado da estatística t para 
# 95% de confiança ou 5% de significância (usamos isso para obter o
# valor tabelado, conforme comando abaixo)

qt(0.025, df=9646)

# Como a estistica t calculada (34.175) é maior que a estatística tabelada
# (1.96) rejeitamos a hipótese (H0) de que as médias são iguais
# ou seja, de que a diferença verdadeira entre as médias é igual a zero.
# Ressalta-se que se o valor da estatística t calculada fosse menor que 
# -1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatística t calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela função acima) aceitaríamos H0, portanto poderíamos considerar
# que as médias seriam estatisticamente iguais, com 95% de confiança ou
# 5% de significância

################# Teste da diferença entre duas variâncias (F)


x <- salarios$husearns

y <- salarios$earns

var.test(x, y, alternative = "two.sided", conf.level = 0.95)

# A razão da diferença é de 2.388767 e a estatística F = 2.3888 com 5633 graus 
# de liberdade no numerador e denominador. Devemos confrontar esse valor da
# estatística F com o valor da tabela F, conforme função abaixo:

qf(0.95, 5633, 5633)

# como a estatística F (2.3888) é superior ao valor tabelado (1.04481) consideramos 
# que as variâncias não são estatísticamente iguais. Ressalta-se que se o valor
# calculado da estatística F fosse inferior a (1/1.04481 = 0.95711) o resultado 
# seria o mesmo. Mas se a estatística F calculada estivesse entre (0.95711 e 1.04481)
# poderíamos dizer que as variâncias seriam estatisticamente iguais com 95% de 
# confiança ou 5% de significância.


###### Teste de independencia/equivalência ou igualdade entre duas amostras (t)
# Pressuposto: as variâncias devem ser estatisticamente iguais e normalidade

# Vamos fazer de conta que as variâncias da amostra das duas variáveis são
# estatisticamente iguais, já sabemos que não são (teste acima), mas só para executar 
# a rotina/função e explicar como funciona

# install.packages("ggpubr")

library(ggpubr)

X <- salarios$husearns

y <- salarios$earns

t.test(x, y, alternative = "two.sided", var.equal = TRUE)

# A média de husearns é 454.5406 e de earns é 232.833, e para o teste o 
# resultado apresentou que a média conjunta deve ficar entre 204.0486 e
# 233.3667.

# Estatística t = 34.175, com 11266 graus de liberdade 

# Para fazer o teste devemos comparar o valor da estatistica t calculada 
# com a estatística t tabelada, obtida com a função abaixo:

qt(0.025, df=11266)

# Como a estistica t calculada (34.175) é maior que a estatística tabelada
# (1.96) rejeitamos a hipótese (H0) de que as amostras são similares
# ou seja, de que a diferença verdadeira entre elas é igual a zero.
# Ressalta-se que se o valor da estatística t calculada fosse menor que 
# -1.96 o resultado seria o mesmo ao interpretar o teste. Mas, se a
# estatística t calculada se situasse entre -1.96 e 1.96 (valor tabelado
# obtido pela função acima) aceitaríamos H0, portanto poderíamos considerar
# que as amostras seriam estatisticamente similares/dependentes, com 95% de
# confiança ou 5% de significância

########### Teste de Wilcoxon-Mann-Whitney para amostras independentes 

X <- salarios$husearns

y <- salarios$earns

options(scipen = 999)

wilcox.test(x, y, alternative = "two.sided") 

# Se no Resultado p-value <= 0.05; então as amostras são independentes
# como o resultado do p-value é menor que 0.05 as amostras são
# independentes (rejeitamos H0 de que as amostras são idênticas)

########### Teste de normalidade de Kolmogorov-Smirnov

# install.packages("RcmdrMisc")

library(RcmdrMisc)

normalityTest(~earns, test="lillie.test", data=salarios)

normalityTest(~husearns, test="lillie.test", data=salarios)

# Valor crítico de Kolmogorov-smirnov para nosso exemplo = 0.018133333
# Os valores calculados são D=0.18829 para "earns" e D=0.13256 para husearns
# 
# H0: A amostra provem de uma população normalmente distribuída
# Ha: A amostra não provem de uma população normalmente distribuída
# 
# como os valores "D" calculados são maiores que o valor crítico (tabelado) 
# estes se encontram na região de rejeição de H0, ou seja rejeita-se "normalidade"
# REGRA DE BOLSO: se P-value > 0.05, existe normalidade da amostra. 

###### Teste de normalidade de Shapiro-Wilk

shapiro.test(salarios$earns)

### Não "deu", amostra maior que 5000 observações

# Vamos usar a base de dados "ceo""

shapiro.test(ceo$sales)

# REGRA DE BOLSO: se P-value >= 0.05, existe normalidade da amostra.

# Como o p-value do teste aplicado é menor que 0.05 então a amostra não é
# considerada como de distribuição "normal"


######## Teste de normalidade de Anderson-Darling

library(nortest)

ad.test(salarios$earns)

# Como p-value é menor que 0.05 rejeita-se a hipótese de normalidade da 
# variável

######## Teste de normalidade de Cramer-von Mises

cvm.test(salarios$earns)

# p-value < 0.05 rejeita-se normalidade

################ Transformação Box-Cox para uma variável #####################

# install.packages('AID')
library(AID)
data(textile)
X11(width = 10, height = 12)
out <- boxcoxnc(textile[,1], method = "sw")
out$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Wilk test statistic
out$p.value # p.value of Shapiro-Wilk test for transformed data (p-value>0.05)
out$tf.data # transformed data set
confInt(out) # mean and confidence interval for back transformed data
X11(width = 10, height = 12)
out2 <- boxcoxnc(textile[,1], method = "sf")
out2$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Francia test stat.
out2$p.value # p.value of Shapiro-Francia test for transformed data
out2$tf.data
confInt(out2)


############### Transformação Box-Cox para modelos lineares ##################

library(AID)
trees=as.matrix(trees)
X11(width = 10, height = 12)
outlm <- boxcoxlm(x = trees[,1:2], y = trees[,3])
outlm$lambda.hat #Estimate of Box-Cox param. based on Shapiro-Wilk test stat.
outlm$p.value # p.value of Shapiro-Wilk test for transformed data (p-value>0.05)
outlm$tf.residuals # transformed data residuals
outlm$tf.data # transformed data for y variable



################################## AULA 3 #################################### 
#              REGRESSÃO POR MQO (MÍNIMOS QUADRADOS ORDINÁRIOS)              #
##############################################################################
# Carregando o arquivo de dados

load("C:/iaa/estat_I/wage.RData")
gc()
################### Estimando o modelo preliminar ############################

resultados <- lm(hrwage~husage+husearns+huseduc+husblck+hushisp+
                   hushrs+kidge6+age+black+educ+hispanic+union+
                   kidlt6+earns,data=wage)
summary (resultados)


################Testando a especificação do modelo

#install.packages("PanJen")

library("PanJen")

formBase<-formula(hrwage~husearns+huseduc+husblck+hushisp+
                    hushrs+kidge6+age+black+educ+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJenhusage<-fform(wage,"husage",formBase)
#base foi a forma vencedora, não incluir essa variável no modelo,
#mas vamos deixar e depois no procedimento stepwise poderemos 
#tirar caso seja necessário 

formBase<-formula(hrwage~husage+huseduc+husblck+hushisp+
                    hushrs+kidge6+age+black+educ+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJenhusearns<-fform(wage,"husearns",formBase)
#smoothing foi vencedora, então devemos incluir essa variável transformada
#no modelo
#Para usar a transformação smoothing devemos instalar e carregar os pacotes:
#install.packages("timetk")
#install.packages("hardhat")
#library(timetk)

formBase<-formula(hrwage~husage+husearns+husblck+hushisp+
                    hushrs+kidge6+age+black+educ+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJenhuseduc<-fform(wage,"huseduc",formBase)
#base foi a forma vencedora, não incluir essa variável no modelo,
#mas vamos deixar e depois no procedimento stepwise poderemos 
#tirar caso seja necessário

formBase<-formula(hrwage~husage+husearns+huseduc+husblck+hushisp+
                    kidge6+age+black+educ+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJenhushrs<-fform(wage,"hushrs",formBase)
#base foi a forma vencedora, não incluir essa variável no modelo,
#mas vamos deixar e depois no procedimento stepwise poderemos 
#tirar caso seja necessário

formBase<-formula(hrwage~husage+husearns+huseduc+husblck+hushisp+
                    hushrs+kidge6+black+educ+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJenage<-fform(wage,"age",formBase)
#base foi a forma vencedora, não incluir essa variável no modelo,
#mas vamos deixar e depois no procedimento stepwise poderemos 
#tirar caso seja necessário

formBase<-formula(hrwage~husage+husearns+huseduc+husblck+hushisp+
                    hushrs+kidge6+age+black+hispanic+union+
                    kidlt6+earns)
summary(lm(formBase, data=wage))

PanJeneduc<-fform(wage,"educ",formBase)
#smoothing foi vencedora, então devemos incluir essa variável transformada
#no modelo

formBase<-formula(hrwage~husage+husearns+huseduc+husblck+hushisp+
                    hushrs+kidge6+age+black+educ+hispanic+union+
                    kidlt6)
summary(lm(formBase, data=wage))

PanJenearns<-fform(wage,"earns",formBase)
#smoothing foi vencedora, então devemos incluir essa variável transformada
#no modelo

###Para transformar por smoothing:

wage$husearnss <- smooth_vec(wage$husearns)
wage$educs <- smooth_vec(wage$educ)
wage$earnss <- smooth_vec(wage$earns)

save(wage, file="wage.RData" )

####################### Estimando um modelo preliminar
#Não vamos usar as variáveis transformadas por enquanto, só no final se 
# os resultados não forem bons

resultados <- lm(hrwage~husage+husearns+huseduc+husblck+hushisp+
                   hushrs+kidge6+age+black+educ+hispanic+union+
                   kidlt6+earns,data=wage)
summary (resultados)


################ Verificando a presença de outliers pelo teste de Bonferroni

library (carData)
library(car)

outlierTest(resultados)

########################### Teste Reset de especificação do modelo

# install.packages("zoo")

library (zoo)
library (lmtest)

resettest(hrwage~husage+husearns+huseduc+husblck+hushisp+
            hushrs+kidge6+age+black+educ+hispanic+union+
            kidlt6+earns,power=2:3, type="regressor", data=wage)


# H0 = o modelo está corretamente especificado; 
# HA = o modelo está incorretamente especificado;

#Resultado do teste

#RESET test
#data:  hrwage ~ husage + husearns + huseduc + husblck + hushisp + hushrs +     kidge6 + age + black + educ + hispanic + union + kidlt6 +     earns
#RESET = 1.217, df1 = 28, df2 = 1751, p-value = 0.2009

# F tabelado:

qf(0.95, df1=28, df2=1751)

# Como o F calculado (1.217) é menor que o F tabelado (1,482789), 
# não existe erro de especificação do modelo

#Se houver erro de especiificação do modelo Ver rotina PANJEN (acima)
# Se rodarmos novamente a regressão preliminar ajustada com o novo
# modelo temos:


######################### Verificando a Multicolinearidade

# Correlação (Matriz de correlação)
# As variáveis binárias não entram na análise
cor(wage[,c("husage","husearns","huseduc", "hushrs","age","educ","earns")], 
    use="complete")

#Parece que a idade da esposa e do marido são correlacionadas

# Vamos ver o VIF - Valor de Inflação da Variância

library(car)

vif(lm(hrwage~husage+husearns+huseduc+
         hushrs+age+educ+earns,data=wage), type="high-order")

# Pelo VIF que é uma medida mais acurada da multicolinearidade
# percebe-se que husage e age ultrapassaram o escore de 5
# então são altamente correlacionadas, vamos excluir a idade do
# marido

############# Seleção de modelos por STEPWISE - usa AIC no cálculo

resultados <- lm(hrwage~husearns+huseduc+husblck+hushisp+
                   hushrs+kidge6+age+black+educ+hispanic+union+
                   kidlt6+earns,
                   data=wage)
summary (resultados)

library(RcmdrMisc)
step <- stepwise(resultados, direction= 'backward/forward', criterion ='AIC')
step

# Portanto o melhor modelo é:

resultados <- lm(hrwage~husearns+hushrs+kidge6+age+educ+union+
                   kidlt6+earns, data = wage)
summary(resultados)

################### HOMOCEDASTICIDADE ########################################
# Verificando a homocedasticidade = variância constante (amostra homogênea?)
# para grandes amostras - mais de 100 obs - utilizar o teste de Breusch-Pagan,
# caso contrário utilizar o teste de Goldfeld-Quandt (gqtest) descrito na 
# seção 11.5 do livro de Gujarati e Porter

bptest(hrwage~husearns+hushrs+kidge6+age+educ+union+
         kidlt6+earns,studentize=FALSE, data=wage)

# H0: homocedático (variâncias constantes); 
# HA: Heterocedastico (variâncias não constantes)

# O resultado do teste é:

#Breusch-Pagan test
#data:  hrwage ~ husearns + hushrs + kidge6 + age + educ + union + kidlt6 +     earns
#BP = 1376.3, df = 8, p-value < 2.2e-16

# O valor chiquadrado tabelado é:

qchisq(0.95, df=6, lower.tail = TRUE)

# O resultado deve ser confrontado em um teste qui-quadrado com k-1 
# graus de liberdade.
# Como o resultado do teste BP (1376.3) é maior que o tabelado 
# (12.59159) rejeita-se a hipótese de homocedasticidade 

#Vamos reduzir a variância das variáveis por meio do log das variáveis

wage$hrwage <- log(wage$hrwage)
wage$husearns <- log(wage$husearns)
wage$earns <- log(wage$earns)
wage$hushrs <- log(wage$hushrs)
wage$age <- log(wage$age)
wage$educ <- log(wage$educ)

# Agora o resultado do teste é:
#Breusch-Pagan test
#data:  hrwage ~ husearns + hushrs + kidge6 + age + educ + union + kidlt6 +     earns
#BP = 144.71, df = 8, p-value < 2.2e-16

#Ainda é elevado e persiste a heterocedasticidade, vamos utilizar a 
# regressão robusta

# Correção da variância não constante por regressão robusta

# A regressão normal é:

resultados <- lm(hrwage~husearns+hushrs+kidge6+age+educ+union+
                   kidlt6+earns, data = wage)
summary(resultados)

#Agora a regressão robusta:

# install.packages("robust")
library(robust)

resultrob <- lmRob(hrwage~husearns+hushrs+kidge6+age+educ+union+
                     kidlt6+earns,data=wage)
summary(resultrob)

AIC(resultados)
BIC(resultados)

#install.packages("AICcmodavg")
library(AICcmodavg)

# AICc ? usado para pequenas amostras
AICc(resultados)

#install.packages("performace")
library(performance)

# Menores valores de AIC, BIC, Sigma e RMSE são desejáveis
# Maiores valores de R2 e R2(adj) são melhores
# Sigma é o Erro Padrão dos Resíduos
# RMSE é a raiz quadrada média dos erros (menor é melhor)

model_performance(resultados)
model_performance(resultrob)


# Calculando os intervalos de confiança - Para regressão linear "normal"

confint(resultados, level = 0.95)

# Calculando os intervalos de confiança - Para a regressão robusta

confint(resultrob, level=0.95)

### Mas onde estão os resíduos??
resultrob$residuals
wage<- within(wage, {residuos <- residuals(resultrob) })

## Amostra é grande não precisa testar normalidade dos resíduos

### Mas onde estão os valores preditos (Y chapéu)
resultrob$fitted.values
wage<- within(wage, {preditos <- fitted.values(resultrob)})

################ Fazendo predições no modelos OLS 
#Para:
#age = 33 anos = 3.5
log(33)
#educ = 12 anos de estudo = 2.48
log(12)
#earns = 250 de renda semanal = 5.52
log(250)
#kidlt6 = 1 --> tem filhos com menos de 6 anos

#kidge6 =0 --> parâmetro não significativo a 95%
#union = 0  --> parâmetro não significativo a 95%
#husearns = 0 --> parâmetro não significativo a 95%
#hushrs = 0 --> parâmetro não significativo a 95%

predict(object = resultrob,
        data.frame(age=3.5, educ=2.48, earns=5.52, kidlt6 = 1,
                   husearns=0, hushrs=0, kidge6=0, union=0))

#o salario por hora é:
exp(1.831121)

anova.lmRob(resultrob)
anova(resultados)
