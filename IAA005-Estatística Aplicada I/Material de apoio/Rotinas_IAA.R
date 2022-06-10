
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

salarios$age_new <- with (salarios, age*1)


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

# install.packages("Rcmdr")
library (Rcmdr)

with(salarios, Hist(age, scale="frequency", breaks="Sturges", 
                    col="darkgray"))


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

library (car)

Boxplot( ~ educ, data=salarios, id=list(method="y"))

# Dispersão (scatter plot) X Y

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


###############################  AULA 2 #####################################

####ceos <- read.table("C:/iaa/estat_I/ceo.txt", header = TRUE, sep = "", 
####                   na.strings = "NA", dec = ".", strip.white = TRUE)

####save ("ceos", file = "C:/iaa/estat_I/ceos.RData")

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


# Distribuição Normal padronizada (Z)

# para 99.9% de confiança o valor de "Z" para lado da distribuição é 1.644 

install.packages("RcmdrMisc")
install.packages("tigerstats")
library(RcmdrMisc)
library(tigerstats)

qnorm(0.95)

local({
  .x <- seq(-1.644, 1.644, length.out=1000)  
  plotDistr(.x, dnorm(.x, mean=0, sd=1), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("Normal Distribution (Z):  Mean=0, Standard deviation=1"))
})

pnormGC(c(-1.644,1.644),region="between",mean=0,
        sd=1,graph=TRUE)


# Distribuição t de Student

# para 95,0% de confiança e 100 graus de liberdade t= 1.660

qt(0.95, 100)

local({
  .x <- seq(-1.660, 1.660, length.out=1000)  
  plotDistr(.x, dt(.x, df=100), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("t Distribution: Degrees of freedom=100"))
})

ptGC(c(-1.660,1.660),region="between",df=100, graph = TRUE)

# Distribuição ChiSquare

# Graus de liberdade = 5 e 5.0% (1.145) na cauda inferior e 
# 95.0% (11.070) na cauda superior

qchisq(0.05,5)
qchisq(0.95,5)

local({
  .x <- seq(1.145, 11.070, length.out=1000)  
  plotDistr(.x, dchisq(.x, df=5), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("ChiSquared Distribution: Degrees of freedom=5"))
})

pchisqGC(c(11.070),region="below",df=5, graph = TRUE)


# Distribuição F de Fischer e Snedecor

# install.packages("graphics")
library(graphics)


# com 20 (numerador) e 19 graus de liberdade (denominador)

qf(0.95, 20, 19)
qf(0.05, 20, 19)

local({
  .x <- seq(0.467, 2.155, length.out=1000)  
  plotDistr(.x, df(.x, df1=20, df2=19), cdf=FALSE, xlab="x", ylab="Density", 
            main=paste("F Distribution: Numerator df=20, denominator df=19"))
})


# Distribuição logística

qlogis(0.999)

local({
  .x <- seq(-6.906, 6.906, length.out=1000)  
  plotDistr(.x, plogis(.x, location=0, scale=1), cdf=TRUE, xlab="x", 
            ylab="Cumulative Probability", 
            main=paste("Logistic Distribution:  Location=0, Scale=1"))
})


########### Estatísticas descritivas


# Média

mean <- mean (salarios$husearns)

mean

# Mediana

median <- median (salarios$husage)

median

# Moda

table(salarios$husage)
subset(table(salarios$husage), 
       table(salarios$husage) == max(table(salarios$husage)))


# Variância

var <- var (salarios$age)

var

# Desvio Padrão

sd <- sd (salarios$husearns)

sd

# Coeficiente de Variação

cv <- (sd/mean)

cv


# Valores Minimo e Máximo

range(salarios$husearns)


# Quantis

Q1 <- quantile(salarios$husearns, probs = 0.25)

Q1

Q2 <- quantile(salarios$husearns, probs = 0.50)

Q2

Q3 <- quantile(salarios$husearns, probs = 0.75)

Q3



#Distancia interquantílica (IQR)

husearns = salarios$husearns

IQR(husearns)


# Percentis

quantile(husearns, c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) 



############################### AULA 3 ########################################


# Distribuição amostral da média (dam) - população infinita

#Exemplo do slide: Média = m = 150; Desvio padrão da amostra = s = 36; n = 36

m <- 150

s <- 36

n <- 36

dam <- s/n^0.5

dam

dam1 <- m + dam

dam2 <- m - dam

dam1

dam2

# Exemplo para a variável husearns da base de dados salários

# criando uma variável "n"

salarios$n <- with(salarios, 1)

# somando os valores de n para obter o tamanho da amostra

n <- sum (salarios$n)

m <- mean(salarios$husearns)

s <- sd(salarios$husearns)

dam <- s/n^0.5

dam1 <- m + dam
dam2 <- m - dam 

dam1
dam2

# Distribuição amostral da média (damf) - população finita

# Exemplo: n = 16; N = 100; s = 57

n <- 16

N <- 100

s <- 57


damf <- (s/(n^0.5))*((N-n)/(N-1))^0.5

damf


# Intervalo de confiança para a média (Z)

library(carData)

library(datasets)

install.packages("BSDA")

library(BSDA)

sd <- sd(ceo$sales)

x <- ceos$sales

z.test(x, y = NULL, alternative = "two.sided", mu = 0, sigma.x = sd,
       sigma.y = NULL, conf.level = 0.95)

# Intervalo de confiança para a média (t)

 install.packages("stats")

library(stats)

t.test(husearns, y = NULL,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95, data=salarios)


# Intervalo de confiança para a variância pressupondo
# que a "distribuição é normal"

var <- var(salarios$husearns)

n <- sum (salarios$n)

# obtendo os valores de chi quadrado

chiinf <- qchisq(0.025, df=5633)
chisup <- qchisq(0.975, df=5633)

superior <- ((n-1)*var)/chiinf
inferior <- ((n-1)*var)/chisup

inferior
superior

# Teste para a diferença entre duas médias (z)

library(BSDA)

# calculando os desvios padrão de x e y

sdx <- sd(salarios$husearns)

sdy <- sd(salarios$earns)


# Criando os objetos com as variáveis em análise

x <- salarios$husearns

y <- salarios$earns

z.test(x, y, alternative = "two.sided", mu = 0, sigma.x = sdx,
       sigma.y = sdy, conf.level = 0.95)


# Teste para a diferença entre duas médias (t)

library(stats)

x <- salarios$husearns

y <- salarios$earns

t.test(x, y,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


# Teste de diferença entre duas variâncias (F)

library(stats)

x <- salarios$husearns

y <- salarios$earns

var.test(x, y, alternative = "two.sided", conf.level = 0.95)


# Teste de normalidade de Kolmogorov-Smirnov

install.packages("RcmdrMisc")

library(RcmdrMisc)

normalityTest(~sales, test="lillie.test", data=ceo)

normalityTest(~salary, test="lillie.test", data=ceo)

#  tabela de kolmogorv-smirnov em:
#  http://www.portalaction.com.br/inferencia/62-teste-de-kolmogorov-smirnov
# Valor crítico de Kolmogorov-smirnov para nosso exemplo = 0.018133333
# Os valores calculados são D=0.18829 para "earns" e D=0.13256
# H0: A amostra provem de uma população normalmente distribuída
# Ha: A amostra não provem de uma população normalmente distribuída
# como os valores "D" calculados são maiores que o valor crítico (tabelado) 
# estes se encontram na região de rejeição de H0, ou seja rejeita-se "normalidade"
# REGRA DE BOLSO: se P-value > 0.05, existe normalidade da amostra. 


# Teste de independencia/equivalência ou igualdade entre duas amostras (t)
# Pressuposto: as variâncias devem ser estatisticamente iguais

 install.packages("ggpubr")

library(ggpubr)

X <- salarios$husearns

y <- salarios$earns

t.test(x, y, alternative = "two.sided", var.equal = TRUE)


# Teste de Wilcoxon-Mann-Whitney para amostras independentes 

library(stats)

X <- salarios$husearns

y <- salarios$earns

wilcox.test(x, y, alternative = "two.sided") 

# Se no Resultado p-value <= 0.05; então as amostras são independentes 




# Correlação (Matriz de correlação)

cor(salarios[,c("earns","husearns","age")], use="complete")


# REGRESSÃO POR MQO (MÍNIMOS QUADRADOS ORDINÁRIOS)

# Importar conjunto de dados

ceo <- read.table("C:\iaa\estat_1\ceo.txt", header = TRUE, sep = "", 
                  na.strings = "NA", dec = ".", strip.white = TRUE)

# Adicionar número das observações nos dados, verificar quantas linhas
# tem o arquivo e usar o seguinte comando

ceo$ObsNumber <- 1:177


# Salvar conjunto de dados

save ("ceo", file = "C:\iaa\estat_1\ceo.Rdata")


# Estimando um modelo preliminar

resultados <- lm (salary~age+college+comten+grad+mktval+sales, data=ceo)
summary (resultados)


# "FIM" da aula 3


# Verificando a presença de outliers pelo teste de Bonferroni

library (carData)
library(car)

outlierTest(resultados)

#Resultado do teste de outliers
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#   rstudent unadjusted p-value Bonferroni p
# 60  3.52176         0.00055369     0.096343
# Não precisa fazer nada porque não existem outliers


# Exemplo de teste com a presença de outliers
#    rstudent unadjusted    p-value           Bonferroni p
#103 11.26731               3.1213e-22         5.5246e-20
#74   3.94004               1.1979e-04         2.1202e-02
# Quer dizer que as observações das linhas 103 e 74 são outliers e 
# devem ser removidas, neste caso vc deve deletar as linhas indicadas
# na tabela de dados. 
# Editando o conjunto de dados (eliminação das linhas 103 e 74)
# ceo <- ceo[ceo$ObsNumber != 103, ]
# ceo <- ceo[ceo$ObsNumber != 74, ]
# Depois, deve-se refazer a estimativa dos resultados e posteriormente
# refazer o teste de outliers
# Se necessário deve-se extrair novas obserções e recalcular o modelo
# e o teste de outlier

# Teste Reset de especificação do modelo

# install.packages("zoo")

library (zoo)
library (lmtest)

resettest(salary~age+college+comten+grad+mktval+sales, 
          power=2:3, type="regressor", data=ceo)

# H0 = o modelo está corretamente especificado; 
# HA = o modelo está incorretamente especificado;

#Resultado do teste
# RESET test

# data:  salary ~ age + college + comten + grad + mktval + sales
# RESET = 2.6122, df1 = 12, df2 = 155, p-value = 0.003378

# F tabelado = 1,83

# Como o F calculado (2,6122) é maior que o F tabelado (1,83) existe
# erro de especificação do modelo

# ceo$agenew <- with (ceo, age*age) --> transformação para idade ao quadrado

# ceo$lnsalary <- with (ceo, log(salary))  --> tranformação para logaritmo
# neperiano

# Modelo em logaritmos é uma alternativa, variáveis binárias não são 
# transformadas


ceo$lnsalary <- with(ceo, log(salary))

ceo$lnage <- with(ceo, log(age))

ceo$lncomten <- with(ceo,log(comten))

ceo$lnsales <- with(ceo, log(sales))

ceo$lnmktval <- with(ceo, log(mktval))


# Refazendo o teste reset de especifica??o do modelo

resettest(lnsalary~lnage+college+lncomten+grad+lnmktval+lnsales, 
          power=2:3, type="regressor", data=ceo)

# Resultado do teste
# RESET test

# data:  lnsalary ~ lnage + college + lncomten + grad + lnmktval + lnsales
# RESET = 1.342, df1 = 12, df2 = 155, p-value = 0.2002

# F tabelado = 1,83

# Como o F calculado (1,342) ? menor que o F tabelado (1,83) não existe 
# erro de especificação do modelo
# ou seja, o modelo é log-linear.


# Verificando a autocorrelação dos resíduos - empregável para dados 
# em séries de tempo

library(lmtest)

dwtest(lnsalary~lnage+college+lncomten+grad+lnmktval+lnsales, 
       alternative="greater", data=ceo)

# Durbin-Watson test

# data:  lnsalary ~ lnage + college + lncomten + grad + lnmktval + lnsales
# DW = 2.0505, p-value = 0.6329
# alternative hypothesis: true autocorrelation is greater than 0

# HIPÓTESE NULA              DECISÃO                SE
# não existe autocorr +      rejeitar           0 < d < dl
# não existe autocorr +      sem decisão        dl <= d <= du
# não existe autocorr -      rejeitar           4 - dl < d < 4
# não existe autocorr -      sem decisão        4 - du <= d <= 4 - dl
# nenhuma autocorr + ou -    não rejeitar       du < d < 4-du ***

# k = 6, n = 177 
## dl = 1,651, du = 1,817

# 4 - dl = 2,349
# 4 - du = 2,183

# portanto não existe nenhuma autocorrelação
# caso exista autocorrelação positiva ou negativa utilizar o método da 
# primeira diferença exposto na seção 12.9 "método da primeira diferença"
# do livro do Gujarati e Porter
# Alternativamente pode-se utilizar o estimador sandwich "HAC"



# Verificando a homocedasticidade = variância constante (amostra homogênea?)
# para grandes amostras - mais de 100 obs - utilizar o teste de Breusch-Pagan,
# caso contrário utilizar o teste de Goldfeld-Quandt (gqtest) descrito na 
# seção 11.5 do livro de Gujarati e Porter

bptest(lnsalary ~ lnage + college + lncomten + grad + lnmktval + lnsales, studentize=FALSE, data=ceo)

# H0: homocedático (variâncias constantes); 
# HA: Heterocedastico (variâncias não constantes)

# Breusch-Pagan test

#  data:  lnsalary ~ lnage + college + lncomten + grad + lnmktval + lnsales
#  BP = 23.666, df = 6, p-value = 0.0006014

# O resultado deve ser confrontado em um teste qui-quadrado com k-1 graus de 
# liberdade.
# Se o resultado do teste BP (23.666) é maior que o tabelado (12,5916) 
# rejeita-se a hipétese de homocedasticidade 


# Correção da variância não constante por regressão robusta

save ("ceo", file = "C:/iaa/estat_1/ceo.Rdata")

library(Rcmdr)
library (lmtest)
library (sandwich)

resultados <- lm(lnsalary~college+grad+lnage+lncomten+lnmktval+lnsales, data=ceo)
summary(resultados)

coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))


# Seleção de modelos por STEPWISE - usa AIC no cálculo

stepwise(resultados, direction= 'backward/forward', criterion ='AIC')

# Portanto o melhor modelo é:

resultados <- lm(lnsalary ~ lncomten+lnmktval+lnsales, data = ceo)
summary(resultados)

coeftest(resultados, vcov=vcovHC(resultados, type="HC1"))


# Escolhendo o melhor modelo pelos critérios AIC e BIC (AIC é incluso na 
# análise step.lmRob da regressão robusta)
# Modelos com menores valores de AIC e BIC são os melhores

AIC (resultados)
BIC (resultados)


# Calculando os intervalos de confiança - Para regressão linear "normal"

# confint (resultados, level = 0.95)


# Calculando os intervalos de confiança - Para regressão linear "robusta"

temp <- summary(resultados, robust = TRUE)

CI.LL <- temp$coefficients[,1] - 1.96 * temp$coefficients[,2]
CI.UL <- temp$coefficients[,1] + 1.96 * temp$coefficients[,2]

cbind(CI.LL, CI.UL)

