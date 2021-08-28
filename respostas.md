# De: Gisela Cesario de Araujo

## Exemplo 4.2
````R
exemplo42_parametro_media_desconhecida=function(n,media,dp){
z=2.58
erro=(z*(dp/sqrt(n)))
de=(media-erro)
ate=(media+erro)
return(c(de,ate))}
exemplo42_parametro_media_desconhecida(38,45,6)
[1] 42.48881 47.51119
### Conclusão: o tempo médio está no intervalo 42.48881;47.51119, com 99% de confiança
````

## Exemplo 4.3
````R
exemplo43_t_de_student=function(n,media,dp){
gl=n-1
t=2.08
estimativa_erro=(dp/sqrt(n))
erro=(t*estimativa_erro)
de=(media-erro)
ate=(media+erro)
return(c(de,ate))}
exemplo43_t_de_student(22,15,5)
[1] 12.78271 17.21729
### Conclusão: o tempo médio está no intervalo 12.78271;17.21729, com 95% de confiança
````

## Exemplo 4.4
````R
exemplo44_intervalo_diferenca_duas_medias=function(n1,media1,dp1,n2,media2,dp2){
z=1.96
diferenca=(media1-media2)
erro=(z*(sqrt(((dp1**2)/n1)+((dp2**2)/n2))))
de=(diferenca-erro)
ate=(diferenca+erro)
return(c(de,ate))}
exemplo44_intervalo_diferenca_duas_medias(30,21.3,2.6,30,13.4,1.9)
[1] 6.747649 9.052351
### Conclusão: com o intervalo 6.747649;9.052351, conclui-se que a resposta da questão é significativa, pois não não há 0 no intervalo (com 95% de confiança!)
````

## Exemplo 4.5
````R
exemplo45_intervalo_diferenca_duas_medias_amostras_pequenas=function(nh,mediah,vh,nm,mediam,vm){
t=2.86
diferenca=(mediah-mediam)
variancia=((((nh-1)*vh)+((nm-1)*vm))/(nh+(nm-2)))
erro=(t*sqrt(variancia*((1/nh)+(1/nm))))
de=(diferenca-erro)
ate=(diferenca+erro)
return(c(de,ate))}
exemplo45_intervalo_diferenca_duas_medias_amostras_pequenas(10,45.33,1.54,11,43.54,2.96)
[1] -0.09993731  3.67993731
### Conclusão: conclui-se, com 99% de confiança, que não há diferença entre as médias das amostras 
````

## Exemplo 4.6
````R
exemplo46_intervalo_confianca_proporcao=function(n,amostra){
z=1.96
estimativa=(amostra/n)
erro=(z*sqrt((estimativa*(1-estimativa))/n))
de=(estimativa-erro)
ate=(estimativa+erro)
return(c(de*100,ate*100))}
exemplo46_intervalo_confianca_proporcao(600,420)
[1] 66.33318 73.66682
### Conclusão: o intervalo 66.33318%;73.66682% contém a porcentagem de sucessos, com 95% de confiança
````

## Exemplo 4.7
````R
exemplo47_determinar_tamanho_amostra=function(de,ate){
z=1.96
n=4 #os dados não são assimétricos
dp=((ate-de)/n)
e=100
tamanho=(((z*dp)/e)**2)
return(tamanho)}
exemplo47_determinar_tamanho_amostra(50,1000)
[1] 21.66903
### Conclusão: conclui-se que aproximadamente 22 dados devem ser considerados, com 95% de confiança 
````

## Exemplo 4.8 (a)
````R
exemplo48a_amostra_confianca=function(d){
z=1.96
estimacao_p=0.25 # abordagem conservadora, amostra pode ser maior que o necessário
erro=(0.05^2)
tamanho=((1.96**2)*(0.25/(erro)))
return(tamanho)}
exemplo48a_amostra_confianca()
[1] 384.16
### Conclusão: considerando um erro de no máximo 5%, conclui-se que o tamanho necessário da amostra é de aproximadamentoe 364
````


## Q1
### (a) Calcule o coeficiente de correlação linear entre X e Y:

````R
coeficiente_correlacao_linar=function(x,y){
media_x=sum(x)/15
media_y=sum(y)/15
Sxx=sum(x*x)
Syy=sum(y*y)
Sxy=sum(x*y)
return((Sxy-(media_x*media_y*15))/(sqrt((Sxx-(media_x*media_x*15))*(Syy-(media_y*media_y*15)))))}
x=matrix(c(2,2,2,4,4,4,6,6,6,8,8,8,10,10,10),ncol=1)
y=matrix(c(2.1,1.8,1.9,4.5,4.2,4.0,6.2,6.0,6.5,8.2,7.8,7.7,9.6,10,10.1),ncol=1)
coeficiente_correlacao_linar(x,y)
[1] 0.9960838
````
### (b) Obtenha a reta de regressão da variável Y em função de X:

````R
reta_regresao_linear=function(x,y){
media_x=sum(x)/15
media_y=sum(y)/15
media_xY=(sum(x)/15)*(sum(y)/15)
Sxx=sum(x*x)
Sxy=sum(x*y)
b=(Sxy-(media_xY*15))/(Sxx-(media_x*media_x*15))
a=(media_y-(b*media_x))
return(matrix(c(a,b),ncol=1))}
reta_regresao_linear(x,y)
plot(reta_regresao_linear(x,y))
[1,] 0.16
[2,] 0.98
````
![Alt Reta](https://github.com/gicesario/funcoes-linguagem-R/blob/main/q1_b.jpeg "Reta")

## Q2
### (a) Usando todos os dados, encontre os limites de controle para os gráfico R, construa o gráfico e plote os dados.

````R
amostras = matrix(c(34.2,31.6,31.8,33.4,35.0,32.1,32.6,33.8,34.8,38.6,35.4,34.0,36.0,37.2,35.2,33.4,35.0,34.4,33.9,34.0), ncol=1)

q_amostras <- qcc(data = amostras,
                   type = "R",
                   sizes = 5,
                   plot = TRUE)
summary(q_amostras)

#R chart for amostras
#Summary of group statistics:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#31.60   33.40   34.10   34.32   35.05   38.60

#Group sample size:  5
#Number of groups:  20
#Center of group statistics:  34.32
#Standard deviation:  0
````

![Alt Reta](https://github.com/gicesario/funcoes-linguagem-R/blob/main/q2_amostras.jpeg "amostras")

````R
amplitude = matrix(c(3,4,4,5,4,2,7,9,10,4,8,6,4,7,3,10,4,7,8,4), ncol=1)

q_amplitude <- qcc(data = amplitude,
                   type = "R",
                   sizes = 5,
                   plot = TRUE)
summary(q_amplitude)

#R chart for amostras
#Summary of group statistics:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#2.00    4.00    4.50    5.65    7.25   10.00

#Group sample size:  5
#Number of groups:  20
#Center of group statistics:  5.65
#Standard deviation:  0
````
![Alt Reta](https://github.com/gicesario/funcoes-linguagem-R/blob/main/q2_amplitude.jpeg "Amplitude")

## b) Avalie no gráfico se existem amostras fora los limites de controle e interpretre.
Resposta: De acordo com o gráfico da amosta, verifica-se que o ponto 10 é o único que está fora de destoante. Além disso, percebe-se que os dados estão relativamente aleatórios.


## Q3
### a)Você acha que há evidência suficiente ao nível 5% de significância para indicar uma diferença (populacional) entre os resultados médios para os três métodos, após o tratamento? 

````R
# Teste Anova
amostras <- matrix(c(73,83,76,68,80,54,74,71,60,50,79,95,87,90,80),ncol=1)
metodos <- rep(c("ma","mb","mc"),each=5)
dados <- data.frame(amostras,metodos)
modelo.anova <- lm(amostras ~ metodos, data= dados)
summary(modelo.anova)
anova(modelo.anova)
Residuals:
   Min     1Q Median     3Q    Max
 -11.8   -6.7    0.0    5.5   12.2

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   76.000      3.553  21.388 6.36e-11 ***
metodosmb    -14.200      5.025  -2.826   0.0153 *
metodosmc     10.200      5.025   2.030   0.0652 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 7.946 on 12 degrees of freedom
Multiple R-squared:  0.6647,    Adjusted R-squared:  0.6088
F-statistic: 11.89 on 2 and 12 DF,  p-value: 0.001422
````
Resposta: Com base na tabela ANOVA é possível perceber que a confiabilidade é igual a 0.001422. Inferior à 5%
### b) Caso ocorra diferença aplique o teste Tukey a 5% de significância para avaliar se existe um melhor método
Resposta: Aplicando tukey com 95% de confiança 
 ````R
modelo.tukey <- aov(amostras ~ metodos, data= dados)
TukeyHSD(modelo.tukey)
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = amostras ~ metodos, data = dados)

$metodos
       diff        lwr        upr     p adj
mb-ma -14.2 -27.606734 -0.7932662 0.0378263
mc-ma  10.2  -3.206734 23.6067338 0.1473871
mc-mb  24.4  10.993266 37.8067338 0.0010612
````
