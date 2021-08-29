> Gisela Cesario de Araujo

# PARTE 1

## Exemplo 4.2
````R
exemplo42_parametro_media_desconhecida=function(n,media,dp){
z=qnorm(0.99,0,1)
erro=(z*(dp/sqrt(n)))
de=(media-erro)
ate=(media+erro)
return(c(de,ate))}
exemplo42_parametro_media_desconhecida(38,45,6)
[1] 42.7357 47.2643
````

## Exemplo 4.3
````R
exemplo43_t_de_student=function(n,media,dp){
gl=n-1
t=qt(0.975,gl)
estimativa_erro=(dp/sqrt(n))
erro=(t*estimativa_erro)
de=(media-erro)
ate=(media+erro)
return(c(de,ate))}
exemplo43_t_de_student(22,15,5)
[1] 12.78312 17.21688
````

## Exemplo 4.4
````R
exemplo44_intervalo_diferenca_duas_medias=function(n1,media1,dp1,n2,media2,dp2){
z=qnorm(0.975,0,1)
diferenca=(media1-media2)
erro=(z*(sqrt(((dp1**2)/n1)+((dp2**2)/n2))))
de=(diferenca-erro)
ate=(diferenca+erro)
return(c(de,ate))}
exemplo44_intervalo_diferenca_duas_medias(30,21.3,2.6,30,13.4,1.9)
[1] 6.74767 9.05233
````

## Exemplo 4.5
````R
exemplo45_intervalo_diferenca_duas_medias_amostras_pequenas=function(nh,mediah,vh,nm,mediam,vm){
t=qt(0.995,(nh+(nm-2)))
diferenca=(mediah-mediam)
variancia=((((nh-1)*vh)+((nm-1)*vm))/(nh+(nm-2)))
erro=(t*sqrt(variancia*((1/nh)+(1/nm))))
de=(diferenca-erro)
ate=(diferenca+erro)
return(c(de,ate))}
exemplo45_intervalo_diferenca_duas_medias_amostras_pequenas(10,45.33,1.54,11,43.54,2.96)
[1] -0.1005549  3.6805549
````

## Exemplo 4.6
````R
exemplo46_intervalo_confianca_proporcao=function(n,amostra){
z=qnorm(0.975,0,1)
estimativa=(amostra/n)
erro=(z*sqrt((estimativa*(1-estimativa))/n))
de=(estimativa-erro)
ate=(estimativa+erro)
return(c(de*100,ate*100))}
exemplo46_intervalo_confianca_proporcao(600,420)
[1] 66.33324 73.66676
````

## Exemplo 4.7
````R
exemplo47_determinar_tamanho_amostra=function(de,ate){
z=qnorm(0.975,0,1)
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
exemplo48a_amostra_confianca=function(erro_maximo){
z=qnorm(0.975,0,1)
estimacao_p=0.25 # abordagem conservadora, amostra pode ser maior que o necessário
erro=(erro_maximo**2)
tamanho=((1.96**2)*(0.25/(erro)))
return(tamanho)}
exemplo48a_amostra_confianca(0.05)
[1] 384.16
### Conclusão: considerando um erro de no máximo 5%, conclui-se que o tamanho necessário da amostra é de aproximadamentoe 365
````

## Exemplo 4.8 (b)
````R
exemplo48b_amostra_confianca_estimativa_previa=function(erro_maximo, estimativa_previa){
z=qnorm(0.975,0,1)
erro=(erro_maximo**2)
tamanho=((1.96**2)*((estimativa_previa*(1-estimativa_previa))/(erro)))
return(tamanho)}
exemplo48b_amostra_confianca_estimativa_previa(0.07,0.6)
[1] 188.16
### Conclusão: considerando um erro de no máximo 7%, conclui-se que o tamanho necessário da amostra é de aproximadamentoe 189
````

## Exemplo 4.8 (c)
````R
exemplo48c_amostra_mais_adequada=function(populacao,amostra1,amostra2){
n1=(populacao*amostra1)/(populacao+amostra1)
n2=(populacao*amostra2)/(populacao+amostra2)
if(n1>n2)
    return(amostra1)
else
    return(amostra2)}
exemplo48c_amostra_mais_adequada(2000,385,189)
[1] 385
###Conclusão: a amostra a ser considerada é a 385
````

## Exemplo 4.9
````R
exemplo49_amostra_mais_confiavel_com_estimativa_apriori=function(dp,populacao){
z=qnorm(0.995,0,1)
amostra=(z*dp)**2
amostra_corrigida=((populacao*amostra)/(populacao+amostra))
return(amostra_corrigida)}
exemplo49_amostra_mais_confiavel_com_estimativa_apriori(2.8,200)
[1] 41.28092
### Conclusão: o tamanho aproximado da amostra é 42, com erro máximo de 1%
````

## Exemplo 5.1
````R
exemplo52_teste_hipotese=function(confianca, n, media_ammostra, media_populacao, dp){
z_calc=((media_ammostra-media_populacao)/(dp/sqrt(n)))
z_teorico=-qnorm(confianca,0,1)
if (z_calc<z_teorico)
  conclusao="H0 rejeirado"
else
   conclusao="H0 nao rejeitado"
return(c(z_calc,z_teorico,conclusao))}
exemplo51_teste_hipotese(0.95, 27, 167, 171, 9)
[1] "-2.3094010767585"  "-1.64485362695147" "H0 rejeirado"
````

## Exemplo 5.2

````R
exemplo52_teste_hipotese=function(confianca, n, media_ammostra, media_populacao, dp){
z_calc=((media_ammostra-media_populacao)/(dp/sqrt(n)))
z_teorico=-qnorm(confianca,0,1)
if (z_calc<z_teorico)
  conclusao="H0 rejeirado"
else
   conclusao="H0 nao rejeitado"
return(c(z_calc,z_teorico,conclusao))}
exemplo51_teste_hipotese(0.95, 27, 167, 171, 9)
[1] "-2.3094010767585"  "-1.64485362695147" "H0 rejeirado"
````

## Exemplo 5.3

````R
exemplo53_teste_sem_desvio_padrao_populacao=function(confianca,n,media_amostra,media_populacao,dp){
t_calc=(media_amostra-media_populacao)/(dp/(sqrt(n)))
p_valor=pt(t_calc,n-1)
t_teorico=-qt(confianca, n-1)
if (t_calc<t_teorico)
  conclusao="H0 rejeitado"
else
   conclusao="H0 nao rejeitado"
return(c(t_calc,t_teorico,conclusao))}
exemplo53_teste_sem_desvio_padrao_populacao(0.99,16,495,500,5)
[1] "-4" "-2.60248029501112" "H0 rejeitado"
````

## Exemplo 5.4
````R
exemplo54_teste_amostras_desvio_padrao_independentes=function(confianca,racao_a,racao_b){
dp_ra=3.12
dp_rb=0.11
na=length(racao_a)
nb=length(racao_b)
media_a=mean(racao_a)
media_b=mean(racao_b)
s_aux1=((na-1)*dp_ra**2)+((nb-1)*dp_rb**2)
gl=(na+nb-2)
sp=s_aux1/gl
t_calc=(media_a-media_b)/sqrt(sp*(1/na+1/nb))
t_teorico=qt(confianca, gl)
if (t_calc>t_teorico)
  conclusao="H0 rejeirado"
else
   conclusao="H0 nao rejeitado"   
return(c(t_calc, conclusao))}
a=c(3.40,2.99,3.21,3.07,3.01,3.27,3.23,3.02)
b=c(2.82,3.16,2.98,3.04,3.15,3.20,3.00,3.01,3.08,3.06)
exemplo54_teste_amostras_desvio_padrao_independentes(0.95,a,b)
[1] "0.102074757987168" "H0 nao rejeitado"
````

## Exemplo 5.5
````R
exemplo55_teste_amostras_desvio_padrao_independentes=function(confianca,c1,c2){
n=length(c1)
sc1=var(c1)
sc2=var(c2)
media_c1=mean(c1)
media_c2=mean(c2)
t_aux1=(media_c1-media_c2)
t_aux2=(sqrt((sc1/n)+(sc2/n)))
t_calc=t_aux1/t_aux2
t_teorico=qt(confianca, n-1)
if (t_calc>t_teorico)
  conclusao="H0 rejeirado"
else
   conclusao="H0 nao rejeitado"  
return(c(t_calc,t_teorico,conclusao))}
c1=c(101.2,102.0,100.8,102.3,101.6)
c2=c(100.0,102.8,101.5,99.0,102.0)
exemplo55_teste_amostras_desvio_padrao_independentes(0.95,c1,c2)
[1] "0.703731550548992" "2.13184678632665"  "H0 nao rejeitado"
````

## Exemplo 5.6
````R
exemplo56_teste_amostras_dependentes=function(confianca,dif){
n=length(dif)
media=mean(dif)
dp=sd(dif)
t_calc=media/(dp/(sqrt(n)))
t_teorico=-qt(confianca, n-1)
if (t_calc>t_teorico)
  conclusao="H0 rejeitado"
else
   conclusao="H0 nao rejeitado"  
return(c(t_calc, t_teorico, conclusao))}
dif=c(-19,-20,0,10,-16,-20,-8,-4,-25,-5)
exemplo56_teste_amostras_dependentes(0.995,dif)
[1] "-3.05769759183389" "-3.24983554159213" "H0 rejeitado"
````

## Exemplo 5.7
````R
exemplo57_teste_proporcao_populacional=function(confianca,populacao,amostra,previsao){
p_frequencia=amostra/populacao
z_calc=(p_frequencia-previsao)/(sqrt((previsao*(1-previsao))/populacao))
z_teorico=-qnorm(confianca,0,1)

if (z_calc<z_teorico)
  conclusao="H0 rejeitado"
else
   conclusao="H0 nao rejeitado"  
return(c(z_calc, z_teorico, conclusao))}
exemplo57_teste_proporcao_populacional(0.95,200,98, 0.57)
[1] "-2.28524795616017" "-1.64485362695147" "H0 rejeitado"
````

## Exemplo 5.8
````R
exemplo58_teste_proporcao_duas_populacoes=function(confianca,amostra_romi,total_romi,amostra_concorrente,total_concorrente){
pr=amostra_romi/total_romi
pc=amostra_concorrente/total_concorrente
p=((total_romi*pr)+(total_concorrente*pc))/(total_romi+total_concorrente)
z_calc=(pr-pc)/(sqrt((p*(1-p)*(1/total_romi+1/total_concorrente))))
z_teorico=qnorm(confianca,0,1)
if (z_calc>z_teorico)
  conclusao="H0 rejeitado"
else
   conclusao="H0 nao rejeitado"  
return(c(z_calc, z_teorico, conclusao))}
exemplo58_teste_proporcao_duas_populacoes(0.99,171,180,171,190)
[1] "1.81757294999992" "2.32634787404084" "H0 nao rejeitado"
````

## Exemplo 5.9
````R
exemplo59_teste_aderencia=function(confianca,amostra,esperado,observado){
total=sum(a)
gl=length(a)-1
qui2=sum(observado**2/esperado)
x_calc=qchisq(confianca,gl)

if (qui2>x_calc)
  conclusao="H0 rejeitado"
else
   conclusao="H0 nao rejeitado"  

return(c(qui2,x_calc,conclusao))}
amostra=c(30,18,26,13,7,8,17)
esperado=c(17,17,17,17,17,17,17)
observado=c(13,1,9,-4,-10,-9,0)
exemplo59_teste_aderencia(0.975,amostra,esperado,observado)
[1] "26.3529411764706" "14.4493753354479" "H0 rejeitado"
````

## Exemplo 5.10
````R
exemplo510_teste_tabelas_contingencia=function(observado,esperado){
total=sum(observado)
gl=6
dados=cbind(observado,esperado)
qui2=chisq.test(dados)
return(c(qui2))}
observado=c(15,27,50,43,8,13,9,10,25,30,12,8)
esperado=c(25.92,37.80,38.34,32.94,14.40,21,21.30,18.30,7.68,11.20,11.36,9.76)
exemplo510_teste_tabelas_contingencia(observado,esperado)
X-squared
 36.67406
$p.value
[1] 0.0001306524
###Conclusão: Não há evidências suficiente para rejeitar H0, pois p-valor(0.0001306524) é baixo
````


















# PARTE 2
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
