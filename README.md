### Q1
> (a) Calcule o coeficiente de correlação linear entre X e Y:

````R
coeficiente=function(x,y){
n=length(x)
MedX=sum(x)/n
MedY=sum(y)/n
SomX2=sum(x*x)
SomY2=sum(y*y)
SomXY=sum(x*y)
r=(SomXY-(n*MedX*MedY))/(sqrt((SomX2-(n*MedX*MedX))*(SomY2-(n*MedY*MedY))))
return(r)}
x=c(2,2,2,4,4,4,6,6,6,8,8,8,10,10,10)
y=c(2.1,1.8,1.9,4.5,4.2,4.0,6.2,6.0,6.5,8.2,7.8,7.7,9.6,10,10.1)
coeficiente(x,y)
[1] 0.9960838
````
> (b) Obtenha a reta de regressão da variável Y em função de X:

````R
Reta=function(x,y){
n=length(x)
MedX=mean(x)
MedY=mean(y)
MedXY=mean(x)*mean(y)
SomX2=sum(x*x)
SomXY=sum(x*y)
betha=(SomXY-n*MedXY)/(SomX2-n*(MedX*MedX))
alpha=MedY-betha*MedX
return(c(alpha,betha))}
Reta(x,y)
[1] 0.16 0.98 
plot(Reta(x,y))
````
![Alt Reta](https://github.com/gicesario/funcoes-linguagem-R/blob/main/q1_b.jpeg "Reta")

###Q2
> (a) Usando todos os dados, encontre os limites de controle para os gráfico R, construa o gráfico e plote os dados.

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






