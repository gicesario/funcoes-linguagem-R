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
