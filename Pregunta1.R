x<-c(0,1)
fx<-c(0.68,0.32)

cbind(x,fx)
#ensayo Bernoulli
plot(x,fx,ylim=c(0,1),pch=16,col="red")
lines(x,fx,type="h",col="red")

mu<-sum(x*fx)
mu

sigmassq<-sum(((x-mu)^2)*fx)
sigmassq

# Bernoulli, coger una bola y volvarla a meter da el resultado
n<-43
sample(x,n,prob=fx,replace=TRUE)
y<-sample(x,n,prob=fx,replace=TRUE) # resultado de una encuesta
sum(y) # suma de resultados
# y= número de 1 en 13
n<-43
Y<-function(i){sum(sample(x,n,prob=fx,replace=TRUE))} 
m<-400000
barplot(table(sapply(1:m,Y)))

encuestas<-sapply(1:m,Y)
fi<-table(encuestas)/m
Fi<-cumsum(fi)

cbind(2:29,fi,Fi)

dbinom(13,43,0.32)#respuesta


resultados<-0:43
fy<-dbinom(resultados,43,0.32)
Fy<-cumsum(fy)

cbind(resultados,fy,Fy)

mu<-sum(resultados*fy)
mu

43*0.32

sigmassq<-sum((resultados-mu)^2*fy)
sigmassq

43*0.32*0.68

plot(resultados,fy,ylim=c(0,0.2),pch=16,col="red")
lines(resultados,fy,type="h",col="red")

pbinom(16,44,0.32)

plot(resultados,Fy,type="s",col="red")


######

dbinom(10,24,0.68)
#mu=E(x)=n*p , n=numero experimentos, p=0.68
#Var(x)=n*p(1-p)

24*0.68

24*0.68*0.32

#####

Fy<-pbinom(0:24,24,0.68)
plot(0:24,Fy,type="s",col="red")

qbinom(0.25,24,0.68)
