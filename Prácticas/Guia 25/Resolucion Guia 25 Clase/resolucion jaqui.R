lidar<-read.table("lidar.txt", header = T, sep = "")
names(lidar)<-c("range", "logratio")
attach(lidar)


# separo un %10 para el training set
set.seed(100)
indices<-sort(sample(1:length(range), 22, replace = FALSE))

lidar.T<-lidar[-indices,]
lidar.V<-lidar[indices,]



potencias<-function(i){
  return(lidar.T$range^i)
}





#1
modelo.4<-lm(formula = lidar.T$logratio ~ potencias(1)+potencias(2)+potencias(3)+potencias(4))

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.4), lwd=2, col="magenta")




#2

modelo.9<-lm(formula = lidar.T$logratio ~ potencias(1)+potencias(2)+potencias(3)+potencias(4)+potencias(5)+potencias(6)+potencias(7)+potencias(8)+potencias(9))
modelo.9

modelo.10<-lm(formula = lidar.T$logratio ~ potencias(1)+potencias(2)+potencias(3)+potencias(4)+potencias(5)+potencias(6)+potencias(7)+potencias(8)+potencias(9)+potencias(10))
modelo.10

# el coeficiente 9 de estos modelos da NA ????

lines(lidar.T$range, predict(modelo.9), lwd=5, col="blue")
lines(lidar.T$range, predict(modelo.10), lwd=2, col="red")





#3
#paso a variables porque si bien no hay problemas con la funcion lm(), la funcion predict() solo acepta variables

potencias1<-potencias(1)
potencias2<-potencias(2)
potencias3<-potencias(3)
potencias4<-potencias(4)
potencias5<-potencias(5)
potencias6<-potencias(6)
potencias7<-potencias(7)
potencias8<-potencias(8)
potencias9<-potencias(9)
potencias10<-potencias(10)



modelo.1<-lm(formula = lidar.T$logratio ~ potencias1)
modelo.2<-lm(formula = lidar.T$logratio ~ potencias1+potencias2)
modelo.3<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3)
modelo.4<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4)
modelo.5<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5)
modelo.6<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5+potencias6)
modelo.7<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5+potencias6+potencias7)
modelo.8<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5+potencias6+potencias7+potencias8)
modelo.9<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5+potencias6+potencias7+potencias8+potencias9)
modelo.10<-lm(formula = lidar.T$logratio ~ potencias1+potencias2+potencias3+potencias4+potencias5+potencias6+potencias7+potencias8+potencias9+potencias10)


#necesito agregar esas variables al training set porque tienen que existir para la función predict() 
names(lidar.V)<-c("potencias1", "logratio")
lidar.V$potencias2<-lidar.V$potencias1^2
lidar.V$potencias3<-lidar.V$potencias1^3
lidar.V$potencias4<-lidar.V$potencias1^4
lidar.V$potencias5<-lidar.V$potencias1^5
lidar.V$potencias6<-lidar.V$potencias1^6
lidar.V$potencias7<-lidar.V$potencias1^7
lidar.V$potencias8<-lidar.V$potencias1^8
lidar.V$potencias9<-lidar.V$potencias1^9   
lidar.V$potencias10<-lidar.V$potencias1^10 


# errores para modelos lineales de 1 a 10
mean((lidar.V$logratio-predict(modelo.1, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.2, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.3, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.4, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.5, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.6, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.7, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.8, lidar.V))^2)
mean((lidar.V$logratio-predict(modelo.9, lidar.V))^2) #da un warning message porque hay un NA en el coef 9 (?)
mean((lidar.V$logratio-predict(modelo.10, lidar.V))^2) #da un warning message porque hay un NA en el coef (?)




##################################################


par(mfrow=c(2,5))


plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.1), lwd=2, col="black")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.2), lwd=2, col="red")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.3), lwd=2, col="cadetblue")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.4), lwd=2, col="yellow3")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.5), lwd=2, col="deeppink1")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.6), lwd=2, col="darkmagenta")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.7), lwd=2, col="coral3")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.8), lwd=2, col="orange")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.9), lwd=2, col="blue")

plot(range, logratio, lwd=1, col="darkgreen")
lines(lidar.T$range, predict(modelo.10), lwd=2, col="red")

par(mfrow=c(1,1))











