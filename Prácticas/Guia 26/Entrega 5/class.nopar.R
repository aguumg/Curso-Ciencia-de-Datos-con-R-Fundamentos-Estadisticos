# Y debe ser un vector de 1's y 2's.
class.nopar=function(x,X,Y,h_1,h_2){
  # Defimos las proporciones de 1's y 2's estimadas
  p1=length(Y==1)/length(Y)
  p2=length(Y==2)/length(Y)
  # Definimos las estimaciones de f_1 y f_0
  f_1_hat=function(x,X,h){
    df <- approxfun(density(X, kernel='gaussian',bw=h))
    df(x)
  }
  f_2_hat=function(x,X,h){
    df <- approxfun(density(X, kernel='gaussian',bw=h))
    df(x)
  }
  # Definimos el clasificador por la regla de Bayes
  ifelse(f_1_hat(x,X,h_1)*p1>f_2_hat(x,X,h_2)*p2,1,2)
}