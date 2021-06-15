lanzamientos.completar=function(){
  album=c(0,0,0,0,0,0)
  figus_compradas=0
  contador=0
  while(sum(album==0)!=0){
    figu_nueva=sample(1:6,1)
    album[figu_nueva]=album[figu_nueva]+1
    contador=contador+1
  }
  return(list('Intentos totales hasta completar: '=contador,'Salidas totales: '=album))
}
  