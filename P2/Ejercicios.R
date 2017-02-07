pausa = function(){
  cat("Pulsa cualquier caracter")
  ch= scan()
}
###########################################################################
#           PARTE 1
###########################################################################
# EJERCICIO 1 -------------------------------------------------------------
# Apartado a --------------------------------------------------------------
# Definimos la función para calcular el gradiente descendente
gradiente_descendente <-function(w,f,df,mu,it_max){
  
  num_it <- 0
  aux <- c(0,0)
  primera_vez <- TRUE
  x <- seq(from=-5,to=5,by=0.25)
  y <- seq(from=-5,to=5,by=0.25)
  m <- matrix(w,nrow=1)
  
  #Dibujamos la funci?n y el punto inicial
  contour(x,y,outer(x,y,Vectorize(function(x,y){f(x,y)})),xlab="Eje X",ylab="Eje Y",col='black')
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='blue')
  
  for (i in 1:it_max){
    
    # Actualizamos w seg?n el valor de la derivada
    w <- w - df(w[1],w[2])*mu
    m <- rbind(m,w)
    
    if (f(w[1],w[2]) < 10**(-14) & primera_vez) {
      num_it <- i
      aux <- w
      primera_vez <- FALSE
    }
    
    # Dibujamos el nuevo punto    
    w1 <- df(w[1],w[2])
    points(w[1],w[2],col='red')
    
  }
  
  # Dibujamos el punto final
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='green')
  
  # Dibujamos el camino
  lines(m[,1],m[,2],col='red')
  
  return(list(w,num_it,aux))
}

# Definimos la funci?n E(u,v)
E <- function(x,y){
  
  return((x*exp(y) - 2*y*exp(-x))**2)
}

# Definimos la derivada de la funci?n E(u,v)
dE <- function(x,y){
  
  du <- 2*exp(-2*x)*(x*exp(x+y)-2*y)*(exp(x+y)+2*y)
  dv <- 2*exp(-2*x)*(x*exp(x+y)-2)*(x*exp(x+y)-2*y)
  
  return(c(du,dv))
}

# Calculamos los resultados
w <- c(1,1)

resultado <- gradiente_descendente(w,E,dE,0.1,1000)

print("El vector de pesos es: ")
print(resultado[2])
pausa()

print("El numero de iteraciones es: ")
print(resultado[3])
pausa()
# Apartado b --------------------------------------------------------------
# Definimos la funci?n f(x,y)
f <- function(x,y){
  
  return (x**2 + 2*y**2 + 2*sin(2*pi*x)*sin(2*pi*y))
}

# Definimos la funci?n de la derivada de f(x,y)
df <- function(x,y){
  
  dx <- 2*(2*pi*cos(2*pi*x)*sin(2*pi*y) + x)
  dy <- 4*(pi*sin(2*pi*x)*cos(2*pi*y) + y)
  
  return (c(dx,dy))
}

# Calculamos los resultados
w <- c(1,1)

resultado <- gradiente_descendente(w,f,df,0.01,50)
pausa()

resultado <- gradiente_descendente(w,f,df,0.1,50)
pausa()

# Funcion para calcular los valores minimos
gradiente_descendente_minimos <-function(w,f,df,mu,it_max){
  
  aux <- c(0,0)
  w_min <- w
  
  for (i in 1:it_max){
    
    w_ant <- w
    
    # Actualizamos w según el valor de la derivada
    w <- w - df(w[1],w[2])*mu
    
    # Comprobamos si el valor actual de la funci?n es m?s peque?o
    if (f(w[1],w[2]) < f(w_ant[1],w_ant[2])) {
      w_min <- w
    }
    
  }
  
  return(c(f(w_min[1],w_min[2]),w_min))
}

valores_minimos <- c()
coordenadas_minimas <- c()

# Calculamos los valores m?nimos para cada punto de inicio
w <- c(0.1,0.1)
resultado <- gradiente_descendente_minimos(w,f,df,0.1,50)
valores_minimos <- rbind(valores_minimos,resultado[1])
c_min <- cbind(resultado[2],resultado[3])
coordenadas_minimas <- rbind(coordenadas_minimas,c_min)
pausa()

w <- c(1,1)
resultado <- gradiente_descendente_minimos(w,f,df,0.1,50)
valores_minimos <- rbind(valores_minimos,resultado[1])
c_min <- cbind(resultado[2],resultado[3])
coordenadas_minimas <- rbind(coordenadas_minimas,c_min)
pausa()

w <- c(-0.5,-0.5)
resultado <- gradiente_descendente_minimos(w,f,df,0.1,50)
valores_minimos <- rbind(valores_minimos,resultado[1])
c_min <- cbind(resultado[2],resultado[3])
coordenadas_minimas <- rbind(coordenadas_minimas,c_min)
pausa()

w <- c(-1,-1)
resultado <- gradiente_descendente_minimos(w,f,df,0.1,50)
valores_minimos <- rbind(valores_minimos,resultado[1])
c_min <- cbind(resultado[2],resultado[3])
coordenadas_minimas <- rbind(coordenadas_minimas,c_min)
pausa()

tabla <- cbind(valores_minimos,coordenadas_minimas)
colnames(tabla) <- c("f(x,y)","x","y")
rownames(tabla) <- c("[0.1,0.1]","[1,1]","[-0.5,-0.5]","[-1,-1]")

# Mostramos la tabla
print("Los valores minimos son: ")
print(tabla)
pausa()

# EJERCICIO 2 -------------------------------------------------------------
# Apartado a --------------------------------------------------------------
coordenada_descendente <-function(w,f,df,mu,it_max){
  
  x <- seq(from=-5,to=5,by=0.25)
  y <- seq(from=-5,to=5,by=0.25)
  m <- matrix(w,nrow=1)
  
  #Dibujamos la funci?n y el punto inicial
  contour(x,y,outer(x,y,Vectorize(function(x,y){f(x,y)})),xlab="Eje X",ylab="Eje Y",col='black')
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='blue')
  
  for (i in 1:it_max){
    
    #Calculamos las derivadas parciales
    du <- (df(w[1],w[2]))[1]
    dv <- (df(w[1],w[2]))[2]
    
    # Actualizamos seg?n el valor de la derivada
    if(i%%2 == 0)
      w[1] <- w[1] - du*mu
    else
      w[2] <- w[2] - dv*mu
    
    m <- rbind(m,w)
    
    # Dibujamos el nuevo punto    
    w1 <- df(w[1],w[2])
    points(w[1],w[2],col='red')
    
  }
  
  # Dibujamos el punto final
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='green')  
  
  # Dibujamos el camino
  lines(m[,1],m[,2],col='red')
  
  return(list(f(w[1],w[2]),w))
}

w <- c(1,1)

resultado <- coordenada_descendente(w,E,dE,0.1,30)

resultado

pausa()
# Apartado b --------------------------------------------------------------
resultado_coor <- coordenada_descendente(w,E,dE,0.1,30)

resultado_grad <- gradiente_descendente(w,E,dE,0.1,15)

a <- resultado_coor[1]
a <- unlist(a)

b <- resultado_grad[1]
b <- unlist(b)
b <- E(b[1],b[2])

tabla <- cbind(a,b)
colnames(tabla) = c("coordenada desc","gradiente desc")

tabla
pausa()
# EJERCICIO 3 -------------------------------------------------------------
ddf <- function(x,y){
  
  dxx <- 2-8*pi**2*sin(2*pi*x)*sin(2*pi*y)
  dxy <- 8*pi**2*cos(2*pi*x)*cos(2*pi*y)
  dyx <- 8*pi**2*cos(2*pi*x)*cos(2*pi*y)
  dyy <- 4-8*pi**2*sin(2*pi*x)*sin(2*pi*y)
  
  return (c(dxx,dxy,dyx,dyy))
  
}

newton <-function(w,f,df,ddf,mu,it_max){
  
  x <- seq(from=-5,to=5,by=0.25)
  y <- seq(from=-5,to=5,by=0.25)
  m <- matrix(w,nrow=1)
  
  #Dibujamos la funci?n y el punto inicial
  contour(x,y,outer(x,y,Vectorize(function(x,y){f(x,y)})),xlab="Eje X",ylab="Eje Y",col='black')
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='blue')
  
  for (i in 1:it_max){
    
    # Calculamos la funci?n Hessiana
    H <- matrix(ddf(w[1],w[2]),nrow=2)
    
    # Actualizamos w 
    w <- solve(H)*(w-df(w[1],w[2])*mu)
    m <- rbind(m,w)
    
    # Dibujamos el nuevo punto    
    w1 <- df(w[1],w[2])
    points(w[1],w[2],col='red')
    
  }
  
  # Dibujamos el punto final
  w1 <- df(w[1],w[2])
  points(w[1],w[2],pch=21,bg='green')
  
  # Dibujamos el camino
  lines(m[,1],m[,2],col='red')
  
  return(c(f(w[1],w[2]),w))
}

# NEWTON
print("Resultados con Newton: ")
par(mfrow=c(2,2))
w <- c(0.1,0.1)
resultado <- newton(w,f,df,ddf,0.1,50)

w <- c(1,1)
resultado <- newton(w,f,df,ddf,0.1,50)

w <- c(-0.5,-0.5)
resultado <- newton(w,f,df,ddf,0.1,50)

w <- c(-1,-1)
resultado <- newton(w,f,df,ddf,0.1,50)

pausa()

# GRADIENTE DESCENDENTE
print("Resultados con gradiente descendente: ")
par(mfrow=c(2,2))
w <- c(0.1,0.1)
resultado <- gradiente_descendente(w,f,df,0.1,50)

w <- c(1,1)
resultado <- gradiente_descendente(w,f,df,0.1,50)

w <- c(-0.5,-0.5)
resultado <- gradiente_descendente(w,f,df,0.1,50)

w <- c(-1,-1)
resultado <- gradiente_descendente(w,f,df,0.1,50)
pausa()

# EJERCICIO 4 -------------------------------------------------------------
# Apartado a --------------------------------------------------------------
regresion_logistica <-function(w,x,y,N,mu){
  
  w_ant = c(1,1,1)
  x <- cbind(x,1)
  
  while(sqrt(sum((w_ant-w)**2)) >= 0.001){
    
    indices <- sample(1:length(y),N)
    x_muestra <- x[indices,]
    y_muestra <- y[indices]
    w_ant <- w
    
    # Calculamos el valor del gradiente
    gradient <- (-y_muestra*x_muestra)/c(1+exp(y_muestra*x_muestra%*%w))
  
    # Actualizamos w seg?n el valor de la derivada
    w <- w-mu*1/N*colSums(gradient)
  }
  
  return(w)
}

simula_unif <- function(N,dim,rango){
  
  #Creamos una lista vac?a
  lista <- list()
  
  #En un bucle vamos generando los vectores y los guardamos en la lista vac?a
  #que creamos anteriormente
  for(i in 1:N){
    v <- sample(rango[[1]]:rango[[2]],dim,rep=TRUE)
    lista[[i]] <- v
  }
  
  #Devolvemos la lista
  lista
}

simula_recta <- function(intervalo){
  
  #Generamos los puntos aleatorios dentro del intervalo
  punto1 <- sample(intervalo[[1]]:intervalo[[2]],2,rep=TRUE)
  punto2 <- sample(intervalo[[1]]:intervalo[[2]],2,rep=TRUE)
  
  #Calculamos los valores de a y b
  a <- (punto2[[2]]-punto1[[2]])/(punto2[[1]]-punto1[[1]])
  b <- punto1[[2]] - a*punto1[[1]]
  
  #Mostramos el resultado
  v <- c(a,b)
  v
}

#Generamos las variables
N <- 100
rango <- c(-30,30)
muestra <- simula_unif(2,N,rango)
v <- simula_recta(rango)

#Clasificamos las variables 
y <- sign(muestra[[2]]-v[[1]]*muestra[[1]]-v[[2]])
x <- matrix(unlist(muestra),nrow = N, ncol = 2)
w <- matrix(c(0,0,0),ncol=1)

r<-regresion_logistica(w,x,y,50,0.01)
par(mfrow=c(1,1))
plot(muestra[[1]],muestra[[2]],xlabel="Eje X",ylabel="Eje Y",col=y+2)
abline(-r[3]/r[2],-r[1]/r[2],col='red')
pausa()
# Apartado b --------------------------------------------------------------
N <- 1000
rango <- c(-30,30)
muestra <- simula_unif(2,N,rango)
v <- simula_recta(rango)

#Clasificamos las variables 
y <- sign(muestra[[2]]-v[[1]]*muestra[[1]]-v[[2]])
x <- matrix(unlist(muestra),nrow = N, ncol = 2)
w <- matrix(c(0,0,0),ncol=1)

r<-regresion_logistica(w,x,y,100,0.01)

y_regresion <- sign(muestra[[2]]-(-r[1]/r[2])*muestra[[1]]-(-r[3]/r[2]))

errores <- sum(y!=y_regresion)

print("Coeficiente a: ")
print(-r[1]/r[2])
print("Coeficiente b: ")
print(-r[3]/r[2])
print("El porcentaje de error es: ")
print(errores/10)

par(mfrow=c(1,1))
plot(muestra[[1]],muestra[[2]],xlab="Eje X",ylab="Eje Y",col=y+2)
abline(-r[3]/r[2],-r[1]/r[2],col='red')
pausa()

# EJERCICIO 5 -------------------------------------------------------------

# Leemos los datos de entrenamiento
digit.train <- read.table("./zip.train",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos = digitos15.train[,1]
ndigitos = nrow(digitos15.train)
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos,16,16))

simetria1 <- function(A){
  #  A = matrix(v,nrow=16,ncol=16,byrow=T)
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

intensidad = apply(grises[1:ndigitos,,],1, mean)
simetria = apply(grises[1:ndigitos,,],1,simetria1)
datos1 = as.matrix(cbind(intensidad,simetria))
# Leemos los datos de test
digit.test <- read.table("./zip2.test",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]
digitos2 = digitos15.test[,1]
ndigitos2 = nrow(digitos15.test)
grises2 = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos2,16,16))

intensidad2 = apply(grises2[1:ndigitos2,,],1, mean)
simetria2 = apply(grises2[1:ndigitos2,,],1,simetria1)
datos2 = as.matrix(cbind(intensidad2,simetria2))
# Apartado a --------------------------------------------------------------
# Hacemos regresi?n lineal
# Funcion regresion lineal
regresion_lineal <- function(datos, signos){
  
  datos <- datos[,ncol(datos):1]
  datos <- cbind(1,datos)
  
  # Calculamos la pesudoinversa
  datos.svd <- svd(t(datos)%*%datos)
  U <- datos.svd$u
  V <- datos.svd$v
  D_inversa <- diag(1/datos.svd$d)
  x_pseudo_inver <- (V%*%D_inversa%*%t(U))%*%t(datos)
  
  # Calculamos los pesos
  w <- x_pseudo_inver%*%digitos
  
  return(w)
}

# Funci?n PLA pocket
PLA_pocket <- function(datos,label,max_iter,vini){
  
  pesos_pla <- matrix(vini,nrow=3,ncol=1)
  pesos_pocket <- pesos_pla
  niter <- 1
  errores <- 0
  errores_ant <- length(datos)+1
  
  while(niter <= max_iter){
    errores <- 0
    
    # Ejecutamos PLA para obtener los pesos nuevos
    for(i in 1:nrow(datos)){
      
      punto <- c(datos[i,],1)
      error <- sum(label[i]-t(pesos_pocket)%*%punto)
    
      #Actualizamos los pesos
      if(sign(sum(t(pesos_pocket)%*%punto)) != label[i]){
        umbral_ant <- pesos_pocket[length(pesos_pocket)]
        pesos_pla <- pesos_pocket + label[i]*punto
        pesos_pla[length(pesos_pla)] <- umbral_ant + error 
        errores <- errores+1
      }
    }
    
    # Comprobamos si los pesos nuevos son mejores que los anteriores
    if(errores < errores_ant){
      pesos_pocket <- pesos_pla
      errores_ant <- errores
    }
  
    niter <- niter+1
  }
  
  #Calculamos los coeficientes
  return(pesos_pocket)
}

regresion <- regresion_lineal(datos1, digitos)
digitos[digitos==5] <- -1
pocket <- PLA_pocket(datos1, digitos, 4, regresion)
plot(datos1,xlab="Intensidad Promedio",ylab="Simetria",col=digitos+5)
abline(-pocket[3]/pocket[2],-pocket[1]/pocket[2])

plot(datos2,xlab="Intensidad Promedio",ylab="Simetria",col=digitos2+5)
abline(-pocket[3]/pocket[2],-pocket[1]/pocket[2])
pausa()
# Apartado b --------------------------------------------------------------
e_in <- (sum(digitos!=sign(pocket[1]*datos1[,1]+pocket[2]*datos1[,2]+pocket[3])))/nrow(datos1)*100
digitos2[digitos2==5] <- -1
e_test <- (sum(digitos2!=sign(pocket[1]*datos2[,1]+pocket[2]*datos2[,2]+pocket[3])))/nrow(datos2)*100

print("El error en la muestra es: ")
print(e_in)
print("El error en el test es: ")
print(e_test)
pausa()
# Apartado c --------------------------------------------------------------
N = nrow(datos1)
dvc = ncol(datos1)+1
cota_VC_in <- e_in/100 + sqrt(dvc*(log(N))/N)
cota_VC_test <- e_test/100 + sqrt(dvc*(log(N))/N)
print("Las cotas son: ")
print("Cota basada en Ein: ")
print(cota_VC_in)
print("Cota basada en Etes: ")
print(cota_VC_test)
pausa()
