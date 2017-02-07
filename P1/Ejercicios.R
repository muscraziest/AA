##########################################################################
# EJERCICIO 1
##########################################################################
# Apartado 1 --------------------------------------------------------------
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

# Apartado 2 --------------------------------------------------------------
simula_gauss <- function(N,dim,sigma){
  
  #Creamos una lista vac?a
  lista <- list()
  
  #En un bucle vamos generando los vectores y los guardamos en la lista vac?a
  #que creamos anteriormente
  for(i in 1:N){
    #Utilizamos la funci?n rnorm para generar los n?meros aleatorios
    v <- rnorm(dim,0,sigma)
    lista[[i]] <- v
  }
  
  #Devolvemos la lista
  lista
}

# Apartado 3 --------------------------------------------------------------
muestra1 <- simula_unif(50,2,c(-50,50))
plot(do.call(rbind,muestra1),main='Gr?fica de lista generada con simula_unif', xlab="Eje X",ylab="Eje Y")

readline("Pulse una tecla para continuar ")

# Apartado 4 --------------------------------------------------------------
muestra2 <- simula_gauss(50,2,c(5,7))
plot(do.call(rbind,muestra2),main='Gr?fica de lista generada con simula_gauss',xlab="Eje X",ylab="Eje Y",xlim=c(-50,50),ylim=c(-50,50))

# Apartado 5 --------------------------------------------------------------
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

# Apartado 6 --------------------------------------------------------------
rango <- c(-30,30)
variables <- simula_unif(2,1000,rango)
v <- simula_recta(rango)

#Clasificamos las variables 
clasificadas <- sign(variables[[2]]-v[[1]]*variables[[1]]-v[[2]])

#Dibujamos las variables calsificadas y la recta
#Primero dibujamos la recta
par(mfrow=c(1,1))
plot(seq(rango[[1]],rango[[2]]),seq(rango[[1]],rango[[2]])*v[[1]]+v[[2]],type='l',main='Funci?n apartado 6',ylim=rango,xlab="Eje X",ylab="Eje Y")
#Dibujamos las variables
points((variables[[1]])[clasificadas==1],(variables[[2]])[clasificadas==1],col='green')
points((variables[[1]])[clasificadas==-1],(variables[[2]])[clasificadas==-1],col='red')

# Apartado 7 --------------------------------------------------------------
#Definimos las funciones
f1 <- function(x,y){
  (x-10)^2 + (y-20)^2-400
}

f2 <- function(x,y){
  0.5*(x+10)**2 + (y-20)**2-400
}

f3 <- function(x,y){
  0.5*(x-10)**2 - (y+20)**2-400
}

f4 <- function(x,y){
  y - 20*x**2 - 5*x + 3
}

#Comprobamos el signo 
c1 <- sign(f1(unlist(variables[[1]]),unlist(variables[[2]])))
c2 <- sign(f2(unlist(variables[[1]]),unlist(variables[[2]])))
c3 <- sign(f3(unlist(variables[[1]]),unlist(variables[[2]])))
c4 <- sign(f4(unlist(variables[[1]]),unlist(variables[[2]])))

#Dibujamos las gr?ficas
par(mfrow=c(2,2))
plot(variables[[1]],variables[[2]],main='Funci?n 1',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[c1==1],(variables[[2]])[c1==1],col='green')
lines(rango[[1]]:rango[[2]],f1(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 2',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[c2==1],(variables[[2]])[c2==1],col='green')
lines(rango[[1]]:rango[[2]],f2(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 3',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[c3==1],(variables[[2]])[c3==1],col='green')
lines(rango[[1]]:rango[[2]],f3(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 4',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[c4==1],(variables[[2]])[c4==1],col='green')
lines(rango[[1]]:rango[[2]],f4(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

# Apartado 8 --------------------------------------------------------------
#Funci?n para calcular el 10% de las etiquetas positivas
porcentaje1 <- function(c){
  
  cont1 <- 0
  
  for(i in c){
    if(i==1){
      cont1 <- cont1+1
    }
  }
  
  round(cont1*0.1)
}

#Función para calcular el 10% de las etiquetas negativas
porcentaje_1 <- function(c){
  
  cont1 <- 0
  
  for(i in c){
    if(i==-1){
      cont1 <- cont1+1
    }
  }
  
  round(cont1*0.1)
}

#Funci?n para modificar las etiquetas
modifica_etiquetas <- function(c){
  
  p1 <- porcentaje1(c)
  p <- porcentaje_1(c)
  
  j <- 0
  i <- 0
  k <- 1
  
  while(i < p1 || j < p){
    
    if(c[[k]]==1){
      c[[k]] <- -1
      i <- i+1
    }
    else{
      c[[k]] <- 1
      j <- j+1
    }
    
    k <- k+1
  }
  
  c
}

#Funci?n del apartado 6

copia_clasificadas <- modifica_etiquetas(clasificadas)
par(mfrow=c(1,1))
plot(seq(rango[[1]],rango[[2]]),seq(rango[[1]],rango[[2]])*v[[1]]+v[[2]],type='l',main='Funci?n apartado 6',ylim=rango,xlab="Eje X",ylab="Eje Y")
points((variables[[1]])[copia_clasificadas==1],(variables[[2]])[copia_clasificadas==1],col='green')
points((variables[[1]])[copia_clasificadas==-1],(variables[[2]])[copia_clasificadas==-1],col='red')

#Funciones del apartado 7:

#Modificamos las etiquetas
copia_c1 <- modifica_etiquetas(c1)
copia_c2 <- modifica_etiquetas(c2)
copia_c3 <- modifica_etiquetas(c3)
copia_c4 <- modifica_etiquetas(c4)

#Dibujamos las gr?ficas
par(mfrow=c(2,2))
plot(variables[[1]],variables[[2]],main='Funci?n 1',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[copia_c1==1],(variables[[2]])[copia_c1==1],col='green')
lines(rango[[1]]:rango[[2]],f1(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 2',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[copia_c2==1],(variables[[2]])[copia_c2==1],col='green')
lines(rango[[1]]:rango[[2]],f2(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 3',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[copia_c3==1],(variables[[2]])[copia_c3==1],col='green')
lines(rango[[1]]:rango[[2]],f3(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

plot(variables[[1]],variables[[2]],main='Funci?n 4',xlab="Eje X",ylab="Eje Y",col='red')
points((variables[[1]])[copia_c4==1],(variables[[2]])[copia_c4==1],col='green')
lines(rango[[1]]:rango[[2]],f4(rango[[1]]:rango[[2]],rango[[1]]:rango[[2]]))

##########################################################################
# EJERCICIO 2
##########################################################################

#contour

# Apartado 1 --------------------------------------------------------------
ajusta_PLA <- function(datos,label,max_iter,vini){
  
  pesos <- matrix(vini,nrow=3,ncol=1)
  niter <- 0
  coef <- c(0,0)
  hay_error <- TRUE
  errores <- 0
 
  while(hay_error & niter <= max_iter){
    hay_error <- FALSE
    
    for(i in 1:nrow(datos)){
      
      punto <- c(datos[i,],1)
        
      #Actualizamos los pesos
      if(sign(sum(t(pesos)%*%punto)) != label[i]){
        
        pesos <- pesos + label[i]*punto
        hay_error <- TRUE
        errores <- errores+1
      }
    }
    
    niter <- niter+1
  }
  
  #Calculamos los coeficientes
  coef[1] = -pesos[1]/pesos[2]
  coef[2] = -pesos[3]/pesos[2]
  
  return(c(coef[1],coef[2],niter,errores))
}

# Apartado 2 --------------------------------------------------------------
#Ejecutamos con los datos del ejercicio 6
datos <- matrix(unlist(variables),nrow=length(variables[[1]]),ncol=2)
pesos <- c(0,0,0)
iteraciones <- c()
for(i in 1:10){
  a <- ajusta_PLA(datos,clasificadas,1000,pesos)
  iteraciones <- c(iteraciones,a[3])
  pesos <- sample(c(0,1),3,rep=TRUE)
}

mean(iteraciones)
pesos
a[3]

par(mfrow=c(1,1))
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[clasificadas==1],(variables[[2]])[clasificadas==1],col='green')
abline(a[2],a[1])

# Apartado 3 --------------------------------------------------------------
#Ejecutamos con 10 iteraciones
pesos <- c(0,0,0)
a <- ajusta_PLA(datos,copia_clasificadas,10,pesos)
par(mfrow=c(1,1))
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[copia_clasificadas==1],(variables[[2]])[copia_clasificadas==1],col='green')
abline(a[2],a[1])
errores1 <- c(a[4])

#Ejecutamos con 100 iteraciones
a <- ajusta_PLA(datos,copia_clasificadas,100,pesos)
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[copia_clasificadas==1],(variables[[2]])[copia_clasificadas==1],col='green')
abline(a[2],a[1])
errores1 <- c(errores1,a[4])

#Ejecutamos con 1000 iteraciones
a <- ajusta_PLA(datos,copia_clasificadas,1000,pesos)
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[copia_clasificadas==1],(variables[[2]])[copia_clasificadas==1],col='green')
abline(a[2],a[1])
errores1 <- c(errores1,a[4])

errores1

# Apartado 4 --------------------------------------------------------------
pesos <- c(0,0,0)
a <- ajusta_PLA(datos,c1,10,pesos)
par(mfrow=c(1,1))
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[c1==1],(variables[[2]])[c1==1],col='green')
abline(a[2],a[1])
errores2 <- c(a[4])

#Ejecutamos con 100 iteraciones
a <- ajusta_PLA(datos,c1,100,pesos)
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y", col='red')
points((variables[[1]])[c1==1],(variables[[2]])[c1==1],col='green')
abline(a[2],a[1])
errores2 <- c(errores2,a[4])

#Ejecutamos con 1000 iteraciones
a <- ajusta_PLA(datos,c1,1000,pesos)
plot(variables[[1]],variables[[2]],xlabel="Eje X",ylabel="Eje Y",col='red')
points((variables[[1]])[c1==1],(variables[[2]])[c1==1],col='green')
abline(a[2],a[1])
errores2 <- c(errores2,a[4])

errores2


# Apartado 5 --------------------------------------------------------------
ajusta_PLA2 <- function(datos,label,max_iter,vini){
  
  pesos <- matrix(vini,nrow=3,ncol=1)
  niter <- 0
  coef <- c(0,0)
  hay_error <- TRUE
  errores <- 0
  
  while(hay_error & niter <= max_iter){
    hay_error <- FALSE
    
    for(i in 1:nrow(datos)){
      
      punto <- c(datos[i,],1)
      
      #Actualizamos los pesos
      if(sign(sum(t(pesos)%*%punto)) != label[i]){
        
        pesos <- pesos + label[i]*punto
        hay_error <- TRUE
        errores <- errores+1
      }
      
    }
    
    #Calculamos los coeficientes
    if(pesos[2]==0){
      coef[1] = 0
      coef[2] = 0
    }
    else{
      coef[1] = -pesos[1]/pesos[2]
      coef[2] = -pesos[3]/pesos[2]
    }
    
    #Dibujamos el ajuste
    plot(datos,xlabel="Eje X",ylabel="Eje Y",col='red')
    points((datos[,1])[label==1],(datos[,2])[label==1], col='green')
    abline(coef[2],coef[1])
    niter <- niter+1
  }
  
  return(c(coef[1],coef[2],niter,errores))
}

#Ejecutamos el nuevo algoritmo
pesos <- c(0,0,0)
a <- ajusta_PLA2(datos,copia_clasificadas,10,pesos)
errores <- c(a[4])

#Ejecutamos con 100 iteraciones
a <- ajusta_PLA2(datos,copia_clasificadas,100,pesos)
errores <- c(errores,a[4])

#Ejecutamos con 1000 iteraciones
a <- ajusta_PLA2(datos,copia_clasificadas,1000,pesos)
errores <- c(errores,a[4])

errores

# Apartado 6 --------------------------------------------------------------
ajusta_PLA_MOD <- function(datos,label,max_iter,vini){}


##########################################################################
# EJERCICIO 3
##########################################################################

# Apartado 2 --------------------------------------------------------------
library(lattice)

#Leemos los datos
digits <- read.table("zip.train",header = FALSE)

#Dibujamos las im?genes
levelplot((digit.data[,1:9]) ~ rep(rep(1:16, 9), 16) + rep(rep(1:16, each = 16), 9) | gl(9, 256), col.regions = rev(gray.colors(256)),at = c(-1, 0, 1), labels = FALSE, ylim = c(17, 0))