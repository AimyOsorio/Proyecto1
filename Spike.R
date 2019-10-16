
#Lectura de los datos
data<-read.csv("caudal_extra.csv")
#Transformaci�n a formato fecha
data$fecha<-as.Date(data$fecha)

################ Pregunta 2 ############################
# De los datos, a trav�s de un summary(data), se puede destacar lo siguiente:
# - La variable "caudal" posee un amplio rango de valores que van desde 0 hasta 15805.00 los cuales
#no est�n distribuidos uniformemente en ese intervalo, sino m�s bien, el 50%
#de los datos son menores un valor aprox de 8, y el 75% es menor a un valor 70 aprox. De esta forma se observa 
#que son pocas los individuos con valores m�s grandes a los anteriores. 
# - Similar al caso anterior, se encuentra la variable "precip_promedio" esta
est� entre 0 y 258.6, siendo el 75% de los valores iguales a 0.
# - La variable temp_max_promedio, a diferencia de las anteriores, posee observaciones
con valores no tan disimiles entre si, sin embargo es la variable con mayor cantidad
de observaciones nulas. 
#- Se destaca finalmente que, debido a como fue construida la base de datos, la �nica variable con mediciones 
#relevantes que no posee NA es caudal, y que tanto las variables de precipitaci�n y temperatura poseen NA debido
# a la ausencia de dichas mediciones en el poligono considerado.  


################ Pregunta 3 ############################

#### a)
time_plot_una_estacion<-function(codigo_estacion,columna,fecha_min,fecha_max){
	
	subdata<-subset(data,codigo_estacion==codigo_estacion & fecha>=fecha_min & fecha<=fecha_max,select=c(columna,"fecha"))
	subdata<-subdata[order(subdata[,2]),]
	plot(subdata[,2],subdata[,1],xlab="Tiempo",ylab=columna,type='b')
}

# time_plot_una_estacion es una funci�n que recibe los par�metros de entrada solicitados en el inciso de la
#pregunta 3. Un ejemplo de su uso se ejecuta a continuaci�n

time_plot_una_estacion(1020003,"caudal","2000-01-02","2000-03-09")

#### b)

#Con el fin de normalizar una variable se crea la funci�n normalizacion, la cual, la lleva al intervalo [0,1]

#Par�metros de entrada:
#variable: vector numerico de la variable a normalizar
normalizacion<-function(variable){
	minimo<-min(na.omit(variable))
	maximo<-max(na.omit(variable))
	Xchanged <- (variable - minimo)/(maximo-minimo)
	return(Xchanged)
}

#Se crea la funci�n pedida, con los par�metros pedidos.
time_plot_estaciones_varias_columnas<-function(codigo_estacion,columnas, fecha_min, fecha_max){
	subdata<-subset(data,codigo_estacion==codigo_estacion & fecha>=fecha_min & fecha<=fecha_max,select=c(columnas,"fecha"))
	subdata<-subdata[order(subdata[,4]),]
	norm_temp<-normalizacion(subdata[,1])
	norm_pp<-normalizacion(subdata[,2])
	norm_caudal<-normalizacion(subdata[,3])
	
	plot(subdata[,4],norm_caudal,xlab="Fecha",ylab="Rango",col="blue",type="o",ylim=c(0,1.4))
	points(subdata[,4],norm_pp,xlab="Fecha",ylab="Rango",col="green",type="o")
	points(subdata[,4],norm_temp,xlab="Fecha",ylab="Rango",col="orange",type="o")
	legend(x="topright",c("Caudal", "Precipitaci�n", "Temperatura"), fill = c("blue", "green", "orange"),title="Variables")
}

#Un ejemplo de como se utiliza la funci�n anterior se detalla a continuaci�n:
time_plot_estaciones_varias_columnas(1020003,c("temp_max_promedio","precip_promedio","caudal"),"2000-01-02", "2000-03-09")



################ Pregunta 4 ############################
#Antes de crear las variables pedida se crea la funci�n estaciones.Su objetivo principal es determinar a que 
#estaci�n del a�o pertenece cada fila de la vriable fecha, todo esto para despu�s c�lcular el percentil 95 
#de las variables T�,PP y caudal en cada estacion del a�o.

#Parametros de entrada:
#fecha: vector de fechas /formato "2019-10-15"

estaciones<-function(fecha){
	
	#Se determina el a�o min y max que existe en la BD
	minimo<-strsplit(as.character(min(fecha)),split="-")[[1]][1]
	maximo<-strsplit(as.character(max(fecha)),split="-")[[1]][1]
	#Se crea un vector con el rango de los a�os
	years<-seq(minimo,maximo)

	#Se inicializan los vectores que determinaran el inicio de cada estaci�n por cada a�o existente
	inicio_primavera<-c()
	inicio_verano<-c()
	inicio_otono<-c()
	inicio_invierno<-c()
	
	#Se llenan los vectores anteriores
	for(i in years){
		inicio_primavera<-c(inicio_primavera,paste(i,09,23,sep="-"))
		inicio_verano<-c(inicio_verano,paste(i,12,21,sep="-"))
		inicio_otono<-c(inicio_otono,paste(i,03,20,sep="-"))
		inicio_invierno<-c(inicio_invierno,paste(i,06,21,sep="-"))
	}

	#Se transforman a formato "date"
	inicio_primavera<-as.Date(inicio_primavera)
	inicio_verano<-as.Date(inicio_verano)
	inicio_otono<-as.Date(inicio_otono)
	inicio_invierno<-as.Date(inicio_invierno)
	
	#Se iniciliza un vector que contendr� la estaci�n a la que pertenece cada fecha de la BD
	estacion<-rep(0,length(fecha))
	n<-length(years)-1
	
	#Se llena el vector anterior
	for(k in 1:n){
		estacion[fecha>=inicio_otono[k] & fecha<inicio_invierno[k]]<-"otono"
		estacion[fecha>=inicio_invierno[k] & fecha<inicio_primavera[k]]<-"invierno"
		estacion[fecha>=inicio_primavera[k] & fecha<inicio_verano[k]]<-"primavera"
		estacion[fecha>=inicio_verano[k] & fecha<inicio_invierno[k+1]]<-"verano"
	
	}
	estacion[fecha<inicio_otono[1]]<-"verano"
	
	#se retorna el valor de la funcion
	return(estacion)
}

#Un ejemplo del uso de la funcion estaciones
estaciones(data$fecha)

#Tambi�n se cre� la funci�n perc_estancion que retorna el percentil 95 de una variable en cada estaci�n del a�o.
#Par�metros de entrada:
#estacion : vector con las estaciones a la cual pertenece una fecha. Un ejemplo ser�a el vector c("primavera","verano") si las fechas son 24-09-1995 y 02-01-2019)
#var      : variable de inter�s
perc_estancion<-function(estacion,var){
	
	quant_otono<-quantile(var[estacion=="otono"], prob = 0.95,na.rm=TRUE)	
	quant_invierno<-quantile(var[estacion=="invierno"], prob = 0.95,na.rm=TRUE)
	quant_primavera<-quantile(var[estacion=="primavera"], prob = 0.95,na.rm=TRUE)
	quant_verano<-quantile(var[estacion=="verano"], prob = 0.95,na.rm=TRUE)
	
	salida<-data.frame(otono=quant_otono,invierno=quant_invierno,primavera=quant_primavera,verano=quant_verano)
	return(salida)
	}

#Finalmente se crea la funci�n "indicadora_extremo", la cual, devuelve un vector con  valores 1 y 0. Es 1 si la variable de inter�s (ya sea,
#caudal, T� o pp) registra un valor m�s alto que el percentil 95 en la estaci�n del a�o correspondiente.

#P�rametros de entrada:

#variable: 	Vector con la variable de inter�s, ejemplo: caudal
#fecha: 	Vector con las fechas en las cuales se realizaron las mediciones de la variable

indicadora_extremo<-function(variable,fecha){
	#Creamos el vector que se llenar� con los 1 o 0
	indicadora<-rep(0,1411180)

	#Llamamos a la funci�n estaciones
	estacion<-estaciones(fecha)

	#Llamamos a la funci�n perc_estacion
	percentiles_estaciones<-perc_estancion(estacion,variable)


	indicadora[(variable>percentiles_estaciones[1,1] & estacion=="otono")]<-1
	indicadora[(variable>percentiles_estaciones[1,2] & estacion=="invierno")]<-1
	indicadora[(variable>percentiles_estaciones[1,3] & estacion=="primavera")]<-1
	indicadora[(variable>percentiles_estaciones[1,4] & estacion=="verano")]<-1
return(indicadora)
}

################### respuesta 
#As� finalmente, para generar las variables pedidas solo basta llamar a la funci�n indicadora_extremo
caudal_extremo<-indicadora_extremo(data$caudal,data$fecha)
temp_extremo<-indicadora_extremo(data$temp_max_promedio,data$fecha)
precip_extremo<-indicadora_extremo(data$precip_promedio,data$fecha)

#�Les parece razonable esta medida para capturar algo �extremo�? �Usar�an otra? �Cu�l? ( Solamente
#descr�banla, no la codifiquen! Vamos a usar la definici�n de Spike para esta desaf�o)

#La medida considerada es razonable ya que claramente las observaciones que alcancen valores "extremos" se alejan bastante del resto; La
#otra medida que considerar�a ser�a tal vez considerar adem�s la localizaci�n de las estaciones, ya que si las mediciones est�n hechas a nivel 
#pa�s el clima var�a mucho entre el norte o sur. Un valor de 23�C en primavera no es extremo en Santiago, mientras que en Chilo� tal vez si.

################ Pregunta 5 ############################

# An�lizando la variable caudal_extremo

# Se crea un vector llamado medidas el cual contiene el porcentaje de eventos extremos en una determinada estaci�n, en los 
#a�os de 1960 a 2018

extremo<-as.data.frame(caudal_extremo)
extremo$Nombre<-data$nombre
class(extremo$Nombre)
nombres<-unique(data$nombre)

medidas<-c()
for(i in 1:length(nombres)){
obs<-subset(extremo,Nombre==nombres[i] & caudal_extremo==1,select=c(caudal_extremo))
registro_total<-subset(extremo,Nombre==nombres[i],select=c(Nombre))
medidas<-c(medidas,(sum(as.matrix(obs))/dim(registro_total)[1])*100)
}

# Con lo anterior se hace un grafico de fechas vs porcentaje
plot(medidas,nombres,col=nombres,main="Porcentaje de extremos alcanzados por las distintas estaciones en el rango 1960-2018")


## Como se puede apreciar del gr�fico y del vector llamado medidas, en general existen varias estaciones que
#no alcanzan valores extremos en la variable caudal, a lo largo de los a�os 1960 y 2018, estas son un 52% aproximadamente del total de estaciones
,sin embargo, del 48% de restante, el porcentaje de registros de valores extremos de alcanzados en cada estaci�n,en general var�a bastante, estos van 
# desde un 0.34% hasta un 80% aproximadamente. 
En particular se destaca "Rio Baker Bajo ÑAdis" que posee un 84.31% aprox de valores extremos, en el total de mediciones realizadas en esta estaci�n. 


################ Pregunta 6 ############################

# Para realizar y contestar lo pedido se crea la funci�n plot_extremos

plot_extremos<-function(variable,fecha){
	fechas_unicas<-unique(fecha)
	datos<-as.data.frame(variable)
	datos$fechas<-fecha
	X_cant<-c()
	Y_max<-c()
	for(i in 1:length(fechas_unicas)){

		#Datos asociados a la fecha i-�sima
		X<-subset(datos,fechas==fechas_unicas[i],select=c(variable))

		#Vector que contiene la cantidad de registros asociados a cada fecha �nica
		X_cant<-c(X_cant,dim(X)[1])

		#Vector que contiene la cantidad de  registros maximos asociados a cada fecha
		Y_max<-c(Y_max,sum(as.numeric(X!=0)))
	}

	#Porcentaje
	porcentaje<-(Y_max/X_cant)*100
	dt_plot<-as.data.frame(cbind(porcentaje))
	dt_plot$fechas<-fechas_unicas

	dt_plot<-dt_plot[order(dt_plot$fechas),]

	plot(dt_plot[,2],dt_plot[,1],xlab="Fechas",ylab="Porcentaje de extremos",type='p')
}

# Gr�ficos asociados al porcentaje de eventos extremos en cada fecha distinta registrada.
#OBS: Se realizan los gr�ficos a las primeras 5000 observaciones debido al tiempo de ejecuci�n 
#de la funci�n; esto �ltimo debido al ciclo for en la funci�n de 21252 iteraciones 

plot_extremos(caudal_extremo[1:5000],data$fecha[1:5000])
plot_extremos(temp_extremo[1:5000],data$fecha[1:5000])
plot_extremos(precip_extremo[1:5000],data$fecha[1:5000])

# A partir de los gr�ficos creados se puede observar que el comportamiento extremo, a medida que los a�os pasan efectivamente se 
#van haciendo m�s comunes.

################ Pregunta 7 ############################

# Modelo predicci�n caudal extremo
#El modelo a utilizar es una regresi�n logistica con dos variables predictoras: Caudal y precipitaci�n 
#Se descart� el uso de la variable temperatura dado que 

#Se genera un dataset con las variables de inter�s

datos_modelo<-as.data.frame(cbind(caudal_extremo,normalizacion(data$caudal),normalizacion(data$precip_promedio)))
names(datos_modelo)<-c("caudal_extremo","caudal","pp")

#Por simpleza se omitiran los valores nulos
datos_modelo<-na.omit(datos_modelo)

#Se divide en entrenamiento y testeo

n_train<-floor((dim(datos_modelo)[1]*0.8)/100)
index<-sample(n_train)
train<-datos_modelo[index,]
test<-datos_modelo[-index,]

modelo<-glm(caudal_extremo~caudal+pp,data=train) 

#Uso del modelo planteado
#El modelo generado, dado que se utilizaron todos los datos disponibles, se basa b�sicamente en predecir que
#tan probable es que el evento sea extremo si los valores alcanzados por caudal y precipitaci�n alcanzan ciertas magnitudes.

################ Pregunta 8 ############################

predicted_value <- predict(modelo,test,type = "response")
predicted_class <- ifelse(predicted_value>0.5, 1,0)
performance_data<-data.frame(observed=test$caudal_extremo,
           predicted= predicted_class)
positive <- sum(performance_data$observed==1)
negative <- sum(performance_data$observed==0)
predicted_positive <- sum(performance_data$predicted==1)
predicted_negative <- sum(performance_data$predicted==0)
total <- nrow(performance_data)
data.frame(positive, negative,predicted_positive,predicted_negative)


tp<-sum(performance_data$observed==1 & performance_data$predicted==1)
tn<-sum(performance_data$observed==0 & performance_data$predicted==0)
fp<-sum(performance_data$observed==0 & performance_data$predicted==1)
fn<-sum(performance_data$observed==1 & performance_data$predicted==0)
data.frame(tp,tn,fp,fn)

accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/positive
especificity <- tn/negative
precision <- tp/predicted_positive
npv <- tn / predicted_negative
data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)

# a) El modelo planteado tiene un accuracy de 98% aproximadamente, es decir, clasifica correctamente un 98% de los datos.
#Adem�s posee una sensibilidad de un 74% aprox. es decir que, cuando la clase es positiva, logra clasificar 74% aprox de esta.
#As�, en mi opini�n, los resultados son bastante buenos, pues los valores mencionados nos indican que en general el 
#modelo logra con su pr�posito.



