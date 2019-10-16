
#Lectura de los datos
data<-read.csv("caudal_extra.csv")
#Transformación a formato fecha
data$fecha<-as.Date(data$fecha)

################ Pregunta 2 ############################
# De los datos, a través de un summary(data), se puede destacar lo siguiente:
# - La variable "caudal" posee un amplio rango de valores que van desde 0 hasta 15805.00 los cuales
#no están distribuidos uniformemente en ese intervalo, sino más bien, el 50%
#de los datos son menores de un 8 aprox, y el 75% es menor a un valor 70 aprox. De esta forma se observa 
#que son pocas los individuos con valores más grandes a los anteriores. 
# - Similar al caso anterior, se encuentra la variable "precip_promedio" esta
#está entre 0 y 258.6, siendo el 75% de los valores iguales a 0.
# - La variable temp_max_promedio, a diferencia de las anteriores, posee observaciones
#con valores no tan disimiles entre si, sin embargo es la variable con mayor cantidad
#de observaciones nulas. 
#- Se destaca finalmente que, debido a como fue construida la base de datos, la única variable con mediciones 
#relevantes que no posee NA es caudal, y que tanto las variables de precipitación y temperatura poseen NA debido
# a la ausencia de dichas mediciones en el poligono considerado.  


################ Pregunta 3 ############################

#### a)
time_plot_una_estacion<-function(codigo_estacion,columna,fecha_min,fecha_max){
	
	subdata<-subset(data,codigo_estacion==codigo_estacion & fecha>=fecha_min & fecha<=fecha_max,select=c(columna,"fecha"))
	subdata<-subdata[order(subdata[,2]),]
	plot(subdata[,2],subdata[,1],xlab="Tiempo",ylab=columna,type='b')
}

# time_plot_una_estacion es una función que recibe los parámetros de entrada solicitados en el inciso de la
#pregunta 3. Un ejemplo de su uso se ejecuta a continuación

time_plot_una_estacion(1020003,"caudal","2000-01-02","2000-03-09")

#### b)

#Con el fin de normalizar una variable se crea la función normalizacion, la cual, la lleva al intervalo [0,1]

#Parámetros de entrada:
#variable: vector numerico de la variable a normalizar
normalizacion<-function(variable){
	minimo<-min(na.omit(variable))
	maximo<-max(na.omit(variable))
	Xchanged <- (variable - minimo)/(maximo-minimo)
	return(Xchanged)
}

#Se crea la función pedida, con los parámetros pedidos.
time_plot_estaciones_varias_columnas<-function(codigo_estacion,columnas, fecha_min, fecha_max){
	subdata<-subset(data,codigo_estacion==codigo_estacion & fecha>=fecha_min & fecha<=fecha_max,select=c(columnas,"fecha"))
	subdata<-subdata[order(subdata[,4]),]
	norm_temp<-normalizacion(subdata[,1])
	norm_pp<-normalizacion(subdata[,2])
	norm_caudal<-normalizacion(subdata[,3])
	
	plot(subdata[,4],norm_caudal,xlab="Fecha",ylab="Rango",col="blue",type="o",ylim=c(0,1.4))
	points(subdata[,4],norm_pp,xlab="Fecha",ylab="Rango",col="green",type="o")
	points(subdata[,4],norm_temp,xlab="Fecha",ylab="Rango",col="orange",type="o")
	legend(x="topright",c("Caudal", "Precipitación", "Temperatura"), fill = c("blue", "green", "orange"),title="Variables")
}

#Un ejemplo de como se utiliza la función anterior se detalla a continuación:
time_plot_estaciones_varias_columnas(1020003,c("temp_max_promedio","precip_promedio","caudal"),"2000-01-02", "2000-03-09")



################ Pregunta 4 ############################
#Antes de crear las variables pedida se crea la función estaciones.Su objetivo principal es determinar a que 
#estación del año pertenece cada fila de la vriable fecha, todo esto para después cálcular el percentil 95 
#de las variables T°,PP y caudal en cada estacion del año.

#Parametros de entrada:
#fecha: vector de fechas /formato "2019-10-15"

estaciones<-function(fecha){
	
	#Se determina el año min y max que existe en la BD
	minimo<-strsplit(as.character(min(fecha)),split="-")[[1]][1]
	maximo<-strsplit(as.character(max(fecha)),split="-")[[1]][1]
	#Se crea un vector con el rango de los años
	years<-seq(minimo,maximo)

	#Se inicializan los vectores que determinaran el inicio de cada estación por cada año existente
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
	
	#Se iniciliza un vector que contendrá la estación a la que pertenece cada fecha de la BD
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

#También se creó la función perc_estancion que retorna el percentil 95 de una variable en cada estación del año.
#Parámetros de entrada:
#estacion : vector con las estaciones a la cual pertenece una fecha. Un ejemplo sería el vector c("primavera","verano") si las fechas son 24-09-1995 y 02-01-2019)
#var      : variable de interés
perc_estancion<-function(estacion,var){
	
	quant_otono<-quantile(var[estacion=="otono"], prob = 0.95,na.rm=TRUE)	
	quant_invierno<-quantile(var[estacion=="invierno"], prob = 0.95,na.rm=TRUE)
	quant_primavera<-quantile(var[estacion=="primavera"], prob = 0.95,na.rm=TRUE)
	quant_verano<-quantile(var[estacion=="verano"], prob = 0.95,na.rm=TRUE)
	
	salida<-data.frame(otono=quant_otono,invierno=quant_invierno,primavera=quant_primavera,verano=quant_verano)
	return(salida)
	}

#Finalmente se crea la función "indicadora_extremo", la cual, devuelve un vector con  valores 1 y 0.
#Es 1 si la variable de interés (ya sea, caudal, T° o pp) registra un valor más alto que el percentil 95 en la estación del 
#año correspondiente.

#Párametros de entrada:

#variable: 	Vector con la variable de interés, ejemplo: caudal
#fecha: 	Vector con las fechas en las cuales se realizaron las mediciones de la variable

indicadora_extremo<-function(variable,fecha){
	#Creamos el vector que se llenará con los 1 o 0
	indicadora<-rep(0,1411180)

	#Llamamos a la función estaciones
	estacion<-estaciones(fecha)

	#Llamamos a la función perc_estacion
	percentiles_estaciones<-perc_estancion(estacion,variable)


	indicadora[(variable>percentiles_estaciones[1,1] & estacion=="otono")]<-1
	indicadora[(variable>percentiles_estaciones[1,2] & estacion=="invierno")]<-1
	indicadora[(variable>percentiles_estaciones[1,3] & estacion=="primavera")]<-1
	indicadora[(variable>percentiles_estaciones[1,4] & estacion=="verano")]<-1
return(indicadora)
}

################### respuesta 
#Así finalmente, para generar las variables pedidas solo basta llamar a la función indicadora_extremo
caudal_extremo<-indicadora_extremo(data$caudal,data$fecha)
temp_extremo<-indicadora_extremo(data$temp_max_promedio,data$fecha)
precip_extremo<-indicadora_extremo(data$precip_promedio,data$fecha)

#¿Les parece razonable esta medida para capturar algo “extremo”? ¿Usarían otra? ¿Cuál? ( Solamente
#descríbanla, no la codifiquen! Vamos a usar la definición de Spike para esta desafío)

#La medida considerada es razonable ya que claramente las observaciones que alcancen valores "extremos" se alejan bastante del resto; 
#La otra medida que consideraría sería tal vez considerar además la localización de las estaciones, ya que si las mediciones están 
#hechas a nivel país el clima varía mucho entre el norte o sur. Un valor de 23°C en primavera no es extremo en Santiago,
#mientras que en Chiloé tal vez si.

################ Pregunta 5 ############################

# Análizando la variable caudal_extremo

# Se crea un vector llamado medidas el cual contiene el porcentaje de eventos extremos en una determinada estación, en los 
#años de 1960 a 2018

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


## Como se puede apreciar del gráfico y del vector llamado medidas, en general existen varias estaciones que
#no alcanzan valores extremos en la variable caudal, a lo largo de los años 1960 y 2018, estas son un 52% aproximadamente del
#total de estaciones ,sin embargo, del 48% de restante, el porcentaje de registros de valores extremos de alcanzados en cada estación,
#en general varía bastante, estos van desde un 0.34% hasta un 80% aproximadamente. 
#En particular se destaca "Rio Baker Bajo Ã‘Adis" que posee un 84.31% aprox de valores extremos, 
#en el total de mediciones realizadas en esta estación. 


################ Pregunta 6 ############################

# Para realizar y contestar lo pedido se crea la función plot_extremos

plot_extremos<-function(variable,fecha){
	fechas_unicas<-unique(fecha)
	datos<-as.data.frame(variable)
	datos$fechas<-fecha
	X_cant<-c()
	Y_max<-c()
	for(i in 1:length(fechas_unicas)){

		#Datos asociados a la fecha i-ésima
		X<-subset(datos,fechas==fechas_unicas[i],select=c(variable))

		#Vector que contiene la cantidad de registros asociados a cada fecha única
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

# Gráficos asociados al porcentaje de eventos extremos en cada fecha distinta registrada.
#OBS: Se realizan los gráficos a las primeras 5000 observaciones debido al tiempo de ejecución 
#de la función; esto último debido al ciclo for en la función de 21252 iteraciones 

plot_extremos(caudal_extremo[1:5000],data$fecha[1:5000])
plot_extremos(temp_extremo[1:5000],data$fecha[1:5000])
plot_extremos(precip_extremo[1:5000],data$fecha[1:5000])

# A partir de los gráficos creados se puede observar que el comportamiento extremo, a medida que los años pasan efectivamente se 
#van haciendo más comunes.

################ Pregunta 7 ############################

# Modelo predicción caudal extremo
#El modelo a utilizar es una regresión logistica con dos variables predictoras: Caudal y precipitación 
#Se descartó el uso de la variable temperatura dado que 

#Se genera un dataset con las variables de interés

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
#El modelo generado, dado que se utilizaron todos los datos disponibles, se basa básicamente en predecir que
#tan probable es que el evento sea extremo si los valores alcanzados por caudal y precipitación alcanzan ciertas magnitudes.

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
#Además posee una sensibilidad de un 74% aprox. es decir que, cuando la clase es positiva, logra clasificar 74% aprox de esta.
#Así, en mi opinión, los resultados son bastante buenos, pues los valores mencionados nos indican que en general el 
#modelo logra con su próposito.

#b) Si se quisiera capturar alrededor del 70% de los eventos extremos el modelo planteado sería útil, pues, dados los valores de
#caudal y precipitación, este es capaz de decir si dichos valores corresponden a un evento extremo como una precisión aprox del 96%




