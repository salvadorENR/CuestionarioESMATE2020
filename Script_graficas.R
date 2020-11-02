#Código para la sistematización de la información

datos <- read.csv(file="Cuestionario2020.csv",header=TRUE,sep=";")
datos
datos$var3<-enc2utf8(datos$var3)
options(encoding="utf-8")
library(ggplot2)
#**************************************** Gráfica de barras *****************************************

#++++++++++++++++++++++++++++++++++++++++++VAR1++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++edades = datos$var1
table(edades)
cortes= c(21,31,41,51,61,71)
rangos=cut(edades,cortes,right = FALSE)
rangos.freq=table(rangos)

d1=data.frame(rangos.freq)
d2=data.frame(x=d1$rangos,y=d1$Freq)
d2$x=factor(d2$x,levels=c("[21,31)","[31,41)","[41,51)","[51,61)","[61,71)"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))
+ theme_minimal()

#ggplot(d2, aes(x,y)) +
 # geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))
#++++++++++++++++++++++++++++++++++++++++++VAR2++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
edades = datos$var2
table(edades)
cortes= c(21,31,41,51,61,71)
rangos=cut(edades,cortes,right = FALSE)
rangos.freq=table(rangos)

d1=data.frame(rangos.freq)
d2=data.frame(x=d1$rangos,y=d1$Freq)
d2$x=factor(d2$x,levels=c("[21,31)","[31,41)","[41,51)","[51,61)","[61,71)"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))
+ theme_minimal()

#+++++++++++++++++++++++++++++++++++++++++++VAR3+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

d1=data.frame(table(datos$var3))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x<-enc2utf8(c("Ahuachapán","Cabañas","Chalatenango","Cuscatlán","La Libertad","La Paz","La Unión","Morazán","San Miguel","San Salvador",
        "San Vicente","Santa Ana","Sonsonate","Usulután"))
d2$x=factor(d2$x,levels=c("Ahuachapán","Cabañas","Chalatenango","Cuscatlán","La Libertad","La Paz","La Unión","Morazán","San Miguel","San Salvador",
                            "San Vicente","Santa Ana","Sonsonate","Usulután"))

ggplot(d2, aes(x,y)) +
geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Departamento") +ggtitle("Participación de los profesores por departamento\n ")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal() +  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))
#+++++++++++++++++++++++++++++++++++++++++++VAR4+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()

#+++++++++++++++++++++++++++++++++++++++++++VAR5+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR6+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR7+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR8+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR9+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR10+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR11+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR12+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR13+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR15+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#+++++++++++++++++++++++++++++++++++++++++++VAR20+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
d1=data.frame(table(datos$var4))
d2=data.frame(x=d1$Var1,y=d1$Freq)
d2$x=factor(d2$x,levels=c("Excelente","Muy buena","Regular","Mala"))

ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="blue", fill=rgb(0,0.65490,0.52549))+ geom_text(aes(label = y), vjust = -0.3, color = "blue")+labs(y="Cantidad de profesores", x = "Calidad de la comunicación") +ggtitle("¿Cuál es su valoración de la comunicación de los lineamientos por parte\n del MINEDUCYT en el marco de la continuidad educativa?")+theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()
#********************************* Tabla de doble entrada *************************************
#Percepción de la utilidad de LT, CE y GM según la edad
Doble1=data.frame(x=datos$ï..var1,y=datos$var6)
td1 <- table(Doble1$x,Doble1$y)
#Percepción de la utilidad de LT, CE y GM según el tiempo de servicio
Doble2=data.frame(x=datos$var2,y=datos$var6)
td2 <- table(Doble2$x,Doble2$y)
#Percepción de la comunicación de esmate según la edad
Doble3=data.frame(x=datos$ï..var1,y=datos$var4)
td3 <- table(Doble3$x,Doble3$y)
#Percepción de la comunicación del mined según la edad
Doble4=data.frame(x=datos$ï..var1,y=datos$var5)
td3 <- table(Doble3$x,Doble3$y)





#---------------------------------------Bloques de código descartados-----------------------------
edades = c(rep(22,4),rep(23,4),rep(25,17),rep(26,17),rep(27,36),rep(28,47),
           rep(29,40),rep(30,56),rep(31,38),rep(32,43),rep(33,41),rep(34,43),rep(35,49),
           rep(36,41),rep(37,50),rep(38,39),rep(39,64),rep(40,73),rep(41,61),rep(42,92),
           rep(43,79),rep(44,87),rep(45,90),rep(46,100),rep(47,110),rep(48,94),rep(49,104),
           rep(50,118),rep(51,91),rep(52,93),rep(53,112),rep(54,110),rep(55,102),rep(56,115),
           rep(57,88),rep(58,83),rep(59,66),rep(60,78),rep(68,164))
