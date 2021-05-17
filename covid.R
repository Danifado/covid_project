data_path <- readline(prompt="Ingrese la ruta en la cual está ubicado el dataset: ")
data_path = paste(data_path, "data.csv",sep="/")
data <- read.csv(data_path)

data$Fecha_de_Notificación <- as.Date(data$Fecha_de_Notificación, "%d/%m/%Y")

data$Fecha_de_Inicio_de_Síntomas <- as.Date(data$Fecha_de_Inicio_de_Síntomas, "%d/%m/%Y")

data$Fecha_de_Diagnóstico <- as.Date(data$Fecha_de_Diagnóstico, "%d/%m/%Y")

data$Fecha_de_Recuperación <- as.Date(data$Fecha_de_Recuperación, "%d/%m/%Y")

data$Fecha_de_Muerte <- as.Date(data$Fecha_de_Muerte, "%d/%m/%Y")
#Inicio del codigo

diferencia_recuperacion <- data$Fecha_de_Recuperación - data$Fecha_de_Diagnóstico

diferencia_recuperacion <- as.numeric(diferencia_recuperacion)

data["Dias_de_Recuperacion"] <- diferencia_recuperacion


edadesPrimerPico <-data$Edad[data$Fecha_de_Diagnóstico >= "2020-07-07" & data$Fecha_de_Diagnóstico < "2020-08-08"]
hist(edadesPrimerPico, xlab = "Edades del Paciente", ylab = "Cantidad de Contagiados", col = "red", main = "Cantidad de Contagios por Edad en el Primer Pico")
medianaEdadesPrimerPico <- median(edadesPrimerPico, na.rm = TRUE)
mediaEdadesPrimerPico <- mean(edadesPrimerPico, na.rm = TRUE)
modaEdadesPrimerPico <- table(edadesPrimerPico)
modaEdadesPrimerPico <- sort(modaEdadesPrimerPico, decreasing = TRUE)
modaEdadesPrimerPico <- modaEdadesPrimerPico[1]
muertesPrimerPico <- data[data$Estado == "Fallecido" & 
                            data$Fecha_de_Muerte >= "2020-07-07" & 
                            data$Fecha_de_Muerte < "2020-08-08",]
barplot(table(muertesPrimerPico$Edad), main = "Muertes Por Edad en el Primer Pico", col=rgb(0.8,0.1,0.1,0.6), xlab = "Edad del Paciente Fallecido", ylab = "Cantidad de Pacientes fallecidos")

recuperadosPrimerPico <- data[data$Dias_de_Recuperacion <= 30 & 
                                data$Fecha_de_Diagnóstico >= "2020-07-07" & 
                                data$Fecha_de_Diagnóstico < "2020-08-08",]

barplot(table(recuperadosPrimerPico$Edad),
        main = "Pacientes Recuperados Por Edad en el Primer Pico",
        col=rgb(0.2,0.8,0.1,0.9), xlab = "Edad del Paciente Recuperado",
        ylab = "Cantidad de Pacientes Recuperados")

#Segundo Pico

edadesSegundoPico <-data$Edad[data$Fecha_de_Diagnóstico >= "2020-12-21" & data$Fecha_de_Diagnóstico < "2021-01-21"]
hist(edadesSegundoPico, xlab = "Edades del Paciente", ylab = "Cantidad de Contagiados", col = "red", main = "Cantidad de Contagios por Edad en el Segundo Pico")
medianaEdadesSegundoPico <- median(edadesSegundoPico, na.rm = TRUE)
mediaEdadesSegundoPico <- mean(edadesSegundoPico, na.rm = TRUE)
modaEdadesSegundoPico <- table(edadesSegundoPico)
modaEdadesSegundoPico <- sort(modaEdadesSegundoPico, decreasing = TRUE)
modaEdadesSegundoPico <- modaEdadesSegundoPico[1]
muertesSegundoPico <- data[data$Estado == "Fallecido" & 
                            data$Fecha_de_Muerte >= "2020-12-21" & 
                            data$Fecha_de_Muerte < "2021-01-21",]
barplot(table(muertesSegundoPico$Edad),
        main = "Muertes Por Edad en el Segundo Pico",
        col=rgb(0.8,0.1,0.1,0.6), xlab = "Edad del Paciente Fallecido",
        ylab = "Cantidad de Pacientes fallecidos")

recuperadosSegundoPico <- data[data$Dias_de_Recuperacion <= 30 & 
                                data$Fecha_de_Diagnóstico >= "2020-12-21" & 
                                data$Fecha_de_Diagnóstico < "2021-01-21",]

barplot(table(recuperadosSegundoPico$Edad),
        main = "Pacientes Recuperados Por Edad en el Segundo Pico",
        col=rgb(0.2,0.8,0.1,0.9), xlab = "Edad del Paciente Recuperado",
        ylab = "Cantidad de Pacientes Recuperados")

#Tercer Pico
edadesTercerPico <-data$Edad[data$Fecha_de_Diagnóstico >= "2021-03-21" &
                               data$Fecha_de_Diagnóstico < "2021-04-21"]
hist(edadesTercerPico, xlab = "Edades del Paciente",
     ylab = "Cantidad de Contagiados",
     col = "red",
     main = "Cantidad de Contagios por Edad en el Tercer Pico")
medianaEdadesTercerPico <- median(edadesTercerPico, na.rm = TRUE)
mediaEdadesTercerPico <- mean(edadesTercerPico, na.rm = TRUE)
modaEdadesTercerPico <- table(edadesTercerPico)
modaEdadesTercerPico <- sort(modaEdadesTercerPico, decreasing = TRUE)
modaEdadesTercerPico <- modaEdadesTercerPico[1]
muertesTercerPico <- data[data$Estado == "Fallecido" & 
                   data$Fecha_de_Muerte >= "2021-03-21" & 
                   data$Fecha_de_Muerte < "2021-04-21",]
barplot(table(muertesTercerPico$Edad),
        main = "Muertes Por Edad en el Tercer Pico",
        col=rgb(0.8,0.1,0.1,0.6), xlab = "Edad del Paciente Fallecido",
        ylab = "Cantidad de Pacientes fallecidos")


muertesTercerPico <- data[data$Estado == "Fallecido" & 
                            data$Fecha_de_Muerte >= "2021-03-21" & 
                            data$Fecha_de_Muerte < "2021-04-21",]

recuperadosTercerPico <- data[data$Dias_de_Recuperacion <= 30 & 
                     data$Fecha_de_Diagnóstico  >= "2021-03-21" & 
                     data$Fecha_de_Diagnóstico < "2021-04-21",]

barplot(table(recuperadosTercerPico$Edad),
        main = "Pacientes Recuperados Por Edad en el Tercer Pico",
        col=rgb(0.2,0.8,0.1,0.9), xlab = "Edad del Paciente Recuperado",
        ylab = "Cantidad de Pacientes Recuperados")

#2020-07-07 to 2020-08-08
#2020-12-21 to 2021-01-21
#2021-03-21 to 2021-04-21




