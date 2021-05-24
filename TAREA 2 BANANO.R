#Datos higrologicos 

inp <- read.csv("FDC.csv",na.strings = "")

head(inp)
dim(inp)

inp[!complete.cases(inp),]

plot(inp[,2],type= "l",
     col = "blue",
     main = "Volumen de agua",
     xlab = ("Rio Estrella"),
     ylab = ("Rio Banano")
     )

lines(inp[ ,3], col = "green")

summary(inp[,2:3])
hist(inp[,2],
     main = "Histograma Río estrella",
     xlab = ("Frecuencia"),
     ylab = ("Aguas crecidas"),
     Col = "Red"
     )
   
hist(inp[,3], 
     main = "Histograma Río Banano",
     xlab = ("Frecuencia"),
     ylab = ("Aguas crecidas"),
     Col = "Red"
     ) 

names(inp) <- c("fecha", "Estrella","Banano")
attach(inp)
plot(Estrella)

Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")

MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum)   
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum)   

write.csv(rbind(MAQ_Estrella, MAQ_Banano),file= "MAQ.csv")

plot(MAQ_Banano,ylim = c(100,3000),
     col = "Yellow",
     main = "Valores anuales",
     xlab = ("Meses"),
     ylab = ("Años")
     )
lines(MAQ_Estrella, col = 2)

MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)   
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum)   


# CORRELA ANALISIS

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3],
     col = "Grey",
     main = "Coeficientes de CAUDALES",
     xlab = ("Estrella"),
     ylab = ("Bananoa")
     )

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)

summary(inp.lm)

plot(inp.lm)

                