data("quakes") #importam datasetul
cor(quakes$stations, quakes$mag) #corelatia pe care am gasit-o cea mai mare (0.85)

simpleRegression <- lm(mag~stations,data=quakes) #facem regresie liniara pentru datele puternic corelate
simpleRegressionSummary<-summary(simpleRegression)
coefs<-simpleRegressionSummary$coefficients

beta.estimate<-coefs["stations","Estimate"] #extragem coeficientii regresiei simple
std.error<-coefs["stations","Std. Error"] # 0.003055589
t.value<-beta.estimate/std.error #51.2314
p.value<-2*pt(-abs(t.value),df=nrow(quakes)-ncol(quakes)) #2.835015e-281
fstat<-summary(simpleRegression)$fstatistic

model_p1<-pf(fstat[1],fstat[2],fstat[3],lower=FALSE)

aic<-AIC(simpleRegression) #-265.0973
bic<-BIC(simpleRegression) #-250.3471

plot(x=quakes$mag,y=quakes$stations)
seq<-seq(0,100,0.01)
func<-simpleRegression$coefficients[1]+simpleRegression$coefficients[2]*seq
plot(seq,func) #dreapta care aproximeaza

cor(quakes)
multipleRegression<-lm(mag~stations+depth,data=quakes)
multipleRegressionSummary<-summary(multipleRegression)

fstat2<-multipleRegressionSummary$fstatistic
AIC(multipleRegression) #-371.8911
BIC(multipleRegression) #-253.2601

model_p2<-pf(fstat2[1],fstat2[2],fstat2[3],lower=FALSE) #2.178577e-303

#Putem adauga la baza de date o variabila afarea (aria afectata de un cutremur
#in km2)
#Intuitiv, aria afectata ar putea fi corelata cu adancimea cutremurului si cu
#magnitudinea acestuia. Distrubutia acestei variabile ar fi una normala. Analizand
#care sunt cele mai dese tipuri de cutremure, putem trage concluzia ca o distributie
#plauzibila ar fi Normala(medie=9,dispersie=2.5)

size <- length(quakes$lat)
afarea <- rnorm(size, mean = 9, sd = 2.5)
plot(density(afarea), main="Affected area")
polygon(density(afarea), col="blue")