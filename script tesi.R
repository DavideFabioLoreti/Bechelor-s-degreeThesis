library(readxl)
serie_energia <- read_excel("C:/Users/HP/Desktop/serie energia.xlsx")
View(serie_energia)



dati<-ts(serie_energia$`COSTO c€/kWh`,start=c(2004,1),frequency = 4)
dati<-dati[1:72] #levo i valori che causano lo shock
#dati<-ts(dati,start=c(2004,1),frequency = 4)

ts.plot(dati,gpars=list(xlab="ANNO", ylab=" c€/kWh"))
#dati<-round(dati,2) 
#points(dati,pch=19,col="darkorange",cex=0.7)
title(main="Serie Storica del P.U.N. (1° trim 2004 - 2° trim 2023)")

abline(h=mean(dati),lty=3)



seasplot(dati,periodo = 4)

#serie non stazionaria in media e in varianza
#ldati<-log(dati) #non tratto la non stazionarietà in varianza
ddati<-diff(dati,1)#tratto solo la non stazionarietà in media
ts.plot(ddati)

seasplot(ddati,4) #non sembra necessario fare una differenziazione
#di ordine 4 per trattare la stagionalità

correlogrammi(ddati)

#d4ldati<-diff(dati,4)
#correlogrammi(d4ldati)



arimaest(ddati,nsorder = c(1,0,0), sorder = c(1,0,0), periodo= 4) #204.71<<<<<< 
arimaest(ddati,nsorder = c(0,0,1), sorder = c(1,0,0), periodo= 4) #208.26
arimaest(ddati,nsorder = c(2,0,2), sorder = c(1,0,0), periodo= 4) #204.01<<<<<
arimaest(ddati,nsorder = c(0,0,1), sorder = c(0,0,1), periodo= 4) #211.79


#mettere i 2 modelli ar(1)ar(1)s [piu semplice] e arma(2,2), arma(1,0,0)s [piu informativo]

res<-arimaest(ddati,nsorder = c(2,0,2), sorder = c(1,0,0), periodo= 4,res=T)
correlresidui(res, npar=5)
testnorm(res , nclassi=20) # dal graifico ha andamento normale
#ma la presneza di outlier fa si che il pvalore venga 0 (molto sensibile alla
#presenza di outlier)

fit<-arimaest(dati,nsorder = c(2,1,2), sorder = c(1,0,0), periodo= 4)$stime
forecastplot(dati,fit,passi=10) #previsioni di 10 trimestri avanti, dal IV 2021

#il modello ar(1) prevede lo shock ma non torna in corrispondenza della media
#il modello arma prevede lo shock ma in maniera molto piu contenuta rispetto
#alla realtà (picco previsto a 35 vs picco reale a 60) ma torna alla media

