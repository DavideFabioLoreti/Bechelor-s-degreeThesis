correlogrammi<-function(dati, lm=36){
  n.obs <- length(dati)
  par(mfrow=c(1,2))
  acfs<-acf(dati,lag.max = lm, plot=F)
  m.acfs<-max(abs(acfs$acf[-1])) + 0.1
  pacfs<-pacf(dati,lag.max = lm, plot=F)
  m.pacfs<-max(abs(pacfs$acf)) + 0.1
  
  #plot acf
  barplot(rev(as.matrix(acfs$acf[-1])), beside = T, col= "yellow", xlim = c(-m.acfs, m.acfs), horiz = T, 
          main = "Acf globale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  #plot pacf
  barplot(rev(as.matrix(pacfs$acf)), beside = T, col="yellow", xlim=c(-m.pacfs,m.pacfs), horiz = T,
          main= "Acf parziale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  par(mfrow=c(1,1))
}

arimaest<-function(dati, nsorder= c(0,0,0), sorder= c(0,0,0), periodo=NA, vf=F, res=F){
  ris<-arima(dati, order=nsorder, seasonal= list(order=sorder, period=periodo), include.mean = vf)
  coef.t<-ris$coef/diag(ris$var.coef)^(1/2)
  out<- if (res==F) list (ris, coef.t) else ris$residuals
  names(out)<-list("stime", "tstatistics")
  out
}

testnorm<-function(residui, nclassi){
  res.stand<- (residui - mean(residui))/sd(residui) #residui standardizzati
  hist(res.stand, freq = F, main="", col="lightblue", ylim = c(0,0.6), nclass=nclassi) #istogramma
  curve(dnorm, add=T, col="red", lwd=2) # adattiamo curva normale per confronto
  library(tseries)
  jb<-jarque.bera.test(res.stand) #test di Jarque-Bera
  legend("topright", legend = paste("JB test p.value = ", round(jb$p.value,3)), bty = "n", cex=1.2)
  jb
}

correlresidui<-function(residui,lm=36, npar=0){
  par(mfrow=c(1,3))
  n.obs<-length(residui)
  acfs<-acf(residui, lag.max = lm, plot = F)
  m.acfs <- max(abs(acfs$acf[-1])) + 0.1
  pacfs<-pacf(residui, lag.max = lm, plot = F)
  m.pacfs<-max(abs(pacfs$acf)) + 0.1
  
  #plot acf
  barplot(rev(as.matrix(acfs$acf[-1])), beside = T, col= "yellow", xlim = c(-m.acfs, m.acfs), horiz = T, 
          main = "Acf globale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  #plot pacf
  barplot(rev(as.matrix(pacfs$acf)), beside = T, col="yellow", xlim=c(-m.pacfs,m.pacfs), horiz = T,
          main= "Acf parziale", ylab = "", cex.names = 0.9, names.arg = rev(seq(1:lm)))
  
  abline(v=0)
  abline(v=c(-1.96/n.obs^(1/2),1.96/n.obs^(1/2)), lty = 2)
  
  #grafico statistica Ljung-Box
  LBv<-rep(0,lm)
  for (i in ((npar+1):lm)){
    LBv[i]<-Box.test(residui, lag=i, type = "Ljung-Box",fitdf = npar)$p.value
  }
  plot(LBv, rev(seq(1:lm)),main = "Ljung-Box stat", axes = F, ylab = "lag", xlab="p.value",
       xlim = c(0,1), type = "n")
  points(LBv[(npar+1):lm], rev(seq(1:(lm-npar))))
  abline(v=0.05, col="red")
  axis(1,seq(0,1,0.1))
  axis(2,at=seq(1:lm),labels = rev(seq(1:lm)))
  
  par(mfrow=c(1,1))
  
}

seasplot<-function(dati, periodo){
  #dati: dati da plottare
  #periodo: 12 se serie mensile, 4 se serie trimestrale
  
  nomi<-if (periodo==12) list ("G", "F", "M", "A", "M", "G", "L", "A", "S", "O", "N", "D")
  else list ("I", "II", "III", "IV")
  
  n<-length(dati)
  li<-rep(0, periodo)
  mi<-rep(0, periodo)
  
  li[1]<-length(dati[seq(1, length(dati),by=periodo)]) #serie gennaio
  labx<-if (periodo==12) "month" else "quarter" #asse delle ascisse
  
  par(mfrow=c(1,1), cex=0.8)
  plot(seq(1:li[1]),dati[seq(1, length(dati), by=periodo)], ylab="", ylim = c(min(dati),max(dati)), xlim=c(1,n),
       xlab=labx, main="Serie stagionali",type = "l",col="blue", lwd=2, axes=F) #grafico spezzata gennaio
  mi[1]<-mean(dati[seq(1, length(dati),by=periodo)]) #media gennaio
  segments(1, mi[1], li[1], mi[1], col="red") #aggiungo media gennaio sul grafico
  
  for (i in 2:periodo){
    li[i]<-length(dati[seq(i, length(dati),by=periodo)]) #
    lines(cumsum(li)[i-1] + seq(1:li[i]), dati[seq(i, length(dati),by=periodo)], col="blue", lwd=2) #altri grafici
    mi[i]<-mean(dati[seq(i, length(dati),by=periodo)]) #medie altri mesi
    segments(1+cumsum(li)[i-1], mi[i],li[i] + cumsum(li)[i-1], mi[i], col="red") #aggiungo medie successive grafico
  }
  axis(1, at = (li[i])*seq(1:periodo) - li[1]/2, labels= nomi)
  axis(2)
}

forecastplot<-function(dati, stime, passi){
  #dati = dati originali
  #stime = stime arimaest a partire da serie originale con d, D diversi da 0
  #passi = numero di istanti di tempo da prevedere
  
  forecast <- predict(stime, passi)$pred
  forecast.se <- predict(stime, passi)$se
  
  plot(c(dati,forecast), type = "l")
  lines(forecast, col = "blue")
  lines(forecast + 1.96*forecast.se, col="blue", lty=2)
  lines(forecast - 1.96*forecast.se, col="blue", lty=2)
  
  list(forecast, forecast.se)
}

