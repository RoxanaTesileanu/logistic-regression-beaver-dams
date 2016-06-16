##PREZENTA/ABSENTA BARAJELOR DE CASTOR - ANALIZA DE REGRESIE LOGISTICA

#model viteza apei
cale_viteza<-"C:/Users/roxana/Desktop/info_viteza.txt"
info_viteza<-read.table(cale_viteza, header=TRUE)
names(info_viteza)
model_viteza<-glm(info_viteza$prezenta~info_viteza$viteza.apei, binomial)
summary(model_viteza)
anova(model_viteza, test="Chisq")


#model latimea albiei
cale_latime<-"C:/Users/roxana/Desktop/info_latimealbie.txt"
info_latimealbie<-read.table(cale_latime, header=TRUE)
names(info_latimealbie)
model_latimealbie<-glm(info_latimealbie$prezenta~info_latimealbie$latimea.albiei)
summary(model_latimealbie)

# model altitudine
cale_alt<-"C:/Users/roxana/Desktop/info_altitudine.txt"
info_alt<-read.table(cale_alt, header=TRUE)
names(info_alt)
model_alt <- glm(info_alt$prezenta~info_alt$altitudine, binomial)
summary(model_alt)


#model meandrare
cale_meandr<-"C:/Users/roxana/Desktop/info_meandr.txt"
info_meandr<-read.table(cale_meandr, header=TRUE)
names(info_meandr)
model_meandr1<-glm(info_meandr$prezenta ~info_meandr$meandr.av, binomial)
summary(model_meandr1)
model_meandr2<-glm(info_meandr$prezenta~info_meandr$curs.meandr)
summary(model_meandr2)

# model suprafata hrana
cale_s.hrana<-"C:/Users/roxana/Desktop/info_supraf_hrana.txt"
info_s.hrana<-read.table(cale_s.hrana, header=TRUE)
names(info_s.hrana)
model_s.hrana<-glm(info_s.hrana$prezenta~info_s.hrana$s.hrana.av + info_s.hrana$s.hrana.am, binomial)
summary(model_s.hrana)


#model acoperire mal
cale_acoperire<-"C:/Users/roxana/Desktop/info_ac.maluri.txt"
info_ac<-read.table(cale_acoperire, header=TRUE)
names(info_ac)
model_ac<-glm(info_ac$prezenta~info_ac$ac.maluri, binomial)
summary(model_ac)

#model inclinare maluri
cale_inclinare<-"C:/Users/roxana/Desktop/info_inclinare.maluri.txt"
info_incl<-read.table(cale_inclinare, header=TRUE)
names(info_incl)
model_incl<-glm(info_incl$prezenta~info_incl$in.dr, binomial)
summary(model_incl)

#model inaltime maluri
cale_inaltime.maluri<-"C:/Users/roxana/Desktop/info_inaltime.maluri.txt"
info_inal.maluri<-read.table(cale_inaltime.maluri, header=TRUE)
names(info_inal.maluri)
model_inaltime.maluri<-glm(info_inal.maluri$prezenta~info_inal.maluri$inaltime.dr, binomial)
summary(model_inaltime.maluri)

#### CONCLUZIE : viteza relationeaza cu prezenta/absenta barajelor

#sa vedem ce-i cu panta

#model panta + viteza
cale_panta.vit<-"C:/Users/roxana/Desktop/info_viteza_panta.txt"
info_v.p<-read.table(cale_panta.vit, header=TRUE)
names(info_v.p)
model_p<-glm(info_v.p$prezenta~info_v.p$panta, binomial)
summary(model_p)
model_vp<-glm(info_v.p$prezenta~info_v.p$viteza.apei + info_v.p$panta, binomial)
summary(model_vp)

cale_panta.vit2<-"C:/Users/roxana/Desktop/info_viteza_panta.txt"
info_v.p2<-read.table(cale_panta.vit2, header=TRUE)
names(info_v.p2)
model_p2<-glm(info_v.p2$prezenta~info_v.p2$panta, binomial)
summary(model_p2)
model_vp2<-glm(info_v.p2$prezenta~info_v.p2$viteza.apei + info_v.p2$panta, binomial)
summary(model_vp2)

# model adancime medie
cale_adancime<-"C:/Users/roxana/Desktop/info_viteza_panta_adancime.txt"
info_adancime<-read.table(cale_adancime, header=TRUE)
names(info_adancime)
model_adancime<- glm(info_adancime$prezenta~info_adancime$adancime, binomial)
summary(model_adancime)


# PLOT VITEZA
names(info_viteza)
model_viteza<-glm(info_viteza$prezenta~info_viteza$viteza.apei, binomial)
summary(info_viteza)
newXs<-seq(0,0.65, 0.01)
newXs
new.log.odds<-4.170-8.381*newXs
new.log.odds
new.pis<-exp(4.170-8.381*newXs)/(1+exp(4.170-8.381*newXs))
new.pis
plot.sim.df<-data.frame(newXs,new.pis)
names(plot.sim.df)

cale_plot<-"C:/Users/roxana/Desktop/plot_baraje_viteza.bmp"
bmp(cale_plot, width=700, height=500)
plot(info_viteza$viteza.apei, info_viteza$prezenta, xlim=c(0,0.65), ylim=c(0,1),
     xlab="viteza apei [m/s]", ylab="probabilitatea de prezenta a barajelor", 
     main="Prezenta/absenta observata a barajelor si modelul de predictie a 
     probabilitatii prezentei barajelor in functie de viteza") 
  lines(plot.sim.df$newXs, plot.sim.df$new.pis)
dev.off()

cale_plot2<-"C:/Users/roxana/Desktop/plot_baraje_viteza2.bmp"
bmp(cale_plot2, width=700, height=500)
plot(info_viteza$viteza.apei, info_viteza$prezenta, xlim=c(0,0.65), ylim=c(0,1),
     xlab="viteza apei [m/s]", ylab="probabilitatea de prezenta a barajelor", 
     main="Prezenta/absenta observata a barajelor si modelul de predictie a 
     probabilitatii prezentei barajelor in functie de viteza") 
lines(plot.sim.df$newXs, plot.sim.df$new.pis) 
dev.off()
