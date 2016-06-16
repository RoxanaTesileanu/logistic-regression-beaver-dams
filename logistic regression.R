cale<-"C:/Users/roxana/Desktop/icas/logistic regression/inventariere baraje castor_2.csv"
mydata1<-read.table(cale, sep=";", dec=",", header=TRUE)
names(mydata1)
rauri<-mydata1[,3]
rauri
prezenta<-mydata1[,9]
prezenta
prezenta.df<-data.frame(rauri, prezenta)
#####le tai prezenta.df<-prezenta.df1[-(2:3),]
####### informatii curs apa
cale2<-"C:/Users/roxana/Desktop/icas/logistic regression/inventariere baraje castor_2curs apa.csv"
mydata_cursapa<-read.table(cale2, sep=";", dec=",", header=TRUE)
names(mydata_cursapa)
viteza.apei<-mydata_cursapa[,7]
panta.terenului<-mydata_cursapa[,8]
adancime.aval<-mydata_cursapa[,9]
latime.albie<-mydata_cursapa[,10]
curs.df<-data.frame(viteza.apei, panta.terenului, adancime.aval, latime.albie)
curs.df
####informatii hrana


###informatii meandrare
meandr.mal<-mydata_cursapa[,11:12]
curs.meands<-mydata_cursapa[,6]
meandr.df<-data.frame(meandr.mal, curs.meands)
meandr.df

#### insumare informatii (fara hrana)

info.baraje<-data.frame(prezenta.df, curs.df, meandr.df)
names(info.baraje)

summary(info.baraje)

####in plus

cale_3<-"C:/Users/roxana/Desktop/info_viteza.txt"
info_viteza<-read.table(cale_3, header=TRUE)
names(info_viteza)
model_viteza<-glm(info_viteza$prezenta~info_viteza$viteza.apei, binomial)
summary(model_viteza)
#######


glm(info.baraje$prezenta~info.baraje$viteza.apei, binomial)

