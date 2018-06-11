library(knitr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)
library(nortest)
# Carreguem el dataset en una variable mitjançant la funció read.csv
bd <- read.csv("CSAP.csv",header=TRUE)
# Presentem una mostra amb els valors de les 2 primeres files del dataset
kable(t(bd[1:2,]))
# Mostrem un resum estadístic i del tipus de dades del dataset
summary(bd)
variables<-as.matrix(sapply(bd, class))
kable(variables, caption = "Tipus de dades")
bd$PacientsNivell4Adults<-as.integer(round((bd$PacientsNivell4*0.85),0))
bd$PacientsNivell4Pediatrics<-as.integer(bd$PacientsNivell4-bd$PacientsNivell4Adults)
kable(bd[10,1:3])
str_sub(bd$dataCaptura[1],-2,-1)
str_sub(substring(bd$dataActualitzacioWeb[1],regexpr(",",bd$dataActualitzacioWeb[1])),3,
3-nchar(substring(bd$dataActualitzacioWeb[1],regexpr(",",bd$dataActualitzacioWeb[1]))))
#Amb aquest bucle comprovem si a alguna fila les dues dates son diferents en dia:
for ( i in 1:nrow(bd))
{if(str_sub(bd[i,2],-2,-1)!=str_sub(substring(bd[i,1],regexpr(",",bd[i,1])),
    3,3-nchar(substring(bd[i,1],regexpr(",",bd[i,1])))))
{print("data captura diferent a data actualització")}
}
#Canviem el nom de la variable dataCaptura
names(bd)<-gsub("dataCaptura","dataActualitzacio",names(bd))
#Creem la variable horaActualitzacio i l'omplim amb la informació de l'hora i minuts que 
#disposem a la variable dataActualitzacioWeb
bd$horaActualitzacio<-substr(gsub("'",":",sub(".*\\s+", "", bd[,1])),1,5)
#Mostrem un parell d'exemples de com queden les variables
kable(t(bd[10:11,]))
for ( i in 1:nrow(bd)){bd$Torn[i]<-if (as.integer(substr(bd[i,12],1,2))>=7 
& as.integer(substr(bd[i,12],1,2))<15){"Mati"} else if (substr(bd[i,12],1,2)>=15 
& substr(bd[i,12],1,2)<23){"Tarda"}else {"Nit"}}
bd<-bd[,-c(1,3)]
variables<-as.matrix(sapply(bd, class))
kable(variables, caption = "Tipus de dades")
bd$dataActualitzacio<-as.Date(bd$dataActualitzacio)
class(bd$dataActualitzacio)

bd$minutstempsEsperaAdults<-as.integer(as.integer(substr(gsub("h. ","",
bd$tempsEsperaAdults),0,2))*60+as.integer(ifelse((substr(substr(
gsub("h. ","",bd$tempsEsperaAdults),3,4),2,2))=="m",substr(substr(gsub("h. ","",
bd$tempsEsperaAdults),3,4),1,1),substr(substr(gsub("h. ","",
bd$tempsEsperaAdults),3,4),1,2))))

bd$minutstempsEsperaPediatrics<-as.integer(as.integer(substr(gsub("h. ","",
bd$tempsEsperaPediatrics),0,2))*60+as.integer(ifelse((substr(substr(gsub("h. ","",
bd$tempsEsperaPediatrics),3,4),2,2))=="m",substr(substr(gsub("h. ","",
bd$tempsEsperaPediatrics),3,4),1,1),substr(substr(gsub("h. ","",
bd$tempsEsperaPediatrics),3,4),1,2))))
bd$dataActualitzacioCompleta <- as.POSIXct(paste(as.factor(bd$dataActualitzacio),
                                                 bd$horaActualitzacio, sep=" "))

bd<-bd[,-c(6,7)]
variables<-as.matrix(sapply(bd, class))
kable(variables, caption = "Tipus de dades")

summary(bd)

print("No existeix cap valor buit?")
sapply(bd, function(x){all(!is.na(x))})
print("No existeix cap valor 0?")
sapply(bd, function(x){all((x)!=0)})
par(mfrow=c(3,3))
for(i in 1:ncol(bd)){if(is.integer(bd[,i])){boxplot(bd[,i],main=colnames(bd)[i])}}
for(i in 1:ncol(bd)){if(is.integer(bd[,i])){ print(cat(names(bd[i]),":",
boxplot.stats(bd[,i])$out,"\n"))}}

outliers=subset(bd,minutstempsEsperaAdults>=87)
outliers[,c(1,6,8)]

write.csv(bd,"CSAP_clean.csv")
adults<-bd[,c("Torn","PacientsNivell4Adults","minutstempsEsperaAdults","dataActualitzacioCompleta")]
adults<-subset(adults, adults$PacientsNivell4Adults!=0)
pediatrics<-bd[,c("Torn","PacientsNivell4Pediatrics","minutstempsEsperaPediatrics","dataActualitzacioCompleta")]
pediatrics<-subset(pediatrics, pediatrics$PacientsNivell4Pediatrics!=0)
kable(head(adults), align = "c", caption = "Adults")
kable(head(pediatrics), align = "c",caption = "Pediàtrics")

by(adults$minutstempsEsperaAdults, adults$Torn, summary)
by(pediatrics$minutstempsEsperaPediatrics, pediatrics$Torn, summary)

#adults
a1<-ggplot(adults, aes(x=PacientsNivell4Adults),
xlab = "Número pacients nivell 4 i 5 adults")+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) 
a2<-ggplot(adults, aes(x=PacientsNivell4Adults),
xlab = "Número pacients nivell 4 i 5 adults")+ylab("Densitat") +
  geom_histogram(binwidth=.5)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
a3<-ggplot(adults, aes(x=minutstempsEsperaAdults),
xlab = "minuts espera adults")+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
a4<-ggplot(adults, aes(x=minutstempsEsperaAdults),
xlab = "minuts espera adults")+ylab("Densitat") +
  geom_histogram(binwidth=.5)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
a9<-ggplot(adults, aes(sample=minutstempsEsperaAdults),
xlab = "minuts espera adults")+xlab("quantils teorics")+ylab("quantils mostra") +
  stat_qq()+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

grid.arrange(a1, a2,a3,a4,a9, nrow = 3, ncol=2, top=textGrob("Adults", 
gp=gpar(fontsize=18,font=2)))

#pediatrics
p1<-ggplot(pediatrics, aes(x=PacientsNivell4Pediatrics),
xlab = "Número pacients nivell 4 i 5 pediatrics")+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
p2<-ggplot(pediatrics, aes(x=PacientsNivell4Pediatrics),
xlab = "Número pacients nivell 4 i 5 pediatrics")+ylab("Densitat") +
  geom_histogram(binwidth=.5)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
p3<-ggplot(pediatrics, aes(x=minutstempsEsperaPediatrics),
xlab = "minuts espera pediatrics")+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
p4<-ggplot(pediatrics, aes(x=minutstempsEsperaPediatrics), 
xlab = "minuts espera pediatrics")+ylab("Densitat")+geom_histogram(binwidth=.5)+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=16,face="bold"))
p9<-ggplot(pediatrics, aes(sample=minutstempsEsperaPediatrics),
xlab = "minuts espera pediatrics")+xlab("quantils teorics")+ylab("quantils mostra") +
  stat_qq()+theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

grid.arrange(p1, p2,p3, p4,p9,nrow = 3, ncol=2, top=textGrob("Pediàtrics", 
gp=gpar(fontsize=18,font=2)))

a5<-ggplot(adults, aes(x=Torn, y=adults$minutstempsEsperaAdults))+
  ylab("Temps espera") +geom_boxplot()
a6<-ggplot(adults, aes(x=minutstempsEsperaAdults))+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold")) + facet_wrap(~Torn)
a7<-ggplot(adults, aes(x=Torn, y=adults$PacientsNivell4Adults))+
  ylab("Número pacients") +geom_boxplot()
a8<-ggplot(adults, aes(x=PacientsNivell4Adults))+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold")) + facet_wrap(~Torn)
grid.arrange(a5, a6,a7,a8, nrow = 2, ncol=2, top=textGrob("Adults", 
gp=gpar(fontsize=10)))

p5<-ggplot(pediatrics, aes(x=Torn, y=pediatrics$minutstempsEsperaPediatrics))+
  ylab("Temps espera") +geom_boxplot()
p6<-ggplot(pediatrics, aes(x=minutstempsEsperaPediatrics))+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10,face="bold")) + facet_wrap(~Torn)
p7<-ggplot(pediatrics, aes(x=Torn, y=pediatrics$PacientsNivell4Pediatrics))+
  ylab("Número pacients") +geom_boxplot()
p8<-ggplot(pediatrics, aes(x=PacientsNivell4Pediatrics))+ylab("Densitat") + 
  geom_density(fill="black", alpha=0.25)+theme(axis.text=element_text(size=6),
        axis.title=element_text(size=10,face="bold")) + facet_wrap(~Torn)
grid.arrange(p5, p6,p7,p8, nrow = 2, ncol=2, top=textGrob("Pediàtrics", 
gp=gpar(fontsize=10)))

print(ad.test(adults$PacientsNivell4Adults))
print(ad.test(adults$minutstempsEsperaAdults))
print(ad.test(pediatrics$PacientsNivell4Pediatrics))
print(ad.test(pediatrics$minutstempsEsperaPediatrics))

fligner.test(minutstempsEsperaAdults~interaction(Torn),data=adults)
fligner.test(minutstempsEsperaPediatrics~interaction(Torn),data=pediatrics)

ANOVA= aov( adults$minutstempsEsperaAdults ~ adults$Torn)
summary(ANOVA)

eta_quadrat <- 16999/(16999 + 133003)
eta_quadrat

TukeyHSD(ANOVA)

plot(ANOVA)
plot(TukeyHSD(ANOVA))
