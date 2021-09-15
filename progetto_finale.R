library(ggmap)
library(maptools)
library(rgdal)
library(dplyr)
library(plyr)
library(spatstat)
library(sp)
library(rgeos)

#Funzione trasforma coordinate
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

tsup=readShapeLines("tpl_percorsi.shp")
l90=tsup[tsup$linea==90,]
head(coordinates(l90)[[1]][[1]])

l90.1=l90[l90$percorso==3575,]
l90.2=l90[l90$percorso==4086,]

lat_l90.1=coordinates(l90.1)[[1]][[1]][,2]
lat_l90.2=coordinates(l90.2)[[1]][[1]][,2]
long_l90.1=coordinates(l90.1)[[1]][[1]][,1]
long_l90.2=coordinates(l90.2)[[1]][[1]][,1]

cord_l90.1=LongLatToUTM(long_l90.1,lat_l90.1, 32)
cord_l90.1$X=cord_l90.1$X+1000000
coordinates(cord_l90.1)=cord_l90.1[,2:3]

cord_l90.2=LongLatToUTM(long_l90.2,lat_l90.2, 32)
cord_l90.2$X=cord_l90.2$X+1000000
coordinates(cord_l90.2)=cord_l90.2[,2:3]

l90.1.sp=SpatialLines2PolySet(l90.1)

library(readxl)

#cinema e teatro
cinteat=read_xlsx("cinemateatro.xlsx")
head(cinteat)
ct_lat=as.numeric(cinteat$X__4)
ct_long=as.numeric(cinteat$X__5)
cord_ct=LongLatToUTM(ct_long, ct_lat, 32)
cord_ct$X=cord_ct$X+1000000
coordinates(cord_ct)=cord_ct[,2:3]

punti_ct=as.data.frame(coordinates(cord_ct))

#architettura
architettura=read_xlsx("dataset finale.xlsx", sheet = 1)
head(architettura)
arc_lat=as.numeric(architettura$X__4)
arc_long=as.numeric(architettura$X__5)
cord_arc=LongLatToUTM(arc_long, arc_lat, 32)
cord_arc$X=cord_arc$X+1000000
coordinates(cord_arc)=cord_arc[,2:3]

punti_arc=as.data.frame(coordinates(cord_arc))

v.c.arc=conta_arc@data@values

#bar
bar=read_xlsx("dataset finale.xlsx", sheet = 2)
head(bar)
bar_lat=as.numeric(bar$X__4)
bar_long=as.numeric(bar$X__5)
cord_bar=LongLatToUTM(bar_long, bar_lat, 32)
cord_bar$X=cord_bar$X+1000000
coordinates(cord_bar)=cord_bar[,2:3]

punti_bar=as.data.frame(coordinates(cord_bar))

v.c.bar=conta_bar@data@values

#clubdisco
cd=read_xlsx("dataset finale.xlsx", sheet = 4)
head(cd)
cd_lat=as.numeric(cd$X__4)
cd_long=as.numeric(cd$X__5)
cord_cd=LongLatToUTM(cd_long, cd_lat, 32)
cord_cd$X=cord_cd$X+1000000
coordinates(cord_cd)=cord_cd[,2:3]

punti_cd=as.data.frame(coordinates(cord_cd))

v.c.cd=conta_cd@data@values

#musei
mus=read_xlsx("dataset finale.xlsx", sheet = 5)
head(mus)
mus_lat=as.numeric(mus$X__4)
mus_long=as.numeric(mus$X__5)
cord_mus=LongLatToUTM(mus_long, mus_lat, 32)
cord_mus$X=cord_mus$X+1000000
coordinates(cord_mus)=cord_mus[,2:3]

punti_mus=as.data.frame(coordinates(cord_mus))

#ristoranti
risto=read_xlsx("dataset finale.xlsx", sheet = 6)
head(risto)
risto_lat=as.numeric(risto$X__4)
risto_long=as.numeric(risto$X__5)
cord_risto=LongLatToUTM(risto_long, risto_lat, 32)
cord_risto$X=cord_risto$X+1000000
coordinates(cord_risto)=cord_risto[,2:3]

punti_risto=as.data.frame(coordinates(cord_risto))

#mappe con conteggi
ct.in <- as.ppp(punti_ct, W=milano)
conta_ct <- quadratcount(ct.in,18,17)

arc.in <- as.ppp(punti_arc, W=milano)
conta_arc <- quadratcount(arc.in,18,17)

bar.in <- as.ppp(punti_bar, W=milano)
conta_bar <- quadratcount(bar.in,18,17)

cd.in <- as.ppp(punti_cd, W=milano)
conta_cd <- quadratcount(cd.in,18,17)

mus.in <- as.ppp(punti_mus, W=milano)
conta_mus <- quadratcount(mus.in,18,17)

risto.in <- as.ppp(punti_risto, W=milano)
conta_risto <- quadratcount(risto.in,18,17)

df.ct=data.frame(conta_ct)
tess.ct=as.tess(conta_ct)
tiles.ct=tess.ct$tiles

a <- tiles.ct
b <- c()
e <- c()
h <- c()
t <- c()
i=1
f <- names(a)

for (j in a) {
  b[[i]]<- j$xrange[1]
  e[[i]] <- j$yrange[1]
  h[[i]]<- j$xrange[2]
  t[[i]] <- j$yrange[2]
  i = i+1
}

dati <- data.frame('nomi' = f, 'xrange1' = b, 'xrange2'=h, 'yrange1'=e, 'yrange2'=t)

df.arc=data.frame(conta_arc)
df.arc$xrange1 <- dati$xrange1
df.arc$xrange2 <- dati$xrange2
df.arc$yrange1 <- dati$yrange1
df.arc$yrange2 <- dati$yrange2

tess.arc=as.tess(conta_arc)
df.bar=data.frame(conta_bar)
tess.bar=as.tess(conta_bar)
df.cd=data.frame(conta_cd)
tess.cd=as.tess(conta_cd)
df.mus=data.frame(conta_mus)
tess.mus=as.tess(conta_mus)
df.risto=data.frame(conta_risto)
tess.risto=as.tess(conta_risto)

table(df.ct$Freq)
df.ct$ct.cat=0
df.ct[which(df.ct$Freq>=1),"ct.cat"]<-1

table(df.arc$Freq)
df.arc$arc.cat=0
df.arc[which(df.arc$Freq==1|df.arc$Freq==2),"arc.cat"]=1
df.arc[which(df.arc$Freq==3|df.arc$Freq==4),"arc.cat"]=2
df.arc[which(df.arc$Freq>=5),"arc.cat"]=3

df.bar$bar.cat=0
df.bar[which(df.bar$Freq==1),"bar.cat"]=1
df.bar[which(df.bar$Freq==2),"bar.cat"]=2
df.bar[which(df.bar$Freq>=3),"bar.cat"]=3

df.cd$cd.cat=0
df.cd[which(df.cd$Freq==1),"cd.cat"]=1
df.cd[which(df.cd$Freq>=2),"cd.cat"]=2

df.mus$mus.cat=0
df.mus[which(df.mus$Freq==1),"mus.cat"]=1
df.mus[which(df.mus$Freq>=2),"mus.cat"]=2

df.risto$ris.cat=0
df.risto[which(df.mus$Freq==1),"ris.cat"]=1
df.risto[which(df.mus$Freq==2),"ris.cat"]=2
df.risto[which(df.mus$Freq>=3),"ris.cat"]=3

df.freq=as.data.frame(cbind(arc=df.arc$arc.cat, bar=df.bar$bar.cat,
                            cd=df.cd$cd.cat, ct=df.ct$ct.cat,
                             mus=df.mus$mus.cat, risto=df.risto$ris.cat))
rownames(df.freq)=df.arc$tile

library(parsec)

prf <- pop2prof(df.freq, labtype = "progressive")
Z <- getzeta(prf)

plot(Z)

mrp <- MRP(Z, method="approx")

scomp_val_sing <- svd(mrp)

dominance_autovector <- abs(scomp_val_sing$v[,1]) #scores che descrivono i profili
#non portano ad un ordinamento completo (profili uguali, tra di loro equivalenti)
puf <- data.frame(dominance_autovector)

profili <- prf$profiles

pif <- cbind(profili,puf)
pif$indice <- rownames(pif)

df.freq$posizione=rownames(df.freq)
df.merge=merge(df.freq, pif, by=c("arc","bar","cd","ct","mus", "risto"), sort=F)

#colmeans(M) probabilità che l'elemento della colonna domini un qualsiasi altro elemento del poset
#M=MRP
#con uno massimale possiamo fare score/score maggiore, ma non danno un significato di probabilità

#0.2 è ragionevole, rispetto a colmeans è un risultato che raccoglie tutta la struttura del poset, l'altro u esperimento casuale che si ripete una sola volta
#suggerimento: non riscalare

#poset coome una rete o un grafo con orientamenti vs basso.
#catena di markov, abbiamo una rete ma si può tornare indietro, ci possono essere dei cilci
#prob da a a b?
#si costruisce matrice simile a mrp, se si ccalcolasse autovett pricipale
#i valori di questo sommano a 1 e ci dicono dopo un numero indìfinito di passaggi qual è la prob che mi trovi in a?


#qui si ottiene con svd non con auotvett principale

df.finale=merge(df.merge,dati, by.x="posizione", by.y="nomi")
df.finale$mediax=rowMeans(df.finale[,c("xrange1","xrange2")])
df.finale$mediay=rowMeans(df.finale[,c("yrange1","yrange2")])

plot(milano)
centroidi=as.data.frame(cbind(df.finale$mediax,df.finale$mediay))
coordinates(centroidi)=centroidi
points(centroidi, col="red", pch=19, cex=0.5)

ppp<-df.finale[,c("mediax","mediay","dominance_autovector")]
rownames(ppp)=rownames(df.finale)


ppp0=as.ppp(ppp,W=milano,mark=ppp$dominance_autovector);ppp0

sm=Smooth.ppp(ppp0,weights=ppp0$marks,na.rm=T,sigma=600, at="points")

v=as.data.frame(sm$v)

plot(sm)



plot(ppp0)
plot(Smooth.ppp(ppp0,weights=ppp0$marks,na.rm=T,sigma=600),type='l',bty='n', main="Ranking attrattività")

modif=as.data.frame(ppp0,sm)

p.smooth=data.frame(cordx=ppp0$x, cordy=ppp0$y, sm)

grid <- raster::raster(extent(milano), nrows=18, ncol=17)
proj4string(grid)<-proj4string(milano)
gridpolygon <- rasterToPolygons(grid)

dry.grid <- intersect(milano, gridpolygon)

windows()
plot(ppp0)
plot(dry.grid, add=T)
text(coordinates(dry.grid)[,1],coordinates(dry.grid)[,2], rownames(dry.grid@data),col="blue", cex=0.5)
