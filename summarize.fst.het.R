
setwd('C:/Users/mcros/Desktop/Postdoc UGA/Whitefly project 2020/manuscripts/review Insects special issue')

dat0 = read.table('whitefly.review.summary.txt',sep='\t',as.is=T,check.names=F,header=T)

dat0$Year1[which(dat0$Year1=='Unknown')] = 1990
dat0$Year2[which(dat0$Year2=='Unknown')] = 2000
dat0$Year1 = as.numeric(dat0$Year1)
dat0$Year2 = as.numeric(dat0$Year2)

# Time span of study collections
par(mfrow=c(1,2),oma=c(0,0,0,0),mar=c(7,5,0,2))
h1 = hist(dat0$Year1,breaks=seq(1990,2016,1),xaxt='n',yaxt='n',col='grey50',cex.lab=2,main='',xlab='',ylab='No. studies',right=F,ylim=c(0,10))
axis(1,lwd=3,at=seq(1990,2016,1),labels=F)
text(x=c(1990,seq(1992,2016,4)),y=-0.75,cex=1.5,labels=c('Unknown',seq(1992,2016,4)),srt=-90,adj=0,xpd=T)
title(xlab='First year',line=5,cex.lab=2)
axis(2,lwd=3,cex.axis=1.5)
hist(dat0$Year2,breaks=seq(2000,2018,1),xaxt='n',yaxt='n',col='grey50',cex.lab=2,main='',xlab='',ylab='No. studies',right=F,ylim=c(0,10))
axis(1,lwd=3,at=seq(2000,2018,1),labels=F)
text(x=c(2000,seq(2002,2018,4)),y=-0.75,cex=1.5,labels=c('Unknown',seq(2002,2018,4)),srt=-90,adj=0,xpd=T)
title(xlab='Last year',line=5,cex.lab=2)
axis(2,lwd=3,cex.axis=1.5)

dat = read.table('whitefly.review.data.txt',sep='\t',as.is=T,check.names=F,header=T)

# AMOVA
par(oma=c(0,0,0,0),mar=c(1,5,1,2))
boxplot(dat$AMOVA.within,frame=F,yaxt='n',cex.lab=2,ylab='Variance within populations (%)',ylim=c(40,100),whisklty=1,whisklwd=3,staplecol='white',boxcol='grey50',col='grey50',medlwd=3)
axis(2,lwd=3,cex.axis=1.5)
legend('topright',cex=1.5,legend=('N = 10'),bty='n')

# Range of FST
fst = dat[which(!is.na(dat$Min.FST)),]
studies = unique(fst$Citation)
study.max = apply(array(studies),1,function(x){max(fst$Max.FST[which(fst$Citation==x)])})
studies = studies[order(study.max,decreasing=T)]
par(oma=c(0,0,0,0),mar=c(5,16,0,1))
plot(x=c(0,1),y=c(0,nrow(fst)+length(studies)),col='white',xlab='FST',ylab='',xaxt='n',yaxt='n',bty='n',cex.lab=2)
axis(1,lwd=3,cex.axis=1.5,at=seq(0,1,0.2))
title(ylab='Study',cex.lab=2,line=14)
ylow = 0
label.pos = c()
# MEAM 1 and MED data
cols = c('black','grey90','lightgoldenrod','cornflowerblue')
biotypes = c('MEAM 1','MED','Indian Ocean','SSA 1')
for (i in 1:length(studies)){
	pos = which(fst$Citation==studies[i])
	if (length(pos)>1){
		label.pos = c(label.pos,ylow+1)
		fst2 = fst[pos,]
		for (j in 1:length(pos)){
			fst.low = fst2$Min.FST[j]
			fst.high = fst2$Max.FST[j]
			polygon(x=c(fst.low,fst.high,fst.high,fst.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==fst2$Biotypes[j])])
			if (which(pos==pos[j])==2){
				addy = 2
			} else {
				addy = 1			
			}
			ylow = ylow + addy
		}
	} else {
		label.pos = c(label.pos,ylow+0.5)
		fst.low = fst$Min.FST[pos]
		fst.high = fst$Max.FST[pos]
		polygon(x=c(fst.low,fst.high,fst.high,fst.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==fst$Biotypes[pos])])
		ylow = ylow + 2
	}
}
text(x=-0.05,y=label.pos,labels=c(studies),cex=1.5,xpd=T,adj=1)
legend('topright',legend=c('MEAM 1','MED','IO','SSA 1'),pch=22,pt.bg=c('black','grey90','lightgoldenrod','cornflowerblue'),cex=2,bty='n')


# Heterozygosity
het = dat[which(!is.na(dat$Min.HO)),]
studies = unique(het$Citation)
study.max = apply(array(studies),1,function(x){max(het$Min.HO[which(het$Citation==x)])})
studies = studies[order(study.max,decreasing=F)]
par(oma=c(0,0,0,0),mar=c(5,16,0,4))
plot(x=c(0,1),y=c(0,(nrow(het)*2)+length(studies)),col='white',xlab='Heterozygosity',ylab='',xaxt='n',yaxt='n',bty='n',cex.lab=2)
axis(1,lwd=3,cex.axis=1.5,at=seq(0,1,0.2))
title(ylab='Study',cex.lab=2,line=14)
ylow = 0
label.pos = c()
# MEAM 1 and MED data
cols = c('black','grey90','lightgoldenrod','cornflowerblue')
biotypes = c('MEAM 1','MED','Indian Ocean','SSA 1')
for (i in 1:length(studies)){
	pos = which(het$Citation==studies[i])
	het2 = het[pos,]
	if (length(pos)>1){
		label.pos = c(label.pos,ylow+3)
		for (j in 1:length(pos)){
			# Observed heterozygosity
			het.low = het2$Min.HO[j]
			het.high = het2$Max.HO[j]
			polygon(x=c(het.low,het.high,het.high,het.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==het2$Biotypes[j])],border='black')
			ylow = ylow + 1
			# Expected heterozygosity
			het.low = het2$Min.HE[j]
			het.high = het2$Max.HE[j]
			polygon(x=c(het.low,het.high,het.high,het.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==het2$Biotypes[j])],density=50,angle=45,border='black')
			if (which(pos==pos[j])==2){
				addy = 2
			} else {
				addy = 1			
			}
			ylow = ylow + addy
		}
	} else {
		label.pos = c(label.pos,ylow+1)
		# Observed heterozygosity
		het.low = het2$Min.HO
		het.high = het2$Max.HO
		polygon(x=c(het.low,het.high,het.high,het.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==het2$Biotypes)],border='black')
		ylow = ylow + 1
		# Expected heterozygosity
		het.low = het2$Min.HE
		het.high = het2$Max.HE
		polygon(x=c(het.low,het.high,het.high,het.low),y=c(ylow,ylow,ylow+1,ylow+1),col=cols[which(biotypes==het2$Biotypes)],density=50,angle=45,border='black')
		ylow = ylow + 2
	}
}
text(x=-0.05,y=label.pos,labels=c(studies),cex=1.5,xpd=T,adj=1)
legend(x=0.7,y=42,legend=c('MEAM 1','MED','IO','SSA 1'),pch=22,pt.bg=c('black','grey90','lightgoldenrod','cornflowerblue'),cex=2,bty='n',xpd=T)
legend(x=0.71,y=25,legend=c('',''),pt.bg='white',cex=1.2,bty='n',density=c(50,0),angle=c(45,0),xpd=T,y.intersp=1.5)
