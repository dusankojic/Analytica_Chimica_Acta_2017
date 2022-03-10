cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/Opt_MQ.RData')
setwd('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/')
outname <- 'Opt_MQ'

#### AVERAGE BACKGROUND CONSECUTIVES ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/Opt_MQ_Backg.RData')
setwd('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/')


k <- 1; en <- c(15:24); en <- c(15,17:20,22:24); le <- length(en)
data_rawBck_avg <- data.frame(matrix(0,nrow=20,ncol=ncol(data_rawBck)),row.names = NULL)
colnames(data_rawBck_avg) <- colnames(data_rawBck)
for (i in 1:le){
  i_ <- which(data_rawBck$ExpN == 14 + i)
  if (length(i_) > 20){
    data_rawBck_avg[k,1:2074] <- colMeans(data_rawBck[i_[1:10],1:2074])
    data_rawBck_avg[k,-c(1:2074)] <- data_rawBck[i_[1],-c(1:2074)]
    k <- k + 1

    data_rawBck_avg[k,1:2074] <- colMeans(data_rawBck[i_[(length(i_)-9):length(i_)],1:2074])
    data_rawBck_avg[k,-c(1:2074)] <- data_rawBck[i_[1],-c(1:2074)]
    k <- k + 1
  }else {
    data_rawBck_avg[k,1:2074] <- colMeans(data_rawBck[i_[1:10],1:2074])
    data_rawBck_avg[k,-c(1:2074)] <- data_rawBck[i_[1],-c(1:2074)]
    k <- k + 1
  }
}
rm(i,i_,k)
dev.off()
par(mfrow=c(2,3),mar=c(2,2,1.5,.5),oma=c(2,1,2,0))
matplot(w,t(data_rawBck_avg[,1:2074]),type='l',lty=1)
rect(w[760],.03,w[890],.05)
rect(w[1180],.09,w[1360],.12)
rect(w[1650],.18,w[1840],.2)
mtext('Raw spectra',side=3,line=-1.5,at=1200,cex=.85)

matplot(w,apply(data_rawBck[,1:2074],2,sd),type='l',lty=1)
rect(w[760],0,w[890],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1180],0,w[1360],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1650],0,w[1840],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
mtext('SD: raw',side=3,line=-1.5,at=1200,cex=.85)
mtext(paste('Sum:',round(sum(apply(data_rawBck[,1:2074],2,sd)),2)),side=3,line=-2.6,at=1200,cex=.85)

matplot(w,apply(data_rawBck_avg[,1:2074],2,sd),type='l',lty=1)
rect(w[760],0,w[890],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1180],0,w[1360],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1650],0,w[1840],.002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
mtext('SD: averaged',side=3,line=-1.5,at=1300,cex=.85)
mtext(paste('Sum:',round(sum(apply(data_rawBck_avg[,1:2074],2,sd)),2)),side=3,line=-2.6,at=1200,cex=.85)

xcld_vap_bands <- setdiff(c(1:2074),c(786:886,1180:1360,1650:1840))
matplot(w[xcld_vap_bands],t(data_rawBck[,xcld_vap_bands]),type='l',lty=1)
rect(w[760],.03,w[890],.05,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1180],.09,w[1360],.12,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1650],.18,w[1840],.2,border=NA,col=rgb(.4,.4,.4,alpha=.2))
mtext('No vapour',side=3,line=-1.5,at=1300,cex=.85)

matplot(w[xcld_vap_bands],apply(data_rawBck[,xcld_vap_bands],2,sd),type='l',lty=1)
rect(w[760],0,w[890],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1180],0,w[1360],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1650],0,w[1840],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
mtext('SD: raw',side=3,line=-1.5,at=1200,cex=.85)
mtext(paste('Sum:',round(sum(apply(data_rawBck[,xcld_vap_bands],2,sd)),2)),side=3,line=-2.6,at=1200,cex=.85)

matplot(w[xcld_vap_bands],apply(data_rawBck_avg[,xcld_vap_bands],2,sd),type='l',lty=1)
rect(w[760],0,w[890],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1180],0,w[1360],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
rect(w[1650],0,w[1840],.0002,border=NA,col=rgb(.4,.4,.4,alpha=.2))
mtext('SD: averaged',side=3,line=-1.5,at=1300,cex=.85)
mtext(paste('Sum:',round(sum(apply(data_rawBck_avg[,xcld_vap_bands],2,sd)),2)),side=3,line=-2.6,at=1200,cex=.85)

mtext('Comparison of standard devations (SD)',side=3,line=0,cex=.9,outer=TRUE)
mtext('Wavelengths / nm',side=1,line=.7,cex=.9,outer=TRUE)

#### SUBTRACT EXP_OPT ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/Opt_MQ.RData')
setwd('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/')
library(prospectr)

c(15:24); en <- c(14,15,17:20,22:24); le <- length(en)
d_ <- data_snv[which(is.element(data_snv$ExpN,en)),]

data_subE <- data.frame(matrix(0,nrow(d_),ncol(d_)),row.names=NULL)
colnames(data_subE) <- colnames(d_)
data_subE$sameMQ <- rep(0,each=nrow(d_))
i_nQ <- which(d_$Solvent!='MilliQ')
E <- unique(d_[i_nQ,'ExpN'])
C <- unique(d_[i_nQ,'Conc'])
k <- 1; cat(C,'\t'); cat(E)
iQ  <- which(d_$Solvent=='MilliQ')
matxQ <- d_[iQ,1:944]
w_ <- w[intW]; w1 <- which(w_ >= 1200)[1]; w2 <- which(w_ >= 1800)[1]; sq_ <- seq(w1,w2,3)
for (i in 1:length(C)){
  temp_ <- c()
  for (ii in 1:length(E)){
    isub  <- which((d_$Conc==C[[i]])&(d_$Solute=='Gly')&(d_$ExpN==E[[ii]]))
    if (length(isub)!=0){
      nrw <- nrow(matxQ)
      nrs_ <- length(isub)
      a1 <- d_[rep(isub,each=nrw),sq_]
      a2 <- as.matrix(matxQ[rep(1:nrw,times=nrs_),sq_])
      difS <- a1 - a2
      min_ <- sort(apply(abs(difS),1,sum),decreasing=F,index.return=T)$ix
      temp_ <- rbind(temp_,difS[min_[1],])
    }
  }

  if (nrow(temp_)>1){
    mx <- apply(temp_,2,median)
    mx_ <- matrix(rep(mx,each=100),nrow=100,ncol=length(sq_))
    for (ii in 1:length(E)){
      isub  <- which((d_$ExpN==E[[ii]])&(d_$Solute=='Gly')&(d_$Conc==C[[i]]))
      if (length(isub)>0){
        cat(paste0('E',E[[ii]],' ',C[[i]],'mM','\t'))
        nrw <- nrow(matxQ)
        nrs_ <- length(isub)
        a1 <- d_[rep(isub,each=nrw),sq_]
        a2 <- as.matrix(matxQ[rep(1:nrw,times=nrs_),sq_])
        difS <- a1 - a2
        min_ <- sort(apply(abs(difS),1,sum),decreasing=F,index.return=T)$ix

        difS_ <- difS[min_[1:100],] - mx_
        min_1 <- sort(apply(abs(difS_),1,sum),decreasing=F,index.return=T)$ix

        # ABS IND
        ism <- ceiling(min_[min_1[1]]/nrw)
        iw <- min_[min_1[1]]%%nrw
        if (iw==0){iw <- nrw}
        data_subE[k,1:944] <- as.matrix(d_[isub[ism],1:944]) - matxQ[iw,1:944]
        data_subE[k,-c(1:944)] <- c(d_[isub[ism],-c(1:944)],iw)
        cat(rownames(d_[isub,])[ism],'\n')
        rownames(data_subE)[k] <- rownames(d_[isub,])[ism]
        k <- k + 1
      }
    }
  }
}
data_subE <- data_subE[-which(apply(data_subE[,1:944],1,sum)==0),]
wnd <- 25; dS <- data_subE[,1:944]; l <- ncol(dS)
x <- savitzkyGolay(dS[1,],p=3,w=wnd,m=0); z <- (l-length(x))/2
for (i in 1:nrow(dS)){
  pad0 <- rev(dS[i,1:z])
  pad1 <- rev(dS[i,(ncol(dS)-z+1):ncol(dS)])
  xS <- as.numeric(c(pad0,dS[i,],pad1))
  dS[i,] <- savitzkyGolay(xS,p=3,w=wnd,m=0)
}
data_subE[,1:944] <- dS

dev.off()
i_ <- which(data_subE$Conc==15)
matplot(w[intW],t(data_subE[i_,1:944]),type='l',lty=1,lwd=c(2,rep(1,length(i_)-1)))
rm(difS,dS,pad0,pad1,x,C,E,i,i_nQ,ii,iQ,isub,k,l,min_,nrs_,nrw,wnd,xS,z,a1,a2,matxQ)
#

#### SUBTRACT DB GLY ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/Opt_MQ.RData')
setwd('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/')
library(prospectr)

# IMPORT DATABASE
load('/Users/YakuriNIR/Documents/Rcode/waterdb_old.RData')
waterSNV <- t(scale(t(water_DB),center=T,scale=T))

en <- c(15,17:20,22:24)
d_ <- data_snv[which((is.element(data_snv$ExpN,en)) & (data_snv$Solute=='Gly')),]
i_nQ <- which(d_$Solvent!='MilliQ')
E <- unique(d_[i_nQ,'ExpN']); C <- unique(d_[i_nQ,'Conc'])

data_subEdb <- data.frame(matrix(0,nrow(d_),ncol(d_)),row.names = NULL)
colnames(data_subEdb) <- colnames(d_)
data_subEdb$sameMQ <- rep(0,each=nrow(d_))
k <- 1; cat(C); cat(E); w_ <- w[intW]
w1 <- which(w_ >= 1200)[1]; w2 <- which(w_ >= 1800)[1]
seq_ <- seq(w1,w2,3)
sq <- seq(1,100,2) # samo svaki drugi uzastopni da skratim vreme
#dev.off()
#par(mfrow=c(2,5),mar=c(2,2,1,1),oma=c(2,2,0,0))
wsnv <- waterSNV[,seq_]
nrw <- nrow(wsnv)
for (i in 1:length(C)){
  temp_ <- c()
  for (ii in 1:length(E)){
    isub  <- which((d_$ExpN==E[[ii]])&(d_$Solute=='Gly')&(d_$Conc==C[[i]]) & (is.element(d_$Rep,sq)))
    if (length(isub)!=0){
      nrs_ <- length(isub)
      samp_ <- d_[isub,seq_]
      a1 <- samp_[rep(1:nrow(samp_),each=nrw),]
      a2 <- as.matrix(wsnv[rep(1:nrw,times=nrs_),])
      difS <- a1 - a2
      min_ <- sort(apply(abs(difS),1,sum),decreasing=F,index.return=T)$ix
      temp_ <- rbind(temp_,difS[min_[1],])
    }
  }

  if (length(isub)!=0){
    mx <- apply(temp_,2,median)
    mx_ <- matrix(rep(mx,each=100),nrow=100,ncol=length(seq_))
    for (ii in 1:length(E)){
      isub  <- which((d_$ExpN==E[[ii]])&(d_$Solute=='Gly')&(d_$Conc==C[[i]])&(is.element(d_$Rep,sq)))
      cat(paste0('E',E[[ii]],' Gly ',C[[i]],' ','\t'))
      nrs_ <- length(isub)
      samp_ <- d_[isub,seq_]
      a1 <- samp_[rep(1:nrow(samp_),each=nrw),]
      a2 <- as.matrix(wsnv[rep(1:nrw,times=nrs_),])
      difS <- a1 - a2
      min_ <- sort(apply(abs(difS),1,sum),decreasing=F,index.return=T)$ix

      difS_ <- difS[min_[1:100],] - mx_
      min_1 <- sort(apply(abs(difS_),1,sum),decreasing=F,index.return=T)$ix

      # ABS IND
      ism <- ceiling(min_[min_1[1]]/nrw)
      iw <- min_[min_1[1]]%%nrw
      if (iw==0){iw <- nrw}
      data_subEdb[k,1:944] <- as.matrix(d_[isub[ism],1:944]) - waterSNV[iw,1:944]
      data_subEdb[k,-c(1:944)] <- c(d_[isub[ism],-c(1:944)],iw)
      rownames(data_subEdb)[k] <- rownames(d_)[isub[ism]]
      k <- k + 1
    }
  }
}
data_subEdb <- data_subEdb[-which(apply(data_subEdb[,1:944],1,sum)==0),]
# SMOOTH
wnd <- 25; dS <- data_subEdb[,1:944]; l <- ncol(dS)
x <- savitzkyGolay(dS[1,],p=3,w=wnd,m=0); z <- (l-length(x))/2
for (i in 1:nrow(dS)){
  if (data_subEdb[i,'Conc'] <= 125){
    pad0 <- rev(dS[i,1:z])
    pad1 <- rev(dS[i,(ncol(dS)-z+1):ncol(dS)])
    xS <- as.numeric(c(pad0,dS[i,],pad1))
    dS[i,] <- savitzkyGolay(xS,p=3,w=wnd,m=0)
  }
}
data_subEdb[,1:944] <- dS
rm(difS,dS,pad0,pad1,x,i,i_nQ,ii,isub,k,l,min_,nrs_,nrw,wnd,xS,z,a1,a2)

#dev.off()
#par(mfrow=c(2,4),mar=c(2,2,1,1),oma=c(2,2,0,0))
#for (i in 1:length(C)){matplot(w[intW],t(data_subEdb[which(data_subEdb$Conc==C[[i]]),1:944]),type='l',lty=1); abline(h=0)}
#

#### AVERAGE GLYCINE REP, EXP ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/Opt_MQ.RData')
setwd('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/')

d_ <- data_snv; en <- c(15:24); en <- c(14,15,17:20,22:24); le <- length(en)
d_ <- d_[which(is.element(data_snv$ExpN,en)),]
C <- unique(d_$Conc); C <- C[2:length(C)]

# AVG_REP
data_avg_rep <- data.frame(matrix(0,10000,ncol(d_)),row.names=NULL)
colnames(data_avg_rep) <- colnames(d_)
# AVG_EXP
data_avg_exp <- data.frame(matrix(0,10000,ncol(d_)),row.names=NULL)
colnames(data_avg_exp) <- colnames(d_)
data_avg_exp_15 <- data_avg_exp
data_avg_exp_610 <- data_avg_exp
l <- 1; rseq <- seq(1,10*le,10); m <- 1
rsq15 <- c(1:5,11:15,21:25,31:35,41:45,51:55,61:65,71:75,81:85,91:95)
rsq610 <- c(6:10,16:20,26:30,36:40,46:50,56:60,66:70,76:80,86:90,96:100)
m15 <- 1; m610 <- 1
for (i in 1:le){
  for (j in 1:length(C)){
    for (k in 1:le){
      ijk <- which((d_$ExpN == en[[i]]) & (d_$Conc == C[[j]]) & (is.element(d_$Rep,rseq[k]:(rseq[k]+9))))
      if (length(ijk) > 0){
        data_avg_rep[l,c(1:944)] <- colMeans(d_[ijk,c(1:944)])
        data_avg_rep[l,-c(1:944)] <- d_[ijk[1],-c(1:944)]
        rownames(data_avg_rep)[l] <- rownames(d_)[ijk[1]]
        l <- l + 1
      }
    }
    ij <- which((d_$ExpN == en[[i]]) & (d_$Conc == C[[j]]))
    ij15 <- which((d_$ExpN == en[[i]]) & (d_$Conc == C[[j]]) & (is.element(d_$Rep,rsq15)))
    ij610 <- which((d_$ExpN == en[[i]]) & (d_$Conc == C[[j]]) & (is.element(d_$Rep,rsq610)))
    if (length(ij) > 0){
      data_avg_exp[m,c(1:944)] <- colMeans(d_[ij,c(1:944)])
      data_avg_exp[m,-c(1:944)] <- d_[ij[1],-c(1:944)]
      rownames(data_avg_exp)[m] <- rownames(d_)[ij[1]]
      m <- m + 1
    }
    if (length(ij15) > 0){
      data_avg_exp_15[m15,c(1:944)] <- colMeans(d_[ij15,c(1:944)])
      data_avg_exp_15[m15,-c(1:944)] <- d_[ij15[1],-c(1:944)]
      rownames(data_avg_exp_15)[m15] <- rownames(d_)[ij15[1]]
      m15 <- m15 + 1
    }
    if (length(ij610) > 0){
      data_avg_exp_610[m610,c(1:944)] <- colMeans(d_[ij610,c(1:944)])
      data_avg_exp_610[m610,-c(1:944)] <- d_[ij610[1],-c(1:944)]
      rownames(data_avg_exp_610)[m610] <- rownames(d_)[ij610[1]]
      m610 <- m610 + 1
    }
  }
}
i <- which(apply(data_avg_rep[,1:944],1,sum)==0)
data_avg_rep <- data_avg_rep[-i,]
i <- which(apply(data_avg_exp[,1:944],1,sum)==0)
data_avg_exp <- data_avg_exp[-i,]
i <- which(apply(data_avg_exp_15[,1:944],1,sum)==0)
data_avg_exp_15 <- data_avg_exp_15[-i,]
i <- which(apply(data_avg_exp_610[,1:944],1,sum)==0)
data_avg_exp_610 <- data_avg_exp_610[-i,]
#

#### SUBTRACT GLYCINE ALL AVG MILLIQ ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/Opt_MQ.RData')
setwd('/Users/YakuriNIR/Documents/Rcode/Opt_MQ/')

en <- c(15:24); en <- c(15,17:20,22:24); le <- length(en)
dQ <- data_snv[which((is.element(data_snv$ExpN,en)) & (data_snv$Solvent == 'MilliQ')),]
dQ <- colMeans(dQ[,1:944])

data_sub_avg_exp <- data_avg_exp[,1:944] - matrix(rep(dQ,each=nrow(data_avg_exp)),nrow=nrow(data_avg_exp),ncol=944)
#

#### FIG 2: BACKGROUND - RAW/AVG ####

cat(rep('\n',50)); rm(list = ls()) # clear workspace
load('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/Opt_MQ_Backg.RData')
setwd('/Users/kojicdusan/Documents/Rcode/proj_optim_mq/')

axis_ <- function(x_,y_,pan_,pow_,rnd_){
  xL <- range(x_); sx <- round(seq(xL[1],xL[2],length.out=5),0)
  yL <- range(y_); sy <- seq(yL[1],yL[2],length.out=5)
  t_ <- 2.01*sx[1]-sx[2]
  axis(side=1,at=sx,labels=NA,tck=-.02,line=-.1)
  axis(side=1,at=sx,labels=sx,lwd=0,cex.axis=cx,line=-.9,hadj=.6)
  axis(side=2,at=sy,labels=NA,tck=-.02,line=.2)
  axis(side=2,at=sy,labels=round(sy*10^pow_,rnd_),lwd=0,cex.axis=cx,line=-.4,las=2)
  mtext(pan_,side=3,line=.1,at=t_,cex=cx+.1,font=2)
}
d_ <- data_rawBck_avg; cx <- .7;  al <- .03; aa <- 9
col_txt <- 'black'; col_sd <- 'gray50'
dev.off()
#tiff('Figure_2.tiff',width=140,height=180,units='mm',res=300,compression = 'lzw')
jpeg('Figure_2.jpeg',width=140,height=170,units='mm',res=300)
par(mfcol=c(5,4),mar=c(1.1,1.1,1.1,1.1),oma=c(2,3,.5,0))

# PANEL A
cut_ <- c(260:450)
x <- w[cut_]
y <- d_[,cut_]; yL <- range(y)
matplot(x,t(y),type='l',lty=1,lwd=.5,axes=FALSE,col=col_txt)
mtext(expression(paste('Abs. (',A%*%10^-2,'/ a.u.)')),side=2.5,line=2,cex=cx)
parm_ <- matrix(0,nrow=nrow(d_),ncol=3)
for (i in 1:nrow(d_)){m <- lm(unlist(d_[i,cut_]) ~ poly(x,2,raw=TRUE)); parm_[i,] <- coef(m)}
parm_ <- colMeans(parm_)
parab_1 <- parm_[1]+.00015+parm_[2]*x+parm_[3]*x^2
par(new=TRUE)
plot(x,parab_1,type='l',lty=3,ylim=yL,axes=FALSE)
mtext('R1',side=3,line=-1,at=w[280],cex=cx-.1)
axis_(x,y,'A',2,1)
# PANEL E
y1 <- -d_[,cut_]+matrix(rep(parab_1,each=nrow(d_)),nrow=nrow(d_)); yL <- range(y1)
matplot(x,t(y1),type='l',lty=1,lwd=.5,ylim=yL,axes=FALSE,xlab='',ylab='',col=col_sd)
mtext(expression(paste('Diff. Abs. (',dA%*%10^-4,'/ a.u.)')),side=2,line=2.5,cex=cx)
axis_(x,y1,'E',4,1)
idx_ <- c(which(x>=905)[1],which(x>=922.3)[1],which(x>=923.5)[1],which(x>=965)[1])
arrows(x[idx_[1]],max(y1[,idx_[1]]),x[idx_[1]],max(y1[,idx_[1]])*1.2,length=al,angle=aa,code=1)
arrows(x[idx_[4]],max(y1[,idx_[4]]),x[idx_[4]],max(y1[,idx_[4]])*1.2,length=al,angle=aa,code=1)
arrows(x[idx_[2]],max(y1[,idx_[2]]),x[idx_[2]],max(y1[,idx_[2]])*1.2,length=al,angle=aa,code=1)
arrows(x[idx_[3]],max(y1[,idx_[2]]),x[idx_[3]],max(y1[,idx_[2]])*1.2,length=al,angle=aa,code=1)
par(new=TRUE)
sd_ <- apply(y1,2,sd)
matplot(x,sd_,type='l',lty=1,lwd=1,col=col_txt,axes=FALSE,xlab='',ylab='')
mtext('R1',side=3,line=-1,at=w[280],cex=cx-.1)
# PANEL I
pca_ <- prcomp(y,center=FALSE,scale=FALSE)
l <- pca_$rotation
plot(x,-l[,1],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
mtext(expression(paste('Loadings (',L%*%10,')')),side=2,line=2.5,cex=cx)
axis_(x,-l[,1],'I',1,2)
mtext('PC1',side=3,line=-1,at=w[280],cex=cx-.1)
# PANEL M
plot(x,l[,2],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
mtext(expression(paste('Loadings (',L%*%10,')')),side=2,line=2.5,cex=cx)
axis_(x,l[,2],'M',1,2)
mtext('PC2',side=3,line=-1,at=w[280],cex=cx-.1)
# PANEL Q
plot(x,l[,3],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
mtext(expression(paste('Loadings (',L%*%10,')')),side=2,line=2.5,cex=cx)
axis_(x,l[,3],'P',1,2)
mtext('PC3',side=3,line=-1,at=w[280],cex=cx-.1)


# PANEL B
cut_ <- c(760:890)
x <- w[cut_]
y <- d_[,cut_]; yL <- range(y)
matplot(x,t(y),type='l',lty=1,lwd=.5,axes=FALSE,col=col_txt)
parm_ <- matrix(0,nrow=nrow(d_),ncol=3)
for (i in 1:nrow(d_)){m <- lm(unlist(d_[i,cut_]) ~ poly(w[cut_],2,raw=TRUE)); parm_[i,] <- coef(m)}
parm_ <- colMeans(parm_)
parab_1 <- .0002+parm_[1]+parm_[2]*x+parm_[3]*x^2
par(new=TRUE)
plot(x,parab_1,type='l',lty=3,ylim=yL,axes=FALSE)
mtext('R2',side=3,line=-1,at=1110,cex=cx-.1)
axis_(x,y,'B',2,1)
# PANEL F
y1 <- -d_[,cut_]+matrix(rep(parab_1,each=nrow(d_)),nrow=nrow(d_)); yL <- range(y1)
matplot(x,t(y1),type='l',lty=1,lwd=.5,ylim=yL,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,y1,'F',4,1)
idx_ <- c(which(x>=1112)[1],which(x>=1131.5)[1],which(x>=1134.8)[1],which(x>=1160)[1])
arrows(x[idx_[1]],max(y1[,idx_[1]]),x[idx_[1]],max(y1[,idx_[1]])*1.2,length=al,angle=aa,code=1)
arrows(x[idx_[2]],max(y1[,idx_[1]]),x[idx_[2]],max(y1[,idx_[1]])*1.2,length=al,angle=aa,code=1)
arrows(x[idx_[3]]-1,max(y1[,idx_[1]])*1.37,x[idx_[2]]-3,max(y1[,idx_[1]])*1.37,length=al,angle=aa,code=1)
arrows(x[idx_[4]],max(y1[,idx_[4]]),x[idx_[4]],max(y1[,idx_[4]])*1.2,length=al,angle=aa,code=1)
par(new=TRUE)
sd_ <- apply(y1,2,sd)
matplot(x,sd_,type='l',lty=1,lwd=1,col=col_txt,axes=FALSE,xlab='',ylab='')
mtext('R2',side=3,line=-1,at=1107,cex=cx-.1)
# PANEL J
pca_ <- prcomp(y,center=FALSE,scale=FALSE)
l <- pca_$rotation
plot(x,-l[,1],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,-l[,1],'J',1,2)
mtext('PC1',side=3,line=-1,at=1107,cex=cx-.1)
# PANEL N
plot(x,l[,2],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,l[,2],'N',1,2)
mtext('PC2',side=3,line=-1,at=1107,cex=cx-.1)
# PANEL R
plot.new()


# PANEL C
cut_ <- c(900:1180,1360:1500)
x <- w[cut_]
y <- d_[,cut_]; yL <- range(y)
parm_ <- matrix(0,nrow=nrow(d_),ncol=2)
for (i in 1:nrow(y)){m <- lm(unlist(y[i,]) ~ x); parm_[i,] <- coef(m)}
parm_ <- colMeans(parm_)
cut_ <- c(1150:1390)
x <- w[cut_]
y <- d_[,cut_]
matplot(x,t(y),type='l',lty=1,lwd=.5,axes=FALSE,col=col_txt)
abline(a=parm_[1]+.001,b=parm_[2],lty=3)
line_ <- .001+parm_[1]+x*parm_[2]
mtext('R3',side=3,line=-1,at=1335,cex=cx-.1)
axis_(x,y,'C',2,1)
# PANEL G
line_ <- parm_[1] + .0012 + x*parm_[2]
y1 <- -d_[,cut_] + matrix(rep(line_,each=nrow(d_)),nrow=nrow(d_)); yL <- range(y1)
matplot(x,t(y1),type='l',lty=1,lwd=.5,ylim=yL,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,y1,'G',4,1)
idx_ <- c(which(x>=1340)[1],which(x>=1362)[1],which(x>=1374)[1],which(x>=1483)[1])
arrows(x[idx_[1]],max(y1[,idx_[1]]),x[idx_[1]],max(y1[,idx_[1]])*3,length=al,angle=aa,code=1)
arrows(x[idx_[2]]-1,max(y1[,idx_[2]])*1.07,x[idx_[2]]-15,max(y1[,idx_[2]])*1.07,length=al,angle=aa,code=1)
arrows(x[idx_[3]],max(y1[,idx_[2]])*.8,x[idx_[3]],max(y1[,idx_[2]])*1.06,length=al,angle=aa,code=1)
arrows(x[idx_[4]],max(y1[,idx_[4]]),x[idx_[4]],max(y1[,idx_[4]])*4,length=al,angle=aa,code=1)
par(new=TRUE)
sd_ <- apply(y1,2,sd)
matplot(x,sd_,type='l',lty=1,lwd=1,col=col_txt,axes=FALSE,xlab='',ylab='')
mtext('R3',side=3,line=-1,at=1335,cex=cx-.1)
# PANEL K
pca_ <- prcomp(d_[,cut_],center=FALSE,scale=FALSE)
l <- pca_$rotation
plot(x,-l[,1],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,-l[,1],'K',1,2)
mtext('PC1',side=3,line=-1,at=1335,cex=cx-.1)
# PANEL O
plot(x,l[,2],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,l[,2],'O',1,2)
mtext('PC2',side=3,line=-1,at=1335,cex=cx-.1)
# PLOT S
plot.new()

# PANEL D
cut_ <- c(1500:1660,1800:1850)
parm_ <- matrix(0,nrow=nrow(d_),ncol=3)
for (i in 1:nrow(d_)){m <- lm(unlist(d_[i,cut_]) ~ poly(w[cut_], 2, raw=TRUE)); parm_[i,] <- coef(m)}
parm_ <- colMeans(parm_)
cut_ <- c(1650:1800)
x <- w[cut_]
y <- d_[,cut_]; yL <- range(y)
matplot(x,t(y),type='l',lty=1,lwd=.5,ylim=yL,axes=FALSE,col=col_txt)
par(new=TRUE)
x <- w[cut_+10]; parab_ <- parm_[1]+.001+parm_[2]*x+parm_[3]*x^2
plot(x,parab_, type='l',lty=3,ylim=yL,axes=FALSE)
mtext('R4',side=3,line=-1,at=1800,cex=cx-.1)
axis_(x,y,'D',2,1)
# PANEL H
cut_ <- c(1500:1660,1800:1850)
parm_ <- matrix(0,nrow=nrow(d_),ncol=3)
for (i in 1:nrow(d_)){m <- lm(unlist(d_[i,cut_]) ~ poly(w[cut_], 2, raw=TRUE)); parm_[i,] <- coef(m)}
parm_ <- colMeans(parm_)
cut_ <- c(1650:1800)
x3 <- w[cut_+10]
parab_ <- parm_[1]+.001+parm_[2]*x3+parm_[3]*x3^2
y3 <- -d_[,cut_] + matrix(rep(parab_,each=nrow(d_)),nrow=nrow(d_)); yL <- range(y3)
matplot(x3,t(y3),type='l',lty=1,lwd=.5,ylim=yL,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x3,y3,'H',4,1)
idx_ <- c(which(x3>=1796)[1],which(x3>=1885)[1],which(x3>=1893)[1],which(x3>=1980)[1])
arrows(x3[idx_[1]],max(y3[,idx_[1]]),x3[idx_[1]],max(y3[,idx_[1]])*3,length=al,angle=aa,code=1)
arrows(x3[idx_[2]]-1,max(y3[,idx_[2]]),x3[idx_[2]]-20,max(y3[,idx_[2]]),length=al,angle=aa,code=1)
arrows(x3[idx_[3]],max(y3[,idx_[2]])*.8,x3[idx_[3]],max(y3[,idx_[2]])*1.02,length=al,angle=aa,code=1)
arrows(x3[idx_[4]],max(y3[,idx_[4]]),x3[idx_[4]],max(y3[,idx_[4]])*3,length=al,angle=aa,code=1)
par(new=TRUE)
sd_ <- apply(y3,2,sd)
matplot(x3,sd_,type='l',lty=1,col=col_txt,axes=FALSE,xlab='',ylab='')
mtext('R4',side=3,line=-1,at=1800,cex=cx-.1)
# PANEL L
pca_ <- prcomp(y,center=FALSE,scale=FALSE)
l <- pca_$rotation
plot(x,-l[,1],type='l',lty=1,axes=FALSE,xlab='',ylab='',col=col_sd)
axis_(x,-l[,1],'L',1,2)
mtext('PC1',side=3,line=-1,at=1800,cex=cx-.1)
# PANEL P
plot.new()
# PANEL T
plot.new()

mtext(expression(paste('Wavelengths (',lambda,' / nm)')),side=1,line=.6,cex=cx,outer=TRUE)
dev.off()
cat(rep('\n',10)); rm(list = ls()) # clear workspace
#
