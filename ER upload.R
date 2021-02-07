# temp unit transform wh
for (i in 1:dim(wuhan_climate)[1]) {
  wuhan_climate$temp.c[i]=(wuhan_climate$TEMP[i]-32)/1.8
  wuhan_climate$dewp.c[i]=(wuhan_climate$DEWP[i]-32)/1.8
  
}

# relative humidity calculation
for (i in 1:dim(wuhan_climate)[1]) {
  
  tc=wuhan_climate$temp.c[i]
  td=wuhan_climate$dewp.c[i]  
  wuhan_climate$rh[i]=exp( (td*17.27/(td+237.7)) - (tc*17.27/(tc+237.7))     )*100
  
}

write.csv(wuhan_climate,file="./Desktop/wuhan_climate.csv",quote = T,row.names = F, col.names = T)
# Reff estimation
library(EpiEstim)
wh_start=seq(2,length(whdata$NEW.ADD[1:58])-10)
wh_end=au_start+10
wh.reff = estimate_R(whdata[1:58,], 
                       method = "parametric_si",
                       config = make_config
                       (
                         list(mean_si = 7.5, 
                              std_si = 3.4,
                              t_start=wh_start,
                              t_end=wh_end
                         )
                       )
)

wh.reff[["R"]][["Mean(R)"]]
wh.reff[["R"]][["Quantile.0.95(R)"]]
wh.reff[["R"]][["Quantile.0.05(R)"]]

x=c(c(1:58),rev(c(1:58)))
y=c(wh.reff[["R"]][["Quantile.0.05(R)"]],rev(wh.reff[["R"]][["Quantile.0.95(R)"]]))
plot(x,y,type = 'l',xaxt="n",bty='l',col='white',xlab = "Date", ylab = "Mean with 95% CrI",
     ylim = c(0,4), cex.lab=1.3)
polygon(x,y, border = 'white',col='grey')
lines(wh.reff[["R"]],lwd=2,col='red')
abline(h=1,lty=3)
plot(whdata$new.add, type = 'l')
axis(1,at=c(1,28,57),labels=whdata$Date[c(1,28,57)]) 
legend("topright",lty=c(2,1),lwd=2,bty='n',col=c("grey","red"),cex =1.3,
       c("95% CrI","Mean")
)


# choose lag duration
ccf(whfull$reff,ts(wh_climate$ave.temp,start=1,end=58),ylab='cross-cor',main='Cross Correlation Function')
ccf(whfull$reff,ts(wh_climate$ave.rh,start=1,end=58),ylab='cross-cor',main='Cross Correlation Function')

# select df for gam
for (i in 1:3) {
  mod[[i]] = gam(reff~s(ave.temp,k=2+i), family = Gamma(link = log),data = lag7[1:58,])
  par(mfrow = c(2,2))
  gam.check(mod[[i]])
  
}

for (i in 1:3) {
  mod[[i]] = gam(reff~s(ave.temp,k=2+i)+s(ave.rh,k=2+i), family = Gamma(link = log),data = lag7[1:58,])
  par(mfrow = c(2,2))
  gam.check(mod[[i]])
  
}

# plots & results
mod7 = gam(reff~s(ave.temp, k=3), family = Gamma(link = log),data = lag7[1:58,])
par(mfrow = c(2,2))
gam.check(mod7)
summary(mod7)
