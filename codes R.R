###########<getion des risques avec R book############################
normale #
x <- seq(-4,4, length=1000)
hx <- dnorm(x,mean=0, sd=1)
plot(x, hx, type="l", xlab="valeur de x",
     ylab="Densit�", main="loi normale")
-----------------------
  #figure 2 
  fs = function(x,epsilon,delta) dnorm(sinh(delta*asinh(x)-epsilon))*delta*cosh(delta*asinh(x)-epsilon)/sqrt(1+x^2)
vec = seq(-5,5,0.001)
plot(vec,fs(vec,0,1),type="l")
points(vec,fs(vec,-2,1.3),type="l",col="red")
points(vec,fs(vec,1.3,1),type="l",col="blue")

vec = seq(-5,5,0.001)

plot(vec,fs(vec,0,1),type="l",ylim=c(0,0.5), xlab="Valeurs de x",ylab="Densit� de probabilit�")
points(vec,fs(vec,0,1.25),type="l",col="red")
points(vec,fs(vec,0,0.7),type="l",col="blue")
points(vec,fs(vec,1,1),type="l",col="grey")
points(vec,fs(vec,-1,1),type="l",col="darkgoldenrod")
---------------------------------------------------
  #figure 3
  plot(vec,fs(vec,0,1),type="l",ylim=c(0,0.5), xlab="Rendement esp�r�",ylab="Densit� de probabilit�")
points(vec,fs(vec,0.5,1),type="l",col="red")
--------------------------------------------------
  ##########figure 4
  plot(vec,fs(vec,0,1.2),type="l",ylim=c(0,0.5),xlab="", ylab="Densit� de probabilit�", col="red")
points(vec,fs(vec,0,0.9),type="l")
---------------------------------------------
  #calcul de mode median moyenne
  
  ----------------------------------------------
  ######mode median moyenne asymetrie#############
fs = function(x,epsilon,delta) dnorm(sinh(delta*asinh(x)-epsilon))*delta*cosh(delta*asinh(x)-epsilon)/sqrt(1+x^2)
x = seq(-5,5,length=10000)
plot(x,fs(x,0,1),type="l",ylim=c(0,0.5), xlab="Valeurs de x",ylab="Densit� de probabilit�")
syme=density(fs(x,-0.8,-1))
n <- length(syme$y)                       #$
dx <- mean(diff(syme$x))                  # Typical spacing in x $
y.unit <- sum(syme$y) * dx                # Check: this should integrate to 1 $
dx <- dx / y.unit                         # Make a minor adjustment
x.mean <- sum(syme$y * syme$x) * dx
y.mean <- syme$y[length(syme$x[syme$x < x.mean])] #$
x.mode <- syme$x[i.mode <- which.max(syme$y)]
y.mode <- syme$y[i.mode]                  #$
y.cs <- cumsum(syme$y)                    #$
x.med <- syme$x[i.med <- length(y.cs[2*y.cs <= y.cs[n]])] #$
y.med <- syme$y[i.med]                                    #$
#
########## Plot the density and the statistics.
#
plot(syme, xlim=c(-0.2,0.2), type="l", col="green", xlab="valeur de x", main="Asym�trie � gauche")
temp <- mapply(function(x,y,c) lines(c(x,x), c(0,y), col=c), 
               c(x.mean, x.med, x.mode), 
               c(y.mean, y.med, y.mode), 
               c("Blue", "Gray", "Red"))
-----------------------------------------
  ######mode median moyenne symetrie#############
x = seq(-5,5,length=10000)
nor=dnorm(x,0,1)
plot(x,nor,type="l",col="green",main="sym�trique", ylim=c(0,0.4),xlab="Valeurs de x",ylab="Densit� de probabilit�")
abline(v=0, col="red")
################################
#------pourquoi la VaR------------------
x <- seq(-6,1, length=1000)
hx <- dnorm(x,mean=-2, sd=1)
plot(x, hx, type="l", xlab="Perte", ylab="probabilit�")
points(vec,fs(vec,-2,1.3),type="l",col="red")
text(0.3, 0.2, expression("L"[h]))
arrows(0,0,0,0.3)
abline(v=-2,lty='dotted')  
text(-1.9,0.01,~mu)
arrows(0.2,0.2,-0.5,0.15)
text(-4,0.3, expression("L"[h^"'"]))
arrows(-4,0.25,-3.2,0.2)
#---------#tracer and fill under prob---------------------
x <- seq(-4,4, length=1000)
hx <- dnorm(x,mean=0, sd=1)
plot(x, hx, type="l", xlab="valeur de x",
     ylab="Densit�", main="loi normale")
shadenorm = function(below=NULL, above=NULL, pcts = c(0.025,0.975), mu=0, sig=1, numpts = 500, color = "gray", dens = 40,
                     
                     justabove= FALSE, justbelow = FALSE, lines=FALSE,between=NULL,outside=NULL){
  
  
  
  if(is.null(between)){
    
    below = ifelse(is.null(below), qnorm(pcts[1],mu,sig), below)
    
    above = ifelse(is.null(above), qnorm(pcts[2],mu,sig), above)
    
  }
  
  
  
  if(is.null(outside)==FALSE){
    
    below = min(outside)
    
    above = max(outside)
    
  }
  
  lowlim = mu - 4*sig
  
  uplim  = mu + 4*sig
  
  
  
  x.grid = seq(lowlim,uplim, length= numpts)
  
  dens.all = dnorm(x.grid,mean=mu, sd = sig)
  
  if(lines==FALSE){
    
    plot(x.grid, dens.all, type="l", xlab="X", ylab="Density")
    
  }
  
  if(lines==TRUE){
    
    lines(x.grid,dens.all)
    
  }
  
  
  
  if(justabove==FALSE){
    
    x.below    = x.grid[x.grid<below]
    
    dens.below = dens.all[x.grid<below]
    
    polygon(c(x.below,rev(x.below)),c(rep(0,length(x.below)),rev(dens.below)),col=color,density=dens)
    
  }
  
  if(justbelow==FALSE){
    
    x.above    = x.grid[x.grid>above]
    
    dens.above = dens.all[x.grid>above]
    
    polygon(c(x.above,rev(x.above)),c(rep(0,length(x.above)),rev(dens.above)),col=color,density=dens)
    
  }
  
  
  
  if(is.null(between)==FALSE){
    
    from = min(between)
    
    to   = max(between)
    
    
    
    x.between    = x.grid[x.grid>from&x.grid<to]
    
    dens.between = dens.all[x.grid>from&x.grid<to]
    
    polygon(c(x.between,rev(x.between)),c(rep(0,length(x.between)),rev(dens.between)),col=color,density=dens)
    
  }
  
  
  
}

shadenorm(below=1.645, justbelow = TRUE)
text(1.8,0,~alpha[p])
text(0,0.2,~alpha, cex=2)
##############graph de la VaR################
ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm) + 
  stat_function(fun = dnorm, 
                xlim = c(-3,-1),
                geom = "area") + ylab("") +xlab("") +
  geom_text(x=-2.5, y=-0.01, label="Perte")+
  geom_text(x=2.5, y=-0.01, label="Gain")+
  geom_text(x=-1, y=-0.01, label="V")+
  geom_text(x=-2.3, y=0.18, label="(1-x%)")+
  geom_segment(aes(x = -2, y = 0.15, xend = -1.3, yend = 0.08),
               size=0.5,arrow = arrow(type="closed",length = unit(4, "mm")))
#####################2 eme graph VaR##############
library(ggplot2)
ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm) + 
  stat_function(fun = dnorm, 
                xlim = c(1,3),
                geom = "area") + ylab("") +xlab("") +
  geom_text(x=-2.5, y=-0.01, label="Gain")+
  geom_text(x=2.5, y=-0.01, label="Perte")+
  geom_text(x=1, y=-0.01, label="V")+
  geom_text(x=2.3, y=0.18, label="(1-x%)")+
  geom_segment(aes(x = 2, y = 0.15, xend = 1.3, yend = 0.08),
               size=0.5,arrow = arrow(type="closed",length = unit(4, "mm")))
###################Var au seuil P###########
ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm) + 
  stat_function(fun = dnorm, 
                xlim = c(1,3),
                geom = "area", fill="lemonchiffon4") + ylab("") +xlab("") +
  geom_text(x=1.5, y=-0.01, label="VaR(h,P)= P-quantile de L")+
  geom_text(x=1.5, y=0.05, label="(1-p)")+
  geom_text(x=-0.05,y=0.1,label=("P"))+
  ylab("Densit� de probabilit� de la perte")
#############cumulative disdribution#######
ggplot(data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = pnorm) + 
  stat_function(fun = pnorm, 
                xlim = c(1,3))+
  ylab("") + xlab("") +
  geom_text(x=2, y=-0.01, label="VaR(h,P)= P-quantile de L") +
  geom_segment(aes(x=0,xend=1,y=0.84134474,yend=0.84134474),
               linetype="dotdash")+
  geom_segment(aes(x=1,xend=1,y=0,yend=0.84134474),
               linetype="dotdash")+
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1),
               size=0.5,arrow = arrow(type="closed",length = unit(4, "mm")))+
  geom_segment(aes(x=0,xend=3,y=1, yend=1),linetype="dotdash")+
  geom_text(x=-0.25, y=1, label="P")+
  geom_text(aes(label="F[L](x) == Prob(L[h]<=x) ",
                x = -1.7, y =0.93), parse = T, colour = "black", size = 4)
##########call option value##################

library(derivmkts)
s=c(10,20,30,40,50,60,70,80)
veee=as.data.frame(s)
f=function(x)  {
  bscall(x,50, 0.2, 0.05, 0.37, 0)
}
prix_option=apply(veee,1,f)
data=data.frame(s,prix_option)
library(ggplot2)
ggplot(data, aes(x=s, y=prix_option)) + geom_line() +
  xlab("prix de l'action") +
  ylab("prix de l'option")
#############Delta call option###################
s=seq(1,80, by=0.05)
ff=function(x)  {
  greeks2(bscall, list(s=x, k=50, v=0.2, r=0.05, tt=0.37, d=0))['Delta',]
}
greek=apply(veee,1,ff)
gdata=data.frame(s,greek)
ggplot(gdata, aes(x=s, y=greek)) + geom_line() +
  xlab("prix de l'action") +
  ylab("Delta")
###############gamma call option################# 

s=seq(10,80, by=0.05)
fg=function(x)  {
  greeks2(bscall, list(s=x, k=50, v=0.2, r=0.05, tt=0.37, d=0))['Gamma',]
}
greekg=apply(veee,1,fg)
gdata=data.frame(s,greekg)
ggplot(gdata, aes(x=s, y=greekg)) + geom_line() +
  xlab("prix de l'action") +
  ylab("Gamma")
#########################vega call option#################

library(derivmkts)
library(ggplot2)
s=seq(10,80, by=0.05)
fv=function(x)  {
  greeks2(bscall, list(s=x, k=50, v=0.2, r=0.05, tt=0.37, d=0))['Vega',]
}
greekv=apply(veee,1,fv)
gdata=data.frame(s,greekv)
ggplot(gdata, aes(x=s, y=greekv)) + geom_line() +
  xlab("prix de l'action") +
  ylab("Vega")
#########################theta call option#################
library(derivmkts)
library(ggplot2)
s=seq(10,80, by=0.05)
ft=function(x)  {
  greeks2(bscall, list(s=x, k=50, v=0.2, r=0.05, tt=0.37, d=0))['Theta',]
}
greekt=apply(veee,1,ft)
gdata=data.frame(s,greekt)
ggplot(gdata, aes(x=s, y=greekt)) + geom_line() +
  xlab("prix de l'action") +
  ylab("Theta")
##################Monte carlo simulation  function########################
GeometricBrownian<-function()
{
  paths<-10
  count<-5000
  interval<-5/count
  mean<-0.06
  sigma<-0.3
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-100
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
  }	
  
  matplot(sample,main="Geometric Brownian",xlab="Temps",ylab="Trajectoire",type="l")
}
GeometricBrownian()
############################Monte carlo simulation##############

path<-1
count<-5000
interval<-5/count
mean<-0.06
sigma<-0.3
samples<-matrix(0,nrow=(count+1),ncol=path)
for(i in 1:path)
{
  samples[1,i]<-100
  for(j in 2:(count+1))
  {
    samples[j,i]<-samples[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
  }
}	
plot(samples, type="l", xlab="Temps", ylab="Prix de l'action")





:path)
{
  samples[1,i]<-100
  for(j in 2:(count+1))
  {
    samples[j,i]<-samples[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
  }
}	
plot(samples, type="l", xlab="Temps", ylab="Prix de l'action")





")









 l'action")





")









