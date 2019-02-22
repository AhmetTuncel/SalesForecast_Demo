library(readxl)

getwd()
setwd("C:/Users/DILEK.DOGAN/Desktop/Masaüstü20190116/Udemy/Polynomial_Regression/")
dat<-read_excel("Dat.xlsx")


ggplot()+
  geom_point(data=dat,aes(x=x,y=y))

lm(y~x, data=dat)


f<-function(x){
  return(34.04*x-65.27)
}

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)


head(dat)
library(data.table)
data<-data.table(dat)
x<-dat$x
y<-f(x)
means<-data.frame(x,y)


ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)


dat$group<-1:100
head(dat)
means$group<-1:100
head(means)
groups<-rbind(dat,means)
groups


ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)+
  geom_line(data=groups,aes(x=x,y=y,group=group))

#residual sum of square
sum((dat$y-means$y)^2)

#####################

lm(y~x+I(x^2),data=dat)

f<-function(x){
  return(2.9522*x^2+0.9719*x-0.5685)
}

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)#+
#geom_point(data=means,aes(x=x,y=y),color='red',size=3)+
#geom_line(data=groups,aes(x=x,y=y,group=group))

head(means)

means$y<-f(means$x)

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)
#geom_line(data=groups,aes(x=x,y=y,group=group))

groups<-rbind(dat,means)

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)+
  geom_line(data=groups,aes(x=x,y=y,group=group))


sum((dat$y-means$y)^2)


######################
m<-lm(y~x+I(x^2)+I(x^3),data=dat)


f<- function(x) {
  return(m$coefficients[4]*x^3+m$coefficients[3]*x^2+m$coefficients[2]*x+m$coefficients[1])
}

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)
#geom_point(data=means,aes(x=x,y=y),color='red',size=3)+
#geom_line(data=groups,aes(x=x,y=y,group=group))

head(means)

means$y<-f(means$x)

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)
#geom_line(data=groups,aes(x=x,y=y,group=group))


groups<-rbind(dat,means)

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-5,15)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)+
  geom_line(data=groups,aes(x=x,y=y,group=group))

sum((dat$y-means$y)^2)

###############dogrusal olmayan özellikler verir modele kubik############################

fit<-smooth.spline(dat$x,dat$y,df=50)
predict(fit,5)$y
predict(fit,10)$y


f<-function(x){
  return(predict(fit,x)$y)
}


ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-2,14)),aes(x=x),fun=f)


means$y<-predict(fit,means$x)$y
head(means)

ggplot()+
  geom_point(data=dat,aes(x=x,y=y))+
  stat_function(data = data.frame(x=c(-2,14)),aes(x=x),fun=f)+
  geom_point(data=means,aes(x=x,y=y),color='red',size=3)
#geom_line(data=groups,aes(x=x,y=y,group=group))

sum((dat$y-means$y)^2)
