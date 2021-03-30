library(tidyverse)
library(dslabs)
library(ggplot2)
data(death_prob)
head(death_prob)

prob_death50_F<-death_prob%>%filter(age==50 & sex == "Female")%>%pull(prob)

a<-(-150000)
b<-1150
n<-10#deleted a zero, dont forget to add comments

ev<-a*prob_death50_F+b*(1-prob_death50_F)#added a comment
se<-abs(b-a)*sqrt(prob_death50_F*(1-prob_death50_F))

ev_1000F<-ev*n
se_s1000F<-sqrt(n)*se

prob_lose_moneyF<-pnorm(0,ev_1000F,se_s1000F)


 
prob_death50_M<-death_prob%>%filter(age==50 & sex == "Male")%>%pull(prob)

ev_M<-700000/n
premium<-(ev_M-(a*prob_death50_M))/(1-prob_death50_M)

se_s1000M<-sqrt(n)*abs(premium-a)*sqrt(prob_death50_M*(1-prob_death50_M))

prob_lose_moneyM<-pnorm(0,700000,se_s1000M)

new_prob<-0.015

new_ev1000<-n*((a*new_prob)+b*(1-new_prob))
new_se1000<-abs(b-a)*sqrt(new_prob*(1-new_prob))*sqrt(n)

new_prob_losing_money<-pnorm(0,new_ev1000,new_se1000)
new_prob_losing_more_than_1M<-pnorm(-1000000,new_ev1000,new_se1000)


p<-seq(0.01,0.03,0.001)

lowest<-sapply(p, function(p){
  a<--150000
  b<-1150
  ev<-n*((a*p)+b*(1-p))
  se<-abs(b-a)*sqrt(p*(1-p))*sqrt(n)
  pnorm(0,ev,se)
})
new_data<-data.frame(p,lowest)
new_data%>%filter(lowest>0.9)%>%pull(p)%>%min()


new_p<-seq(0.01,0.03,0.0025)

lowest<-sapply(new_p, function(new_p){
  a<--150000
  b<-1150
  ev<-n*((a*new_p)+b*(1-new_p))
  se<-abs(b-a)*sqrt(new_p*(1-new_p))*sqrt(n)
  pnorm(-1000000,ev,se)
})
new_data<-data.frame(new_p,lowest)
new_data%>%filter(lowest>0.9)%>%pull(new_p)%>%min()


set.seed(25)
p_loss<-0.015
model<-sample(c(1150,-150000),1000,replace=TRUE,prob = c(1-p_loss,p_loss))
sum(model)/10^6


set.seed(27)

B<-10000
l<-1000
p_loss<-0.015
MC<-replicate(B,{
  X<-sample(c(1150,-150000),l,replace=TRUE,prob = c(1-p_loss,p_loss))
  sum(X)
})
pnorm(-1*10^6,mean(MC),sd(MC))

z<-qnorm(0.05)
l<-(-150000)
p<-0.015
n<-1000

x<-(-l)*(((n*p)-z*(sqrt((n*p)*(1-p))))/(n*(1-p)+z*(sqrt((n*p)*(1-p)))))

(l*p+x*(1-p))


set.seed(28)
B<-10000
l<-1000
p<-0.015
MC<-replicate(B,{
  X<-sample(c(x,-150000),l,replace=TRUE,prob = c(1-p,p))
  sum(X)
})

mean(MC<0)
pnorm(0,mean(MC),sd(MC))

set.seed(29)

B<-10000
l<-1000
p<-0.015


MC_plus_minus<-replicate(B,{
  new_p<-p+sample(seq(-0.01,0.01,length=100),1)
  X<-sample(c(x,-150000),l,replace=TRUE,prob = c(1-new_p,new_p))
  sum(X)
})

mean(MC_plus_minus)
mean(MC_plus_minus<0)
mean(MC_plus_minus<(-1*10^6))

take_pull(10)































