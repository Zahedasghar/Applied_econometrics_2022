library(WDI)
library(tidyverse)
library(tseries)
#cons<-WDI(indicator="NE.CON.PRVT.PC.KD",start=1960, end=2020)
#cons|>filter(country=="Pakistan")

age0_14 <- WDI(indicator = "SP.POP.0014.TO.ZS", start = 2019, end = 2019) # proportion of 0-14 year olds 

#pak_veit<-age0_14|>filter(country=="Pakistan"|country=="Vietnam"|country=="Bangladesh")

age0_14|>glimpse()
pol_stab <- WDI(indicator = "PV.EST", start = 2019, end = 2019) # political stability indicator
pol_stab|>glimpse()
pol_stab|>filter(country=="Pakistan")
#View(age0_14)
#View(pol_stab)
#youth1<-inner_join(age0_14,pol_stab)|>filter(country=="Pakistan")

youth <- merge(age0_14, pol_stab) |> na.omit()
youth<-youth|>select(country, SP.POP.0014.TO.ZS, PV.EST)
#View(youth)
colnames(youth) <- c("country", "age0_14", "pol_stab")
df2<-youth|>filter(country%in% c("Pakistan","India", "Afghanistan","Bangladesh","Sri Lanka"))
youth |> summary()

youth %>% arrange(pol_stab) %>% top_n(-10)

youth %>% arrange(desc(pol_stab)) %>% top_n(-10)

youth %>% mutate(rank = dense_rank(desc(pol_stab))) %>% arrange(rank) %>% filter(country%in% c("Pakistan","India", "Afghanistan","Bangladesh","Sri Lanka"))

youth %>% arrange(desc(age0_14)) %>% top_n(-10)
youth[order(data$pol_stab), ] |> head(10)
#View(data)
ggplot(data=youth)+aes(x=age0_14,y=pol_stab)+geom_point()+geom_smooth(method = "lm",se=FALSE)+
  geom_point(aes(x =35.05438 , y = -2.265187,colour="red",size=2))+
  labs(x="Proportion of young people (in %)",y="Political stability",title = "Youth bulge and political stability", 
       subtitle = "There is close association between youth bulge and political stability. 
       Pakistan has huge youth and high political instability over last two decades", caption="By Zahid Asghar,data:WDI")+
  geom_text( x =35.05438 , y = -2.265187, label = "Pakistan")

ggplot(data=youth)+aes(x=age0_14,y=pol_stab)+geom_point()+geom_smooth(method = "lm",se=FALSE)+
  geom_point(aes(x =35.05438 , y = -2.265187,colour="red",size=2))+ theme_tufte()+
  labs(x="Proportion of young people (in %)",y="Political stability",title = "Youth bulge and political stability", 
       subtitle = "There is close association between youth bulge and political stability. Pakistan has high level of \n political instability and huge population under 14 ", caption="By Zahid Asghar,data:WDI")+
  geom_text( x =35.05438 , y = -2.265187, label = "Pakistan")
ggplot(data = youth)+aes(x=age0_14,y=pol_stab)+geom_point()+geom_smooth(method = "lm",se=FALSE)+
  geom_point(aes(x =42.47227 , y = -2.655531,colour="red",size=2))+
  labs(x="Proportion of young people (in %)",y="Political stability")+
  geom_text( x =42.47227 , y = -2.655531, label = "Afghanistan")

g<-ggplot(data=youth)+aes(x=age0_14,y=pol_stab)+geom_point()+geom_smooth(method = "lm",se=FALSE)
 
df2
library(ggthemes)
g+geom_point(data=youth,aes(x=age0_14 , y =pol_stab))+
  geom_text(data = df2,label=df2$country,colour="red",size=3)+
  theme_tufte()+
  labs(x="Proportion of young people (in %)",y="Political stability",title = "Youth bulge and political stability in 2019", 
       subtitle = "There is close association between youth bulge and political stability. 
       Pakistan had high level of political instability even in 2019 ", caption="By Zahid Asghar,data:WDI")
plot(data$age0_14, data$pol_stab, xlab = "Proportion of young people (in %)", ylab = "Political stability", main = "Youth bulge")
lm(pol_stab ~ age0_14, data = data) |> abline(col = "blue", lwd = 3)
country <- "Pakistan"
points(data$age0_14[data$country == country], data$pol_stab[data$country == country], col = "red", lwd = 8)
text(data$age0_14[data$country == country], data$pol_stab[data$country == country], labels = country, pos = 4)

#install.packages("OneR")
library(OneR)
youth$pol_stab_bin <- cut(youth$pol_stab, breaks = c(-Inf, 0, Inf), labels = c("unstable", "stable"))
optbin(pol_stab_bin ~ age0_14, data = youth, method = "infogain") |> OneR() |> summary()




n=5
for(i in 1:10){
  if(i==n) next else print('x')
}
library(lubridate)
library(tsibble)
pak<-as_tibble(pak)
pak
pak1<-pak|>rename(youth_per="SP.POP.0014.TO.ZS")
pak1$year<-as_date(pak1$year)
pak|>glimpse()
ggplot(pak1)+aes(x=year,y=youth_per)+geom_line()
pak_veit<-pak_veit |> rename(youth_per="SP.POP.0014.TO.ZS")
ggplot(pak_veit)+aes(x=year,y=youth_per,color=country)+geom_line()
