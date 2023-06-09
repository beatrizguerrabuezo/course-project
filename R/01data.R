library(here)
library(tidyverse)
library(gtsummary)
library(unibeCols)
head(ebola)
library(cowplot)
data_insurance <- read.csv(here("data", 'raw', 'insurance_with_date.csv'))

str(data_insurance)

View(data_insurance)
installed.packages('tidyverse')

sex<-c('male','female')

data_insurance <- data_insurance|>mutate (sex_fct=factor(sex))
data_insurance <- data_insurance|>mutate (region_fct=factor(region))
data_insurance <- data_insurance|>mutate (gt2_children=factor(children>2))
data_insurance <- data_insurance|>mutate (smokes=smoker=='yes')
library (RColorBrewer)
library(ggplot2)
library(ggplot.multistats)
library(ggplot2)
ebola <- read_csv("ebola.csv") |> arrange(Date)
install.packages("unibeCols", repos = "https://ctu-bern.r-universe.dev")
library(unibeCols)
ebola2 <- ebola |> filter(Date<as.Date('2015-03-31')&
                        (Country=='Guinea'|Country=='Liberia'|Country=='Sierra Leone'))
Grafico<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+geom_point(aes(group=Country))
Grafico
Grafico<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+geom_point(aes(group=Country))
Grafico<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+geom_point(aes(group=Country))
Grafico
Grafico_1<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+
  geom_col(aes(group=Country))+
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c('orange','blue','purple'),
                      labels = c("Guinea", "Liberia", "Sierra Leone"))

Grafico_2<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+
  geom_line(aes(group=Country))+
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1],unibeApricotS()[3],unibeGreenS()[4]),
                      labels = c("Guinea", "Liberia", "Sierra Leone"))
Grafico_2
Grafico_3<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+
  geom_point(aes(group=Country))+
  scale_colour_manual(name = "Country",
                      breaks=c("Guinea", "Liberia", "Sierra Leone"),
                      values=c(unibeRedS()[1],unibeApricotS()[3],unibeGreenS()[4]),
                      labels=c("Guinea", "Liberia", "Sierra Leone"))+
  scale_x_date(breaks=as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01","2015-04-01")),
               labels=c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits=as.Date(c("2014-08-28", "2015-04-02"))) +
  scale_y_continuous(breaks= seq(from=0, to=15000, by=1500),
                     limits = c(0,15000))+
  theme_bw() + theme(legend.position="bottom")+
  facet_grid(cols = vars(Country))
Grafico_3
Grafico_4<-ggplot(data=ebola2,mapping=aes(x=Date, y=Cum_conf_cases,color=Country))+
  geom_col(aes(group=Country))+
  scale_colour_manual(name = "Country",
                      breaks=c("Guinea", "Liberia", "Sierra Leone"),
                      values=c(unibeRedS()[1],unibeApricotS()[3],unibeGreenS()[4]),
                      labels=c("Guinea", "Liberia", "Sierra Leone"))+
  scale_x_date(breaks=as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01","2015-04-01")),
               labels=c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits=as.Date(c("2014-08-28", "2015-04-02"))) +
  scale_y_continuous(breaks= seq(from=0, to=15000, by=1500),
                     limits = c(0,15000))+
  theme_bw() + theme(legend.position="bottom")+
  facet_grid(cols = vars(Country))
Grafico_4
