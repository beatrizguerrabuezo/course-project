#instalacion
install.packages('TH.data')
breastCancer <- TH.data::GBSG2
str(breastCancer)
library(ggplot2)
library(tidyverse)

#Lectura de datos
head(breastCancer,10)
tail(breastCancer,10)
summary(breastCancer)
head(breastCancer)


#graficos
breastCancer %>%
  ggplot(aes(x = tsize, y =pnodes, color =age)) +
  geom_point(color='red') + 
  theme_bw()

#histogram tsize, color
breastCancer %>%
  ggplot(aes(tsize)) +
  geom_histogram(color = "purple",
                 fill = "blue", 
                 bins = 30) + 
  theme_bw()



#ejercicio 1
#cens (0 = no recurrence, 1 = recurrence)
#horTh (no = hormonal therapy not-received, yes = hormonal therapy received)

Tab_horTh_recur <-  table(breastCancer$horTh, breastCancer$cens)
Tab_horTh_recur

SE_recur_byhorTh <- sqrt(p_recur_byhorTh * (1 - p_recur_byhorTh) / n_byhorTh)


lowerCI_p_recur_byhorTh <- p_recur_byhorTh - qnorm(0.975) * SE_recur_byhorTh

upperCI_p_recur_byhorTh <- p_recur_byhorTh + qnorm(0.975) * SE_recur_byhorTh

cbind(p_recur_byhorTh, lowerCI_p_recur_byhorTh, upperCI_p_recur_byhorTh)

Tab_horTh_recur <-  table(breastCancer$horTh, breastCancer$cens)
Tab_horTh_recur

n_byhorTh <-proporcion #cada vez que obtengo un resutltado lo nombro con <-
p_recur_byhorTh <- Tab_horTh_recur[, "1"] / n_byhorTh
as.matrix(p_recur_byhorTh)

#cuando nombras un resultado, lo mencionas en la ecuacion siguiente
#importante ir paso a paso y nombrar todos los resultados (pintar tabla2x2)

SE_recur_byhorTh <- sqrt(p_recur_byhorTh * (1 - p_recur_byhorTh) / n_byhorTh)
lowerCI_p_recur_byhorTh <- p_recur_byhorTh - qnorm(0.975) * SE_recur_byhorTh

upperCI_p_recur_byhorTh <- p_recur_byhorTh + qnorm(0.975) * SE_recur_byhorTh
cbind(p_recur_byhorTh, lowerCI_p_recur_byhorTh, upperCI_p_recur_byhorTh)

diffp_recur_byhorTh <- p_recur_byhorTh[1] - p_recur_byhorTh[2]
#por ejemplo diffp_recur_byhorTh hace referencia a la resta de los otros valores

SE_diffp_recur_byhorTh <- sqrt(sum(SE_recur_byhorTh ^ 2))

lowerCI_diffp_recur_byhorTh <- diffp_recur_byhorTh - 
  qnorm(0.975) * SE_diffp_recur_byhorTh

upperCI_diffp_recur_byhorTh <- diffp_recur_byhorTh + 
  qnorm(0.975) * SE_diffp_recur_byhorTh

cbind(diffp_recur_byhorTh, lowerCI_diffp_recur_byhorTh,
      upperCI_diffp_recur_byhorTh)

#prop.test
prop.test(cbind(Tab_horTh_recur[, 2], Tab_horTh_recur[, 1]))

#chi2 test
chisq.test(cbind(Tab_horTh_recur[, 2], Tab_horTh_recur[, 1]))
#para calcular los 2 tests tienes que haber hecho las proporciones por separado


#ejercicio 2

# first calculate the values
# RR
recurRelativeRisk <-  p_recur_byhorTh["yes"] / p_recur_byhorTh["no"]
recurRelativeRisk
#Voy a renombrar recurRelativeRisk a RR
RR<-recurRelativeRisk
RR

#(nombramos lo que queremos calcular, hay que hacer TODOS los pasos)

# OR
recurOddsRatio <-  (Tab_horTh_recur["yes", "1"]/ Tab_horTh_recur["yes", "0"]) / 
  (Tab_horTh_recur["no", "1"]/Tab_horTh_recur["no", "0"])
recurOddsRatio 
#nombramos lo que vamos a calcular; xej OR o RR y despues hacemos la operacion
#voy a renombrar recurOddsRatio a OR
OR<-recurOddsRatio
OR

# Confidence intervals
z<-qnorm(0.975)
se_lnRR <-  sqrt(1/Tab_horTh_recur["yes", "1"] - 1/n_byhorTh["yes"] + 
                   1/Tab_horTh_recur["no", "1"] - 1/n_byhorTh["no"])
#Calculo del limite superior
   #1 tengo que nombrar el limite (xej: upperCI_recurRelartiveRisk o UpperCI_RR)
upperCI_recurRelativeRisk <- exp(log(RR)+z*se_lnRR)

  #2 tengo que nombrar el limite inferior (xej: lower...)
lowerCI_recurRelativeRisk <- exp(log(RR)-z*se_lnRR)

#para conseguir la relacion y el resultado tengo que nombrar mis variables (upper, lower)
cbind(recurRelativeRisk,lowerCI_recurRelativeRisk, upperCI_recurRelativeRisk)


#ejercicio 3

#Calculate the rates for recurrence or death and their 95% CIs
#in women with NO  hormonal therapy VS. woman WITH hormonal therapy.


breastCancer$time_years <- breastCancer$time 
#calcular las recurrencias en las mujeres NO hormonal theraphy
recur_byhorTh <- aggregate(cens ~ horTh, data = breastCancer, FUN = sum)
#creamos la varibale:recur_byhorTh y hacemos lo mimso con WITH HT. 

pYr_byhorTh <- aggregate(time_years ~ horTh, data = breastCancer, FUN = sum)
recur_pYr_byhorTh <- merge(recur_byhorTh, pYr_byhorTh, by = "horTh")

#una vez calculas mujeres vs HT vs NoNT, creas la recur_pYr_byhorTh$rate 
recur_pYr_byhorTh$rate <- recur_pYr_byhorTh$cens / recur_pYr_byhorTh$time_years

SE_logrecurRate <- 1 / sqrt(recur_pYr_byhorTh$cens)
recur_pYr_byhorTh$rate_lowerCI <- exp(
  log(recur_pYr_byhorTh$rate) - 1.96 * SE_logrecurRate)
recur_pYr_byhorTh$rate_upperCI <- exp( log(recur_pYr_byhorTh$rate) + 1.96 * SE_logrecurRaterecur_pYr_byhorTh

  #Rate ratio
#Calculate the rate ratio for recurrence or death and its 95% CI
#in women WITH HT vs. women NO hormonal therapy.

 recurRateRatio <- recur_pYr_byhorTh$rate[recur_pYr_byhorTh$horTh == "yes"] /
    recur_pYr_byhorTh$rate[recur_pYr_byhorTh$horTh == "no"]
  
#Creamos recurRateRatio como nueva variable 
 
 SE_logrecurRateRatio <- sqrt(
   1 / recur_pYr_byhorTh$cens[recur_pYr_byhorTh$horTh == "yes"] +
     1 / recur_pYr_byhorTh$cens[recur_pYr_byhorTh$horTh == "no"])

#calculamos los intervalos:
 lowerCI_recurRateRatio <- exp(log(recurRateRatio) - 1.96 * SE_logrecurRateRatio)
 upperCI_recurRateRatio <- exp(log(recurRateRatio) + 1.96 * SE_logrecurRateRatio)
 cbind(recurRateRatio, lowerCI_recurRateRatio, upperCI_recurRateRatio)
 
 