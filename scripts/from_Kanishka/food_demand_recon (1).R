library(ambrosia)
library(dplyr)
library(tidyr)

###FIRST GET ALL GCAM variables

#This is GDP per capita at PPP in GCAM
Y=0.614396

#Price of staples in GCAM
PS=0.0393309

#Price of non-staples in GCAM
PN=0.358485
#Sub-regional population in GCAM
pop=14559.3
#Income share
inc_share= 0.015608713

#Convert this back to Y for the income group
Y=(0.614396*14559.3*10*0.015608713)/(14559.3)

#Bias in GCAM converted back to Pcal/year
bias_QS <- 0.482247* 1e-9 * 365*1000*pop
bias_QN <- 0.343417* 1e-9 * 365*1000*pop

###AMBROSIA variables below
param_vector <- c(1.123,1.2972,-6.7e-05,-0.003825,-0.0805,0.44300028,0.067210179,16,5.068,100,20.1)


food_dem <- food.dmnd(Ps=PS, Pn=PN,Y=Y, params = vec2param(param_vector))

Qs_calc <- food_dem$Qs
Qn_calc <- food_dem$Qn



Qs_new <- Qs_calc
Qn_new <- Qn_calc


###RECONCILE GCAM and Ambrosia
Qs_Pcal <- (Qs_new* 1e-9 * 365*1000*pop)+bias_QS
Qn_Pcal <- (Qn_new* 1e-9 * 365*1000*pop)+bias_QN

Qs_pcap <- (Qs_Pcal/(pop * 1e-9 * 365*1000))
Qn_pcap <- (Qn_Pcal/(pop * 1e-9 * 365*1000))


#Verify below from MI
print(paste0("Qs by ambrosia is ",Qs_pcap))
print(paste0("Qn by ambrosia is ",Qn_pcap))

