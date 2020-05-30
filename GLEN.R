

###Glenmore sampling data###
###Scotlad###

setwd("C:\\R_game\\GLEN")

#oh horizon depth
Glen_Oh_d <- read.table("C:\\R_game\\GLEN\\Glen_Oh_depth.txt", header=TRUE,sep="\t")
Glen_Oh_d2 <- aggregate(cbind(Glen_Oh_d$Oh_depth_cm), 
                      by= list(Pit = Glen_Oh_d$Pit),
                      FUN = mean,  na.rm= TRUE)
write.table(Glen_Oh_d2, "Glen_Oh_d2.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)

#litter weight and C, N concentration
Glen_lit <- read.table("C:\\R_game\\GLEN\\Litter_weight.txt", header=TRUE,sep="\t")
Glen_lit2 <- aggregate(cbind(Glen_lit$N, Glen_lit$C, Glen_lit$Weight_g), 
                        by= list(Pit_nr = Glen_lit$Site_nr,
                                 Age = Glen_lit$Age,
                                Name = Glen_lit$Name),
                        FUN = mean,  na.rm= TRUE)

#the square that was used was 25x25 cm, calculate C and N t/ha from weight of C, N in 1cm2

Glen_lit2$N_tha <- NA
Glen_lit2$N_tha <- (Glen_lit2$V1/100)*Glen_lit2$V3/625*100
Glen_lit2$C_tha <- (Glen_lit2$V2/100)*Glen_lit2$V3/625*100
Glen_lit2$Sample.nr. <- NA
Glen_lit3 <- Glen_lit2 [, c( -4, -5, -6 )]
Glen_lit3$Horizon <- "Litter"
Glen_lit3$PSA_nr <- NA
Glen_lit3$pH <- NA
Glen_lit3$Nperc <- NA
Glen_lit3$Cperc <- NA
Glen_lit3$Depth <- NA
Glen_lit3$Stones_observed_perc <- NA
Glen_lit3$Sotnes_measured_perc <- NA
Glen_lit3$Moisture <- NA
Glen_lit3$BD <- NA
Glen_lit3$Clay <- NA
Glen_lit3$Silt <- NA
Glen_lit3$Sand <- NA
Glen_lit3$PSA_Moisture <- NA
Glen_lit3$Horizon_Width <- NA
Glen_lit3$Comments <- NA 
Glen_lit3$marker = paste (Glen_lit3$Pit_nr, Glen_lit3$Horizon, sep = "_" )

#horizon samples, the main dataset
Glen_hor <- read.table("C:\\R_game\\GLEN\\Glen_hor.txt", header=TRUE,sep="\t")
Glen_hor$N_tha <- NA
Glen_hor$C_tha <- NA

#join litter samples to the soil horizon measures
setdiff(Glen_hor, Glen_lit3)
Glen_hor$marker = paste (Glen_hor$Pit_nr, Glen_hor$Horizon, sep = "_" )
Glen_hor <- rbind(Glen_hor, Glen_lit3)

#BD measurements 1 per site
Glen_BD <- read.table("C:\\R_game\\GLEN\\Glen_BD.txt", header=TRUE,sep="\t")
Glen_BD$marker = paste (Glen_BD$Pit_nr, Glen_BD$Horizon, sep = "_" )

#join BD and horizon samples 
Glen_hor$Moisture <- Glen_BD$Moisture [match(Glen_hor$marker,
                                             Glen_BD$marker)]
Glen_hor$BD <- Glen_BD$BD [match(Glen_hor$marker,
                                             Glen_BD$marker)]
Glen_hor$Sotnes_measured_perc <- Glen_BD$Sotnes_measured_perc [match(Glen_hor$marker,
                                             Glen_BD$marker)]

Glen_hor$Sotnes_measured_perc <- Glen_hor$Sotnes_measured_perc/100

Glen_hor$BD <- ifelse(Glen_hor$Horizon =="Ah", "0.51583011", Glen_hor$BD) # only one BD for Ah was measured as it is such a thin horizon
write.table(Glen_hor, "Glen_hor2.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)

#Full horizon measures, with all attributes
Glen_hor3 <- read.table("C:\\R_game\\GLEN\\Glen_hor3.txt", header=TRUE,sep="\t")

#Calculate stocks in t/ha 
Glen_hor3$N_tha <- ifelse (is.na(Glen_hor3$N_tha) , ((Glen_hor3$Nperc/100)*Glen_hor3$BD*(Glen_hor3$Horizon_Width/100)*10000), 
                           Glen_hor3$N_tha)
Glen_hor3$C_tha <- ifelse (is.na(Glen_hor3$C_tha) , ((Glen_hor3$Cperc/100)*Glen_hor3$BD*(Glen_hor3$Horizon_Width/100)*10000), 
                           Glen_hor3$C_tha)

#remove Drum. V.OLD site as it is the only brown earth site in whole forest it cannot be compared
Glen_hor4 <- Glen_hor3 [! (Glen_hor3$Pit_nr == "8"),]
write.table(Glen_hor4, "Glen_hor4.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)

Glen_1per_site <- aggregate(cbind(Glen_hor4$C_tha, Glen_hor4$N_tha),
                                    by = list(Age= Glen_hor4$Age,
                                              Name = Glen_hor4$Name,
                                              Pit_nr = Glen_hor4$Pit_nr),
                                    FUN = sum,  na.rm= TRUE)#returns matrixes
Glen_1per_site<- do.call(data.frame, Glen_1per_site)
Glen_hor4$Age <- ordered (Glen_hor4$Age, levels = c("GRASS", "YNG", "OLD", "V.OLD"))
Glen_hor4$Horizon <- ordered (Glen_hor4$Horizon, levels = c("Litter", "Oh_UPP","Oh_LOW", "Ah_UPP", "Ah_LOW", "Ah", "E", "Bhs"))
boxplot(V1~Age, data = Glen_1per_site, xlab = "Age group", ylab = "C t/ha" )

#peat layer Oh_UPP and LOW seems to be highly dependent on moisture level
#export and invetigate the relationship, determine an expantion function and adjust to one level
Glen_hor_Oh <- Glen_hor4[which(Glen_hor4$Horizon == "Oh_UPP" | Glen_hor4$Horizon == "Oh_LOW"), ]

Glen_moisture_BD <- lm(BD ~ Moisture+Horizon, data=Glen_hor_Oh)
# #look at the p value
summary.aov(Glen_moisture_BD)
summary.lm(Glen_moisture_BD)
Glen_hor_Oh$Moisture_300 <- 300
Glen_hor_Oh_moist <- Glen_hor_Oh
Glen_hor_Oh_moist$Moisture <- 300
Glen_hor_Oh$newBD <-predict(Glen_moisture_BD, Glen_hor_Oh_moist) # predict function uses 40% constant moisture

coef(Glen_moisture_BD)
par(mar=c(5,5,4,2)) # to display superscrp properly we need to push the graph a bit
plot(Glen_hor_Oh$Moisture, Glen_hor_Oh$BD*1000, xlab = "", ylab= "", main="Measured bulk density")
title(ylab=expression(paste('kg/m'^'3')), line=2, cex.lab=1.2)
title(xlab=expression(paste('Moisture %')), line=2, cex.lab=1.2)
res <- residuals(lm(BD~ Moisture+Horizon, data=Glen_hor_Oh, na.action=na.exclude))
is.numeric(Glen_moisture_BD$residuals)
Glen_hor_Oh$residuals_newBD <- NA
Glen_hor_Oh$residuals_newBD <- res
Glen_hor_Oh$newBD2 <- NA
Glen_hor_Oh$newBD2 <- Glen_hor_Oh$newBD + Glen_hor_Oh$residuals_newBD 

plot(Glen_hor_Oh$newBD2, Glen_hor_Oh$Moisture_300)
plot(Glen_hor_Oh$Moisture_300,Glen_hor_Oh$newBD2*1000, xlab = "", ylab= "", main="Modelled bulk density")
title(ylab=expression(paste('kg/m'^'3')), line=2, cex.lab=1.2)
title(xlab=expression(paste('Moisture %')), line=2, cex.lab=1.2)
par(mfrow=c(1,1))
#plot (Glen_moisture_BD)
# library(ggplot2)
# library(ggpmisc) # to get the result of fitted line

#new total C and N tha calculations with new BD for Ah layer
Glen_hor4$newBD2 <- NA
Glen_hor4$newBD2 <- Glen_hor_Oh$newBD2 [match(Glen_hor4$marker, Glen_hor_Oh$marker)]

Glen_hor4$newBD2 <- ifelse (Glen_hor4$Horizon =="Bhs" | Glen_hor4$Horizon =="E" | Glen_hor4$Horizon =="Ah"| Glen_hor4$Horizon =="Ah_LOW"| Glen_hor4$Horizon =="Ah_UPP", 
                            paste(Glen_hor4$BD), 
                            Glen_hor4$newBD2)
# Glen_hor4$newBD2 <- ifelse (Glen_hor4$Horizon =="Litter" , 
#                             "0", Glen_hor4$newBD2)
Glen_hor4$newBD2 <- as.numeric(Glen_hor4$newBD2)
is.numeric(Glen_hor4$newBD2)
Glen_hor4$N_tha_Ohnew <- ifelse (Glen_hor4$Horizon =="Oh_UPP" | Glen_hor4$Horizon =="Oh_LOW" , 
                           ((Glen_hor4$Nperc/100)*Glen_hor4$newBD2 *(Glen_hor4$Horizon_Width/100)*10000), 
                           Glen_hor4$N_tha)
Glen_hor4$C_tha_Ohnew <- ifelse (Glen_hor4$Horizon =="Oh_UPP" | Glen_hor4$Horizon =="Oh_LOW" ,
                                 ((Glen_hor4$Cperc/100)*Glen_hor4$newBD2 *(Glen_hor4$Horizon_Width/100)*10000), 
                           Glen_hor4$C_tha)

Glen_1per_site2 <- aggregate(cbind(Glen_hor4$C_tha_Ohnew, Glen_hor4$N_tha_Ohnew),
                            by = list(Age= Glen_hor4$Age,
                                      Name = Glen_hor4$Name,
                                      Pit_nr = Glen_hor4$Pit_nr),
                            FUN = sum,  na.rm= TRUE)#returns matrixes
Glen_1per_site2 <- do.call(data.frame, Glen_1per_site2)

boxplot(V1~Age, data = Glen_1per_site2, xlab = "Age group", ylab = "C t/ha" )

#sum all Horizon_width per pit and calculate mineral soil depth at each pit

Glen_mineral <-  Glen_hor4[which (!Glen_hor4$Horizon == "Oh_LOW" & !Glen_hor4$Horizon == "Oh_UPP" & !Glen_hor4$Horizon == "Litter" ),]

Glen_min_1persite <- aggregate(cbind(Glen_mineral$C_tha, Glen_mineral$N_tha, Glen_mineral$Horizon_Width),
                             by = list(Age= Glen_mineral$Age,
                                       Name = Glen_mineral$Name,
                                       Pit_nr = Glen_mineral$Pit_nr),
                             FUN = sum,  na.rm= TRUE)#returns matrixes
Glen_min_1persite <- do.call(data.frame, Glen_min_1persite)
boxplot(V1~Age, data = Glen_min_1persite, xlab = "Age group", ylab = "C t/ha" )
write.table(Glen_hor4, "Glen_hor4", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
write.table(Glen_mineral, "Glen_mineral", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)

#import equivalent mass recalculated mineral layer in the Glen_hor4 dataset
Glen_min_eq_mass <- read.table("C:\\R_game\\GLEN\\Glen_miner_eq_mass.txt", header=TRUE,sep="\t")
Glen_min_eq_mass$marker = paste( Glen_min_eq_mass$Pit_nr, Glen_min_eq_mass$Horizon, sep = "_" )
#copy equivalent mass recalculated C and N
Glen_hor4$N_tha_2_eq_mass <- NA
Glen_hor4$C_tha_2_eq_mass <- NA
Glen_hor4$N_tha_2_eq_mass <- Glen_min_eq_mass$N_eq_tha [match(Glen_hor4$marker,
                                                              Glen_min_eq_mass$marker)]
Glen_hor4$C_tha_2_eq_mass <- Glen_min_eq_mass$C_eq_tha [match(Glen_hor4$marker,
                                                              Glen_min_eq_mass$marker)]
Glen_hor4$N_tha_2_eq_mass <- ifelse (Glen_hor4$Horizon =="Oh_UPP" | Glen_hor4$Horizon =="Oh_LOW" |Glen_hor4$Horizon =="Litter" 
                                 | Glen_hor4$Horizon =="Ah_LOW" | Glen_hor4$Horizon =="Ah_UPP", 
                                 paste(Glen_hor4$N_tha_Ohnew), 
                                 Glen_hor4$N_tha_2_eq_mass)
Glen_hor4$N_tha_2_eq_mass <- ifelse (Glen_hor4$Horizon =="Bhs" & Glen_hor4$Age =="GRASS" , 
                                     paste(Glen_hor4$N_tha_Ohnew), 
                                     Glen_hor4$N_tha_2_eq_mass)
Glen_hor4$C_tha_2_eq_mass <- ifelse (Glen_hor4$Horizon =="Oh_UPP" | Glen_hor4$Horizon =="Oh_LOW" |Glen_hor4$Horizon =="Litter" 
                                     | Glen_hor4$Horizon =="Ah_LOW" | Glen_hor4$Horizon =="Ah_UPP", 
                                     paste(Glen_hor4$C_tha_Ohnew), 
                                     Glen_hor4$C_tha_2_eq_mass)

Glen_hor4$C_tha_2_eq_mass <- ifelse (Glen_hor4$Horizon =="Bhs" & Glen_hor4$Age =="GRASS" , 
                                     paste(Glen_hor4$C_tha_Ohnew), 
                                     Glen_hor4$C_tha_2_eq_mass)
Glen_hor4$C_tha_2_eq_mass<- as.numeric(Glen_hor4$C_tha_2_eq_mass)

Glen_hor4$N_tha_2_eq_mass<- as.numeric(Glen_hor4$N_tha_2_eq_mass)
#aggreagte new C t/ha and see how it affected the results
Glen_hor4$Age <- ordered(Glen_hor4$Age, levels = c("GRASS", "YNG", "OLD", "V.OLD"))

#remove litter for equivalent mass dateset it is not a part of soil 
Glen_hor4_no_litter <- Glen_hor4[which (!Glen_hor4$Horizon =='Litter'), ]

Glen_eq_mass_1persite <- aggregate(cbind(Glen_hor4_no_litter$C_tha_2_eq_mass, Glen_hor4_no_litter$N_tha_2_eq_mass, Glen_hor4_no_litter$C_tha),
                               by = list(Age = Glen_hor4_no_litter$Age,
                                         Name = Glen_hor4_no_litter$Name,
                                         Pit_nr = Glen_hor4_no_litter$Pit_nr),
                               FUN = sum,  na.rm= TRUE)#returns matrixes
Glen_eq_mass_1persite <- do.call(data.frame, Glen_eq_mass_1persite)

boxplot(V1~Age, data = Glen_eq_mass_1persite, xlab = "Age group", ylab = "C t/ha" ) # seems to be diferences


#illustrate by horizon, separate mineral and organic horizons
Glen_hor4$Age <- ordered (Glen_hor4$Age, levels = c("GRASS", "YNG", "OLD", "V.OLD"))
Glen_hor4$Horizon <- ordered (Glen_hor4$Horizon, levels = c("Litter", "Oh_UPP","Oh_LOW", "Ah", "Ah_UPP", "Ah_LOW", "E", "Bhs"))

Glen_org <- Glen_hor4[which (Glen_hor4$Horizon == "Oh_UPP"| Glen_hor4$Horizon == "Oh_LOW"),]

Glen_min <- Glen_hor4[which (Glen_hor4$Horizon == "Ah"| Glen_hor4$Horizon == "E"| Glen_hor4$Horizon == "Bhs" & ! Glen_hor4$Age== "GRASS"),]

library(ggplot2)
Glen_org_SOCplot <- ggplot(Glen_org, aes( x=Horizon, y=Glen_hor4_no_litter)) +
  theme_bw()+
  geom_point( data = Glen_org,aes(fill= Age), 
              stat = 'identity',
              colour="black",pch=21,
              position = position_dodge(width = .4),
              size=3)+
  #geom_hline(yintercept=6, linetype="dashed", color = "red")+
  coord_cartesian(ylim = c(0, 310))+
  labs( x=" Horizon", y = expression(paste("SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8)) +
  scale_fill_brewer(palette="OrRd", name= "Age_class",
                    labels = c( 
                               "Young",
                               "Mature",
                               "Ancient"))+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=guide_legend(title="Age class"))

Glen_org_SOCplot

Glen_org_SOCbox <- ggplot(Glen_org, aes( x=Horizon, y=C_tha_2_eq_mass)) +
  theme_bw()+
  geom_boxplot(aes(colour = Age))+

  coord_cartesian(ylim = c(0, 300))+
  labs( x=" Horizon", y = expression(paste("SOC","  ", "t ha"^"-1")))+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8)) 

Glen_org_SOCbox

#mineral soil horizons (no grassland)
Glen_min_SOCplot <- ggplot(Glen_min, aes( x=Horizon, y=C_tha_2_eq_mass)) +
  theme_bw()+
  geom_point( data = Glen_min,aes(fill= Age), 
              stat = 'identity',
              colour="black",pch=21,
              position = position_dodge(width = .4),
              size=3)+
  #geom_hline(yintercept=6, linetype="dashed", color = "red")+
  coord_cartesian(ylim = c(0, 90))+
  labs( x=" Horizon", y = expression(paste("Eqiuvalent mass adjusted SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8)) +
  scale_fill_brewer(palette="OrRd", name= "Age_class",
                    labels = c("Young",
                               "Mature",
                               "Ancient"))+
  scale_y_continuous(expand = c(0, 0))+
  guides(fill=guide_legend(title="Age class"))
Glen_min_SOCplot

Glen_min_SOCbox <- ggplot(Glen_min, aes( x=Horizon, y=C_tha_2_eq_mass)) +
  theme_bw()+
  geom_boxplot(aes(colour = Age))+
  coord_cartesian(ylim = c(0, 100))+
  labs( x=" Horizon", y = expression(paste("SOC","  ", "t ha"^"-1")))+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8)) 

Glen_min_SOCbox

#### Descriptive measurements pH, BD, horizon width ####


#### Mineral horizons' width, pH, BD, Clay analysis####
#### no grassland as only Bhs layer is the same ####

#add LAT and LONG coordinates to Equivalent_mass_1persite
Coordinates <- read.table("C:\\R_game\\Glen\\Glen_coord.txt", header=TRUE,sep="\t")
Glen_min$LAT <- NA
Glen_min$LAT <- Coordinates$LAT[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
Glen_min$LONG <- NA
Glen_min$LONG <- Coordinates$LONG[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
Glen_min$ALT <- NA
Glen_min$ALT <- Coordinates$ALT[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
#jitter is addded to avoid ties as couple of sites are very close to each other 
Glen_min$LAT <- jitter (Glen_min$LAT)
Glen_min$LONG <- jitter (Glen_min$LONG)
Glen_min$ALT <-jitter (Glen_min$ALT)
#order depth to display the right order in the illustrations
is.ordered(Glen_min$Horizon)
# Order age groups
is.ordered(Glen_min$Age)

# make a new column with GrASS and N_GRASS for all the other values
Glen_min$Age_group3 <- NA
Glen_min$Age_group3 <- ifelse (Glen_min$Age == 'GRASS', 'GRASS', 'N_GRASS')

# rename GRASS into V.OLD
Glen_min$Age_group2 <- NA
Glen_min$Age_group2 <- ifelse(Glen_min$Age=='GRASS', 'V.OLD', paste(Glen_min$Age))

is.ordered(Glen_min$Age_group2)
Glen_min$Age_group2 <- ordered(Glen_min$Age_group2, levels = c("YNG", "OLD", "V.OLD"))
Glen_min$Age_group3 <- as.factor(Glen_min$Age_group3)


### Random effects are added to adjust model for sample dependance to each other, because they are taken from the same pit


Glen_min$log_Horizon_Width <- log(Glen_min$Horizon_Width)
library(nlme)
Glen_depth <- lme(log_Horizon_Width ~  Age + Horizon, random = ~1|Pit_nr,
                  correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),
VarCorr(Glen_depth)
summary(Glen_depth)
anova(Glen_depth)
plot(Glen_depth)
Glen_depth_fitted <- fitted(Glen_depth)
plot(Glen_min$log_Horizon_Width, Glen_depth_fitted)
abline(0,1)

Glen_ph_min_lme <- lme(pH ~ Age + Horizon, random = ~1|Pit_nr,
                     correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_ph_min_lme)
summary(Glen_ph_min_lme)
anova(Glen_ph_min_lme)
plot(Glen_ph_min_lme)
Glen_ph_min_lme_fitted <- fitted(Glen_ph_min_lme)
plot(Glen_min$pH, Glen_ph_min_lme_fitted)
abline(0,1)

## Remove Ah layer as it is not fully measured througout the survey 
Glen_min_naAh <- Glen_min [!(Glen_min$Horizon =="Ah"),]
Glen_BD_min <- lme(BD ~ Age + Horizon, random = ~1|Pit_nr,
                     correlation = corSpher(form = ~LAT+LONG),data = Glen_min_naAh)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_BD_min)
summary(Glen_BD_min)
anova(Glen_BD_min)
plot(Glen_BD_min)
Glen_BD_fitted <- fitted(Glen_BD_min)
plot(Glen_min_naAh$BD, Glen_BD_fitted)
abline(0,1)

Glen_min$log_Clay <- log(Glen_min$Clay)
Glen_clay_lme <- lme(log_Clay~Age+Horizon , random = ~1|Pit_nr,
                     correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_clay_lme)
summary(Glen_clay_lme)
anova(Glen_clay_lme)
plot(Glen_clay_lme)
Glen_clay_lme_fitted <- fitted(Glen_clay_lme)
plot(Glen_min$log_Clay, Glen_clay_lme_fitted)
abline(0,1)

### Organic layer statistical ananlysis width, pH & BD ###

#add LAT and LONG coordinates to Equivalent_mass_1persite
Coordinates <- read.table("C:\\R_game\\Glen\\Glen_coord.txt", header=TRUE,sep="\t")
Glen_hor4$LAT <- NA
Glen_hor4$LAT <- Coordinates$LAT[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
Glen_hor4$LONG <- NA
Glen_hor4$LONG <- Coordinates$LONG[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
Glen_hor4$ALT <- NA
Glen_hor4$ALT <- Coordinates$ALT[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
#jitter is addded to avoid ties as couple of sites are very close to each other 
Glen_hor4$LAT <- jitter (Glen_hor4$LAT)
Glen_hor4$LONG <- jitter (Glen_hor4$LONG)
Glen_hor4$ALT <-jitter (Glen_hor4$ALT)

#order depth to display the right order in the illustrations
is.ordered(Glen_hor4$Horizon)
Glen_org <- Glen_hor4 [which (Glen_hor4$Horizon == "Oh_UPP" | Glen_hor4$Horizon == "Oh_LOW"),]

#Statistics, testing the best model and data transformation

head(Glen_org)

# Order age groups
is.ordered(Glen_hor4$Age)
Glen_depth_org <- lme(Horizon_Width~Age+Horizon , random = ~1|Pit_nr,
                  correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_depth_org)
summary(Glen_depth_org)
anova(Glen_depth_org)
plot(Glen_depth_org)
Glen_depth_org_fitted <- fitted(Glen_depth_org)
plot(Glen_org$Horizon_Width, Glen_depth_org_fitted)
abline(0,1)

Glen_pH_org <- lme(pH ~ Age + Horizon, random = ~1|Pit_nr,
                   correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_pH_org)
summary(Glen_pH_org)
anova(Glen_pH_org)
plot(Glen_pH_org)
Glen_pH_org_fitted <- fitted(Glen_pH_org)
plot(Glen_org$pH, Glen_pH_org_fitted)
abline(0,1)

Glen_BD_org <- lme(newBD2~Age+Horizon , random = ~1|Pit_nr,
                      correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),

VarCorr(Glen_BD_org)
summary(Glen_BD_org)
anova(Glen_BD_org)
plot(Glen_BD_org)
Glen_BD_org_fitted <- fitted(Glen_BD_org)
plot(Glen_org$newBD2, Glen_BD_org_fitted)
abline(0,1)

### Average measurements of the descriptive variables with CI included ###
## Grassland is included here even though comparison can not be made

# Glen_1per_site <_ log
Glen_min_grass <- Glen_hor4[which (Glen_hor4$Horizon == "Ah"| Glen_hor4$Horizon == "E"| Glen_hor4$Horizon == "Bhs"|
                                     Glen_hor4$Horizon == "Ah_UPP"|Glen_hor4$Horizon == "Ah_LOW"),]
Glen_min_grass$Age_group3 <- ifelse (Glen_min_grass$Age == 'GRASS', 'GRASS', 'N_GRASS')

Glen_Min_BD_pH_Clay_mean_CI <- aggregate( cbind( Glen_min_grass$Horizon_Width, Glen_min_grass$pH, Glen_min_grass$BD, Glen_min_grass$Clay, Glen_min_grass$Sand, Glen_min_grass$Cperc),
                               by = list(Land_use = Glen_min_grass$Age_group3, Age = Glen_min_grass$Age,  Horizon = Glen_min_grass$Horizon),
                               FUN = function(x) c(mean = mean(x),
                                                   sd = sd(x),
                                                   n = length(x),
                                                   stderror=sd(x)/sqrt(length(x)),
                                                   CI = ((qt(0.975,length(x)-1))*(sd(x)/sqrt(length(x))))))#returns matrixes
Glen_Min_BD_pH_Clay_mean_CI<- do.call(data.frame, Glen_Min_BD_pH_Clay_mean_CI)


colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V1.mean"] <- "Min_width"
colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V2.mean"] <- "pH"
colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V3.mean"] <- "BD"
colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V4.mean"] <- "Clay"
colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V5.mean"] <- "Sand"
colnames (Glen_Min_BD_pH_Clay_mean_CI) [colnames(Glen_Min_BD_pH_Clay_mean_CI)== "V6.mean"] <- "Cperc"

Glen_org_mean_pH_BD_CI <- aggregate( cbind( Glen_org$Horizon_Width, Glen_org$pH, Glen_org$newBD2, Glen_org$Cperc),
                               by = list(Age = Glen_org$Age,  Horizon = Glen_org$Horizon),
                               FUN = function(x) c(mean = mean(x),
                                                   sd = sd(x),
                                                   n = length(x),
                                                   stderror=sd(x)/sqrt(length(x)),
                                                   CI = ((qt(0.975,length(x)-1))*(sd(x)/sqrt(length(x))))))#returns matrixes
Glen_org_mean_pH_BD_CI<- do.call(data.frame, Glen_org_mean_pH_BD_CI)

colnames (Glen_org_mean_pH_BD_CI) [colnames(Glen_org_mean_pH_BD_CI)== "V1.mean"] <- "Min_width"
colnames (Glen_org_mean_pH_BD_CI) [colnames(Glen_org_mean_pH_BD_CI)== "V2.mean"] <- "pH"
colnames (Glen_org_mean_pH_BD_CI) [colnames(Glen_org_mean_pH_BD_CI)== "V3.mean"] <- "BD"
colnames (Glen_org_mean_pH_BD_CI) [colnames(Glen_org_mean_pH_BD_CI)== "V4.mean"] <- "Cperc"

#export and put in one table all the measurements
write.table(Glen_Min_BD_pH_Clay_mean_CI, "Glen_Min_BD_pH_Clay_mean_CI.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
write.table(Glen_org_mean_pH_BD_CI, "Glen_org_mean_pH_BD_CI.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
#Total pit depth plus CI - 95%
Glen_hor_total_depth <- aggregate(Glen_hor$Horizon_Width, 
                                  by = list(Pit_nr = Glen_hor$Pit_nr, Age = Glen_hor$Age), 
                                  FUN =  sum, na.rm= TRUE)
Glen_hor_total_depth<- do.call(data.frame, Glen_hor_total_depth)

Glen_hor_total_depth_CI <- aggregate( cbind( Glen_hor_total_depth$x),
                                      by = list(Age = Glen_hor_total_depth$Age),
                                      FUN = function(x) c(mean = mean(x),
                                                          sd = sd(x),
                                                          n = length(x),
                                                          stderror=sd(x)/sqrt(length(x)),
                                                          CI = ((qt(0.975,length(x)-1))*(sd(x)/sqrt(length(x))))))#returns matrixes
Glen_hor_total_depth_CI<- do.call(data.frame, Glen_hor_total_depth_CI)
write.table(Glen_hor_total_depth_CI, "Glen_hor_total_depth_CI.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)


####
#### Statistical analysis of total SOC and N####


#order the factor to use polynomial contrast for total C t/ha age groups

is.factor(Glen_eq_mass_1persite$Age)
is.ordered(Glen_eq_mass_1persite$Age)
#x <- factor( x , ordered = FALSE ) to remove ordering from a factor

# make a new column with GrASS and N_GRASS for all the other values
Glen_eq_mass_1persite$Age_group3 <- NA
Glen_eq_mass_1persite$Age_group3 <- ifelse (Glen_eq_mass_1persite$Age == 'GRASS', 'GRASS', 'N_GRASS')

# rename GRASS into V.OLD
Glen_eq_mass_1persite$Age_group2 <- NA
Glen_eq_mass_1persite$Age_group2 <- ifelse(Glen_eq_mass_1persite$Age=='GRASS', 'V.OLD', paste(Glen_eq_mass_1persite$Age))

# read in table with coordinates of sites
Coordinates <- read.table("C:\\R_game\\Glen\\Glen_coord.txt", header=TRUE,sep="\t")
Glen_eq_mass_1persite$LAT <- NA
Glen_eq_mass_1persite$LAT <- Coordinates$LAT[match (Glen_eq_mass_1persite$Pit_nr, Coordinates$Pit_nr)]##match and copy correcponding lat coordinate to sites
Glen_eq_mass_1persite$LONG <- NA
Glen_eq_mass_1persite$LONG <- Coordinates$LONG[match (Glen_eq_mass_1persite$Pit_nr, Coordinates$Pit_nr)]##match and copy correcponding long coordinate to sites

#order forest ages, ordering changes how these factors will be treated. R will use orthogonal polynomial contrasts.
Glen_eq_mass_1persite$Age_group2 <- ordered(Glen_eq_mass_1persite$Age_group2, levels=c('YNG', 'OLD', 'V.OLD'))


#make a statstical model for total C t/ha with ajusted BD (to moisture 300% and added residuals)
#adjust model for sample dependance to each otther if they are taken from the same pit
names(Glen_eq_mass_1persite)[names(Glen_eq_mass_1persite) == 'V1'] <- 'SOC'
names(Glen_eq_mass_1persite)[names(Glen_eq_mass_1persite) == 'V2'] <- 'N'


Glen_eq_mass_1persite$dummy_group <- factor('Dummy')

# log transforming SOC t/ha - x didn't make much difference
Glen_eq_mass_1persite$logSOC <- log( Glen_eq_mass_1persite$SOC)
library(nlme)
Glen_Eq_Tot_SOC_lme <- lme(SOC~Age_group2+Age_group3, random = ~1|dummy_group,
                           correlation = corSpher(form = ~LAT+LONG), data=Glen_eq_mass_1persite, method = "ML")  #correlation = corSpher(form = ~Lat+Long),

Glen_Eq_Tot_SOC_lme2 <- update(Glen_Eq_Tot_SOC_lme, .~. -Age_group2)
summary(Glen_Eq_Tot_SOC_lme2)
anova(Glen_Eq_Tot_SOC_lme, Glen_Eq_Tot_SOC_lme2)
Glen_Eq_Tot_SOC_lme3 <- update(Glen_Eq_Tot_SOC_lme, .~. -Age_group3)
summary(Glen_Eq_Tot_SOC_lme3)
anova(Glen_Eq_Tot_SOC_lme, Glen_Eq_Tot_SOC_lme3)
Glen_Eq_Tot_SOC_lme4 <- update(Glen_Eq_Tot_SOC_lme3, .~. -Age_group2)
summary(Glen_Eq_Tot_SOC_lme4)
anova(Glen_Eq_Tot_SOC_lme3, Glen_Eq_Tot_SOC_lme4)

Glen_Eq_Tot_SOC_lme3 <- lme(SOC~Age_group2, random = ~1|dummy_group,
                           correlation = corSpher(form = ~LAT+LONG), data=Glen_eq_mass_1persite, method = "REML")  #correlation = corSpher(form = ~Lat+Long),


VarCorr(Glen_Eq_Tot_SOC_lme3)
summary(Glen_Eq_Tot_SOC_lme3)
anova(Glen_Eq_Tot_SOC_lme)
plot(Glen_Eq_Tot_SOC_lme)
Eq_mas_Totl_SOC_lme_fitted <- fitted(Glen_Eq_Tot_SOC_lme)
plot(Glen_eq_mass_1persite$SOC, Eq_mas_Totl_SOC_lme_fitted)
abline(0,1)

library(mgcv)
Glen_Eq_Tot_SOC_gam <- gamm(SOC ~Age_group2+Age_group3,
                     correlation = corSpher(form = ~LAT+LONG), 
                     random = list(dummy_group=~ 1), data=Glen_eq_mass_1persite, 
                     family = gaussian)

summary(Glen_Eq_Tot_SOC_gam$gam)
summary(Glen_Eq_Tot_SOC_gam$lme)
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
anova(Glen_Eq_Tot_SOC_gam$lme)
gam.check(Glen_Eq_Tot_SOC_gam$gam)
foo1 <- plot(Glen_Eq_Tot_SOC_gam$gam, all.terms=TRUE)
par(mfrow=c(1,1))
Glen_Eq_Tot_SOC_gam$fitted
predicted.values <- predict.gam(Glen_Eq_Tot_SOC_gam$gam, se.fit = TRUE)
gam.check(Glen_Eq_Tot_SOC_gam$gam)
fitted(Glen_Eq_Tot_SOC_gam$gam)
Glen_eq_mass_1persite$predictions <- Eq_mas_Totl_SOC_lme_fitted
  # predict.gam(Glen_Eq_Tot_SOC_gam$gam)
Glen_eq_mass_1persite$predictionsSE <- predicted.values$se.fit
Glen_eq_mass_1persite$predictionsCI <- 2*Glen_eq_mass_1persite$predictionsSE

head(Glen_eq_mass_1persite)

#extract fitted values from the model new lme
Glen_eq_mass_1persite$fitted.val <- fitted(Glen_Eq_Tot_SOC_lme, level = 0)
library(dplyr)
Glen_Total_SOC <-group_by(Glen_eq_mass_1persite, Age)%>%
  summarise(
    Ccount=n(), 
    Cmean = mean(SOC, na.rm = TRUE), 
    Csd = sd(SOC, na.rm = TRUE)
  )

#the different names in the different data sets mess up the graph, not sure why, but it seems to matter, the same naming is important
colnames(Glen_Total_SOC)[colnames(Glen_Total_SOC)=="Cmean"] <- "SOC"


cmult <- 1.96  ## could use 1.96 instead
#Total SOC t/ha bar plot for keeping things simple and easy to interpret

Glen_Total_SOC_plot <- ggplot(Glen_Total_SOC, aes(fill=Age, x=Age, y= SOC)) +
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_bw()+
  # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(data= Glen_eq_mass_1persite, aes(y=(predictions), group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             size=5,
             shape = 4)+
  # geom_linerange(data = Glen_eq_mass_1persite, aes( ymin=(predictions-2*predictionsSE),
  #                                      ymax=(predictions+2*predictionsSE), group = Age),
  #                stat = 'identity',
  #                position = position_dodge(width = .9),
  #                colour = "blue")+
  geom_point(data= Glen_eq_mass_1persite, aes(y=SOC, group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             size=2)+
  labs( x="Group",
        y = expression(paste("Total SOC","  ", "Mg ha"^"-1","  ", "stock")))+
  theme(axis.text=element_text(size=13),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position="none")+
  scale_x_discrete(limit = c("YNG", "OLD", "V.OLD", "GRASS"),
                   labels = c("Young", "Mature", "Ancient", "Grassland"))+
  scale_fill_manual(values = c( "grey70", "white","grey40",  "grey20"), name= "Age_class")
Glen_Total_SOC_plot


Glen_tot_SOC <- ggplot(Glen_eq_mass_1persite, aes( x=Age, y=SOC)) +
  theme_bw()+
  geom_boxplot(aes(colour = Age))+
  coord_cartesian(ylim = c(0, 400))+
  labs( x=" Age group", y = expression(paste("SOC","  ", "t ha"^"-1")))+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8)) 
Glen_tot_SOC

# Layer statistical analysis


##### A statstical model for depth and age dependant C t/ha with  BD  
### Random effects are added to adjust model for sample dependance to each otther if they are taken from the same pit

Glen_org$log_SOC <- log(Glen_org$C_tha_Ohnew)
# grasslnad eliminated form this column and coded into v.old as exactly same variable can't be in a code


library(nlme)
citation("nlme")
# Explained well lme https://www.youtube.com/watch?v=VhMWPkTbXoY random effects that are acounted in lme
Glen_org_SOC_lm <- lme(log_SOC~Horizon*Age+pH*Horizon+pH*Age,
                      correlation = corSpher( form = ~LAT+LONG ),
                      random = ~ 1|Pit_nr, data=Glen_org, method = "ML")  #correlation =corSpher ( form= Lat+Long ),
Glen_org_SOC_lm2 <- update(Glen_org_SOC_lm, .~. -pH*Horizon)
summary(Glen_org_SOC_lm2)
anova(Glen_org_SOC_lm, Glen_org_SOC_lm2)

Glen_org_SOC_lm3 <- update(Glen_org_SOC_lm2, .~. -pH:Age)
summary(Glen_org_SOC_lm3)
anova(Glen_org_SOC_lm2, Glen_org_SOC_lm3)

Glen_org_SOC_lm4 <- lme(log_SOC~Horizon+Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "ML")
summary(Glen_org_SOC_lm4)
anova(Glen_org_SOC_lm3, Glen_org_SOC_lm4)

Glen_org_SOC_lm5 <- update(Glen_org_SOC_lm4, .~. -Horizon)
summary(Glen_org_SOC_lm5)
anova(Glen_org_SOC_lm4, Glen_org_SOC_lm5)

Glen_org_SOC_lm6 <- update(Glen_org_SOC_lm4, .~. -Age)
summary(Glen_org_SOC_lm6)
anova(Glen_org_SOC_lm4, Glen_org_SOC_lm6)


Glen_org_SOC_lm7 <- update(Glen_org_SOC_lm5, .~. -Age)
summary(Glen_org_SOC_lm7)
anova(Glen_org_SOC_lm4, Glen_org_SOC_lm7)

Glen_org_SOC_lm4 <- lme(log_SOC~Horizon+Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "REML")

VarCorr(Glen_org_SOC_lm4)
summary(Glen_org_SOC_lm4)
anova(Glen_org_SOC_lm4)
plot(Glen_org_SOC_lm)
plot(ranef(Glen_org_SOC_lm))
dev.off() # restarts plots vizualization
Glen_org_SOC_lm_fitted <- fitted(Glen_org_SOC_lm) # extracts fitted values from the linear model
plot( Glen_org_SOC_lm_fitted, Glen_org$log_SOC)
abline(0,1)

##### https://stackoverflow.com/questions/40504505/mgcv-fix-smoothing-parameter-in-gamm-and-validity-of-model-nesting
library(mgcv)
Glen_org_SOC_gam <- gamm(log_SOC~Horizon+Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = list(Pit_nr=~ 1), data=Glen_org,
                        family = gaussian)

summary(Glen_org_SOC_gam$gam)
summary(Glen_org_SOC_gam$lme)
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
anova(Glen_org_SOC_gam$lme)
gam.check(Glen_org_SOC_gam$gam)
foo1 <- plot(Glen_org_SOC_gam$gam, all.terms=TRUE)

par(mfrow=c(1,1))
Glen_org_SOC_gam$fitted

predicted.values2 <- predict.gam(Glen_org_SOC_gam$gam, se.fit = TRUE)
plot(Glen_org_SOC_gam,residuals=TRUE,pch=19,cex=.5)
fitted(Glen_org_SOC_gam$gam)
Glen_org$predictions <- Glen_org_SOC_lm_fitted
  # predict.gam(Glen_org_SOC_gam$gam)
Glen_org$predictionsSE <- predicted.values2$se.fit
head(Glen_org)


#extract fitted values from the model
Glen_org_SOC_lm$fitted
Glen_org$fitted.val <- fitted(Glen_org_SOC_lm, level = 0)
cmult <- 1.96  ## could use 2 instead
coef(summary(Glen_org_SOC_lm))


Glen_org$Horizon <- ordered(Glen_org$Horizon, levels=c('Oh_UPP','Oh_LOW'))


Glen_org_aver <- aggregate( cbind( Glen_org$C_tha_Ohnew, Glen_org$N_tha_Ohnew),by = list(Age = Glen_org$Age, Horizon = Glen_org$Horizon),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),n = length(x),
                                                stderror=sd(x)/sqrt(length(x)), CI = ((qt(0.975,length(x)-1))*(sd(x)/sqrt(length(x))))))#returns matrixes
Glen_org_aver<- do.call(data.frame, Glen_org_aver)
colnames (Glen_org_aver)[colnames(Glen_org_aver)=="V1.mean"] <- "C_tha_Ohnew" #rename SOC and N as in the other datsets otherwise ggplot will not connect 

par(mar=c(5,5,4,2)) # adjusts margins to accommodate superscripts in the axis text
library(ggplot2)

Glen_Oh_SOCplot <- ggplot(Glen_org_aver, aes(fill=Age, x=Horizon, y= C_tha_Ohnew)) +
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_bw()+
  # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(data= Glen_org, aes(y=exp(predictions), group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             shape=4,
             size=4)+
  # geom_linerange(data = Glen_org,
  #                aes( ymin=exp (predictions-2*predictionsSE),
  #                     ymax=exp (predictions+2*predictionsSE),
  #                     group = Age),
  #                stat = 'identity',
  #                position = position_dodge(width = .9),
  #                colour = "blue")+
  geom_point(data= Glen_org, aes(y=C_tha_Ohnew, group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             size=2)+
  labs( x="Organic horizons", y = expression(paste("Mg C ha"^"-1")))+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8))+
  scale_x_discrete(limit = c("Oh_UPP", "Oh_LOW"),
                   labels = c("F ", "H "))+
  guides(fill=guide_legend(title="Age group"))+
  scale_fill_manual(values = c("white", "grey55", "grey30"), name= "Age_class",
                    labels = c("Young",
             "Mature",
             "Ancient"))
Glen_Oh_SOCplot


##### Mineral layers' statistical ananlysis

#add LAT and LONG coordinates to Equivalent_mass_1persite
Coordinates <- read.table("C:\\R_game\\Glen\\Glen_coord.txt", header=TRUE,sep="\t")
Glen_min$LAT <- NA
Glen_min$LAT <- Coordinates$LAT[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
Glen_min$LONG <- NA
Glen_min$LONG <- Coordinates$LONG[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
Glen_min$ALT <- NA
Glen_min$ALT <- Coordinates$ALT[match (Glen_min$Pit_nr, Coordinates$Pit_nr)]
#jitter is addded to avoid ties as couple of sites are very close to each other 
Glen_min$LAT <- jitter (Glen_min$LAT)
Glen_min$LONG <- jitter (Glen_min$LONG)
Glen_min$ALT <-jitter (Glen_min$ALT)
#order depth to display the right order in the illustrations
is.ordered(Glen_min$Horizon)
# Order age groups
is.ordered(Glen_min$Age)

#Statistics, testing the best model and data transformation

head(Glen_min)

##### A statstical model for depth and age dependant C t/ha with ajusted equivalent mass
### Random effects are added to adjust model for sample dependance to each other, because they are taken from the same pit

Glen_min$log_SOC <- log(Glen_min$C_tha_2_eq_mass)

library(nlme)
citation("nlme")
# Explained well lme https://www.youtube.com/watch?v=VhMWPkTbXoY random effects that are acounted in lme
Glen_min_SOC_lm <- lme(log_SOC~Horizon*Age+Age*Clay+Age*pH+pH*Clay+pH:Horizon+Clay*Horizon,
                       correlation = corSpher( form = ~LAT+LONG ),
                       random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
Glen_min_SOC_lm2 <- lme(log_SOC~Horizon*Age+Age*Clay+Age*pH+pH*Clay+pH:Horizon,
                       correlation = corSpher( form = ~LAT+LONG ),
                       random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")

summary(Glen_min_SOC_lm2)
anova(Glen_min_SOC_lm, Glen_min_SOC_lm2)

Glen_min_SOC_lm3 <- lme(log_SOC~Horizon*Age+Age*Clay+Age*pH+pH:Horizon+Clay*Horizon,
                       correlation = corSpher( form = ~LAT+LONG ),
                       random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm3)
anova(Glen_min_SOC_lm, Glen_min_SOC_lm3)

Glen_min_SOC_lm4 <- lme(log_SOC~Horizon*Age+Age*pH+pH*Clay+pH:Horizon+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm4)
anova(Glen_min_SOC_lm, Glen_min_SOC_lm4)

Glen_min_SOC_lm5 <- lme(log_SOC~Horizon*Age+pH*Clay+pH:Horizon+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm5)
anova(Glen_min_SOC_lm4, Glen_min_SOC_lm5)

Glen_min_SOC_lm6 <- lme(log_SOC~Horizon*Age+Age*pH+pH*Clay+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm4)
anova(Glen_min_SOC_lm4, Glen_min_SOC_lm6)

Glen_min_SOC_lm7 <- lme(log_SOC~Age*pH+pH*Clay+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm7)
anova(Glen_min_SOC_lm4, Glen_min_SOC_lm7)

Glen_min_SOC_lm8 <- lme(log_SOC~Horizon*Age+Age*pH+pH:Horizon+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm8)
anova(Glen_min_SOC_lm4, Glen_min_SOC_lm8)

Glen_min_SOC_lm9 <- lme(log_SOC~Horizon+Age+Clay+pH,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "REML")  #correlation spherical
summary(Glen_min_SOC_lm9)
anova( Glen_min_SOC_lm9)

#The chosen model is slightly overparametrised
#However the procedure to select is correct, so I will stick with it
#It does not matter, because the main age effect is not significant anyway. 
Glen_min_SOC_lm4 <- lme(log_SOC~Horizon*Age+Age*pH+pH*Clay+pH:Horizon+Clay*Horizon,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min, method = "REML")  #correlation spherical
plot(Glen_min_SOC_lm4, col = 1) # check the residuals of model 
VarCorr(Glen_min_SOC_lm4)
summary(Glen_min_SOC_lm4)
anova(Glen_min_SOC_lm4)
plot(Glen_min_SOC_lm4)
plot(ranef(Glen_min_SOC_lm4))
dev.off() # restarts plots vizualization
Glen_min_SOC_lm_fitted <- fitted(Glen_min_SOC_lm4) # extracts fitted values from the linear model
plot( Glen_min_SOC_lm_fitted, Glen_min$log_SOC)
abline(0,1)

##### https://stackoverflow.com/questions/40504505/mgcv-fix-smoothing-parameter-in-gamm-and-validity-of-model-nesting
library(mgcv)
Glen_min_SOC_gam <- gamm(log_SOC~Horizon*Age+Age*pH+pH*Clay+pH:Horizon+Clay*Horizon,
                         correlation = corSpher( form = ~LAT+LONG ),
                         random = list(Pit_nr=~ 1), data=Glen_min,
                         family = gaussian)

summary(Glen_min_SOC_gam$gam)
summary(Glen_min_SOC_gam$lme)
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
anova(Glen_min_SOC_gam$lme)
anova(Glen_min_SOC_gam$gam)
gam.check(Glen_min_SOC_gam$gam)
foo1 <- plot(Glen_min_SOC_gam$gam, all.terms=TRUE)
par(mfrow=c(1,1))
Glen_min_SOC_gam$fitted
plot(Glen_min_SOC_gam,residuals=TRUE,pch=19,cex=.5)
fitted(Glen_min_SOC_gam$lme)
predicted.values <- predict.gam(Glen_min_SOC_gam$gam, se.fit = TRUE)
Glen_min$predictions <- Glen_min_SOC_lm_fitted
  # predict.gam(Glen_min_SOC_gam$gam)
Glen_min$predictionsSE <- predicted.values$se.fit
head(Glen_min)


#extract fitted values from the model
Glen_min_SOC_lm$fitted
Glen_min$fitted.val <- fitted(Glen_min_SOC_lm, level = 0)
cmult <- 1.96  ## could use 2 instead
coef(summary(Glen_min_SOC_lm))


Glen_min$Horizon <- ordered(Glen_min$Horizon, levels=c('Ah','E','Bhs'))


Glen_min_aver <- aggregate( cbind( Glen_min$C_tha_Ohnew, Glen_min$N_tha_Ohnew),by = list(Age = Glen_min$Age, Horizon = Glen_min$Horizon),
                            FUN = function(x) c(mean = mean(x), sd = sd(x),n = length(x),
                                                stderror=sd(x)/sqrt(length(x)), CI = ((qt(0.975,length(x)-1))*(sd(x)/sqrt(length(x))))))#returns matrixes
Glen_min_aver <- do.call(data.frame, Glen_min_aver)
colnames (Glen_min_aver)[colnames(Glen_min_aver)=="V1.mean"] <- "C_tha_2_eq_mass" #rename SOC and N as in the other datsets otherwise ggplot will not connect 

par(mar=c(5,5,4,2)) # adjusts margins to accommodate superscripts in the axis text
library(ggplot2)

# barpolt with fitted fixed effects values  shown as stars  instead of confidence intervals and errors, to see where model overpredicts
Glen_min_SOCplot <- ggplot(Glen_min_aver, aes(fill=Age, x=Horizon, y= C_tha_2_eq_mass)) +
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_bw()+
  # theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(data= Glen_min, aes(y=exp(predictions), group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             shape=4,
             size=4)+
  # geom_linerange(data = Glen_min, aes( ymin=exp(predictions-2*predictionsSE),
  #                                      ymax=exp(predictions+2*predictionsSE), group = Age),
  #                stat = 'identity',
  #                position = position_dodge(width = .9),
  #                colour = "blue")+
  geom_point(data= Glen_min, aes(y=C_tha_2_eq_mass, group=Age),
             stat = 'identity',
             position = position_dodge(width = .9),
             size=1.5)+
  labs( x="Mineral horizons", y = expression(paste( "Mg C ha"^"-1")))+
  theme(axis.text=element_text(size=13),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position=c(0.2,0.8))+
  scale_x_discrete(limit = c("Ah", "E", "Bhs"),
                   labels = c("Ah", "E", "Bhs"))+
  guides(fill=guide_legend(title="Age group"))+
  scale_fill_manual(values = c("white", "grey55", "grey30"), name= "Age_class",
                    labels = c("Young",
                               "Mature",
                               "Ancient"))
Glen_min_SOCplot

#print both graphs together 

library(ggpubr)
tiff("Statistical_analysis_three_figures.tiff", units="mm", width=360, height=100, res=300)
ggarrange(Glen_Total_SOC_plot, Glen_Oh_SOCplot, Glen_min_SOCplot, ncol = 3)
dev.off()
#, labels = c("A", "B", "C")

#forest Plant Input distribution according to CARBINE outputs for pine 200 year old, based on Christie and ... yield tables
Glen_PI_distribution <- read.table("C:\\R_game\\GLEN\\Glenmore_pine_plant_input_distribution.txt", header=TRUE,sep="\t")
Glen_PI_distribution_split<- Glen_PI_distribution[rep(seq_len(nrow (Glen_PI_distribution)),each=12),] #repeats each line 12 times to divide into 12 months 
write.table(Glen_PI_distribution_split, "Glen_PI_distribution_split.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
Glen_PI_distribution_split2 <- read.table("C:\\R_game\\GLEN\\Glen_PI_distribution_split2.txt", header=TRUE,sep="\t")
Glen_PI <- aggregate(cbind(Glen_PI_distribution_split2$PI.proportion), 
                        by= list(Age = Glen_PI_distribution_split2$Age),
                        FUN = sum,  na.rm= TRUE)
Glen_aver <- mean(Glen_PI$V1)
Glen_PI$perc <- (Glen_PI$V1*100)/Glen_aver


### GLENMORE PINE RothC simmulations ###
### Aggregated results ###
### Illustrations in graphs ###



library(ggplot2)
## Read in files from main simmulations ##
#Total Soc and pools
Glen_RothC_main <- read.table("C:\\R_game\\GLEN\\Forest_grass_RothC2.txt", header=TRUE,sep="\t")
Glen_RothC_main$F_H_SOCacc <- Glen_RothC_main$F_SOCacc + Glen_RothC_main$H_SOCacc
Glen_RothC_main$F_H_SOC_stock <- Glen_RothC_main$F_SOC_stock + Glen_RothC_main$H_SOC_stock

Glen_RothC_main_grass <- Glen_RothC_main[ which(Glen_RothC_main$Land_use=='Gassland'), ]

RothC_DPM_SOC <- ggplot(Glen_RothC_main_grass, aes(x = Year, y = DPM_stock_tha, colour = factor(Pit_number))) +
  geom_point(alpha=0.5) +
   labs( x="Year", y = expression(paste("Decomposable plant material SOC","  ", "t ha"^"-1")))+
  facet_grid(RothC_run~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="right")
RothC_DPM_SOC

# Glen_RothC_main$Land_use <-as.factor(Glen_RothC_main$Land_use)
head(Glen_RothC_main)
Glen_RothC_mean <- aggregate(cbind(Glen_RothC_main$F_SOCacc, 
                                   Glen_RothC_main$F_SOC_stock,
                                   Glen_RothC_main$H_SOCacc, 
                                   Glen_RothC_main$H_SOC_stock, 
                                   Glen_RothC_main$Total_SOC_stock_tha, 
                                   Glen_RothC_main$DPM_stock_tha,
                                   Glen_RothC_main$RPM_stock_tha,
                                   Glen_RothC_main$BIO_stock_tha,
                                   Glen_RothC_main$HUM_stock_tha,
                                   Glen_RothC_main$IOM_stock_tha,
                                   Glen_RothC_main$Tot_PI_tha,
                                   Glen_RothC_main$Tot_SOC_calulcated_from_pools, 
                                   Glen_RothC_main$F_H_SOCacc,
                                   Glen_RothC_main$F_H_SOC_stock), 
                         by= list(Land_use = Glen_RothC_main$Land_use, Year = Glen_RothC_main$Year, 
                                  RothC_run = Glen_RothC_main$RothC_run),
                         FUN = function(x) c(mean=mean(x), min=min(x), max=max(x)))

Glen_RothC_mean<- do.call(data.frame, Glen_RothC_mean)
#rename columns
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V1.mean"] <- "F_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V2.mean"] <- "F_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V3.mean"] <- "H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V4.mean"] <- "H_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V5.mean"] <- "Total_SOC_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V6.mean"] <- "DPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V7.mean"] <- "RPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V8.mean"] <- "BIO_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V9.mean"] <- "HUM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V10.mean"] <- "IOM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V11.mean"] <- "Tot_PI_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V12.mean"] <- "Tot_SOC_calulcated_from_pools"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V13.mean"] <- "F_H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V14.mean"] <- "F_H_SOC_stock"

# Min stocks per ha
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V1.min"] <- "Min_F_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V2.min"] <- "Min_F_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V3.min"] <- "Min_H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V4.min"] <- "Min_H_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V5.min"] <- "Min_Total_SOC_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V6.min"] <- "Min_DPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V7.min"] <- "Min_RPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V8.min"] <- "Min_BIO_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V9.min"] <- "Min_HUM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V10.min"] <- "Min_IOM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V11.min"] <- "Min_Tot_PI_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V12.min"] <- "Min_Tot_SOC_calulcated_from_pools"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V13.min"] <- "Min_F_H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V14.min"] <- "Min_F_H_SOC_stock"
# MAx stocks
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V1.max"] <- "Max_F_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V2.max"] <- "Max_F_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V3.max"] <- "Max_H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V4.max"] <- "Max_H_SOC_stock"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V5.max"] <- "Max_Total_SOC_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V6.max"] <- "Max_DPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V7.max"] <- "Max_RPM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V8.max"] <- "Max_BIO_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V9.max"] <- "Max_HUM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V10.max"] <- "Max_IOM_stock_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V11.max"] <- "Max_Tot_PI_tha"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V12.max"] <- "Max_Tot_SOC_calulcated_from_pools"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V13.max"] <- "Max_F_H_SOCacc"
colnames (Glen_RothC_mean) [colnames(Glen_RothC_mean)== "V14.max"] <- "Max_F_H_SOC_stock"

library(rlang)


#the graph show steady state in the oldest pine forest sites and grassland afforestation runs
#with F and H layers actively accummulating for another 200 years

#provides right order for the graph
Glen_RothC_mean$RothC_run <- factor(Glen_RothC_mean$RothC_run , ordered = TRUE, 
                                levels = c("sstate", "manag"))

# New facet label names for RothC_run variable
RothC_run_labels <- c("Steady state", "Managed forest")
names(RothC_run_labels) <- c("sstate", "manag")

tiff("Total_SOC.tiff", units="mm", width=180, height=180, res=300)
# insert ggplot code

RothC_sstat_tot_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = Total_SOC_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(name="Land use",values=c( "black", "dimgrey"), labels = c("Forest", "Grassland"))+
  geom_ribbon(data = Glen_RothC_mean, aes(x = Year, ymax = Max_Total_SOC_stock_tha, ymin = Min_Total_SOC_stock_tha),
              alpha = 0.3, fill = "grey")+
  labs(x="Year", y = expression(paste("Total SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(RothC_run ~ ., labeller = labeller(RothC_run = RothC_run_labels))+ #
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  ylim(0, 575)
RothC_sstat_tot_SOC
dev.off()

#find location of min and max values in the managed forest and afforestation
library(dplyr)
Glen_RothC_mean_manag <- filter(Glen_RothC_mean, RothC_run == "manag" & Year > -1)


#copy one year of steady state simmulation it will be the comparisson point
Glen_RothC_mean_sstate <- filter(Glen_RothC_mean, RothC_run == "sstate")
RothC_sstate <- filter(Glen_RothC_mean_sstate, Year == "-10")

#find actual min and max numbers in managed forest and afforestation
RothC_min_max_tha <- aggregate(cbind(Glen_RothC_mean_manag$Total_SOC_stock_tha, Glen_RothC_mean_manag$DPM_stock_tha,
                             Glen_RothC_mean_manag$RPM_stock_tha, Glen_RothC_mean_manag$BIO_stock_tha,
                             Glen_RothC_mean_manag$HUM_stock_tha),
                       by = list (Land_use = Glen_RothC_mean_manag$Land_use),
                       FUN = function(x) c(min = min(x), 
                                           max = max(x)))
RothC_min_max_tha <- do.call(data.frame, RothC_min_max_tha)

head(RothC_min_max_tha)
RothC_min_max_tha<- RothC_min_max_tha %>%
  rename( Total_SOC_stock_tha_min= V1.min, 
          Total_SOC_stock_tha_max= V1.max, 
          DPM_stock_tha_min = V2.min,
          DPM_stock_tha_max = V2.max,
          RPM_stock_tha_min = V3.min,
          RPM_stock_tha_max = V3.max,
          BIO_stock_tha_min = V4.min, 
          BIO_stock_tha_max = V4.max,
          HUM_stock_tha_min= V5.min,
          HUM_stock_tha_max= V5.max)

RothC_min_max_tha
head(RothC_min_max_tha)

library(tidyr)
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
Long_RothC_min_max_tha <- gather(RothC_min_max_tha, Pools, t_ha, Total_SOC_stock_tha_min:HUM_stock_tha_max, factor_key=TRUE)
head(Long_RothC_min_max_tha)
Long_RothC_min_max_tha$Year <- NA
Long_RothC_min_max_tha$Year <- Glen_RothC_mean_manag$Year [match (Long_RothC_min_max_tha$t_ha, 
                                                                         Glen_RothC_mean_manag$Total_SOC_stock_tha)]
  
Long_RothC_min_max_tha$Year2 <- Glen_RothC_mean_manag$Year [match (Long_RothC_min_max_tha$t_ha, 
                                                                  Glen_RothC_mean_manag$DPM_stock_tha )]
Long_RothC_min_max_tha$Year3 <- Glen_RothC_mean_manag$Year [match (Long_RothC_min_max_tha$t_ha, 
                                                                   Glen_RothC_mean_manag$RPM_stock_tha )]
Long_RothC_min_max_tha$Year4 <- Glen_RothC_mean_manag$Year [match (Long_RothC_min_max_tha$t_ha, 
                                                                   Glen_RothC_mean_manag$BIO_stock_tha )]
Long_RothC_min_max_tha$Year5 <- Glen_RothC_mean_manag$Year [match (Long_RothC_min_max_tha$t_ha, 
                                                                   Glen_RothC_mean_manag$HUM_stock_tha )]
#run if else and copy to Year 
Long_RothC_min_max_tha$Year <- ifelse(Long_RothC_min_max_tha$Year2 > -20 & !is.na(Long_RothC_min_max_tha$Year2), 
                                      Long_RothC_min_max_tha$Year2, Long_RothC_min_max_tha$Year)
Long_RothC_min_max_tha$Year <- ifelse(Long_RothC_min_max_tha$Year3 > -20 & !is.na(Long_RothC_min_max_tha$Year3), 
                                      Long_RothC_min_max_tha$Year3, Long_RothC_min_max_tha$Year)
Long_RothC_min_max_tha$Year <- ifelse(Long_RothC_min_max_tha$Year4 > -20 & !is.na(Long_RothC_min_max_tha$Year4), 
                                      Long_RothC_min_max_tha$Year4, Long_RothC_min_max_tha$Year)
Long_RothC_min_max_tha$Year <- ifelse(Long_RothC_min_max_tha$Year5 > -20 & !is.na(Long_RothC_min_max_tha$Year5), 
                                      Long_RothC_min_max_tha$Year5, Long_RothC_min_max_tha$Year)
Long_RothC_min_max_tha$Year2 <- NULL
Long_RothC_min_max_tha$Year3 <- NULL
Long_RothC_min_max_tha$Year4 <- NULL
Long_RothC_min_max_tha$Year5 <- NULL
Long_RothC_min_max_tha$RothC_run <- "manag"

##Add steady state runs to the table 

#Make long list of steady state and remove unused columns
Long_RothC_sstate_tha <- gather(RothC_sstate, Pools, t_ha, Total_SOC_stock_tha:HUM_stock_tha, factor_key=TRUE)

Long_RothC_sstate_tha <- select(Long_RothC_sstate_tha, -(F_SOCacc:Max_F_H_SOC_stock))

#remove usnused lines
Long_RothC_sstate_tha<-Long_RothC_sstate_tha[!grepl("Min" , Long_RothC_sstate_tha$Pools),]
Long_RothC_sstate_tha<-Long_RothC_sstate_tha[!grepl("Max" , Long_RothC_sstate_tha$Pools),]

#connect Long_RothC_sstate_tha table to the Long_RothC_min_max_tha table
Long_RothC_min_max_ss_tha <- rbind(Long_RothC_min_max_tha, Long_RothC_sstate_tha) 

library(tidyverse)
Long_RothC_min_max_ss_tha <- Long_RothC_min_max_ss_tha  %>% separate(Pools, 
                                      into = c("Pool", "Setting"), sep = "_stock_tha", convert = TRUE)
Long_RothC_min_max_ss_tha$Setting <- if_else(Long_RothC_min_max_ss_tha$Setting == "", "sstate", Long_RothC_min_max_ss_tha$Setting )
# For illustrations sstate would look better not -10 but year 0 exchange year -10 to number 0

#Make illustrations with bar charts 

library(ggplot2)
library(egg) #to name each facet 
#Forest graph
#In excel include percent colunm for min and max values, compared to sstate
write.table(Long_RothC_min_max_ss_tha, "Long_RothC_min_max_ss_tha.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
Long_RothC_min_max_ss_tha2 <- read.table("C:\\R_game\\GLEN\\Long_RothC_min_max_ss_tha2.txt", header=TRUE,sep="\t")


data <- subset(Long_RothC_min_max_ss_tha2, Land_use=="Forest") 

tiff("Forest_pools.tiff", units="mm", width=180, height=100, res=300)
Glen_min_max_SOCplot <- ggplot(data, aes(fill=Setting, x=Setting, y= t_ha)) +
  facet_grid(. ~ Pool, scales = "free")+
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_bw()+
  theme( panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
  labs( x="RothC outputs", y = expression(paste("Mg C ha"^"-1")))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position= "none")+
  geom_text(aes(label= Percent_change), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_x_discrete(limit = c("sstate", "_min", "_max"),
                    labels = c("Ss ", "Min", "Max"))+
  scale_fill_manual(values = c("white", "grey55", "grey30"))

Glen_min_max_SOCplot
dev.off()

#Grassland graph
data2 <- subset(Long_RothC_min_max_ss_tha2, Land_use=="Gassland") 
tiff("Grass_pools.tiff", units="mm", width=180, height=100, res=300)
Glen_grass_min_max_SOCplot <- ggplot(data2, aes(fill=Setting, x=Setting, y= t_ha)) +
  facet_grid(. ~ Pool, scales = "free")+
  geom_bar(position="dodge", stat="identity", color="black")+
  theme_bw()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+ 
  labs( x="RothC outputs", y = expression(paste("Mg C ha"^"-1")))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=13))+
  theme(legend.position= "none")+
  geom_text(aes(label= Percent_change), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_x_discrete(limit = c("sstate", "_min", "_max"),
                   labels = c("Ss ", "Min", "Max"))+
  scale_fill_manual(values = c("white", "grey55", "grey30"))

Glen_grass_min_max_SOCplot
dev.off()

#In Excel prepare a table with min max year included. 
write.table(Long_RothC_min_max_ss_tha, "Long_RothC_min_max_ss_tha.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)



RothC_DPM_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = DPM_stock_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = DPM_stock_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Decomposable plant material SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")


RothC_DPM_SOC

RothC_RPM_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = RPM_stock_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = RPM_stock_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Resistant plant material  SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")


RothC_RPM_SOC

RothC_BIO_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = BIO_stock_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = BIO_stock_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Biological pool SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 8)
RothC_BIO_SOC #the break at 0 years in grassland graph is not a mistake, because it shows two different simmulations grassland -10 to -1 and forest from 0 years
#I did not make any assumptions and made a transition time, to allow intermediate state between forest and grassland

Glen_RothC_main$Pit_number <- as.factor(Glen_RothC_main$Pit_number)

RothC_HUM_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = HUM_stock_tha, colour = Land_use)) +
  # geom_point(alpha=0.8) +
  # scale_color_manual(values = c("grey11", "#636363", "black",
  #                               "grey76", "grey65", "grey15", "Grey67", "Grey90"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = HUM_stock_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Humified matter SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 130)
RothC_HUM_SOC

RothC_IOM_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = IOM_stock_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = IOM_stock_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Inert organic matter SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 30)
RothC_IOM_SOC #no changes in grassland affostation is due to an assumption that accummulation peat layer has 0 IOM

library(gridExtra)
grid.arrange(RothC_ToT_SOC, RothC_RPM_SOC, RothC_DPM_SOC, RothC_BIO_SOC, RothC_HUM_SOC, RothC_IOM_SOC, ncol = 2)

#need to add back Peat layer accummulation, now it is not included, previous graphs are just PI distribution illustration

#These grapgs show PI distibution and adding back F and H layer active accummulation
#F and H layers are not in equilibrium they are counted as actively accumulating layers
RothC_ToT_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = TotaSOC_stock_with_FH_losses_tha)) +
  # geom_point(alpha=0.3) +
  #   scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #   labels = c("Forest",
  #              "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = TotaSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland")
  )+
  labs( x="Year", y = expression(paste("Total SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 375)


RothC_ToT_acc_SOC

RothC_DPM_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = DPMSOC_stock_with_FH_losses_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = DPMSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Decomposable plant material SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")


RothC_DPM_acc_SOC

RothC_RPM_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = RPMSOC_stock_with_FH_losses_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = RPMSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Resistant plant material  SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")


RothC_RPM_acc_SOC

RothC_BIO_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = BIOSOC_stock_with_FH_losses_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = BIOSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Biological pool SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 12)
RothC_BIO_acc_SOC #the break at 0 years in grassland graph is not a mistake, because it shows two different simmulations grassland -10 to -1 and forest from 0 years
#I did not make any assumptions and made a transition time, to allow intermediate state between forest and grassland


RothC_HUM_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = HUMSOC_stock_with_FH_losses_tha, colour = Land_use)) +
  # geom_point(alpha=0.8) +
  # scale_color_manual(values = c("grey11", "#636363", "black",
  #                               "grey76", "grey65", "grey15", "Grey67", "Grey90"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = HUMSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Humified matter SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 130)
RothC_HUM_acc_SOC

RothC_IOM_acc_SOC <- ggplot(Glen_RothC_mean, aes(x = Year, y = IOMSOC_stock_with_FH_losses_tha, colour = factor(Land_use))) +
  # geom_point(alpha=0.03) +
  # scale_color_manual(values = c("grey11", "#636363"), name= "Land use",
  #                    labels = c("Forest",
  #                               "Grassland"))+
  geom_line(data = Glen_RothC_mean, aes(x = Year, y = IOMSOC_stock_with_FH_losses_tha, colour = factor (Land_use)),
            size=2)+
  scale_fill_manual(values = c("grey11", "#636363"), name= "Land use",
                    labels = c("Forest",
                               "Grassland"))+
  labs( x="Year", y = expression(paste("Inert organic matter SOC","  ", "t ha"^"-1")))+
  facet_grid(Land_use ~ .)+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="none")+
  ylim(0, 30)
RothC_IOM_acc_SOC #no changes in grassland affostation is due to an assumption that accummulation peat layer has 0 IOM

library(gridExtra)
grid.arrange(RothC_ToT_SOC, RothC_ToT_acc_SOC, RothC_RPM_SOC, RothC_RPM_acc_SOC, 
             RothC_DPM_SOC, RothC_DPM_acc_SOC, RothC_BIO_SOC, RothC_BIO_acc_SOC,
             RothC_HUM_SOC, RothC_HUM_acc_SOC, RothC_IOM_SOC, RothC_IOM_acc_SOC, ncol = 2)

#Both PI distribution and F, H layer accummulation connected in one grapgh per one pool
## Read in files from main simmulations ##
#Total Soc and pools
Glen_RothC_main2 <- read.table("C:\\R_game\\GLEN\\Forest_grass_RothC_modif.txt", header=TRUE,sep="\t")

Glen_RothC_main2$Land_use <-as.factor(Glen_RothC_main2$Land_use)
head(Glen_RothC_main2)
Glen_RothC_mean2 <- aggregate(cbind(Glen_RothC_main2$Total_SOC_stock_tha_without_cummulative_changes, 
                                    Glen_RothC_main2$DPM_stock_tha,
                                   Glen_RothC_main2$RPM_stock_tha, Glen_RothC_main2$BIO_stock_tha, 
                                   Glen_RothC_main2$HUM_stock_tha, Glen_RothC_main2$IOM_stock_tha), 
                             by= list(Land_use = Glen_RothC_main2$Land_use, Year = Glen_RothC_main2$Year, 
                                      Modelling = Glen_RothC_main2$Modelling),
                             FUN = function(x) c(mean = mean(x), min = min(x), max = max(x)))

Glen_RothC_mean2<- do.call(data.frame, Glen_RothC_mean2)
#rename columns
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V1.mean"] <- "TotalSOC_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V1.min"] <- "Min_TotalSOC_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V1.max"] <- "Max_TotalSOC_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V2.mean"] <- "DPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V2.min"] <- "Min_DPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V2.max"] <- "Max_DPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V3.mean"] <- "RPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V3.min"] <- "Min_RPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V3.max"] <- "Max_RPM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V4.mean"] <- "BIO_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V4.min"] <- "Min_BIO_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V4.max"] <- "Max_BIO_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V5.mean"] <- "HUM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V5.min"] <- "Min_HUM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V5.max"] <- "Max_HUM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V6.mean"] <- "IOM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V6.min"] <- "Min_IOM_stock_tha"
colnames (Glen_RothC_mean2) [colnames(Glen_RothC_mean2)== "V6.max"] <- "Max_IOM_stock_tha"

Glen_RothC_mean2$Modelling <- factor(Glen_RothC_mean2$Modelling, levels = c("PI_dist", "FH_acc"))
Model_settings <- c(PI_dist = "Step 1", FH_acc = "Step 2")
Grassland_affor <- Glen_RothC_mean2[which (Glen_RothC_mean2$Land_use == "Grassland"), ]
Forest <- Glen_RothC_mean2[which (Glen_RothC_mean2$Land_use == "Forest "), ]
head(Glen_RothC_mean2)
library(ggplot2)
#these graphs show just PI distibution influence
RothC_ToT_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = TotalSOC_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c( "black"), labels = c("Grassland afforestation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_TotalSOC_stock_tha, ymin = Min_TotalSOC_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("Total SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 525)


RothC_ToT_SOC2

RothC_DPM_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = DPM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Grassland afforestation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_DPM_stock_tha, 
                                           ymin = Min_DPM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("DPM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 2.5)


RothC_DPM_SOC2

RothC_RPM_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = RPM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Grassland afforestation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_RPM_stock_tha, 
                                           ymin = Min_RPM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("RPM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 250)
RothC_RPM_SOC2

RothC_BIO_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = BIO_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c( "black"), labels = c("Grasslnad afforestation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_BIO_stock_tha, 
                                           ymin = Min_BIO_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("BIO SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 12)
RothC_BIO_SOC2
#the break at 0 years in grassland graph is not a mistake, because it shows two different simmulations grassland -10 to -1 and forest from 0 years
#I did not make any assumptions and made a transition time, to allow intermediate state between forest and grassland

Glen_RothC_main2$Pit_number <- as.factor(Glen_RothC_main2$Pit_number)

RothC_HUM_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = HUM_stock_tha, colour = Land_use)) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Grassland afforestation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_HUM_stock_tha,
                                           ymin = Min_HUM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("HUM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 250)
RothC_HUM_SOC2

RothC_IOM_SOC2 <- ggplot(Grassland_affor, aes(x = Year, y = IOM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Grassland afforesation"))+
  geom_ribbon(data = Grassland_affor, aes(x = Year, ymax = Max_IOM_stock_tha,
                                           ymin = Min_IOM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("IOM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 40)

RothC_IOM_SOC2 #no changes in grassland affostation is due to an assumption that accummulation peat layer has 0 IOM

library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

grid_arrange_shared_legend(RothC_ToT_SOC2, RothC_RPM_SOC2, RothC_DPM_SOC2, RothC_BIO_SOC2, RothC_HUM_SOC2, RothC_IOM_SOC2, ncol = 2, nrow = 3)


#these grapgs show just PI distibution influence
RothC_ToT_SOC3 <- ggplot(Forest, aes(x = Year, y = TotalSOC_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c( "black"), labels = c("Forest growth"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_TotalSOC_stock_tha, ymin = Min_TotalSOC_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("Total SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 525)


RothC_ToT_SOC3

RothC_DPM_SOC3 <- ggplot(Forest, aes(x = Year, y = DPM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Forest growth"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_DPM_stock_tha, 
                                          ymin = Min_DPM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("DPM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 2.5)


RothC_DPM_SOC3

RothC_RPM_SOC3 <- ggplot(Forest, aes(x = Year, y = RPM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Forest grawth"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_RPM_stock_tha, 
                                          ymin = Min_RPM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("RPM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 250)

RothC_RPM_SOC3

RothC_BIO_SOC3 <- ggplot(Forest, aes(x = Year, y = BIO_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c( "black"), labels = c("Forest growth"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_BIO_stock_tha, 
                                          ymin = Min_BIO_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="", y = expression(paste("BIO SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 12)
RothC_BIO_SOC3
#the break at 0 years in grassland graph is not a mistake, because it shows two different simmulations grassland -10 to -1 and forest from 0 years
#I did not make any assumptions and made a transition time, to allow intermediate state between forest and grassland

Glen_RothC_main2$Pit_number <- as.factor(Glen_RothC_main2$Pit_number)

RothC_HUM_SOC3 <- ggplot(Forest, aes(x = Year, y = HUM_stock_tha, colour = Land_use)) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Forest"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_HUM_stock_tha,
                                          ymin = Min_HUM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("HUM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 250)
RothC_HUM_SOC3

RothC_IOM_SOC3 <- ggplot(Forest, aes(x = Year, y = IOM_stock_tha, colour = factor(Land_use))) +
  geom_line(size= 2)+
  scale_color_manual(values=c("black"), labels = c("Forest growth"))+
  geom_ribbon(data = Forest, aes(x = Year, ymax = Max_IOM_stock_tha,
                                          ymin = Min_IOM_stock_tha), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("IOM SOC","  ", "Mg C ha"^"-1")))+
  facet_grid(Modelling ~ ., labeller = labeller(Modelling = Model_settings))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")+
  labs(color='Simulation')+
  ylim(0, 40)

RothC_IOM_SOC3 #no changes in grassland affostation is due to an assumption that accummulation peat layer has 0 IOM

library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

grid_arrange_shared_legend(RothC_ToT_SOC3, RothC_RPM_SOC3, RothC_DPM_SOC3, RothC_BIO_SOC3, RothC_HUM_SOC3, RothC_IOM_SOC3, ncol = 2, nrow = 3)


#Oh layers analysis

Glen_RothC_F_H <- read.table("C:\\R_game\\GLEN\\F_H_layer_afforestation_RothC.txt", header=TRUE,sep="\t")



library(ggplot2)
RothC_FH_SOC <- ggplot(Glen_RothC_F_H, aes(x = Year, y = Total_C_Stock_tha, colour = factor(Peat_amount)))+
  geom_point(alpha=1) +
  scale_color_manual(values = c("grey11", "#bdbdbd", "#636363"), name= "Plant input changes",
                     labels = c("Average", 
                                "-50% of plant inputs",
                                "+50% of plant inputs"))+
  # facet_grid(Layer_name ~ .)+
  labs( x="Year", y = expression(paste("Total SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")


RothC_FH_SOC

#Modifyed tables plus minus 50% will be in a grey zone as uncertainty illustartion
Glen_RothC_F_H2 <- read.table("C:\\R_game\\GLEN\\F_H_layer_afforestation_RothC_modif.txt", header=TRUE,sep="\t")
head(Glen_RothC_F_H2)

Glen_RothC_F_H2mean <- aggregate(cbind(Glen_RothC_F_H2$DPM_pool_accummulated_SOC,
                                       Glen_RothC_F_H2$RPM_pool_accummulated_SOC,
                                       Glen_RothC_F_H2$BIO_pool_accummulated_SOC,
                                       Glen_RothC_F_H2$HUM_pool_accummulated_SOC,
                                       Glen_RothC_F_H2$Total_SOC_accummulated,
                                       Glen_RothC_F_H2$SOC_stock_with_accummulation),
                                 by= list(Peat_amount = Glen_RothC_F_H2$Peat_amount,
                                          Layer = Glen_RothC_F_H2$Layer,
                                          Year = Glen_RothC_F_H2$Year),
                                 FUN = function(x) c(mean = mean(x)))

Glen_RothC_F_H2mean<- do.call(data.frame, Glen_RothC_F_H2mean)
head(Glen_RothC_F_H2mean)
#rename columns
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V1"] <- "DPM_pool_accummulated_SOC"
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V2"] <- "RPM_pool_accummulated_SOC"
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V3"] <- "BIO_pool_accummulated_SOC"
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V4"] <- "HUM_pool_accummulated_SOC"
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V5"] <- "Total_SOC_accummulated"
colnames (Glen_RothC_F_H2mean) [colnames(Glen_RothC_F_H2mean)== "V6"] <- "SOC_stock_with_accummulation"

Glen_RothC_F_H2mean_SOC <- Glen_RothC_F_H2mean[c(-4,-5, -6, -7, -8)]
library(tidyr)
Glen_RothC_F_H2mean_SOC_total <- spread(Glen_RothC_F_H2mean_SOC, Peat_amount, SOC_stock_with_accummulation)

library(ggplot2)
tiff("SOC_plus_minus.tiff", units="mm", width=180, height=180, res=300)
# insert ggplot code

RothC_FH_SOC2 <- ggplot(Glen_RothC_F_H2mean_SOC_total, aes(x = Year, y = Mean, colour = factor(Layer)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermenting", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean_SOC_total, aes(x = Year, ymax = Plus50,
                                          ymin = Minus50), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("Total SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_SOC2 
dev.off()


RothC_FH_DPM2 <- ggplot(Glen_RothC_F_H2mean, aes(x = Year, y = DPM_SOC_stock_tha_with_FH_cummulative_changes, colour = factor(Layer_name)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_DPM_SOC_stock_tha_with_FH_cummulative_changes,
                                          ymin = Min_DPM_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("DPM SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_DPM2

RothC_FH_RPM2 <- ggplot(Glen_RothC_F_H2mean, aes(x = Year, y = RPM_SOC_stock_tha_with_FH_cummulative_changes, colour = factor(Layer_name)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_RPM_SOC_stock_tha_with_FH_cummulative_changes,
                                          ymin = Min_RPM_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("RPM SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_RPM2

RothC_FH_BIO2 <- ggplot(Glen_RothC_F_H2mean, aes(x = Year, y = BIO_SOC_stock_tha_with_FH_cummulative_changes, colour = factor(Layer_name)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_BIO_SOC_stock_tha_with_FH_cummulative_changes,
                                              ymin = Min_BIO_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("BIO SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_BIO2

RothC_FH_HUM2 <- ggplot(Glen_RothC_F_H2mean, aes(x = Year, y = HUM_SOC_stock_tha_with_FH_cummulative_changes, colour = factor(Layer_name)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_HUM_SOC_stock_tha_with_FH_cummulative_changes,
                                              ymin = Min_HUM_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("HUM SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_HUM2

RothC_FH_IOM2 <- ggplot(Glen_RothC_F_H2mean, aes(x = Year, y = IOM_SOC_stock_tha_with_FH_cummulative_changes, colour = factor(Layer_name)))+
  geom_line(size= 2)+
  scale_color_manual(values=c("#636363", "black"), name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_IOM_SOC_stock_tha_with_FH_cummulative_changes,
                                              ymin = Min_IOM_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("IOM SOC","  ", "t ha"^"-1")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()+
  theme(legend.position="bottom")
RothC_FH_IOM2 # NOT INTERESTING as I have no values here. Assumption is that there is no time to form IOM.

head(Glen_RothC_F_H2mean)
library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}

grid_arrange_shared_legend(RothC_FH_SOC2,  RothC_FH_RPM2, RothC_FH_DPM2, RothC_FH_BIO2, RothC_FH_HUM2, RothC_FH_IOM2, ncol = 2, nrow = 3)

sstate_and_pools <- Glen_RothC_mean2 [which(Glen_RothC_mean2$Year== "-10"|Glen_RothC_mean2$Year== "199"),]
min_max_pools <- aggregate(cbind(Glen_RothC_mean2$TotalSOC_stock_tha, Glen_RothC_mean2$DPM_stock_tha,
                                 Glen_RothC_mean2$RPM_stock_tha, Glen_RothC_mean2$BIO_stock_tha, 
                                 Glen_RothC_mean2$HUM_stock_tha, Glen_RothC_mean2$IOM_stock_tha),
                           by = list (Land_use = Glen_RothC_mean2$Land_use,
                                      Modelling = Glen_RothC_mean2$Modelling),
                           FUN = function(x) c(min = min(x),
                                                max = max (x)))
min_max_pools <- do.call(data.frame, min_max_pools)
                           
write.table(min_max_pools, "min_max_pools.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
write.table(sstate_and_pools, "sstate_and_pools.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
write.table(Glen_RothC_mean2, "Glen_RothC_mean2.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)
write.table(Glen_RothC_F_H2mean, "Glen_RothC_F_H2mean.txt", append=FALSE, sep="\t", row.names=FALSE, col.names = TRUE)

#PI distribution illustration
PI_distribution<- read.table("C:\\R_game\\GLEN\\PI_distribution.txt", header=TRUE,sep="\t")
library(ggplot2)
tiff("PI_distribu.tiff", units="mm", width=85, height=85, res=300)
PI_distribu <- ggplot(PI_distribution, aes(x = Age, y = Live_vol_Prop))+
  geom_line(size= 2)+
  scale_color_manual(values=c( "black"))+
                     #, name= "Peaty soil layers", labels = c("Fermented peat", "Humified peat"))+
  #geom_ribbon(data = Glen_RothC_F_H2mean, aes(x = Year, ymax = Max_IOM_SOC_stock_tha_with_FH_cummulative_changes,
                                            #  ymin = Min_IOM_SOC_stock_tha_with_FH_cummulative_changes), alpha = 0.3, fill = "grey")+
  labs( x="Year", y = expression(paste("Proportion of living biomass")))+
  theme(axis.text=element_text(size=14),   
        axis.title=element_text(size=15))+
  theme(legend.title=element_text(size=15),
        legend.text=element_text(size=15))+
  theme_bw()#+
  #theme(legend.position="bottom")
PI_distribu
dev.off()
