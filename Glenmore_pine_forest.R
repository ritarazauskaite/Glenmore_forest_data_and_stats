
###Glenmore pine chronosequence sampling data and statistical analysis###
###Scotlad###

setwd("C:\\R_game\\GLEN")

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
# Glen_org_SOCplot <- ggplot(Glen_org, aes( x=Horizon, y=Glen_hor4_no_litter)) +
#   theme_bw()+
#   geom_point( data = Glen_org,aes(fill= Age), 
#               stat = 'identity',
#               colour="black",pch=21,
#               position = position_dodge(width = .4),
#               size=3)+
#   #geom_hline(yintercept=6, linetype="dashed", color = "red")+
#   coord_cartesian(ylim = c(0, 310))+
#   labs( x=" Horizon", y = expression(paste("SOC","  ", "t ha"^"-1")))+
#   theme(axis.text=element_text(size=14),
#         axis.title=element_text(size=15))+
#   theme(legend.title=element_text(size=15),
#         legend.text=element_text(size=13))+
#   theme(legend.position=c(0.2,0.8)) +
#   scale_fill_brewer(palette="OrRd", name= "Age_class",
#                     labels = c( 
#                                "Young",
#                                "Mature",
#                                "Ancient"))+
#   scale_y_continuous(expand = c(0, 0))+
#   guides(fill=guide_legend(title="Age class"))
# 
# Glen_org_SOCplot

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
# Glen_depth <- lme(log_Horizon_Width ~  Age + Horizon, random = ~1|Pit_nr,
#                   correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),
# VarCorr(Glen_depth)
# summary(Glen_depth)
# anova(Glen_depth)
# plot(Glen_depth)
# Glen_depth_fitted <- fitted(Glen_depth)
# plot(Glen_min$log_Horizon_Width, Glen_depth_fitted)
# abline(0,1)
# 
# Glen_ph_min_lme <- lme(pH ~ Age + Horizon, random = ~1|Pit_nr,
#                        correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_ph_min_lme)
# summary(Glen_ph_min_lme)
# anova(Glen_ph_min_lme)
# plot(Glen_ph_min_lme)
# Glen_ph_min_lme_fitted <- fitted(Glen_ph_min_lme)
# plot(Glen_min$pH, Glen_ph_min_lme_fitted)
# abline(0,1)
# 
# ## Remove Ah layer as it is not fully measured througout the survey 
# Glen_min_naAh <- Glen_min [!(Glen_min$Horizon =="Ah"),]
# Glen_BD_min <- lme(BD ~ Age + Horizon, random = ~1|Pit_nr,
#                    correlation = corSpher(form = ~LAT+LONG),data = Glen_min_naAh)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_BD_min)
# summary(Glen_BD_min)
# anova(Glen_BD_min)
# plot(Glen_BD_min)
# Glen_BD_fitted <- fitted(Glen_BD_min)
# plot(Glen_min_naAh$BD, Glen_BD_fitted)
# abline(0,1)
# 
# Glen_min$log_Clay <- log(Glen_min$Clay)
# Glen_clay_lme <- lme(log_Clay~Age+Horizon , random = ~1|Pit_nr,
#                      correlation = corSpher(form = ~LAT+LONG),data = Glen_min)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_clay_lme)
# summary(Glen_clay_lme)
# anova(Glen_clay_lme)
# plot(Glen_clay_lme)
# Glen_clay_lme_fitted <- fitted(Glen_clay_lme)
# plot(Glen_min$log_Clay, Glen_clay_lme_fitted)
# abline(0,1)
# 
# ### Organic layer statistical ananlysis width, pH & BD ###
# 
# #add LAT and LONG coordinates to Equivalent_mass_1persite
# Coordinates <- read.table("C:\\R_game\\Glen\\Glen_coord.txt", header=TRUE,sep="\t")
# Glen_hor4$LAT <- NA
# Glen_hor4$LAT <- Coordinates$LAT[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
# Glen_hor4$LONG <- NA
# Glen_hor4$LONG <- Coordinates$LONG[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
# Glen_hor4$ALT <- NA
# Glen_hor4$ALT <- Coordinates$ALT[match (Glen_hor4$Pit_nr, Coordinates$Pit_nr)]
# #jitter is addded to avoid ties as couple of sites are very close to each other 
# Glen_hor4$LAT <- jitter (Glen_hor4$LAT)
# Glen_hor4$LONG <- jitter (Glen_hor4$LONG)
# Glen_hor4$ALT <-jitter (Glen_hor4$ALT)
# 
# #order depth to display the right order in the illustrations
# is.ordered(Glen_hor4$Horizon)
# Glen_org <- Glen_hor4 [which (Glen_hor4$Horizon == "Oh_UPP" | Glen_hor4$Horizon == "Oh_LOW"),]
# 
# #Statistics, testing the best model and data transformation
# 
# head(Glen_org)
# 
# # Order age groups
# is.ordered(Glen_hor4$Age)
# Glen_depth_org <- lme(Horizon_Width~Age+Horizon , random = ~1|Pit_nr,
#                       correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_depth_org)
# summary(Glen_depth_org)
# anova(Glen_depth_org)
# plot(Glen_depth_org)
# Glen_depth_org_fitted <- fitted(Glen_depth_org)
# plot(Glen_org$Horizon_Width, Glen_depth_org_fitted)
# abline(0,1)
# 
# Glen_pH_org <- lme(pH ~ Age + Horizon, random = ~1|Pit_nr,
#                    correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_pH_org)
# summary(Glen_pH_org)
# anova(Glen_pH_org)
# plot(Glen_pH_org)
# Glen_pH_org_fitted <- fitted(Glen_pH_org)
# plot(Glen_org$pH, Glen_pH_org_fitted)
# abline(0,1)
# 
# Glen_BD_org <- lme(newBD2~Age+Horizon , random = ~1|Pit_nr,
#                    correlation = corSpher(form = ~LAT+LONG),data = Glen_org)  #correlation = corSpher(form = ~Lat+Long),
# 
# VarCorr(Glen_BD_org)
# summary(Glen_BD_org)
# anova(Glen_BD_org)
# plot(Glen_BD_org)
# Glen_BD_org_fitted <- fitted(Glen_BD_org)
# plot(Glen_org$newBD2, Glen_BD_org_fitted)
# abline(0,1)

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

# Layer statistical analysis


##### A statstical model for depth and age dependant C t/ha with  BD  
### Random effects are added to adjust model for sample dependance to each otther if they are taken from the same pit

Glen_org$log_SOC <- log(Glen_org$C_tha_Ohnew)
# grasslnad eliminated form this column and coded into v.old as exactly same variable can't be in a code


library(nlme)
citation("nlme")
# Explained well lme https://www.youtube.com/watch?v=VhMWPkTbXoY random effects that are acounted in lme
Glen_org_SOC_lm1 <- lme(log_SOC~Horizon*Age*pH,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "ML")
summary(Glen_org_SOC_lm1)

Glen_org_SOC_lm2 <- lme(log_SOC~Horizon*Age+pH*Horizon+pH*Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "ML")
summary(Glen_org_SOC_lm2)
anova(Glen_org_SOC_lm1, Glen_org_SOC_lm2)

Glen_org_SOC_lm3 <- update(Glen_org_SOC_lm2, .~. -pH*Horizon)
summary(Glen_org_SOC_lm3)
anova(Glen_org_SOC_lm2, Glen_org_SOC_lm3)

Glen_org_SOC_lm4 <- lme(log_SOC~Horizon*Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "ML"
                        ,control = lmeControl(sing.tol=1e-20))
summary(Glen_org_SOC_lm4)
anova(Glen_org_SOC_lm3, Glen_org_SOC_lm4)

Glen_org_SOC_lm5 <- lme(log_SOC~Horizon+Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "ML")
summary(Glen_org_SOC_lm5)
anova(Glen_org_SOC_lm4, Glen_org_SOC_lm5)

Glen_org_SOC_lm6 <- update(Glen_org_SOC_lm5, .~. -Horizon)
summary(Glen_org_SOC_lm6)
anova(Glen_org_SOC_lm5, Glen_org_SOC_lm6)

Glen_org_SOC_lm7 <- update(Glen_org_SOC_lm5, .~. -Age)
summary(Glen_org_SOC_lm7)
anova(Glen_org_SOC_lm5, Glen_org_SOC_lm7)


Glen_org_SOC_lm5 <- lme(log_SOC~Horizon+Age,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data=Glen_org, method = "REML")

VarCorr(Glen_org_SOC_lm5)
summary(Glen_org_SOC_lm5)
anova(Glen_org_SOC_lm5)
plot(Glen_org_SOC_lm5)
plot(ranef(Glen_org_SOC_lm5))
dev.off() # restarts plots vizualization
Glen_org_SOC_lm_fitted <- fitted(Glen_org_SOC_lm5) # extracts fitted values from the linear model
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
Glen_org_SOC_lm5$fitted
Glen_org$fitted.val <- fitted(Glen_org_SOC_lm5, level = 0)
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
Glen_min_SOC_lm0 <- lme(log_SOC~Horizon*Age*Clay*pH,
                        correlation = corSpher( form = ~LAT+LONG ),
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
# too many variables too little independent samples

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
                        random = ~ 1|Pit_nr, data = Glen_min,  method = "ML")  #correlation spherical
summary(Glen_min_SOC_lm9)
anova(Glen_min_SOC_lm4, Glen_min_SOC_lm9)

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