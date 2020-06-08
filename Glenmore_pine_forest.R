
###Glenmore pine chronosequence sampling data and statistical analysis###
###Scotlad###

setwd("C:\\R_game\\GLEN")

####
#### Statistical analysis of total SOC and N####

Glen_eq_mass_1persite <- readRDS("Glen_eq_mass_1persite.rds")


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

Glen_min <- readRDS("Glen_min00.rds")
Glen_org <- readRDS("Glen_org00.rds")

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
coef(summary(Glen_org_SOC_lm5))


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
