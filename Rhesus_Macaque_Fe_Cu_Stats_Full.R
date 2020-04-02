#Testing differences in Fe and Cu among mammal species 

#Cu Mammal Means

Mammal_Means_Full_Cu <- read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Cu csv files/Mammal_Cu_Raw.csv")

kruskal.test(del_65_Cu ~ Species, data = Mammal_Means_Full_Cu)

Kruskal-Wallis rank sum test

data:  del_65_Cu by Species
Kruskal-Wallis chi-squared = 47.05, df = 8, p-value = 1.5e-07

#Fe Mammal Means 

Mammal_Means_Full_Fe <- read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Fe csv files/Mammal_Fe_Raw.csv")

kruskal.test(del_56_Fe ~ Species, data = Mammal_Means_Full_Fe)

Kruskal-Wallis rank sum test

data:  del_56_Fe by Species
Kruskal-Wallis chi-squared = 41.637, df = 9, p-value = 3.829e-06

#Visualizing Mammal Means Cu
library(ggplot2)
library(gridExtra)
Mammal_Means_Bone_Cu = read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Cu csv files/Mammal Means Bone Cu.csv")

Comparison_of_delta_65_Cu_Fractionation_in_Mammals <- ggplot(Mammal_Means_Bone_Cu,aes(y = Mean_del_65_Cu, x = Species)) + 
  ggtitle(bquote('Comparison of '*delta ^65*'Cu in Mammals')) + xlab("Species") + ylab(bquote('Mean of '*delta ^65*'Cu Fractionation')) + 
  geom_errorbar(aes(ymin = Mean_del_65_Cu-Stdev_del_65_Cu, ymax = Mean_del_65_Cu+Stdev_del_65_Cu))+ geom_point(size = 2.5)+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ coord_flip()

#Visualizing Mammal Means Fe

Mammal_Means_Bone_Fe =read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Fe csv files/Mammal Means Fe.csv")

Comparison_of_delta_56_Fe_Fractionation_in_Mammals <- ggplot(Mammal_Means_Bone_Fe,aes(y = Mean_del_56_Fe, x = Species)) + 
  ggtitle(bquote('Comparison of '*delta ^56*'Fe in Mammals')) + xlab("Species") + ylab(bquote('Mean of '*delta ^56*'Fe Fractionation')) + 
  geom_errorbar(aes(ymin = Mean_del_56_Fe-Stdev_del_56_Fe, ymax = Mean_del_56_Fe+Stdev_del_56_Fe))+ geom_point(size = 2.5)+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ coord_flip()

#Testing differences in Fe and Cu in Humans and Rhesus Macaques 

Human_Cu_Data =read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Cu csv files/Human Cu Data - Final .csv")
#human Fe
Human_Fe_Data=read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Fe csv files/Human Fe Data - Final .csv")
#macaque Occipital Cu
Rhesus_Macaque_Occipital_Cu=read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Cu csv files/Rhesus Macaque Occipital Cu.csv")
#macaque teeth Cu
Rhesus_Macaque_Teeth_Cu = read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Cu csv files/Rhesus Macaque Teeth Cu.csv")
#macaque occipital Fe
Rhesus_Macaque_Occipital_Fe =read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Fe csv files/Rhesus Macaques Occipital Fe.csv")
#macaque teeth Fe
Rhesus_Macaque_Teeth_Fe = read.csv("C:/Users/Renee/Desktop/Rutgers University/Senior Thesis/Senior Thesis/Lab Notebook/Data/Used Fe csv files/Rhesus Macaques Teeth Fe.csv")

# Visualizing Human and Rhesus Macaques Cu in Bone 

Variation_of_delta_65_Cu_in_Human_Bone <- ggplot(Human_Cu_Data, aes(x = Sex, y = del_65_Cu, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^65*'Cu in Human Bone')) + xlab("Sex") +
  ylab(bquote(delta ^65*'Cu')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + 
  scale_colour_manual(values = c("male" = "black", "female" = "red"))

Variation_of_delta_65_Cu_in_Rhesus_Macaque_Bone <- ggplot(Rhesus_Macaque_Occipital_Cu, aes(x = Sex, y = del_65_Cu, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^65*'Cu in Rhesus Macaque Bone')) + xlab("Sex") +
  ylab(bquote(delta ^65*'Cu')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  scale_colour_manual(values = c("male" = "black", "female" = "red"))


grid.arrange(Variation_of_delta_65_Cu_in_Rhesus_Macaque_Bone,Variation_of_delta_65_Cu_in_Human_Bone, nrow = 1, ncol = 2)

#Visualizing Rhesus macaques Cu in Teeth and Occipital

Variation_of_delta_65_Cu_in_Rhesus_Macaque_Teeth <- ggplot(Rhesus_Macaque_Teeth_Cu, aes(x = Sex, y = del_65_Cu, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta^65*'Cu in Rhesus Macaque Teeth')) + xlab("Sex") +
  ylab(bquote(delta^65*'Cu')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  scale_colour_manual(values = c("male" = "black", "female" = "red"))

Variation_of_delta_65_Cu_in_Rhesus_Macaque_Occipital <- ggplot(Rhesus_Macaque_Occipital_Cu, aes(x = Sex, y = del_65_Cu, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^65*'Cu in Rhesus Macaque Occipital')) + xlab("Sex") +
  ylab(bquote(delta ^65*'Cu')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) + scale_colour_manual(values = c("male" = "black","female" = "red"))

grid.arrange(Variation_of_delta_65_Cu_in_Rhesus_Macaque_Teeth,Variation_of_delta_65_Cu_in_Rhesus_Macaque_Occipital, nrow = 1, ncol = 2)

#wilcoxins to test for significance of Cu variation between humans and macaques 

#human bone cu
wilcox.test(Human_Cu_Data$del_65_Cu ~ Human_Cu_Data$Sex, data = Human_Cu_Data)

Wilcoxon rank sum test with continuity correction

data:  Human_Cu_Data$del_65_Cu by Human_Cu_Data$Sex
W = 60.5, p-value = 0.04609
alternative hypothesis: true location shift is not equal to 0
#macaque bone cu
wilcox.test(Rhesus_Macaque_Occipital_Cu$del_65_Cu ~ Rhesus_Macaque_Occipital_Cu$Sex, data = Rhesus_Macaque_Occipital_Cu)

Wilcoxon rank sum test

data:  Rhesus_Macaque_Occipital_Cu$del_65_Cu by Rhesus_Macaque_Occipital_Cu$Sex
W = 8, p-value = 0.001451
alternative hypothesis: true location shift is not equal to 0
#macaque teeth cu 
wilcox.test(Rhesus_Macaque_Teeth_Cu$del_65_Cu ~ Rhesus_Macaque_Teeth_Cu$Sex, data = Rhesus_Macaque_Teeth_Cu)

Wilcoxon rank sum test

data:  Rhesus_Macaque_Teeth_Cu$del_65_Cu by Rhesus_Macaque_Teeth_Cu$Sex
W = 48, p-value = 0.9118
alternative hypothesis: true location shift is not equal to 0

#Visualizing differences in bone Fe between humans and macaques

#macaques Fe in bone and teeth

Variation_of_delta_56_Fe_in_Rhesus_Macaque_Occipital <- ggplot(Rhesus_Macaque_Occipital_Fe, aes(x = Sex, y = del_56_Fe, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^56*'Fe in Rhesus Macaque Occipital')) + xlab("Sex") +
  ylab(bquote(delta ^56*'Fe')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  scale_colour_manual(values = c("male" = "black", "female" = "red"))

Variation_of_delta_56_Fe_in_Rhesus_Macaque_Teeth <- ggplot(Rhesus_Macaque_Teeth_Fe, aes(x = Sex, y = del_56_Fe, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^56*'Fe in Rhesus Macaque Teeth')) + xlab("Sex") +
  ylab(bquote(delta ^56*'Fe')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ 
  scale_colour_manual(values = c("male" = "black", "female" = "red"))

grid.arrange(Variation_of_delta_56_Fe_in_Rhesus_Macaque_Teeth,Variation_of_delta_56_Fe_in_Rhesus_Macaque_Occipital, nrow = 1, ncol = 2)

#fe in human and macaque bone 

Variation_of_delta_56_Fe_in_Human_Bone <- ggplot(Human_Fe_Data, aes(x = Sex, y = del_56_Fe, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^56*'Fe in Human Bone')) + xlab("Sex") +
  ylab(bquote(delta ^56*'Fe')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  scale_colour_manual(values = c("male" = "black", "female" = "red"))

Variation_of_delta_56_Fe_in_Rhesus_Macaque_Bone <- ggplot(Rhesus_Macaque_Occipital_Fe, aes(x = Sex, y = del_56_Fe, color = Sex)) +
  geom_boxplot() + ggtitle(bquote(delta ^56*'Fe in Rhesus Macaque Occipital')) + xlab("Sex") +
  ylab(bquote(delta ^56*'Fe')) + guides(color=guide_legend(title="Sex")) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  scale_colour_manual(values = c("male" = "black", "female" = "red"))


grid.arrange(Variation_of_delta_56_Fe_in_Rhesus_Macaque_Bone,Variation_of_delta_56_Fe_in_Human_Bone, nrow = 1, ncol = 2)

#wilcoxins for significance in Fe groups 

#Macaque Fe teeth

wilcox.test(Rhesus_Macaque_Teeth_Fe$del_56_Fe ~ Rhesus_Macaque_Teeth_Fe$Sex, data = Rhesus_Macaque_Teeth_Fe)

Wilcoxon rank sum test

data:  Rhesus_Macaque_Teeth_Fe$del_56_Fe by Rhesus_Macaque_Teeth_Fe$Sex
W = 66, p-value = 0.09472
alternative hypothesis: true location shift is not equal to 0

#Macaque Fe Occipital
wilcox.test(Rhesus_Macaque_Occipital_Fe$del_56_Fe ~ Rhesus_Macaque_Occipital_Fe$Sex, data = Rhesus_Macaque_Occipital_Fe)

Wilcoxon rank sum test

data:  Rhesus_Macaque_Occipital_Fe$del_56_Fe by Rhesus_Macaque_Occipital_Fe$Sex
W = 68, p-value = 0.1903
alternative hypothesis: true location shift is not equal to 0

#Human Fe bone
wilcox.test(Human_Fe_Data$del_56_Fe ~ Human_Fe_Data$Sex, data = Human_Fe_Data)


Wilcoxon rank sum test with continuity correction

data:  Human_Fe_Data$del_56_Fe by Human_Fe_Data$Sex
W = 309.5, p-value = 0.005169
alternative hypothesis: true location shift is not equal to 0

#Modelling the success of age and/or sex predicting Cu and Fe values in teeth and bone

library(mgcv)
library(gratia)

#Plot that incorporates raw rhesus macaque vs age with GAM

Model_3a = gam(del_65_Cu~s(Age, k = 6) + s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3b = gam(del_65_Cu~s(Age, k = 6) + te(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3c = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3d = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Cu)
Model_3e = gam(del_65_Cu~te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Cu)

Model_3f = gam(del_65_Cu~s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)

AIC(Model_3a,Model_3b,Model_3c,Model_3d,Model_3e,Model_3f) 

##Model_3d wins

Family: gaussian 
Link function: identity 

Formula:
  del_65_Cu ~ s(Age, k = 6) + te(Age, by = Sex, k = 6)

Parametric coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -1.5237     0.2116  -7.202 1.11e-05 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
  edf Ref.df      F p-value   
s(Age)            2.8085 3.2265  6.812 0.00585 **
  te(Age):Sexfemale 2.7104 3.1755  1.606 0.13144   
te(Age):Sexmale   0.5243 0.5243 16.238 0.01263 * 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Rank: 15/16
R-sq.(adj) =  0.683   Deviance explained =   79%
GCV = 0.73056  Scale est. = 0.45975   n = 19

###make plot for model 3d####
library(visreg)
p.crude <- visreg(Model_3d,  scale='response', "Age", by="Sex",line.par = list(col = 'black'),gg=TRUE)
p.crude+geom_point(data=Rhesus_Macaque_Occipital_Cu,aes(x=Age,y=del_65_Cu,group=Sex,color=Sex)) +theme_classic() +
  guides(color=guide_legend(title = "Sex")) + scale_colour_manual(values = c("male" = "black", "female" = "red"))

#alternative model for occipital vs sex and age (used in senior thesis)
Model_3 = gam(del_65_Cu~s(Age, k = 6) + s(Age, Sex, bs = "fs", k = 6) + Sex,data = Rhesus_Macaque_Occipital_Cu) #model is significant, OG paper
summary(Model_3)

#AIC for models 


Model_3a = gam(del_65_Cu~s(Age, k = 6) + s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3b = gam(del_65_Cu~s(Age, k = 6) + te(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3c = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
Model_3d = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Cu)
Model_3e = gam(del_65_Cu~te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Cu)

Model_3f = gam(del_65_Cu~s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)

AIC(Model_3a,Model_3b,Model_3c,Model_3d,Model_3e,Model_3f) #Model 3d wins


Model_6a = gam(del_56_Fe~s(Age, k = 6) + s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Fe)
Model_6b = gam(del_56_Fe~s(Age, k = 6) + te(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Fe)
Model_6c = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Occipital_Fe)
Model_6d = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Fe)
Model_6e = gam(del_56_Fe~te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Fe)

Model_6f = gam(del_56_Fe~s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Occipital_Fe)

AIC(Model_6a,Model_6b,Model_6c,Model_6d,Model_6e,Model_6f) #Model 6d wins

Model_Fa = gam(del_56_Fe~s(Age, k = 6) + s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Fe)
Model_Fb = gam(del_56_Fe~s(Age, k = 6) + te(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Fe)
Model_Fc = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Teeth_Fe)
Model_Fd = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Teeth_Fe)
Model_Fe = gam(del_56_Fe~te(Age, by=Sex,k=6),data = Rhesus_Macaque_Teeth_Fe)

Model_Ff = gam(del_56_Fe~s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Fe)

AIC(Model_Fa,Model_Fb,Model_Fc,Model_Fd,Model_Fe,Model_Ff) #Model Fb wins

Model_C1 = gam(del_65_Cu~s(Age, k = 6) + s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Cu)
Model_C2 = gam(del_65_Cu~s(Age, k = 6) + te(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Cu)
Model_C3 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Teeth_Cu)
Model_C4 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Teeth_Cu)
Model_C5 = gam(del_65_Cu~te(Age, by=Sex,k=6),data = Rhesus_Macaque_Teeth_Cu)

Model_C6 = gam(del_65_Cu~s(Age,Sex, bs="fs",k=6) + Sex,data = Rhesus_Macaque_Teeth_Cu)

AIC(Model_C1,Model_C2,Model_C3,Model_C4,Model_C5,Model_C6) #Model C1 wins

#Conservative Models for age and sex are independent variables influencing delta fe and cu values in macaques 

Model_3c = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Occipital_Cu)
summary(Model_3c) #not significant
Model_6d = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex, k = 6) + Sex,data = Rhesus_Macaque_Occipital_Fe)
summary(Model_6d) #not significant
Model_6c = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Occipital_Fe) #same as 6d
summary(Model_6c)
Model_Fb = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex, k = 6) + Sex,data = Rhesus_Macaque_Teeth_Fe)
summary(Model_Fb) #significant 
Model_Fc = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Teeth_Fe) #same as Fb
summary(Model_Fc)
Model_C4 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex, k = 6) + Sex,data = Rhesus_Macaque_Teeth_Cu)
summary(Model_C4)#not significant 
Model_C3 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6) + Sex,data = Rhesus_Macaque_Teeth_Cu) #model in paper, same as C4
summary(Model_C3)
#without Sex as an independent variable, less conservative models 
Model_1 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex,k=6),data = Rhesus_Macaque_Occipital_Cu)
summary(Model_1) #significant, same as 3d, significant for age interaction and male and age interaction 
Model_2 = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex, k = 6),data = Rhesus_Macaque_Occipital_Fe)
summary(Model_2) #significant for age and male sex interaction
Model_3 = gam(del_56_Fe~s(Age, k = 6) + te(Age, by=Sex, k = 6),data = Rhesus_Macaque_Teeth_Fe)
summary(Model_3) #significant for age, like model Fb
Model_4 = gam(del_65_Cu~s(Age, k = 6) + te(Age, by=Sex, k = 6),data = Rhesus_Macaque_Teeth_Cu)
summary(Model_4) #not significant 

#Visualizing Model Fb

ggplot(Rhesus_Macaque_Teeth_Fe,aes(x=Age,y=del_56_Fe))+geom_smooth(method="gam",formula=y~s(x),color="black")+
  geom_point(color="black")+
  xlab("Age")+ylab(bquote(delta ^56*'Fe'))+ggtitle(bquote('Age and '*delta ^56*'Fe in Teeth')) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())  
