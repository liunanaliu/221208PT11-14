#20220303standard curve nanaliu
library(ggplot2)
library(ggpubr)
library(plyr)
library(reshape2)
library(ggpmisc)
stand <- X20220223_second_animal_experiment_for_supplement
stan <- stand[2:10,1:7]
p=ggplot(stan,aes(log10_CN,mean_ct))+
  geom_point()+
  geom_smooth(method = "lm",se=T,show.legend = F)+
  theme_bw()+
  labs(x="log_concentraion",y="Ct values")
p

p=ggplot(stan,aes(log10_CN,mean_ct))+
  geom_point()+
  theme_bw()

p+ stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', 
            label.x.npc = 'left', label.y.npc = 'top', size = 2.7, show.legend = FALSE)

p + geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 2.7)


#linear analysis for the standard curve of qPCR 03.02/na
cor()
myfit15=lm(in_15ul_react~sample,data = standard_curve_for_qpcr0302)
summary(myfit155)
plot(standard_curve_for_qpcr0302$sample,standard_curve_for_qpcr0302$in_15ul_react,
     xlab="sample concentration", ylab="in_15ul_reaction")
abline(myfit15)

myfit20=lm(in_20ul_react~sample,data = standard_curve_for_qpcr0302)
summary(myfit20)
plot(standard_curve_for_qpcr0302$sample,standard_curve_for_qpcr0302$in_20ul_react,
     xlab="sample concentration", ylab="in_20ul_reaction")
abline(myfit20)

#add log(sample)
standard_curve_for_qpcr0302$log_sample=log(standard_curve_for_qpcr0302$sample)

myfit15log=lm(in_15ul_react~log_sample,data = standard_curve_for_qpcr0302)
summary(myfit15log)
plot(standard_curve_for_qpcr0302$log_sample,standard_curve_for_qpcr0302$in_15ul_react,
     xlab="sample log concentration", ylab="in_15ul_reaction")
abline(myfit15log)

myfit20log=lm(in_20ul_react~log_sample,data = standard_curve_for_qpcr0302)
summary(myfit20log)
plot(standard_curve_for_qpcr0302$log_sample,standard_curve_for_qpcr0302$in_20ul_react,
     xlab="sample log concentration", ylab="in_20ul_reaction")
abline(myfit20log)
#or
abline(lm(in_20ul_react~log_sample,data = standard_curve_for_qpcr0302))

#add r2, combined the two lines
cor.test(standard_curve_for_qpcr0302$log_sample,
         standard_curve_for_qpcr0302$in_15ul_react)
cor.test(standard_curve_for_qpcr0302$log_sample,
         standard_curve_for_qpcr0302$in_20ul_react)

library(ggplot2)
library(ggpubr)

library(MASS)
data(Boston)
library(reshape2)
standard_curve=melt(standard_curve_for_qpcr0302,
                    id=c("sample","log_sample"))
library(plyr)
standard_curve=rename(standard_curve,c(variable="reaction_group",
                                       value="ct_values"))
#https://www.cxyzjd.com/article/weixin_39641173/111172294
p=ggplot(standard_curve,aes(log_sample,ct_values))+
  geom_point(aes(color=reaction_group))+
  geom_smooth(aes(color=reaction_group), method = "lm",se=T,show.legend = F)+
  theme_bw()+
  labs(x="log_concentraion",y="Ct values")
p
library(ggpubr)
#https://www.modb.pro/db/137780
BiocManager::install("ggpmisc")
library(ggpmisc)
p=ggplot(standard_curve,aes(log_sample,ct_values,
                            color=reaction_group,group=reaction_group))+
  geom_point()+
  theme_bw()

p+ stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = '~`,`~')), method = 'spearman', 
            label.x.npc = 'left', label.y.npc = 'top', size = 2.7, show.legend = FALSE)

p + geom_smooth(method = 'lm', formula = y~x, se = TRUE, show.legend = FALSE) +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., stat(p.value.label), sep = '~`,`~')),
               formula = y~x, parse = TRUE, label.x.npc = 'left', label.y.npc = 'top', size = 2.7)
