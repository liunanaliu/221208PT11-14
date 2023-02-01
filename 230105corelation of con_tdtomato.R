##230105add corelation of td expression to rna template usage with different concentration
###221208qpcr_pt11_14_analysis_1.r for more details
library(ggpubr)
library(ggplot2)
tdtomato_ct$log2_RNA_amount<- log2(tdtomato_ct$`mRNA template / reaction`)
ggplot(tdtomato_ct,aes(`mRNA template / reaction`, HPRT))+
  geom_point(aes(color=factor(Formulation)))+
  theme_classic()+
  #scale_x_continuous(trans = 'log2')+
  geom_smooth(method = 'lm',formula = y~log2(x),
              se=F)
##
ggscatter(tdtomato_ct,x='mRNA template / reaction', y='HPRT',
          add = 'reg.line')+
  stat_cor(label.x = 200, label.y = 24)+
  stat_regline_equation(label.x = 200,label.y = 23.6)

ggsave('230107correlation_con_ct.tiff',units = 'cm',
       width = 26, height = 14,
       dpi=600, compression='lzw')

ggscatter(tdtomato_ct,x='log2_RNA_amount', y='HPRT',
          add = 'reg.line')+
  stat_cor(label.x = 7, label.y = 24)+
  stat_regline_equation(label.x = 7,label.y = 23.6)


ggsave('230109correlation_log2con_ct.tiff',units = 'cm',
       width = 26, height = 14,
       dpi=600, compression='lzw')
plot(tdtomato_ct$`mRNA template / reaction`,tdtomato_ct$HPRT)
#how to add a slope2
plot(tdtomato_ct$`mRNA template / reaction`,tdtomato_ct$TdTomato)
tdtomato_ct$Formulation
