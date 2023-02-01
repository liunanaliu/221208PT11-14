#20221208 qpcr analysis for pt11 to pt14 na

#######230112 new update for qpcr with repeatation
library(readxl)
X20221208_PT11_14_pcr <- read_excel("20221208_PT11_14_pcr.xlsx", 
                                     sheet = "qPCR4", skip = 1)
tdtomato_ct <- X20221208_PT11_14_pcr
library(ggplot2)

tdtomato_ct$group <- paste(tdtomato_ct$Formulation,
                       tdtomato_ct$Incubation,
                       sep = '_')
table(tdtomato_ct$group)

colnames(tdtomato_ct)

###########
tdtomato_ct$group <- factor(tdtomato_ct$group,
                        levels = c('PT_11_6','PT_11_24','PT_11_48',
                                   'PT_12_6','PT_12_24','PT_12_48',
                                   'PT_13_6','PT_13_24','PT_13_48',
                                   'PT_14_6','PT_14_24','PT_14_48'))
###############
library(stringr)
tdtomato_ct$group <- str_replace(tdtomato_ct$group,'PT_','PT')
tdtomato_ct$group <- factor(tdtomato_ct$group,
                            levels = c('PT11_6','PT11_24','PT11_48',
                                       'PT12_6','PT12_24','PT12_48',
                                       'PT13_6','PT13_24','PT13_48',
                                       'PT14_6','PT14_24','PT14_48'))

tdtomato_ct$Sex <- ifelse(tdtomato_ct$Sex=='f','Female','Male')
tdtomato_ct$Sex <- factor(tdtomato_ct$Sex,
                          levels = c('Female','Male'))

save(tdtomato_ct,file = '230113_tdtomato_ct.rda')
tdtomato_ct_left <- tdtomato_ct[tdtomato_ct$tissue=='Muscle_L',]
tdtomato_ct_right <- tdtomato_ct[tdtomato_ct$tissue=='Muscle_R',]

ggplot(tdtomato_ct,aes(group,values,fill=tissue))+
  theme_classic()+
  scale_y_log10()+
  geom_boxplot(aes(color=tissue),#binaxis='y', stackdir='center',
             position=position_dodge(0.75), 
             size=3,alpha=0.3)+
  labs(x="",y="TdTomato expression(CN/ng mRNA)")

colnames(tdtomato_ct)
#######################################230107 same as before PT_9/10
p1a <- ggplot(tdtomato_ct_left, aes(x=group, y=values,fill=tissue)) + 
  geom_bar(stat="summary", color="grey", binwidth=2,
           position=position_dodge2(0.75, preserve = 'single'),
           alpha=.5) +
  theme_classic()+
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank())+
  scale_y_log10()+
  geom_point(aes(color=tissue),binaxis='y', stackdir='center',
             position=position_dodge(0.9, preserve = 'total'), #position = 'jitter',#
             size=3)+
  labs(title = 'Injected muscle expression', 
       x='',y='TdTomato expression(CN/ng mRNA)')+
  scale_color_manual(values = c('Muscle_L'='orange'))+
  scale_fill_manual(values = c('Muscle_L'='orange'))
p1a

p1a_r <- ggplot(tdtomato_ct_right, aes(x=group, y=values,fill=tissue)) + 
  geom_bar(stat="summary", color="grey", binwidth=2,
           position=position_dodge2(0.75, preserve = 'single'),
           alpha=.3) +
  theme_classic()+
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank())+
  scale_y_log10()+
  geom_point(aes(color=tissue),binaxis='y', stackdir='center',
             position=position_dodge(0.9, preserve = 'total'), #position = 'jitter',#
             size=3)+
  labs(title = 'Injected muscle expression', 
       x='',y='TdTomato expression(CN/ng mRNA)')
p1a_r
######## plot 1a
ggsave('230112_Injected_muscle.tiff',units = 'cm',
       width = 26, height = 14,
       dpi=600, compression='lzw')

p1b <- ggplot(tdtomato_ct, aes(x=group, y=values,
                               fill=tissue,color=tissue),
              palette=c('orange','blue')) + 
  geom_bar(stat="summary", color="grey", #width=0.6,#fill=tissue,
           position=position_dodge2(0.75, preserve = 'single'),#control the width of each group
           alpha=.5) +
  theme_classic()+####scale_y_discrete(limits = c(0,NA))+
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank())+
  scale_y_log10()+
 ## scale_y_discrete(limits =  c(0,NA)))+
  labs(title = 'Single muscle expression',#fill='Tissue',
       x='',y='TdTomato expression(CN/ng mRNA)')+
  geom_point(aes(color=tissue),#palette=c('orange','blue'),
             binaxis='y', stackdir='center',
             position=position_dodge(0.75),
             stat = 'identity',#position_dodge(0.75), 
             size=3)+
  scale_color_manual(values = c('Muscle_L'='orange','Muscle_R'='blue'))+
  scale_fill_manual(values = c('Muscle_L'='orange','Muscle_R'='blue'))
p1b

library(ggpubr)
ggarrange(p1b,p1a,ncol = 1,
          common.legend = T,
          labels = c('A','B'),
          font.label = list(size=22,color='red',face='bold'))

ggsave('230112_p1ab.tiff',units = 'cm',
       width = 30, height = 28,
       dpi=600, compression='lzw')
########plot 1b
ggsave('230112Combined_muscle.tiff',units = 'cm',
       width = 26, height = 14,
       dpi=600, compression='lzw')


p1c <- ggplot(tdtomato_ct, aes(x=Incubation, y=values,
                               fill=Formulation,color=Formulation))+ 
  facet_grid(cols = vars(tissue))+
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  scale_y_continuous(limits = c(0,NA))+
  theme_classic()+
  scale_y_log10()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))+
  geom_bar(stat="identity", color="black", 
           #position=position_dodge2(),
           position=position_dodge(preserve = 'total'),alpha=0.5)+
  scale_y_continuous(limits = c(0,NA))+
  #geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD))+
  #stat_smooth(method = 'loess')+
  labs(title='Single muscle expression', 
       x='Incubation time',y='TdTomato expression')
##################################did not work
  geom_point(aes(color=tissue),#palette=c('orange','blue'),
             binaxis='y', stackdir='centered',
             position=position_dodge(0.75),
             stat = 'identity',#position_dodge(0.75), 
             size=3)+
  scale_color_manual(values = c('Muscle_L'='orange','Muscle_R'='blue'))
  ##################################
p1c
##http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
ggsave('230112_single_muscle.tiff',units = 'cm',
       width = 15, height = 14,
       dpi=600, compression='lzw')

p1d <- ggplot(tdtomato_ct, aes(x=Incubation, y=values,fill=Formulation,color=tissue))+ 
  facet_grid(cols = vars(Formulation))+
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  scale_y_continuous(limits = c(0,NA))+
  theme_classic()+
  scale_y_log10()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))+
  geom_bar(stat="identity",color="black", position=position_dodge(),
           #position=position_dodge2(preserve = 'single'),
           alpha=0.5)+
  #scale_y_continuous(limits = c(0,NA))+
  #geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD))+
  #stat_smooth(method = 'loess')+
  labs(title='Single muscle expression', 
       x='Incubation time',y='TdTomato expression')+
  geom_point(aes(color=tissue),#palette=c('orange','blue'),
             binaxis='y', stackdir='center',
             position=position_dodge(0.75),
             stat = 'identity',#position_dodge(0.75), 
             size=3)+
  scale_color_manual(values = c('Muscle_L'='orange','Muscle_R'='blue'))
p1d


library(ggpubr)
ggarrange(p1d,p1c,ncol = 1,
          common.legend = T,
          labels = c('C','D'),
          font.label = list(size=22,color='red',face='bold'))

ggsave('230112_p1cd_2.tiff',units = 'cm',
       width = 30, height = 28,
       dpi=600, compression='lzw')

ggplot(tdtomato_ct, aes(x=Incubation, y=values,fill=Formulation,color=tissue))+ 
  facet_grid(cols = vars(Formulation))+
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  scale_y_continuous(limits = c(0,NA))+
  theme_classic()+
  theme(legend.position = 'bottom',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))+
  geom_bar(stat="identity",color="black", position=position_dodge(),
           #position=position_dodge2(preserve = 'single'),
           alpha=0.5)+
  #geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD))+
  #stat_smooth(method = 'loess')+
  labs(title='Single muscle expression', 
       x='Incubation time',y='TdTomato expression')
###############################################################
ggplot(tdtomato_ct, aes(x=Incubation, y=values,fill=tissue,color=tissue))+ 
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  # geom_point()+
  geom_bar(stat="identity", color="black", 
           position=position_dodge())+
  #stat_smooth(method = 'loess')+
  labs(title='Injected muscle expression', 
       x='Incubation time',y='TdTomato expression')+
  theme(legend.position = 'bottom')


ggplot(tdtomato_ct_left, aes(x=Incubation, y=values,fill=Formulation,color=Formulation))+ 
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  # geom_point()+
  geom_bar(stat="identity", color="black", 
           position=position_dodge())+
  #stat_smooth(method = 'loess')+
  labs(title='Injected muscle expression', 
       x='Incubation time',y='TdTomato expression')+
  theme(legend.position = 'bottom')


ggplot(tdtomato_ct_right, aes(x=Incubation, y=values,fill=Formulation,color=Formulation))+ 
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  # geom_point()+
  geom_bar(stat="identity", color="black", 
           position=position_dodge())+
  #stat_smooth(method = 'loess')+
  labs(title='Uninjected muscle expression', 
       x='Incubation time',y='TdTomato expression')+
  theme(legend.position = 'bottom')
  
##https://www.datanovia.com/en/product/r-graphics-essentials-for-great-data-visualization/?url=/5-bookadvisor/52-r-graphics-essentials-for-great-data-visualization-200-practical-examples-you-want-to-know-for-data-science/
