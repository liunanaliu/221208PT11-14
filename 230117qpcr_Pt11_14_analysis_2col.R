#################230117 repeatation for pt11_pt14 qpcr results
library(readxl)
X20221208_PT11_14_pcr <- read_excel("20221208_PT11_14_pcr.xlsx", 
                                    sheet = "data_summary_na")

tdtomato_ct <- X20221208_PT11_14_pcr[,c(1:7)]
colnames(tdtomato_ct)[4] <- 'Injection'
colnames(tdtomato_ct)[5] <- 'tissue'
colnames(tdtomato_ct)[6] <- 'values'

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

##tdtomato_ct$Sex <- ifelse(tdtomato_ct$Sex=='f','Female','Male')
#tdtomato_ct$Sex <- factor(tdtomato_ct$Sex,levels = c('Female','Male'))

save(tdtomato_ct,file = '230115_tdtomato_ct.rda')


########################
options(scipen = -2)
load("/media/l33/My Passport/221208PT11_14_qpcr/230115_tdtomato_ct.rda")
tdtomato_ct[82,6] <- 0####80858_R, PT11_48
tdtomato_ct[28,6] <- 0####80878_R, PT13_6
tdtomato_ct[95,6] <- 0###80895_R, PT14_48
tdtomato_ct_left <- tdtomato_ct[tdtomato_ct$tissue=='Muscle_L',]
tdtomato_ct_right <- tdtomato_ct[tdtomato_ct$tissue=='Muscle_R',]

library(ggpubr)
res_tdt_1 <- compare_means(values ~ Formulation,
                           data = tdtomato_ct,
                           group.by = c('tissue','Incubation'))
#######################################230107 same as before PT_9/10
#install.packages("ggbreak")
##230117 use geom_col instread of geom_bar
library(ggbreak)
options(scipen = -2)
getOption("scipen")#1
P_left1 <- ggplot(tdtomato_ct_left, aes(x=group, y=values,fill=tissue)) +
  geom_col(alpha=.3)+
  theme_classic()+
  scale_y_break(c(1e+05,1e+07),scales = 2,ticklabels = c(1e+07,7e+09)
                )+
  scale_y_continuous(labels=scales::label_scientific(digits = 1))+
  scale_y_continuous(labels=c(0,5e+04,1e+05,1e+07,7e+09),
                     breaks=c(0,5e+04,1e+05,1e+07,7e+09))+
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank())+
  #scale_y_log10()+
  labs(title = 'Injected muscle expression', 
       x='',y='TdTomato expression(CN/ng)')+
  scale_color_manual(values = c('Muscle_L'='orange'))+
  scale_fill_manual(values = c('Muscle_L'='orange'))
P_left1

##############################
options(scipen = -2)
P_right1 <- ggplot(tdtomato_ct_right, aes(x=group, y=values,fill=tissue)) +
  geom_col(alpha=.3)+
  scale_y_continuous(labels=scales::label_scientific(digits = 1))+
  scale_y_continuous(labels=c(0,5e+03,1e+04,1e+05),
                     breaks = c(0,5e+03,1e+04,1e+05))+
  theme_classic()+
  scale_y_break(c(1e+04,5e+04),scales = 2,ticklabels = c(5e+04,1e+05)
                )+
  theme(legend.position = 'top',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        legend.text = element_text(face = 'bold',size = 14),
        legend.title = element_blank())+
  #scale_y_log10()+
  labs(title = 'Uninjected muscle expression', 
       x='',y='TdTomato expression(CN/ng)')+
  scale_color_manual(values = c('Muscle_R'='blue'))+
  scale_fill_manual(values = c('Muscle_R'='blue'))
P_right1

###########
################
library(ggpubr)
##library(devtools)
#remotes::install_github("thomasp85/patchwork")
library(patchwork)
library(aplot)
P_left1+P_right1+
  patchwork::plot_layout(ncol = 1)+
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 22,color='red',face = 'bold'))

ggsave('230117_p1a_rl.png',
       units = 'cm',
       dpi = 600,width = 38,height = 26)
#####error, do not run
ggarrange(P_left1,P_right1,ncol = 1,
          labels = c('A','B'),#heights = c(1,.6),
          font.label = list(size=22,color='red',face='bold'))

######################################

######################################
facet <- scales::hue_pal()(4)
scales::show_col(facet)
p1_Left_form <- ggplot(tdtomato_ct_left, 
                       aes(x=Incubation, y=values,fill=Formulation,color=tissue))+
  facet_grid(cols = vars(Formulation))+
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  #scale_y_continuous(limits = c(0,NA))+
  theme_classic()+
  scale_y_continuous(labels=scales::label_scientific(digits = -2))+
  scale_y_continuous(labels=c(0,5e+04,1e+05,1e+07,7e+09),
                     breaks=c(0,5e+04,1e+05,1e+07,7e+09))+
  scale_y_break(c(1e+05,1e+07),scales = 2,
                ticklabels = c(1e+07,7e+09))+
  #scale_y_log10()+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        #legend.text = element_text(face = 'bold',size = 14),
        legend.text = element_blank(),
        legend.title = element_blank(),
        #strip.background.x = element_rect(fill = c("#F8766D", "#7CAE00",
                                                  # "#00BFC4", "#C77CFF")),
        #strip.background = element_rect(fill = NA),
        strip.text.x = element_text(size = 18,face = 'bold'))+
  geom_col(color=vars(Formulation),alpha=.3)+
  labs(title='Injected muscle expression', 
       x='Incubation time',y='TdTomato expression (CN/ng)')+
  scale_color_manual(values = c('Muscle_L'='orange','Muscle_R'='blue',
                                'PT_11'="#F8766D",'PT_12'="#7CAE00",
                                'PT_13'="#00BFC4",'PT_14'="#C77CFF"))
p1_Left_form
######################
##############################
p1_Right_form <- ggplot(tdtomato_ct_right, 
                        aes(x=Incubation, y=values,fill=Formulation,color=tissue))+ 
  facet_grid(cols = vars(Formulation))+
  geom_col(color=vars(Formulation),alpha=.3)+
  scale_x_continuous(breaks = c(6,24,48),labels=c('6 H','24 H','48 H'))+
  scale_y_continuous(limits = c(0,NA))+
  theme_classic()+
  scale_y_continuous(labels=scales::label_scientific(digits = 1))+
  scale_y_continuous(labels=c(0,5e+03,1e+04,1e+05),
                     breaks = c(0,5e+03,1e+04,1e+05))+
  scale_y_break(c(1e+04,5e+04),scales = 2,
                ticklabels = c(5e+04,1e+05))+
  #scale_y_log10()+
  theme(legend.position = 'none',
        axis.text.x = element_text(face = 'bold',size = 12),
        axis.text.y = element_text(face = 'bold',size = 12),
        plot.title = element_text(face = 'bold',size = 20),
        axis.title.y = element_text(face = 'bold',size = 16),
        axis.title.x = element_text(face = 'bold',size = 16),
        #legend.text = element_text(face = 'bold',size = 14),
        legend.text = element_blank(),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18,face = 'bold'))+
  labs(title='Uninjected muscle expression', 
       x='Incubation time',y='TdTomato expression (CN/ng)')+
  scale_color_manual(values = c('Muscle_L'='orange','Muscle_R'='blue'))
p1_Right_form

####################
p1_Left_form+p1_Right_form+
  patchwork::plot_layout(ncol = 1)+
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 22,color='red',face = 'bold'))

ggsave('230117_p1rl_form.png',
       units = 'cm',
       dpi = 600,width = 38,height = 26)
#####################################

#################################


