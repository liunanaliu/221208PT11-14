#20220303nanaliu 
library(readxl)
library(ggplot2)
library(svglite)
X20220223_second_animal_experiment_for_supplement <- read_excel("20220223_second animal experiment for supplement.xlsx", 
                                                                  sheet = "combine_6h_24h_48h")
tdtomato_ct <- X20220223_second_animal_experiment_for_supplement
tdtomato=tdtomato_ct[,c(3,4,5,6,7,27)]

names(tdtomato)[6]="expression"
tdtomato$injection=ifelse(tdtomato$muscle=='Right',
                             'free_DNA(R)','polyplex(L)')
tdtomato$group=paste(tdtomato$License,
                        tdtomato$treatment,sep = "")

tdtomato$group=ifelse(tdtomato$treatment=='con',
                      'Naive',
                      tdtomato$group)
tdtomato$group=factor(tdtomato$group,
                         levels = c('Naive',
                                    'HAC6h',
                                    'HAC24h',
                                    'HAC48h',
                                    'PEI6h',
                                    'PEI24h',
                                    'PEI48h'))
save(tdtomato,file = 'tdtomato_combined_0307.rda')

tdtomato_left <- tdtomato[tdtomato$injection=='polyplex(L)',]
tdtomato_right <- tdtomato[tdtomato$injection=='free_DNA(R)',]

table(tdtomato_left$group)
table(tdtomato_right$group)
table(tdtomato$group)


p=ggplot(tdtomato_left,aes(group,expression,fill=injection))+
  geom_bar(position = position_dodge(),stat = 'summary',alpha=.3)+
  theme_classic()+
  scale_y_log10()+
  geom_point(aes(color=injection),binaxis='y', stackdir='center',
             position=position_dodge(0.75), 
             size=3)+
  labs(x="",y="TdTomato expression(CN/ng mRNA)")
p



data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

td_left <- data_summary(tdtomato_left,varname = 'expression',
                   groupnames = 'group')

td_right <- data_summary(tdtomato_right,varname = 'expression',
                        groupnames = 'group')


write.csv(td_left,file = 'td_left.csv')
write.csv(td_right,file = 'td_right.csv')

library(writexl)
write_xlsx(td_left,'td_left.xlsx')
write_xlsx(td_right,'td_right.xlsx')

p <- ggplot(td_left, aes(x=group, y=expression)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=expression-sd, ymax=expression+sd), width=.2,
                position=position_dodge(.9))+
  labs(x='',y='Polyplex TdTomato expression(CN/ng mRNA)')
print(p)
p

p <- ggplot(td_right, aes(x=group, y=expression)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=expression-sd, ymax=expression+sd), width=.2,
                position=position_dodge(.9))+
  labs(x='',y='FreeDNA TdTomato expression(CN/ng mRNA)')

p
p+geom_errorbar(aes(ymin=expression-sd,ymax=expression+sd))

p=ggplot(tdtomato_right,aes(group,expression,fill=injection))+
  geom_bar(position = position_dodge(),stat = 'summary',alpha=.3)+
  theme_classic()+
  scale_y_log10()+
  geom_point(aes(color=injection),binaxis='y', stackdir='center',
             position=position_dodge(0.75), 
             size=3)+
  labs(x="",y="TdTomato expression(CN/ng mRNA)")
p
