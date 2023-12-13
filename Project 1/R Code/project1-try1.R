#Load Libraries
library(ggplot2)
library(dplyr)
library(gt)
library(naniar)
library(ggpubr)
library(gtsummary)
library(outliers)
#Set wd
#setwd("C:/Users/monic/OneDrive/Desktop/PHP2050/Project 1/")

#Load data
df <- read.csv("project1.csv",na.strings=c("","NA"))
View(df)


#Change NA from num_X_30 from NA to 0 if they had never used X before
df <- df %>% mutate(num_cigs_30 = case_when(cig_ever == 0 ~ 0,
                        cig_ever == 1 ~ num_cigs_30,
                        TRUE ~ NA),
                    num_e_cigs_30 = case_when(e_cig_ever == 0 ~ 0,
                                              e_cig_ever == 1 ~ num_e_cigs_30,
                                              TRUE ~ NA),
                    num_mj_30 = case_when(mj_ever == 0 ~ 0,
                                          mj_ever == 1 ~ num_mj_30,
                                          TRUE ~ NA),
                    num_alc_30 = case_when(alc_ever == 0 ~ 0,
                                           alc_ever == 1 ~ num_alc_30,
                                           TRUE ~ NA))
#Change 1=Yes to 1, and 2=No to 0
df <- df %>% mutate(mom_smoke_16wk = case_when(mom_smoke_16wk == "1=Yes" ~ 1,
                                               mom_smoke_16wk == "2=No" ~ 0,
                                           TRUE ~ NA),
                    mom_smoke_22wk = case_when(mom_smoke_22wk == "1=Yes" ~ 1,
                                               mom_smoke_22wk == "2=No" ~ 0,
                                               TRUE ~ NA),
                    mom_smoke_32wk = case_when(mom_smoke_32wk == "1=Yes" ~ 1,
                                               mom_smoke_32wk == "2=No" ~ 0,
                                               TRUE ~ NA),
                    mom_smoke_pp1 = case_when(mom_smoke_pp1 == "1=Yes" ~ 1,
                                              mom_smoke_pp1 == "2=No" ~ 0,
                                               TRUE ~ NA),
                    mom_smoke_pp2 = case_when(mom_smoke_pp2 == "1=Yes" ~ 1,
                                              mom_smoke_pp2 == "2=No" ~ 0,
                                              TRUE ~ NA),
                    mom_smoke_pp12wk = case_when(mom_smoke_pp12wk == "1=Yes" ~ 1,
                                              mom_smoke_pp12wk == "2=No" ~ 0,
                                              TRUE ~ NA),
                    mom_smoke_pp6mo = case_when(mom_smoke_pp6mo == "1=Yes" ~ 1,
                                              mom_smoke_pp6mo == "2=No" ~ 0,
                                              TRUE ~ NA))


#issues with income 
df$income[6] <- 250000 #value had one space
range(as.numeric(df$income), na.rm = T)
sort(as.numeric(df$income), decreasing = F) #maybe outlier 760?? Or just incorrect

#issues with momcig
range(df$momcig, na.rm = T) #40 does not make sense. Typo? Was it 4? Was it 30 the max?
df[which(df$momcig == 40),]$momcig <- NA

#issues which mom_numcig 
df[which(df$mom_numcig == "2 black and miles a day"),]$mom_numcig <- 2
df[which(df$mom_numcig == "44989"),]$mom_numcig <- NA
df[which(df$mom_numcig == "20-25"),]$mom_numcig <- mean(20:25)
df[which(df$mom_numcig == "None"),]$mom_numcig <- 0

#Change to factor levels and numerical variables
str(df)
df <- df %>%  mutate_at(c('psex', 'plang', 'pethnic','paian', 'pasian', 'pnhpi', 'pblack',
                          'pwhite','prace_other', 'employ', 'pedu', 'childasd', 
                          'nidaalc', 'nidatob', 'nidaill', "nidapres",
                          'mom_smoke_16wk','mom_smoke_22wk','mom_smoke_32wk',
                          'mom_smoke_pp1', 'mom_smoke_pp2', 'mom_smoke_pp12wk',
                          'mom_smoke_pp6mo', 'smoke_exposure_6mo', 
                          'smoke_exposure_12mo', 'smoke_exposure_2yr',
                          'smoke_exposure_3yr', 'smoke_exposure_4yr',
                          'smoke_exposure_5yr', 'tsex', 'language', 'tethnic', 
                          'taian', 'tasian', 'tnhpi', 'tblack', 'twhite', 
                          'trace_other', 'cig_ever', 'e_cig_ever', 'mj_ever',
                          'alc_ever', 'parent_id'),as.factor)
df <- df %>% mutate_if(is.character, as.numeric)
df <- df %>% mutate_if(is.integer, as.numeric)



#Missing Data
# % complete cases
sum(complete.cases(df))
rowSums(is.na(df))  #each patient has at least one missing value

#creating missing table
missing_table <- df %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) %>%
  t() %>%
  as.data.frame() %>%
  mutate(missing=V1) %>%
  select(missing)
missing_cols <- missing_table %>%
  filter(missing > 0) %>%
  arrange(desc(missing)) %>% mutate(percent = round(missing/dim(df)[1],4)*100)
#missing table
missing_cols

#add variable names
missing_cols$var_name <- rownames(missing_cols)


#Missing Data 
#Using package naniar


#Postpartum
mpos <- vis_miss(df %>% 
           select(mom_smoke_pp1, mom_smoke_pp2, mom_smoke_pp12wk, 
                  mom_smoke_pp6mo))
#smoking during pregnancy
mmom_smoke <- vis_miss(df %>% 
           select(mom_smoke_16wk,mom_smoke_22wk ,mom_smoke_32wk ))

#parent characteristics missing together
mis_8 <- missing_cols %>% filter(missing == 8) %>% select(var_name)
miss8 <- vis_miss(df %>% 
           select(mis_8$var_name))



#all missing variables wiht 10 missing values have them missing together
mis_10 <- missing_cols %>% filter(missing == 10) %>% select(var_name)
miss10 <- vis_miss(df %>% 
           select(c(mis_10$var_name)))
mis_11 <- missing_cols %>% filter(missing == 11) %>% select(var_name)
vis_miss(df %>% 
           select(c(mis_11$var_name)))
mis_12 <- missing_cols %>% filter(missing == 12) %>% select(var_name)
vis_miss(df %>% 
           select(c(mis_12$var_name)))
mis_13 <- missing_cols %>% filter(missing == 13) %>% select(var_name)
vis_miss(df %>% 
           select(c(mis_13$var_name)))
mis_14 <- missing_cols %>% filter(missing == 14) %>% select(var_name)
vis_miss(df %>% 
           select(c(mis_14$var_name)))

vis_miss(df) + theme(text = element_text(size=4))
gg_miss_var(df, show_pct = TRUE)+theme(text = element_text(size=5))

#PARENTS
#Race Variable
#creating counts to do a race plot
counts <- df %>% select(paian, pasian, pnhpi, pblack, pwhite, prace_other) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(across(everything(), ~  . - 1)) %>%
  colSums() %>% 
  as.data.frame()

colnames(counts) <- "count"
counts <- counts %>% mutate(Race = rownames(counts))

#race plot
prace <- ggplot(counts, aes(y = Race, x = count))+geom_point(color = '#293352')+
  geom_segment(aes(x = rep(0,6), y = 1:6, xend = c(4,0,0,8,6,26), yend = 1:6), 
               color = '#293352')+
  theme_minimal()+
  scale_y_discrete(labels = c("paian" = "American Indian\nAlaskan Native",
                              "pasian" = "Asian",
                              "pnhpi" = "Native Hawaian\nPacific Islander",
                              "pblack" = "Black",
                              "pwhite" = "White",
                              "prace_other" = "Other"
                              ))+ggtitle("Race")
#education plot
pedu <- ggplot(df,aes(x = as.factor(pedu)))+geom_bar(fill='#293352')+theme_minimal()+
  scale_x_discrete(labels=c("0" = "Some\nhighschool", "1" = "High school",
                            "2" = "GED","3" = "Some\ncollege",
                            "4" = "2 year\ndegree","5" = "4 year\ndegree",
                            "6" = "Postgraduate\ndegree"))+
  xlab("")+
  ggtitle("Parent Education Level")

#age plot
page <- ggplot(df,aes(x = as.factor(page)))+geom_bar(fill = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Parent Age")

#view a table of age values
table(df$page)

#employ plot
pemploy <- ggplot(df, aes(x = employ))+geom_bar(fill = '#293352')+theme_minimal()+
  scale_x_discrete(labels = c("0" = "No", "1" = "Part-Time", 
                              "2" = "Full-Time"))+
  xlab("")+ggtitle("Parent Employment")

#income plot
pincome <- ggplot(df,aes(x = income))+geom_density(color = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Family Estimated Income")

#sex plot
psex <- ggplot(df,aes(x = as.factor(psex)))+geom_bar(fill = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Parent Sex")

#ethnicity plot
pethnic <- ggplot(df,aes(x = as.factor(pethnic)))+geom_bar(fill = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Hispanic or Latino")

#join the plots together
p1 <- ggarrange(prace, pethnic, nrow = 1,ncol = 2,labels = c("A", "B"))
p2 <- ggarrange(psex,page, labels = c("C", "D"), nrow = 1,ncol = 2)
p3 <- ggarrange(pemploy,pincome, labels = c("E","F"), ncol = 2)
p4 <- ggarrange(pedu, labels = "F")
ggarrange(p1,p2,p3,p4, nrow = 4) 


#CHILDREN
#Race Variable
#creating counts variable to plot race
counts <- df %>% select(taian, tasian, tnhpi, tblack, twhite, trace_other) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(across(everything(), ~  . - 1)) %>%
  colSums() %>% 
  as.data.frame()

colnames(counts) <- "count"
counts <- counts %>% mutate(Race = rownames(counts))
#race plot
trace <- ggplot(counts, aes(y = Race, x = count))+geom_point(color = '#293352')+
  geom_segment(aes(x = rep(0,6), y = 1:6, xend = c(5,0,15,0,5,19), yend = 1:6), 
               color = '#293352')+
  theme_minimal()+
  scale_y_discrete(labels = c("paian" = "American Indian\nAlaskan Native",
                              "pasian" = "Asian",
                              "pnhpi" = "Native Hawaian\nPacific Islander",
                              "pblack" = "Black",
                              "pwhite" = "White",
                              "prace_other" = "Other"
  ))+ggtitle("Race")


#age plot
tage <- ggplot(df,aes(x = as.factor(tage)))+geom_bar(fill = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Children Age")

#table for ages
table(df$page)

#sex plot
tsex <- ggplot(df,aes(x = tsex))+geom_bar(fill = '#293352')+theme_minimal()+
  xlab("")+ggtitle("Children Sex")

#ethnicity plot
tethnic <- ggplot(df, aes(x = as.factor(tethnic)))+geom_bar(fill = '#293352')+
  theme_minimal()+
  xlab("")+ggtitle("Hispanic or Latino")

#join the plots together
p1 <- ggarrange(tage,tsex,labels = c("A", "B"), ncol = 2)
p2 <- ggarrange(trace,tethnic,labels = c("C", "D"), ncol = 2)
ggarrange(p1,p2, nrow = 2)
ggarrange(trace, tage, tsex, labels = c("A", "B", "C"), nrow = 2, ncol = 2)

#Set Color
bp2 <- c( "#4E84C4", "#293352", "#C3D7A4" ,"#52854C", "#F4EDCA")

#During Pregnancy 
#View somking variables
df %>% select(mom_smoke_16wk, mom_smoke_22wk, mom_smoke_32wk, cotimean_34wk,
              mom_numcig)
df %>% filter(mom_smoke_16wk==1 |  mom_smoke_22wk == 1|  mom_smoke_32wk == 1) %>%
  select(mom_smoke_16wk, mom_smoke_22wk, mom_smoke_32wk, cotimean_34wk)
df %>% filter(mom_smoke_16wk==0  &  mom_smoke_22wk == 0 &  mom_smoke_32wk == 0) %>%
  select(mom_smoke_16wk, mom_smoke_22wk, mom_smoke_32wk, cotimean_34wk)


#Plotin Child ASD
asd1 <- ggplot(df, aes(x = childasd, fill = as.factor(mom_smoke_16wk))) + 
  geom_bar(position = position_dodge())+ theme_minimal()+
  guides(fill = guide_legend(title = ""))+
  scale_x_discrete(labels = c("0" = "No",
                              "1" = "Diagnosed",
                              "2" = "Suspected"))+
  ggtitle("16wk")+ 
  scale_fill_manual(values=cbp2, label = c("No", "Yes", "NA"))+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+xlab("")

asd2 <- ggplot(df, aes(x = childasd, fill = as.factor(mom_smoke_22wk))) + 
  geom_bar(position = position_dodge())+ theme_minimal()+
  guides(fill = guide_legend(title = ""))+
  scale_x_discrete(labels = c("0" = "No",
                              "1" = "Diagnosed",
                              "2" = "Suspected"))+
  ggtitle("22wk")+ 
  scale_fill_manual(values=cbp2, label = c("No", "Yes", "NA"))+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+xlab("")

asd3 <- ggplot(df, aes(x = childasd, fill = as.factor(mom_smoke_32wk))) + 
  geom_bar(position = position_dodge())+ theme_minimal()+
  guides(fill = guide_legend(title = ""))+
  scale_x_discrete(labels = c("0" = "No",
                              "1" = "Diagnosed",
                              "2" = "Suspected"))+
  ggtitle("32wk")+
  scale_fill_manual(values=cbp2, label = c("No", "Yes", "NA"))+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))+xlab("")

#Join the plots
ggarrange(asd1, asd2, asd3, ncol = 3, common.legend = T, legend = "bottom") %>%
  annotate_figure(top = text_grob("Autism Spectrum Disorder and \nSmoking during Pregnancy", face = "bold"))



#Creating smoke level variable
 df <- df %>% 
  mutate(Smoke_level = case_when((as.numeric(mom_smoke_16wk)-1+as.numeric(mom_smoke_22wk)-1+as.numeric(mom_smoke_32wk)-1)==3 ~ "HeavySmoker",
                                 (as.numeric(mom_smoke_16wk)-1+as.numeric(mom_smoke_22wk)-1+as.numeric(mom_smoke_32wk)-1)==0 ~ "NonSmoker",
                                 (as.numeric(mom_smoke_16wk)-1+as.numeric(mom_smoke_22wk)-1+as.numeric(mom_smoke_32wk)-1)==1 ~ "LightSmoker",
                                 (as.numeric(mom_smoke_16wk)-1+as.numeric(mom_smoke_22wk)-1+as.numeric(mom_smoke_32wk)-1)==2 ~ "LightSmoker",
                                                                  TRUE ~ NA))
         

#Plotting again child asd with the smoke level
df_plt <- df %>%
  select(childasd, Smoke_level) %>%
  na.omit()

ggplot(data=df_plt, aes(x = childasd, fill = Smoke_level))+
  geom_bar(position = position_dodge())+theme_minimal()+
  scale_fill_manual(values= cbp2)+xlab("Autism Spectrum Disorder")+
  scale_x_discrete(labels = c("0" = "No",
                              "1" = "Diagnosed",
                              "2" = "Suspected"))

#Plotting BPM variables
df_plt <- df %>%
  select(bpm_att, Smoke_level) %>%
  na.omit()
bpm_att <- ggplot(df_plt, aes(x = bpm_att, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+scale_fill_manual(values= cbp2)+
  xlab("BPM Attention")
df_plt <- df %>%
  select(bpm_ext, Smoke_level) %>%
  na.omit()
bpm_ext <- ggplot(df_plt, aes(x = bpm_ext, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+scale_fill_manual(values= cbp2)+
  xlab("BPM Externalizing")
df_plt <- df %>%
  select(bpm_int, Smoke_level) %>%
  na.omit()
bpm_int <- ggplot(df_plt, aes(x = bpm_int, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+scale_fill_manual(values= cbp2)+
  xlab("BPM Internalizing")

#joining them together
ggarrange(bpm_ext, bpm_att, bpm_int, common.legend = T, ncol = 3)



###Parent on child BPM
#plotting bpm
df_plt <- df %>%
  select(bpm_att_p, Smoke_level) %>%
  na.omit()
bpm_att <- ggplot(df_plt, aes(x = bpm_att_p, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+scale_fill_manual(values= cbp2)+
  xlab("BPM Attention")+
  scale_x_continuous(breaks = 0:10)
df_plt <- df %>%
  select(bpm_ext_p, Smoke_level) %>%
  na.omit()
bpm_ext <- ggplot(df_plt, aes(x = bpm_ext_p, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+scale_fill_manual(values= cbp2)+
  xlab("BPM Externalizing")+
  scale_x_continuous(breaks = 0:10)
df_plt <- df %>%
  select(bpm_int_p, Smoke_level) %>%
  na.omit()
bpm_int <- ggplot(df_plt, aes(x = bpm_int_p, fill = Smoke_level))+
  geom_density(alpha = .5)+theme_minimal()+
  scale_fill_manual(values= cbp2)+
  xlab("BPM Internalizing")+
  scale_x_continuous(breaks = 0:10)

#joining the plots
ggarrange(bpm_ext, bpm_att, bpm_int, common.legend = T, ncol = 3)


#Plottin erq_cog and erq_exp
df_plt <- df %>%
  select(erq_cog, Smoke_level) %>%
  na.omit()
ggplot(df_plt, aes(x = Smoke_level, y = erq_cog, fill = Smoke_level))+
  geom_boxplot(alpha = .5)+theme_minimal()+
  scale_fill_manual(values= cbp2)+
  xlab("ERQ Cognitive")
ggplot(df_plt, aes(x = erq_cog, y = Smoke_level, color = Smoke_level))+
  geom_point()+theme_minimal()+
  scale_color_manual(values= cbp2)+
  xlab("ERQ Cognitive")+
  scale_x_continuous(breaks = 0:10)
ggplot(df_plt, aes(x = erq_cog,y = Smoke_level ,fill = Smoke_level))+
  geom_violin()+theme_minimal()+
  scale_fill_manual(values= cbp2)+
  xlab("ERQ Cognitive")+
  scale_x_continuous(breaks = 0:10)
ggplot(df_plt, aes(x = Smoke_level,y = erq_cog ,fill = Smoke_level))+
  geom_boxplot()+theme_minimal()+
  scale_fill_manual(values= cbp2)+
  xlab("ERQ Cognitive")


#table for means of erq_cog
df %>% select(erq_cog, Smoke_level) %>%
  na.omit() %>%
  group_by(Smoke_level) %>%
  summarise(Mean = mean(erq_cog),
            Sd = sd(erq_cog)) 
  

#summary 
df %>% 
  tbl_summary(include = c(erq_cog,erq_exp, bpm_att_p, bpm_ext_p, bpm_int_p,
                          swan_inattentive, swan_hyperactive),
              type = list(everything() ~ 'continuous'),
                     digits = list(everything() ~ c(2)),
                     statistic = list(~ "{mean} ({sd})"),
                     by = Smoke_level,
                     missing = "no") 


#Creating smoke exposure level variable
#first calculate mean accros all exposures
smoke_mean <- df %>% 
  select(smoke_exposure_6mo, smoke_exposure_12mo, smoke_exposure_2yr, 
         smoke_exposure_3yr, smoke_exposure_4yr,
              smoke_exposure_5yr) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate_all(~ . - 1) %>% 
  rowMeans(na.rm = T)

#cbind
df <- cbind(df,smoke_mean)
#create the new variable
df <- df %>%
  mutate(smoke_exposure_level = case_when(smoke_mean == 0 ~ "Not exposed",
                                          (smoke_mean > 0 & smoke_mean <= .5) ~ "Moderately Exposed",
                                          smoke_mean > .5 ~ "Extremely exposed"
                                          ))

#plot erq
df1 <- df %>% select(erq_cog, smoke_exp2) %>% na.omit()
ggplot(df1, aes(y = erq_cog, x = smoke_exp2, fill = smoke_exp2))+
  geom_boxplot()+theme_minimal()+scale_fill_manual(values = cbp2)

#try creating variable with cotimean of baby
df <- df %>%
  mutate(smoke_exp2 = case_when(cotimean_pp6mo_baby < 5 ~ "Not exposed", 
                                (cotimean_pp6mo_baby > 5 & cotimean_pp6mo_baby< 39) ~ "Moderately Exposed", 
                                cotimean_pp6mo_baby > 39 ~ "Extremely exposed"))



#substance use
#creating plots
df1 <- df %>% select(cig_ever,Smoke_level)%>%na.omit()
p1 <- ggplot(df1, aes(x = as.factor(cig_ever), fill = Smoke_level))+geom_bar(position = position_dodge())+theme_minimal()+scale_fill_manual(values = cbp2)+
  scale_x_discrete(labels = c("0" = "Never",
                              "1" = "At least once"))+ggtitle("Cigarette Use")+xlab("")

df1 <- df %>% select(num_e_cigs_30,Smoke_level)%>%na.omit()
p2 <- ggplot(df1, aes(x = as.factor(num_e_cigs_30), fill = Smoke_level))+geom_bar(position = position_dodge())+theme_minimal()+scale_fill_manual(values = cbp2)+
  scale_x_discrete(labels = c("0" = "Never",
                              "1" = "At least once"))+ggtitle("E-Cigarette Use")+xlab("")

df1 <- df %>% select(mj_ever,Smoke_level)%>%na.omit()
p3 <- ggplot(df1, aes(x = as.factor(mj_ever), fill = Smoke_level))+geom_bar(position = position_dodge())+theme_minimal()+scale_fill_manual(values = cbp2)+
  scale_x_discrete(labels = c("0" = "Never",
                              "1" = "At least once"))+ggtitle("Marijuana Use in")+xlab("")

df1 <- df %>% select(alc_ever,Smoke_level)%>%na.omit()
p4 <- ggplot(df1, aes(x = as.factor(alc_ever), fill = Smoke_level))+geom_bar(position = position_dodge())+theme_minimal()+scale_fill_manual(values = cbp2)+
  scale_x_discrete(labels = c("0" = "Never",
                              "1" = "At least once"))+ggtitle("Alcohol Use")+xlab("")
#joinin the plots
ggarrange(p1,p2,p3,p4,common.legend = T, legend = "bottom") %>% annotate_figure( text_grob("Substance Use in Children", 
                                                                                           face = "bold", size = 14))





#number of days with substance use
#plots
df1 <- df %>% select(num_cigs_30, Smoke_level) %>% na.omit()
p1 <- ggplot(df1, aes(x = Smoke_level,y = num_cigs_30, color = Smoke_level))+
  geom_boxplot()+theme_minimal()

df1 <- df %>% select(num_e_cigs_30, Smoke_level) %>% na.omit()
p2 <- ggplot(df1, aes(x = Smoke_level,y = num_e_cigs_30, color = Smoke_level))+
  geom_boxplot()+theme_minimal()

df1 <- df %>% select(num_mj_30, Smoke_level) %>% na.omit()
p3 <- ggplot(df1, aes(x = Smoke_level,y = num_mj_30, color = Smoke_level))+
  geom_boxplot()+theme_minimal()

df1 <- df %>% select(num_alc_30, Smoke_level) %>% na.omit()
p4<- ggplot(df1, aes(x = Smoke_level,y = num_alc_30, color = Smoke_level))+
  geom_boxplot()+theme_minimal()
#joinin the plots
ggarrange(p1,p2,p3,p4,common.legend = T, legend = "bottom")


################ ETS

#do plots with smoke exposure level
#plots 
df <- df_exp %>% select(num_cigs_30, smoke_exposure_level) %>% na.omit()
p1 <- ggplot(df1, aes(x = smoke_exposure_level,y = num_cigs_30, color = smoke_exposure_level))+
  geom_boxplot()+theme_minimal()+theme(axis.text.x=element_text(size=rel(.7)))+
  scale_color_manual(values = cbp2)

df1 <- df_exp %>% select(num_e_cigs_30, smoke_exposure_level) %>% na.omit()
p2 <- ggplot(df1, aes(x = smoke_exposure_level,y = num_e_cigs_30, color = smoke_exposure_level))+
  geom_boxplot()+theme_minimal()+theme(axis.text.x=element_text(size=rel(.7)))+
  scale_color_manual(values = cbp2)

df1 <- df_exp %>% select(num_mj_30, smoke_exposure_level) %>% na.omit()
p3 <- ggplot(df1, aes(x = smoke_exposure_level,y = num_mj_30, color = smoke_exposure_level))+
  geom_boxplot()+theme_minimal()+theme(axis.text.x=element_text(size=rel(.7)))+
  scale_color_manual(values = cbp2)

df1 <- df_exp %>% select(num_alc_30, smoke_exposure_level) %>% na.omit()
p4<- ggplot(df1, aes(x = smoke_exposure_level,y = num_alc_30, color = smoke_exposure_level))+
  geom_boxplot()+theme_minimal()+theme(axis.text.x=element_text(size=rel(.7)))+
  scale_color_manual(values = cbp2)

#join the plots
ggarrange(p1,p2,p3,p4,common.legend = T, legend = "bottom") %>% annotate_figure( text_grob("Substance use in Children during the last 30 days", 
                                                                                           face = "bold", size = 14))


#cotimean plot
df1 <- df %>% select(Smoke_level, cotimean_pp6mo_baby) %>% na.omit()
ggplot(df1, aes(x = Smoke_level, y = cotimean_pp6mo_baby, fill = Smoke_level)) + geom_boxplot()+
  scale_fill_manual(values = cbp2)+theme_minimal()

#outliers
grubbs.test(df_exp$num_mj_30)

