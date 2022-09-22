# Importing Data
data_df <- read.csv('C:/Users/dawca598/OneDrive - University of Otago/2022/Thesis/Intrinsic Motivations, Personality Traits and Mental Health Survey_May 3, 2022_18.06.csv')
data_df <- read.csv('C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Intrinsic Motivations, Personality Traits and Mental Health Survey_May 3, 2022_18.06.csv')
# Only Mental Health Questionnaires
mh_data_df <- data_df[,35:100]
IPM_all_data <- data_df[,218:247]
#any(is.na(data_df))
#data_df <- na.omit(data_df)
participant_id <- data_df$ResponseId
#GAD7 Data
GAD_7 <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/GAD7.csv")
GAD_7 <- GAD_7[,2:9]
sum_GAD_score <- GAD_7$X

#STAI Data analysis
STAI <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/STAI.csv")
sum_STAI_score <- STAI$Sum

#ASI Data 
ASI <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/ASI.csv")
sum_ASI_score <- ASI$sum

# ASI subscores
ASI_phyiscal_concerns <- data.frame((ASI$Q29_4) +(ASI$Q29_12) + (ASI$Q29_7) + 
                                    (ASI$Q29_7) + (ASI$Q29_15) + (ASI$Q29_3))
ASI_cognitive_concerns <- data.frame((ASI$Q29_14) +(ASI$Q29_18) + (ASI$Q29_10) + 
                                     (ASI$Q29_16) + (ASI$Q29_2) + (ASI$Q29_5))
ASI_social_concerns <- data.frame((ASI$Q29_9) +(ASI$Q29_6) + (ASI$Q29_11) + 
                                  (ASI$Q29_13) + (ASI$Q29_17) + (ASI$Q29_1))
ASI_subscores <- data.frame((ASI_cognitive_concerns), (ASI_phyiscal_concerns), (ASI_social_concerns))

#CESD Data
CESD <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/CESD.csv")
CESD <- CESD[,1:21]
sum_CESD_score <- CESD$Sum

sd(CESD$Sum)

mean(full_data_IPIP_facets$SelfAssert.CE)

# Personality Data 
IPIP_data_df <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPIP Neo 120.csv")

# Exclusion of blank columns
#any(is.na(IPIP_data_df))
#IPIP_data_df <- na.omit(IPIP_data_df)

#Calculating IPIP Neuroticism (subcategories) 
anxiety <- data.frame((IPIP_data_df$Q30_1)+(IPIP_data_df$Q30_31)+(IPIP_data_df$Q30_61)+(IPIP_data_df$Q30_91))
anger  <- data.frame((IPIP_data_df$Q30_6)+(IPIP_data_df$Q30_36)+(IPIP_data_df$Q30_66)+(IPIP_data_df$Q30_96))
depression <- data.frame((IPIP_data_df$Q30_11)+(IPIP_data_df$Q30_41)+(IPIP_data_df$Q30_71)+(IPIP_data_df$Q30_101))
self_consciousness <- data.frame((IPIP_data_df$Q30_16)+(IPIP_data_df$Q30_46)+(IPIP_data_df$Q30_76)+(IPIP_data_df$Q30_106))
immoderation <- data.frame((IPIP_data_df$Q30_21)+(IPIP_data_df$Q30_51)+(IPIP_data_df$Q30_81)+(IPIP_data_df$Q30_111))
vulnerability  <- data.frame((IPIP_data_df$Q30_26)+(IPIP_data_df$Q30_56)+(IPIP_data_df$Q30_86)+(IPIP_data_df$Q30_116))
sum_Neuroticism <- data.frame((anxiety)+(anger)+(depression)+(self_consciousness)+(immoderation)+(vulnerability))
Neuroticism <- data.frame(anxiety, anger, depression, self_consciousness, immoderation, vulnerability, sum_Neuroticism)
colnames(Neuroticism) <- c('anxiety', 'anger', 'depression', 'self_consciousness', 'immoderation', 'vulnerability', 'Neuroticism')

#Calculating IPIP Extraversion (subcategories) 
friendliness <- data.frame((IPIP_data_df$Q30_2)+(IPIP_data_df$Q30_32)+(IPIP_data_df$Q30_62)+(IPIP_data_df$Q30_92))
gregariousness  <- data.frame((IPIP_data_df$Q30_7)+(IPIP_data_df$Q30_37)+(IPIP_data_df$Q30_67)+(IPIP_data_df$Q30_97))
assertiveness <- data.frame((IPIP_data_df$Q30_12)+(IPIP_data_df$Q30_42)+(IPIP_data_df$Q30_72)+(IPIP_data_df$Q30_102))
activity_level <- data.frame((IPIP_data_df$Q30_17)+(IPIP_data_df$Q30_47)+(IPIP_data_df$Q30_77)+(IPIP_data_df$Q30_107))
excitement_seeking <- data.frame((IPIP_data_df$Q30_22)+(IPIP_data_df$Q30_52)+(IPIP_data_df$Q30_82)+(IPIP_data_df$Q30_112))
cheerfulness  <- data.frame((IPIP_data_df$Q30_27)+(IPIP_data_df$Q30_57)+(IPIP_data_df$Q30_87)+(IPIP_data_df$Q30_117))
sum_extraversion <- data.frame(friendliness+gregariousness+assertiveness+activity_level+excitement_seeking+cheerfulness)
extraversion <- data.frame(friendliness, gregariousness, assertiveness, activity_level, excitement_seeking, cheerfulness, sum_extraversion)
colnames(extraversion) <- c('friendliness', 'gregariousness', 'assertiveness', 'activity_level', 'excitement_seeking', 'cheerfulness', 'extraversion')

#Calculating IPIP Openness (subcategories) 
imagination <- data.frame((IPIP_data_df$Q30_3)+(IPIP_data_df$Q30_33)+(IPIP_data_df$Q30_63)+(IPIP_data_df$Q30_93))
artistic_interests  <- data.frame((IPIP_data_df$Q30_8)+(IPIP_data_df$Q30_38)+(IPIP_data_df$Q30_68)+(IPIP_data_df$Q30_98))
emotionality <- data.frame((IPIP_data_df$Q30_13)+(IPIP_data_df$Q30_43)+(IPIP_data_df$Q30_73)+(IPIP_data_df$Q30_103))
adventurousness <- data.frame((IPIP_data_df$Q30_18)+(IPIP_data_df$Q30_48)+(IPIP_data_df$Q30_78)+(IPIP_data_df$Q30_108))
intellect <- data.frame((IPIP_data_df$Q30_23)+(IPIP_data_df$Q30_53)+(IPIP_data_df$Q30_83)+(IPIP_data_df$Q30_113))
liberalism  <- data.frame((IPIP_data_df$Q30_28)+(IPIP_data_df$Q30_58)+(IPIP_data_df$Q30_88)+(IPIP_data_df$Q30_118))
sum_openness <- data.frame(imagination+artistic_interests+emotionality+adventurousness+intellect+liberalism)
openness <- data.frame(imagination,artistic_interests,emotionality,adventurousness,intellect,liberalism,sum_openness)
colnames(openness) <- c('imagination','artistic_interests','emotionality','adventurousness','intellect','liberalism','openness')

#Calculating IPIP agreeableness (subcategories) 
trust <- data.frame((IPIP_data_df$Q30_4)+(IPIP_data_df$Q30_34)+(IPIP_data_df$Q30_64)+(IPIP_data_df$Q30_94))
morality  <- data.frame((IPIP_data_df$Q30_9)+(IPIP_data_df$Q30_39)+(IPIP_data_df$Q30_69)+(IPIP_data_df$Q30_99))
altruism <- data.frame((IPIP_data_df$Q30_14)+(IPIP_data_df$Q30_44)+(IPIP_data_df$Q30_74)+(IPIP_data_df$Q30_104))
cooperation <- data.frame((IPIP_data_df$Q30_19)+(IPIP_data_df$Q30_49)+(IPIP_data_df$Q30_79)+(IPIP_data_df$Q30_109))
modesty <- data.frame((IPIP_data_df$Q30_24)+(IPIP_data_df$Q30_54)+(IPIP_data_df$Q30_84)+(IPIP_data_df$Q30_114))
sympathy <- data.frame((IPIP_data_df$Q30_29)+(IPIP_data_df$Q30_59)+(IPIP_data_df$Q30_89)+(IPIP_data_df$Q30_119))
sum_agreeableness <- data.frame(trust+morality+altruism+cooperation+modesty+sympathy)
agreeableness <- data.frame(trust, morality,altruism,cooperation,modesty,sympathy,sum_agreeableness)
colnames(agreeableness) <- c('trust','morality','altruism','cooperation','modesty','sympathy','agreeableness')

#Calculating IPIP conscientiousness (subcategories) 
self_efficacy <- data.frame((IPIP_data_df$Q30_5)+(IPIP_data_df$Q30_35)+(IPIP_data_df$Q30_65)+(IPIP_data_df$Q30_95))
orderliness  <- data.frame((IPIP_data_df$Q30_10)+(IPIP_data_df$Q30_40)+(IPIP_data_df$Q30_70)+(IPIP_data_df$Q30_100))
dutifulness <- data.frame((IPIP_data_df$Q30_15)+(IPIP_data_df$Q30_45)+(IPIP_data_df$Q30_75)+(IPIP_data_df$Q30_105))
achievement_striving <- data.frame((IPIP_data_df$Q30_20)+(IPIP_data_df$Q30_50)+(IPIP_data_df$Q30_80)+(IPIP_data_df$Q30_110))
self_discipline <- data.frame((IPIP_data_df$Q30_25)+(IPIP_data_df$Q30_55)+(IPIP_data_df$Q30_85)+(IPIP_data_df$Q30_115))
cautiousness <- data.frame((IPIP_data_df$Q30_30)+(IPIP_data_df$Q30_60)+(IPIP_data_df$Q30_90)+(IPIP_data_df$Q30_120))
sum_conscientiousness <- data.frame(self_efficacy+orderliness+dutifulness+achievement_striving+self_discipline+cautiousness)
conscientiousness <- data.frame(self_efficacy,orderliness,dutifulness,achievement_striving,self_discipline,cautiousness,sum_conscientiousness)
colnames(conscientiousness) <- c('self_efficacy','orderliness','dutifulness','achievement_striving','self_discipline','cautiousness','conscientiousness')

# IPIP Neo scores
full_IPIP_NEO <- data.frame(agreeableness,extraversion,Neuroticism,openness,conscientiousness)
IPIP_neo_scores <- data.frame(sum_agreeableness,sum_extraversion,sum_Neuroticism,sum_openness,sum_conscientiousness)
colnames(IPIP_neo_scores) <- c('Agreeablenesss','Extraversion','Neuroticism','Openness','Conscientiousness')

#Calculating scores for IPM personality 

IPM_data <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPM.csv", head = TRUE)
absolute_IPM_values <- IPM_data[,0:12]
colnames(absolute_IPM_values) <- c("SelfAssert-IM", "Integration-IM","Security-IM","Individual-IM","Knowledge-IM","Empathy-IM",
                                   "SelfAssert-CE","Integration-CE","Security-CE","Individual-CE","Knowledge-CE","Empathy-CE")
IPM_data$ï..Red <- as.numeric(IPM_data$Red)
IPM_data$Yellow <- as.numeric(IPM_data$Yellow)
IPM_data$Green <- as.numeric(IPM_data$Green)
IPM_data$Blue <- as.numeric(IPM_data$Blue)
IPM_data$Know. <- as.numeric(IPM_data$Know.)
IPM_data$Emp. <- as.numeric(IPM_data$Emp.)
IPM_data$Red <- as.numeric(IPM_data$Red)
IPM_data$Yellow.1 <- as.numeric(IPM_data$Yellow.1)
IPM_data$Green.1 <- as.numeric(IPM_data$Green.1)
IPM_data$Blue.1 <- as.numeric(IPM_data$Blue.1)
IPM_data$Know..1 <- as.numeric(IPM_data$Know..1)
IPM_data$Emp..1 <- as.numeric(IPM_data$Emp..1)

# Personality Difference
self_assertion_diff <- data.frame(abs((IPM_data$Red)-(IPM_data$Red.1)))
integration_diff <-data.frame(abs((IPM_data$Yellow)-(IPM_data$Yellow.1)))
security_diff <-data.frame(abs((IPM_data$Green)-(IPM_data$Green.1)))
individuality_diff <-data.frame(abs((IPM_data$Blue)-(IPM_data$Blue.1)))
knowledge_diff <-data.frame(abs((IPM_data$Know.)-(IPM_data$Know..1)))
empathy_diff <-data.frame(abs((IPM_data$Emp.)-(IPM_data$Emp..1)))
differences_df <- data.frame(self_assertion_diff,integration_diff,security_diff,individuality_diff
                             ,knowledge_diff,empathy_diff)

## relative difference
r_self_assertion_diff <- data.frame(((IPM_data$Red)-(IPM_data$Red.1)))
r_integration_diff <-data.frame(((IPM_data$Yellow)-(IPM_data$Yellow.1)))
r_security_diff <-data.frame(((IPM_data$Green)-(IPM_data$Green.1)))
r_individuality_diff <-data.frame(((IPM_data$Blue)-(IPM_data$Blue.1)))
r_knowledge_diff <-data.frame(((IPM_data$Know.)-(IPM_data$Know..1)))
r_empathy_diff <-data.frame(((IPM_data$Emp.)-(IPM_data$Emp..1)))
r_differences_df <- data.frame(r_self_assertion_diff,r_integration_diff,r_security_diff,r_individuality_diff
                             ,r_knowledge_diff,r_empathy_diff)
colnames(r_differences_df) <- c('rel_Self_Assertion', 'rel_Integration', 'rel_Security', 'rel_Individuality', 'rel_Knowledge', 'rel_Empathy')

demographics <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Demographics .csv")
colnames(demographics) <- c('age','gender')

## full dataframe 
difference_IM_CE <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,IM_CE_df,CE_IM_df)
full_data_with_id <- data.frame(participant_id,sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,full_IPIP_NEO,differences_df,r_differences_df,IPM_all_data,demographics)
write.csv(full_data_with_id,"C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data with ID.csv", row.names = TRUE)

full_data <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score, IPIP_neo_scores[,0:5],differences_df,r_differences_df,absolute_IPM_values,demographics)
colnames(full_data) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                         'abs_Self_Assertion','abs_Integration', 'abs_Security', 'abs_Individuality', 'abs_Knowledge', 'abs_Empathy',
                         'rel_Self_Assertion', 'rel_Integration', 'rel_Security', 'rel_Individuality', 'rel_Knowledge', 'rel_Empathy',
                         "IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                         "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge","CE Empathy",
                         'Age','Gender')
IPIP_MH <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score, IPIP_neo_scores[,0:5])
colnames(IPIP_MH) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness')
# Other data frames
difference_IM_CE <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,r_differences_df)
full_data_IPIP_facets <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,full_IPIP_NEO,r_differences_df,absolute_IPM_values)

# Experimental Q1 personality measures 
exp_q1_df <- data.frame(IPIP_neo_scores[,0:5],absolute_IPM_values)
colnames(exp_q1_df) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                        'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                        'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
personality <- data.frame(IPIP_neo_scores[.0:5],differences_df,r_differences_df)
colnames(personality) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                         'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                         'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
mental_health_absolute <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,absolute_IPM_values)
colnames(mental_health_absolute) <- c('CESD', 'ASI', 'GAD', 'STAI',
                         'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                         'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
mental_health_diff <- data.frame(sum_CESD_score, sum_ASI_score, sum_GAD_score, sum_STAI_score,differences_df,r_differences_df)
colnames(mental_health_diff) <- c('CESD', 'ASI', 'GAD', 'STAI',
                           'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                           'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
write.csv(full_data,"C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data.csv", row.names = TRUE)

## Creating Violin Plots 
#install.packages("ggplot2")

library(ggplot2)

## all mental health in long data format
sum_mh_scores <- data.frame(sum_ASI_score, sum_CESD_score, sum_GAD_score, sum_STAI_score)
sum_mh_scores$Subject <- 1:nrow(sum_mh_scores)
sum_mh_scores$Subject <- as.factor(sum_mh_scores$Subject)

mh_long_df <- data.frame()
for (Subject in 1:nrow(sum_mh_scores)){
  Condition <- 'CESD'
  Score <- sum_mh_scores$sum_CESD_score[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  mh_long_df <- rbind(mh_long_df,one_cond)
  Condition <- 'GAD'
  Score <- sum_mh_scores$sum_GAD_score[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  mh_long_df <- rbind(mh_long_df,one_cond)
  Condition <- 'STAI'
  Score <- sum_mh_scores$sum_STAI_score[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  mh_long_df <- rbind(mh_long_df,one_cond)
  Condition <- 'ASI'
  Score <- sum_mh_scores$sum_ASI_score[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  mh_long_df <- rbind(mh_long_df,one_cond)
  }

dp <- ggplot(mh_long_df, aes(Condition, Score)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Mental Health Assessments", y = "Scores")
dp + theme_grey()

## Personality Plots IPIP Neo plots 
IPIP_neo_scores <- data.frame(sum_agreeableness,sum_extraversion,sum_Neuroticism,sum_openness,sum_conscientiousness)
IPIP_neo_scores$Subject <- 1:nrow(IPIP_neo_scores)
colnames(IPIP_neo_scores) <- c('A','E','N','O','C','Subject')
IPIP_neo_scores$Subject <- as.factor(IPIP_neo_scores$Subject)
IPIP_long_df <- data.frame()
for (Subject in 1:nrow(IPIP_neo_scores)){
  Condition <- 'Agreeableness'
  Score <- IPIP_neo_scores$A[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPIP_long_df <- rbind(IPIP_long_df,one_cond)
  Condition <- 'Extraversion'
  Score <- IPIP_neo_scores$E[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPIP_long_df <- rbind(IPIP_long_df,one_cond)
  Condition <- 'Neuroticism'
  Score <- IPIP_neo_scores$N[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPIP_long_df <- rbind(IPIP_long_df,one_cond)
  Condition <- 'Openness'
  Score <- IPIP_neo_scores$O[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPIP_long_df <- rbind(IPIP_long_df,one_cond)
  Condition <- 'Conscientiousness'
  Score <- IPIP_neo_scores$C[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPIP_long_df <- rbind(IPIP_long_df,one_cond)
}

dp <- ggplot(IPIP_long_df, aes(Condition, Score)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Dimensions", y = "Scores")
dp + theme_grey()

## Full IPM Plot IM and CE
IPM_violin_all_scores <- IPM_data[0:12]
IPM_violin_all_scores$Subject <- 1:nrow(IPM_violin_all_scores)
colnames(IPM_violin_all_scores) <- c("SelfAssert-IM", "Integration-IM","Security-IM","Individual-IM","Knowledge-IM","Empathy-IM",
                                 "SelfAssert-CE","Integration-CE","Security-CE","Individual-CE","Knowledge-CE","Empathy-CE","subject")
IPM_violin_all_scores$subject <- as.factor(IPM_violin_all_scores$subject)
IPM_long_df <- data.frame()
for (Subject in 1:nrow(IPM_violin_all_scores)){
  Condition <- 'Self Assertion IM'
  Score <- IPM_violin_all_scores$`SelfAssert-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Self Assertion CE'
  Score <- IPM_violin_all_scores$`SelfAssert-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Integration IM'
  Score <- IPM_violin_all_scores$`Integration-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Integration CE'
  Score <- IPM_violin_all_scores$`Integration-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Security IM'
  Score <- IPM_violin_all_scores$`Security-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Security CE'
  Score <- IPM_violin_all_scores$`Security-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Individuality IM'
  Score <- IPM_violin_all_scores$`Individual-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Individuality CE'
  Score <- IPM_violin_all_scores$`Integration-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Knowledge IM'
  Score <- IPM_violin_all_scores$`Knowledge-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Knowledge CE'
  Score <- IPM_violin_all_scores$`Knowledge-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Empathy IM'
  Score <- IPM_violin_all_scores$`Empathy-IM`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
  Condition <- 'Empathy CE'
  Score <- IPM_violin_all_scores$`Empathy-CE`[Subject]
  one_cond <- data.frame(Subject, Condition,Score)
  IPM_long_df <- rbind(IPM_long_df,one_cond)
}

dp <- ggplot(IPM_long_df, aes(Condition, Score)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(x="Dimensions", y = "Scores")
dp + theme_grey()






