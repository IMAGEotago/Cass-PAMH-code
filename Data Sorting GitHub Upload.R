# Importing Data
##install.packages("dplyr")

## Matching the Data
library(dplyr)

intrinsic <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Data Files for R/Intrinsic Motivations, Personality Traits and Mental Health Survey_May 10, 2023_08.35.csv")
IPM <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Data Files for R/2023 IPM Export.csv")

intrinsic <- intrinsic[1:445,]
intrinsic.7 <- intrinsic %>% 
  mutate(id = substr(intrinsic$ResponseId,1,7)) 

combined.all <- full_join(intrinsic.7, IPM, by = join_by(id == X))

combined.match <- inner_join(intrinsic.7, IPM, by = join_by(id == X))

mh_scores <- colnames(combined.match[35:219])

questionnaires <- combined.match %>% 
  mutate_at(mh_scores,as.numeric)



## Mental Health Data
## Correcting and Calculations with the Data

# GAD7 Data 
# GAD-7 - just subtract 1 from all of the scores, no need to invert
questionnaire_GAD <- questionnaires %>%
  mutate(across(starts_with("GAD.7"),~.-1)) %>%
  mutate(GAD_sum = rowSums(select(.,GAD.7_1:GAD.7_7)))


#CESD Data
#	CESD - convert scores from 1, 5, 6, 7 to 0, 1, 2, 3; then invert relevant scores
reverse <- function(x){((x-max(x))*-1)+min(x)}

questionnaire_CESD <- questionnaire_GAD %>%
  mutate(across(starts_with("CESD"),~ifelse(.==1,0,ifelse(.==5,1,ifelse(.==6,2,ifelse(.==7,3,.)))))) %>%
  mutate_at(.vars = c("CESD_4","CESD_8","CESD_12","CESD_16"),~reverse(.))

questionnaire_CESD <- questionnaire_CESD %>%
  mutate(across(starts_with("CESD"))) %>%
  mutate(CESD_sum = rowSums(select(.,"CESD_1":"CESD_20")))


#ASI Data 
# ASI scores
questionnaire_ASI <- questionnaire_CESD %>%
  mutate(across(starts_with("Q29_"),~.-1)) %>%
  mutate(ASI_sum = rowSums(select(.,Q29_1:Q29_19)))

# ASI subscores 
questionnaire_ASI <- questionnaire_ASI %>% 
  mutate(ASI_physical_concerns = rowSums(select(.,Q29_4, Q29_12, Q29_7, Q29_7, Q29_15, Q29_3)))

questionnaire_ASI <- questionnaire_ASI %>% 
  mutate(ASI_cognitive_concerns = rowSums(select(.,Q29_14, Q29_19, Q29_10, Q29_16, Q29_2, Q29_5)))

questionnaire_ASI <- questionnaire_ASI %>% 
  mutate(ASI_social_concerns = rowSums(select(.,Q29_9, Q29_6, Q29_11, Q29_13, Q29_18, Q29_1)))

#STAI Data 
## Invert relevant scores 
questionnaire_STAI <- questionnaire_ASI %>%
  mutate_at(.vars = c("STAI_S_1","STAI_S_2","STAI_S_5","STAI_S_8","STAI_S_11","STAI_S_12","STAI_S_15","STAI_S_16","STAI_S_19","STAI_S_20"),~reverse(.)) %>%
  mutate(STAI_sum = rowSums(select(.,STAI_S_1:STAI_S_20)))


# Personality Data 
IPIP_data_df <- combined.match %>% select(Q30_1:Q30_120)

#Calculating IPIP Neuroticism (subcategories) 
questionnaire_IPIP <- questionnaire_STAI
questionnaire_IPIP<- questionnaire_IPIP %>% 
  mutate(anxiety = rowSums(select(.,Q30_1, Q30_31, Q30_61, Q30_91))) %>%
  mutate(anger = rowSums(select(.,Q30_6, Q30_36, Q30_66, Q30_96))) %>%
  mutate(depression = rowSums(select(.,Q30_11, Q30_41, Q30_71, Q30_101))) %>%
  mutate(self_consciousness = rowSums(select(.,Q30_16, Q30_46, Q30_76, Q30_106))) %>%
  mutate(immoderation = rowSums(select(.,Q30_21, Q30_51, Q30_81, Q30_111))) %>%
  mutate(vulnerability = rowSums(select(.,Q30_26, Q30_56, Q30_86, Q30_116)))

#Calculating IPIP Extraversion (subcategories) 
questionnaire_IPIP<- questionnaire_IPIP %>% 
  mutate(friendliness = rowSums(select(.,Q30_2, Q30_32, Q30_62, Q30_92))) %>%
  mutate(gregariousness = rowSums(select(.,Q30_7, Q30_37, Q30_67, Q30_97))) %>%
  mutate(assertiveness = rowSums(select(.,Q30_12, Q30_42, Q30_72, Q30_102))) %>%
  mutate(activity_level = rowSums(select(.,Q30_17, Q30_47, Q30_77, Q30_107))) %>%
  mutate(excitement_seeking = rowSums(select(.,Q30_22, Q30_52, Q30_82, Q30_112))) %>%
  mutate(cheerfulness = rowSums(select(.,Q30_27, Q30_57, Q30_87, Q30_117)))

#Calculating IPIP Openness (subcategories) 
questionnaire_IPIP<- questionnaire_IPIP %>% 
  mutate(imagination = rowSums(select(.,Q30_3, Q30_33, Q30_63, Q30_93))) %>%
  mutate(artistic_interests = rowSums(select(.,Q30_8, Q30_38, Q30_68, Q30_98))) %>%
  mutate(emotionality = rowSums(select(.,Q30_13, Q30_43, Q30_73, Q30_103))) %>%
  mutate(adventurousness = rowSums(select(.,Q30_18, Q30_48, Q30_78, Q30_108))) %>%
  mutate(intellect = rowSums(select(.,Q30_23, Q30_53, Q30_83, Q30_113))) %>%
  mutate(liberalism = rowSums(select(.,Q30_28, Q30_58, Q30_88, Q30_118)))

#Calculating IPIP agreeableness (subcategories) 
questionnaire_IPIP<- questionnaire_IPIP %>% 
  mutate(trust = rowSums(select(.,Q30_4, Q30_34, Q30_64, Q30_94))) %>%
  mutate(morality = rowSums(select(.,Q30_9, Q30_39, Q30_69, Q30_99))) %>%
  mutate(altruism = rowSums(select(.,Q30_14, Q30_44, Q30_74, Q30_104))) %>%
  mutate(cooperation = rowSums(select(.,Q30_19, Q30_49, Q30_79, Q30_109))) %>%
  mutate(modesty = rowSums(select(.,Q30_24, Q30_54, Q30_84, Q30_114))) %>%
  mutate(sympathy = rowSums(select(.,Q30_29, Q30_59, Q30_89, Q30_119)))

#Calculating IPIP consientiousness (subcategories) 
questionnaire_IPIP<- questionnaire_IPIP %>% 
  mutate(self_efficacy = rowSums(select(.,Q30_5, Q30_35, Q30_65, Q30_95))) %>%
  mutate(orderliness = rowSums(select(.,Q30_10, Q30_40, Q30_70, Q30_100))) %>%
  mutate(dutifulness = rowSums(select(.,Q30_15, Q30_45, Q30_75, Q30_105))) %>%
  mutate(achievement_striving = rowSums(select(.,Q30_20, Q30_50, Q30_80, Q30_110))) %>%
  mutate(self_discipline = rowSums(select(.,Q30_25, Q30_55, Q30_85, Q30_115))) %>%
  mutate(cautiousness = rowSums(select(.,Q30_30, Q30_60, Q30_90, Q30_120)))

# IPIP Neo category scores
questionnaire_IPIP <- questionnaire_IPIP %>%
  mutate(conscientiousness = rowSums(select(.,self_efficacy, orderliness, dutifulness, achievement_striving, self_discipline, cautiousness))) %>%
  mutate(agreeableness = rowSums(select(.,trust, morality,altruism,cooperation,modesty,sympathy))) %>%
  mutate(neuroticism = rowSums(select(.,anxiety, anger, depression, self_consciousness, immoderation, vulnerability))) %>%
  mutate(extraversion = rowSums(select(.,friendliness, gregariousness, assertiveness, activity_level, excitement_seeking, cheerfulness))) %>%
  mutate(openness = rowSums(select(.,imagination,artistic_interests,emotionality,adventurousness,intellect,liberalism,)))

## IPM DATA 
IPM <- colnames(combined.match[230:241])

questionnaires_IPIP <- combined.match %>% 
  mutate_at(IPM,as.numeric)

## Relative Values of personality Difference 
questionnaire_IPIP <- questionnaire_IPIP %>%
  mutate(self_asseration_diff = Red-Red.1) %>%
  mutate(integration_diff = Yellow-Yellow.1) %>%
  mutate(security_diff = Green-Green.1) %>%
  mutate(individuality_diff = Blue-Blue.1) %>%
  mutate(knowledge_diff = Know.-Know..1) %>%
  mutate(empathy_diff = Emp.-Emp..1)

## Absolute difference
questionnaire_IPIP <- questionnaire_IPIP %>%
  mutate(a_self_asseration_diff = abs(self_asseration_diff)) %>%
  mutate(a_integration_diff = abs(integration_diff)) %>%
  mutate(a_security_diff = abs(security_diff)) %>%
  mutate(a_individuality_diff = abs(individuality_diff)) %>% 
  mutate(a_knowledge_diff = abs(knowledge_diff)) %>% 
  mutate(a_empathy_diff = abs(empathy_diff))


## Compact Dataframe
data <- questionnaire_IPIP %>% select(ResponseId,Q10,Q11,Red:Emp..1,GAD_sum:STAI_sum,conscientiousness:a_empathy_diff)
write.csv(data,"C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data 2023.csv", row.names = TRUE)


data_ipip_sub <- questionnaire_IPIP %>% select(Red:Emp..1,anxiety:cautiousness)
colnames(data_ipip_sub) <- c("IM Self Assertion", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                             "CE Self Assertion","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy',
                             'anxiety', 'anger', 'depression','self_consciousness','immoderation','vulnerability',
                             'friendliness','gregariousness','assertiveness','activity_level','excitement_seeking','cheerfulness',
                             'imagination','artistic_interests','emotionality','adventurousness','intellect','liberalism',
                             'trust','morality','altruism','cooperation','modesty','sympathy',
                             'self_efficacy','orderliness','dutifulness','achievement_striving','self_discipline','cautiousness'
)
write.csv(data_ipip_sub,"C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPIP Data 2023.csv", row.names = TRUE)                                               
