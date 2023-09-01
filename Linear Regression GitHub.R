## Logisitic Regression 
data <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data 2023.csv")
colnames(data) <- c('X','Participant ID', 'Age','Biological_Sex',"IM_Self_Assertion", "IM_Integration","IM_Security","IM_Individual","IM_Knowledge","IM_Empathy",
                         "CE_Self_Assertion","CE_Integration","CE_Security","CE_Individual","CE_Knowledge","CE_Empathy",
                    'AnxGAD','depCESD','AnxASI','ASI Physical Concerns','ASI Cognitive Concerns', 'ASI Social Concerns','AnxSTAI', 'Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness',
                    'rel_Self_Assertion', 'rel_Integration', 'rel_Security', 'rel_Individuality', 'rel_Knowledge', 'rel_Empathy',
                    'abs_Self_Assertion','abs_Integration', 'abs_Security', 'abs_Individuality', 'abs_Knowledge', 'abs_Empathy')
## Explantory Question 1 relative differences and mental health measures


#GAD
lmGADQ1 <- lm(AnxGAD~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy
             +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
               abs_Knowledge+abs_Empathy, data = data)
summary(lmGADQ1)

#STAI
lmSTAIQ1 <- lm(AnxSTAI~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy
              +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = data)
summary(lmSTAIQ1)

#ASI
lmASIQ1 <- lm(AnxASI~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+
                abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = data)

summary(lmASIQ1)

#CESD
lmCESDQ1 <- lm(depCESD~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy
              +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = data)
summary(lmCESDQ1)



## Explanatory Q2 absolute values and mental health questionnaires
#Create the linear regression

#GADs
lmGADQ2 = lm(AnxGAD~Age+Biological_Sex+IM_Self_Assertion+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
             CE_Self_Assertion+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = data)
summary(lmGADQ2)

#CESD
lmCESDQ2 = lm(depCESD~Age+Biological_Sex+IM_Self_Assertion+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
                CE_Self_Assertion+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = data)
summary(lmCESDQ2)

#ASI
lmASIQ2 = lm(AnxASI~Age+Biological_Sex+IM_Self_Assertion+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
               CE_Self_Assertion+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = data)
summary(lmASIQ2)

#STAI
lmSTAIQ2 = lm(AnxSTAI~Age+Biological_Sex+IM_Self_Assertion+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
                CE_Self_Assertion+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = data)
summary(lmSTAIQ2)


# Q3 regression with IPIP NEO, IPM and mental health
#GAD
lmGADQ3 = lm(AnxGAD~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = data)
summary(lmGADQ3)

#CESD
lmCESDQ3 = lm(depCESD~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = data)
summary(lmCESDQ3)

#ASI
lmASIQ3 = lm(AnxASI~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = data)
summary(lmASIQ3)

#STAI
lmSTAIQ3 = lm(AnxSTAI~Age+Biological_Sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
              +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
              +Neuroticism+Openness+Conscientiousness, data = data)
summary(lmSTAIQ3)


