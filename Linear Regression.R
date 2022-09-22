## Logisitic Regression 
full_data <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data.csv")
full_data$biological_sex <- as.factor(full_data$biological_sex)
## Explantory Question 1 relative differences and mental health measures
colnames(full_data) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neurotism', 'Openness', 'Conscientousness',
                         'abs_Self_Assertion','abs_Integration', 'abs_Security', 'abs_Individuality', 'abs_Knowledge', 'abs_Empathy',
                         'rel_Self_Assertion', 'rel_Integration', 'rel_Security', 'rel_Individuality', 'rel_Knowledge', 'rel_Empathy',
                         "IM_SelfAssert", "IM_Integration","IM_Security","IM_Individual","IM_Knowledge","IM_Empathy",
                         "CE_SelfAssert","CE_Integration","CE_Security","CE_Individual","CE_Knowledge","CE_Empathy",
                         'age','biological_sex')
View(full_data)

#GAD
lmGADQ1 <- lm(AnxGAD~age+biological_sex+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy
             +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
               abs_Knowledge+abs_Empathy, data = full_data)
summary(lmGADQ1)

#STAI
lmSTAIQ1 <- lm(AnxSTAI~age+biological_sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy
              +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = full_data)
summary(lmSTAIQ1)

#ASI
lmASIQ1 <- lm(AnxASI~age+biological_sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+
                abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = full_data)
summary(lmASIQ1)

#CESD
lmCESDQ1 <- lm(depCESD~age+biological_sex+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy
              +abs_Self_Assertion+abs_Integration+abs_Security+abs_Individuality+
                abs_Knowledge+abs_Empathy, data = full_data)
summary(lmCESDQ1)


## Explanatory Q2 absolute values and mental health questionnaires
#Create the linear regression

#GADs
lmGADQ2 = lm(AnxGAD~age+biological_sex+IM_SelfAssert+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
             CE_SelfAssert+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = full_data)
summary(lmGADQ2)

#CESD
lmCESDQ2 = lm(depCESD~age+biological_sex+IM_SelfAssert+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
                CE_SelfAssert+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = full_data)
summary(lmCESDQ2)

#ASI
lmASIQ2 = lm(AnxASI~age+biological_sex+IM_SelfAssert+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
               CE_SelfAssert+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = full_data)
summary(lmASIQ2)

#STAI
lmSTAIQ2 = lm(AnxSTAI~age+biological_sex+IM_SelfAssert+IM_Integration+IM_Security+IM_Individual+IM_Knowledge+IM_Empathy+
                CE_SelfAssert+CE_Integration+CE_Security+CE_Individual+CE_Knowledge+CE_Empathy, data = full_data)
summary(lmSTAIQ2)

View(full_data)

# Q3 regression with IPIP NEO, IPM and mental health
#GAD
lmGADQ3 = lm(AnxGAD~Age+Gender+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = full_data)
summary(lmGADQ3)

#CESD
lmCESDQ3 = lm(depCESD~Age+Gender+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = full_data)
summary(lmCESDQ3)

#ASI
lmASIQ3 = lm(AnxASI~Age+Gender+rel_Self_Assertion+rel_Integration+rel_Security
             +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
             +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
             +Neuroticism+Openness+Conscientiousness, data = full_data)
summary(lmASIQ3)

#STAI
lmSTAIQ3 = lm(AnxSTAI~Age+Gender+rel_Self_Assertion+rel_Integration+rel_Security
              +rel_Individuality+rel_Knowledge+rel_Empathy+abs_Self_Assertion+abs_Integration+abs_Security
              +abs_Individuality+abs_Knowledge+abs_Empathy+Agreeableness+Extraversion
              +Neuroticism+Openness+Conscientiousness, data = full_data)
summary(lmSTAIQ3)


