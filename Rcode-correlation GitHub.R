# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/corrplot_0.90.tar.gz", repos = NULL, type = "source")
# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/gtsummary_1.4.2.tar.gz", repos = NULL, type = "source")
# install.packages("xlsx")

library(haven)
library(Hmisc)
library(corrplot)
library(gtsummary)
library(xlsx)

## Colour is IM 
## .1 is CE

data <- read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/Full Data 2023.csv")
colnames(data) <- c('X','Participant ID', 'Age','Biological Sex',"IM Self Assertion", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                    "CE Self Assertion","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy',
                    'AnxGAD','depCESD','AnxASI','ASI Physical Concerns','ASI Cognitive Concerns', 'ASI Social Concerns','AnxSTAI', 'Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness',
                    'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                    'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')

## Full Matrix - uncorrected 
full_matrix <- data %>% select(Age:AnxASI,AnxSTAI:`Abs Empathy`)
corMatrix <- rcorr(as.matrix(full_matrix))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60,p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',mar=c(0,0,1,0),diag=FALSE,number.cex = 0.45,cl.pos = 'n')


## Experimental Q1 - Mental health and IPIP NEO - corrected

exp_q1_df <- data %>% select('AnxGAD':'AnxASI',AnxSTAI, Conscientiousness:Openness)
corMatrix <- rcorr(as.matrix(exp_q1_df))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,9,9)
colnames(r_p) <- c('AnxGAD','DepCESD','AnxASI','AnxSTAI','Conscientiousness',
                   'Agreeableness', 'Neuroticism', 'Extraversion','Openness')

rownames(r_p) <- c('AnxGAD','DepCESD','AnxASI','AnxSTAI','Conscientiousness',
                      'Agreeableness', 'Neuroticism', 'Extraversion','Openness')

colnames(exp_q1_df) <- c('AnxGAD','DepCESD','AnxASI','AnxSTAI','Conscientiousness',
                   'Agreeableness', 'Neuroticism', 'Extraversion','Openness')

corMatrix <- rcorr(as.matrix(exp_q1_df))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE,number.cex = 0.5,cl.pos = 'n')

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_MH.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_MH.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


## Experimental Q2
exp_q2_df <- data %>% select('IM Self Assertion':'CE Empathy', Conscientiousness:Openness)
corMatrix <- rcorr(as.matrix(exp_q2_df))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0

# Create the correlation matrix figure uncorrected 
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')
r_values_personality_correlation <- (corMatrix[["r"]])


# corrections 
pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,17,17)
colnames(r_p) <- c("IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                   "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy',
                   'Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness')
rownames(r_p) <- c("IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                   "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy',
                   'Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness')
colnames(exp_q2_df) <- c("IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                         "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy',
                         'Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness')

corMatrix <- rcorr(as.matrix(exp_q2_df))
col <- colorRampPalette(c("blue", "white", "red"))(20)
title <- "Corrected FDR P values for correlations between personality measures"
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper', title=title,mar=c(0,0,1,0),diag=FALSE,number.cex = 0.5,cl.pos = 'n')

# Getting R and P values
pvalues <- corMatrix[["P"]]
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,17,17)
p_excel <- (r_p)
write.xlsx(p_excel, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p values for IPIPNEO and IPM CE and IM.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)
r_excel <- (corMatrix[["r"]])
write.xlsx(r_excel, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r values for IPIPNEO and IPM CE and IM.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


## Experiment Q3
## Personality and IPM differences
exp_q3_df <- data %>% select(Conscientiousness:'Abs Empathy')
corMatrix <- rcorr(as.matrix(exp_q3_df))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0

# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')
title <- "Uncorrected P values for correlations between personality measures"
r_values_personality_correlation <- (corMatrix[["r"]])

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,17,17)
colnames(r_p) <- c('Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                   'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')
rownames(r_p) <- c('Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                   'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')
colnames(exp_q3_df) <- c('Conscientiousness','Agreeableness','Neuroticism','Extraversion','Openness',
                           'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                           'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')

# Set up FDR corrected correlation matrix

corMatrix <- rcorr(as.matrix(exp_q3_df))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper', title=title,mar=c(0,0,1,0),diag=FALSE,number.cex = 0.5,cl.pos = 'n')


## Experimental Q4
## IPM diff and MH
exp_q4_df <- data %>% select('AnxGAD','depCESD','AnxASI','AnxSTAI','Rel Self Assertion':'Abs Empathy')
corMatrix <- rcorr(as.matrix(exp_q4_df))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0

col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper', title=title,mar=c(0,0,1,0))
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
colnames(r_p) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                   'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')

rownames(r_p) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                   'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')
colnames(exp_q4_df) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                                  'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy',
                                  'Abs Self Assertion','Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy')

# Set up FDR corrected correlation matrix
corMatrix <- rcorr(as.matrix(exp_q4_df))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper', diag=FALSE,number.cex = 0.5,cl.pos = 'n')

# Getting R and P values
pvalues <- corMatrix[["P"]]
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
p_excel <- (r_p)
write.xlsx(pvalues, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p values for MH and IPM Diff.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

r_excel <- (corMatrix[["r"]])
write.xlsx(r_excel, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r values for MH and IPM Diff.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



## IPIP and MH
# Mental Health Absolute
exp_q5_df <- data %>% select('AnxGAD','depCESD','AnxASI','AnxSTAI','IM Self Assertion':'CE Empathy')
corMatrix <- rcorr(as.matrix(exp_q5_df))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0

# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
colnames(r_p) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                   "IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                   "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy')
rownames(r_p) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                   "IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                   "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy')
colnames(exp_q5_df) <- c('AnxGAD','depCESD','AnxASI','AnxSTAI',
                         "IM SelfAssert", "IM Integration","IM Security","IM Individual","IM Knowledge","IM Empathy",
                         "CE SelfAssert","CE Integration","CE Security","CE Individual","CE Knowledge", 'CE Empathy')

corMatrix <- rcorr(as.matrix(exp_q5_df))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
r_MH_IPM_measures <- (corMatrix[["r"]])
p_MH_IPM_measures <- (corMatrix[["P"]])


write.xlsx(r_MH_IPM_measures, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_MH_IPM_measures.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_MH_IPM_measures, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_MH_IPM_measures.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


## Correlation between IPIP Sub Categories and IPM 
data_IPIP <-read.csv("C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPIP Data 2023.csv")                                          
View(data_IPIP)
## Neuroticism Sub and IPM 
neuroticism_sub <- data_IPIP %>% select('IM.Self.Assertion':vulnerability)

corMatrix <- rcorr(as.matrix(neuroticism_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_Neur.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_Neu.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



extraversion_sub <- data_IPIP %>% select('IM.Self.Assertion':'CE.Empathy',friendliness:cheerfulness)

corMatrix <- rcorr(as.matrix(extraversion_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_Ext.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_Ext.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)




open_sub <- data_IPIP %>% select('IM.Self.Assertion':'CE.Empathy',imagination:liberalism)

corMatrix <- rcorr(as.matrix(open_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_ope.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_ope.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



agree_sub <- data_IPIP %>% select('IM.Self.Assertion':'CE.Empathy',trust:sympathy)

corMatrix <- rcorr(as.matrix(agree_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_agr.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_agr.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



open_sub <- data_IPIP %>% select('IM.Self.Assertion':'CE.Empathy',imagination:liberalism)

corMatrix <- rcorr(as.matrix(open_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_ope.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_ope.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)



con_sub <- data_IPIP %>% select('IM.Self.Assertion':'CE.Empathy',self_efficacy:cautiousness)

corMatrix <- rcorr(as.matrix(con_sub))
corMatrix[["P"]][is.na(corMatrix[["P"]])] <- 0
col <- colorRampPalette(c("blue", "white", "red"))(20)
#corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
#col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_Sub_con.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_Sub_con.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)
