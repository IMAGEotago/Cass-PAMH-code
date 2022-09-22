# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/corrplot_0.90.tar.gz", repos = NULL, type = "source")
# install.packages("~/Network-Shares/DataLabNas/GenData/R_User_Libraries/gtsummary_1.4.2.tar.gz", repos = NULL, type = "source")
# install.packages("xlsx")

library(haven)
library(Hmisc)
library(corrplot)
library(gtsummary)
library(xlsx)

## Personality Measures IPIP and IPM
corMatrix <- rcorr(as.matrix(exp_q1_df))
# Create the correlation matrix figure uncorrected 
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')
r_values_personality_correlation <- (corMatrix[["r"]])

# corrections 
pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,17,17)
colnames(r_p) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                   'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
rownames(r_p) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                   'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
colnames(exp_q1_df) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                         'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                         'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
corMatrix <- rcorr(as.matrix(exp_q1_df))
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

## Personality and IPM differences
corMatrix <- rcorr(as.matrix(personality))
# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper', title=title,mar=c(0,0,1,0))
title <- "Uncorrected P values for correlations between personality measures"
r_values_personality_correlation <- (corMatrix[["r"]])

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,17,17)
colnames(r_p) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
rownames(r_p) <- c('Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
colnames(personality) <- c('Agreeableness','Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                         'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')

# Set up FDR corrected correlation matrix
corMatrix <- rcorr(as.matrix(personality))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper', title=title,mar=c(0,0,1,0),diag=FALSE,number.cex = 0.5,cl.pos = 'n')

## IPM diff and MH
corMatrix <- rcorr(as.matrix(mental_health_diff))
# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper', title=title,mar=c(0,0,1,0))
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
colnames(r_p) <- c('CESD', 'ASI', 'GAD', 'STAI',
                   'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                   'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
rownames(r_p) <- c('CESD', 'ASI', 'GAD', 'STAI',
                    'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                    'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')
colnames(mental_health_diff) <- c('CESD', 'ASI', 'GAD', 'STAI',
                                      'Abs Self Assertion', 'Abs Integration', 'Abs Security', 'Abs Individuality', 'Abs Knowledge', 'Abs Empathy',
                                      'Rel Self Assertion', 'Rel Integration', 'Rel Security', 'Rel Individuality', 'Rel Knowledge', 'Rel Empathy')

# Set up FDR corrected correlation matrix
corMatrix <- rcorr(as.matrix(mental_health_diff))
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
corMatrix <- rcorr(as.matrix(IPIP_MH))
# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper', title=title,mar=c(0,0,1,0))
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,9,9)
colnames(r_p) <- c('depCESD', 'AnxASI', 'AnxGAD', 'AnxSTAI',
                   'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness')
                   
rownames(r_p) <- c('depCESD', 'AnxASI', 'AnxGAD', 'AnxSTAI',
                   'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness')
colnames(IPIP_MH) <- c('depCESD', 'AnxASI', 'AnxGAD', 'AnxSTAI',
                                      'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness')

corMatrix <- rcorr(as.matrix(IPIP_MH))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper', title=title,mar=c(0,0,1,0),diag=FALSE,number.cex = 0.5,cl.pos = 'n')

r_IPIP_MH <- corMatrix[["r"]]
p_IPIP_MH <- corMatrix[["P"]]

write.xlsx(r_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_IPIP_MH.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_IPIP_MH, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_values_IPIP_MH.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

# Mental Health Absolute
corMatrix <- rcorr(as.matrix(mental_health_absolute))
# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
colnames(r_p) <- c('CESD', 'ASI', 'GAD', 'STAI',
                   'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                   'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
rownames(r_p) <- c('CESD', 'ASI', 'GAD', 'STAI',
                   'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                   'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')
colnames(mental_health_absolute) <- c('CESD', 'ASI', 'GAD', 'STAI',
                         'IM Self Assertion', 'IM Integration', 'IM Security', 'IM Individuality', 'IM Knowledge', 'IM Empathy',
                         'CE Self Assertion', 'CE Integration', 'CE Security', 'CE Individuality', 'CE Knowledge', 'CE Empathy')

corMatrix <- rcorr(as.matrix(mental_health_absolute))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
r_MH_IPM_measures <- (corMatrix[["r"]])
p_MH_IPM_measures <- (corMatrix[["P"]])

View(corMatrix[["P"]])

write.xlsx(r_MH_IPM_measures, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_MH_IPM_measures.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(p_MH_IPM_measures, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/p_MH_IPM_measures.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


# Diff 
corMatrix <- rcorr(as.matrix(difference_IM_CE))
colnames(difference_IM_CE) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI','Rel_Self_Assertion', 'Rel_Integration', 'Rel_Security', 'Rel_Individuality', 'Rel_Knowledge', 'Rel_Empathy',)
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",'upper',diag=FALSE)

pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,16,16)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank")
colnames(r_p) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI','IM_CE_Self_Assertion', 'IM_CE_Integration', 'IM_CE_Security', 'IM_CE_Individuality', 'IM_CE_Knowledge', 'IM_CE_Empathy',
                         'CE_IM_Self_Assertion', 'CE_IM_Integration', 'CE_IM_Security', 'CE_IM_Individuality', 'CE_IM_Knowledge', 'CE_IM_Empathy')
rownames(r_p) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI','IM_CE_Self_Assertion', 'IM_CE_Integration', 'IM_CE_Security', 'IM_CE_Individuality', 'IM_CE_Knowledge', 'IM_CE_Empathy',
                   'CE_IM_Self_Assertion', 'CE_IM_Integration', 'CE_IM_Security', 'CE_IM_Individuality', 'CE_IM_Knowledge', 'CE_IM_Empathy')
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=r_p, sig.level=(0.05), insig="blank")


## IPIP facets correlation 
ipm_measures <- IPM_data[0:12]
colnames(IPM_data) <- c("SelfAssert_IM", "Integration_IM","Security_IM","Individual_IM","Knowledge_IM","Empathy_IM",
"SelfAssert_CE","Integration_CE","Security_CE","Individual_CE","Knowledge_CE","Empathy_CE")
facet_IPM_dataset <- data.frame(conscientiousness,ipm_measures)
View(facet_IPM_dataset)

corMatrix <- rcorr(as.matrix(facet_IPM_dataset))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank")
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")
IPIP_facets <- (corMatrix[["P"]])
IPIP_r_values <- (corMatrix[["r"]])


write.xlsx(IPIP_facets, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPIP_facets.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(IPIP_r_values, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/IPIP_r_values.xlsx"
           , sheetName = "Sheet2",col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(r_values_personality_correlation, "C:/Users/Alan/OneDrive - University of Otago/2022/Thesis/Dissertation Data/r_values_personality_correlation.xlsx"
           , sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)


#full_data <- full_data[,2:46]
# Set up the correlation matrix
## Personality Measures 
corMatrix <- rcorr(as.matrix(full_data))
# Create the correlation matrix figure
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank",type = 'upper')
#corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", order = "hclust")

colnames(full_data) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                         'abs Self Assertion','abs Integration', 'abs Security', 'abs Individuality', 'abs Knowledge', 'abs Empathy',
                         'rel Self Assertion', 'rel Integration', 'rel Security', 'rel Individuality', 'rel Knowledge', 'rel Empathy',
                         "SelfAssert_IM", "Integration_IM","Security_IM","Individual_IM","Knowledge_IM","Empathy_IM",
                         "SelfAssert_CE","Integration_CE","Security_CE","Individual_CE","Knowledge_CE","Empathy_CE",'age','biological sex')
pvalues <- corMatrix[["P"]]
pvalues[lower.tri(pvalues)] <- NA
p <- (p.adjust(pvalues, method = "fdr", length(pvalues)))
r_p <- matrix(p,35,35)
colnames(r_p) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'abs Self Assertion','abs Integration', 'abs Security', 'abs Individuality', 'abs Knowledge', 'abs Empathy',
                   'rel Self Assertion', 'rel Integration', 'rel Security', 'rel Individuality', 'rel Knowledge', 'rel Empathy',
                   "SelfAssert_IM", "Integration_IM","Security_IM","Individual_IM","Knowledge_IM","Empathy_IM",
                   "SelfAssert_CE","Integration_CE","Security_CE","Individual_CE","Knowledge_CE","Empathy_CE",'age','biological sex')
rownames(r_p) <- c('depCESD','AnxASI','AnxGAD','AnxSTAI', 'Agreeableness', 'Extraversion', 'Neuroticism', 'Openness', 'Conscientiousness',
                   'abs Self Assertion','abs Integration', 'abs Security', 'abs Individuality', 'abs Knowledge', 'abs Empathy',
                   'rel Self Assertion', 'rel Integration', 'rel Security', 'rel Individuality', 'rel Knowledge', 'rel Empathy',
                   "SelfAssert_IM", "Integration_IM","Security_IM","Individual_IM","Knowledge_IM","Empathy_IM",
                   "SelfAssert_CE","Integration_CE","Security_CE","Individual_CE","Knowledge_CE","Empathy_CE",'age','biological sex')


corMatrix <- rcorr(as.matrix(full_data))
col <- colorRampPalette(c("blue", "white", "red"))(20)
corrplot(corMatrix[["r"]], method="circle", col=col, tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE)
corrplot(corMatrix[["r"]], method="number", col='black', tl.col="black", tl.srt=60, p.mat=corMatrix[["P"]], sig.level=(0.05), insig="blank", type = 'upper',diag=FALSE,number.cex = 0.3,cl.pos = 'n')




