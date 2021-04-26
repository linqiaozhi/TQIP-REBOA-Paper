library(tidyverse)
library(ggrepel)
library(FNN)
library('MatchIt')
library('caret')
library(haven)
library('cobalt')
library(arsenal)
library(twang)
library(survey)
library(ggplot2)
print.rds <- function( mat, outs)  {
    rds <- as.data.frame(matrix(NA, ncol=3,nrow=length(outs)))
    rownames(rds) <- c(outs)
    colnames(rds) <- c('estimate','low_ci', 'high_ci')
    rds$y_axis <- 1:nrow(rds)
    rds$outcome <- rownames(rds)
    for (i in 1:length(outcomes)) {
            # In the raw data
            outcome  <- outcomes[i]
            mat <- mat %>% mutate(Y=get(outcome)=='Complication')  %>% drop_na(Y)
            Y_A1 <- with(mat, mean(Y[ reboa == 1]))
            Y_A0 <- with(mat, mean(Y[ reboa == 0]))
            SE1 <- with(mat, sqrt( Y_A1*(1-Y_A1)/sum(reboa==1)))
            SE2 <- with(mat, sqrt( Y_A0*(1-Y_A0)/sum(reboa==0)))
            SE_ <- sqrt( SE1^2 + SE2^2)
            rds[i,1:3]  <- c( (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96)
        }
    (rds)
}

label_list = list (
                  "AGE"                     = 'Age',
                   "GENDER"                  = 'Gender',
                   "RACE"                    = 'Race',
                   "TEACHINGSTATUS"          = 'Teaching Status',
                   "SBP"                     = 'Systolic Blood Pressure',
                   "PULSE"                   = 'Pulse',
                   "RR"                      = 'Respiratory Rate',
                   "GCS"                      = 'Glasgow Coma Score',
                   "Head_Neck"               = 'Head and Neck',
                   "splenic"                 = 'Spleen',
                   "kidney"                  = 'Kidney',
                   "liver"                   = 'Liver',
                   "femur_fracture"          = 'Femur Fracture',
                   "tibia_fracture"          = 'Tibia Fracture',
                   "pelvis"                  = 'Pelvis Fracture',
                   "lower_extremity_vascular" = 'Lower Extremity Vascular',
                   "iliac_vascular"         = 'Iliac vessels',
                   "any_vascular"           = 'Any vascular'
                                  )
################################
# 1.2.1 Load and filter 
################################
#outcomes <- c('deceased', 'dvt', 'pe', 'stroke', 'mi', 'compartment_syndrome', 'unplanned_return_to_or', 'major_amputation', 'aki', 'splenectomy', 'abdominal_bleeding_control', 'heart_repair')
outcomes <- c('deceased', 'dvt', 'pe', 'stroke', 'mi', 'compartment_syndrome', 'unplanned_return_to_or',  'aki')
covariates <- c('AGE', 'GENDER', 'RACE',  'TEACHINGSTATUS', 'SBP', 'PULSE', 'RR',    'GCSEYE', 'GCSVERB', 'GCSMOT', 'GCS_Q1','RRAQ', 'Head_Neck', 'Face', 'Thorax', 'Abdomen', 'Extremities', 'External', 'splenic', 'kidney', 'liver', 'femur_fracture', 'tibia_fracture',  'pelvis', 'lower_extremity_vascular', 'iliac_vascular', 'any_vascular', 'ISS')

if  (! file.exists('data/A_alldeceased.rds') ){
    source('preprocess.R')
}
#A <- readRDS('data/A_time.rds')
A <- readRDS('data/A_alldeceased.rds')


#sum(A$missing_baseline_variable)
#table( A$liver[A$reboa == 'REBOA'] , useNA="ifany")

#mean(A$deceased[A$reboa == 'REBOA'] == 'Complication')
A %>% filter(reboa == 'REBOA') %>% summarise_at(covariates,list(~sum(is.na(.)))) %>% t
A2 <- A %>% 
    mutate( reboa = reboa =='REBOA') %>% 
    mutate_if(is.character, as.factor) %>% 
    droplevels()
table( A$reboa, useNA="ifany")
table( A2$reboa, useNA="ifany")

# Remove patients who died in the ED from the calculation of AKI
A2$aki <- as.factor(ifelse(A2$ed_deceased == 'Complication', NA_character_, as.character(A2$aki)))


# Check the ISS scores
#A %>%  group_by (reboa, reboa_indicated) %>%  summarize( mean(ISS, na.rm = T), mean(ISS>25,na.rm=T))
A %>% group_by (reboa) %>%  summarize( mean(ISS, na.rm = T), mean(ISS>25,na.rm=T), sum(ISS>25,na.rm=T))

summary(A2$dvt)
table( A$dvt, useNA="ifany")


# Print the raw treatment effects
raw.rds <- print.rds( A2, outcomes)
as_tibble(raw.rds)

covariates <- covariates[covariates %in% colnames(A2)]
################################
# 1.2.2 Build a propensity score model 
################################

#library(broom)
psmodel <- glm(reboa ~ ., data=A2[,c('reboa', covariates)], family = binomial())
summary(psmodel)

psmodelout <- broom::tidy(psmodel) 
psmodelout  <- psmodelout %>% mutate( sig = case_when ( p.value < 0.001 ~ '***',
                                        p.value < 0.01 ~ '**',
                                        p.value < 0.1 ~ '*', 
                                        T ~ ' ')) %>% mutate_if( is.numeric, ~signif(.,2))

psmodelout$term2  <-  psmodelout$term
for (i in 1:length(label_list) ) {
        psmodelout$term2 <- gsub(names(label_list)[i], sprintf('%s ', label_list[i]), psmodelout$term2,fixed=T)
}
psmodelout %>%  write_csv('output/psmodel.csv')

A_df <- A2 %>% mutate(  pr = predict( psmodel, type="response"), w = ifelse( reboa, 1, pr/(1-pr)))

#################################
## 1.2.4 Assess balance 
#################################
labels(A_df) <- label_list 


A_df  <- A_df %>% mutate( GCS = GCSEYE + GCSVERB + GCSMOT)
covariates_toprint <- c('AGE', 'GENDER', 'RACE',  'TEACHINGSTATUS', 'SBP', 'PULSE', 'RR','GCS',  'Head_Neck', 'Face', 'Thorax', 'Abdomen', 'Extremities', 'External', 'splenic', 'kidney', 'liver', 'femur_fracture', 'tibia_fracture',  'pelvis', 'lower_extremity_vascular', 'iliac_vascular', 'any_vascular', 'ISS')
# Usng Arsenal
tblcontrol <- tableby.control( numeric.simplify = T, digits = 1,total = F,test = F, numeric.stats=c('meansd'),)
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
tunweighted <- tableby(reboa ~ ., data = A_df[, c('reboa',  covariates_toprint)], control = tblcontrol)
tweighted <- tableby(reboa ~ ., data = A_df[, c('reboa', 'w', covariates_toprint)], control = tblcontrol, weights=w)

print('Printing baseline characteristics')
summary(tunweighted, text=T) %>% as.data.frame %>% write_csv('output/tunweighted.csv')
summary(tweighted, text=T) %>% as.data.frame %>% write_csv('output/tweighted.csv')


A_df %>% group_by(reboa) %>% summarise_at(c('ISS', 'SBP'),  list(med=~median(.), q2=~quantile(., 0.25), q3=~quantile(., 0.75)))
sum(A_df$w[!A_df$reboa])

#summary(tt) %>% write2html('all_vars_by_reboa_weighted.htm')



# Using Cobalt for balance on all
#bal.tab.out <- bal.tab(A_df %>% select(-reboa, -w),treat = A_df$reboa, weights = A_df$w, un =T, binary = 'std')
#bal.tab.out




#bal.tab.out <- bal.tab(A_df %>% select(bone_procedures, splenectomy, abdominal_bleeding_control),treat = A_df$reboa, weights = A_df$w, un =T)

# Using Cobalt for balance on all
A_df %>% summarise_if(is.factor , ~length(levels(.))) 
bal.tab.out <- bal.tab(A_df %>% select(covariates),treat = A_df$reboa, weights = A_df$w, un =T, binary = 'std')

bal.out <- bal.tab.out$Balance
bal.out$var <- rownames(bal.out)
toplot <- bal.out %>% pivot_longer( cols = c('Diff.Un', 'Diff.Adj'))  %>% mutate( name = as.factor(name), value = (value))%>%
        mutate(lab = case_when( abs(value) > 0.5  ~ as.character(var),
                               T ~ NA_character_) )
levels(toplot$name) <- c('Adjusted', 'Unadjusted') 
toplot$name  <- factor(toplot$name,levels = rev(levels(toplot$name)))
levels(toplot$name)

gg <- ggplot(toplot, aes(x = name, y = value, group = var, label = lab)) + geom_point() + geom_line()  +
        geom_hline(aes(yintercept = 0.1), size = 0.25, linetype = "dashed") +
        geom_hline(aes(yintercept = -0.1), size = 0.25, linetype = "dashed") +
        ylab('Standardized mean difference')+
     geom_label_repel(size=2, seed = 3) + cowplot::theme_cowplot()+
        theme(axis.title.x=element_blank())  
ggsave(plot = gg, filename = 'figures/balance.pdf')

# Balance on just the covariates we balanced
#bal.tab.out <- bal.tab(A_df %>% select(covariates),treat = A_df$reboa, weights = A_df$w, un =T, binary = 'std')
#bal.tab.out




################################
# 1.2.5 All outcomes
################################

weighted.rds <- as.data.frame(matrix(NA, ncol=3,nrow=length(outcomes)))
rownames(weighted.rds) <- c(outcomes)
colnames(weighted.rds) <- c('estimate','low_ci', 'high_ci')
weighted.rds$y_axis <- 1:nrow(weighted.rds)
weighted.rds$outcome <- rownames(weighted.rds)
for (i in 1:length(outcomes)) {
    # In the raw data
    outcome  <- outcomes[i]
    A_df_2 <- A_df %>% mutate(Y=get(outcome)=='Complication')  %>% drop_na(Y)
    design <- svydesign(ids=~1, weights = ~w, data = A_df_2)
    Y_n_A1 <- with(A_df_2, sum(Y[ reboa == 1]*w[reboa==1]))
    Y_n_A0 <- with(A_df_2, sum(Y[ reboa == 0]*w[reboa==0]))
    Y_A1 <- with(A_df_2, sum(Y[ reboa == 1]*w[reboa==1])/sum(w[reboa==1]))
    Y_A0 <- with(A_df_2, sum(Y[ reboa == 0]*w[reboa==0])/sum(w[reboa==0]))
    SE1 <- as.data.frame(svymean(~Y, design = subset(design, reboa==1)))[1,2]
    SE2 <- as.data.frame(svymean(~Y, design = subset(design, reboa==0)))[1,2]
    SE_ <- sqrt( SE1^2 + SE2^2)
    weighted.rds[i,1:3]  <- c( (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96)
    #cat(sprintf('%s, RD: %.3f (95%% CI %.3f, %.3f)\n', outcome, (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96))
    cat(sprintf('%s, %.0f (%.2f), %.0f (%.2f), %.3f (%.3f to %.3f)\n', outcome, 
                (Y_n_A0), (Y_A0), 
                (Y_n_A1), (Y_A1), 
                (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96))
    #cat(sprintf('%s, %.2f (%.2f-%.2f), %.2f (%.2f-%.2f), %.2f (%.2f-%.2f)\n', outcome, 
    #            (Y_A0), (Y_A0)- SE1*1.96, (Y_A0) + SE_*1.96,
    #            (Y_A1), (Y_A1)- SE1*1.96, (Y_A1) + SE_*1.96,
    #            (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96))
}
print(as_tibble(weighted.rds))



for (i in 1:length(outcomes)) {
    # In the raw data
    outcome  <- outcomes[i]
    A_df_2 <- A_df %>% mutate(Y=get(outcome)=='Complication')  %>% drop_na(Y)
    Y_n_A1 <- with(A_df_2, sum(Y[ reboa == 1]))
    Y_n_A0 <- with(A_df_2, sum(Y[ reboa == 0]))
    Y_A1 <- with(A_df_2, sum(Y[ reboa == 1]/sum(reboa==1)))
    Y_A0 <- with(A_df_2, sum(Y[ reboa == 0]/sum(reboa==0)))
    SE1 <- sqrt(Y_A1*(1-Y_A1)/sum(A_df_2$reboa==1))
    SE2 <- sqrt(Y_A0*(1-Y_A0)/sum(A_df_2$reboa==0))
    SE_ <- sqrt( SE1^2 + SE2^2)
    cat(sprintf('%s, %.0f (%.2f), %.0f (%.2f), %.3f (%.3f to %.3f)\n', outcome, 
                (Y_n_A0), (Y_A0), 
                (Y_n_A1), (Y_A1), 
                (Y_A1 - Y_A0), (Y_A1 - Y_A0)- SE_*1.96, (Y_A1 - Y_A0) + SE_*1.96))
}






################################
#  Mortality treatment effect and sensitiovity analysis
################################


# First, get the actual treatment effect for Mortality
A_df_2 <- A_df %>% mutate(Y=deceased=='Complication')  %>% drop_na(Y)
design <- svydesign(ids=~1, weights = ~w, data = A_df_2)
Y_A1 <- with(A_df_2, sum(Y[ reboa == 1]*w[reboa==1])/sum(w[reboa==1])) # Weighted risk of death among REBOA patients 
Y_A0 <- with(A_df_2, sum(Y[ reboa == 0]*w[reboa==0])/sum(w[reboa==0])) # Weighted risk of death among non-REBOA patients

RR <- Y_A1/Y_A0
RD <- Y_A1-Y_A0
SE1 <- as.data.frame(svymean(~Y, design = subset(design, reboa==1)))[1,2]
SE2 <- as.data.frame(svymean(~Y, design = subset(design, reboa==0)))[1,2]
SE_ <- sqrt( SE1^2 + SE2^2)
lowerRD  <- (Y_A1 - Y_A0)- SE_*1.96 # Lower confidence interval for the Risk Difference

# Below we specify the confounders that will be plotted. Each entry of
# confounder.labels corresponds to an entry of confounders.vectors.
A2$Y <- A2$deceased == 'Complication'
confounders.labels <-  list(  'Any Pelvic injury',
                               'Pelvic ring fx, Complete disruption of posterior arch',
                               # 'Pelvic ring fx, Incomplete disruption of posterior arch',
                               'Pelvic ring fx, NFS',
                               # 'Pelvic ring fx, posterior arch intact',
                               'Liver injury',
                               'Kidney injury',
                               # 'Spleen injury',
                               # 'Major vascular injury',
                               'SBP < 90',
                               'SBP < 70',
                               'ISS > 25',
                               # 'ISS > 9',
                               'ISS > 15'
                            )
# Each entry should be a Boolean-valued vector for whether or not the patient has the "confounder"
confounders.vectors <- list( A2$pelvis != 'No injury',
                            A2$pelvis == 'Pelvic ring fracture, complete disruption of posterior arch',
                            # A2$pelvis == 'Pelvic ring fracture, incomplete disruption of posterior arch',
                            A2$pelvis == 'Pelvic ring fracture, not further specified',
                            # A2$pelvis == 'Pelvic ring fracture, posterior arch intact',
                            A2$liver != 'No injury',
                            A2$kidney != 'No injury',
                            # A2$splenic != 'No injury',
                            # A2$any_vascular == 'Laceration, perforation, puncture major >20% volume loss',
                            A2$SBP < 90,
                            A2$SBP < 70,
                            A2$ISS >25,
                            # A2$ISS >9,
                            A2$ISS >15
)
# Each row is a confounder to be plotted
toplot.confounders <- as.data.frame(matrix(ncol=3, nrow=0))
colnames(toplot.confounders)  <- c('x', 'y', 'll')
for (i in 1:length(confounders.labels)) {
        A2$U  <- confounders.vectors[[i]]
        (delta <- with (A2,  mean(U[reboa]) - mean(U[!reboa])))
        (gamma <- with (A2,  mean(Y[U]) - mean(Y[!U])))
        toplot.confounders[i,]  <- list( x = gamma, y = delta, ll = confounders.labels[[i]])
}
toplot.confounders
library(ggrepel)
EU.sample <- seq(RD, 1.5, by = 0.001)
toplot <- data.frame(x=EU.sample) 


# Recall the formula for a confounder that can explain the RD: 
# RD = gamma* delta. So, we for every x we can easily compute the y as RD/x. 
xs <- seq(RD, 1, by = 0.01)
dotted <- data.frame( x =xs, y = RD/xs) 
g <- ggplot(toplot) + xlim(0,1) + ylim(0,1)+ 
            geom_label_repel(data = toplot.confounders, aes(x,y, label = ll), min.segment.length = 0 , size=3, force=5, max.iter = 4000, seed=5) + 
            geom_line(data=dotted, mapping=aes(x=x,y=y), linetype = 2)+
            cowplot::theme_cowplot() + labs( x = 'Mortality risk difference \nby confounder', y = 'Prevalence difference of confounder \nby REBOA')+ 
            geom_point( data=toplot.confounders, aes(x=x, y = y)) +
            annotate("text",  x=0.62, y = 0.75, label = "To explain the increased mortality risk,\n an unmeasured confounder would \nneed to be in this region.", vjust=1, hjust=0.5)
ggsave(g, file = 'figures/sensitivity.pdf', width=6, height=6) 


