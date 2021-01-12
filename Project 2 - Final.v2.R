setwd("/Py_Files/Hist_Baseball_Data")
## Read in data
detach(data)
data<-read.table("hist_baseball_data.csv", header=TRUE, sep=",")
attach(data)
set.seed(110)
#############################################
## Convert columns to factors, if necessary
data$league <- factor(data$league)
data$season <- factor(data$season)
data$decade <- factor(data$decade)

#############################################
## Create potential response variables and weights
data$wins <- round(data$win_loss_perc_pit * data$G_off)
data$wins_pct <-  data$wins / data$G_off
data$wins <- round(data$wins_pct * 162)

#############################################
## Import packages
library(leaps)
library(MASS)
library(car)

#############################################
## Remove unnecessary regressors
data$X <- NULL
data$W_pit <- NULL
data$L_pit <- NULL
data$season <- NULL
data$team_ID <- NULL
data$GS_pit <- NULL
data$GF_pit <- NULL
data$G_pit <- NULL
data$IP_pit <- NULL
data$SV_pit <- NULL
data$win_loss_perc_pit <- NULL

#############################################
## Convert missing values to mean of the entire column
data[data==""] <- NA
data <- data[, colSums(is.na(data)) == 0]
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#############################################
## Remove duplicated columns
values <- c()
for (i in 1:ncol(data)){
  if(sub('.*(?=...$)', '', names(data[i]), perl=T) == "def") {
    values <- append(values, (i), after = length(values))
  }
}
data <- data[,-values]
attach(data)

#############################################
## Identify columns that need to be adjusted to per game from season totals
scalars <- c('X2B_off','X3B_off','AB_off','BB_off','H_off','HBP_off','HR_off','LOB_off','PA_off','R_off',
             'RBI_off','SB_off','SH_off','SO_off','TB_off','BB_pit','BK_pit','CG_pit',
             'ER_pit','H_pit','HBP_pit','HR_pit','LOB_pit','R_pit','SHO_cg_pit','SHO_team_pit',
             'SO_pit','WP_pit','batters_faced_pit')

## Identify columns that DO NOT need to be adjusted to per game from season totals
nonscalars <- c('G_off','age_bat_off','batting_avg_off','onbase_perc_off','onbase_plus_slugging_off',
                'onbase_plus_slugging_plus_off','runs_per_game_off','slugging_perc_off','G_off_scaled',
                'age_pitch_pit','bases_on_balls_per_nine_pit','earned_run_avg_pit','earned_run_avg_plus_pit',
                'fip_pit','hits_per_nine_pit','home_runs_per_nine_pit','runs_allowed_per_game_pit',
                'strikeouts_per_base_on_balls_pit','strikeouts_per_nine_pit','whip_pit',
                'league','batters_used_off','decade','pitchers_used_pit','wins_pct','wins')

#############################################
## Scale colums previously identified columns that need to be adjusted to per game
for (i in 1:ncol(data)){
  if(names(data[i]) %in% scalars){
    data[i] <- data[i] / data$G_off
  }
}

#############################################
## Sample the data to create train and test sets
sample<-sample.int(nrow(data), floor(.99*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]

##########################################################################################
## Select type of stepwise and regression type
step_type <- "forward" #"backward" "forward" "step"
reg_type <- "lm"        #"lm" "glm"

#############################################
## Create regnull and regfull based on reg_type
if (reg_type == "glm") {
  regnull <- glm(train$wins_pct~1, family="binomial", weights=train$G_off,
                 data = train[-which( colnames(train)=="wins")])
  regfull <- glm(train$wins_pct~., family="binomial", weights=train$G_off,
                 data = train[-which( colnames(train)=="wins")])
} else {
  regnull <- lm(train$wins~1,data = train[-c(which(colnames(train) == "wins_pct"),
                                             which(colnames(train) == "G_off"))])
  regfull <- lm(train$wins~.,data = train[-c(which(colnames(train) == "wins_pct"),
                                             which(colnames(train) == "G_off"))])
}

#############################################
## Use stepwise method to fit model based on step_type
if (step_type == "backward") { 
  result <- step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
} else if (step_type == "forward") {
  result <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
} else {
  result <- step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
}

#############################################
## Remove high vif regressors
#vif(result)
#formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+','decade'),'',
#                                                           as.character(formula(result))[3]))
#formula <- as.formula(formula)
#result <- update(result, formula=formula)
while (max(vif(result)) > 10) {
  remove.var = variable.names(result)[unname((which(vif(result) == max(vif(result)))+1))]
  formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+',remove.var),'',
                                                             as.character(formula(result))[3]))
  formula <- as.formula(formula)
  result <- update(result, formula=formula)
}

#############################################
## Reduce number of parameters by iteratively re-fitting and dropping high p value > 0.05
coef <- as.data.frame(summary(result)$coefficients)
coef <- coef[-1,][which(coef[-1,][,4] == max(coef[-1,][,4])),]
remove.var = as.symbol(rownames(coef[which(coef[,4] == max(coef[,4])),]))
remove.var = as.symbol(gsub('NL','',remove.var))
remove.var = as.symbol(gsub('AL','',remove.var))
i = 1
while (coef[which(coef[,4] == max(coef[,4])),][,4] > 0.05){
  if (i == 1){
    formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+',remove.var),'',
                                                               as.character(formula(result))[3]))
    formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+',remove.var),'',
                                                               as.character(formula(result))[3]))
    
    formula <- gsub(remove.var,'',formula)
    formula <- as.formula(formula)
    reduced <- update(result, formula=formula)
  }
  else{
    formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+',remove.var),'',
                                                               as.character(formula(reduced))[3]))
    formula <- paste(as.character(formula(result))[2],'~',gsub(paste(' \\+',remove.var),'',
                                                               as.character(formula(reduced))[3]))
    formula <- gsub(remove.var,'',formula)
    formula <- as.formula(formula)
    reduced <- update(reduced, formula=formula)
  }
  coef <- as.data.frame(summary(reduced)$coefficients)
  coef <- coef[-1,][which(coef[-1,][,4] == max(coef[-1,][,4])),]
  remove.var = as.symbol(rownames(coef[which(coef[,4] == max(coef[,4])),]))
  remove.var = as.symbol(gsub('NL','',remove.var))
  remove.var = as.symbol(gsub('AL','',remove.var))
  if (step_type == "forward" & reg_type == "lm") { 
    fwd_lm_full <- result
    fwd_lm_reduced <- reduced
  } else if (step_type == "backward" & reg_type == "lm") {
    bkwrd_lm_full <- result
    bkwrd_lm_reduced <- reduced
  } else if  (step_type == "forward" & reg_type == "glm") {
    fwd_glm_full <- result
    fwd_glm_reduced <- reduced
  } else if  (step_type == "backward" & reg_type == "glm") {
    bkwrd_glm_full <- result
    bkwrd_glm_reduced <- reduced
  }
  i = i + 1
}

#############################################
  
#Plot residuals
plot(reduced$fitted.values,reduced$residuals)
abline(h=0)

# Test for auto-correlation of residuals
acf(reduced$residuals)

#Test for normality of residuals
qqnorm(reduced$residuals)
qqline(reduced$residuals,col='red')


## Partial f test and equivalent

summary(fwd_lm_full)
sqrt(sum((predict(fwd_lm_full,test) - test$wins)**2) / nrow(test))
summary(fwd_lm_reduced)
sqrt(sum((predict(fwd_lm_reduced,test) - test$wins)**2) / nrow(test))
anova(fwd_lm_reduced,fwd_lm_full)

summary(bkwrd_lm_full)
sqrt(sum((predict(bkwrd_lm_full,test) - test$wins)**2) / nrow(test))
summary(bkwrd_lm_reduced)
sqrt(sum((predict(bkwrd_lm_reduced,test) - test$wins)**2) / nrow(test))
anova(bkwrd_lm_reduced,bkwrd_lm_full)

summary(fwd_glm_full)
sqrt(sum((predict(fwd_glm_full,test,type="response") - test$wins_pct)**2) / nrow(test)) * 162
summary(fwd_glm_reduced)
sqrt(sum((predict(fwd_glm_reduced,test,type="response") - test$wins_pct)**2) / nrow(test)) * 162
1-pchisq(fwd_glm_reduced$deviance-fwd_glm_full$deviance,1)

summary(bkwrd_glm_full)
sqrt(sum((predict(bkwrd_glm_full,test,type="response") - test$wins_pct)**2) / nrow(test)) * 162
summary(bkwrd_glm_reduced)
sqrt(sum((predict(bkwrd_glm_reduced,test,type="response") - test$wins_pct)**2) / nrow(test)) * 162
1-pchisq(bkwrd_glm_reduced$deviance-bkwrd_glm_full$deviance,1)

#
data[1310,]

sum(data[,home_runs_per_nine_pit])/length(data[,home_runs_per_nine_pit])
data[,home_runs_per_nine_pit]
