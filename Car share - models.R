load("Prep_workspace.RData")
library(arm)
library(ggplot2)
library(knitr)

table(surveydata$hhincome_bins)

#relevel income_bins_rl, but as new variable so as not have it affect order shown on graphs. 
surveydata$hhincome_bins_rl = relevel(surveydata$hhincome_bins, "75 - 100k")

glm(car_share ~ hhincome_bins_rl, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined)
glm(car_share ~ I(hhincome_num/95), family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined)
#use hhincome_num instead of hhincome_bins. 

#build logistic regression models:
Model1 <- glm (car_share ~ I((hhincome_num-95)/10), family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) #base model
Model2 <- glm (car_share ~ I((hhincome_num-95)/10) + vehicle_count, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) 
Model3 <- glm (car_share ~ I((hhincome_num-95)/10) + vehicle_count + urban_village_type, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) 
Model4 <- glm (car_share ~ I((hhincome_num-95)/10) + urban_village_type, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) #releveled to median income
Model5 <- glm (car_share ~ I((hhincome_num-95)/10) + vehicle_count:I((hhincome_num-95)/10), family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) #recentered at median income = 95
Model6 <- glm (car_share ~ I((hhincome_num-95)/10) + I((hhincome_num-95)/10):urban_village_type, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined) #comparison is no urban village
Model7 <- glm (car_share ~ I((hhincome_num-95)/10) + vehicle_count:I((hhincome_num-95)/10) + urban_village_type, family=binomial(link="logit"), data = surveydata, weights = hh_wt_combined)


model_table1 = data.frame(model = paste0("Model ", 1:7),
       `hhincome_num` = "x",
       `vehicle_count` = c("", "x", "x", "", "", "", ""),
       `urban_village_type` = c("", "", "x", "x", "", "", "x"))

model_table2 = data.frame(model = paste0("Model ", 1:7),
                          `hhincome_num:urban_village_type` = c("", "", "", "", "", "x", ""),
                          `vehicle_count:hh_income` = c("", "", "", "", "x", "", "x"))



Model1BIC = BIC(Model1)
Model2BIC = BIC(Model2)
Model3BIC = BIC(Model3)
Model4BIC = BIC(Model4)
Model5BIC = BIC(Model5)
Model6BIC = BIC(Model6)
Model7BIC = BIC(Model7)

model_selection = data.frame(model_name=c("Model1","Model2","Model3", "Model4", "Model5", "Model6", "Model7"), #names here have to match model names above
                             BIC= c(Model1BIC, Model2BIC, Model3BIC, Model4BIC, Model5BIC, Model6BIC, Model7BIC))



best_model_row <- model_selection[which.min(model_selection$BIC),] 
#will need this for rmd file

best_model_name = best_model_row[,1] #just the name of the best model. but it's a character.
best_model_name
best_model = get(best_model_name) #get() searches by name for the variable stored in best_model. 
best_model


#beta means the odds (not p) for y=1 (having car share) are multiplied by exp(beta) per unit increase in x.
#It's a multiplicative effect.


#prepare to interpret model
toprobs<-function(x,digs=0) round(invlogit(x)*100,digs) # for interpreting intercept as a probability. not for coefficients.
lmat <- data.frame(summary(best_model)$coef) %>%
  mutate( loCI = Estimate - 1.96*Std..Error, upCI = Estimate + 1.96*Std..Error)

lmat[,-(2:4)] #conf int for B for each term. raise to exp(B) to get log odds of each term
toprobs(lmat[1,1]) #for intercept. HH with mean vehicle count and income has a 10% chance of being a car share member. 
exp(lmat$loCI[2])

lmat
toprobs(lmat[1,1])


table(surveydata$vehicle_count)

ggplot(surveydata, aes(x = vehicle_count, fill = factor(car_share))) + geom_bar(position = "fill") +
  labs(x='Vehicle Count', y='Proportion with Car Share') + guides(fill='none') +
  scale_fill_manual(values = c('white','black')) +
  facet_grid(.~hhincome_bins, labeller = labeller(hhincome_bins = label_wrap_gen(width = 8))) + scale_x_continuous(breaks=c(0, 2, 4, 6), limits = c(0,6)) + theme_bw() +
  theme(panel.grid.major = element_blank())

exp(.33) #interpretation: people making $100-150k have 1.39 x higher odds of being car share members, compared to median income. don't multiply x 100.
#anything above zero means higher odds.


save.image('Model_workspace.RData')
