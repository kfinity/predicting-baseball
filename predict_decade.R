# run after "Project 2.R" script imports and does cleanup

# testing multinomial logistic regression - predicting the decade by some team stats
dectyp <-multinom(decade ~ X2B_off+X3B_off+HR_off+HBP_off+SB_off+SH_off+age_bat_off+BK_pit+BB_pit+WP_pit,data=data)
summary(dectyp)
# eyeballing, looks like BB_pit, age_bat_off are not very good (coefficient / std err), remove them...
dectyp <-multinom(decade ~ X2B_off+X3B_off+HR_off+HBP_off+SB_off+SH_off+BK_pit+WP_pit,data=data)
summary(dectyp)
# yikes, now std errors are much higher! still seem ok though
# test/train
set.seed(111)
sample<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F)
train<-data[sample, ]
test<-data[-sample, ]
# run model on test/train sets
dectyp <-multinom(decade ~ X2B_off+X3B_off+HR_off+HBP_off+SB_off+SH_off+BK_pit+WP_pit,data=train)
summary(dectyp)
predicted_scores <- predict (dectyp, test, "probs")
predicted_class <- predict (dectyp, test)
table(predicted_class, test$decade) # heck yeah, looks pretty good
mean(as.character(predicted_class) != as.character(test$decade)) # 42% misclassification rate
mean((as.numeric(as.character(predicted_class)) >= as.numeric(as.character(test$decade))-11) & (as.numeric(as.character(predicted_class)) <= as.numeric(as.character(test$decade))+11)) 
# buuut 90.5% are within a decade of the correct one. pretty neat.


# plots of the stats against decade, to see trends
plot(decade,data$X2B_off) # up and down (doubles)
plot(decade,data$X3B_off) # downward trend (triples)
plot(decade,data$HR_off) # mostly up (home runs)
plot(decade,data$HBP_off) # mostly up, recently (hit by pitch)
plot(decade,data$SB_off) # popular in the 70s-90s (stolen bases)
plot(decade,data$SH_off) # fell way off (sacrifice hits)
plot(decade,data$age_bat_off) # steady, dipped in the 60s-70s
plot(decade,data$BK_pit) # very popular in 80s (balks)
plot(decade,data$BB_pit) # steady, a lot of low outliers recently and in 80s (bases on balls/walks)
plot(decade,data$WP_pit) # up a bit in 60s, steady (wild pitches)
