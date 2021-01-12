bb<-read.table("hist_baseball_data.csv", header=TRUE, sep=",")
attach(bb)
results <- lm(win_loss_perc_pit~batting_avg_off+runs_per_game_off+HBP_pit+age_pitch_pit+season)

summary(results)

# *** R_off, R_def - when included, these ones dominate
# *** 
# **
# *
# .

vcov(results)

plot(win_loss_perc_pit, AB_off) # 
plot(win_loss_perc_pit, G_off) #  games played
plot(win_loss_perc_pit, HR_off) # *
plot(win_loss_perc_pit, IBB_off) # *
plot(win_loss_perc_pit, PA_off/G_off) # a lot of outliers with low number of games played distort this
plot(win_loss_perc_pit, R_off) # **
plot(win_loss_perc_pit, RBI_off) # **
plot(win_loss_perc_pit, batting_avg_off) # *
plot(win_loss_perc_pit, runs_per_game_off) # *
plot(win_loss_perc_pit, SO_off/G_off) # *
plot(win_loss_perc_pit, team_ID) # very striated data - categorical - the same team has a band of win loss percentages

plot(win_loss_perc_pit, L_pit) # almost perfect negative, but see outliers
plot(win_loss_perc_pit, W_pit) # almost perfect negative, but see outliers
plot(win_loss_perc_pit, W_pit/G_pit) # basically perfect

plot(win_loss_perc_pit, age_bat_off) # 
plot(win_loss_perc_pit, onbase_perc_off) # *
plot(win_loss_perc_pit, runs_per_game_off) # *
plot(win_loss_perc_pit, slugging_perc_off) # *
plot(win_loss_perc_pit, bases_on_balls_per_nine_pit) # 
plot(win_loss_perc_pit, hits_per_nine_pit) # * -
plot(win_loss_perc_pit, home_runs_per_nine_pit) #
plot(win_loss_perc_pit, runs_allowed_per_game_pit) #
plot(HR_off/G_off, SO_off/G_off) # *

plot(win_loss_perc_pit, onbase_plus_slugging_plus_off) # **
plot(win_loss_perc_pit, whip_pit) # ** -

plot(win_loss_perc_pit, age_pitch_pit) 
plot(win_loss_perc_pit, earned_run_avg_pit) # * -
plot(win_loss_perc_pit, earned_run_avg_plus_pit) # ***
plot(win_loss_perc_pit, strikeouts_per_base_on_balls_pit) 

plot(win_loss_perc_pit, X3B_pit)

plot(HR_off/G_off, SO_off/G_off)

plot(season, HR_off)
plot(season, home_runs_per_nine_pit)
