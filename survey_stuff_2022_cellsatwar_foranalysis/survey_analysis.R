###CELLS AT WAR 2022 SURVEY DATA ANALYSIS - BEGAN FEB 23

###IMPORTING THE DATA
morning_df_csv <- read.csv("Depersonalized_survey_data_morning(2).csv", header = TRUE)
combined_df_csv <- read.csv("combined_data_morning_afternoon_raw_CSV.csv", header = TRUE)

###CREATING SOME GRAPHS
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggpubr) #For ggarrange function

### FUNCTION FOR PUMPING OUT BARPLOTS
plot_survey_fun <- function(df, bar_lables, xtitle, ytitle, chart_title) {
  p <- ggplot(df, aes(x = df[,1], y = df[,2])) + 
    geom_bar(show.legend = FALSE, width = 0.7, fill = "#000075", stat = "identity") +
    geom_text(aes(label = df[,2]), vjust = -0.5, colour = "black", nudge_x = -0.14) +
    geom_text(aes(label = paste0("(",round(df[,2]/sum(df[,2])*100),")%")),vjust = -0.5, nudge_x = 0.14 ) +
    geom_text(x = -Inf, y = Inf, label = paste0("n = ", sum(df[,2])), 
              hjust = -1, vjust = 3, size = 4, color = "black") +
    scale_x_discrete(labels = bar_lables) +
    xlab(xtitle) +
    ylab(ytitle) +
    ggtitle(chart_title)
  
  p + theme_clean(base_size = 11.5)
}

#Q1
########################################################################
###ON AVERAGE, HOW MUCH TIME DO YOU SPEND PLAYING VIDEO GAMES EACH WEEK#
########################################################################

time_spent_vg <- as.data.frame(table(combined_df_csv$On.average..how.much.time.do.you.spend.playing.video.games.each.week....Select.one.option.))
time_spent_vg$Var1 <- factor(time_spent_vg$Var1, levels = c("", "I don't play video games", "Very little (less than 3 hours a week)", "Some (between 3-10 hours per week)", "Quite a bit (between 10-20 hours per week)", "Very much (greater than 20 hours per week)"))
time_spent_vg_labels <- c("No Response", "I don't play video games", "Under 3 hours a week", "3-10 hours/week", "Between 10-20 hours/week", "More than 20 hours/week")
time_spent_vg_xtitle <- "Responses"
time_spent_vg_ytitle <- "Frequency"
time_spent_vg_title <- "Time Spent Playing Video Games"

##SAVING THE PLOT TO DIRECTORY
tiff("figures/time_spent_playing_videogames.tiff", units="in", width=12, height=6, res=300)
plot_survey_fun(time_spent_vg, time_spent_vg_labels, time_spent_vg_xtitle, time_spent_vg_ytitle, time_spent_vg_title)
dev.off()

#Q2
#############################################################################
###WHAT TYPES OF COURSES HAVE YOU TAKEN AT MCMASTER THIS CURRENT SCHOOL YEAR#
#############################################################################

course_types_df <- as.data.frame(table(combined_df_csv$What.types.of.courses.have.you.taken.at.McMaster.this.current.school.year...Select.one.option.))
course_types_labels <- c("No Response", "Balanced mix of course types", "Mostly hybrid or blended courses", "Mostly in-person courses", "Mostly remote courses")
course_types_xtitle <- "Responses"
course_types_ytitle <- "Frequency"
course_types_title <- "Types of Courses Taken"

###SAVING PLOT TO DIRECTORY
tiff("figures/types_of_courses_taken.tiff", units="in", width=12, height=5, res=300)
plot_survey_fun(course_types_df, course_types_labels, course_types_xtitle, course_types_ytitle, course_types_title)
dev.off()

#Q3
#######################################################################################
###DURING THE CURRENT SCHOOL YEAR, HOW OFTEN WOULD YOU SAY YOU HAVE DONE THE FOLLOWING#
#######################################################################################

###COMBINED IDEAS FROM DIFFERENT COURSES WHEN COMPLETING ASSIGNMENTS
combined_ideas_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.often.would.you.say.you.have.done.the.following...Combined.ideas.from.different.courses.when.completing.assignments.))
combined_ideas_df$Var1 <- factor(combined_ideas_df$Var1, levels = c("", "Never", "Sometimes", "Often", "Very Often"))
combined_ideas_labels <- c("No Response", "Never", "Sometimes", "Often", "Very Often")
combined_ideas_xtitle <- "Responses"
combined_ideas_ytitle <- "Frequency"
combined_ideas_title <- "Combined Ideas from Different Courses When Completing Assignments"

a <- plot_survey_fun(combined_ideas_df, combined_ideas_labels, combined_ideas_xtitle, combined_ideas_ytitle, combined_ideas_title)

###CONNECTED YOUR LEARNING TO SOCIETAL PROBLEMS OR HEALTH-RELATED ISSUES
connected_learning_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.often.would.you.say.you.have.done.the.following...Connected.your.learning.to.societal.problems.or.health.related.issues.))
connected_learning_df$Var1 <- factor(connected_learning_df$Var1, levels = c("", "Never", "Sometimes", "Often", "Very Often"))
connected_learning_labels <- c("No Response", "Never", "Sometimes", "Often", "Very Often")
connected_learning_xtitle <- "Responses"
connected_learning_ytitle <- "Frequency"
connected_learning_title <- "Connected Learning to Societal/Health-Related Issues"

b <- plot_survey_fun(connected_learning_df, connected_learning_labels, connected_learning_xtitle, connected_learning_ytitle, connected_learning_title)

###LEARNED SOMETHING THAT CHANGED THE WAY YOU UNDERSTAND AN ISSUE OR CONCEPT
learned_something_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.often.would.you.say.you.have.done.the.following...Learned.something.that.changed.the.way.you.understand.an.issue.or.concept.))
learned_something_df$Var1 <- factor(learned_something_df$Var1, levels = c("", "Never", "Sometimes", "Often", "Very Often"))
learned_something_labels <- c("No Response", "Never", "Sometimes", "Often", "Very Often")
learned_something_xtitle <- "Responses"
learned_something_ytitle <- "Frequency"
learned_something_title <- "Learned Something that Changed Understanding of An Issue/Concept"

c <- plot_survey_fun(learned_something_df, learned_something_labels, learned_something_xtitle, learned_something_ytitle, learned_something_title)

###CONNECTED IDEAS FROM YOUR COURSES TO YOUR PRIOR EXPERIENCES AND KNOWLEDGE
connected_ideas_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.often.would.you.say.you.have.done.the.following...Connected.ideas.from.your.courses.to.your.prior.experiences.and.knowledge.))
connected_ideas_df$Var1 <- factor(connected_ideas_df$Var1, levels = c("", "Sometimes", "Often", "Very Often"))
connected_ideas_labels <- c("No Response","Sometimes", "Often", "Very Often")
connected_ideas_xtitle <- "Responses"
connected_ideas_ytitle <- "Frequency"
connected_ideas_title <- "Connected Ideas from Courses to Prior Experiences and Knowledge"

d <- plot_survey_fun(connected_ideas_df, connected_ideas_labels, connected_ideas_xtitle, connected_ideas_ytitle, connected_ideas_title)
d
###SAVING PLOT TO DIRECTORY
tiff("figures/how_often_haveyou_done_thefollowing.tiff", units="in", width=16, height=9, res=300)
ggarrange(a,b,c,d, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
dev.off()

#Q4
#######################################################################################################
###DURING THE CURRENT SCHOOL YEAR, HOW MUCH WOULD YOU SAY YOUR COURSEWORK HAS EMPHASIZED THE FOLLOWING#
#######################################################################################################
###MEMORIZING COURSE MATERIAL
memorizing_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.much.would.you.say.your.coursework.has.emphasized.the.following...Memorizing.course.material.))
memorizing_df$Var1 <- factor(memorizing_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
memorizing_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
memorizing_xtitle <- "Responses"
memorizing_ytitle <- "Frequency"
memorizing_title <- "Memorizing Course Material"

a_2 <- plot_survey_fun(memorizing_df, memorizing_labels, memorizing_xtitle, memorizing_ytitle, memorizing_title)

###Applying facts, theories, or methods to practical problems or new situations
applying_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.much.would.you.say.your.coursework.has.emphasized.the.following...Applying.facts..theories..or.methods.to.practical.problems.or.new.situations.))
applying_df$Var1 <- factor(applying_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
applying_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
applying_xtitle <- "Responses"
applying_ytitle <- "Frequency"
applying_title <- "Applying Facts, Theories, Methods to Problems/New Situations"

b_2 <- plot_survey_fun(applying_df, applying_labels, applying_xtitle, applying_ytitle, applying_title)

###Analyzing an idea, experience, or line of reasoning in depth by examining its parts
analyzing_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.much.would.you.say.your.coursework.has.emphasized.the.following...Analyzing.an.idea..experience..or.line.of.reasoning.in.depth.by.examining.its.parts.))
analyzing_df$Var1 <- factor(analyzing_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
analyzing_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
analyzing_xtitle <- "Responses"
analyzing_ytitle <- "Frequency"
analyzing_title <- "Analyzing an Idea, Experience, or Line of Reasoning in Depth"

c_2 <- plot_survey_fun(analyzing_df, analyzing_labels, analyzing_xtitle, analyzing_ytitle, analyzing_title)

###Forming a new idea or understanding from various pieces of information
forming_df <- as.data.frame(table(combined_df_csv$During.the.current.school.year..how.much.would.you.say.your.coursework.has.emphasized.the.following...Forming.a.new.idea.or.understanding.from.various.pieces.of.information.))
forming_df$Var1 <- factor(forming_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
forming_labels <- c("No Response","Very Little", "Some", "Quite a Bit", "Very Much")
forming_xtitle <- "Responses"
forming_ytitle <- "Frequency"
forming_title <- "Forming a New Idea or Understanding from Various Pieces of Information"

d_2 <- plot_survey_fun(forming_df, forming_labels, forming_xtitle, forming_ytitle, forming_title)

tiff("figures/howmuch_coursework_emphasized_thefollowing.tiff", units="in", width = 16, height=9, res=300)
ggarrange(a_2,b_2,c_2,d_2, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
dev.off()

#Q5
########################################################################################################################################################
###IF GAME BASED LEARNING APPROACHES WERE INCORPORATED IN ONE OF YOUR LIFE SCIENCES COURSES, HOW DO YOU THINK THIS MODE OF LEARNING HELPS THE FOLLOWING#
########################################################################################################################################################
###COMBINED IDEAS FROM DIFFERENT COURSES WHEN COMPLETING ASSIGNMENTS
combined_ideas2_df <- as.data.frame(table(combined_df_csv$If.game.based.learning.approaches.were.incorporated.into.one.of.your.science.courses..how.do.you.think.that.this.mode.of.learning.can.help.with.the.following...Combine.ideas.from.different.courses.when.completing.assignments.))
combined_ideas2_df$Var1 <- factor(combined_ideas2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
combined_ideas2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
combined_ideas2_xtitle <- "Responses"
combined_ideas2_ytitle <- "Frequency"
combined_ideas2_title <- "Combined Ideas from Different Courses When Completing Assignments"

a_3 <- plot_survey_fun(combined_ideas2_df, combined_ideas2_labels, combined_ideas2_xtitle, combined_ideas2_ytitle, combined_ideas2_title)

###CONNECTED YOUR LEARNING TO SOCIETAL PROBLEMS OR HEALTH-RELATED ISSUES
connected_learning2_df <- as.data.frame(table(combined_df_csv$If.game.based.learning.approaches.were.incorporated.into.one.of.your.science.courses..how.do.you.think.that.this.mode.of.learning.can.help.with.the.following...Connecting.learning.to.societal.problems.or.health.related.issues.))
connected_learning2_df$Var1 <- factor(connected_learning2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
connected_learning2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
connected_learning2_xtitle <- "Responses"
connected_learning2_ytitle <- "Frequency"
connected_learning2_title <- "Connected Learning to Societal/Health-Related Issues"

b_3 <- plot_survey_fun(connected_learning2_df, connected_learning2_labels, connected_learning2_xtitle, connected_learning2_ytitle, connected_learning2_title)

###LEARNED SOMETHING THAT CHANGED THE WAY YOU UNDERSTAND AN ISSUE OR CONCEPT
learned_something2_df <- as.data.frame(table(combined_df_csv$If.game.based.learning.approaches.were.incorporated.into.one.of.your.science.courses..how.do.you.think.that.this.mode.of.learning.can.help.with.the.following...Learn.something.that.changes.the.way.you.understand.an.issue.or.concept.))
learned_something2_df$Var1 <- factor(learned_something2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
learned_something2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
learned_something2_xtitle <- "Responses"
learned_something2_ytitle <- "Frequency"
learned_something2_title <- "Learned Something that Changed Understanding of An Issue/Concept"

c_3 <- plot_survey_fun(learned_something2_df, learned_something2_labels, learned_something2_xtitle, learned_something2_ytitle, learned_something2_title)

###CONNECTED IDEAS FROM YOUR COURSES TO YOUR PRIOR EXPERIENCES AND KNOWLEDGE
connected_ideas2_df <- as.data.frame(table(combined_df_csv$If.game.based.learning.approaches.were.incorporated.into.one.of.your.science.courses..how.do.you.think.that.this.mode.of.learning.can.help.with.the.following...Connect.ideas.from.your.courses.to.your.prior.experiences.and.knowledge.))
connected_ideas2_df$Var1 <- factor(connected_ideas2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
connected_ideas2_labels <- c("No Response","Very Little", "Some", "Quite a Bit", "Very Much")
connected_ideas2_xtitle <- "Responses"
connected_ideas2_ytitle <- "Frequency"
connected_ideas2_title <- "Connected Ideas from Courses to Prior Experiences and Knowledge"

d_3 <- plot_survey_fun(connected_ideas2_df, connected_ideas2_labels, connected_ideas2_xtitle, connected_ideas2_ytitle, connected_ideas2_title)

###SAVING PLOT TO DIRECTORY
tiff("figures/how_does_gbl_help_in_science.tiff", units="in", width=16, height=9, res=300)
ggarrange(a_3,b_3,c_3,d_3, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
dev.off()

#Q6
#####################################################################################################################
###IF BIO1A03 ADDED A GAME-BASED LEARNING COMPONENT, HOW MUCH WOULD THIS IMPROVE YOUR MOTIVATION TO DO THE FOLLOWING#
#####################################################################################################################
###MEMORIZING COURSE MATERIAL
memorizing2_df <- as.data.frame(table(combined_df_csv$If.BIO1A03.added.a.game.based.learning.component..how.much.would.this.improve.your.motivation.to.do.the.following...Memorizing.course.material.))
memorizing2_df$Var1 <- factor(memorizing2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
memorizing2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
memorizing2_xtitle <- "Responses"
memorizing2_ytitle <- "Frequency"
memorizing2_title <- "Memorizing Course Material"

a_4 <- plot_survey_fun(memorizing2_df, memorizing2_labels, memorizing2_xtitle, memorizing2_ytitle, memorizing2_title)

###Applying facts, theories, or methods to practical problems or new situations
applying2_df <- as.data.frame(table(combined_df_csv$If.BIO1A03.added.a.game.based.learning.component..how.much.would.this.improve.your.motivation.to.do.the.following...Applying.facts..theories..or.methods.to.practical.problems.or.new.situations.))
applying2_df$Var1 <- factor(applying2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
applying2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
applying2_xtitle <- "Responses"
applying2_ytitle <- "Frequency"
applying2_title <- "Applying Facts, Theories, Methods to Problems/New Situations"

b_4 <- plot_survey_fun(applying2_df, applying2_labels, applying2_xtitle, applying2_ytitle, applying2_title)

###Analyzing an idea, experience, or line of reasoning in depth by examining its parts
analyzing2_df <- as.data.frame(table(combined_df_csv$If.BIO1A03.added.a.game.based.learning.component..how.much.would.this.improve.your.motivation.to.do.the.following...Analyzing.an.idea..experience..or.line.of.reasoning.in.depth.by.examining.its.parts.))
analyzing2_df$Var1 <- factor(analyzing2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
analyzing2_labels <- c("No Response", "Very Little", "Some", "Quite a Bit", "Very Much")
analyzing2_xtitle <- "Responses"
analyzing2_ytitle <- "Frequency"
analyzing2_title <- "Analyzing an Idea, Experience, or Line of Reasoning in Depth"

c_4 <- plot_survey_fun(analyzing2_df, analyzing2_labels, analyzing2_xtitle, analyzing2_ytitle, analyzing2_title)

###Forming a new idea or understanding from various pieces of information
forming2_df <- as.data.frame(table(combined_df_csv$If.BIO1A03.added.a.game.based.learning.component..how.much.would.this.improve.your.motivation.to.do.the.following...Forming.a.new.idea.or.understanding.from.various.pieces.of.information.))
forming2_df$Var1 <- factor(forming2_df$Var1, levels = c("", "Very little", "Some", "Quite a bit", "Very much"))
forming2_labels <- c("No Response","Very Little", "Some", "Quite a Bit", "Very Much")
forming2_xtitle <- "Responses"
forming2_ytitle <- "Frequency"
forming2_title <- "Forming a New Idea or Understanding from Various Pieces of Information"

d_4 <- plot_survey_fun(forming2_df, forming2_labels, forming2_xtitle, forming2_ytitle, forming2_title)

tiff("figures/ifbio1a03_added_gblcomponent.tiff", units="in", width=16, height=9, res=300)
ggarrange(a_4,b_4,c_4,d_4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2)
dev.off()

#Q7
#################################################################################################################################################################
###How likely is it that you would play the video game Cells at War on your own time, outside of class to further consolidate material taught during this class?#
#################################################################################################################################################################
how_likely_df <- as.data.frame(table(combined_df_csv$How.likely.is.it.that.you.would.play.the.video.game.Cells.at.War.on.your.own.time..outside.of.class.to.further.consolidate.material.taught.during.this.class...Select.one.option.))
how_likely_df$Var1 <- factor(how_likely_df$Var1, levels = c("", "Not Likely at All", "Not Very Likely", "Somewhat Likely", "Very Likely"))
how_likely_labels <- c("No Response", "Not Likely at All", "Not Very Likely", "Somewhat Likely", "Very Likely")
how_likely_xtitle <- "Responses"
how_likely_ytitle <- "Frequency"
how_likely_title <- "How Likely is it You Would Play Cells at War on Your Own Time"

##SAVING THE PLOT TO DIRECTORY
tiff("figures/how_likely_you_would_play_cellsatwar.tiff", units="in", width=12, height=6, res=300)
plot_survey_fun(how_likely_df, how_likely_labels, how_likely_xtitle, how_likely_ytitle, how_likely_title)
dev.off()

#Q8
###########################################################################################################################################################################################
###How prepared would you feel if you were given a quiz on Pompe Disease based on the Cells at War game, compared to studying off traditional lecture slides (with accompanying readings)?#
###########################################################################################################################################################################################
how_prepared_df <- as.data.frame(table(combined_df_csv$How.prepared.would.you.feel.if.you.were.given.a.quiz.on.Pompe.Disease.based.on.the.Cells.at.War.game..compared.to.studying.off.traditional.lecture.slides..with.accompanying.readings....Select.one.option.))
how_prepared_labels <- c("No Response", "Not Prepared at All", "Not Very Prepared", "Somewhat Prepared", "Very Prepared")
how_prepared_xtitle <- "Responses"
how_prepared_ytitle <- "Frequency"
how_prepared_title <- "If Given a Quiz on Pompe Disease based off Cells at War, How Prepared Would you Be"

##SAVING THE PLOT TO DIRECTORY
tiff("figures/how_prepared_if_tested_on_pompe_based_off_cellsatwar.tiff", units="in", width=12, height=6, res=300)
plot_survey_fun(how_prepared_df, how_prepared_labels, how_prepared_xtitle, how_prepared_ytitle, how_prepared_title)
dev.off()

