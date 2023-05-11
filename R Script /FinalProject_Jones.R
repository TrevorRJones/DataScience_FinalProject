# Data Science Final Project Code

##### Loading in Packages and Data ######
pacman::p_load(tidyverse, modelsummary, tidymodels, nflreadr, mice)

# load in combine dataset
combine <- load_combine(
  seasons = TRUE,
  file_type = getOption("nflreadr.prefer", default = "rds")) %>% 
  filter(season >= 2013)
# create drafted variable
combine$drafted <- ifelse(is.na(combine$draft_round), 0, 1)
combine$drafted <- as.factor(combine$drafted)

##### Data Cleaning + Manipulation #####

# select variables of interest
df <- combine %>% select(draft_year, draft_round, draft_ovr, player_name, pos, school, 
                         ht, wt, forty, bench, vertical, broad_jump, cone, shuttle, 
                         drafted)

# change height from character to numerical variable
feet_to_inches <- function(x) {
  parts <- strsplit(x, "-")[[1]]
  as.numeric(parts[1]) * 12 + as.numeric(parts[2])
}
df$ht <- sapply(df$ht, feet_to_inches)
# Creating dummy variables for school conference
# Created dummy variables for SEC, ACC, PAC12, Big12, Big10, and whether or not school is P5 (included ND)
# create variable for if prospect came from SEC school or not
SECsc <- c("Mississippi", "Mississippi St.", "Mississippi State", "Missouri", 
           "Florida", "Alabama", "LSU", "Georgia", "South Carolina", "Florida", 
           "Arkansas", "Tennessee", "Texas A&M", "Vanderbilt", "Auburn", 
           "Kentucky")
df$SEC <- ifelse(df$school %in% SECsc, 1, 0) %>% factor()
# Big 12 school
Big12sc <- c("Baylor", "Iowa State", "Iowa St.", "Kansas", "Kansas State","Kansas St.",
             "Oklahoma", "Oklahoma St.",
             "Oklahoma State", "TCU", "Texas", "Texas Tech", "West Virginia")
df$Big12 <- ifelse(df$school %in% Big12sc, 1, 0) %>% factor()
# Big 10 schools
Big10sc <- c("Illinois", "Indiana", "Iowa", "Maryland", "Michigan", "Michigan St.", 
             "Michigan State", "Penn State", "Ohio State",
             "Minnesota", "Nebraska", "Northwestern", "Ohio St.", "Penn St.",
             "Purdue", "Rutgers", "Wisconsin")
df$Big10 <- ifelse(df$school %in% Big10sc, 1, 0) %>% factor()
# PAC 12 schools
Pac12sc <- c("California", "Oregon", "UCLA", "Colorado", "USC", "Washington", 
             "Arizona", "Arizona St.", "Stanford", "Utah", "Washington State",
             "Oregon St.", "Oregon State", "Washington St.")
df$PAC12 <- ifelse(df$school %in% Pac12sc, 1, 0) %>% factor()
# ACC Schools
ACCsc <- c(
  "Boston College", "Boston Col.","Clemson","Duke","Florida State", "Florida St.",
  "Georgia Tech","Louisville","Miami","North Carolina", "North Carolina St.",
  "North Carolina State","Pittsburgh","Syracuse","Virginia","Virginia Tech",
  "Wake Forest")
df$ACC <- ifelse(df$school %in% ACCsc, 1, 0) %>% factor()

# Now, create dummy variables for each position in the dataset
df <- df %>%
  mutate(C = ifelse(pos == "C", 1, 0),
         CB = ifelse(pos == "CB", 1, 0),
         DB = ifelse(pos == "DB", 1, 0),
         DE = ifelse(pos == "DE", 1, 0),
         DL = ifelse(pos == "DL", 1, 0),
         DT = ifelse(pos == "DT", 1, 0),
         EDGE = ifelse(pos == "EDGE", 1, 0),
         FB = ifelse(pos == "FB", 1, 0),
         ILB = ifelse(pos == "ILB", 1, 0),
         K = ifelse(pos == "K", 1, 0),
         LB = ifelse(pos == "LB", 1, 0),
         LS = ifelse(pos == "LS", 1, 0),
         OG = ifelse(pos == "OG", 1, 0),
         OL = ifelse(pos == "OL", 1, 0),
         OLB = ifelse(pos == "OLB", 1, 0),
         OT = ifelse(pos == "OT", 1, 0),
         P = ifelse(pos == "P", 1, 0),
         QB = ifelse(pos == "QB", 1, 0),
         RB = ifelse(pos == "RB", 1, 0),
         S = ifelse(pos == "S", 1, 0),
         TE = ifelse(pos == "TE", 1, 0),
         WR = ifelse(pos == "WR", 1, 0))
# read them as factor variables
df[, c("OT", "CB", "WR", "P", "OG", "ILB", "RB", "DE", "QB", "DT", "FB", "OLB", 
       "S", "K", "C", "TE", "LS", "DL", "EDGE", "LB", "DB", "OL")] <- 
  lapply(df[, c("OT", "CB", "WR", "P", "OG", "ILB", "RB", "DE", "QB", "DT", 
                "FB", "OLB", "S", "K", "C", "TE", "LS", "DL", "EDGE", "LB", 
                "DB", "OL")], factor)

# splitting data into two sets
# now, I'm going to divide the data into two separate sets. One with only individuals who were drafted, one with all the datasets
df_all <- select(df, -draft_year, -draft_round, -draft_ovr)
df_drafted <- df %>% filter(!is.na(draft_ovr)) # only individuals who are drafted
datasummary_skim(df_all)
datasummary_skim(df_drafted)

#### Data Imputation ####

# there's a lot of missing data. often times, players opt out of certain drills from advice from their agents or coaches
# therefore, I think the data is missing not at random, because it is due to some unobserved variable in the data
# going to impute with multiple imputation through MICE

# first, impute data with all players (both drafted and undrafted)
imp.data.all <- mice(data = df_all, m = 50, maxit = 10, seed = 27, print = F)
imp.data.all # if you want to see what data was used, MICE used pmm for all variables of interest
df.all <- complete(imp.data.all, "long")
df.all <- as.data.frame(df.all)

# next, we impute the data for all players that were drafted
imp.data.drafted <- mice(data = df_drafted, m = 50, maxit = 10, seed = 27, print = F)
imp.data.drafted # MICE used pmm for all variables of interest
df.drafted <- complete(imp.data.drafted, "long")

datasummary_skim(df.all)
datasummary_skim(df.drafted)


#### Linear Models ####
# model for draft overall
est1 <- lm(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
           data = df.drafted)
# now include conference variables
est2 <- lm(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle +
             ACC + Big10 + Big12 + PAC12 + SEC,
           data = df.drafted)
models1 <- list(est1, est2)
modelsummary(models1, stars = T)


# model for if drafted or not
est3 <- glm(drafted ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle,
            data = df.all, family = binomial(link = "logit"))
est4 <- glm(drafted ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle +
              ACC + Big10 + Big12 + PAC12 + SEC,
            data = df.all, family = binomial(link = "logit"))
models2 <- list(est3, est4)
modelsummary(models2, stars = T)

#### First group of Models: overall draft pick ####

# Split data into training and testing data
df_split <- initial_split(df.drafted, prop = 0.8)
df_train <- training(df_split)
df_test <- testing(df_split)

    # because the prediction is a continuous variable, mode of the models will be regression

# TREE MODEL #
      
      print('Starting TREE')
      
      tune_tree_spec <- decision_tree(
        min_n = tune(),
        tree_depth = tune(),
        cost_complexity = tune(),
      ) %>% 
        set_engine("rpart") %>% 
        set_mode("regression")
      
      # regularization parameter
      tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
      tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
      tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
      tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% 
        full_join(.,tree_parm_df3,by=character())
      
      # 3-Fold CV
      rec_folds_tree <- vfold_cv(df_train, v = 3)
      
      # Workflow
      rec_wf_tree <- workflow() %>%
        add_model(tune_tree_spec) %>%
        add_formula(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning Results
      rec_res_tree <- rec_wf_tree %>%
        tune_grid(
          resamples = rec_folds_tree,
          grid = tree_parm_df
        )
      # issue with this is that there is not a lot of variation within some of the estimates, 
      # may lead to overfitting
      
      # what is the best value of lambda?
      top_acc_tree  <- show_best(rec_res_tree, metric = "rmse")
      best_acc_tree <- select_best(rec_res_tree, metric = "rmse")
      final_tree_lasso <- finalize_workflow(rec_wf_tree,
                                            best_acc_tree
      )
      print('*********** TREE MODEL **************')
      tree_test <- last_fit(final_tree_lasso,df_split) %>%
        collect_metrics()
      
      tree_test %>% print(n = 1)
      top_acc_tree %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      tree_ans <- top_acc_tree %>% slice(1)
      tree_ans %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "tree") %>% select(-starts_with(".config"))

# Report all answers in a single summary
      datasummary_df(tree_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
      
#### Second Group of Models: Drafted or Undrafted ####
      
      # Split data into training and testing data
      df_split <- initial_split(df.all, prop = 0.8)
      df_train <- training(df_split)
      df_test <- testing(df_split)
      
      # because the prediction is a continuous variable, mode of the models will be regression

# TREE MODEL #
      
      print('Starting TREE')
      
      tune_tree_spec <- decision_tree(
        min_n = tune(),
        tree_depth = tune(),
        cost_complexity = tune(),
      ) %>% 
        set_engine("rpart") %>% 
        set_mode("classification")
      
      # regularization parameter
      tree_parm_df1 <- tibble(cost_complexity = seq(.001,.2,by=.05))
      tree_parm_df2 <- tibble(min_n = seq(10,100,by=10))
      tree_parm_df3 <- tibble(tree_depth = seq(5,20,by=5))
      tree_parm_df  <- full_join(tree_parm_df1,tree_parm_df2,by=character()) %>% 
        full_join(.,tree_parm_df3,by=character())
      
      # 3-Fold CV
      rec_folds_tree <- vfold_cv(df_train, v = 3)
      
      # Workflow
      rec_wf_tree <- workflow() %>%
        add_model(tune_tree_spec) %>%
        add_formula(drafted ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning Results
      rec_res_tree <- rec_wf_tree %>%
        tune_grid(
          resamples = rec_folds_tree,
          grid = tree_parm_df
        )
      # issue with this is that there is not a lot of variation within some of the estimates, 
      # may lead to overfitting
      
      # what is the best value of lambda?
      top_acc_tree  <- show_best(rec_res_tree, metric = "accuracy")
      best_acc_tree <- select_best(rec_res_tree, metric = "accuracy")
      final_tree_lasso <- finalize_workflow(rec_wf_tree,
                                            best_acc_tree
      )
      print('*********** TREE MODEL **************')
      tree_test <- last_fit(final_tree_lasso,df_split) %>%
        collect_metrics()
      
      tree_test %>% print(n = 1)
      top_acc_tree %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      tree_ans1 <- top_acc_tree %>% slice(1)
      tree_ans1 %<>% left_join(tree_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "tree") %>% select(-starts_with(".config"))
      
      # Report all answers in a single summary
      datasummary_df(tree_ans1 %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
      
      all_ans <- bind_rows(tree_ans,tree_ans1)
      datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
      
#### OUTPUTS AND VISUALIZATIONS ####
# outputs
  # tables
  datasummary_skim(df, histogram = F, output = "sum.tex")  # initial data analysis
  datasummary_skim(df.all, histogram = F, output = "sum1.tex") # imputed data for drafted and undrafted variables
  datasummary_skim(df.drafted, histogram = F, output = "sum2.tex") # imputed data for all drafted individuals
  modelsummary(models1, stars = T, output = "res1.tex") # regression models
  modelsummary(models2, stars = T, output = "res2.tex") # logit models
  datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="res3.tex")

  # figures
library(gridExtra)
  # 40
p1 <- df %>% filter(pos %in% c("QB", "CB", "WR", "RB", "DB", "S")) %>% 
    ggplot(aes(x = forty, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "40 Yard Dash: Skill Positions")
p2 <- df %>% filter(pos %in% c("ILB", "FB", "OLB", "TE", "EDGE", "LB")) %>% 
    ggplot(aes(x = forty, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "40 Yard Dash: Big-Skill Positions")
p3 <- df %>% filter(pos %in% c("OT", "OG", "DE", "DT", "C", "DL", "OL")) %>% 
    ggplot(aes(x = forty, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "40 Yard Dash: Big Positions")
fourty <- grid.arrange(p1,p2,p3, ncol = 3)
ggsave("FOURTYSPLITS.png", fourty, width = 10, height = 4, dpi = 300)


  #bench
p4 <- df %>% filter(pos %in% c("QB", "CB", "WR", "RB", "DB", "S")) %>% 
  ggplot(aes(x = bench, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Bench Press: Skill Positions")
p5 <- df %>% filter(pos %in% c("ILB", "FB", "OLB", "TE", "EDGE", "LB")) %>% 
  ggplot(aes(x = bench, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Bench Press: Big-Skill Positions")
p6 <- df %>% filter(pos %in% c("OT", "OG", "DE", "DT", "C", "DL", "OL")) %>% 
  ggplot(aes(x = bench, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Bench Press: Big Positions")
benchpress <- grid.arrange(p4,p5,p6, ncol = 3)
ggsave("BENCHPRESS.png", benchpress, width = 10, height = 4, dpi = 300)

  
  # broad jump
p7 <- df %>% filter(pos %in% c("QB", "CB", "WR", "RB", "DB", "S")) %>% 
  ggplot(aes(x = broad_jump, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Broad Jump: Skill Positions")
p8 <- df %>% filter(pos %in% c("ILB", "FB", "OLB", "TE", "EDGE", "LB")) %>% 
  ggplot(aes(x = broad_jump, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Broad Jump: Big-Skill Positions")
p9 <- df %>% filter(pos %in% c("OT", "OG", "DE", "DT", "C", "DL", "OL")) %>% 
  ggplot(aes(x = broad_jump, y = draft_ovr)) + geom_point() + geom_jitter() + 
  theme_minimal() + labs(title = "Broad Jump: Big Positions")
broadjump <- grid.arrange(p7,p8,p9, ncol = 3)
ggsave("BROADJUMP.png", broadjump, width = 10, height = 4, dpi = 300)



# distribution of 40 yd dash
plot1 <- ggplot(data = df) +
  geom_histogram(aes(x = forty, fill = factor(drafted)), 
                 color = "black", 
                 bins = 55, 
                 alpha = 0.8) +
  labs(title = "Distribution of Forty Yard Dash Times", 
       x = "Forty Yard Dash Time (seconds)", 
       y = "Count") +
  scale_fill_manual(values = c("red", "navy"),
                    name = "Drafted", 
                    labels = c("No", "Yes")) + theme_minimal()
ggsave("FORTYHISTO.png", plot1, width = 6, height = 4, dpi = 300)

# distribution of bench press
plot2 <- ggplot(data = df) +
  geom_histogram(aes(x = bench, fill = factor(drafted)), 
                 color = "black", 
                 bins = 40, 
                 alpha = 0.8) +
  labs(title = "Distribution of Bench Press Reps", 
       x = "Repetitions of 225 lbs Bench Press", 
       y = "Count") +
  scale_fill_manual(values = c("red", "navy"),
                    name = "Drafted", 
                    labels = c("No", "Yes")) + theme_minimal()
ggsave("BENCHHISTO.png", plot2, width = 6, height = 4, dpi = 300)

# horizontal jump
plot3 <- ggplot(data = df) +
  geom_histogram(aes(x = broad_jump, fill = factor(drafted)), 
                 color = "black", 
                 bins = 40, 
                 alpha = 0.8) +
  labs(title = "Distribution of Broad Jump Performances", 
       x = "Distance of Broad Jump (Inches)", 
       y = "Count") +
  scale_fill_manual(values = c("red", "navy"),
                    name = "Drafted", 
                    labels = c("No", "Yes")) + theme_minimal()
ggsave("JUMPHISTO.png", plot3, width = 6, height = 4, dpi = 300)
