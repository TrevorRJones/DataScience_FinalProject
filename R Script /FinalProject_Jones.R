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

      
# NEURAL NETWORK #
      
      print('Starting NNET')
      # set up the task and the engine
      tune_nnet_spec <- mlp(
        hidden_units = tune(), # tuning parameter
        penalty = tune()
      ) %>% 
        set_engine("nnet") %>%
        set_mode("regression")
      
      # define a set over which to try different values of the regularization parameter (number of neighbors)
      nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
      lambda_grid   <- grid_regular(penalty(), levels = 10)
      nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())
      
      # 5-fold cross-validation
      rec_folds_nnet <- vfold_cv(df_train, v = 3)
      
      # Workflow
      rec_wf_nnet <- workflow() %>%
        add_model(tune_nnet_spec) %>%
        add_formula(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning results
      rec_res_nnet <- rec_wf_nnet %>%
        tune_grid(
          resamples = rec_folds_nnet,
          grid = nnet_parm_df
        )
      
      # what is the best value of lambda?
      top_acc_nnet  <- show_best(rec_res_nnet, metric = "rmse")
      best_acc_nnet <- select_best(rec_res_nnet, metric = "rmse")
      final_nnet_lasso <- finalize_workflow(rec_wf_nnet,
                                            best_acc_nnet
      )
      print('*********** NEURAL NET **************')
      nnet_test <- last_fit(final_nnet_lasso,df_split) %>%
        collect_metrics()
      
      nnet_test %>% print(n = 1)
      top_acc_nnet %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      nnet_ans <- top_acc_nnet %>% slice(1)
      nnet_ans %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "nnet") %>% select(-starts_with(".config"))

      
# K NEAREST NEIGHBOR #
      
      print('Starting KNN')
      # set up the task and the engine
      tune_knn_spec <- nearest_neighbor(
        neighbors = tune() # tuning parameter
      ) %>% 
        set_engine("kknn") %>%
        set_mode("regression")
      
      # define a set over which to try different values of the regularization parameter (number of neighbors)
      knn_parm_df <- tibble(neighbors = seq(1,30))
      
      # 3-fold cross-validation
      rec_folds_knn <- vfold_cv(income_train, v = 3)
      
      # Workflow
      rec_wf_knn <- workflow() %>%
        add_model(tune_knn_spec) %>%
        add_formula(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning results
      rec_res_knn <- rec_wf_knn %>%
        tune_grid(
          resamples = rec_folds_knn,
          grid = knn_parm_df
        )
      
      # what is the best value of lambda?
      top_acc_knn  <- show_best(rec_res_knn, metric = "rmse")
      best_acc_knn <- select_best(rec_res_knn, metric = "rmse")
      final_knn_lasso <- finalize_workflow(rec_wf_knn,
                                           best_acc_knn
      )
      print('*********** K NEAREST NEIGHBORS **************')
      knn_test <- last_fit(final_knn_lasso,df_split) %>%
        collect_metrics()
      
      knn_test %>% print(n = 1)
      top_acc_knn %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      knn_ans <- top_acc_knn %>% slice(1)
      knn_ans %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "knn") %>% select(-starts_with(".config"))

# Report all answers in a single summary
      all_ans <- bind_rows(tree_ans,nnet_ans,knn_ans)
      datasummary_df(all_ans %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
      
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
      
      
  # NEURAL NETWORK #
      
      print('Starting NNET')
      # set up the task and the engine
      tune_nnet_spec <- mlp(
        hidden_units = tune(), # tuning parameter
        penalty = tune()
      ) %>% 
        set_engine("nnet") %>%
        set_mode("classification")
      
      # define a set over which to try different values of the regularization parameter (number of neighbors)
      nnet_parm_df1 <- tibble(hidden_units = seq(1,10))
      lambda_grid   <- grid_regular(penalty(), levels = 10)
      nnet_parm_df  <- full_join(nnet_parm_df1,lambda_grid,by=character())
      
      # 5-fold cross-validation
      rec_folds_nnet <- vfold_cv(df_train, v = 3)
      
      # Workflow
      rec_wf_nnet <- workflow() %>%
        add_model(tune_nnet_spec) %>%
        add_formula(drafted ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning results
      rec_res_nnet <- rec_wf_nnet %>%
        tune_grid(
          resamples = rec_folds_nnet,
          grid = nnet_parm_df
        )
      
      # what is the best value of lambda?
      top_acc_nnet  <- show_best(rec_res_nnet, metric = "accuracy")
      best_acc_nnet <- select_best(rec_res_nnet, metric = "accuracy")
      final_nnet_lasso <- finalize_workflow(rec_wf_nnet,
                                            best_acc_nnet
      )
      print('*********** NEURAL NET **************')
      nnet_test <- last_fit(final_nnet_lasso,df_split) %>%
        collect_metrics()
      
      nnet_test %>% print(n = 1)
      top_acc_nnet %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      nnet_ans1 <- top_acc_nnet %>% slice(1)
      nnet_ans1 %<>% left_join(nnet_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "nnet") %>% select(-starts_with(".config"))
      
      
 # K NEAREST NEIGHBOR #
      
      print('Starting KNN')
      # set up the task and the engine
      tune_knn_spec <- nearest_neighbor(
        neighbors = tune() # tuning parameter
      ) %>% 
        set_engine("kknn") %>%
        set_mode("classification")
      
      # define a set over which to try different values of the regularization parameter (number of neighbors)
      knn_parm_df <- tibble(neighbors = seq(1,30))
      
      # 3-fold cross-validation
      rec_folds_knn <- vfold_cv(income_train, v = 3)
      
      # Workflow
      rec_wf_knn <- workflow() %>%
        add_model(tune_knn_spec) %>%
        add_formula(draft_ovr ~ wt + ht + forty + bench + vertical + broad_jump + cone + shuttle + SEC + 
                      Big12 + Big10 + PAC12 + ACC + C + CB + DB + DE + DL + DT + EDGE + FB + ILB +
                      K + LB + LS + OG + OL + OLB + OT + P + QB + RB + S + TE + WR)
      
      # Tuning results
      rec_res_knn <- rec_wf_knn %>%
        tune_grid(
          resamples = rec_folds_knn,
          grid = knn_parm_df
        )
      
      # what is the best value of lambda?
      top_acc_knn  <- show_best(rec_res_knn, metric = "accuracy")
      best_acc_knn <- select_best(rec_res_knn, metric = "accuracy")
      final_knn_lasso <- finalize_workflow(rec_wf_knn,
                                           best_acc_knn
      )
      print('*********** K NEAREST NEIGHBORS **************')
      knn_test <- last_fit(final_knn_lasso,df_split) %>%
        collect_metrics()
      
      knn_test %>% print(n = 1)
      top_acc_knn %>% print(n = 1)
      
      # combine results into a nice tibble (for later use)
      knn_ans1 <- top_acc_knn %>% slice(1)
      knn_ans1 %<>% left_join(knn_test %>% slice(1),by=c(".metric",".estimator")) %>%
        mutate(alg = "knn") %>% select(-starts_with(".config"))
      
      # Report all answers in a single summary
      all_ans1 <- bind_rows(tree_ans1,nnet_ans1,knn_ans1)
      datasummary_df(all_ans1 %>% select(-.metric,-.estimator,-mean,-n,-std_err),output="markdown")
