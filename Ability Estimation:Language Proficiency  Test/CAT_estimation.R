#####################################################################
#
#                   ?? Estimation for a Computer Adaptive
#                   English Language Proficiency Assessment
#                       Author: Meltem Yumsek Akbaba
#                         Last Update: 12/5/2025
#
#####################################################################

rm(list = ls())

setwd("path to your directory")

#--------------------------------------------------------------------
# Required libraries
lapply(c("dplyr", 
         "tidyr",
         "readxl",
         "openxlsx",
         "mirt"), 
       library, character.only = T)

#----------------------------------------------------------------------


##################### Part A: Overall ?? Estimation ####################

#----------------------------------------------------------------------

# 1. Data Preparation & Wrangling


### Reading in the data
dat_scored = read.csv("your data file")

### Creating a new ID to differentiate attempts per test-taker (TT)
dat_scored = dat_scored %>% 
  mutate(session_id = paste0(user_id, "_A", attempt_number))

### Extracting the b parameters to create a pool of difficulty parameters
item_params = dat_scored %>%
  dplyr::select(item_id, raw_difficulty) %>%
  distinct(item_id, .keep_all = TRUE)

### Transforming the long data to wide data
resp_wide = dat_scored %>%
  dplyr::select(session_id, user_id, attempt_number, item_id, score, skill, answered_at) %>%
  arrange(session_id, answered_at) %>%
  pivot_wider(
    id_cols = c(session_id, user_id, attempt_number),
    names_from = item_id,
    names_prefix = "I_",
    values_from = score
  )

### Extracting only item responses from the wide data
### Ensuring responses are coded as numeric
resp_matrix = resp_wide %>%
  dplyr::select(starts_with("I_")) %>%
  mutate(across(everything(), ~ as.numeric(.)))

# Removing items if there is: 
# a) no response (NA-only) 
# b) single response category(all 0, all 0)
# mirt will throw an error otherwise

cols_all_na = sapply(resp_matrix, function(col) all(is.na(col)))
resp_matrix = resp_matrix[, !cols_all_na, drop = FALSE]

cols_one_category = sapply(resp_matrix, function(col) {
  vals = unique(na.omit(col)); length(vals) <= 1
})
resp_matrix = resp_matrix[, !cols_one_category, drop = FALSE]

#--------------------------------------------------------------------


#--------------------------------------------------------------------
# 2.Estimation Preperation

### Building an item parameter map to ensure alignment w/ response matrix
item_cols = colnames(resp_matrix)                         
item_param_map = item_params %>%
  mutate(item_col = paste0("I_", item_id)) %>%
  filter(item_col %in% item_cols) %>%
  arrange(match(item_col, item_cols))

b_map = item_param_map$raw_difficulty

### mirt uses d parameter (intercept) (d = -a * b)
### convert b parameter mapping
d_map = -1 * b_map    

### Quick check to confirm alignment
print(data.frame(item_cols = item_cols[1:20],
                 item_param_item_col = item_param_map$item_col[1:20],
                 raw_b = b_map[1:20],
                 set_d = d_map[1:20]))

### Creating initial model shell to get structure mirt item parameter file
### Item parameters are not to be estimated, pool parameters will be used
mod_init = mirt(resp_matrix, 
                model = 1, 
                itemtype = "2PL", 
                verbose = FALSE, 
                technical = list(NCYCLES = 1))

vals = mod2values(mod_init)

### Overwriting a1 = 1 and mark the parameter not to be estimated
idx_a1 = which(vals$name == "a1")
vals$value[idx_a1] = 1
vals$est[idx_a1]   = FALSE

### Overwriting the d parameters by assigning the pull parameters
### Matching by item name (item_cols)
for(i in seq_along(item_cols)) {
  this_item = item_cols[i]
  row_d = which(vals$item == this_item & vals$name == "d")
  if(length(row_d) != 1) {
    warning(paste0("Unexpected number of rows for item ", 
                   this_item, 
                   " (rows found = ", length(row_d), ")."))
  } else {
    vals$value[row_d] = d_map[i]
    vals$est[row_d]   = FALSE
  }
}
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# 3) ?? Estimation using Warm's Maximum Likelihood (WML)

mod_fixed = mirt(resp_matrix, 1, itemtype = "2PL",
                  pars = vals, verbose = TRUE)


theta_overall = fscores(mod_fixed, method = "WML",
                         full.scores.SE = TRUE)

### Merging ??s back to wide response file
resp_wide_with_theta = bind_cols(resp_wide, as.data.frame(theta_overall))

write.csv(resp_wide_with_theta, 
          "session_thetas_overall.csv", 
          row.names = FALSE)
#----------------------------------------------------------------------


############### Part B: Estimating Skill Specific  ??s ##################


#----------------------------------------------------------------------

# 1. Skill Data Prep:  Extracting skill specific item responses

skills = unique(dat_scored$skill)
skills = skills[!is.na(skills)]

theta_skill_list = list()

for (sk in skills) {
  message("\nProcessing skill: ", sk)

  skill_items = dat_scored %>%
    filter(skill == sk) %>%
    distinct(item_id) %>%
    mutate(item_col = paste0("I_", item_id))
  
  skill_item_cols = intersect(skill_items$item_col, colnames(resp_matrix))
  
  if (length(skill_item_cols) < 3) {
    warning(paste("Skill", sk, "has fewer than 3 usable items. Skipping."))
    next
  }
  
  resp_skill = resp_matrix[, skill_item_cols, drop = FALSE]
  
### Droping NA-only or 1-category items 
  cols_all_na = sapply(resp_skill, function(col) all(is.na(col)))
  resp_skill = resp_skill[, !cols_all_na, drop = FALSE]
  
  cols_one_category = sapply(resp_skill, function(col){
    vals = unique(na.omit(col)); length(vals) <= 1
  })
  resp_skill = resp_skill[, !cols_one_category, drop = FALSE]
  
  skill_item_cols = colnames(resp_skill)
#---------------------------------------------------------------------- 

  
#----------------------------------------------------------------------   
# 2.Estimation Preperation
  
  ### Building an item parameter map to ensure alignment w/ response matrix
  ### Transforing b to d
  item_param_map_skill = item_params %>%
    mutate(item_col = paste0("I_", item_id)) %>%
    filter(item_col %in% skill_item_cols) %>%
    arrange(match(item_col, skill_item_cols))
  
  b_map = item_param_map_skill$raw_difficulty
  d_map = -1 * b_map

  
  mod_init_sk = mirt(resp_skill, 1, itemtype = "2PL",
                      verbose = FALSE, technical = list(NCYCLES = 1))
  vals_sk = mod2values(mod_init_sk)
  
### Fixing a parameters
  idx_a1 = which(vals_sk$name == "a1")
  vals_sk$value[idx_a1] = 1
  vals_sk$est[idx_a1]   = FALSE
  
### Fixing b parameters
  for (i in seq_along(skill_item_cols)) {
    this_item = skill_item_cols[i]
    row_d = which(vals_sk$item == this_item & vals_sk$name == "d")
    vals_sk$value[row_d] = d_map[i]
    vals_sk$est[row_d]   = FALSE
  }
#----------------------------------------------------------------------   
  
  
  
# ---------------------------------------------------------------------
# 3) Skill ?? Estimation using WML 
 
  mod_fixed_sk = mirt(resp_skill, 1, itemtype = "2PL",
                       pars = vals_sk, verbose = FALSE)
  
  theta_sk = fscores(mod_fixed_sk, method = "WML", full.scores.SE = TRUE)
  colnames(theta_sk) = paste0(sk, "_", colnames(theta_sk))
  
  theta_skill_list[[sk]] = theta_sk
}

###Combining ??s for each skill

theta_skills = do.call(cbind, theta_skill_list)

### Merging ??s back to wide response file

resp_wide_with_theta_all = bind_cols(resp_wide, as.data.frame(theta_overall), as.data.frame(theta_skills))

write.xlsx(resp_wide_with_theta_all,
          "session_thetas_overall_and_skills.xlsx",
          RowNames = FALSE)

# ---------------------------------------------------------------------


