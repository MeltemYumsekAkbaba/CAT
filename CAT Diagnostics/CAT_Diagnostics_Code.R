#####################################################################
#
#                         CAT Diagnostics for an
#                   English Language Proficiency Assessment
#                       Author: Meltem Yumsek Akbaba
#                         Last Update: 12/5/2025
#
#####################################################################

rm(list = ls())

setwd("path to your directory")

out_dir = "CAT_diagnostics_outputs"
if(!dir.exists(out_dir)) dir.create(out_dir)

#--------------------------------------------------------------------
# Required libraries
lapply(c("dplyr", 
         "tidyr",
         "readxl",
         "openxlsx",
         "ggplot2",
         "scales",
         "lubridate",
         "DescToola"), 
       library, character.only = T)

#----------------------------------------------------------------------


#----------------------------------------------------------------------

# 1. Data Preparation & Wrangling

options(stringsAsFactors = FALSE)

### Reading in the datasets
dat_long = read.csv("long data file")
dat_wide = read.csv("wide data file")

names(dat_long) = tolower(names(dat_long))
names(dat_wide) = tolower(names(dat_wide))

if("answered_at" %in% names(dat_long)) {
  dat_long = dat_long %>%
    mutate(answered_at_parsed = parse_date_time(answered_at,
                                                orders = c("d/m/Y H:M:S",
                                                           "d/m/Y H:M","d/m/Y",
                                                           "Y-m-d H:M:S",
                                                           "Y-m-d H:M"),
                                                tz = "UTC", quiet = TRUE))
} else {
  dat_long$answered_at_parsed = NA
}

### Preparing item pool table (b paramter) 
item_pool = dat_long %>%
  filter(!is.na(item_id)) %>%
  select(item_id, raw_difficulty) %>%
  distinct(item_id, .keep_all = TRUE) %>%
  mutate(item_col = paste0("I_", item_id),
         b = as.numeric(raw_difficulty),
         a = 1) %>%
  filter(!is.na(b))

### Some items may be in the response matrix but not in pool; warn
resp_item_cols = grep("^I_", names(dat_wide), value = TRUE)
missing_items = setdiff(resp_item_cols, item_pool$item_col)
if(length(missing_items) > 0) {
  warning("Difficulty is missing for the following items: ",
          paste(head(missing_items, 10), collapse = ", "), 
          if(length(missing_items)>10) "..." else "")
}

### Ensuring thetas exist for skills overall ability and skills
req_theta_cols = c("f1","se_f1","listening_f1","listening_se_f1",
                    "grammar_f1","grammar_se_f1","reading_f1","reading_se_f1")
missing_theta_cols = setdiff(req_theta_cols, tolower(names(dat_wide)))
if(length(missing_theta_cols) > 0) {
  warning("Missing expected theta/SE columns in wide data: ", 
          paste(missing_theta_cols, collapse = ", "))
}

wide = dat_wide

#----------------------------------------------------------------------



#----------------------------------------------------------------------
# 2. Conditional Standard Error of Measurement (CSEM)


### Overall
csem_df = wide %>%
  filter(!is.na(f1) & !is.na(se_f1)) %>%
  mutate(f1 = as.numeric(f1), se_f1 = as.numeric(se_f1))

p1 = ggplot(csem_df, aes(x = f1, y = se_f1)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "CSEM (Overall): SE vs Theta", 
       x = "Theta (F1)", y = "SE (F1)") +
  theme_minimal()
ggsave(file.path(out_dir, "CSEM_overall.png"), 
       p1, width = 7, height = 5, dpi = 150)

### Skills: create CSEM for each skill if present
skill_cols = list(
  Listening = c("listening_f1", "listening_se_f1"),
  Grammar   = c("grammar_f1", "grammar_se_f1"),
  Reading   = c("reading_f1", "reading_se_f1")
)

for(sk in names(skill_cols)) {
  thc = skill_cols[[sk]][1]; sec = skill_cols[[sk]][2]
  if(all(c(thc, sec) %in% names(wide))) {
    dfk = wide %>% filter(!is.na(.data[[thc]]) & !is.na(.data[[sec]]))
    p = ggplot(dfk, aes_string(x = thc, y = sec)) +
      geom_point(alpha = 0.35, size = 1) +
      geom_smooth(method = "loess", se = TRUE) +
      labs(title = paste0("CSEM (", sk, "): SE vs Theta"), 
           x = paste0(sk, " Theta"),
           y = "SE") +
      theme_minimal()
    ggsave(file.path(out_dir, paste0("CSEM_", sk, ".png")), 
           p, width = 7, height = 5, dpi = 150)
  }
}

#----------------------------------------------------------------------



#----------------------------------------------------------------------
# 3. Marginal reliability 


### function to compute marginal reliability
marginal_rel = function(theta_vec, se_vec) {
  theta_vec = as.numeric(theta_vec); se_vec = as.numeric(se_vec)
  ok = !is.na(theta_vec) & !is.na(se_vec)
  if(sum(ok) < 3) return(NA_real_)
  1 - mean(se_vec[ok]^2) / var(theta_vec[ok])
}

marginal_results = tibble(
  scale = "Overall",
  marginal_reliability = marginal_rel(wide$f1, wide$se_f1)
)

for(sk in names(skill_cols)) {
  thc = skill_cols[[sk]][1]; sec = skill_cols[[sk]][2]
  if(all(c(thc, sec) %in% names(wide))) {
    marginal_results = marginal_results %>%
      bind_rows(tibble(scale = sk,
                       marginal_reliability = marginal_rel(wide[[thc]], 
                                                           wide[[sec]])))
  }
}
write.csv(marginal_results, file.path(out_dir,
                                      "marginal_reliabilities.csv"), 
          row.names = FALSE)
#----------------------------------------------------------------------



#----------------------------------------------------------------------
# 4. Targeting: mean administered b vs person theta

### Using long file to compute mean b per session 
dat_long2 = dat_long %>%
  mutate(item_col = paste0("I_", item_id),
         session_id = if("session_id" %in% names(dat_long)) dat_long$session_id 
         else paste0(user_id, "_A", attempt_number))


dat_long_items = dat_long2 %>% filter(item_col %in% resp_item_cols)

session_b = dat_long_items %>%
  group_by(session_id) %>%
  summarise(mean_b_admin = mean(as.numeric(raw_difficulty), na.rm = TRUE),
            sd_b_admin = sd(as.numeric(raw_difficulty), na.rm = TRUE),
            n_items = n(),
            first_time = min(answered_at_parsed, na.rm = TRUE),
            last_time  = max(answered_at_parsed, na.rm = TRUE)) %>%
  ungroup()

### Merging with person theta
target_df = wide %>%
  mutate(session_id = if("session_id" %in% names(wide)) wide$session_id 
         else paste0(user_id, "_A", attempt_number)) %>%
  select(session_id, f1, se_f1) %>%
  left_join(session_b, by = "session_id") %>%
  filter(!is.na(f1) & !is.na(mean_b_admin))

### Correlation and slope
target_cor = cor(target_df$f1, target_df$mean_b_admin, use = "complete.obs")
target_lm = lm(mean_b_admin ~ f1, data = target_df)
target_slope = coef(target_lm)["f1"]

cat("Targeting: correlation (rho) =", round(target_cor,3), 
    " ; slope =", round(target_slope,3), "\n")

p_target = ggplot(target_df, aes(x = f1, y = mean_b_admin)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Targeting: Person Theta vs Mean Administered b",
       x = "Theta (F1)", 
       y = "Mean administered item difficulty (b)") + theme_minimal()
ggsave(file.path(out_dir, "targeting_theta_vs_mean_b.png"), 
       p_target, width = 7, height = 5, dpi = 150)

#----------------------------------------------------------------------


#----------------------------------------------------------------------

# 5. Information at final theta for administered items

invlogit = function(x) 1/(1+exp(-x))
item_prob = function(theta, a, b) invlogit(a * (theta - b))
item_info = function(theta, a, b) {
  p = item_prob(theta, a, b)
  a^2 * p * (1 - p)
}

### For each session computing:
# - final theta (from wide)
# - administered items list
# - avg info of administered items at final theta
# - max pool info at final theta
# - ratio avg_admin_info / max_pool_info

wide_sessions = wide %>%
  mutate(session_id = if("session_id" %in% names(wide)) wide$session_id 
         else paste0(user_id, "_A", attempt_number)) %>%
  select(session_id, user_id, attempt_number, f1, se_f1)

### Preparing pool info function vectorized
pool_b = item_pool %>% filter(item_col %in% resp_item_cols)
if(nrow(pool_b) == 0) {
  stop("No item difficulty info (pool_b) matches the item columns in the wide file.")
}

### Computing info for each session
session_list = wide_sessions$session_id
info_comp = vector("list", length(session_list))
names(info_comp) = session_list

### Preparing lookup of administered items by session from long
admin_by_session = dat_long_items %>%
  group_by(session_id) %>%
  arrange(answered_at_parsed) %>%
  summarise(admin_item_cols = list(paste0("it_", item_id)), 
            admin_item_ids = list(item_id),
            admin_b = list(as.numeric(raw_difficulty)), n_items = n()) %>% 
  ungroup()

admin_lookup = admin_by_session %>% select(session_id, admin_item_cols, admin_b) 

for(i in seq_along(session_list)) {
  sid = session_list[i]
  theta_final = wide_sessions$f1[i]
  if(is.na(theta_final)) {
    info_comp[[sid]] = tibble(session_id = sid, avg_info_admin = NA_real_, 
                              max_pool_info = NA_real_, ratio = NA_real_)
    next
  }
  ### pool info at theta
  pool_infos = item_info(theta_final, pool_b$a, pool_b$b)
  max_pool_info = max(pool_infos, na.rm = TRUE)
  ### admin items for this session
  admin_row = admin_by_session %>% filter(session_id == sid)
  if(nrow(admin_row) == 0) {
    avg_admin_info = NA_real_
    ratio = NA_real_
  } else {
    admin_bs = unlist(admin_row$admin_b)
    ### remove NA bs if any
    admin_bs = admin_bs[!is.na(admin_bs)]
    if(length(admin_bs) == 0) {
      avg_admin_info = NA_real_
      ratio = NA_real_
    } else {
      admin_infos = item_info(theta_final, 1, admin_bs)
      avg_admin_info = mean(admin_infos, na.rm = TRUE)
      ratio = ifelse(max_pool_info == 0, NA_real_, 
                     avg_admin_info / max_pool_info)
    }
  }
  info_comp[[sid]] = tibble(session_id = sid,
                            avg_info_admin = avg_admin_info, 
                            max_pool_info = max_pool_info, ratio = ratio)
}

info_comp_df = bind_rows(info_comp)
write.csv(info_comp_df, file.path(out_dir, "info_admin_vs_pool_by_session.csv"),
          row.names = FALSE)

p_info_ratio = ggplot(info_comp_df, aes(x = ratio)) +
  geom_histogram(bins = 40, na.rm = TRUE) +
  labs(title = "Distribution: avg administered item info / max pool info",
       x = "avg_info_admin / max_pool_info", y = "count") + theme_minimal()
ggsave(file.path(out_dir, "info_ratio_hist.png"), 
       p_info_ratio, width = 7, height = 5, dpi = 150)

#----------------------------------------------------------------------


#----------------------------------------------------------------------
# 6. Efficiency summaries per attempt

### Final theta/se and number of items + time taken
final_by_session = wide %>%
  mutate(session_id = if("session_id" %in% names(wide)) wide$session_id 
         else paste0(user_id, "_A", attempt_number)) %>%
  select(session_id, user_id, attempt_number, f1, se_f1) %>%
  left_join(session_b %>% select(session_id, n_items, first_time, last_time), 
            by = "session_id") %>%
  mutate(time_secs = as.numeric(difftime(last_time, first_time, units = "secs")))


final_by_session = final_by_session %>%
  left_join(admin_by_session %>% select(session_id, n_items) ,
            by = "session_id", suffix = c("", ".admin")) %>%
  mutate(n_items = ifelse(is.na(n_items), n_items.admin, n_items)) %>%
  select(-n_items.admin)

write.csv(final_by_session, file.path(out_dir, 
                                      "efficiency_summary_per_attempt.csv"), 
          row.names = FALSE)

### Plotting
ggsave(file.path(out_dir, "hist_n_items.png"),
       ggplot(final_by_session, aes(x = n_items)) +
         geom_histogram(bins = 40) + 
         labs(title = "Distribution: Number of items"),
       width = 6, height = 4, dpi = 150)

if("se_f1" %in% names(final_by_session)) {
  ggsave(file.path(out_dir, "hist_final_SE.png"),
         ggplot(final_by_session, aes(x = se_f1)) +
           geom_histogram(bins = 40) +
           labs(title = "Distribution: Final SE"),
         width = 6, height = 4, dpi = 150)
}
ggsave(file.path(out_dir, "hist_time_secs.png"),
       ggplot(final_by_session, aes(x = time_secs)) +
         geom_histogram(bins = 40) + 
         labs(title = "Distribution: Time taken (secs)"),
       width = 6, height = 4, dpi = 150)


if("se_f1" %in% names(final_by_session)) {
  ggsave(file.path(out_dir, "nitems_vs_finalSE.png"),
         ggplot(final_by_session, aes(x = n_items, y = se_f1)) + 
           geom_point(alpha = 0.4) + geom_smooth(method = "loess") +
           labs(title = "n_items vs final SE", x = "n_items", y = "final SE"),
         width = 7, height = 5, dpi = 150)
}

#----------------------------------------------------------------------

# 7. Item parameter evaluation: ICCs & info curves 
# build item-par grid for plotting
theta_grid = seq(-4, 4, length.out = 161)
item_plot_df = item_pool %>%
  filter(item_col %in% resp_item_cols) %>%
  rowwise() %>%
  mutate(prob = list(item_prob(theta_grid, a, b)),
         info = list(item_info(theta_grid, a, b))) %>%
  ungroup() %>%
  select(item_col, item_id, a, b, prob, info) %>%
  tidyr::unnest(cols = c(prob, info)) %>%
  group_by(item_col) %>%
  mutate(theta = rep(theta_grid, times = 1)) %>%
  ungroup()

### ICCs: sample top-k most administered for plotting
item_admin_counts = dat_long_items %>% group_by(item_id) %>%
  summarise(n_admin = n()) %>% arrange(desc(n_admin))
top_items = head(item_admin_counts$item_id, 12)
top_item_cols = paste0("it_", top_items)

for(it in top_item_cols) {
  dfp = item_plot_df %>% filter(item_col == it)
  if(nrow(dfp) == 0) next
  p_icc = ggplot(dfp, aes(x = theta, y = prob)) + 
    geom_line() +
    labs(title = paste0("ICC: ", it), 
         x = "Theta",
         y = "P(X=1)") + theme_minimal()
  ggsave(file.path(out_dir, paste0("ICC_", it, ".png")),
         p_icc, width = 6, height = 4, dpi = 150)
  p_info = ggplot(dfp, aes(x = theta, y = info)) +
    geom_line() +
    labs(title = paste0("Item info: ", it), 
         x = "Theta", y = "Information") + 
    theme_minimal()
  ggsave(file.path(out_dir, paste0("ItemInfo_", it, ".png")), 
         p_info, width = 6, height = 4, dpi = 150)
}

### Pool difficulty histogram
ggsave(file.path(out_dir, "pool_difficulty_hist.png"),
       ggplot(item_pool, aes(x = b)) +
         geom_histogram(bins = 50) + 
         labs(title = "Pool difficulty distribution (b)"),
       width = 7, height = 5, dpi = 150)

#----------------------------------------------------------------------


#----------------------------------------------------------------------
# 8. Item exposure & Gini 
item_exposure = dat_long_items %>%
  group_by(item_id) %>%
  summarise(n_administered = n(),
            unique_attempts = n_distinct(session_id),
            unique_examinees = n_distinct(user_id)) %>%
  ungroup() %>%
  mutate(exposure_rate_attempts = n_administered / n_distinct(dat_long_items$session_id),
         exposure_rate_examinees = unique_examinees / n_distinct(dat_long_items$user_id)) %>%
  arrange(desc(n_administered))

write.csv(item_exposure, file.path(out_dir, "item_exposure_summary.csv"), 
          row.names = FALSE)

gini_attempts = DescTools::Gini(item_exposure$exposure_rate_attempts)
gini_examinees = DescTools::Gini(item_exposure$exposure_rate_examinees)

cat("Gini exposure (by attempts) =", round(gini_attempts,4), 
    " ; (by examinees) =", round(gini_examinees,4), "\n")

# exposure histogram and Lorenz-like plot
ggsave(file.path(out_dir, "exposure_hist_attempts.png"),
       ggplot(item_exposure, aes(x = exposure_rate_attempts)) + 
         geom_histogram(bins = 50) + 
         labs(title = "Exposure rate (attempts)"),
       width = 7, height = 5, dpi = 150)

# Lorenz curve for exposure rates
exposure_sorted = sort(item_exposure$exposure_rate_attempts)
cum_exposure = cumsum(exposure_sorted) / sum(exposure_sorted)
cum_items = seq_along(exposure_sorted) / length(exposure_sorted)
lorenz_df = tibble(cum_items = c(0, cum_items), cum_exposure = c(0, cum_exposure))

p_lorenz = ggplot(lorenz_df, aes(x = cum_items, y = cum_exposure)) +
  geom_line() + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Lorenz Curve: exposure rate (attempts)",
       x = "Cumulative proportion of items", 
       y = "Cumulative exposure") + 
  theme_minimal()
ggsave(file.path(out_dir, "exposure_lorenz.png"), 
       p_lorenz, width = 7, height = 5, dpi = 150)


#----------------------------------------------------------------------



#----------------------------------------------------------------------
# 9. Growth analysis: repeated attempts per user
theta_long = wide %>%
  mutate(session_id = if("session_id" %in% names(wide)) wide$session_id 
         else paste0(user_id, "_A", attempt_number)) %>%
  select(user_id, attempt_number, session_id, f1, se_f1) %>%
  arrange(user_id, attempt_number)

### Keeping users with >=2 attempts
user_counts = theta_long %>% group_by(user_id) %>% 
  summarise(n_attempts = n()) %>% filter(n_attempts >= 2)
multi_users = theta_long %>% filter(user_id %in% user_counts$user_id)

### Sample up to 60 users for spaghetti plot
set.seed(123)
sample_users = sample(unique(multi_users$user_id), 
                      min(60, length(unique(multi_users$user_id))))
p_growth = ggplot(multi_users %>% filter(user_id %in% sample_users), 
                  aes(x = attempt_number, y = f1, group = user_id)) +
  geom_line(alpha = 0.6) + 
  geom_point(size = 1) +
  labs(title = "Theta trajectories (sample of users)",
       x = "Attempt number", y = "Theta (F1)") + 
  theme_minimal()
ggsave(file.path(out_dir, "theta_growth_sample.png"), 
       p_growth, width = 8, height = 6, dpi = 150)


change_summary = multi_users %>%
  group_by(user_id) %>%
  arrange(attempt_number) %>%
  summarise(first_theta = first(f1), last_theta = last(f1), 
            delta = last_theta - first_theta, 
            n_attempts = n()) %>% 
  ungroup()
write.csv(change_summary, file.path(out_dir, 
                                    "theta_growth_summary_by_user.csv"), 
          row.names = FALSE)


#----------------------------------------------------------------------



#----------------------------------------------------------------------
# 10. Save key outputs
write.csv(target_df, file.path(out_dir, "targeting_session_level.csv"), 
          row.names = FALSE)
write.csv(final_by_session, file.path(out_dir, "final_session_summary.csv"), 
          row.names = FALSE)
write.csv(item_pool, file.path(out_dir, "item_pool_table.csv"), 
          row.names = FALSE)

#----------------------------------------------------------------------