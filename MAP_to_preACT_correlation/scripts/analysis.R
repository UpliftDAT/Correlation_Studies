#===============================================================================
# author:       Victor Faner
# date:         2020-05-29
# description:  Analyze relationship between Spring MAP performance 
#               and first-time EOC performance.
#
# code re-used on 2020-08-11 for Spring MAP to PreACT analysis.
#===============================================================================

library(tidyverse)
library(broom)
library(here)
library(RODBC)

channel <- odbcDriverConnect("connect_to_your_db")

dat <- sqlQuery(channel, read_file(here("query_to_get_map_and_pACT_scores.sql")))
dat9 <- dat[ which(dat$preact_grade_level==9), ]
dat10 <- dat[ which(dat$preact_grade_level==10), ]

close(channel)

#===============================================================================
# Calculate correlation coefficients for each subject by grade level
#===============================================================================

# Create folders for output data and figures
dir.create(here("output"), showWarnings = F)
dir.create(here("figs"), showWarnings = F)

# Generate correlation point estimates, p-values, confidence intervals
dat9 %>% 
  drop_na() %>%  # remove NULL test scores
  group_by(preact_subject) %>%
  summarize(r       = cor(preact_scale_score, map_percentile),
            p_val   = cor.test(preact_scale_score, map_percentile)$p.value,
            r_lower = cor.test(preact_scale_score, map_percentile)$conf.int[1],
            r_upper = cor.test(preact_scale_score, map_percentile)$conf.int[2],
            n_tests = n()) %>% 
  write_csv(here("output/pearson9.csv"))

dat10 %>% 
  drop_na() %>%  # remove NULL test scores
  group_by(preact_subject) %>%
  summarize(r       = cor(preact_scale_score, map_percentile),
            p_val   = cor.test(preact_scale_score, map_percentile)$p.value,
            r_lower = cor.test(preact_scale_score, map_percentile)$conf.int[1],
            r_upper = cor.test(preact_scale_score, map_percentile)$conf.int[2],
            n_tests = n()) %>% 
  write_csv(here("output/pearson10.csv"))


# Based on Pearson results, drop Science -> Science and Reading -> Science from subsequent charts.
dat9 <- dat9[ which(dat9$preact_subject!="MAP Science to PreACT Science" & dat9$preact_subject!="MAP Reading to PreACT Science"), ]
dat10 <- dat10[ which(dat10$preact_subject!="MAP Science to PreACT Science" & dat10$preact_subject!="MAP Reading to PreACT Science"), ]

#===============================================================================
# Percentile to scale score comparison
#===============================================================================

# Helper function for pulling regression coefficients
lm_fit <- function(df) {
  lm(preact_scale_score ~ map_percentile, data = df) %>% 
    tidy()
}

# Helper function for pulling regression R-Squared and fitted p-values
lm_summary <- function(df) {
  lm(preact_scale_score ~ map_percentile, data = df) %>% 
    ##summary() %>% 
    glance()
}

# Run regressions on Percentile scores (9th Grade)
models <- dat9 %>%
  group_by(preact_subject) %>%
  
  # Run regressions on each EOC subject. The nest() and unnest() functions 
  # are needed to create new columns for each regression summary statistic
  nest() %>% 
  summarize(fit = map(data, lm_fit),
            summary = map(data, lm_summary)) %>%
  unnest(fit) %>% 
  
  # Remove unneeded columns
  select(-p.value, -statistic) %>% 
  
  # Pivot into a wide format 
  pivot_wider(id_cols = preact_subject,
              names_from = term,
              values_from = c(estimate, summary)) %>% 
  
  # Get additional regression summary statistic columns, remove any unneeded
  # fields
  select(-`summary_(Intercept)`) %>%
  unnest(summary_map_percentile) %>% 
  select(-r.squared, -sigma, -statistic, -df)

# Generate scatterplot with regression line of MAP on EOC
dat9 %>% 
  drop_na() %>% 
  inner_join(models) %>%
  
  # Define scatter plot
  ggplot(aes(x = map_percentile, y = preact_scale_score)) +
  
  geom_point() +
  labs(x = "MAP Percentile", y = "PreACT Scale Score") +
  
  # Create regression line
  geom_smooth(method = lm) +
  
  # Set all plots as perfect squares
  theme(aspect.ratio = 1) +
  
  # Add regression formula and Adjusted R-Squared
  geom_text(
    aes(x = 50,
        y = 40,
        label = paste("PreACT Scale Score = ",
                      round(`estimate_(Intercept)`, 2),
                      " + ",
                      round(estimate_map_percentile, 2),
                      "(MAP Percentile) \n",
                      " Pearson's R = ",
                      round(sqrt(adj.r.squared), 2))),
    size = 2
  ) +
  
  # Set Y axes limits.
  coord_cartesian(ylim=c(0, 40)) +
  
  # Break out into separate plots by EOC Subject
  facet_grid(cols = vars(preact_subject)) + 
  
  # Save to file
  ggsave(here("figs/percentile_scatterplots9.png"), width = 12.6, height = 4)

# Run regressions on Percentile scores
models <- dat10 %>%
  group_by(preact_subject) %>%
  
  # Run regressions on each EOC subject. The nest() and unnest() functions 
  # are needed to create new columns for each regression summary statistic
  nest() %>% 
  summarize(fit = map(data, lm_fit),
            summary = map(data, lm_summary)) %>%
  unnest(fit) %>% 
  
  # Remove unneeded columns
  select(-p.value, -statistic) %>% 
  
  # Pivot into a wide format 
  pivot_wider(id_cols = preact_subject,
              names_from = term,
              values_from = c(estimate, summary)) %>% 
  
  # Get additional regression summary statistic columns, remove any unneeded
  # fields
  select(-`summary_(Intercept)`) %>%
  unnest(summary_map_percentile) %>% 
  select(-r.squared, -sigma, -statistic, -df)

# Generate scatterplot with regression line of MAP on EOC
dat10 %>% 
  drop_na() %>% 
  inner_join(models) %>%
  
  # Define scatter plot
  ggplot(aes(x = map_percentile, y = preact_scale_score)) +
  
  geom_point() +
  labs(x = "MAP Percentile", y = "PreACT Scale Score") +
  
  # Create regression line
  geom_smooth(method = lm) +
  
  # Set all plots as perfect squares
  theme(aspect.ratio = 1) +
  
  # Add regression formula and Adjusted R-Squared
  geom_text(
    aes(x = 50,
        y = 40,
        label = paste("PreACT Scale Score = ",
                      round(`estimate_(Intercept)`, 2),
                      " + ",
                      round(estimate_map_percentile, 2),
                      "(MAP Percentile) \n",
                      " Pearson's R = ",
                      round(sqrt(adj.r.squared), 2))),
    size = 2
  ) +
  
  # Set Y axes limits.
  coord_cartesian(ylim=c(0, 40)) +  
  
  # Break out into separate plots by EOC Subject
  facet_grid(cols = vars(preact_subject)) + 
  
  # Save to file
  ggsave(here("figs/percentile_scatterplots10.png"), width = 12.6, height = 4)

#===============================================================================
# MAP Quintile to PreACT College Readiness comparison
#===============================================================================

# MAP Quintile/PreACT College Readiness crosstab (9th)
dat9 %>%
  
  # Transform EOC performance levels
  mutate(
    
    # Map performance ordinals onto actual buckets. For the sake of simpler
    # visualization this combines Does Not Meet and Approaches into the same 
    # bucket
    preact_college_ready_indicator = 
      case_when(preact_college_ready_indicator == 0 ~ "Not College Ready",
                preact_college_ready_indicator == 1 ~ "College Ready"),
    
    # Re-order factor levels for plotting purposes later on
    preact_college_ready_indicator = fct_relevel(preact_college_ready_indicator, 
                                        levels = c("Not College Ready", 
                                                   "College Ready"))
  ) %>%   
  
  # Calculate number of scholars within each MAP quintile/PreACT College Readiness
  # combination
  group_by(preact_subject, map_quintile, preact_college_ready_indicator) %>% 
  summarize(n = n()) %>%
  
  # For each MAP quintile, calculate the percent of scholars in that quintile
  # who fell into each of the corresponding EOC performance levels
  group_by(preact_subject, map_quintile) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  # Create stacked bar charts
  ggplot(aes(fill = preact_college_ready_indicator, x = map_quintile, y = pct)) +
  
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "MAP Quintile", y = "Percentage") +
  
  # Set all plots as perfect squares
  theme(aspect.ratio = 1) +
  
  # Convert y-axis values from decimals to percentages
  scale_y_continuous(labels = scales::percent) +
  
  # Manually define stacked bar color schemes:
  # red = Does Not Meet/Approaches, blue = Meets, green = Masters
  scale_fill_viridis_d(alpha = .5) +
  
  # Print text percentages on each stacked bar
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  
  # Break out into separate plots by EOC Subject
  facet_grid(cols = vars(preact_subject)) +
  
  # Save to file
  ggsave(here("figs/map_quintile_preact_bucket_bar_charts9.png"), width = 12.6, 
         height = 4)


# MAP Quintile/PreACT College Readiness crosstab
dat10 %>%
  
  # Transform EOC performance levels
  mutate(
    
    # Map performance ordinals onto actual buckets. For the sake of simpler
    # visualization this combines Does Not Meet and Approaches into the same 
    # bucket
    preact_college_ready_indicator = 
      case_when(preact_college_ready_indicator == 0 ~ "Not College Ready",
                preact_college_ready_indicator == 1 ~ "College Ready"),
    
    # Re-order factor levels for plotting purposes later on
    preact_college_ready_indicator = fct_relevel(preact_college_ready_indicator, 
                                                 levels = c("Not College Ready", 
                                                            "College Ready"))
  ) %>%   
  
  # Calculate number of scholars within each MAP quintile/PreACT College Readiness
  # combination
  group_by(preact_subject, map_quintile, preact_college_ready_indicator) %>% 
  summarize(n = n()) %>%
  
  # For each MAP quintile, calculate the percent of scholars in that quintile
  # who fell into each of the corresponding EOC performance levels
  group_by(preact_subject, map_quintile) %>% 
  mutate(pct = n / sum(n)) %>% 
  ungroup() %>% 
  
  # Create stacked bar charts
  ggplot(aes(fill = preact_college_ready_indicator, x = map_quintile, y = pct)) +
  
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "MAP Quintile", y = "Percentage") +
  
  # Set all plots as perfect squares
  theme(aspect.ratio = 1) +
  
  # Convert y-axis values from decimals to percentages
  scale_y_continuous(labels = scales::percent) +
  
  # Manually define stacked bar color schemes:
  # red = Does Not Meet/Approaches, blue = Meets, green = Masters
  scale_fill_viridis_d(alpha = .5) +
  
  # Print text percentages on each stacked bar
  geom_text(aes(label = scales::percent(pct, accuracy = 1)), 
            position = position_stack(vjust = 0.5),
            size = 3) +
  
  # Break out into separate plots by EOC Subject
  facet_grid(cols = vars(preact_subject)) +
  
  # Save to file
  ggsave(here("figs/map_quintile_preact_bucket_bar_charts10.png"), width = 12.6, 
         height = 4)