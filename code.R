# Load necessary packages

library(tidyverse)
library(haven)
library(glmnet)
library(grf)
library(dplyr)
library(data.table) 
library(broom)
library(gglasso)
library(sjlabelled)
library(labelled)
library(caret)
library(leaps)
library(stargazer)
library(ggpubr)
library(lmtest)
library(sandwich)
extrafont::loadfonts(device="all")


### Helper functions for later 
# This one extracts the base names from variables that are typically formatted
# if passed as a factor. For example, factor.industry.5 becomes industry.

get_base_name <- function(name) {
  name <- gsub("[0-9]+$", "", name)
  name <- gsub("\\.", " ", name)
  name <- gsub("factor", "", name)
  name <- trimws(name)
  return(name)
}

# Calculates and groups variable importance for factor variables and also assigns
# the variable names to the coefficients, as otherwise the output of var_importance
# is just a vector.
var_importance_table <- function(forest, column_names) {
  var_importance_raw <- variable_importance(forest)
  importance_df <- data.frame(
    variable = column_names,
    importance = as.vector(var_importance_raw)
  )
  importance_df$base_variable <- sapply(importance_df$variable, get_base_name)
  grouped_importance <- importance_df %>%
    group_by(base_variable) %>%
    summarise(total_importance = sum(importance), .groups = "drop") %>%
    arrange(desc(total_importance))
  return(grouped_importance)
}



# Set working directory.
wd <- "YOUR_PATH"
setwd(wd)

# You should have a folder "data" with all SHP files (household and individual) 
# and the World Bank data called "inflation_rates_worldbank.csv".

### Data preparation

# Setting up the years and variables to then combine the annual personal and 
# annual household data.

year <- c("00","01","02", "03", "04", "05", "06", "07", "08", "09", 10:23)

# These are the names that the variables will have after the combination.
setnames_variables_p <- c("personal_id", "interview_type", "househ_id", "sex", "age", "nationality", "civstat",
                          "isced", "educyrs", "annual_income", "conWH", "occupation_isco",
                          "industry", "part_time", "supervisory_role", "health_status", "first_lang",
                          "firm_size","public_private","n_children","dep_freq")

setnames_variables_h <- c("househ_id", "househ_type", "new_born")


list_of_p_dfs <- lapply(year, function(y) {
  file_path <- paste0("data/shp", y, "_p_user.dta")
  df_p <- read_dta(file_path)
  
  # Define old variable names for the current year to dynamically extract them.
  select_variables_p <- c("idpers",  paste0("status",y), paste0("idhous", y), 
                          paste0("sex", y), paste0("age", y),
                          paste0("reg_1_", y), paste0("civsta", y), 
                          paste0("isced", y), paste0("edyear", y),
                          paste0("i", y, "wyg"), paste0("p", y, "w74"), 
                          paste0("is1maj", y),paste0("noga2m", y),
                          paste0("p", y, "w39"), paste0("p", y, "w87"),
                          paste0("p", y, "c01"), paste0("p", y, "e16"),
                          paste0("p",y,"w85"),paste0("p",y,"w32"),
                          paste0("ownkid",y),paste0("p", y, "c17"))
  
  # Select the specified variables
  df_p <- df_p[, select_variables_p]
  
  # Rename the variables
  setnames(df_p, old = select_variables_p, new = setnames_variables_p)
  
  # Filter for in-person interviews, clean, and add the year column
  if (y != "99"){
  df_p <- df_p %>%
    filter(interview_type == 0) %>%
    select(-interview_type) %>%
    mutate(year = as.numeric(paste0("20", y)))}
  return(df_p)
})

# Combine all the yearly data frames from the list
data_p <- bind_rows(list_of_p_dfs)

# Save the combined personal dataset
saveRDS(data_p, "data/data_p.rds")



# The same procedure for the household data
list_of_h_dfs <- lapply(year, function(y) {
  file_path <- paste0("data/shp", y, "_h_user.dta")
  df_h <- read_dta(file_path)
  
  select_variables_h <- c(paste0("idhous", y), paste0("hldcen", y), paste0("nbb_", y))
  
  df_h <- df_h[, select_variables_h]
  setnames(df_h, old = select_variables_h, new = setnames_variables_h)
  
  if (y != "99"){df_h$year <- as.numeric(paste0("20", y))}
  return(df_h)
})

# Combine the list of yearly household data frames
data_h <- bind_rows(list_of_h_dfs)
saveRDS(data_h, "data/data_h.rds")


# Merge the two data frames by household id and year
dat <- merge(data_p, data_h, by = c("househ_id", "year"))

# Save the final raw dataset 
saveRDS(dat, "data/raw_data.rds")

raw_dat <- readRDS("data/raw_data.rds")






# Keep the relevant variables and calculate a few like hourly_wage naively and
# its log. Also, remove negative values in the
# relevant variables (which are generally NA in the SHP).



dat <- raw_dat %>%
  filter(annual_income > 0, conWH > 0, age > 0, age <= 70) %>%
  mutate(hourly_wage = annual_income / (conWH * 52),
         log_hourly_wage = log(hourly_wage),
         age2 = age^2)%>% 
  filter(!if_any(c(isced,educyrs, part_time,occupation_isco, civstat, nationality, industry, n_children,public_private,firm_size), ~ .x < 0)) %>%
  select(c(personal_id, educyrs,househ_id,year, age,age2,log_hourly_wage, sex, isced, hourly_wage, part_time,occupation_isco, civstat, nationality, industry, n_children,public_private,firm_size)) %>%
  na.omit()


# Recode some relevant variables like nationality, isced and civil status.

dat <- dat %>%
  mutate(nationality = case_when(
    nationality == 10 ~ 0,
    nationality %in% c(11:17, 31) ~ 1,
    nationality %in% c(20, 30, 40, 50, 60) ~ 2,
  )) %>%
  set_value_labels(nationality = c("Switzerland" = 0, "Europe or North America" = 1, "Rest of the world" = 2)) %>%
  set_variable_labels(nationality = "First nationality") 


dat <- dat %>%
  mutate(isced = case_when(
    isced == 10 ~ 0,
    isced == 20 ~ 1,
    isced == 31 ~ 2,
    isced == 32 ~ 3,
    isced == 33 ~ 4,
    isced == 41 ~ 5,
    isced %in% c(51, 52, 60) ~ 6,
  )) %>%
  set_value_labels(isced = c(
    "Primary or first stage of basic education" = 0,
    "Lower secondary or Second stage of basic education" = 1,
    "Upper secondary education (preparation for tertiary education" = 2,
    "Upper secondary education (preparation for further prof. education)" = 3,
    "Upper secondary education (entrance into the labor market)" = 4,
    "Post-secondary education non tertiary (preparation for an institution for higher education)" = 5,
    "First or second stage of tertiary education" = 6
  )) %>%
  set_variable_labels(isced = "Highest level of education")


dat <- dat %>%
  mutate(civstat = case_when(
    civstat %in% c(1, 3, 4, 5, 7) ~ 0,
    civstat %in% c(2, 6) ~ 1,
  )) %>%
  set_value_labels(civstat = c("Single" = 0, "Married or registered partnership" = 1)) %>%
  set_variable_labels(civstat = "Martial status")


dat <- dat %>%
  filter( sex != 3)%>%
  mutate(sex = case_when(
    sex == 1 ~ 0,
    sex == 2 ~ 1
  )) %>%
  set_value_labels(sex = c("Male" = 0, "Female" = 1)) %>%
  set_variable_labels(sex = "Sex")


dat <- dat %>%
  mutate(firm_size = case_when(
    firm_size %in% c(1:2) ~ 1,
    firm_size %in% c(3:5) ~ 2,
    firm_size %in% c(6:7) ~ 3,
    firm_size %in% c(8:9) ~ 4,
    TRUE ~ firm_size
  )) %>%
  set_value_labels(firm_size = c("1-19" = 1, "20-99" = 2, "100-499" = 3, "500+" = 4)) %>%
  set_variable_labels(firm_size = "Number of employees")


# Mutate all factor variables to factors.
dat <- dat %>%
  mutate(across(c(isced,sex, part_time,occupation_isco, civstat, nationality, industry,  public_private, firm_size, n_children),as.factor)) 


# Load inflation data from the world bank.
inflation_rates_worldbank <- read_csv("data/inflation_rates_worldbank.csv", skip = 3)
ch_inflation_data <- inflation_rates_worldbank %>% 
  filter(`Country Name` == "Switzerland")
remove(inflation_rates_worldbank)
ch_inflation_data <-  head(data.frame(t(ch_inflation_data)[-c(1:4),]),-2)
ch_inflation_data$year <- rownames(ch_inflation_data)
ch_inflation_data[,1] <- as.numeric(ch_inflation_data[,1])/100
colnames(ch_inflation_data) <- c("inflation_rate_dec","year")
rownames(ch_inflation_data) <- c(1:length(ch_inflation_data$year))
ch_inflation_data <- ch_inflation_data[which(ch_inflation_data$year %in% c(2000:2022)),] 

cpi_adjust <- data.frame(year = 2000, Value = 1)

# Calculating real hourly wages.
for (i in seq_len(nrow(ch_inflation_data))){
  cpi_adjust <- rbind(cpi_adjust,
                      data.frame(year= 2000+i, Value = cpi_adjust[i,2] / (1 + ch_inflation_data$inflation_rate_dec[i] )))
}

cpi_2023 <- cpi_adjust %>% filter(year == 2023) %>% pull(Value)
dat$Value <- NULL

dat <- dat %>%
  left_join(cpi_adjust, by = "year") %>%
  mutate(
    hourly_wage_adjusted = (hourly_wage / cpi_2023) * Value)

dat <- dat[dat$hourly_wage_adjusted < quantile(dat$hourly_wage_adjusted,0.999),]



# Create a data frame that contains the data I want to plot.

annual_mean_adjusted_salary <- dat %>%
  group_by(year,sex) %>%
  summarize(mean_adjusted_salary = mean(hourly_wage_adjusted),.groups = 'drop') %>%
  mutate(sex = factor(sex, levels = c(0,1),labels =c("Male","Female")))

# Generate and save the plot for mean adjusted salaries for both sexes. 

dir.create("plots",  recursive = TRUE, showWarnings = FALSE)


mean_salary_plot <- ggplot(annual_mean_adjusted_salary, aes(x = year, y = mean_adjusted_salary, color = sex, fill = sex)) +
  geom_line() +
  geom_point() +
  labs(
    title = "",
    x = "",
    y = "Mean adjusted hourly wage",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "CMU Serif"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          legend.position = "bottom")

ggsave(filename = "plots/mean_salary_plot.png", plot = mean_salary_plot, width = 8, height = 5, dpi = 800)                             


# Pipeline to generate clean-cut summary statistics for the defined variables
# for certain years.

dir.create("tables", recursive = TRUE, showWarnings = FALSE)


cols_to_summarise <- c("age","sex","educyrs")


dat_summary_stats <- dat[dat$year %in% c(2000:2023)[seq(1,24,4)],] %>%
  group_by(year) %>%
  summarise(
    n = n(),  
    across(
      all_of(cols_to_summarise),
      list(
        mean = ~mean(as.numeric(as.vector(.x)), na.rm = TRUE),
        sd = ~sd(as.numeric(as.vector(.x)), na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  mutate(across(ends_with("_mean"), 
                ~paste0(round(.x, 2), " (", round(get(sub("_mean$", "_sd", cur_column())), 2),")"),
                .names = "{.col}_plus_minus_sd")) %>%
  select(year, n, ends_with("plus_minus_sd"))

colnames(dat_summary_stats)<- c("Year","N","Age","Sex","Years of education")


# Use stargazer to export the generated data frame.
stargazer(dat_summary_stats, summary = FALSE, rownames = FALSE, type = "latex",out ="tables/summary_stats.tex", float= FALSE)



# Initialized lists to save the data from the loop.

forests <- list()
ols_results <- list()
ols_results_se <- list()
var_importance_results <- list()
years <- 2000:2023

# Loop that runs causal forest and OLS for each year separately.
for (y in years){

  
  # Create temporary data frame that only contains the current years data.
  dat_curr_year <- dat[dat$year == y,]
  
  # Controls for the OLS regression.
  selected_controls <- c("age","age2","educyrs","factor(part_time)", "factor(public_private)","factor(occupation_isco)", "factor(firm_size)","factor(n_children)","factor(industry)", "factor(nationality)","factor(civstat)")
  
  # Run the OLS regression and HC0 heteroscedasticity consistent SEs.
  ols_model <- lm(as.formula(paste("log_hourly_wage ~ factor(sex) +" ,paste(selected_controls, collapse = " + "))), data = dat_curr_year)
  ols_se <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC0"))[,2]
  
  # Numerical features for the causal forest run.
  selected_controls_cf <- c("age","age2","educyrs")
  
  # One hot encode the factor variables.
  public_private  <- model.matrix(~ factor(public_private) -1, data = dat_curr_year)
  firm_size  <- model.matrix(~ factor(firm_size) -1, data = dat_curr_year)
  industry  <- model.matrix(~ factor(industry) -1, data = dat_curr_year)
  nationality  <- model.matrix(~ factor(nationality) -1, data = dat_curr_year)
  civstat  <- model.matrix(~ factor(civstat) -1, data = dat_curr_year)
  n_children <- model.matrix(~ factor(n_children) -1, data = dat_curr_year)
  occupation_isco <- model.matrix(~ factor(occupation_isco) -1, data = dat_curr_year)
  part_time <- model.matrix(~ factor(part_time) -1, data = dat_curr_year)
  
  # Define X,Y and W for the causal forest.
  X <- data.matrix(data.frame(dat_curr_year[,selected_controls_cf],
                              public_private, firm_size,industry,
                              nationality,civstat,n_children,
                              occupation_isco,part_time))
  Y <- as.vector(dat_curr_year$log_hourly_wage)
  W <- as.numeric(as.vector(dat_curr_year$sex))
  
  # Run the causal forest with one tuning parameter and 2000 trees, and try every single
  # variable for splitting (mtry = ncol(X)) for potentially improved variable importance
  # stability.
  cf <- causal_forest(X,Y,W, tune.parameters = "min.node.size", num.trees = 2000, mtry = ncol(X))
  
  # Use the helper function from the beginning to get a data frame of the aggregated importance per variable.
  var_importance_table(cf,colnames(X))
  
  
  # Save all the generated results in the corresponding list and print a progression message.
  forests[[paste0("forest_",y)]] <- cf
  ols_results[[paste0("ols_model_",y)]] <- ols_model
  var_importance_results[[paste0("var_importance_",y)]] <- var_importance_table(cf,colnames(X))
  ols_results_se[[paste0("ols_se_",y)]] <- ols_se
  print(paste0("Done with year: ", y,"."))
}


# Save the results if I want to work from here on out without running the entire
# loop again.

saveRDS(forests,file = "forests.rds")
saveRDS(ols_results,file = "ols_results.rds")
saveRDS(ols_results_se,file = "ols_results_se.rds")
saveRDS(var_importance_results,file = "var_importance_results.rds")



### Analysis part


# Helper function for the table with the results from the causal forests.
add_stars <- function(coef, se, cutoffs = c(0.1, 0.05, 0.01)) {
  z <- abs(coef / se)
  p <- 2 * (1 - pnorm(z))  
  
  stars <- ifelse(p < cutoffs[3], "THREESTAR",
                  ifelse(p < cutoffs[2], "TWOSTAR",
                         ifelse(p < cutoffs[1], "ONESTAR", "")))
  
  return(sprintf("%.3f%s", coef, stars))
}


# Generate the  clean-cut result tables for the causal forests (most likely not too efficiently).

ates <- data.frame(placeholder_column = NA)
ses <- data.frame(placeholder_column = NA)


for (y in years){
  cf <- forests[[paste0("forest_",y)]]
  cf_ate <- average_treatment_effect(cf)
  ates <- cbind(ates, add_stars(cf_ate["estimate"],cf_ate["std.err"]))
  ses <-  cbind(ses,sprintf("(%.5f)",round(cf_ate[2],5)))
}

ates_ses <- transpose(na.omit(data.frame(transpose(ates),
                                         transpose(ses))))[seq(1,24,3)]
colnames(ates_ses) <- c(2000:2023)[seq(1,24,3)]
rownames(ates_ses) <- c("Female","")
stargazer(ates_ses, type = "latex", notes = "*p $<$ 0.1; **p $<$ 0.05; ***p $<$ 0.01",out = "tables/ates_ses.tex",float = FALSE, summary=FALSE)

lines <- readLines("tables/ates_ses.tex")
lines_fixed <- gsub("THREESTAR", "^{***}", lines)
lines_fixed <- gsub("TWOSTAR", "^{**}", lines_fixed)
lines_fixed <- gsub("ONESTAR", "^{*}", lines_fixed)
writeLines(lines_fixed, "tables/ates_ses.tex")

stargazer(ols_results[seq(1,24,3)],
          keep = "sex",
          se = ols_results_se[seq(1,24,3)],
          title = "",
          header = FALSE,
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          covariate.labels = "Sex",
          notes.label = "",
          dep.var.caption = "",
          out="tables/ols_results.tex",
          digits = 3,
          column.labels = as.character(c(2000:2023)[seq(1,24,3)]),
          model.numbers =FALSE,
          omit.stat = c("f","ser"),          
          float = FALSE,
          type = "latex")


# Generate a clean-cut table for the variable importance.

merged_var_importance_table <- Reduce(function(x, y) merge(x,y, by ="base_variable"), var_importance_results)

colnames(merged_var_importance_table) <- c("Variable",c(2000:2023))


merged_var_importance_table <- merged_var_importance_table %>%
  mutate(Variable = case_when(
    Variable == "n_children" ~ "No. of children",
    Variable == "age" ~ "Age",
    Variable == "public_private" ~ "Public/private",
    Variable == "educyrs" ~ "Yrs. of education",
    Variable == "civstat" ~ "Civil status",
    Variable == "firm_size" ~ "Firm size",
    Variable == "industry" ~ "Industry type",
    Variable == "nationality" ~ "Nationality",
    Variable == "part_time" ~ "Part time",
    Variable == "occupation_isco" ~ "Occupation"
  ))

merged_var_importance_table <- merged_var_importance_table[order(rowSums(
  merged_var_importance_table[,-1], na.rm = TRUE), decreasing = TRUE),][1:8,]

merged_var_importance_table

# Save the numerical version (not in paper).
stargazer(merged_var_importance_table, summary = FALSE, rownames = FALSE, type = "latex",out ="tables/var_importance.tex", float= FALSE)


merged_var_importance_table_long <- merged_var_importance_table %>%
  pivot_longer(
    cols = !Variable,
    names_to = "Year",
    values_to = "Value"
  )

merged_var_importance_table_long$Year <- as.numeric(merged_var_importance_table_long$Year)


# Generate the visual one. 
var_importance_plot <- ggplot(merged_var_importance_table_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(
    title = "",
    x = "Year",
    y = "Variable importance",
    color = "Variable"
  ) +
  theme_minimal() + theme(text = element_text(size = 18, family = "CMU Serif"),
           axis.title = element_text(size = 18),
           axis.text = element_text(size = 16),
           axis.title.x = element_text(margin = margin(t = 10)),
           axis.title.y = element_text(margin = margin(r = 10)),
           legend.position = "bottom",legend.text=element_text(size=10), legend.title = element_text(size=12))

ggsave(filename = "plots/var_importance_plot.png", plot = var_importance_plot, width = 8, height = 5, dpi = 800)                             


# Plot cf results 

plot_data <- data.frame()

# Generate mean treatment effects relative to mean age for certain years. 


# Helper data frame
for (year in c(2000,2012,2023)) {
  forest_model <- forests[[paste0("forest_", year)]]
  predictions <- predict(forest_model)$predictions
  ages <- dat[dat$year == year, "age"]
  
  temp_data <- data.frame(
    predictions = predictions,
    age = ages,
    year = rep(year, length(ages))
  )
  
  plot_data <- rbind(plot_data, temp_data)
}





colnames(plot_data) <- c("ite", "age","year")


mean_plot_data <- plot_data %>%
  group_by(age,year) %>%
  summarise(mean_ite = mean(ite, na.rm = TRUE), .groups = "drop")


# Generate plot
mean_age_plot <- ggplot(plot_data, aes(x = age, y = ite)) +
  geom_point(alpha = 0.25, color = "gray") +
  geom_line(data = mean_plot_data,
            aes(x = age, y = mean_ite, color = factor(year), group = year),
            linewidth = 1) +
  geom_point(data = mean_plot_data, aes(x = age, y = mean_ite,  
                 color = factor(year), group = year),
             size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "",
       x = "Age",
       y = "Mean gender gap (log points)",
       color = "Year") +
  theme_minimal() + theme(text = element_text(size = 18, family = "CMU Serif"),
                          axis.title = element_text(size = 18),
                          axis.text = element_text(size = 16),
                          axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          legend.position = "bottom")

# Save plot
ggsave(filename = "plots/mean_age_plot.png", plot = mean_age_plot, width = 8, height = 5, dpi = 800)                             


# Generate mean treatment effects relative to mean number of children for certain years. 

plot_data <- data.frame()

for (year in c(2000,2012,2023)) {
  forest_model <- forests[[paste0("forest_", year)]]
  predictions <- predict(forest_model)$predictions
  n_children_y <- dat[dat$year == year, "n_children"]
  
  temp_data <- data.frame(
    predictions = predictions,
    n_children = n_children_y,
    year = rep(year, length(n_children_y))
  )
  
  plot_data <- rbind(plot_data, temp_data)
}


colnames(plot_data) <- c("ite", "n_children","year")

mean_plot_data <- plot_data %>%
  group_by(n_children,year) %>%
  summarise(mean_ite = mean(ite, na.rm = TRUE), .groups = "drop")


mean_children_plot <- ggplot(plot_data, aes(x = n_children, y = ite)) +
  geom_point(alpha = 0.25, color = "gray") +
  geom_line(data = mean_plot_data,
            aes(x = n_children, y = mean_ite, color = factor(year), group = year),
            linewidth = 1) +
  geom_point(data = mean_plot_data, aes(x = n_children, y = mean_ite,  
                 color = factor(year), group = year),
             size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "",
       x = "No. of children",
       y = "Mean gender gap (log points)",
       color = "Year") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "CMU Serif"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom")

ggsave(filename = "plots/mean_children_plot.png", plot = mean_children_plot, width = 8, height = 5, dpi = 800)                             



