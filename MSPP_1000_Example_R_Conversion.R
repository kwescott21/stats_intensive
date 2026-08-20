###############################################################################
# MSPP 1000 Statistics Course Example File — R Version
#
# Converted from Stata (MSPP_1000_Example.do) to R.
# Uses the UCLA `hsbdemo` dataset (same dataset the original Stata script
# pulls from https://stats.idre.ucla.edu/stat/data/hsbdemo).
#
# Feel free to reuse any code/logic from this file, but perform your own
# independent analysis of the results.
###############################################################################

#install.packages(c("haven", "dplyr", "ggplot2", "moments", "broom",
#                     "car", "emmeans", "tidyr"))
#install.packages("curl")

library(haven)     # read Stata .dta files
library(dplyr)     # data manipulation
library(ggplot2)   # graphics
library(moments)   # skewness / kurtosis
library(broom)     # tidy model output
library(car)        # Anova() helper
library(emmeans)   # pairwise comparisons (margins pwcompare equivalent)
library(tidyr)     # pivot_longer / reshape
library(curl)       # download files from the web

url <- "https://stats.idre.ucla.edu/stat/data/hsbdemo.dta"

###############################################################################
# Day 1
###############################################################################

# Load data (`use ..., clear`). haven::read_dta returns labelled columns;
# as_factor() converts labelled numeric vars into character-like factors,
# equivalent to Stata's `decode`.
df <- read_dta(url)

# General data exploration
# `br` (browse) -> View(df) opens a spreadsheet viewer in RStudio
# View(df)

# `count`
nrow(df)

# `describe`
str(df)

# `summarize`
summary(df)

# `codebook read` / `codebook prog` / `codebook id` / `codebook awards`
for (col in c("read", "prog", "id", "awards")) {
  cat("---", col, "---\n")
  print(summary(df[[col]]))
  cat("class:", class(df[[col]]), "\n\n")
}

# `sum read`
summary(df$read)

# `sum read, detail`  (add skew/kurtosis + extra percentiles for the "detail" flavor)
quantile(df$read, probs = c(.01, .05, .1, .25, .5, .75, .9, .95, .99))
skewness(df$read)
kurtosis(df$read)

# `tabstat read, stats(n mean median min max)`
df %>%
  summarise(n = n(), mean = mean(read), median = median(read),
            min = min(read), max = max(read))

# Creating variables, generating string variable, and filtering data
# `tab prog`
table(as_factor(df$prog))

# `list prog in 1/10`
head(as_factor(df$prog), 10)

# `decode prog, generate(prog_str)`
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))

# `sum write if prog_str == "vocation"`
summary(df$write[df$prog_str == "vocation"])

# `sum write if prog_str == "vocation" & ses_str == "high"`
summary(df$write[df$prog_str == "vocation" & df$ses_str == "high"])

# Exploring different types of variables with graphics
# NOMINAL: `graph bar (count), over(prog) ...`
ggplot(df, aes(x = prog_str)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Nominal: Program Type", x = NULL, y = "Count") +
  theme_minimal()

# SES is ordinal in this dataset
# `tab ses` / `codebook ses`
table(as_factor(df$ses))
summary(as_factor(df$ses))

# Important: We can rank but CANNOT say gaps are equal
# Create an ordinal variable (low to very high) from `read`
df$read_ordinal <- cut(
  df$read,
  breaks = c(-Inf, 40, 50, 60, Inf),
  labels = c("Low", "Medium", "High", "Very High"),
  ordered_result = TRUE
)

# `tab read_ordinal`
table(df$read_ordinal)

# Can use median and percentiles (appropriate for ordinal)
# `sum read_ordinal, detail` -- median makes sense for an ordinal scale
codes <- as.integer(df$read_ordinal)
summary(codes)

# `tabstat read_ordinal, stats(n mean median min max)`
c(n = length(codes), mean = mean(codes), median = median(codes),
  min = min(codes), max = max(codes))

# Visualize ordinal data
# `graph hbar (count), over(read_ordinal) ...`
ggplot(df, aes(x = read_ordinal)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Ordinal: Reading Levels", x = NULL, y = "Count") +
  theme_minimal()

# INTERVAL: read scores (true interval)
# `histogram read, normal ...`
ggplot(df, aes(x = read)) +
  geom_histogram(bins = 15, color = "black", fill = "grey70") +
  stat_function(
    fun = function(x) dnorm(x, mean(df$read), sd(df$read)) *
      nrow(df) * (diff(range(df$read)) / 15),
    color = "red"
  ) +
  labs(title = "INTERVAL: Reading Scores", x = "Score (equal intervals)", y = "Frequency") +
  theme_minimal()

# Create a RATIO variable from the dataset (simulated age, 15-25)
set.seed(12345)
df$age_ratio <- 15 + runif(nrow(df)) * 10

# Compare interval (read) vs ratio (age)
summary(df$read)
cat("READ (INTERVAL): Mean =", mean(df$read), ", Min =", min(df$read), "\n")

summary(df$age_ratio)
cat("AGE (RATIO): Mean =", mean(df$age_ratio), ", Min =", min(df$age_ratio), "\n")

cat("Age of 0 means no age (true zero)\n")
cat("Reading score of 0 means nothing (arbitrary zero)\n")

cat("A 20-year-old is twice as old as a 10-year-old: 20/10 = 2 (VALID)\n")
cat("A 80 reading score is NOT twice as good as 40: 80/40 = 2 (INVALID)\n")

# Range = Maximum - Minimum
summary(df$read)
cat("Range for Reading =", max(df$read) - min(df$read), "\n")
cat("Min =", min(df$read), ", Max =", max(df$read), "\n")

# Range for all test scores
for (var in c("read", "write", "math", "science")) {
  x <- df[[var]]
  cat(var, "Range =", max(x) - min(x), "\n")
  cat("  (Min =", min(x), ", Max =", max(x), ")\n")
}

# IQR for all test scores
for (var in c("read", "write", "math", "science")) {
  x <- df[[var]]
  q <- quantile(x, probs = c(.25, .75))
  cat(var, "IQR =", q[[2]] - q[[1]], "\n")
  cat("  (Q1 =", q[[1]], ", Q3 =", q[[2]], ")\n")
}

# Standard deviation
for (var in c("read", "write", "math", "science")) {
  cat(var, "SD =", sd(df[[var]]), "\n")
}

# Coefficient of variation
for (var in c("read", "write", "math", "science")) {
  cv <- (sd(df[[var]]) / mean(df[[var]])) * 100
  cat(var, ": CV = ", round(cv, 1), "%\n", sep = "")
}

# Boxplot for all subjects
df %>%
  select(read, write, math, science) %>%
  pivot_longer(everything(), names_to = "subject", values_to = "score") %>%
  ggplot(aes(x = subject, y = score)) +
  geom_boxplot() +
  labs(title = "Dispersion Across Subjects", x = NULL, y = "Test Scores",
       caption = "Box = IQR, Line = Median, Whiskers = Range") +
  theme_minimal()

###############################################################################
# Day 2: Probability Distributions & Random Variables
###############################################################################

# Reload main dataset
df <- read_dta(url)
df$prog_str   <- as.character(as_factor(df$prog))
df$ses_str    <- as.character(as_factor(df$ses))
df$schtyp_str <- as.character(as_factor(df$schtyp))

cat("Total observations:", nrow(df), "\n")
head(df[, c("id", "female", "ses", "prog", "read", "write", "math", "science")], 10)

# Law of Large Numbers Demonstration
cat("As more observations are collected, proportion converges to true probability\n")

set.seed(12345)
n <- 1000
flip <- as.integer(runif(n) < 0.5)
trial <- seq_len(n)
cum_prob <- cumsum(flip) / trial

cat("Flip 10: Probability =", cum_prob[10], "\n")
cat("Flip 100: Probability =", cum_prob[100], "\n")
cat("Flip 1000: Probability =", cum_prob[1000], "\n")
cat("True probability = 0.50\n")

# Visualize convergence
llf_df <- data.frame(trial = trial, cum_prob = cum_prob)
ggplot(llf_df, aes(x = trial, y = cum_prob)) +
  geom_line() +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(title = "Law of Large Numbers - Coin Flips",
       x = "Number of Trials", y = "Cumulative Proportion of Heads",
       caption = "Converges to 0.5 as sample size increases") +
  theme_minimal()

# Probability Basics
cat("Probability always between 0 and 1 (0% to 100%)\n")

df <- read_dta(url)
df$prog_str   <- as.character(as_factor(df$prog))
df$ses_str    <- as.character(as_factor(df$ses))
df$schtyp_str <- as.character(as_factor(df$schtyp))

table(df$prog_str)
cat("P(General) =", 45 / 200, "\n")
cat("P(Academic) =", 105 / 200, "\n")
cat("P(Vocation) =", 50 / 200, "\n\n")

cat("These are disjoint - a student can only be in one program\n")
cat("P(General OR Academic) = P(General) + P(Academic) =", 45 / 200 + 105 / 200, "\n")

# Addition Rule
cat("\nAddition Rule\n")
cat("P(A OR B) = P(A) + P(B) - P(A AND B)\n")

# Contingency table for demonstration
prop.table(table(df$prog_str, df$female), margin = 1)
cat("P(General OR Female) =", (45 / 200 + 109 / 200 - 24 / 200), "\n")

# Conditional probability examples
prop.table(table(df$prog_str, df$ses_str), margin = 1)

cat("P(High SES | General) =", 9 / 45, "\n")
cat("P(High SES | Academic) =", 42 / 105, "\n")
cat("P(High SES | Vocation) =", 7 / 50, "\n")

# Discrete and Continuous Random Variables
cat("Countable, individualized, nondivisible values\n")

table(df$awards)
cat("AWARDS: Limited possible values (0 to 7)\n")

# Visualize discrete vs continuous
ggplot(df, aes(x = factor(awards))) +
  geom_bar() +
  labs(title = "Discrete: Awards", x = "Awards", y = "Frequency") +
  theme_minimal()

# Continuous
summary(df$read)
cat("READ: Any value between", min(df$read), "and", max(df$read), "\n")
summary(df$write)
cat("WRITE: Any value between", min(df$write), "and", max(df$write), "\n")

ggplot(df, aes(x = read)) +
  geom_histogram(bins = 15, color = "black", fill = "grey70") +
  stat_function(
    fun = function(x) dnorm(x, mean(df$read), sd(df$read)) *
      nrow(df) * (diff(range(df$read)) / 15),
    color = "red"
  ) +
  labs(title = "Continuous: Reading Scores", x = "Score (any value in range)", y = "Frequency") +
  theme_minimal()

# Bins and Histograms
for (b in c(10, 15, 30)) {
  p <- ggplot(df, aes(x = read)) +
    geom_histogram(bins = b, color = "black", fill = "grey70") +
    labs(title = paste("Reading Scores -", b, "Bins"), x = "Reading Score") +
    theme_minimal()
  if (b != 10) {
    p <- p + stat_function(
      fun = function(x) dnorm(x, mean(df$read), sd(df$read)) *
        nrow(df) * (diff(range(df$read)) / b),
      color = "red"
    )
  }
  print(p)
}

# Expected value
cat("E[READ] = Mean =", mean(df$read), "\n\n")

# Deviations and standard deviations
df$read_dev <- df$read - mean(df$read)
head(df[, c("id", "read", "read_dev")], 10)
cat("Sum of deviations =", sum(df$read_dev), "(should be close to 0)\n")

# Standard deviation
cat("SD(READ) =", sd(df$read), "\n")

# Normal distribution
ggplot(df, aes(x = read)) +
  geom_histogram(bins = 15, color = "black", fill = "grey70") +
  stat_function(
    fun = function(x) dnorm(x, mean(df$read), sd(df$read)) *
      nrow(df) * (diff(range(df$read)) / 15),
    color = "red"
  ) +
  labs(title = "Normal Distribution of Reading Scores", x = "Reading Score") +
  theme_minimal()

# Z-scores
df$z_read <- (df$read - mean(df$read)) / sd(df$read)

n1 <- sum(abs(df$z_read) <= 1)
cat("Within 1 SD (68%):", n1 / nrow(df) * 100, "%\n")

n2 <- sum(abs(df$z_read) <= 2)
cat("Within 2 SD (95%):", n2 / nrow(df) * 100, "%\n")

n3 <- sum(abs(df$z_read) <= 3)
cat("Within 3 SD (99.7%):", n3 / nrow(df) * 100, "%\n")

#### Extra helpful commands ###################################################
# Export summary table / save current data frame with new variables
# saveRDS(df, "day2_data.rds")                       # analogous to `save "day2_data.dta", replace`
# writexl::write_xlsx(df, "day2_data.xlsx")           # analogous to `export excel ...`

# "Search for commands" / "Get help" -> use ?function_name or ??keyword in R, e.g.:
# ?cor

# Export a plot (analogous to `graph export`)
# ggsave("day_2_graph.png", width = 8, height = 6, dpi = 200)

###############################################################################
# Day 3
###############################################################################

df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))

# Population values
cat("Population size =", nrow(df), "\n")

# Population parameters
pop_mean <- mean(df$read)
pop_sd   <- sd(df$read)
cat("Population Mean (\u03bc) =", pop_mean, "\n")
cat("Population SD (\u03c3) =", pop_sd, "\n")

# Random sampling (`sample 30, count`)
set.seed(12345)
sample_df <- df[sample(nrow(df), 30), ]

cat("Sample size =", nrow(sample_df), "\n")
samp_mean <- mean(sample_df$read)
samp_sd   <- sd(sample_df$read)
cat("Sample Mean (x\u0304) =", samp_mean, "\n")
cat("Sample SD (s) =", samp_sd, "\n\n")

cat("Population Mean = 52.23\n")
cat("Sample Mean =", samp_mean, "\n")
cat("Difference =", samp_mean - 52.23, "\n")

# Reload, review a few sample differences
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))
cat("Population size =", nrow(df), "\n")

set.seed(1)
sample_df <- df[sample(nrow(df), 30), ]
summary(sample_df$read)

# Sampling distributions: take 50 samples of n=30 and record the mean each time
sample_means <- numeric(50)
for (i in 1:50) {
  set.seed(12345 + i)
  fresh <- read_dta(url)
  s <- fresh[sample(nrow(fresh), 30), ]
  sample_means[i] <- mean(s$read)
}
means_df <- data.frame(mean = sample_means)

cat("\n--- SAMPLING DISTRIBUTION PROPERTIES ---\n")
summary(means_df$mean)

cat("Mean of sample means =", mean(means_df$mean), "\n")
cat("Population mean (\u03bc) = 52.23\n")
cat("Difference =", mean(means_df$mean) - 52.23, "\n\n")

se_empirical <- sd(means_df$mean)
cat("Standard deviation of sample means (Standard Error) =", se_empirical, "\n")
cat("Theoretical SE = \u03c3/\u221an = 10.23/\u221a30 =", 10.23 / sqrt(30), "\n")

# Visualize sampling distribution
ggplot(means_df, aes(x = mean)) +
  geom_histogram(bins = 15, color = "black", fill = "grey70") +
  stat_function(
    fun = function(x) dnorm(x, mean(means_df$mean), sd(means_df$mean)) *
      nrow(means_df) * (diff(range(means_df$mean)) / 15),
    color = "red"
  ) +
  labs(title = "Sampling Distribution of the Mean (n=30)", x = "Sample Mean") +
  theme_minimal()

#### Quick loop explanation ####################################################
# Example: Simple loop (equivalent to Stata's `forvalues i = 1/5`)
for (i in 1:5) {
  cat("This is iteration number", i, "\n")
}
# How it works:
# - i starts at 1
# - Runs the code inside the loop body
# - i increases by 1 each pass
# - Repeats until the sequence is exhausted
# Analogy: Like telling someone "Repeat this task N times, and keep count."

# Standard error
df <- read_dta(url)
pop_sd <- sd(df$read)
cat("Population SD (\u03c3) =", pop_sd, "\n")

cat("Sample Size (n)    Standard Error\n")
for (n in c(10, 30, 50, 100, 200)) {
  se <- pop_sd / sqrt(n)
  cat(sprintf("   %-15d %s\n", n, round(se, 2)))
}

# Confidence intervals
set.seed(12345)
sample_df <- df[sample(nrow(df), 50), ]

mean_v <- mean(sample_df$read)
sd_v   <- sd(sample_df$read)
n      <- nrow(sample_df)
se     <- sd_v / sqrt(n)

lower_95 <- mean_v - 1.96 * se
upper_95 <- mean_v + 1.96 * se

lower_99 <- mean_v - 2.576 * se
upper_99 <- mean_v + 2.576 * se

cat("Sample Mean =", mean_v, "\n")
cat("Standard Error =", se, "\n\n")
cat(sprintf("95%% Confidence Interval: [%s, %s]\n", round(lower_95, 2), round(upper_95, 2)))
cat(sprintf("99%% Confidence Interval: [%s, %s]\n", round(lower_99, 2), round(upper_99, 2)))

# Simulating confidence intervals: take 25 samples, check how many CIs
# contain the true population mean
pop_df   <- read_dta(url)
pop_mean <- mean(pop_df$read)

results <- vector("list", 25)
for (i in 1:25) {
  set.seed(12345 + i)
  fresh <- read_dta(url)
  s <- fresh[sample(nrow(fresh), 50), ]
  mean_v <- mean(s$read)
  sd_v   <- sd(s$read)
  se     <- sd_v / sqrt(50)

  lower <- mean_v - 1.96 * se
  upper <- mean_v + 1.96 * se
  contains_mean <- as.integer(lower <= pop_mean & pop_mean <= upper)

  results[[i]] <- data.frame(sample_mean = mean_v, lower_ci = lower,
                              upper_ci = upper, contains_mean = contains_mean)
}
ci_df <- bind_rows(results)

cat("\nIntervals containing the population mean:", sum(ci_df$contains_mean), "out of 25\n")
cat("Percentage:", sum(ci_df$contains_mean) / 25 * 100, "%\n")
ci_df

###############################################################################
# Day 4
###############################################################################

# Z-Scores and Hypothesis Testing
# We treat the full dataset as our POPULATION, then draw a SAMPLE from it
# to test against that population mean.

df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))

cat("Is the mean reading score different from the population mean?\n")

# Step 1: Population parameters (from the full dataset)
pop_mean <- mean(df$read)
pop_sd   <- sd(df$read)
pop_n    <- nrow(df)

cat("Population mean (\u03bc) =", pop_mean, "\n")
cat("Population SD (\u03c3) =", pop_sd, "\n")
cat("Population size (N) =", pop_n, "\n\n")

# Step 2: Draw a sample from the population
set.seed(12345)
sample_df <- df[sample(nrow(df), 50), ]

samp_mean <- mean(sample_df$read)
n <- nrow(sample_df)

cat("Sample mean =", samp_mean, "\n")
cat("Sample size (n) =", n, "\n\n")

# An R variable is a temporary storage container for values you use throughout
# your code (this plays the same role as a Stata local macro).

# Step 3: Set up hypotheses
cat(sprintf("H\u2080: sample mean = population mean (%s)\n", pop_mean))
cat("H\u2090: sample mean \u2260 population mean (two-tailed)\n\n")

# Step 4: Calculate z-score
z <- (samp_mean - pop_mean) / (pop_sd / sqrt(n))

cat(sprintf("z = (%s - %s) / (%s / sqrt(%s)) = %s\n",
            round(samp_mean, 2), round(pop_mean, 2), round(pop_sd, 2), n, round(z, 3)))

# Critical value for alpha = 0.05 (two-tailed)
crit <- qnorm(0.975)
cat("Critical value (\u03b1=0.05) = \u00b1", crit, "\n\n")

# Step 5: Decision using critical value
if (abs(z) > crit) {
  cat(sprintf("|z| = %s > %s, REJECT H0\n", abs(z), crit))
  cat("The sample mean IS significantly different from the population mean\n")
} else {
  cat(sprintf("|z| = %s < %s, FAIL TO REJECT H0\n", abs(z), crit))
  cat("The sample mean is NOT significantly different from the population mean\n")
}
# if/else: choose which branch runs based on a condition, same role as
# Stata's `if {} else {}`

# Step 6: Calculate p-value
p <- 2 * (1 - pnorm(abs(z)))
cat("\np-value\n")
cat("p =", p, "\n")

if (p < 0.05) {
  cat(sprintf("p = %s < 0.05, REJECT H0\n", p))
} else {
  cat(sprintf("p = %s > 0.05, FAIL TO REJECT H0\n", p))
}

#### T-test ####################################################################
# We now treat our data as a SAMPLE drawn from a larger population.
df <- read_dta(url)

cat("Testing if mean math score equals 50\n")

# One-sample t-test
t_result <- t.test(df$math, mu = 50)
cat("t-test output\n")
print(t_result)

if (t_result$p.value < 0.05) {
  cat(sprintf("p = %s < 0.05, REJECT H0\n", round(t_result$p.value, 4)))
  cat("Math scores ARE significantly different from 50\n")
} else {
  cat(sprintf("p = %s > 0.05, FAIL TO REJECT H0\n", round(t_result$p.value, 4)))
  cat("Math scores are NOT significantly different from 50\n")
}

# Visualize
ggplot(df, aes(x = math)) +
  geom_histogram(bins = 15, color = "black", fill = "grey70") +
  stat_function(
    fun = function(x) dnorm(x, mean(df$math), sd(df$math)) *
      nrow(df) * (diff(range(df$math)) / 15),
    color = "red"
  ) +
  geom_vline(xintercept = 50, color = "red", linetype = "dashed") +
  labs(title = "Math Score Distribution", x = "Math Score") +
  theme_minimal()

#### Two-sample t-test #########################################################
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))

cat("Writing scores by program type\n")
cat("Testing if General vs Academic programs have different mean writing scores\n")

sub <- df %>% filter(prog_str %in% c("general", "academic"))
t2 <- t.test(write ~ prog_str, data = sub)  # Welch's t-test by default (matches Stata's `unequal` option)
print(t2)

cat("\nInterpretation\n")
cat("t =", round(t2$statistic, 3), "\n")
cat("df \u2248", round(t2$parameter, 1), "\n")
cat("p =", round(t2$p.value, 4), "\n\n")

if (t2$p.value < 0.05) {
  cat(sprintf("p = %s < 0.05, REJECT H0\n", round(t2$p.value, 4)))
  cat("General and Academic programs have SIGNIFICANTLY different writing scores\n")
} else {
  cat(sprintf("p = %s > 0.05, FAIL TO REJECT H0\n", round(t2$p.value, 4)))
  cat("General and Academic programs do NOT have significantly different writing scores\n")
}

# Visualize
ggplot(sub, aes(x = prog_str, y = write)) +
  geom_boxplot() +
  labs(title = "Writing Scores by Program", x = NULL, y = "Writing Score") +
  theme_minimal()

#### ANOVA / F-test #############################################################
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))

# tabstat by group
df %>%
  group_by(prog_str) %>%
  summarise(n = n(), mean = mean(read), sd = sd(read))

# Run the ANOVA (equivalent of `anova read prog`)
model <- aov(read ~ prog_str, data = df)
summary(model)

anova_summary <- summary(model)[[1]]
f_val <- anova_summary["prog_str", "F value"]
df1 <- anova_summary["prog_str", "Df"]
df2 <- anova_summary["Residuals", "Df"]
p_val <- anova_summary["prog_str", "Pr(>F)"]

cat("\nresults\n")
cat("F =", round(f_val, 3), "\n")
cat("df =", df1, ",", df2, "\n")
cat("p =", round(p_val, 7), "\n")
cat("p (unrounded) =", p_val, "\n\n")

if (p_val < 0.05) {
  cat(sprintf("p = %s < 0.05, REJECT H0\n", round(p_val, 4)))
  cat("Not all programs have the same mean reading score\n")
} else {
  cat(sprintf("p = %s > 0.05, FAIL TO REJECT H0\n", round(p_val, 4)))
  cat("All programs have the same mean reading score\n")
}

# Pairwise comparisons (equivalent of `margins prog, pwcompare(effects)`)
emm <- emmeans(model, "prog_str")
pairs(emm)

# Visualize
ggplot(df, aes(x = prog_str, y = read)) +
  geom_boxplot() +
  labs(title = "Reading Scores by Program Type", x = NULL, y = "Reading Score") +
  theme_minimal()

#### Chi-squared: goodness of fit ###############################################
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))

cat("Program type distribution\n")
cat("Testing if students are equally distributed across programs\n")

total <- nrow(df)
expected <- total / 3

obs_general  <- sum(df$prog_str == "general")
obs_academic <- sum(df$prog_str == "academic")
obs_vocation <- sum(df$prog_str == "vocation")

chi_sq <- ((obs_general - expected)^2 / expected
           + (obs_academic - expected)^2 / expected
           + (obs_vocation - expected)^2 / expected)

cat("\nchi2 =", round(chi_sq, 3), "\n")

p <- 1 - pchisq(chi_sq, df = 2)
cat("p =", p, "\n")

crit <- qchisq(0.95, df = 2)
cat("Critical value (df=2, alpha=0.05) =", round(crit, 3), "\n")

if (chi_sq > crit) {
  cat(sprintf("chi2 = %s > %s, REJECT H0\n", round(chi_sq, 3), round(crit, 3)))
} else {
  cat(sprintf("chi2 = %s < %s, FAIL TO REJECT H0\n", round(chi_sq, 3), round(crit, 3)))
}

# chi_sq    - the chi-square test statistic: sum of (observed - expected)^2 / expected
#             across all 3 categories
# p         - p-value for chi_sq using the chi-square distribution with 2 df
#             (df = number of categories - 1 = 3 - 1 = 2)

#### Chi-squared: independence ##################################################
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))

cat("Program type and SES\n")
cat("Testing if program type and SES are independent\n")

contingency <- table(df$prog_str, df$ses_str)
chi_test <- chisq.test(contingency)

cat("\nchi-squared output\n")
print(contingency)
cat("\nRow %:\n")
print(round(prop.table(contingency, margin = 1), 3))
cat("\nExpected counts:\n")
print(round(chi_test$expected, 2))

cat("\ninterpretation\n")
cat("chi2 =", round(chi_test$statistic, 3), "\n")
cat("df =", chi_test$parameter, "\n")
cat("p =", round(chi_test$p.value, 4), "\n\n")

if (chi_test$p.value < 0.05) {
  cat(sprintf("p = %s < 0.05, REJECT H0\n", round(chi_test$p.value, 4)))
  cat("Program type and SES are NOT independent\n")
  cat("There IS a relationship between program and SES\n")
} else {
  cat(sprintf("p = %s > 0.05, FAIL TO REJECT H0\n", round(chi_test$p.value, 4)))
  cat("Program type and SES ARE independent\n")
  cat("There is NO relationship between program and SES\n")
}

# Visualize
as.data.frame(contingency) %>%
  rename(prog_str = Var1, ses_str = Var2, count = Freq) %>%
  ggplot(aes(x = ses_str, y = count, fill = prog_str)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Program Type by SES", x = NULL, y = "Count") +
  theme_minimal()

###############################################################################
# Day 5
###############################################################################

df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))
df$ses_str  <- as.character(as_factor(df$ses))

# Simple linear regression: `regress read write`
model <- lm(read ~ write, data = df)
summary(model)

a  <- coef(model)[["(Intercept)"]]  # Y-intercept
b  <- coef(model)[["write"]]        # Slope
r2 <- summary(model)$r.squared      # R-squared

cat("\nRegression equation:\n")
cat(sprintf("Reading = %s + %s * Writing\n", round(a, 4), round(b, 4)))
cat("\nInterpretation:\n")
cat(sprintf("  - If Writing = 0, predicted Reading = %s\n", round(a, 4)))
cat(sprintf("  - For each 1-point increase in Writing, Reading increases by %s\n", round(b, 4)))
cat(sprintf("  - R-squared = %s (%s%% of variance explained)\n", round(r2, 3), round(r2 * 100, 1)))
cat("\n")

# Predicting Reading score for a Writing score of 50
cat("If a student has a Writing score of 50, what is their predicted Reading score?\n")
predicted <- a + b * 50
cat(sprintf("Reading = %s + %s * 50 = %s\n", round(a, 4), round(b, 4), round(predicted, 4)))

# Basic scatterplot
ggplot(df, aes(x = write, y = read)) +
  geom_point(color = "blue") +
  labs(title = "Reading vs Writing Scores", x = "Writing Score", y = "Reading Score") +
  theme_minimal()

# Scatterplot with regression line
ggplot(df, aes(x = write, y = read)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Reading vs Writing Scores with Regression Line",
       x = "Writing Score", y = "Reading Score") +
  theme_minimal()

# Multiple scatterplots in one, by program type (`by(prog_str)`)
ggplot(df, aes(x = write, y = read)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ prog_str) +
  labs(title = "Reading vs Writing by Program Type", x = "Writing Score", y = "Reading Score") +
  theme_minimal()

# Correlation matrix
cor(df[, c("read", "write", "math", "science")])

# Predict using different variables
for (var in c("write", "math", "science")) {
  m <- lm(as.formula(paste("read ~", var)), data = df)
  r2v <- summary(m)$r.squared
  cat(sprintf("R-squared using %s = %s (%s%%)\n", var, round(r2v, 3), round(r2v * 100, 3)))
}

# Creating predicted values and difference
model <- lm(read ~ write, data = df)
df$predicted_read <- predict(model)
df$diff_read_read <- df$read - df$predicted_read
head(df[, c("read", "predicted_read", "diff_read_read")])

# Quick regression with multiple predictors
model_multi <- lm(read ~ write + math + science, data = df)
summary(model_multi)

cat("\nInterpretation of coefficients:\n")
cat("Reading =", round(coef(model_multi)[["(Intercept)"]], 4), "\n")
cat("  +", round(coef(model_multi)[["write"]], 4), "*Writing\n")
cat("  +", round(coef(model_multi)[["math"]], 4), "*Math\n")
cat("  +", round(coef(model_multi)[["science"]], 4), "*Science\n\n")
cat("R-squared =", round(summary(model_multi)$r.squared, 3),
    sprintf("(%s%%)\n", round(summary(model_multi)$r.squared * 100, 3)))

###############################################################################
# Extra: Data Engineering / Cleaning (not part of the main course)
###############################################################################

# Connecting to a database
# The original script connects via ODBC (`odbc list`, `odbc load`).
# In R, the equivalent is usually DBI + a driver package, e.g.:
#
# library(DBI)
# library(RPostgres)
# con <- dbConnect(RPostgres::Postgres(), dbname = "dbname", host = "host",
#                   port = 5432, user = "user", password = "password")  # e.g. NeonDB
# pokemon <- dbGetQuery(con, "SELECT * FROM rawdata.pokemon")

# Joins / merges
# Build a lookup/dimension table for program type
prog_lookup <- data.frame(
  prog = c(1, 2, 3),
  prog_full = c("General Track", "Academic Track", "Vocational Track"),
  funding_source = c("State Education Fund", "State Education Fund", "Federal Workforce Grant")
)
prog_lookup

# Merge (`merge m:1 prog using prog_lookup.dta`)
df_raw <- read_dta(url)  # keep numeric `prog` codes for the join key
merged <- df_raw %>% left_join(prog_lookup, by = "prog")

# Show ALL 200 rows with the new columns
merged[, c("id", "prog", "prog_full", "funding_source")]

# Union / Append
# Split hsbdemo into its 3 program tracks, then stack them back together.
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))

general_only <- df %>% filter(prog_str == "general")
cat("General track rows:", nrow(general_only), "\n")

academic_only <- df %>% filter(prog_str == "academic")
cat("Academic track rows:", nrow(academic_only), "\n")

vocation_only <- df %>% filter(prog_str == "vocation")
cat("Vocation track rows:", nrow(vocation_only), "\n")

# Union them back together -- bind_rows stacks rows; the columns never change
stacked <- bind_rows(general_only, academic_only, vocation_only)
cat("After appending all three tracks:", nrow(stacked), "rows\n")

stacked <- stacked %>% arrange(prog_str, id)
stacked

cat("Total rows:", nrow(stacked), "\n")  # should equal 200

# Group by (`collapse (mean) read write math, by(prog_str)`)
df <- read_dta(url)
df$prog_str <- as.character(as_factor(df$prog))

collapsed <- df %>%
  group_by(prog_str) %>%
  summarise(read = mean(read), write = mean(write), math = mean(math))
collapsed

# Reshape -- wide vs. long
# hsbdemo starts "wide": one row per student, one column per subject test score.
# Reshape "long" stacks those 5 subject columns into 2: a subject label and a score.
df <- read_dta(url)

cat("WIDE format:", nrow(df), "rows, one row per student\n")
head(df[, c("id", "read", "write", "math", "science", "socst")])

# Reshape long -- stack the 5 score columns into rows (`pivot_longer` = `reshape long`)
score_cols <- c("read", "write", "math", "science", "socst")
long_df <- df %>%
  select(id, all_of(score_cols)) %>%
  pivot_longer(cols = all_of(score_cols), names_to = "subject", values_to = "score_")

cat("\nLONG format:", nrow(long_df), "rows -- 200 students x 5 subjects = 1000 rows\n")
long_df <- long_df %>% arrange(id, subject)
long_df
