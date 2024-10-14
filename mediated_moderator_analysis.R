# Install required packages
install.packages(c("psych", "mediation", "rockchalk", "multilevel", 
                   "stargazer", "dplyr", "ggplot2"))



# Load required libraries
library(psych)          # Descriptive statistics
library(mediation)      # Mediation analysis and bootstrapping
library(rockchalk)      # Plotting interaction effects
library(multilevel)     # Sobel test
library(stargazer)      # Model summary tables
library(dplyr)          # Data manipulation
library(ggplot2)        # Plotting and visualizations



# Load the data  
dat <- read.csv("study2.csv")

# View the dataset structure 
View(dat)
head(dat)

# Check for missing values
colSums(is.na(dat))


# Rename key variables for simplicity
dat <- dat %>%
  mutate(
    X = manipu, # Independent variable (IV): Manipulation 
                # Ostracism ('1') vs Control ('0')
    
    Y = dark,   # Dependent variable (DV): Darkness judgment 
                # Higher value = perceive the room darker, Lower value = perceive the room lighter
    
    M = exist,  # Mediator (M): Meaningful existence 
                # Higher value = more meaningful, Lower value = less meaningfu
    
    W = sc      # Moderator: Self-compassion
                # Higher value = greater self-compassion
  )


# Check the column names of the dataset to confirm changes
colnames(dat)



# DESCRIPTIVE STATISTICS --------------------------------------------
describe(dat[, c("manipu", "Y", "M", "W")], 2)


# MANIPULATION CHECK ------------------------------------------------

manipulation_check_fit <- lm(dat$check ~ dat$X)  # Regression
summary(manipulation_check_fit)  # Display summary of the manipulation check



# 1. Total effect: Effect of X on Y (Ostracism on Darkness Judgment)------------

fit <- lm(Y ~ X, data = dat)
summary(fit)


# 2. Path A: Effect of X on M (Ostracism on Meaningful Existence)----------------

fita <- lm(M ~ X, data = dat)
summary(fita)


# 3. Path B: Effect of M on Y, controlling for X (Mediator on Darkness Judgment)----------------

fitb <- lm(Y ~ M + X, data = dat)
summary(fitb)


# 4. Reversed Path C: Effect of Y on X, controlling for M (Check for confounding paths)----------------

fitc <- lm(X ~ Y + M, data = dat)
summary(fitc)



# MODEL SUMMARY TABLE USING STARGAZER -------------------------------

stargazer(fit, fita, fitb, fitc,
          type = "text",
          title = "Mediation Analysis on Meaningful Existence",
          column.labels = c("Darkness Judgement","Meaningful Existence", "Control for X", "Control for M"),
          dep.var.labels = c("Y", "M", "Y", "X"),
          covariate.labels = c("Y (Darkness Judgement)", "M (Meaningful Existence )", "X (Ostracism vs Control)", "Constant"))



# SOBEL TEST FOR MEDIATION EFFECT SIGNIFICANCE -----------------------
# Use the Sobel test to evaluate the significance of the mediation effect

sobel_test <- sobel(dat$X, dat$M, dat$Y)
print(sobel_test)


# CENTERING THE MODERATOR -------------------------------------------
# Center the moderator (W) for interaction analysis

dat <- dat %>%
  mutate(Wc = as.numeric(scale(W, center = TRUE, scale = FALSE)))
  
#no need to center for X because it's a binary value

# Verify the new column exists
head(dat$Wc)
colnames(dat)


# MODERATION ANALYSIS: Interaction of X and W on M ------------------

fitMod <- lm(M ~ X + Wc + X * Wc, data = dat) # Interaction model
summary(fitMod)



# CREATE SIMPLIFIED DATAFRAME FOR MODERATION-MEDIATION ANALYSIS ----
sc_modmed <- dplyr::select(dat, Y, X, M, Wc)  # Use the already centered moderator (Wc)

# Check structure of the new dataframe
str(sc_modmed)


# DESCRIPTIVE STATISTICS OF THE SIMPLIFIED DATAFRAME ----------------
round(describe(sc_modmed), 2)


# DEFINE MEDIATION FUNCTION -----------------------------------------
# Ensure that the correct mediate function from the "mediation" package is used
mediate <- mediation::mediate

# MEDIATION ANALYSIS WITH INTERACTION -----------------------------------------
out_model <- lm(Y ~ M + X + Wc + M * Wc, data = sc_modmed)
summary(out_model)



# VISUALIZATION -------------------


# Box plot for Path A: X -> M
ggplot(dat, aes(x = as.factor(X), y = Y, fill = as.factor(X))) +
  geom_boxplot() +
  labs(
    title = "Effect of Manipulation on Darkness Judgment",
    x = "Manipulation (0 = Control, 1 = Ostracism)",
    y = "Darkness Judgment (Y)"
  ) +
  theme_minimal()



# Create quartiles for M to group values
dat <- dat %>%
  mutate(M_quartile = ntile(M, 4))

# Box plot for Path B: M -> Y (Effect of Meaningful Existence on Darkness Judgment)
ggplot(dat, aes(x = as.factor(M_quartile), y = Y, fill = as.factor(M_quartile))) +
  geom_boxplot() +
  labs(
    title = "Effect of Meaningful Existence on Darkness Judgment (Path B: M -> Y)",
    x = "Meaningful Existence (M) Quartile",
    y = "Darkness Judgment (Y)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3", name = "M Quartile")


#MEDIATION ANALYSIS VISUALIZATION------------
# Perform bootstrapped mediation analysis with 999 simulations
fitMedBoot <- mediate(fita, fitb, treat = "X", mediator = "M", boot = TRUE, sims = 999)

# Print the summary of the mediation analysis
summary(fitMedBoot)
plot(fitMedBoot)



#INTERACTION EFFECT ANALYSIS VISUALIZATION------------
# Plot interaction slopes using plotSlopes
plotSlopes(
  fitMod, 
  plotx = "X",           # Predictor: Centered Manipulation (X)
  modx = "Wc",            # Moderator: Centered Self-Compassion (W)
  xlab = "Manipulation (Centered)", 
  ylab = "Meaningful Existence", 
  modxVals = "std.dev"    # Plot for +/- 1 standard deviation of the moderator
)



