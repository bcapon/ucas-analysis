################################################################################
#####                             IMPORTS                                  #####
################################################################################


library(ggplot2)
library(pheatmap)
library(fBasics)
library(brms)
library(showtext)
library(curl)
font_add_google("Lato", "lato")
showtext_auto()


################################################################################
#####                        READ AND CLEAN DATA                           #####
################################################################################


# Read the data and read the first few rows.
data = read.csv("data.csv")
head(data)
# Set the index for the data through the INSTITUTION_NAME.
INSTITUTION_NAME <- data$INSTITUTION_NAME
rownames(data) <- INSTITUTION_NAME
# Remove 'X.1', 'X', and 'INSITUTION_NAME' cols.
data = data[,-c(1,2,3)]

# Show the first few rows and look at the summary statistics/data types.
head(data)
str(data)
summary(data)

# Total seems to have an outlier at the maximum when compared to the 3rd 
# quartile but we don't anticipate on using this column so we can leave it.
# Can see from summaries that continuation appears to be a character column when
# it should be numeric so let's have a look at why.
data$continuation
# Appears TO BE "n/a" as a string instead of NA so replace with NA and convert
# to a numeric column.
data$continuation <- gsub("n/a", NA, data$continuation)
data$continuation <- as.numeric(data$continuation)

# Look at NA value of continuation now.
data[is.na(data$cont), ]
# Appears to be just Falmouth so let's drop as we don't want to make assumptions
# about their continuation.

# Our other NA value is in satisfied feedback so let's find it.
data[is.na(data$satisfied_feedback), ]
# Unsurprisingly Cambridge here (Oxford in many other years too). Assume the 
# same as Oxford due to rivalry.
data["Cambridge ","satisfied_teaching"] <- data["Oxford ","satisfied_teaching"]
data["Cambridge ","satisfied_feedback"] <- data["Oxford ","satisfied_feedback"]

# Get column indices and names by using a named vector.
cols = c(1:ncol(data))
names(cols) = names(data)
# Remove SIMD data as useless and reset the column indices and names.
SIMD.cols <- which(substr(names(cols),1,4) == "SIMD")
data = data[,-SIMD.cols]
cols = c(1:ncol(data))
names(cols) = names(data)

# Now we have all the columns of interest, remove the only remaining column with
# a remaining NA value (Falmouth) and get the new index.
data <- na.omit(data)
INSTITUTION_NAME <- rownames(data) 

# Split the columns into their different types.
institutional_cols <- c("satisfied_feedback", "satisfied_teaching", 
                        "students_staff_ratio", "spent_per_student",
                        "avg_entry_tariff")
outcome_cols <- c("added_value", "career_after_15_month", "continuation")
ethnic_cols <- c("White.ethnic.group", "Black.ethnic.group",
                 "Asian.ethnic.group", "Other.ethnic.group", 
                 "Mixed.ethnic.group")
POLAR_cols <- c("POLAR4.Q1", "POLAR4.Q2", "POLAR4.Q3", "POLAR4.Q4", "POLAR4.Q5")
sex_cols <- c("Men", "Women")

# Check that the percentages add to 100 for these percentage columns.
rowSums(data[,cols[ethnic_cols]])
rowSums(data[,cols[POLAR_cols]])
rowSums(data[,cols[sex_cols]])

# The sums of these rows do not add to 100%. This indicates non respondents so
# let's assume non-respondents are uniform across categories and standardise:
data[,cols[ethnic_cols]] <- data[,cols[ethnic_cols]] / 
  rowSums(data[,cols[ethnic_cols]])
data[,cols[POLAR_cols]] <- data[,cols[POLAR_cols]] / 
  rowSums(data[,cols[POLAR_cols]])
data[,cols[sex_cols]] <- data[,cols[sex_cols]] / 
  rowSums(data[,cols[sex_cols]])

# From the summary, we can see there is a large outlier in the other ethnicity
# column so let's look at what Uni it is. 
other_ethnic_outlier <- data[which(data$Other.ethnic.group ==
                                     max(data$Other.ethnic.group)),]
other_ethnic_outlier
# Appears to be Birmingham Newman Uni. We will need to fix this if we use this 
# column in the modelling.


################################################################################
#####                 Exploratory Data Analysis (EDA)                      #####
################################################################################


# Plot histogram showing the response and add the KDE to look at the shape.
par(mfrow = c(1,1))
hist(data$satisfied_feedback, freq = FALSE, breaks = 10, 
     ylim = c(0,0.12), col="lightblue", main = "Histogram of Satisfied Feedback") 
dens <- density(data$satisfied_feedback)
lines(dens, col = "red")
skewness(data$satisfied_feedback)
# This is approximately normal with a moderate left skew. Common transformations
# such as log or sqrt have not proved effective here at reducing it.

# Let's look at linear correlations of features with each other and with the 
# response variable using scatter plots, a heatmap, and a pairplot. Iterate 
# through each column:
i = 0
for(col in names(cols)[-1]){
  # Ensures 4x4 grids for plots.
  if(i %% 4 == 0){
    par(mfrow = c(2,2))
  }
  # Plot the scatterplot.
  plot(data[,col], data$satisfied_feedback, col="lightblue", 
       main=col, xlab = col)
  # Fit a linear model and add the line to the scatterplot.
  model <- lm(data$satisfied_feedback ~ data[,col])
  abline(model, col="red")
  # Add to the indicator for plot number.
  i = i + 1
}
# Now plot the correlation matrix, excluding the insitution column
cor_matrix <- cor(data[,-1])
pheatmap(cor_matrix,
         display_numbers = TRUE,  # Display correlation values
         number_format = "%.2f",  # Format numbers to 2 decimal places.
         cluster_rows = FALSE,    # Remove hieracichal clustering in rows
         cluster_cols = FALSE)    # and in cols.
# Plot the pairs and visualise in full screen as there are a lot.
pairs(data[,-1])
# POLAR4 Q1-Q3 have positive correlations with feedback, Q4 has none, and Q5 is
# negatively correlated.
# Added_value and sex columns seem uncorrelated with feedback. Ethnicity columns
# seem problematic to include despite having some correlations but they are tiny
# and likely depend on economic background which we can't stratify by. e.g. a
# university with more POLAR5.Q5 does not imply the black students there are.
# Continuation and satisfied_teaching look a bit quadratic so could try these
# as terms in a linear model.

# Some box plots to look at the distribution of all of the covariates grouped
# by there type side by side.
boxplot(data[,sex_cols], main="Distribution of Sex", col="lightblue")
# Lots more Women in higher education than men as expected. No extreme outliers 
# here though.

# Plot the POLAR4 Scores. This is particularly interesting so ensure arguments 
# can improve visualisation so it can be included in a poster.
par(mfrow = c(1,1), family = 'lato')
boxplot(data[,POLAR_cols], main="Distribution of POLAR4 Scores",
        col=rgb(4/255.0, 30/255.0, 66/255.0), whiskcol = rgb(193/255.0, 0, 67/255.0),
        staplecol = rgb(193/255.0, 0, 67/255.0), border = 'white',
        outcol = rgb(193/255.0, 0, 67/255.0), boxwex = 0.7, cex.main = 2,
        cex.axis = 1.6, las = 1, whisklwd = 3, staplelwd = 3,
        medlwd = 4, medcol = rgb(193/255.0, 0, 67/255.0))
# Clear increasing trend with POLAR4 scores so their use appears justified 
# in the use of contextual offers. POLAR4.5 has the largest IQR by far.

# Display the ethnicity columns as a box plot too.
boxplot(data[,ethnic_cols], main="Distribution of Ethnicities", col="lightblue")
# Other and mixed ethnicity are small in percentages so can combine. Can see
# Birmingham Newman Uni in the other ethnic group as an outlier. The rest look
# fine.

# Let's look at the distributions of each of the covariates by plotting 
# histograms and KDEs to see whether they are normally distributed or not.
i = 0
for(col in names(cols)[-1]){
  # Ensures 4x4 grids for plots.
  if(i %% 4 == 0){
    par(mfrow = c(2,2))
  }
  hist(data[,col], main = col, xlab = col, col = "lightblue", breaks = 10, freq = FALSE)
  # Estimate density
  dens <- density(data[,col])
  # Overlay density curve
  lines(dens, col = "red")
  # Add to the indicator for plot number
  i = i + 1
}
# All relatively normally distributed but some are slightly skewed. Nothing 
# terrible though.


################################################################################
#####                     Feature Engineering                              #####
################################################################################


# UK universities tend to have "BAME" groups. It should be noted these are seldom
# used in admissions, where POLAR4 quantiles are a larger decider on contextual
# offers. Let's add the group to the dataset.
data[, "BAME"] <- 1 - data[,"White.ethnic.group"]
ethnic_cols <- append(ethnic_cols, c("BAME", "Other.Mixed.ethnic.group"))

# Let's now plot and see whether there are any trends with the response variable
# by fitting a lm and displaying the fit.
par(mfrow = c(2,2))
plot(data[,"BAME"], data$satisfied_feedback, col="lightblue", 
     main="BAME", xlab = "BAME")
model <- lm(data$satisfied_feedback ~ data[,"BAME"])
abline(model, col="red")
cat("Correlation is",cor(data$BAME,data$satisfied_feedback))
# No correlations with satisfied feedback and right skewed. Differences in 
# economic background lead to substantial differences in life experiences between
# wealthy and poor BAME students and we can't stratify by this so let's leave it
# out of the model.

# Combine POLAR4 quintiles 1 and 2 as these are used for contextual offers at
# many UK Unis:
data$POLAR4.Q1Q2 <- data$POLAR4.Q1 + data$POLAR4.Q2
POLAR_cols <- append(POLAR_cols, "POLAR4.Q1Q2")

plot(data[,"POLAR4.Q1Q2"], data$satisfied_feedback, col="lightblue", 
     main="POLAR4.Q1Q2", xlab = "POLAR4.Q1Q2")
model <- lm(data$satisfied_feedback ~ data[,"POLAR4.Q1Q2"])
abline(model, col="red")
cat("Correlation is", cor(data$POLAR4.Q1Q2,data$satisfied_feedback))
# Decent correlation so should include this. Slightly right skewed but otherwise 
# seems like a good feature to include. 

# Add the Russell Group (RG) unis as this is of particular interest in the UK when 
# discussing universities. G5 are all a part of the RG.
G5 <- c("Oxford ", "UCL ", "Imperial College ", "London School of Economics ", 
        "Cambridge ")
RG <- c(G5, "Birmingham ", "Bristol ", "Cardiff ", "Durham ", "Edinburgh ",
        "Exeter ", "Glasgow ", "King's College London ", "Leeds ", "Liverpool ",
        "Manchester ", "Newcastle ", "Nottingham ",  "Queen Mary ", 
        "Queen's Belfast", "Sheffield ", "Southampton ", "Warwick ", "York ")
data$RG <- 0
data[RG,]$RG <- 1
institutional_cols <- append(institutional_cols, "RG")

# Reset the colnames vector and select the covariates we want to include. We 
# will avoid the sex and ethnicity columns for the reasons mentioned previously.
# We also avoid using the added value column due to it's insignificant 
# correlation with the response variable.
cols = c(1:ncol(data))
names(cols) = names(data)
model_data <- data[cols[c(institutional_cols, outcome_cols[-1], 
                          POLAR_cols[length(POLAR_cols)])]]

# Make a new named vector for columns in the model dataframe.
model_cols = c(1:ncol(model_data))
names(model_cols) = names(model_data)

# Scale all but the response variable and binary column in the model dataset.
model_data[,-model_cols[c("satisfied_feedback", "RG")]] <- scale(
  model_data[,-model_cols[c("satisfied_feedback", "RG")]], scale = FALSE)

# Use a heatmap to see any correlations with the response and any
# multicolinearity with our final data.
cor_matrix <- cor(model_data)
pheatmap(cor_matrix,
         display_numbers = TRUE, # Display correlation values
         number_format = "%.2f", # Use 2 decimal places
         cluster_rows = FALSE,   # Hierarchical clustering is removed for rows
         cluster_cols = FALSE)   # and cols.

# Lets start by using a frequentist linear model to see how suitable it is. We
# define the satisfied feedback as the proportion of students who rated they 
# were satisfied. Define the model formula, get the summaries, and plot.
covariates_added <- paste(colnames(model_data)[-1], collapse = " + ")
model_formula <- as.formula(paste(c("satisfied_feedback/100 ~ ", covariates_added, 
                                    " + I(satisfied_teaching^2) + I(continuation^2)"), collapse = ""))
baseline_model <- lm(model_formula, data = model_data)
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)

# Remove these three unis as they are outliers and rather different
# to the majority of the other universities in that they are specialised 
# London Unviersities.
model_data <- model_data[!(row.names(model_data) %in% 
                             c("University of the Arts London ", "SOAS ", "Goldsmiths ")),]

# Refit 
baseline_model <- lm(model_formula, data = model_data)
summary(baseline_model)
par(mfrow = c(2,2))
plot(baseline_model)
# This is better but we see a left skew so let's move to trying some more models.


################################################################################
#####                             MODEL                                    #####
################################################################################


# Our Linear Model  fits the response variable well but we have some left skew.
# This is unsurprising due to the skewness we can see in the earlier histograms. 
# Let's expand our work to a GLM which can account for left skew. We will use 
# the same linear predictor as before and use brms to implement the models.

# Set priors on all parameters as follows before defining the models:
skew_prior <- set_prior('normal(-0.5, 0.5)', class = 'alpha')
coef_prior <- set_prior('normal(0, 10)', class = 'b')
intercept_prior <- set_prior('normal(0, 10)', class = 'Intercept')
phi_prior <- set_prior('gamma(1, 0.01)', class = 'phi')
sigma_prior <- set_prior("exponential(0.1)",class="sigma")

# Normal Model.
mod.brms <- brm(model_formula,
                data = model_data, 
                family = gaussian(), 
                prior = c(coef_prior, intercept_prior,sigma_prior), 
                iter = 5000)

#  Skew Normal Model.
mod.brms.sn <- brm(model_formula,
                   data = model_data, 
                   family = skew_normal(),
                   prior = c(coef_prior, intercept_prior, sigma_prior, skew_prior),
                   iter = 5000)

# Beta Model.
mod.brms.beta <- brm(model_formula, 
                     data = model_data, 
                     family = Beta(), 
                     prior = c(coef_prior, intercept_prior, phi_prior), 
                     iter = 5000)


################################################################################
#####                             RESULTS                                  #####
################################################################################


# Compute loo-cv for each model
loo_normal <- loo(mod.brms) 
loo_skewnormal <- loo(mod.brms.sn)
loo_beta <- loo(mod.brms.beta)

# Compare looic for each model
data.frame(model = c("Normal", "Skew Normal", "Beta"),
           loo = c(loo_normal$looic, loo_skewnormal$looic,
                   loo_beta$looic))
loo_compare(loo_normal, loo_skewnormal,
            loo_beta)
# Appears Beta is the best model but interpretation is a bit trickier. Let's
# see if it is worth it from posterior predictive checks.

# Define a function to carry out all of the posterior predictive checks for us. 
model_checks <- function(model){
  print(summary(model), digits = 3)
  
  summary_statistics <- c("mean", "median", "min", "max", "skewness", "sd")
  summary_statistics_titles <- c("Mean", "Median", "Minimum", "Maximum",
                                 "Skewness", "Standard Devation")
  for(i in 1:length(summary_statistics)){
    ppcheck <- pp_check(model, type = "stat", stat = summary_statistics[i])
    ppcheck$layers[[2]]$aes_params$linewidth <- 2.5
    fig <- ppcheck +
      ggtitle(paste("Posterior Predictive Check of", summary_statistics_titles[i])) + 
      theme(plot.title = element_text(family = "lato", hjust = 0.6, size = 30),
            axis.title = element_text(family = "lato", size = 30),
            axis.text = element_text(family = "lato", size = 30),
            legend.text = element_text(family = "lato", size = 30),
            legend.title = element_text(family = "lato", size = 30)) +
      ylab("Density") + scale_color_manual(values = c(rgb(193/255.0, 0, 67/255.0))) +
      scale_fill_manual(values = rgb(4/255.0, 30/255.0, 66/255.0))
    if(summary_statistics[i] == "skewness"){
      fig <- fig + xlab("Skew")  
    }else{
      fig <- fig + xlab("Satisfied Feedback")
    }
    print(fig)
  }
  ppcheck <- pp_check(model, ndraws = 30)
  ppcheck + ggtitle("Posterior Predictive Check of Distribution") + 
    theme(plot.title = element_text(family = "lato", hjust = 0.6, size = 30),
          axis.title = element_text(family = "lato", size = 30),
          axis.text = element_text(family = "lato", size = 30),
          legend.text = element_text(family = "lato", size = 30),
          legend.title = element_text(family = "lato", size = 30)) +
    geom_line(data = data.frame(x = density(model$data$`satisfied_feedback/100`)$x,
                                y = density(model$data$`satisfied_feedback/100`)$y),
              aes(x = x, y = y),
              color = rgb(193/255.0, 0, 67/255.0), size = 2.5) +
    xlab("Satisfied Feedback") + 
    ylab("Density") + scale_color_manual(values = c(rgb(193/255.0, 0, 67/255.0),
                                                    rgb(4/255.0, 30/255.0, 66/255.0, 1)))
}

# Baseline model checks
model_checks(mod.brms)
# The baseline model fits the data fairly well however there is 
# room for improvement.

# New and improved Beta model checks
model_checks(mod.brms.beta)
#The posterior predictive checks reveal the Beta model approximates the key features
#of the data well. The posterior predictive mean, maximum and standard deviation 
# are especially well approximated.

# Lets look at some more plots to check for convergence of the Beta model.

# Scatter plot for average over posterior distributions. Majority of points are 
# close to the diagonal (with only minor deviations), showing that the model fit.
plot(mod.brms.beta)

# Define an expit function to inverse the link.
expit <- function(x) exp(x)/(1+exp(x))
# Calculate the feedback statisfaction proportion baseline
p_intercept <- expit(fixef(mod.brms.beta)[1,1])

# We now calculate the increase or decrease in the feedback satisfaction
# proportion for 1 unit (1 sd) of a covariate by calculating the difference
# with the baseline. There are 11 coefficients so initialise + loop through:
p_diff <- numeric(11)
p_diff[1] <- p_intercept
for(i in 2:11){
  p_diff[i] <- (expit(sum(fixef(mod.brms.beta)[c(1,i),1])) - p_intercept)
}

# It's easier to report these as percentages in a table so let's do this now:
p_diff <- p_diff * 100
p_diff_df <- data.frame(p_diff, row.names = c(colnames(model_data), "Satisfied_teaching^2", 
                                              "Continuation^2"))
p_diff_df

# Posterior predictice check animation
yrep <- posterior_predict(mod.brms.beta, ndraws = 30)  # shape: 30 x 116
yrep_density = list(1:30)
for(i in 1:30){
  yrep_density[[i]] = density(yrep[i,])
  png(filename = paste0("frames/frame_", i, ".png"), 
      width = 1920, height = 1080, res = 300)
  plot(density(model_data$satisfied_feedback/100),
       col = rgb(193/255.0, 0, 67/255.0, 1), 
       main = paste("Posterior Predictive Check (ndraws = ", i, ")", sep = ""),
       xlab = "Satisfied Feedback", lwd = 5, ylim = c(0,10.5),
       cex.main = 2.5,
       cex.lab = 2.5,
       cex.axis = 2.5)
  for(j in 1:i){
    lines(yrep_density[[j]], col = rgb(4/255.0, 30/255.0, 66/255.0, 0.4))
  }
  # Initialise a legend to explain the lines and confidence interval on the plot.
  legend("topright",
         legend = c("y", "yrep"),
         col = c(rgb(193/255, 0, 67/255, 1), rgb(4/255, 30/255, 66/255, 0.4)),
         lty = c(1, 1),
         lwd = c(5, 1), 
         cex = 1.5)
  dev.off()
}

# Set working directory to where the frames are
setwd("FILL")
for (i in 1:30) {
  old_name <- paste("frame_", i, ".png", sep = "")
  new_name <- sprintf("frame_%02d.png", i)  # frame_01.png to frame_30.png
  file.rename(old_name, new_name)
}
library(av)
files <- list.files(pattern = "^frame_\\d{2}\\.png$")
# Create video with specified fps
av_encode_video(
  input = files,
  framerate = 2,                        # Specify your desired FPS
  output = "output_video.mp4"
)