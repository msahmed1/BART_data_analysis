# Data Analysis Documentation

This README provides a detailed breakdown of our data analysis process, including how we have approached Likert scale questionnaires, structured interviews, and objective behavioural measures. Our primary goal is to ensure transparency and offer a clear understanding of the methodologies employed in our study.

# Repository Structure

Our data analysis is divided into separate R scripts for clarity and modularity:

- ‘demographic_analysis.R’: Analyses participant demographics.
- ‘compliance_behaviour.R’: Examines adherence to robot suggestions.
- ‘risk_behaviour.R’: Assesses participant risk-taking behaviours.
- ‘reaction_time.R’: Measures response times following robot interactions.
- ‘survey_data_analysis.R’: Processes and analyses survey data.
- ‘interview_data_analysis.R’: Analyses interview data quantitatively and thematically.

These scripts operate independently, with database operations intentionally repeated for readability. Console outputs are saved as a .txt file in the output_files folder, and plots are generated but not automatically saved.

Supporting functions are found in analysis_functions.R, which contains statistical models and calculations for effect sizes.

# How to Use

1. Setup: Ensure all required libraries are installed in R.
2. Run Scripts: Execute an R script to perform the respective analysis.
3. Review Outputs: Outputs are stored in output_files. Review these for analysis results and save plots as needed.

## Data Processing

We categorised our analysis into three main areas:

1. **Compliance with robot suggestions**
   - After help was requested, we monitored the participant’s actions and calculated the percentage compliance for each action. Compliance was achieved when the participant pressed the inflate button for an inflate request and the collect button for a collect request.
   - We also recorded the number of inflates after help was requested to indicate the level of trust in the robot. The average inflates after help was requested was calculated by dividing the inflates after help requests by the number of times help was requested.
2. **Risk-taking behaviour** during the experiment, we measured the total balloon inflation by recording the number of times a participant pressed the inflate button for each balloon. We calculated the average balloon inflation by dividing the sum of the total inflates by the number of balloons that did not burst in the given block. This metric assessed any differences in participants’ behaviour across the study conditions and both games, regardless of the study conditions.
3. **Reaction time** to decide what to do after clicking the request help button was recorded. Initially, we normalised this data using the mean reaction time, but due to outliers skewing the data, we used the median reaction time to reduce the impact of outliers on the data analysis.

We excluded data from the first six balloons as participants were assumed to be acclimating to the game and the robot during these initial balloon trials. The exclusion criteria for objective data was set to two standard deviations from the mean, which captured 95% of the dataset. Therefore, any data outside this bound was considered an outlier. Each category above was treated as an independent observation, and the absence of excluded participants in multiple datasets supports this assumption.

For the Likert-scale questionnaire, we computed the mean score for each scale to gauge the general perceptions of the robot among the participants and to determine any changes between groups. The mean for each questionnaire was used since the Likert scale is analysed as interval data, and it helps to find the central tendency for each participant.

# Analytic methods

We employed a combination of parametric and non-parametric tests, including Shapiro-Wilk normality, Levene’s homogeneity tests, Wilcoxon rank-sum, signed-rank tests and Mixed ANOVA, depending on the data distribution and sample size constraints. Post hoc analysis and effect size were calculations performed on significant results to determine the practical significance.

Subjective questionnaire data, meeting assumptions of normality and homogeneity of variance, were analysed with independent samples t-tests alongside effect size calculations to determine practical significance.

Interview data were analysed quantitatively for closed questions and thematically for open-ended responses, identifying key themes related to participants’ perceptions of the robot.
