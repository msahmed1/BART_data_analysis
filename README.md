# Data analysis

This documentation provides a detailed breakdown of the various tests and analyses performed on the collected data, including Likert scale questionnaires, structured face-to-face interviews, and objective behavioural measures, ensuring a transparent and comprehensive insight into the analytical process.

## Data Processing

Our analysis excluded data from the first six balloons as we assumed participants were acclimating to the game and the robot during these initial balloon trials. This approach helped in mitigating potential learning effects on the data.

Data processing was categorised into three main areas:

- **Compliance with robot suggestions**
  - we monitored the actions taken after help was requested. We established that compliance for an inflate request was achieved when the participant pressed inflate after requesting help, and for a collect request, compliance was achieved when the participant pressed the collect button after requesting help. Percentage compliance was calculated as the total number of times the participant complied with the robot’s suggestion divided by the total number of times they asked for help from the robot.
  - The number of inflates after help was requested was also recorded. This was used to indicate the level of trust ascribed to the robot by how much the participant inflated the balloon after requesting help. From this, the average inflates after help was requested was calculated by dividing inflates after help requests by the number of times help was requested.
- **Risk-taking behaviour** during the experiment, we measured the total balloon inflation, which refers to the number of times a participant pressed the inflate button for each balloon. From this, we calculated the average balloon inflation across both blocks by dividing the sum of the total inflates by the number of balloons that did not burst in the given block. This metric assessed any differences in participants’ behaviour across the study conditions and across both games regardless of the study conditions.
- **Reaction time** to decide what to do after the request help button was clicked was recorded. Initially, we normalised this data using the mean reaction time data; however, due to outliers skewing the data, we decided to use the median reaction time to reduce the impact of outliers on the data analysis.

Additionally, the exclusion criteria for objective data was set to two standard deviations from the mean. Two standard deviations capture 95% of the dataset; therefore, it was assumed that any data outside of this bound must be an outlier. Each of the above categories was treated as independent observation; the absence of excluded participants in multiple datasets supports this assumption.

For the Likert-scale questionnaire, we computed the mean score for each scale to gauge the general perceptions of the robot among the participants and if this changed between the groups. The mean for each questionnaire was used since the Likert scale is analysed as interval data; therefore, the mean is used to find the central tendency for each participant.

# Analytic methods

Objective datasets were assessed using Shapiro-Wilk normality and Levene’s homogeneity tests post-exclusion criteria. To ensure the accuracy of the Shapiro-Wilk normality test, the Q-Q plot was examined for the impact of outliers, as the test is sensitive to small sample sizes. Non-parametric methods were used for datasets that did not meet these assumptions.

To compare independent observations, the Wilcoxon rank-sum test, the non-parametric alternative of the independent samples t-test, was used, and the test results were based on an approximation method due to the presence of ties. We used this test to evaluate the compliance analysis for inflation behaviour after receiving help from the robot.

To compare related observations for non-group comparisons, the paired samples t-test was used. In cases where the data violated the assumptions of parametric tests, the Wilcoxon-signed rank test was used. We analysed compliance and risk-taking behaviours for non-group effects.

The data was analysed to evaluate the percentage of compliance with the suggestions made by the robots. The dataset was split into compliance percentages for each robot response (`inflate'' and `collect'') between study conditions. The analysis of these subsets showed little variation between the participants. Due to the non-normal distribution and the prevalence of ties within the data, traditional statistical analysis was deemed inappropriate for comparisons between study conditions. Instead, a visual inspection was conducted on the generated plots

The reaction time data was evaluated for parametric testing; however, one of the data subsets did not satisfy the assumptions of parametric testing; however, after reviewing the Q-Q plot of the distribution, it was observed a single outlier was significantly impacting the Shapiro-Wilks normality test. Therefore, we decided to assume the data satisfied the assumptions of parametric testing, and we used mixed ANOVA to evaluate the impact of the robot’s response on participant reaction time across both study conditions.

In using non-parametric Tests, we acknowledged the potential limitation of reduced statistical power, particularly with smaller sample sizes. We supplemented significant results with post hoc analyses and effect size. This was to ensure significant findings were noticed and provide a measure of the practical significance of our results.

As for the subjective questionnaires, Shapiro-Wilk and Levene’s tests confirmed the normal distribution ($p > 0.05$) and homogeneity of variances ($p > 0.05$) for likeability, intelligence, trustworthiness, and ownership metrics, leading to the use of independent samples t-tests. Effect sizes were also calculated to understand the observed differences’ practical significance.

The structured interview was analysed quantitatively for closed-ended questions, while open-ended questions were analysed thematically. Themes related to the participant’s perception and reception of robot suggestions were identified to understand the subjective aspects of robot and participant interaction. Percentage analysis was carried out to represent the participant’s responses numerically. The themes identified were related to robot intelligence, helpfulness, and ownership.
