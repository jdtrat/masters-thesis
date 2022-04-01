---
bibliography: references.bib
---

# Modeling Counterfactual Predicted Utility Theory

In the last chapter, I addressed my first thesis aim of developing counterfactual predicted utility theory (CPUT) as an alternative to expected utility theory to explain decision-making under risk. I showed how information about each potential outcome (terminal node of Figure \@ref(fig:prospect-diagram-general)) integrates with a counterfactual weighting parameter, $\gamma$, to adjust the utility of one option over another. I walked-through an example calculation assuming $\gamma = 0.1$ (diagrammed in Figure \@ref(fig:prospect-diagram-general)). In this chapter, I address my second thesis aim of assessing the predictive accuracy of CPUT using human choice data from a 'Sure Bet or Gamble' task.[@liebenow2021]

In an attempt to robustly, and transparently, fit CPUT to behavioral data, I follow the general guidelines presented in Robert Wilson and Anne Collins's paper *Ten simple rules for the computational modeling of behavioral data*.[@wilson2019]At a high level, they state that computational modeling allows us to make better sense of behavioral data with mathematical models that may provide insight into mechanisms underlying behavior. Although the exact form of the models differ, the basic steps to assess a model's descriptive and predictive efficacy are similar.

The first two steps Wilson and Collins discuss are designing an experiment and developing a model. For my thesis, I fit CPUT, as formulated in the last chapter, with data collected from an ongoing study by Brittany Liebenow and colleagues[@liebenow2021] where forty-five healthy adults (ages 18-65) were recruited to complete a sure-bet or gamble task (Figure \@ref(fig:sborg-task-description)).

![(\#fig:sborg-task-description)Schematic of a trial from the Sure Bet or Gamble task and subjective rating prompt.](04_modeling_files/figure-docx/sborg-task-description-1.png)

For thirty minutes, participants indicated their preference for a sure bet (values between \$1-\$6 in \$1 increments) or a fifty-fifty gamble between two non-identical values (\$0 - \$6 in \$1 increments). The lateral presentation of the sure bet and gambles were randomized, and each prospect was presented for a random duration based on a Poisson distribution $(\lambda = 6\text{s})$. If the participant answered within the alloted time, their choice was displayed for two seconds before they were shown the outcome for one second. If they did not answer in time, they were shown a late screen for one second. Rounds were separated with a fixation cross whose inter-trial interval was a Poisson distribution $(\lambda = 6\text{s}$, zeros removed$)$. After on-third of of the rounds where choices were made, respondents were asked "How do you feel about the last outcome?" and adjusted a slider ranging from 'very bad' to 'very good'. Participants were paid \$20 per hour and told they would receive winnings from a randomly selected round as bonus compensation.

Using this data from this task, we can group the remaining computational modeling steps into three stages:[@wilson2019]

-   **Simulation & Parameter Recovery**: Use the candidate model (CPUT) to generate 'fake' behavioral data and attempt to recover the parameters of interest following the proposed analysis method for experimental data.

-   **Parameter Estimation:** Find the set of parameters that best account for the experimental data given the candidate model.

-   **Model Comparison:** Compare the candidate models to others (EUT) that may provide alternative explanations of the behavioral data.

Before detailing the methods and results for each stage in my analysis, it is important to understand the mathematics behind our model-fitting.
