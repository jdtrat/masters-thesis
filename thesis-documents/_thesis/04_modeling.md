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

Before detailing the methods and results for each stage in my analysis, it is important to understand the mathematics behind our model-fitting. In the general case, when estimating a model's parameters for a decision-making task, we want to know what parameter(s) best explain the observed choices. For tasks with two outcomes, this can be represented as a binary choice vector where a choice for an option is indicated as a 1 if chosen and a 0 if not.

```{=tex}
\begin{equation*}
\begin{split}
\text{Chose Option 1} =
    \begin{bmatrix}
    1 \\
    1 \\
    1 \\
    0 \\
    1 \\
    0
\end{bmatrix}
\end{split}
\qquad
\begin{split}
\text{Chose Option 2} =
    \begin{bmatrix}
    0 \\
    0 \\
    0 \\
    1 \\
    0 \\
    1
    \end{bmatrix} 
\end{split}
\end{equation*}
```
Consider the prospect depicted in Figure \@ref(fig:sborg-task-description). Given the stated outcomes and probabilities, we can calculate the counterfactual utilities for each option for any $\gamma$ value according to Equations \@ref(#eq:calculating-uc) and \@ref(#eq:eut-cput-transformations). For $0 \leq \gamma \leq 0.5$, we can see the utilities of the sure bet option (\$3) and the gamble (50% chance of \$2 or \$5) in Figure \@ref(fig:sbg-demo-gamma-range)). From this, we might assume that people that place low weight on counterfactual information would more often choose the gamble. At some point, though, there is an indifference point where -- strictly comparing utilities -- an individual is equally likely to choose either option. As people place more weight on counterfactual information, they are more likely to choose the sure bet.

![(\#fig:sbg-demo-gamma-range)Counterfactual utilities for a sample prospect from the sure bet or gamble task simulated for $0 \leq \gamma \leq 0.5$. Blue line represents the counterfactual utility of choosing the 'Sure Bet' and receiving three dollars; red line represents the counterfactual utility of choosing the 'Gamble' and having a 50% chance of receiving two dollars or five dollars. The 'indifference' point of choosing one option over the other occurs when $\gamma \approx 0.16$](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/sbg_demo_figure.png){width=95%}

Although we can determine the utility of each option, we only have peoples' choice behavior from which to fit the model. This means that we need to determine what the most likely parameter value that would give us the observed data. To help answer this question, I looked towards Bayesian statistics, which offers a way to incorporate *prior* information about parameters in a statistical model with observed data to define a *posterior* probability distribution representing the most plausible parameter values.[^1]

[^1]: For excellent resources on Bayesian statistics, I highly recommend the textbooks *Statistical Rethinking: A Bayesian Course with Examples in R and Stan* by Richard McElreath\[@richardmcelreath2020\] and *Bayes Rules! An Introduction to Applied Bayesian Modeling* by Alicia A. Johnson, Miles Q. Ott, and Mine Dogucu.[@dogucu]
