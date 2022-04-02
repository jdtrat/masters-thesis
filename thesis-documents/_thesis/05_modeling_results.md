---
bibliography: references.bib
---

# Modeling Methods and Results

Using the computational modeling techniques I introduced last chapter, I examined the ability of counterfactual predicted utility theory to explain human choice behavior on the sure bet or gamble task[@liebenow2021]. To do this, I first simulated choice behavior generated from counterfactual predicted utility theory to confirm that the experimental design elicits behaviors assumed in the model.[@wilson2019] With this confirmation, I fit variations of counterfactual predicted utility theory with and without risk-aversion parameter of expected utility theory and counterfactual predicted utility theory with that the task was able to relative to the veracity of counterfactual predicted utility theory as a generative theory of decision-making under risk. I did this by comparing the mode

First up is the explicit simulation and parameter recovery. In order to determine whether or not the sure bet or gamble task could capture the parameters of interest, I simulated fifty subjects

## Simulating Choice Data

As a first step to simulate choice data on the sure-bet or gamble task, I looked at the results of Kahneman and Tversky's prospect theory[@kahneman1979] paper in search of information to inform a prior distribution. Although there is no record for each subject's response on specific prospects, Kahneman and Tversky report the proportion of people that chose each option. For each of the nine prospects that follow the form depicted in Figures \@ref(fig:prospect-diagram-general) and \@ref(fig:prospect-diagram-kt), I used the reported proportions to simulate choice behavior of one-hundred subjects.

I assumed that $\gamma$ was uniformly distributed between zero and one and fixed the softmax sensitivity parameter, $\tau = 1$. I ran ten-thousand iterations using the Metropolis-Hastings algorithm described in the previous chapter. The posterior distribution, depicted in Figure \@ref(fig:kt-post-approx), approximates a $\text{Beta}(1.1,1.1)$ distribution. This means that, in estimating $\gamma$ with the choice proportion data from Kahneman and Tversky, it's plausible that $\gamma$ is any value between zero and one, though a little less likely towards the tails.

![(\#fig:kt-post-approx)Posterior distribution of $\gamma$ as estimated from ten-thousand iterations of a Metropolis-Hastings algorithm assuming a uniform prior with choice proportion data from Kahneman and Tversyk's prospect theory[@kahneman1979]. This posterior distribution (orange-red line) is approximated with a $\text{Beta}(1.1,1.1)$ distribution (blue-gray), indicating slightly less plausibility to $\gamma$ values near the bounds of the zero-to-one interval relative to central values.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/kt_post_approx_figure.png){width=95%}

Given the task design, participants saw a random subset of 252 prospects. The posterior distribution for $\gamma$ recovered from Kahneman and Tversky's choice proportion data was used to generate $\gamma$ values for fifty subjects. I then simulated their choices on each of the 252 prospects, again fixing the softmax sensitivity parameter $\tau = 1$. For five-thousand iterations of the Metropolis-Hastings algorithm, I sampled from the posterior distribution for each subject[^1] sampled from the posterior distribution for each simulated subject.

[^1]: Although I simulated participants' counterfactual weighting term following the posterior from Kahneman and Tversky's choice proportion data, $\gamma \sim \text{Beta}(1.1,1.1)$, I used a uniform prior again when sampling from the Metropolis-Hastings algorithm for optimizing my analytical workflow. I felt this was justified because $\text{Beta}(1.1,1.1)$ is (relatively) uninformative prior and the decision would not have a large impact on the recoverability of parameter values (with the possible exception of model fitting time). Further, I wanted to maintain consistency with the uniform distribution of the individual-level priors defined in the hierarchical Bayesian model with the inverse Probit transformation.[@ahn2014; @ahn2017]

Using the same simulated choice data, I fit a hierarchical Bayesian model in Stan with the parameterization as described in the last chapter.

## Parameter Recovery

## Model Comparison
