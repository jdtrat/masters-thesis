---
bibliography: references.bib
---



# Computational Modeling Methods and Results

Using the computational modeling techniques I introduced in the previous chapter, I examined the ability of counterfactual predicted utility theory to explain human choice behavior on the sure bet or gamble task[@liebenow2021]. In this chapter, I present my methods and results concurrently following the three stages adapted from Wilson and Collins discussed in Section \@ref(computational-modeling-concepts).[@wilson2019] I start by simulating choice behavior generated from counterfactual predicted utility theory to confirm that the experimental design elicits behaviors assumed in the model. I then find estimate individual- and group-level parameters for three candidate models. Lastly, I quantify model fit with model comparison techniques.

## Simulating Choice Data

As a first step to simulate choice data on the sure-bet or gamble task, I looked at the results of Kahneman and Tversky's prospect theory[@kahneman1979] paper in search of information to inform a prior distribution. Although there is no record for each subject's response on specific prospects, Kahneman and Tversky report the proportion of people that chose each option. For each of the nine prospects that follow the form depicted in Figures \@ref(fig:prospect-diagram-general) and \@ref(fig:prospect-diagram-kt), I used the reported proportions to simulate choice behavior of one-hundred subjects.

I assumed that $\gamma$ was uniformly distributed between zero and one and fixed the softmax sensitivity parameter, $\tau = 2.2$ following the conceptual visualizations from Section \@ref(visualizing-bayes-theorem-with-choice-datamodeling-concepts-2) consistent with the literature.[@sokol-hessner2009] I ran ten-thousand iterations using the Metropolis-Hastings algorithm described in the previous chapter. The posterior distribution, depicted in Figure \@ref(fig:kt-post-approx), approximates a $\text{Beta}(1.1,1.1)$ distribution. This means that, in estimating $\gamma$ with the choice proportion data from Kahneman and Tversky, it's plausible that $\gamma$ is any value between zero and one, though a little less likely towards the tails.

![(\#fig:kt-post-approx)Posterior distribution of $\gamma$ as estimated from ten-thousand iterations of a Metropolis-Hastings algorithm assuming a uniform prior with choice proportion data from Kahneman and Tversky's prospect theory. This posterior distribution (orange-red line) is approximated with a $\text{Beta}(1.1,1.1)$ distribution (blue-gray), indicating slightly less plausibility to $\gamma$ values near the bounds of the zero-to-one interval relative to central values.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/kt_post_approx_figure.png){width=95%}

Given the sure bet or gamble task design, participants saw a random subset of 252 prospects. The posterior distribution for $\gamma$ recovered from Kahneman and Tversky's choice proportion data was used to generate $\gamma$ values for fifty subjects. I then simulated their choices on each of the 252 prospects, again fixing the softmax sensitivity parameter $\tau = 2.2$. For five-thousand iterations of the Metropolis-Hastings algorithm, I sampled from the posterior distribution for each simulated subject.[^model-results-1] With the same simulated data, I fit a hierarchical Bayesian model in Stan with the non-centered reparameterization as described in the computational modeling concepts chapter. I ran the Hamiltonian Monte Carlo (Stan) sampler for 5000 iterations across four parallel sampling chains for a total of 20,000 samples from the posterior.[^model-results-2] 

I chose to estimate the posterior distribution with both the Metropolis-Hastings and Hamiltonian Monte Carlo algorithms to highlight the similarity in posterior parameter estimation techniques discussed in Chapter \@ref(computational-modeling-concepts). Figure \@ref(fig:sbg-sim-parameter-recovery) shows the 95% highest density interval recovered from each sampler's posterior distribution. For most subjects, the simulated $\gamma$ values are within the highest-density interval. This suggests that the sure-bet or gamble task is able to elicit the behaviors of interest in a way measurable with counterfactual predicted utility theory. 

With the confirmation that I am able to accurately recover simulated parameters from the sure-bet or gamble task, I move on to the next stage of computational modeling, parameter estimation. To do so, I use the hierarchical Bayesian model in Stan for more stable and reliable estimates and computational efficiency.

[^model-results-1]: Although I simulated participants' counterfactual weighting term following the posterior from Kahneman and Tversky's choice proportion data, $\gamma \sim \text{Beta}(1.1,1.1)$, I used a uniform prior again when sampling from the Metropolis-Hastings algorithm for optimizing my analytical workflow. I felt this was justified because $\text{Beta}(1.1,1.1)$ is (relatively) uninformative prior and the decision would not have a large impact on the recoverability of parameter values (with the possible exception of model fitting time). Further, I wanted to maintain consistency with the uniform distribution of the individual-level priors defined in the hierarchical Bayesian model with the inverse Probit transformation.[@ahn2014; @ahn2017]

[^model-results-2]: I included a warmup of 2000 iterations, during which time the Hamiltonian Monte Carlo sampling algorithm was tuned to improve efficiency when sampling from the posterior distribution. See [Chapter 9 of the CmdStan User's Guide](https://mc-stan.org/docs/2_29/cmdstan-guide/mcmc-config.html) for more information on 'MCMC Sampling using Hamiltonian Monte Carlo'[@standevelopmentteam2022]

![(\#fig:sbg-sim-parameter-recovery)95 percent highest density interval of the posterior distribution of $\gamma$ as estimated from five-thousand iterations of a Metropolis-Hastings algorithm and five-thousand iterations across four parallel chains with the Hamiltonian Monte Carlo Sampler (estimated using Stan). The simulated gamma values for each subject are represented as sky-blue dots if they fall within the highest density interval and red dots if they fall outside of it.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/sbg_sim_parameter_recovery_figure.png){width=95%}

## Parameter Estimation

To assess counterfactual predicted utility theory's validity as a theory of decision-making under risk, I estimate parameters for it in comparison with expected utility theory. I primarily do this for two reasons:

1.  Counterfactual predicted utility theory suggests that, if $\gamma = 0$, the counterfactual utility of an option is equivalent to the expected value. The expected value is a special case of expected utility theory where the risk sensitivity parameter, $\rho = 1$.
2.  The sure-bet or gamble task does not include losses, which prohibits me from comparing counterfactual predicted utility theory to prospect theory.

In addition to directly comparing CPUT with EUT, I wanted to see how different risk preferences might affect interact with counterfactual information to inform choice behavior. In total, I fit three models:

* **CPUT + Softmax Sensitivity**: This model looks at how counterfactual information informs choice behavior. Its estimated parameters are the counterfactual weighting term, $\gamma$, and the sensitivity to differences in choice utilities, $\tau$. 
* **CPUT + Risk Sensitivity + Softmax Sensitivity**: This model looks at how differences in risk sensitivity may interact with counterfactual information to inform choice behavior. Its estimated parameters are the counterfactual weighting term, $\gamma$, the risk sensitivity term, $\rho$, and the sensitivity to differences in choice utilities, $\tau$. 
* **EUT + Softmax Sensitivity**: This model looks at how differences in risk sensitivity informs choice behavior. Its estimated parameters are the risk sensitivity term, $\rho$, and the sensitivity to differences in choice utilities, $\tau$.

All models were sampled for 5000 iterations across four parallel chains with the hierarchical Bayesian model formulation described in Section \@ref(posterior-estimation-with-markov-chain-monte-carlo).[@ahn2017] For each model, chain convergence for group-level and transformed individual-level parameters was checked with Gelman-Rubin statistics, $\hat{R} \leq 1.1$, suggesting between-chain variance is lower than within chain variance.[@gelman1992] The group-level posterior distributions for parameters fit from each model are shown in Figure \@ref(fig:population-level-post-param-figure).

![(\#fig:population-level-post-param-figure)Posterior distribution of group-level parameter estimates for each model. Relative distributions for $\gamma$ (left), $\rho$ (middle), and $\tau$ (right) underlined by 95 percent highest density intervals for each model with the median indicated. 'CPUT + Softmax Sensitivity' model shown in blue with estimated parameters for $\gamma, \tau$; 'EUT + Softmax Sensitivity' shown in red with estimated parameters for $\rho, \tau$; 'CPUT + Risk Sensitivity + Softmax Sensitivity' shown in yellow with estimated parameters for $\gamma, \rho, \tau$.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/population_posterior_plot.png){width=95%}

## Model Comparison

With the estimated posterior distributions for each model type, I sought to determine which model best explains choice behavior. To do this, I used three different methods to compare models, the results of which are summarized in Table \@ref(fig:model-fit-summary-table):

1.  Posterior predictive checks where, for each model, I simulated choices given the (joint) posterior distribution of each participants' estimated model parameter(s). This was included in Stan's generated quantities block, which is only executed after a posterior sample has been generated. [@standevelopmentteam2022] I then compared the percentage of predicted choices that matches the observed data and summarized the mean and standard deviation for each model.

2.  Comparing the marginal likelihoods of each model. This is the probability of observing the choice behavior for a given model, $M$, $P(\text{Choices}|M)$. The marginal likelihood of each model was estimated using bridge sampling.[@gronau2020] Marginal likelihoods are often included in calculations of Bayes factors, which describes the relative evidence in favor of one model over another by quantifying the ratio between the probability of observing the data given two models. The marginal likelihoods are computed as log-scaled for computational efficiency, which means that the more positive, or less negative, marginal likelihood indicates a better fit.

3.  Assessing penalized model fit with each model's leave-one-out cross validation predictive accuracy.[@vehtari2017] This is approximated with importance sampling of the posterior distribution to calculate the expected log pointwise predictive density (ELPD), which is the logged sum of pointwise posterior predictive distribution for held out data. By multiplying the ELPD by negative two, we get the leave-one-out information criterion, LOOIC. This transformation makes it easier to compare with other information criterion (e.g., AIC, DIC), highlighting the penalization of model complexity.[@plummer2008] I include both ELPD and LOOIC for easy comparison. Note that a less negative ELPD and a smaller (closer to zero) LOOIC are indicative of better model fits.


![(\#fig:model-fit-summary-table)Parameters for, and description of, the different models fit on human choice data from the sure-bet or gamble task along with model comparison metrics. Posterior predictive choice accuracy represents the mean and standard deviation of correctly predicted choices for individual participants given simulations from the (joint) posterior distribution. ELPD Predictive Density and LOOIC details how well models perform on unobserved data (leave-one-out cross validation). Parantheses for ELPD and LOOIC indicate Monte Carlo sampling error. Marginal likelihood model evidence indicates the plausibility of the data given each model with parantheses representing the interquartile range of the likeihood estimations. In general, better models have smaller LOOIC and higher posterior predictive choice accuracy, ELPD predictive accuracy (less negative), and marginal likelihood model evidence (less negative). CPUT + Softmax Sensitivity and EUT + Softmax Sensitivity are highlighted for easy reference when discussed.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/model_fit_summary_table.png){width=95%}

### EUT versus CPUT 

Taken together, the model comparison metrics show that EUT provides a better explanation of both the observed data on the sure bet or gamble task ($BF_{\frac{\rho, \tau}{\gamma, \tau}}$ = 48.07) and generalizes better than CPUT ($\text{LOOIC}_{\rho, \tau} - \text{LOOIC}_{\gamma, \tau}$ =  -381.34). The posterior predictive choice accuracy -- the percentage of choices simulated with the posterior estimate of each subject's parameters -- are similar, though better for EUT (83.8%) compared to CPUT (82.3%). 

Further, the group-level posterior parameter distribution for $\tau$, the common variable to these two models, is higher for EUT than CPUT (95% HDI for EUT is [1.51, 2.62] with median of 2.05; CPUT = [0.97, 1.31] with median of 1.13). This suggests more random utility maximizing decisions for EUT relative to more random choice behavior for CPUT.

### CPUT + Risk Sensitivity versus EUT

Interestingly, the risk sensitivity term, $\rho$, from EUT with the counterfactual weighting from CPUT offers the best explanation for the observed human choice data ($BF_{\frac{\text{CPUT + Softmax + Risk Sensitivity}}{\rho, \tau}}$ = 109.89). This does come with a cost of decreased generalizability ($\text{LOOIC}_{\rho, \tau} - \text{LOOIC}_\text{CPUT + Softmax + Risk Sensitivity}$ = -158.18) and increased posterior predictive choice accuracy (83.9% relative to EUT's 83.8%). 

Further, the group-level posterior parameter distribution for $\tau$ depict similar sensitivities to utility differences (95% HDI for 'CPUT + Softmax + Risk Sensitivity' = [1.45, 2.57] with median of 1.98; 'EUT + Softmax = [1.51, 2.62] with median of 2.05) and risk sensitivity (95% HDI for 'CPUT + Softmax + Risk Sensitivity' = [0.68, 0.94] with median of 0.8; 'EUT + Softmax' = [0.66, 0.93] with median of 0.79).

### CPUT + Risk Sensitivity versus CPUT

Incorporating risk sensitivity to CPUT, mixing the value estimate transformation of EUT (left-hand side of Equation \@ref(eq:eut-cput-transformations)) with that of CPUT (right-hand side of Equation \@ref(eq:eut-cput-transformations)) results in a better explanation for the observed data ($BF_{\frac{\rho, \gamma, \tau}{\gamma, \tau}}$ = 157.95), is more generalizable, ($\text{LOOIC}_{\rho, \gamma, \tau} - \text{LOOIC}_{\gamma, \tau}$ =  -223.16), and has a higher posterior predictive choice accuracy (83.9%) relative to CPUT's (82.3%). 

Interestingly, the group-level posterior parameter distribution for $\tau$ with the CPUT + Risk Sensitivity more closely reflects that of EUT. At the same time, the group-level posterior parameter distribution for $\gamma$ is much more tightly concentrated towards zero than for CPUT (95% HDI for CPUT + Risk Sensitivity is [<0.001, 0.006] with median of 0.002; CPUT = [<0.001, 0.015] with median of 0.006). This suggests more a lower weighting on counterfactual information while accounting for risk preferences. 

---

By most measures, it seems that Expected Utility Theory provides the most generalizable explanation of human choice data on the sure bet or gamble task. Looking at the In the next chapter, I discuss these results and outline next steps to contribute towards a better understanding of the neurobiological basis of decision-making under risk. 
