---
bibliography: references.bib
---

# Computational Modeling Concepts

In the last chapter, I addressed my first thesis aim of developing counterfactual predicted utility theory (CPUT) as an alternative to expected utility theory to explain decision-making under risk. I showed how information about each potential outcome (terminal node of Figure \@ref(fig:prospect-diagram-general)) integrates with a counterfactual weighting parameter, $\gamma$, to adjust the utility of one option over another. I walked-through an example calculation assuming $\gamma = 0.1$ (diagrammed in Figure \@ref(fig:prospect-diagram-general)). In this chapter, I begin to address my second thesis aim of assessing the predictive accuracy of CPUT using human choice data from a 'Sure Bet or Gamble' task.[@liebenow2021]

In an attempt to robustly, and transparently, fit CPUT to behavioral data, I follow the general guidelines presented in Robert Wilson and Anne Collins's paper *Ten simple rules for the computational modeling of behavioral data*.[@wilson2019]At a high level, they state that computational modeling allows us to make better sense of behavioral data with mathematical models that may provide insight into mechanisms underlying behavior. Although the exact form of the models differ, the basic steps to assess a model's descriptive and predictive efficacy are similar.

The first two steps Wilson and Collins discuss are designing an experiment and developing a model. For my thesis, I fit CPUT, as formulated in the last chapter, with data collected from an ongoing study by Brittany Liebenow and colleagues[@liebenow2021] where forty-five healthy adults (ages 18-65) were recruited to complete a sure-bet or gamble task (Figure \@ref(fig:sborg-task-description)).

![(\#fig:sborg-task-description)Schematic of a trial from the Sure Bet or Gamble task and subjective rating prompt. A prospect is presented for a random duration based on a Poisson distribution $(\lambda = 6\text{s})$. Assuming a timely response, a choice screen was shown for two seconds followed by an outcome screen for one second. Trials were separated with a fixation cross whose inter-trial interval was a random duration from a Poisson distribution with $\lambda = 3\text{s}$.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/sborg_task-diagram.png){width=95%}

For thirty minutes, participants indicated their preference for a sure bet (values between \$1-\$6 in \$1 increments) or a fifty-fifty gamble between two non-identical values (\$0 - \$6 in \$1 increments). The lateral presentation of the sure bet and gambles were randomized, and each prospect was presented for a random duration based on a Poisson distribution $(\lambda = 6\text{s})$. If the participant answered within the alloted time, their choice was displayed for two seconds before they were shown the outcome for one second. If they did not answer in time, they were shown a late screen for one second. Rounds were separated with a fixation cross whose inter-trial interval was a Poisson distribution $(\lambda = 6\text{s}$, zeros removed$)$. After one-third of of the rounds where choices were made, respondents were asked "How do you feel about the last outcome?" and adjusted a slider ranging from 'very bad' to 'very good'. Participants were paid \$20 per hour and told they would receive winnings from a randomly selected round as bonus compensation.

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
Consider the prospect depicted in Figure \@ref(fig:sborg-task-description). Given the stated outcomes and probabilities, we can calculate the counterfactual utilities for each option for any $\gamma$ value according to Equations \@ref(eq:calculating-uc) and \@ref(eq:eut-cput-transformations). For $0 \leq \gamma \leq 0.5$, we can see the utilities of the sure bet option (\$3) and the gamble (50% chance of \$2 or \$5) in Figure \@ref(fig:sbg-demo-gamma-range)). From this, we might assume that people that place low weight on counterfactual information would more often choose the gamble. At some point (here when $\gamma \approx 0.16$), an individual is equally likely to choose either option. As people place more weight on counterfactual information, they are more likely to choose the sure bet for this prospect.

![(\#fig:sbg-demo-gamma-range)Counterfactual utilities for a sample prospect from the sure bet or gamble task simulated for $0 \leq \gamma \leq 0.5$. Blue line represents the counterfactual utility of choosing the 'Sure Bet' and receiving three dollars; red line represents the counterfactual utility of choosing the 'Gamble' and having a 50 percent chance of receiving two dollars or five dollars. The 'indifference' point of choosing one option over the other occurs when $\gamma \approx 0.16$.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/sbg_demo_figure.png){width=95%}

Although we can determine the utility of each option, we only have peoples' choice behavior from which to fit the model. This means that we need to determine what the most likely parameter value that would give us the observed data. To help answer this question, I looked towards Bayesian statistics, which offers a way to incorporate *prior* information about parameters in a statistical model with observed data to define a *posterior* probability distribution representing the most plausible parameter values.[^model-concepts-1]

[^model-concepts-1]: For excellent resources on Bayesian statistics, I highly recommend the textbooks *Statistical Rethinking: A Bayesian Course with Examples in R and Stan* by Richard McElreath[@richardmcelreath2020] and *Bayes Rules! An Introduction to Applied Bayesian Modeling* by Alicia A. Johnson, Miles Q. Ott, and Mine Dogucu.[@dogucu]

The first step in performing Bayesian inference is to construct a *prior* probability model for our parameters of interest. Because I end up fitting multiple models (e.g., perform model comparison), I will outline the procedure here in the general case using $\theta$ to represent a given model's parameters. Our prior probability model, $\pi(\theta)$ contains available information about a parameter's distribution before observing any data. This information could come from, for example, previous experiments. If no information is available, we might assume a uniform prior distribution, which assigns equal probability to all values within the defined range.

The next step in Bayesian inference involves calculating the *likelihood* of observing data given a set of parameters, $\theta$. For my analysis, I focused on the likelihood that an individual chose Option 1 (the sure bet), which we represent as follows:

```{=tex}
\begin{equation}
L(\text{Chose Option 1}|\theta)
(\#eq:likelihood)
\end{equation}
```
To translate binary choice behavior to a likelihood, we need a way of relating an option's utilities into a probability. This can be done with a softmax choice rule, a logistic transformation of the difference in utilities for each option:[@sokol-hessner2009]

```{=tex}
\begin{equation}
P(\text{Chose Option 1}) = \frac{1}{1 + e^{(-\tau \cdot (U_1 - U_2))}}
(\#eq:softmax-choose-1)
\end{equation}
```
where $U_1, U_2$ are the utilities (according to the proposed model such as CPUT or EUT), and $\tau > 0$ is a sensitivity parameter that relates how sensitive one's choice is to a difference in utilities. To complement Figure \@ref(fig:sbg-demo-gamma-range), we can see how the probability of choosing the sure bet changes as $\gamma$ (and subsequently the utility of Option 1) increases:

![(\#fig:sbg-demo-softmax-figure)Probability of choosing Option 1 ('Sure Bet') calculated by a logistic transformation of the difference in utilities for the counterfactual utilities of each option as shown in Figure \@ref(fig:sbg-demo-gamma-range) across $0 \leq \gamma \leq 0.5$. As  $\tau$ increases, individuals are more likely to maximize utility. Note that the 'indifference' point of choosing one option over the other occurs when $\gamma \approx 0.16$, regardless of $\tau$.](/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/sbg_demo_softmax_figure.png){width=95%}

The third step in Bayesian inference is to generate the posterior distribution, $Pr(\theta|\text{Choices})$, which represents the range most likely range of parameter values given the observed choices. In terms of our analysis, the posterior distribution is defined as follows:

```{=tex}
\begin{equation}
Pr(\theta|\text{Choices}) \propto L(\text{Chose Option 1}|\theta) \times \pi(\theta)
(\#eq:posterior-propto)
\end{equation}
```
Where the probability of $\theta$ being some value given the observed choice data, our posterior distribution $Pr(\theta|\text{Choices})$, is *proportional to*[^model-concepts-2] the likelihood of observing that data given that parameter value, $L(\text{Chose Option 1}|\theta)$, times the prior plausibility of the parameter being that value, $\pi(\theta)$.

[^model-concepts-2]: Note that this is a form of Bayes' theorem that excludes the marginal likelihood of the data -- that is, the total probability of observing the data -- which acts as a normalizing constant to ensure the posterior distribution is a valid probability density function. For more details on this 'posterior shortcut', see section 2.3.6 of *Bayes Rules!*.[@dogucu]

To estimate the posterior distribution, I used two 'Markov Chain Monte Carlo' methods. For my first attempt, I manually implemented the Metropolis-Hastings algorithm.[@haines2018; @dogucu] At a high level, the Metropolis-Hastings algorithm involves the following steps:

For iteration $n \in 1:N$ do the following:

1.  Propose a value, $\theta_n^*$ near the current estimate $\theta_n$, propose a value $\theta^*$
2.  Calculate the acceptance probability for proposal $\theta_n^*$ defined as the ratio between the posterior distribution evaluated at $\theta_n^*$ to that of $\theta_n$
3.  Draw a random number between zero and one. If it is less than or equal to the acceptance probability, set $\theta_{n+1} = \theta_n^*$, otherwise $\theta_{n+1} = \theta_n$.

I then validated my implementation by comparing it to posterior distributions computed with a hierarchical Bayesian approach. Hierarchical Bayesian Analysis was implemented in the probabilistic programming language, Stan, which implements an efficient variant of Markov Chain Monte Carlo called the Hamiltonian Monte Carlo sampler.[@standevelopmentteam2022] Hierarchical Bayesian Analysis allows for simultaneous estimation of individual and group-level parameters in a mutually-constraining fashion. This has been shown to improve parameter estimates relative to other methods (e.g., maximum likelihood estimation), resulting in more stable and reliable estimates for individual-level parameters as they are informed by group trends.[@ahn2011]

The specific implementation of hierarchical models I follow has been detailed elsewhere.[@ahn2017] At a high level, individual-participant parameters are assumed to be drawn from normally distributed group-level distributions. Bounded parameters, such as $\gamma$, were estimated in an unconstrained space and transformed with the 'Matt Trick',[@standevelopmentteam2022] an inverse Probit transformation. This is to optimize the MCMC sampling.[@ahn2017] More formally, 

```{=tex}
\begin{equation}
\begin{aligned}
\mu_\theta &\sim \text{Normal}(0,1) \\
\sigma_\theta &\sim \text{Half-Normal}(0,0.2) \\
\mathbf{\theta}^\prime &\sim \text{Normal}(0,1) \\
\mathbf{\theta} &\sim \text{Probit}^{-1}(\mu_\theta + \sigma_\theta \cdot \mathbf{\theta}^\prime) \times U.B.
\end{aligned}
(\#eq:matt-trick)
\end{equation}
```

where $- \infty < \mu_\theta < + \infty$ and $- \infty < \sigma_\theta < + \infty$ are the group-level mean and standard deviation, respectively; $\theta^\prime$ is the unconstrained parameter that gets transformed via the inverse Probit transformation and scaled to some upper bound, $U.B$. As an example, for $\gamma$, $U.B. = 1$; for $\tau$, $U.B. = 30$. This non-centered reparameterization results in a uniform prior for individual participants' parameters across the full range.[@ahn2014; @ahn2017]

With a better grasp of the concepts I employed for my thesis, it's time to revisit each of the three stages of computational modeling I introduced in the chapter introduction. [@wilson2019] In the next chapter, I outline my methods and results from simulating choice data according to counterfactual predicted utility theory, estimating parameters from observed data, and comparing evidence from seven candidate models.
