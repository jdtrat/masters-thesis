---
bibliography: references.bib
---

# Neurobiology of Decision-Making

In the last chapter, I used the term prospect to describe a problem that has two options with stated probabilities and outcomes. In the experimental literature, these prospects are 'description-based'. Formally, researchers who use description-based prospects in their experiments ask participants to select one of the available options that are presented along with a complete description of a non-trivial problem.[@barron2003a]

When looking towards the neuroscientific literature of decision-making, a different type of experimental paradigm is perhaps more common: 'experiential' or 'feedback-based' decisions.[@barron2003a]. By interacting with the environment, humans (and other animals) associate the consequences of different actions and adapt their behavior accordingly. In other words, we learn. We learn over time that we want to eat sushi for dinner, it's faster to take the highway to work, and we're better off if we go to sleep before midnight.[^1]

[^1]: These learned experiences are in response to the choices posed in the thesis's introduction.

Looking towards the animal learning field, we see many accounts of experiential learning. To list a few:

-   Thorndike's Law of Effect states that an animal's behavior can be modified by the consequences of an action.[@thorndike1911]

-   Classical (Pavlovian) conditioning details learning to predict rewards or punishments from a stimuli independent of any action-taken.[@pavlov2010]

-   Operant (instrumental) conditioning involves learning how rewards and punishments are contingent upon one's actions.[@b.f.skinner1938]

That a behavior (an action or decision) is predicated upon prior experiences allows us to formalize the relationship between states. Here, I use state in reference to the representation of stimuli. For example, the state $s \in S$ at time $t$ ($s_t$) may be comprised of internal states (hunger, thirst, fatigue) or external ones (light, music, temperature).

Consider Pavlov's experiment which showed how repeatedly giving a dog food after ringing a bell will condition the dog to salivate after the bell is rung.[@pavlov2010] This means that, over time, behavior elicited by a stimulus (food) can be evoked by a -- previously -- neutral stimulus (the bell). In one of the first attempts to empirically describe this process, Bush and Mosteller suggest the probability of Pavlov's dog salivating, $P(\text{sal})$, on the *next* presentation of food with the bell (next trial, $tr+1$) is a function of what happened *last* presentation (last trial, $tr-1$) discounted by what was experienced during *this* presentation (food reward this trial, $R_{tr}$.[@bush1951; @bush1951a]

```{=tex}
\begin{equation}
P(\text{sal})_{\text{tr}+1} = P(\text{sal})_{\text{tr}-1} + \alpha (R_{\text{tr}} - P(\text{sal})_{\text{tr}-1})
(\#eq:bush-mosteller)
\end{equation}
```
Equation \@ref(eq:bush-mosteller) computes an average of previously experienced rewards. $0 \leq \alpha \leq 1$ is a learning rate, which modulates the influence of more recent rewards. Bush and Mosteller's work forms the basis for modern approaches to this problem of ***reinforcement learning***.[@glimcher2011a][^2] While discretizing learning into trials is a logical first step to modeling behavior in experimental settings, it's not easily applied to biological systems that continuously interact with their environments.

[^2]: For a more detailed explanation of Bush and Mosteller's work, the development of reinforcement learning for applications to Neuroscience, and much of the material I cover in this chapter, please refer to Paul Glimcher's *Understanding dopamine and reinforcement learning: The dopamine reward prediction error hypothesis*.[@glimcher2011a]

In his 1988 paper, *Learning to Predict by the Methods of Temporal Differences*, Richard Sutton introduces the antecedent for temporal-difference reinforcement learning (TDRL) algorithms.[@sutton1988] The TDRL algorithm provides a computational framework for optimally learning from experience how actions and their associated stimuli lead to rewards. [@sutton-barto-2018]

The 'goal' of TDRL algorithms are to estimate the value of a state and use that information to maximize rewards. This is achieved with a 'teaching signal', called the temporal-difference reward prediction error (TD-RPE), which relates the value of being in a state at time $t$ to what was expected previously.

```{=tex}
\begin{equation}
\delta_t = [\text{outcome}_t + \gamma V(S_{t+1})] - V(S_t)
(\#eq:td-rpe)
\end{equation}
```
The current value of a state is the sum of any outcome experienced at time $t$ plus the expectation of future outcomes from being in said state. This expectation of future values, $V(S_{t+1})$ is modulated by the temporal-discounting parameter $0 \leq \gamma \leq 1$ and can preferentially weight more immediate outcomes relative to future ones.

In total, Equation \@ref(eq:td-rpe) shows the TD-RPE, $\delta_t$, as the difference between the current value of a state and the most recent expectation for that state's value, $V(S_t)$. On each time step, $\delta_t$ is used to update the estimated value of the current state as formulated in \@ref(eq:td-value-update). Here, $0 \leq \alpha \leq 1$ is the learning rate and controls how much weight an individual places on the estimated value of the current state in light of the TD-RPE.

```{=tex}
\begin{equation}
\hat{V}(S_t) \leftarrow V(S_t) + (\alpha \cdot \delta_t)
(\#eq:td-value-update)
\end{equation}
```

In the 1990s, Read Montague, Peter Dayan, and colleagues show evidence suggesting that TD-RPEs are reflected by fluctuations in the activity of mesencephalic dopaminergic neurons.[@montague1996; @schultz1997] The TD-RPE hypothesis of dopamine neurons is consistent with behavioral and neural results in rodents, [@hart2014] non-human primates, [@tomasljunberg1992; @schultz1993; @schultz1998; @bayer2005] and humans.[@zaghloul2009; @kishida2011; @moran2018; @bang2020] Because of this biologically conserved mechanism for experiential learning, we can investigate the neural basis of choice behavior empirically with TDRL algorithms. In 2011, these experiments saw an exciting development when Ken Kishida and colleagues used fast-scan cyclic voltammetry to measure dopamine fluctuations in the human striatum with a sub-second temporal resolution. [@kishida2011] 

Kishida and colleagues studied the dopamine levels of one patient, MH, who suffered from late-stage Parkinson's disease. While undergoing an elective surgery as part of deep-brain stimulation treatment, MH completed a sequential decision-making task. MH viewed a graphical depiction of a (fictitious) stock market's price history and was asked to choose how much of their portfolio (initially valued at $100) they would invest. MH played through six 'markets', each with twenty-decisions, and made choices -- with the carbon fiber electrode recording from the right caudate -- based on (i) the history of the market price, (ii) the current portfolio value, and (iii) the most recent fractional change in portfolio value. In line with prior work relating dopamine with unexpected financial outcomes [@zaghloul2009], Kishida and colleagues observed a strong correlation between MH's dopamine levels and the market value during the investment task.[@kishida2011] 

