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

In combination with their 2016 paper, Kishida and colleagues studied the dopamine levels of humans undergoing elective surgery as part of deep-brain stimulation treatment.[@kishida2011; @kishida2016] Participants viewed a graphical depiction of a (fictitous) stock market's price history (Figure \@ref(fig:kishida2016-task-design); B) and were asked to choose how much of their portfolio (initially valued at \$100) they would invest. For six 'markets', each with twenty-decisions, participants used handheld button boxes (Figure \@ref(fig:kishida2016-task-design); A) to increase or decrease their investment in ten-percent increments. Decision were made while dopamine was recorded from the striatum in light of three pieces of information:

1.  The history of the market price (Figure \@ref(fig:kishida2016-task-design); red-trace in panel B)
2.  The current portfolio value (Figure \@ref(fig:kishida2016-task-design); bottom left box, 126, panel B)
3.  The most recent fractional change in portfolio value (Figure \@ref(fig:kishida2016-task-design); bottom right box, 15.2%, panel B).

\begin{figure}

{\centering \includegraphics[width=0.9\linewidth]{/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/kishida2016-task-design} 

}

\caption{(A) Participants played a sequential-choice game during surgery using button boxes (Left) and a visual display (Right). For each patient, bet size adjustments (e.g., increase bet or decrease bet) and the decision to submit one’s answer were performed with button boxes. (B) Investment game (19, 21): participants view a graphical depiction of the market price history (red trace), their current portfolio value (bottom left box), and their most recent outcome (bottom right box) to decide and submit investment decisions (bets) using a slider bar in 10% increments (bottom center). Bet sizes were limited to 0–100% (in 10% increments) of the participant’s portfolio—no shorting of the market was allowed. During an experiment, a participant played 6 markets with 20 decisions made per market. (C) Timeline of events during a single round of the investment game.*Reprinted with permission from Kishida et al., 2016.*}(\#fig:kishida2016-task-design)
\end{figure}

In their 2011 paper, they asked one person, MH, to complete the sequential investment task. In line with prior work relating dopamine with unexpected financial outcomes [@zaghloul2009], Kishida and colleagues observed a strong correlation between MH's dopamine levels and the market value during the investment task.[@kishida2011] In 2016, Kishida and colleagues published results from seventeen humans. Here, they explicitly tested the hypothesis that fluctuations in dopamine released in the human striatum encode TD-RPEs.[@kishida2016] For the sequential investment task, RPEs were calculated as the difference between an investment's return on a given trial relative to the expectation defined by the average return from preceding trials.

Contrary to the hypothesis that dopamine fluctuations in the striatum should track TD-RPEs,[@montague2004; @montague1996; @schultz1997a; @bayer2005a; @hart2014; @roesch2007] the authors found that dopamine fluctuations encode an integration of RPEs with counterfactual prediction errors (CPEs). As discussed in the last chapter, counterfactual signals are an explicit comparison between the present state to alternative ones.[@nealj.roese1997; @liu2016a] For the sequential investment task, CPEs are defined as the difference between the participant's actual return for a trial and what could have been had they invested more or less. This means that dopamine release is a result of the integration of RPEs and CPEs for a given trial. Formally:

```{=tex}
\begin{equation}
\begin{aligned}
\text{dopamine transient} &\propto \text{RPE} - \text{CPE} \\
&\propto \text{RPE} - r_{tr}(1 - b_{tr})
\end{aligned}
(\#eq:rpe-cpe)
\end{equation}
```
where $b_{tr}$ is the individual's factional investment at trial $tr$ and $r_{tr}$ is the relative change in market price. As Kishida and colleagues note, the intuition for Equation \@ref(eq:rpe-cpe) is that better-than-expected outcomes (positive RPEs, increased dopamine release), that *could have been even better* (positive CPEs) should be reduced in value. Similarly, worse-than-expected outcomes (negative RPEs, decreased dopamine release), that *could have been even worse* (negative CPEs) should be increased in value. Importantly, these empirical terms are consistent with the subjective feelings (e.g., regret and relief), one experiences in light of a given outcome.

Figure \@ref(fig:kishida2016-rpe-inversion) depicts changes in dopamine levels as a function of bet size. Consider the 'higher bets' panel (left) where individuals bet close to the maximum amount possible. When they were rewarded with a positive change in portfolio value, the dopamine concentrations increased (green; positive RPEs). Similarly, when the portfolio value decreased, dopamine levels dipped (red; negative RPEs). With high bets, the difference between what they earned and what they could have earned had they bet more is minimal, as is the CPE. With medium bets (middle panel), however, the difference between what individuals experienced and what they could have experienced had they bet more increases. This means that the absolute magnitude of the CPE goes up which subtracts from the dopamine release predicted by positive and negative RPEs. When the CPEs are maximal (small bets; right panel), we observe an inversion in dopamine release in response to positive and negative RPEs.

\begin{figure}

{\centering \includegraphics[width=0.9\linewidth]{/Users/jt/Desktop/R/Academic/Kishida Lab/masters-thesis/thesis-documents/figures/kishida2016-rpe-inversion} 

}

\caption{RPE encoding by dopamine transients invert as a function of bet size. Dopamine responses to equal absolute magnitude positive and negative RPEs (−0.75 > RPE > +0.75) when bets are high (higher bets, 100–90%) (Left), medium (medium bets, 80–60%) (Center), or low (lower bets, 50–10%) (Right). For all three plots, mean normalized dopamine responses (±SEM) to positive RPEs (green traces) and negative RPEs (red traces). Inset legends show sample sizes for event types. Two-way ANOVA (RPE-sign and time: 700 ms following and including outcome reveal) reveals a significant difference comparing dopamine responses for positive and negative RPEs following higher bets [FRPE-sign(1,7) = 21.17, P = 0.00] and lower bets [FRPE-sign(1,7) = 32.64, P = 0.00] but not medium bet sizes [FRPE-sign(1,7) = 0.15, P = 0.6957]. Asterisks indicate significant difference between red and green traces: P < 0.05, post hoc, two-sample t test following ANOVA with time and RPE-sign as the two main factors. Asterisks with parentheses indicate Bonferroni correction for multiple comparisons. For low bets (i.e., large CPEs), only those events where the market price change and the RPE-sign are the same are considered. Horizontal axis: time (ms) from outcome reveal (blue arrowhead); vertical axis: mean change in normalized dopamine response.*Reprinted with permission from Kishida et al., 2016.*}(\#fig:kishida2016-rpe-inversion)
\end{figure}

The 'superposed error signals about actual and counterfactual reward' described in Kishida and colleagues' paper[@kishida2016] directly inspired my thesis work. They provide an empirical, neurobiologically plausible framework for investigating decision-making under risk. The theoretical and quantitative underpinnings of classical and behavioral economic theories of decision-making align well with the computational reinforcement learning literature described in this chapter. The explicit probability and outcome structure for risky prospects afford the opportunity to internalize future states and (potentially) incorporate anticipated counterfactual events into the decision-making process. In the next chapter, I derive 'Counterfactual Predicted Utility Theory' as a new theory of decision-making under risk.
