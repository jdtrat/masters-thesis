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

Consider Pavlov's experiment which showed how repeatedly giving a dog food after ringing a bell will condition the dog to salivate after the bell is rung.[@pavlov2010] This means that, over time, behavior elicited by a stimulus (food) can be evoked by a -- previously -- neutral stimulus (the bell). In one of the first attempts to empirically describe this process, Bush and Mosteller suggest the probability of Pavlov's dog salivating, $P(\text{sal})$, on the *next* presentation of food with the bell (next trial, $tr+1$) is a function of what happened *last* presentation (last trial, $tr-1$) discounted by what was experienced during *this* presentation (food reward this trial, $R_tr$.[@bush1951; @bush1951a]

```{=tex}
\begin{equation}
P(\text{sal})_{\text{tr}+1} = P(\text{sal})_{\text{tr}-1} + \alpha (R_{\text{tr}} - P(\text{sal})_{\text{tr}-1})
(\#eq:bush-mosteller)
\end{equation}
```

Bush and Mosteller's equation computes an average of previously experienced rewards. $0 \leq \alpha \leq 1$ is a learning rate, which modulates the influence of more recent rewards. Bush and Mosteller's work forms the basis for modern approaches to this problem of ***reinforcement learning***.[@glimcher2011a][^2] While discretizing learning into trials is a logical first step to modeling behavior in experimental settings, it's not easily applied to biological systems that continuously interact with their environments.

[^2]: For a more detailed explanation of Bush and Mosteller's work, the development of reinforcement learning for applications to Neuroscience, and much of the material I cover in this chapter, please refer to Paul Glimcher's *Understanding dopamine and reinforcement learning: The dopamine reward prediction error hypothesis* .[@glimcher2011a]

In his 1988 paper, *Learning to Predict by the Methods of Temporal Differences*, Richard Sutton introduces the antecedent for temporal-difference reinforcement learning (TDRL) algorithms.[@sutton1988] The TDRL algorithm provides a computational framework for optimally learning from experience how actions and their associated stimuli lead to rewards. [@sutton-barto-2018] The 'goal' of TDRL algorithms are to estimate the value of a state and use that information to maximize rewards. The crux of the TDRL algorithms is the temporal-difference reward prediction error (TD-RPE). This 'teaching signal' relates an experienced outcome at time $t$ to what was expected.
