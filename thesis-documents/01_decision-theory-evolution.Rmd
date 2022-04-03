---
bibliography: references.bib
---

# Evolution of Decision Theory

Imagine you are in a casino. You are asked to play a new type of roulette. This time only, there is no cost to you for participating. The rules are simple: pick the red option or the black option. If you choose the red option, the dealer will give you three-thousand dollars. If you pick the black option, the dealer will spin the wheel and release the ball. This wheel is weighted so that you have an 80% chance of getting four-thousand dollars and a 20% chance of getting zero dollars. Which option would you choose? Why?

If you were an economic theorist, I would bet that you would choose the black option because it has a higher expected value. Expected values, $EV$, are calculated by multiplying the magnitude, $x$, of an outcome by the probability, $p$, it will occur. In this example, the red option leads to three-thousand dollars with certainty. The black option leads to four-thousand dollars with an 80% chance (a probability of 0.8).

```{=tex}
\begin{equation}
\begin{aligned}
\text{EV}(\text{Red}) &= x_{\text{Red}} \times p_{\text{Red}} \\
&= 3000 \times 1 \\
&= 3000 \\
\\
\text{EV(Black)} &= x_{\text{Black}} \times p_{\text{Black}} \\
&= 4000 \times 0.8 \\
&= 3200
\end{aligned}
(\#eq:ev-red-black)
\end{equation}
```
Looking at the expected values for each option calculated in \@ref(eq:ev-red-black), we see $\text{EV}(\text{Red}) > \text{EV(Black)}$. Therefore, the rational choice according to a classical economist is the black option.

The new type of roulette presented above is a prototypical example of a *prospect* -- a choice between two options with explicit probabilities and outcomes -- used to investigate decision-making under risk. The normative measure of monetary reinforcements allows decision-researchers to probe the mechanisms underlying choice behavior. When participants have a similar reference point, choosing the option with a higher expected monetary payout is straightforward (at least mathematically speaking). However, the decision to accept a risky gamble like the one described above may change if you ask someone with an annual income over one-million dollars compared to someone with an income under fifty-thousand dollars.

This introduces the idea of diminishing marginal returns. The value that a millionaire places on a certain three-thousand dollars (option 1 from above) may not be enough to dissuade her from a probable four-thousand dollars. For the other person, an assured three-thousand dollars, though, is much more valuable than a gamble. This idea is formalized by the term 'utility', coined by Swiss mathematician Daniel Bernoulli. Utiity refers to the subjective or "moral value" of a decision's outcome. As Bernoulli wrote: [@danielbernoulli1954]

> The price of the item is dependent only on the thing itself and is equal for everyone; the utility, however, is dependent on the particular circumstances of the person making the estimate.

Bernoulli's concept formed the basis for von Neumman and Morgenstern's Expected Utility Theory (EUT). [@vonneumann1947] EUT suggest that people ought to -- and do -- choose in a manner that maximizes their expected utility. After EUT was axiomatized, its validity was called into question. Notably, the Allais paradox suggests that rationality defined by maximizing expected utility neglects a specific element in the psychology of risk: the variance of individuals' psychological values.[@allais1953]

In 1979, Daniel Kahneman and Amos Tversky published *Prospect Theory: An Analysis of Decision Under Risk*.[@kahneman1979] Prospect theory critiqued the descriptive validity of EUT. Kahneman and Tversky highlighted systematic ways in which people violated EUT. In so doing, they initiated the field of behavioral economics -- wedding insights from psychology with the quantitative models of economics -- to better understand human choice behavior.

Kahneman and Tversky describe multiple deviations from EUT. Notably, they discuss the certainty and reflection effects. The certainty effect describes how people overweight outcomes considered certain relative to probable ones. The reflection effect -- more commonly known as 'loss aversion' -- accounts for peoples tendency to be risk averse with positive prospects and risk seeking for negative ones. In addition to these effects, one key takeaway of prospect theory is a value function that corresponds to changes in wealth or welfare, rather than final states. This value function, is concave for gains and convex for losses, with losses steeper than gains (which goes hand-in-hand with the reflection effect).

The application of behavioral insights for theories of human decision-making necessitate an emotional component. As discussed by Graham Loomes and Robert Sugden in their 1982 paper, *Regret Theory: An Alternative of Rational Choice Under Uncertainty,* anticipatory feelings of regret are used to inform decisions. [@loomes1982][^decision-theory-1] Loomes and Sugden's describe the idea that choosing one option is a preference for it at the cost of forgoing other opportunities.[^decision-theory-2] This theory introduces the concept of counterfactual thinking, an explicit comparison between the present state of the world to alternative states.[@nealj.roese1997; @liu2016a]

[^decision-theory-1]: David Bell published a similar paper titled *Regret in Decision Making Under Uncertainty* in 1982 that explicitly incorporates notions of regret. [@bell1982] I focus on Loomes and Sugden here as it more directly addresses the behaviors discussed in Kahneman and Tversky's prospect theory.

[^decision-theory-2]: More specifically, they state that favoring $A_i$ over $A_k$ "cannot be read as 'having $A_i$ is at least as preferred as having $A_k$'; it should rather be read as 'choosing $A_i$ and simultaneously rejecting $A_k$ is at least as preferred as choosing $A_k$ and simultaneously rejecting $Ai$'".[@loomes1982].

To revisit the casino example, how would you feel if you chose the black option (gambling on the four-thousand dollars) and lost? You could have chosen the red option and had three-thousand dollars for certain, but you forewent that opportunity for a chance at something more. The comparison between "what is" vs. "what might have been" captures the idea of counterfactual information.

To summarize the material in this chapter:

* I recounted a high-level overview of choice theory, described how expected values are calculated, and introduced the importance of subjective measures (utility) when studying choice behavior.
* I then introduced EUT as the classical economist's theory of decision-making under risk.
* Next, I covered the origin of behavioral economics with Kahneman and Tversky's prospect theory to improve the descriptive models of choice behavior.
* I concludes this chapter by covering regret theory and the necessity of accounting for subjective emotions in choice models.

In the next chapter, I will outline recent work looking at the neurobiological basis of decision-making and show evidence for specific neural systems that track counterfactual signals.
