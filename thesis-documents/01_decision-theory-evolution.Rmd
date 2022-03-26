# Evolution of Decision Theory

Imagine you are in a casino. You are asked to play a new type of roulette. This time only, there is no cost to you for participating. The rules are simple: pick the red option or the black option. If you choose the red option, the dealer will give you three-thousand dollars. If you pick the black option, the dealer will spin the wheel and release the ball. This wheel is weighted so that you have an 80% chance of getting four-thousand dollars and a 20% chance of getting zero dollars. Which option would you choose? Why?

If you were an economic theorist, I would bet that you would choose the black option because it has a higher expected value. Expected values, $EV$, are calculated by multiplying the magnitude, $x$, of an outcome by the probability, $p$, it will occur. In this example, the red option leads to three-thousand dollars with certainty. The black option leads to four-thousand dollars with an 80% chance (a probability of 0.8). 

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

Looking at the expected values for each option calculated in \@ref(eq:ev-red-black), we see $\text{EV}(\text{Red}) > \text{EV(Black)}$. Therefore, the rational choice according to a classical economist is to choose the black option. 

The new type of roulette presented above is a prototypical example of a *prospect* -- a choice between two options with explicit probabilities and outcomes -- used to investigate decision-making under risk. A prospect with two options is commonly denoted as follows where the first option leads to outcome $x_a$ with probability $p_a$ and the second option leads to outcomes $x_b$ with probability $p_b$. 

\begin{equation}
\begin{aligned}
\text{Option A} &= (x_a, p_b) \\
\text{Option B} &= (x_b, p_b)
\end{aligned}
(\#eq:prospect-notation)
\end{equation}

Unless otherwise stated, the alternative outcomes of an option are assumed to be zero. That is, choosing Option A is a choice of receiving $x_a$ with probability $p_a$ or nothing with probability $(1 - p_a)$.


<!-- prospect example, where an individual (you) chooses between two options that have explicit probability and outcome structure, is a narrated form of a *prospect* -->

<!-- is widely studied in the decision-making literature. *prospect*, a choice between two options with explicit probability and outcome structures,  -->
