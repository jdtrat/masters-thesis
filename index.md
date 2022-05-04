--- 
title: "Counterfactuals, Dopamine, and Risky Behavior"
subtitle: "Deriving a neurobiological theory of decision-making under risk"
author: "Jonathan D. Trattner"
date: "2022-05-03"
site: bookdown::bookdown_site
twitter-handle: jdtrat
documentclass: book
bibliography: [packages.bib]
csl: apa-numeric-brackets.csl
link-citations: yes
colorlinks: yes
classoption:
  - oneside
linestretch: 1.5
description: |
  In this thesis, I combine insights from economics, pyschology, and neuroscience to develop 'Counterfactual Predicted Utility Theory' (CPUT) as a neurobiologically-plausible theory of decision-making under risk. CPUT is inspired by the observation that sub-second fluctuations in the levels of the neurotransmitter dopamine seemingly reflect factual and counterfactual information. I propose that people incorporate anticipated counterfactual events when making risky decisions. This leads to behavior that is considered ‘irrational’ from a classical economic perspective as described by Expected Utility Theory (EUT). To assess the predictive accuracy of CPUT, I compared variations of CPUT and EUT on human choice data from a sure bet or gamble task using hierarchical Bayesian modeling techniques. I quantified model fit with multiple methods. This includes comparing marginal likelihood model evidence and leave-one-out cross validation predictive accuracy. I found that CPUT offers a better explanation for the data collected as part of the sure-bet or gamble task while EUT is more likely to generalize to other (similar) datasets. While these results do not provide conclusive evidence favoring CPUT, they suggest a possible role for the integration of counterfactual information in risky decision-making that warrants future investigation.
knit: "bookdown::render_book"
url: 'https\://masters-thesis.jdtrat.com/'
---





# Dedication and Acknowledgements

In May 2022, I will graduate with a Master of Science Degree in Neuroscience from [Wake Forest School of Medicine](https://wakehealth.edu). This website is the embodiment of my master's thesis. None of this would be possible if not for the incredible support of my mentors, family, and friends. 

First and foremost, I'd like to thank Ken Kishida. I've been conducting research with him since February 2018 and am incredibly appreciative of his encouragement and support. Dr. Kishida has given me the space for me to follow my interests with programming and neuroscience, and this website is a testament to what I learned working with him. Within the Kishida Lab, I am especially grateful to L. Paul Sands for conversations about my modeling techniques as well as Emily DiMarco, Rachel Jones, Angela Jiang, and Brittany Liebenow who provided valuable feedback and guidance as I developed my thesis. 

As an undergraduate, Lucy McGowan, S. Mason Garrison, and Staci Hepler had an immeasurable impact on how I understand, and conduct, scientific programming, Bayesian inference, and data analysis. Katy Lack, Melissa Maffeo, Wayne Pratt, and Terry Blumenthal fostered my interest in Neuroscience. I am extremely grateful to have learned from them.

I'd next like to thank my committee members, Drs. Christian Waugh and Todd McFall (and Ken Kishida!) for their support and guidance as I've worked on my thesis. Also, [Nathaniel Haines](http://haines-lab.com)'s publications greatly improved my understanding of the underlying mathematics behind computational modeling and he provided invaluable feedback on my work. 

To my family, I would not be the person I am without your love and support, especially that of my mom, Joanne Fink, and grandpa, Dr. Gordon Fink. I dedicate this thesis to you.



# Abstract

In this thesis, I combine insights from economics, psychology, and neuroscience to develop 'Counterfactual Predicted Utility Theory' (CPUT) as a neurobiologically-plausible theory of decision-making under risk. CPUT is inspired by the observation that sub-second fluctuations in the levels of the neurotransmitter dopamine seemingly reflect factual and counterfactual information. I propose that people incorporate anticipated counterfactual events when making risky decisions. This leads to behavior that is considered ‘irrational’ from a classical economic perspective as described by Expected Utility Theory (EUT). To assess counterfactual predicted utility theory's validity as a theory of decision-making under risk, I compared variations of CPUT and EUT on human choice data from a sure-bet or gamble task using hierarchical Bayesian modeling techniques. I quantified model fit with multiple methods. This includes comparing marginal likelihood model evidence and leave-one-out cross validation predictive accuracy. Compared to EUT, I found that CPUT does not offer a better explanation for the data collected, nor does it generalize as well. However, the task design limits the available counterfactual information, and the data collected may not be sufficient to accurately assess CPUT. Further research into the neurobiological mechanisms for processing factual and counterfactual information and the downstream behavioral consequences is warranted.

