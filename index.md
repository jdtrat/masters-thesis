--- 
title: "Counterfactuals, Dopamine, and Risky Behavior"
subtitle: "Deriving a neurobiological theory of decision-making under risk"
author: "Jonathan D. Trattner"
date: "2022-04-13"
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

> I dedicate my thesis to both my mom, Joanne Fink, and grandpa, Dr. Gordon Fink. I would not be the person I am without their love and guidance. Plus, they are excellent proof-readers, which I always appreciate! I love you both.

In May 2022, I will graduate with a Master of Science Degree in Neuroscience from [Wake Forest School of Medicine](https://wakehealth.edu). This website is the embodiment of my master's thesis. None of this would be possible if not for the incredible support of my mentors, family, and friends. 

First and foremost, I'd like to thank Ken Kishida. I've been conducting research with him since February 2018 and am incredibly appreciative of his encouragement and support. Dr. Kishida has given me the space to follow my interests with programming and neuroscience especially, and this website is the embodiment of that. Within the Kishida Lab, conversations with L. Paul Sands greatly informed my modeling techniques, for which I am appreciative. Emily DiMarco, Rachel Jones, Angela Jiang, and Brittany Liebenow provided feedback and guidance as I developed my thesis. 

As an undergraduate, Lucy McGowan, S. Mason Garrison, and Staci Hepler had an immeasurable impact on how I understand, and conduct, scientific programming, Bayesian inference, and data analysis. Katy Lack, Melissa Maffeo, Wayne Pratt, and Terry Blumenthal fostered my interest in Neuroscience. I am extremely grateful to have learned from them.

I'd next like to thank my committee members, Drs. Christian Waugh and Todd McFall (and Ken Kishida!) for their support and guidance as I've worked on my thesis. Their input has made it that much better. Also, [Nathaniel Haines](http://haines-lab.com)'s publications greatly improved my understanding of the underlying mathematics behind computational modeling and he provided invaluable feedback on my work. 

To my family, your love and support are everything. Thank you. 



# Abstract

In this thesis, I combine insights from economics, pyschology, and neuroscience to develop 'Counterfactual Predicted Utility Theory' (CPUT) as a neurobiologically-plausible theory of decision-making under risk. CPUT is inspired by the observation that sub-second fluctuations in the levels of the neurotransmitter dopamine seemingly reflect factual and counterfactual information. I propose that people incorporate anticipated counterfactual events when making risky decisions. This leads to behavior that is considered ‘irrational’ from a classical economic perspective as described by Expected Utility Theory (EUT). To assess the predictive accuracy of CPUT, I compared variations of CPUT and EUT on human choice data from a sure bet or gamble task using hierarchical Bayesian modeling techniques. I quantified model fit with multiple methods. This includes comparing marginal likelihood model evidence and leave-one-out cross validation predictive accuracy. I found that CPUT offers a better explanation for the data collected as part of the sure-bet or gamble task while EUT is more likely to generalize to other (similar) datasets. While these results do not provide conclusive evidence favoring CPUT, they suggest a possible role for the integration of counterfactual information in risky decision-making that warrants future investigation.

