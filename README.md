# Car Share Analysis
Logistic regression examining the variables associated with car share membership in Puget Sound.

This analysis uses data from the Puget Sound Regional Council's Household Travel Survey from 2019: https://www.psrc.org/household-travel-survey-program. The survey of 6,000 households reports travel behavior, attitudes, and preferences on a trip, household, vehicle, and person-level basis. 


# Summary of Findings

Households with fewer vehicles are generally more likely to be a car share member, but the effect grows more pronounced as income increases. Households with lower incomes, regardless of vehicle ownership, do not join car share programs in high numbers.

Similarly, higher income makes a household more likely to be a car share member, but the effect of income depends on the type of urban village the household is in. The effect is most pronounced between households that are not in an urban village and those that are. This makes sense, because living without a car in an urban village would be easier than in a less dense neighborhood. 

I tried seven different models to predict car share membership. Based on the best model, households with a median household income of $95,000, and no household vehicles, had a 26% probability being car share members (95% CI, (26%, 27%)).
For a given number of household vehicles, each $10,000 increase in household income was associated with a 1.1 times higher odds of being a car share member. The income effect was estimated as an average across all vehicle counts.
For a given income, each increase of one household vehicle was associated with a change in 0.4 times the odds of being a car share member, meaning the odds decreased with higher vehicle counts. The vehicle effect was estimated as an average across all incomes.

Overall, though, the logistic model is still not a very good fit for this data. There is too much overlap among middle-income households who are and are not car share members.

Through this exploration, I’ve found that household income and number of vehicles are the best predictors of a household’s likelihood to join a car share program. However, it is still difficult to predict car share membership. This is because in either group there are many more people who are not car share members than who are; people only tend to be car share members when they have both high incomes and a low number of vehicles, not just one or the other. Even so, it appears there are many more factors than just income or car ownership that prevent people from becoming car share members.

At the time this data was collected, car share programs were only available within Seattle city limits, and that remains the case today. However, about half of the survey respondents were outside the city. Thus, this survey was not able to ascertain a household’s interest in joining car share or what choice they would make if they had it available, so it may not be helpful in informing whether a car share program should consider expanding outside the city limits. This model also may not apply to other cities around the world. In the United States and Seattle in particular, living near good transit and in a walkable neighborhood is a luxury that many people cannot afford, and thus the effect of income on car share membership may be more pronounced here than elsewhere.
