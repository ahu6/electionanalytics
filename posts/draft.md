# Post-Election Reflection
### November 23, 2020


### Recap of Model and Predictions
<br/>
<br/>
![Prediction vs. Outcome Maps](../figures/prediction_outcome_maps.png)
<br/>
<br/>
I incorrectly predicted the outcomes for 4 states: Arizona, Georgia, Florida, and Iowa.

### Accuracy of Model

Overal RMSE: 4.35

Average percentage point difference of actual minus predicted for states Biden won: -2.49
Average percentage point difference of actual minus predicted for states Trump won: 3.72
* In general, overpredicted Biden vote share, underpredicted Trump.

<br/>
<br/>
![Actual - Predicted](../figures/actual_predicted.png)
<br/>
<br/>
![Actual - Predicted by Party](../figures/actual_predicted_party.png)

### Inaccuracies
What can explain the inaccuracies in our model, and in particular, the general tendency to overpredict for Biden and underpredict for Trump? Insights from [*The New York Times*](https://www.nytimes.com/2020/11/10/podcasts/the-daily/election-polls-biden-trump.html?) may provide clues:
* Trump appears to have done well with **Latino voters**; growth in support from this demographic bloc was not included in our model. Even if we had included this bloc in our demographic model, however, the historical trend shows that Latinos tend to favor Democrats; the reverse trend shown in this year's election may indicate an ability of Trump's campaign to appeal to Latino voters in a more effective manner than in 2016, or than previous Republican presidential candidates. Polls also struggle to get enough representation from Latino voters, so the polling data may have missed this trend as well.
* **Black voters** turned out in large numbers, as assumed by our model, but turnout among *non-Black* voters increased at an even greater rate.
* Many scholars have pointed to the pivotal role of **suburban voters** in this year's election. This could be a source of inaccuracy in our model because we did not have a direct proxy for wrongurban vs. suburban vs. rural voter demographics. 
* We included the effects of **COVID-19** on voter support for the incumbent, namely that more COVID-19 deaths in an area would correlate with decreased support for Trump. It appears, however, that Trump maintained his base in a stronger manner than we (and polls) predicted. Perhaps the effects of COVID-19 played a lesser role than our model accounted for. [*The New York Times*](https://www.nytimes.com/2020/11/04/us/politics/poll-results.html?searchResultPosition=3) does find that although the pandemic is on peoples' minds, it was not enough to sway their vote.
* Our model did not use **economic indicators** on the basis that the pandemic created such dramatic changes in such indicators as GDP growth rate that including those would yield unrealistic predictions. It can be argued, however, the Trump, as the incumbent president, was still given credit for how he managed the economy pre-COVID. In addition, some voters may have viewed his stimulus package and push to reopen the economy as an effective handling of the nation.
* Finally, there are numerous theories to why the polls appeared to underestimate support for Trump, similar to 2016. Perhaps there is systematic bias in polling methods in that Democrats are more likely to participate in such polls, or, Trump still has a ["silent majority"](https://www.nytimes.com/2020/11/04/us/politics/poll-results.html?searchResultPosition=3). Nevertheless, as [FiveThirtyEight](https://fivethirtyeight.com/features/the-polls-werent-great-but-thats-pretty-normal/) points out, "polls have *always* come with a degree of uncertainty"; just because the polls showed bias in the same direction for two presidential elections in a row does not necessarily mean that their methodology is biased in this direction. Other factors, specifically the pandemic, may have affected polling numbers this year, too.


(Proposed quantitative tests that could test these hypotheses, e.g., what data, if available, could allow you to test whether the reason proposed really did cause the inaccuracy in your model.  If there is no plausible test of the hypothesis, explain why.  You do not need to perform these tests or explain them in great detail (e.g., there is no need to write down an equation showing your exact test), just propose them.)


### In the Future

(A description of how you might change your model if you were to do it again.)