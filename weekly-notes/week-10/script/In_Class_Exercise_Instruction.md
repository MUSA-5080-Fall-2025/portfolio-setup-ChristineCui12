# Georgia DOC Policy Advisory Challenge
## In-Class Group Activity - Week 10

---

## Your Role

You are policy analysts hired by the **Georgia Department of Corrections**. They are considering deploying a recidivism prediction model to inform parole decisions. Your team must analyze the model and make a **GO/NO-GO recommendation** to the Commissioner.

---

## Instructions

### Phase 1: Individual Exploration 
Run the provided R script (`week10_exercise.R`) and note:

1. What's the model's AUC?
- 0.732
2. At threshold 0.50, what's the sensitivity and specificity?
- Sensitivity: 0.8167219
- Specificity: 0.4923896
3. Which racial group has the highest false positive rate?
- Black, cos the FPR is 0.562 compared with 0.425 in white racial group.
4. Which group has the highest false negative rate?
- White, cos the FNR is 0.227 compared with 0.154 in black.
5. What happens if we change the threshold to 0.30 or 0.70?
- The FPR in both groups fall down dramatically, but the rate in black racial group is still higher than that in white racial group.
- In reverse, FNR shows different pattern if we change the threshold to 0.30 or 0.70, which is an increase trend. But still, FNR in white racial group is higher than FNR in black group.

### Phase 2: Group Analysis 
As a **table or half table team**, discuss your findings and complete the template below. Prepare to present your recommendation.

### Phase 3: Presentation 
Present your recommendation to the "Commissioner" (instructor). 

---

# Policy Recommendation Template

**Complete this as a group and be ready to present**

---

## Consulting Team Information

**Clever Team Name:** _____________

**Team Members:**

- __Christine_______________________
- __Demi_______________________
- __Jinyang_______________________
- _________________________
- _________________________
- _________________________

---

## 1. TECHNICAL ASSESSMENT

### Model Performance Metrics

**AUC (Area Under ROC Curve):** ___0.732_______

**At threshold = 0.50:**

- Sensitivity (True Positive Rate): _0.817_________
- Specificity (True Negative Rate): __0.492________
- Precision (Positive Predictive Value): __0.706________
- Overall Accuracy: __0.686________

### Technical Quality Rating
Select one:

- ☐ Excellent (AUC > 0.90)
- ☐ Good (AUC 0.80-0.90)
- ☐√ Acceptable (AUC 0.70-0.80)
- ☐ Poor (AUC < 0.70)

### Brief Technical Summary (2-3 sentences)
Is the model accurate enough for high-stakes decision-making?

_The model demonstrates acceptable but limited discriminatory power (AUC = 0.732). 
While it has good sensitivity (81.7%), its low specificity (49.2%) results in a high rate of false positives. 
Therefore, it is not accurate enough for high-stakes decision-making without careful consideration of the costs associated with false alarms._______________________________________________________________________

________________________________________________________________________

________________________________________________________________________

---

## 2. EQUITY ANALYSIS

### False Positive Rates by Race (at threshold 0.50)

| Racial Group | False Positive Rate | Sample Size |
|--------------|---------------------|-------------|
| Group 1: Black    |  0.562                   |  3931           |
| Group 2: White    |  0.425                   |  2620           |
| Group 3:     |                     |             |
| Group 4:     |                     |             |

### False Negative Rates by Race (at threshold 0.50)

| Racial Group | False Negative Rate | Sample Size |
|--------------|---------------------|-------------|
| Group 1: Black    |   0.154                  |   3931          |
| Group 2: White    |   0.227                  |   2620          |
| Group 3:     |                     |             |
| Group 4:     |                     |             |

### Disparity Analysis

**Largest disparity identified:**

Group _____Black________ has __13.7____% higher false positive rate than Group ___White__________

**OR**

Group ______White_______ has ___7.3___% higher false negative rate than Group ____Black_________

### Equity Concerns Summary (3-4 sentences)
What are the implications of these disparities? Who is harmed?

The model exhibits significant racial disparity, with the Black population experiencing a 13.7% higher False Positive Rate than the White population. 
This means that Black individuals are disproportionately harmed by being incorrectly flagged as "high-risk" when they are not. 
Such a disparity could lead to unjust outcomes, including denied opportunities or increased scrutiny, thereby perpetuating and automating existing biases. 
The model in its current state raises serious equity concerns and is not suitable for deployment without algorithmic fairness interventions.

---

## 3. THRESHOLD RECOMMENDATION

### If we deploy this model, we recommend:

Select one:

- ☐ Threshold = 0.30 (Aggressive - prioritize catching recidivists)
- ☐ Threshold = 0.50 (Balanced - default)
- ☐√ Threshold = 0.70 (Conservative - minimize false accusations)
- ☐ Other: ________

### Rationale for Threshold Choice (3-4 sentences)
Why this threshold? What does it optimize for? What are the trade-offs?

- We recommend the conservative threshold of 0.70 to prioritize minimizing false positives, 
thereby reducing the risk of unjustly accusing individuals who would not reoffend. 
This choice is critically informed by the equity analysis, 
which revealed that false positives disproportionately harm the Black population. 
The trade-off is a deliberate acceptance of more false negatives, 
meaning some actual recidivists may be missed, 
in order to prevent the more severe societal harm of systematic false accusations against a protected group. 
This approach optimizes for fairness and mitigates the model's potential to amplify existing biases.

### This threshold prioritizes:
Select one:

- ☐ **High Sensitivity** - Catch more people who will reoffend (accept more false positives)
- ☐√ **High Specificity** - Avoid false accusations (accept more false negatives)
- ☐ **Balance** - Try to minimize both types of errors

---

## 4. DEPLOYMENT RECOMMENDATION

### Our recommendation to Georgia DOC:

Select one:

- ☐ **DEPLOY** - Use this model to inform parole decisions
- ☐ **DO NOT DEPLOY** - Do not use this model
- ☐√ **CONDITIONAL DEPLOY** - Deploy only with specific safeguards in place

### Key Reasons for Our Recommendation

Provide 3-5 bullet points supporting your decision:

- ___________________________________________________________________

- ___________________________________________________________________

- ___________________________________________________________________

- ___________________________________________________________________

- ___________________________________________________________________

### What about the equity concerns?

How do you justify your recommendation given the disparate impact you identified?

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

---

## 5. SAFEGUARDS OR ALTERNATIVES

### If DEPLOY - Required Safeguards

What protections must be in place before deployment?

1. ___________________________________________________________________

2. ___________________________________________________________________

3. ___________________________________________________________________

4. ___________________________________________________________________

**OR**

### If DO NOT DEPLOY - Alternative Approaches

What should Georgia DOC do instead?

1. ___________________________________________________________________

2. ___________________________________________________________________

3. ___________________________________________________________________

4. ___________________________________________________________________

---

## 6. LIMITATIONS & UNCERTAINTIES

### What we don't know (but wish we did)

What additional information would strengthen your recommendation?

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

### Weaknesses in our recommendation

What's the strongest argument AGAINST your recommendation?

________________________________________________________________________

________________________________________________________________________

________________________________________________________________________

---

## 7. BOTTOM LINE

### One-Sentence Recommendation

If the Commissioner only reads one thing, what should it be?

________________________________________________________________________

________________________________________________________________________

---

