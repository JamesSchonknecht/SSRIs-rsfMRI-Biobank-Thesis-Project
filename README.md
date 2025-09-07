# Functional Connectivity Correlates of SSRI Use in Anxiety Disorders: A Resting-State fMRI Study
*Thesis Project Repository - James Schonknecht*

---

<p align="center">
  <img src="Figures/independent_components_1.png" alt="Resting State Networks" width="600"/>
</p>

---

## Overview
This repository contains the code associated with my Postgraduate Certificate in Pharmacy dissertation project, which investigated correlations between brain functional connectivity, SSRI use, and lifetime history of anxiety disorders in a large cohort from the UK Biobank.

This project predominantly used R for data cleaning, analysis, and visualisation (`R_code`). Shell scripts were also used for parts of the analysis (`Shell_scripts`), along with the [FSL - FMRIB Software Library](https://fsl.fmrib.ox.ac.uk/fsl/docs/#/).

The final thesis write-up is also included: [James Schonknecht PHCY590 Dissertation](James%20Schonknecht%20PHCY590%20Dissertation.pdf).

## Abstract

**Background**: Anxiety disorders are the most prevalent psychiatric disorders worldwide, responsible for an enormous burden both on the individual and societal levels. Although various effective treatments are now available for these disorders, many people do not respond to any of these treatments. First introduced to the market in the late 1980s, SSRI antidepressants have become the most frequently prescribed first-line pharmaceutical treatment option for anxiety disorders. Despite this, how these medications bring about an improvement in anxiety disorders remains largely unknown, serving as a barrier to the development of new or optimised treatments. Resting-state functional MRI serves as a neuroimaging modality which holds significant potential to reveal new insights into how these medications act upon the human brain. This technique has already been used to gain many insights into various psychiatric disorders and their treatments, however, research focussing on anxiety disorders and the mechanisms of SSRIs is comparatively scarce.

**Aims**: This research aimed to explore and describe the correlations between brain functional connectivity, SSRI use, and lifetime history of anxiety disorders in a large cohort.

**Method**: A subset of 488 participants was selected from the UK Biobank, a large-scale prospective study and database. These participants were matched across various demographic and clinical features and categorised into three study groups based on SSRI use at the time of neuroimaging data collection and lifetime history of anxiety disorders. Group-level independent component analysis was utilised to determine networks of resting-state functional connectivity shared among this cohort of participants. A dual regression approach was employed to statistically compare functional connectivity across the three subject groups.

**Results**: Group-level independent component analysis identified 18 components representing neuronal patterns of resting-state connectivity. None of these components displayed statistically significant differences across subject groups after a conservative correction for multiple comparisons. However, three of these components exhibited results bordering on statistical significance after adjustment. Therefore, exploratory post hoc tests were conducted on these components. These components all consisted of similar brain regions and were all most strongly representative of the sensorimotor resting-state network. Post hoc tests indicated that participants with a history of anxiety disorders who were taking an SSRI at the time of neuroimaging data collection exhibited widespread reduced functional connectivity within each of these three networks, relative to the group of participants without a history of anxiety disorders who were not taking any medications. Large areas of decreased functional connectivity within these networks were localised within the precentral and postcentral gyri. No significant differences were observed between participants with a lifetime history of anxiety disorders and participants without this history.

**Conclusion**: The findings of this study suggest that the use of SSRIs is correlated with a decrease in within-network functional connectivity for several networks which correspond most closely to the sensorimotor resting-state network. Further research which focuses on or incorporates this network into analyses is needed to confirm these results, and longitudinal studies are required to determine whether these findings are directly caused by SSRI use, or if they are correlated through other factors.
