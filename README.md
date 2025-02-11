# Plausibility-in-Visual-Language-Processing
An eye-tracking study using young adults (18-35 years old) that examined how readers process meaning across their 'useful field of view' during reading on a screen
Eye-Tracking & Language Processing Analysis
Project Overview
This repository contains data processing and analysis scripts for an eye-tracking study on language comprehension. The project involved statistical modeling of eye movement data to examine how readers process linguistic stimuli in real time.

Key Features & Tools
✅ Eye-tracking data analysis using lme4 for mixed-effects modeling
✅ Automated data preprocessing to ensure model stability and convergence
✅ Stimuli generation scripts for psycholinguistic experiments
✅ Reproducible statistical modeling with documented workflow

Repository Structure
📂 /analysis – Scripts for eye movement data modeling (R, lme4)
 Description: Features of these analyses include: Linear mixed effect modeling ('keep it maximal' approach; Barr, 2013) for 
    - eye movement measures using a gaze-contingent boundary paradigm 
    - individual difference measures in working memory, and various literacy measures like spelling and print literacy ability
📂 /stimuli – Stimuli norming scripts & experimental materials (i.e., Content Variations, Linguistic Inputs, Semantic Inputs, and Text Prompts)
  Description: I created these by pulling individual words and their statistical features from Large Langauge Corpora (e.g., The English Lexicon Project), and then creating sentences from scratch
    - I then conducted separate experiments and conducted t-tests to ensure there were no experimental conditional differences in stimuli - those can be found in the Linguistic and Semantic Inputs Project!**
📂 /data – Human Eye Movement (compiled) dataset for replicating analyses

How to Use This Repository
- Set up your environment: Ensure lme4 is installed in R for mixed-effects modeling.
- Run the analysis script to replicate findings or adapt the workflow for similar datasets.
- Use the stimuli generation scripts to create customized experimental materials.

While this project was conducted originally for an academic audience, here are some of the key take aways from this study for Industry focused Applications:
💡 UX Research: Insights into user attention and cognitive load via eye-tracking data
💡 Human Factors & Neurotech: Applying gaze data analysis for usability testing & cognitive modeling
💡 Data Science: Advanced statistical modeling for behavioral datasets

For inquiries or collaboration, feel free to reach out!

🚀 Clara Lopes
clara.louise.lopes@gmail.com
