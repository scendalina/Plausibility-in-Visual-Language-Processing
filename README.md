Predictability and Plausibility in Visual Language Processing
An Eye-Tracking Study of Meaning Processing in Young Adults (18–35 years old)

Welcome! This repository is the official home for all data, scripts, and experimental materials from my master’s project, which explored how readers process the meaning of words—such as how common a word is or how plausible it is in a sentence—while reading on a screen. The study focused on the reader’s “useful field of view” and used eye-tracking to capture real-time processing.

Project Overview
This project used eye-tracking to investigate how young adults process words while reading, focusing on both word predictability (how often a word appears in the language) and plausibility (how well a word fits in its sentence context). The study used a gaze-contingent boundary paradigm, allowing control over what readers saw just outside their direct focus, to examine how meaning is processed before a word is directly fixated.

Methods
Tracked eye movements of young adults (18–35) as they read sentences on a screen.

Analyzed how word predictability and plausibility influenced reading behavior using mixed-effects statistical models.

Examined individual differences in cognitive and literacy skills, such as working memory and spelling ability, to see how these factors affected meaning processing.

Key Features & Tools
Eye-tracking data analysis using R’s lme4 for mixed-effects modeling.

Automated data preprocessing for model stability and reliability.

Stimuli generation scripts for building sentences, using statistical features from large language corpora (e.g., the English Lexicon Project).

Documented, reproducible workflow for analysis and stimulus creation.

Repository Structure
Folder	Contents
/analysis	Scripts for modeling eye movement data (R, lme4). Includes linear mixed-effects models and analyses of eye movement measures and individual differences (working memory, spelling, print literacy)
/stimuli	Scripts and materials for stimulus creation and norming. Includes content variations, linguistic and semantic input files, text prompts, and results from norming experiments and t-tests to ensure balanced conditions
/data	Compiled human eye movement dataset for replicating analyses
How to Use This Repository
Set up your environment: Ensure you have R and the lme4 package installed for mixed-effects modeling.

Run the analysis scripts to replicate the findings or adapt the workflow for your own eye-tracking or behavioral data.

Use the stimuli scripts to generate experimental materials or review how the stimuli were constructed and validated.

Authorship
All data, code, and materials are my original work. If you use or adapt anything here, please cite appropriately and respect the authorship.

If you have questions, want to collaborate, or are curious about the methods, feel free to reach out or open an issue.
