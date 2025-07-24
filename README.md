# Automated Scoring of Short Answer Questions with Large Language Models

This repository contains the data and analyses for the research paper "Automated Scoring of Short Answer Questions with Large Language Models: Impacts of Model, Item, and Rubric Design" presented at AIED 2025.

## Abstract

Short answer questions (SAQs) are useful in educational testing but can be resource-intensive to grade at scale. This study explores the performance of several off-the-shelf Large Language Models (LLMs) at automatically scoring responses to High School Math and English SAQs using three different rubric types. Results show that LLM performance improves with more detailed rubrics, and performance varies significantly by item characteristics.

## Research Questions

1. **RQ1**: How much item information is needed in the prompt to produce reliable LM ratings?
2. **RQ2**: How well do various LLMs score SAQs?
3. **RQ3**: What are the features of SAQs with the best and worst LLM performance?

## Dataset

- **20 Short Answer Questions**: 10 High School English Language Arts (ELA) and 10 High School Algebra I (Math)
- **800 Student Responses**: 40 responses per item (20 correct, 20 incorrect)
- **Human Scoring**: 3 human judges scored each response using full rubrics
- **LLM Scoring**: 15 different LLMs scored responses using 3 rubric types

### Models Evaluated

**Original Study (11 models):**
- **Small**: Claude 3.5 Haiku, Gemini 1.5 Flash, Gemini 1.5 Flash 8b, GPT-4o mini, Llama 3.1 70b, Llama 3.1 8b
- **Large**: Claude 3.5 Sonnet, Gemini 1.5 Pro, GPT-4o, Llama 3.1 405b
- **Frontier**: OpenAI o1

**Extended Analysis (4 additional models):**
- Claude 4.0 Sonnet, Gemini 2.5 Pro, OpenAI o3, OpenAI o4 Mini

### Rubric Types

- **Empty**: Question only, no scoring criteria
- **Criteria Only**: Question + scoring criteria
- **Full**: Question + scoring criteria + example correct/incorrect answers

## Repository Structure

```
llm-saq-scoring/
├── data/
│   ├── raw/                          # Original datasets
│   │   ├── human_labels.csv          # Human scorer ratings (800 × 8)
│   │   ├── items.csv                 # Item metadata and characteristics (20 × 11)
│   │   ├── llm_labels.csv           # LLM scores - original models (26,400 × 11)
│   │   └── llm_labels_new_models.csv # LLM scores - newer models (9,600 × 11)
│   └── processed/                    # Analysis-ready datasets
│       ├── dat.csv                   # Main analysis dataset
│       └── dat_new.csv              # Dataset with newer models
├── analysis/
│   ├── 01-analysis.qmd              # Primary analysis (original models)
│   └── 02-analysis-new-models.qmd   # Extended analysis (newer models)
├── functions/
│   └── functions.R                   # Reusable analysis functions
├── output/
│   ├── tables/                      # Results in tabular format
│   ├── results/                     # Statistical test outputs  
│   └── visualizations/              # Plots and figures
└── docs/                            # Documentation
```

## Key Findings
- Off-the-shelf LLMs can score Short Answer Questions on-par with human scorers, demonstrating their viability for automated scoring applications
- Including scoring criteria improved model inter-rater reliability and performance against human ratings, but for some items and models, performance was very good even without scoring criteria or examples
- Including examples may not always improve inter-rater reliability and performance beyond just providing criteria, though this appears to depend on the specific item characteristics
- Larger models tend to outperform smaller models, but some small models (e.g., Gemini 1.5 Flash) still performed very well, achieving acceptable performance levels
- LLMs tend to score items requiring shorter answers and narrow scope more accurately, with performance declining for items requiring longer responses and having broader answer scope
- Using LLMs to score SAQs in development could help identify inconsistencies in scoring, potentially leading to clearer and more robust rubrics
- LLM inter-rater reliability was often higher than human judges (Fleiss' κ = 0.881 for humans), suggesting high consistency in LLM scoring behavior


## Getting Started

### Prerequisites

- R (version 4.0+)
- Required R packages: 
-- yardstick
-- broom
-- emmeans
-- report
-- psych
-- irr
-- tidyverse
-- janitor
-- ggthemes

### Reproducing the Analysis

1. **Clone the repository**
   ```bash
   git clone https://github.com/Khan/llm-saq-scoring.git
   cd llm-saq-scoring
   ```

2. **Install dependencies** (if using renv)
   ```r
   renv::restore()
   ```

3. **Run the analyses**
   ```r
   # Original model analysis
   quarto::quarto_render("analysis/01-analysis.qmd")
   
   # Extended analysis with newer models  
   quarto::quarto_render("analysis/02-analysis-new-models.qmd")
   ```

All outputs (tables, results, visualizations) will be generated in the `output/` directory.

## Data Dictionary

Detailed variable descriptions are available in `docs/DATA_DICTIONARY.txt`. Key variables include:

- **Response scoring**: Binary correct/incorrect ratings from humans and LLMs
- **Item characteristics**: Response length, cognitive demand, answer scope, scoring subjectivity
- **Model information**: Model name, size classification, rubric type used

## Citation

If you use this dataset or findings in your research, please cite:

```
Frohn, S., Burleigh, T., & Chen, J. (2025). Automated Scoring of Short Answer Questions with Large Language Models: Impacts of Model, Item, and Rubric Design. In Proceedings of the 26th International Conference on Artificial Intelligence in Education (AIED 2025).
```

## License

- **Code and Analysis**: MIT License
- **Data and Assessment Items**: CC BY 4.0 License

## Contact

- **Scott Frohn** - scottfrohn@khanacademy.org
- **Tyler Burleigh** - TylerB@khanacademy.org
- **Jing Chen** - jing@khanacademy.org

## Acknowledgments

This research was conducted at Khan Academy. We thank the subject matter experts who created the assessment items and the human judges who provided scoring.

---

For detailed methodology and complete results, please refer to the published paper and the analysis notebooks in this repository.
