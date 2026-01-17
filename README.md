# Union Representation and Support for Racial Inequality Education (CMPS 2020)

This repository contains R code for my conference course project examining whether **perceived union representation** is associated with **support for racial inequality education**, using the **Collaborative Multiracial Post-Election Survey (CMPS) 2020**.

## Research Question
Does perceived union representation increase support for racial inequality education?

## Data
- Source: CMPS 2020 (ICPSR 39096)
- File used: `39096-0001-Data.rda`

**Important:** The dataset is not included in this repo. To run the code, download the CMPS 2020 data from ICPSR and place the `.rda` file in a local `data/` folder (see “How to Run”).

## Key Variables
- **Outcome:** `inequality`  
  Binary indicator for support for racial inequality education, recoded from `Q417_Q424R5`  
  (Support/Somewhat Support/Strongly Support = 1; otherwise = 0)

- **Main predictor:** `unionbinary`  
  Binary indicator for perceived union/work association leader representation, recoded from `Q171R4`  
  (“Union or work association leaders” = 1; “NO TO…” = 0)

- **Controls:** race (`S2_RACE_PRIME`), gender (`S3B`), age (recoded from `S5_AGE`), education (`S13`), community type (`S14`)

## Models
The script estimates four logistic regression models:

1. **Model 1 (Controls only):**  
   `inequality ~ race + gender + age + education + community`

2. **Model 2 (Main hypothesis):**  
   Adds perceived union representation (`unionbinary`) to Model 1

3. **Model 3 (Reverse check / robustness):**  
   `unionbinary ~ inequality + controls`  
   (Used as a diagnostic check; not a causal identification strategy)

4. **Model 4 (Theoretical extension):**  
   Interaction between union representation and race:  
   `inequality ~ unionbinary * race + controls`

## Outputs
The code generates:
- Average Marginal Effects (AME) plots for Models 1–3
  - `model1_ame.pdf`
  - `model2_ame.pdf` and `model2_ame.png`
  - `model3_ame.pdf`

## How to Run
1. Clone/download this repo
2. Create folders:
   - `data/`
   - `outputs/` (optional, for plots)
3. Place the ICPSR `.rda` file here:
   - `data/39096-0001-Data.rda`
4. In the script, update the load path from your local machine path to:
   ```r
   load("data/39096-0001-Data.rda")
