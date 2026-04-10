# Archetypoid Analysis and Spatial Modelling of Political Positions (CHES 2024)

This repository contains R code to perform an **Archetypoid Analysis (ADA)** of political party positions using the CHES 2024 dataset, together with a **spatial analysis at the country level**.

---

## 📌 Overview

The objective is to represent political parties as **convex combinations of extreme profiles (archetypoids)** and to study how these ideological structures aggregate and vary across countries.

The workflow combines:
- **Archetypoid Analysis (ADA)** for party-level representation  
- **α-coefficients** interpretation and clustering  
- **Ternary visualizations** of ideological composition  
- **Spatial econometric analysis** of country-level patterns  

---

## 📂 Repository Structure

```
├── data/        # Input data (CHES 2024)
├── R/           # R scripts
├── Results/     # Output figures (PDF)
├── Rdata/       # Saved R objects
└── README.md
```

---

## ⚙️ Requirements

Main R packages:

```r
pacman::p_load(
  tidyverse, archetypes, ggplot2, ggtern,
  sf, spdep, spatialreg, glmmTMB
)
```

---

## 🚀 Usage

1. Clone the repository:
```bash
git clone https://github.com/your-username/your-repo.git
```

2. Open R or RStudio and set the working directory.

3. Run the main script:
```r
source("analysis_Europeanpoliticalsystem.R")
```

---

## 🔍 Archetypoid Analysis (ADA)

ADA is used to represent each political party as a **convex combination of a small number of extreme profiles (archetypoids)**.

Main steps:
- Selection and scaling of policy variables  
- Estimation of ADA models for different values of \(k\)  
- Model selection using the scree (RSS) criterion  
- Extraction of:
  - Archetypoid parties  
  - **α-coefficients** (mixture weights)  

Each party is interpreted through its α-profile:
- High α for one archetypoid → strong ideological alignment  
- Balanced α values → mixed ideological profile  

---

## 📊 Visualization

The analysis includes several graphical outputs:

- **Scree plots** for model selection  
- **PCA and MDS plots** for structure exploration  
- **Star plots** for cluster profiles  
- **Ternary plots** for ADA representation  

### Ternary plots

- Each point represents a political party  
- Position is determined by its **α-coefficients** (Left–Center–Right)  
- Proximity to a vertex indicates stronger association with that archetypoid  
- **Point size is proportional to parliamentary seat share**  

---

## 🌍 Spatial Analysis

The ADA results are aggregated at the **country level** to study geographical patterns.

Main steps:
- Aggregation of party-level α-coefficients (weighted by seat share)  
- Construction of country-level ideological indicators  
- Dimensionality reduction (PCA)  
- Estimation of regression models (including beta regression)  
- Analysis of spatial dependence:
  - Moran’s I  
  - Spatial autoregressive (SAR) models  
  - Local indicators of spatial association (LISA)  

This allows identifying **spatial clustering and diffusion patterns of political ideologies across countries**.

---

## 📊 Outputs

Results are automatically saved in:

- `Results/` → figures (PDF)
- `Rdata/` → saved R objects  

Key outputs include:
- Scree plots (model selection)  
- Ternary plots (party positions)  
- Cluster visualizations  
- Spatial analysis results  

---

## 📎 Notes

- The Hopkins statistic is used **only as an indicative measure** of structure, not as a formal clustering test.  
- ADA is performed on **scaled variables**.  
- A small offset is applied in ternary plots to improve visualization near edges.  

---

## 🔁 Reproducibility

- Set a seed before running the analysis:
```r
set.seed(123)
```

---

## 📬 Contact

- Daniel Fernández (daniel.fernandez.martinez@upc.edu)
- Marica Manisera (marica.manisera@unibs.it)
- Paola Zuccolotto (paola.zuccolotto@unibs.it)
