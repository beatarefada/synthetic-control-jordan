# Additions and Subtractions: The Effects of the Syrian Refugee Crisis on Mathematics and Reading Outcomes in Jordan

## I. Project Overview
We investigate the effect of the 2011 Syrian refugee crisis on **education quality in Jordan** using a **Synthetic Control Analysis (SCA)**. Jordan, a small neighboring country, received a large influx of refugees relative to its population, making it a critical case study for potential impacts on the national education system due to overcrowding and integration challenges.

## II. Methodology
* **Research Question:** Did the Syrian refugee crisis (starting in 2011) impair the education quality of Jordanian students?
* **Measurement:** We use **PISA (Programme for International Student Assessment)** scores for **Mathematics and Reading** as the internationally comparable measure of educational quality.
* **Treatment:** The treatment year (the point of intervention) is set to **2011**.
* **Synthetic Control:** We constructed a synthetic "Jordan" from a **donor pool of 13 other countries** whose pre-treatment PISA trends closely matched those of the real Jordan.
* **Analysis File:** The complete analysis is contained within the notebook: `notebooks/Synthetic_Control_Jordan.ipynb`.

## III. Key Findings
The Synthetic Control Analysis found a **near-to-zero, statistically insignificant effect** on the Jordanian students’ PISA scores when compared to their synthetic counterpart.

**Conclusion:** The results suggest that the large-scale shock from the incoming refugees was **not large enough to register a significant negative impact** on national-level PISA data. This warrants further investigation into potential **regional- or school-level effects** that may not be captured by national averages.

## IV. Replication and Running the Analysis
This project was developed using **R** within a **Jupyter Notebook environment**.

1.  **Clone the Repository:**
    ```bash
    git clone [Your Repository URL]
    ```
2.  **Install R Dependencies:** The specific R packages used for the analysis are listed in the `requirements.R` file.
    * *Installation: Open the `requirements.R` file and run the `install.packages()` commands in your R console or environment before running the notebook.*
3.  **Run the Notebook:** Open the `notebooks/Synthetic_Control_Jordan.ipynb` file and run all cells.
