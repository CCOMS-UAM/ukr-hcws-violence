# README

This repository includes the analysis workflow for the manuscript entitled "Mental health of nurses and doctors in Ukraine during wartime: a nationwide cross-sectional survey". The analyses were conducted by Vicente Arrona and Roberto Mediavilla at the WHO Collaborating Centre, Universidad Autónoma de Madrid, Spain.

Due to data privacy and licensing restrictions, the raw datasets cannot be publicly shared. However, this repository provides the full analysis code and rendered outputs to ensure transparency and reproducibility.

---

## 📁 Project Structure

project-root/

├── ext/

├── out/

├── src/

├── renv/

├── data/ (gitignored)

├── .gitignore

├── analysis.Rmd

├── analysis.html

└── README.md

### Description

- **`ext/`**  
  Contains external resources required for the analysis (e.g., codebooks and reference files).

- **`out/`**
  Stores outputs generated from the R Markdown file, such as tables, figures, and intermediate results.

- **`src/`**  
  Includes R scripts for data cleaning, preprocessing, and reusable functions.

- **`renv/`**  
  Contains the `renv` environment configuration to ensure reproducibility of package dependencies.

- **`data/`** *(not tracked)*  
  Holds the main datasets (e.g., `.rds` files).  
  This folder is excluded from version control due to data sharing restrictions.

- **`.gitignore`**  
  Specifies files and directories excluded from Git version control.

- **`analysis.Rmd`**  
  Main R Markdown file containing the analysis workflow.

- **`analysis.html`**  
  Rendered output of `analysis.Rmd`, provided so users can view results without access to the raw data.

- **`README.md`**  
  Project documentation.

> **Note:** Due to data privacy and licensing restrictions, the raw datasets are not included in this repository. However, the rendered HTML output is provided to allow users to explore the results and methodology.

---

## ⚙️ Requirements

This project is developed in **R** and uses `renv` for dependency management.

- R (version compatible with `renv.lock`)
- RStudio (recommended)
- `renv` package

---

## 📦 Installation

To set up the project locally:

1. Clone the repository:

   git clone https://github.com/USERNAME/ukr-hcws-violence.git
   cd ukr-hcws-violence

3. Open the project in R or RStudio.

4. Restore the package environment:

   renv::restore()

This will install all required packages in a local project library.

## ▶️ Usage

If you only want to explore the results, open: `analysis.html` in your web browser. This file contains the rendered output of the full analysis.

If you have access to the original datasets:

1. Place the data files in the data/ directory.
2. Make sure file names and formats match those expected by the scripts.
3. Open analysis.Rmd.
4. Knit the document or run: `rmarkdown::render("analysis.Rmd")`
5. Outputs will be generated in the out/ directory.

## 🔁 Reproducibility

This project uses renv to ensure reproducibility. Package versions are recorded in renv.lock. Running renv::restore() recreates the original software environment. All data processing and analysis steps are documented in analysis.Rmd and src/.

## 📊 Data Availability

The datasets used in this project are not publicly available due to privacy and licensing constraints. Researchers interested in reproducing the analysis should contact the data provider or the project maintainer to request access.

🤝 Contributing

Contributions are welcome. If you would like to contribute:

1. Fork the repository.
2. Create a feature branch.
3. Commit your changes.
4. Open a pull request.

Please ensure that all code is documented and reproducible.

## 📄 License

CC0 1.0 Universal

## 📬 Contact

For questions or suggestions, please open an issue on GitHub or contact the repository maintainer.
