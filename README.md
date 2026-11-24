# CEP 11 — Data Preparation
MSDM CEP Group 6 · PhD Orthodontics

- Read the [published report](https://hanamai111.github.io/cep11-data-preparation-phd-orthodontics/).
- Render the QMD with `quarto render Group-6-CEP11-Data-Preparation.qmd`.
- Run each statement in `ga4-integrity-checks.sql` in US BigQuery. The sample mart is optional for the raw-data check and requires separate permissions.
- In R, install haven and dplyr, then run `source("survey-preparation.R"); test_synthetic()` from this directory.
- The variable dictionary uses logical questionnaire IDs. Before any authorized import, verify the actual survey version, export columns, recode values, and provenance. Core preparation includes screeners, contextual single-choice inputs, and ratings. Q2, follow-ups, and restricted text fields require export-specific processing.
- Keep research data and private protocol materials in the approved restricted environment.

If the Quarto HTML theme cache is unavailable, reproduce the standalone HTML using the bundled Pandoc:

```sh
quarto pandoc Group-6-CEP11-Data-Preparation.qmd --from=markdown --to=html5 --standalone --toc --toc-depth=2 --number-sections --embed-resources --include-in-header=report-style.html -o Group-6-CEP11-Data-Preparation.html
```
