VPATH = data scripts reports

WORDSTYLES := reports/word-styles-reference-01.docx

# Variable definitions
#RAW_DATA := $(addprefix data/raw/,...)
#FIGURES := $(addsuffix .jpg,$(addprefix reports/figures/fig-,...))

#all: $(addsuffix .docx,$addprefix reports/,manuscript figures tables)

#clean:
#	rm -f ...

#.PHONY: all clean

# Read raw data
#data/raw.Rdata: read-raw-data.R $(RAW_DATA); Rscript $<

# Clean & collate data for analysis
#data/clean.Rdata: clean-data.R raw.Rdata data-cleaning-functions.R; Rscript $<

# Analyse data and store results
#data/results.Rdata: estimate-results.R clean.Rdata estimation-functions.R; Rscript $<
#data/results-appendix: estimate-results-appendix.R clean.Rdata appendix-functions.R; Rscript $<

# Generate figures
#$(FIGURES): reports/figures/fig-%.jpg: scripts/figures/fig-%.R results.Rdata; Rscript $<
#reports/figures.docx: figures.Rmd $(FIGURES) $(WORDSTYLES)
#	Rscript -e 'rmarkdown::render("$<")'

# Generate tables
#reports/tables.docx: tables.Rmd results.Rdata $(WORDSTYLES)
#	Rscript -e 'rmarkdown::render("$<")'

# Generate manuscript (excl. figures & tables)
#reports/manuscript.docx: manuscript.Rmd results.Rdata $(WORDSTYLES)
#	Rscript -e 'rmarkdown::render("$<")'

# If required, generate appendix
#reports/appendix.docx: appendix.Rmd results-appendix.Rdata $(WORDSTYLES)
#	Rscript -e 'rmarkdown::render("$<")'
