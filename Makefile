VPATH = data scripts

# Variable definitions
RAW_DATA := $(addprefix data/raw/,Performance-Matrix-RW.xlsx Attribute-Weights.xlsx)

all: data/evidenceTables.Rdata

clean:
	rm -f data/data.Rdata data/evidenceTables.Rdata

.PHONY: all clean

# Clean & collate data for analysis
data/clean.Rdata: load-data.R $(RAW_DATA); Rscript $<

# Construct evidence tables and other required inputs for shiny app
data/evidenceTables.Rdata: collate-evidence-tables.R clean.Rdata; Rscript $<
