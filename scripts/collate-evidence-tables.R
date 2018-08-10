library(tidyverse)

load("data/data.Rdata")

create_evidence_table <- function(df, varnames) {
	out <- as.matrix(df %>% select(varnames))
	dimnames(out) <- list(Intervention = interventionNames,
												Attribute = names(attributeNames))
	
	out
}

clean_RACGP <- clean_RACGP[complete.cases(clean_RACGP), ]

attributeNames <- list(
	Recommendation = c("Strong Against", "Conditional Against", "Neutral", "Conditional For", "Strong For"),
	"Quality of Evidence" = c("Very low", "Low", "Moderate", "High"),
	Cost = c("High", "Medium", "Low"),
	"Duration of Effect" = c("Short", "Medium", "Long"),
	Accessibility = c("Inaccessible", "Neither accessible or inaccessible", "Accessible"),
	"Risk of Mild/Moderate Harm" = c("High", "Medium", "Low"),
	"Risk of Serious Harm" = c("High", "Medium", "Low"),
	"Effectiveness (Pain)" = c("Low", "Medium", "High"),
	"Effectiveness (Function)" = c("Low", "Medium", "High")
)
attributeLevels <- plyr::llply(attributeNames, function(x) 0:(length(x) - 1) / (length(x) - 1))
attributeWeights <- setNames(rep(100 / length(attributeNames), length(attributeNames)), names(attributeNames))

interventionNames <- clean_RACGP$Intervention
interventionTypes <- rep(1, length(interventionNames))

evidenceTables <- list(
	Overall = list(RACGP = create_evidence_table(clean_RACGP, 
																							 c("Rec", "Qua", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff", "Fun"))),
	Early = list(RACGP = create_evidence_table(clean_RACGP, 
																						 c("Rec1", "Qua1", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff", "Fun"))),
	Mid = list(RACGP = create_evidence_table(clean_RACGP, 
																					 c("Rec2", "Qua2", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff", "Fun"))),
	Late = list(RACGP = create_evidence_table(clean_RACGP, 
																						c("Rec3", "Qua3", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff", "Fun")))
)

evidenceTablesWeight <- c(RACGP = 1)

save(interventionNames, interventionTypes, 
		 evidenceTables, evidenceTablesWeight, 
		 attributeNames, attributeLevels, attributeWeights,
		 file = "data/evidenceTables.Rdata")