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
	"Effectiveness" = c("Low", "Medium", "High")
)
attributeLevels <- plyr::llply(names(attributeNames), function(x) (clean_weights %>% filter(attribute==x))$weight)
attributeWeights <- setNames(plyr::laply(attributeLevels, max), names(attributeNames))
attributeLevels <- plyr::llply(attributeLevels, function(x) x / max(x))

interventionNames <- clean_RACGP$Intervention
interventionTypes <- rep("All", length(interventionNames))
interventionList <- setNames(lapply(unique(interventionTypes),
																		function(type) interventionNames[interventionTypes==type]),
														 unique(interventionTypes))

evidenceTables <- list(
	Overall = list(RACGP = create_evidence_table(clean_RACGP, c("Rec", "Qua", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Early = list(RACGP = create_evidence_table(clean_RACGP, c("Rec1", "Qua1", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Mid = list(RACGP = create_evidence_table(clean_RACGP, c("Rec2", "Qua2", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Late = list(RACGP = create_evidence_table(clean_RACGP, c("Rec3", "Qua3", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff")))
)

evidenceTablesWeight <- c(RACGP = 1)

save(interventionNames, interventionTypes, interventionList,
		 evidenceTables, evidenceTablesWeight, 
		 attributeNames, attributeLevels, attributeWeights,
		 file = "data/evidenceTables.Rdata")