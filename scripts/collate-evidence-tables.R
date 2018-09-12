library(tidyverse)

load("data/data.Rdata")

create_evidence_table <- function(df, varnames) {
	out <- as.matrix(df %>% select(varnames))
	dimnames(out) <- list(Intervention = interventionNames,
												Attribute = names(attributeNames))
	
	out
}

create_evidence_details_table <- function(df) {
	transmute(df, 
						Intervention = str_c("Category: ", interventionTypes),
						Cost = str_c(`Cost value`, coalesce(str_c(" ", `Cost duration`), "")),
						"Duration of Effect" = `Duration value`,
						"Risk of Mild/Moderate Harm" = `Rmi value`,
						"Risk of Serious Harm" =`Rse value`,
						Effectiveness = str_c("SMD (95% CI): ", `Effectiveness value`))
}

clean_RACGP <- clean_RACGP[complete.cases(clean_RACGP %>% 
																						select(c(c("Rec", "Qua", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
																										 c("Rec1", "Qua1", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
																										 c("Rec2", "Qua2", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
																										 c("Rec3", "Qua3", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff")))), ]

attributeNames <- list(
	Recommendation = c("Strong Against", "Conditional Against", "Neutral", "Conditional For", "Strong For"),
	"Quality of Evidence" = c("Very low", "Low", "Moderate", "High"),
	Cost = c("High", "Medium", "Low"),
	"Duration of Effect" = c("Short", "Short-Medium", "Medium", "Long"),
	Accessibility = c("Inaccessible", "Neither accessible or inaccessible", "Accessible"),
	"Risk of Mild/Moderate Harm" = c("High", "Medium", "Low"),
	"Risk of Serious Harm" = c("High", "Medium", "Low"),
	"Effectiveness" = c("Low", "Medium", "High")
)
attributeLevels <- plyr::llply(names(attributeNames), function(x) (clean_weights %>% filter(attribute==x))$weight)
attributeWeights <- setNames(plyr::laply(attributeLevels, max), names(attributeNames))
attributeLevels <- plyr::llply(attributeLevels, function(x) x / max(x))

interventionNames <- clean_RACGP$Intervention
interventionList <- list(
	"Alternative medicines" = list("Avocado-soybean unsaponifiables", "Boswellia serrata extract", "Curcuma/curcuminoid",
																 "Pycnogenol", "Glucosamine", "Chondroitin", 
																 "Glucosamine and chondroitin in compound form", "Vitamin D", "Omega-3 fatty acids",
																 "Collagen", "Methylsulfonylmethane"),
	"Electrotherapies" = list("Pulsed electromagnetic/ shortwave therapy", "Other electrotherapy (interferential)", 
												 		"Other electrotherapy (laser)", "Other electrotherapy (shockwave)", 
												 		"Transcutaneous electrical nerve stimulation (TENS)", "Therapeutic ultrasound", 
												 		"Acupuncture (electroacupuncture)", "Acupuncture (laser)",
												 		"Acupuncture (traditional with manual stimulation)"),
	"Exercise interventions" = list("ALL LAND-BASED EXERCISE (all land based, muscle-strengthening, walking, Tai Chi)",
															 		"Aquatic exercise/ hydrotherapy", "Knee exercise: Stationary cycling only", 
															 		"Knee exercise: Land-based exercise (stationary cycling, hatha yoga)",
															 		"Knee exercise: MUSCLE STRENGTHENING ONLY for lower limb strengthening", 
															 		"Knee exercise: MUSCLE STRENGTHENING ONLY for quadriceps strengthening",
															 		"Knee exercise: Tai Chi only", "Knee exercise: Walking only", 
															 		"Knee exercise: Yoga only"),
	"Injectable agents" = list("Viscosupplementation injection", "Platelet-rich plasma (PRP) injection", 
														 "Stem cell therapy", "Dextrose prolotherapy", "Fibroblast growth factor (FGF)", 
														 "Corticosteroid injection"),
	"Mechanical aids and devices" = list(
		"Knee braces (re-aligning patellofemoral braces)", "Knee braces (valgus unloading/re-alignment braces)",
		"Knee braces (varus unloading/re-alignment braces)", 
		"Shoe orthotics (lateral wedge insoles for medial tibiofemoral knee OA)",
		"Shoe orthotics (medial wedged insoles for lateral tibiofemoral OA and valgus deformity)",
		"Shoe orthotics (shock absorbing insoles or arch supports)", "Footwear (minimalist footwear)", 
		"Footwear (rocker soled shoes)", "Footwear (unloading shoes)", "Taping (kinesio taping)", 
		"Taping (patellar taping)", "Assistive walking device"
	),
	"Pharmacological interventions (over the counter)" = list("Paracetamol", "Topical capsaicin", "Topical NSAIDs"),
	"Pharmacological interventions (prescription medication only)" = list(
		"Interleukin-1 (IL-1) inhibitors", "Methotrexate", "Oral opioids", "Transdermal opioids - buprenorphine", 
		"Transdermal opioids - Fentanyl", "Colchicine", "Anti-nerve growth factor (NGF)", "Calcitonin", "Biphosphonates", 
		"Doxycycline", "Oral non-steroidal anti-inflammatory drugs (NSAIDs) including COX-2 inhibitors", "Diacerein",
		"Duloxetine", "Strontium ranelate"),
	"Psychological interventions" = list("Cognitive behavioural therapy"),
	"Other physical therapies" = list("Manual therapy (massage)", "Manual therapy (mobilisation and manipulation)"),
	"Self-management and education interventions" = list("self-management education programs", "Heat therapy", 
																											 "Cold therapy"),
	"Surgical interventions" = list("Arthroscopic cartilage repair ", "Arthroscopic lavage and debridement",
															 	  "Arthroscopic meniscectomy ", "Total Joint Replacement"),
	"Weight management" = list("Weight management")
)
interventionTypes <- apply(sapply(interventionList, function(l) interventionNames %in% l), 1, function(x) names(x)[x])


# interventionTypes <- rep("All", length(interventionNames))
# interventionList <- setNames(lapply(unique(interventionTypes),
# 																		function(type) interventionNames[interventionTypes==type]),
# 														 unique(interventionTypes))

evidenceTables <- list(
	Overall = list(RACGP = create_evidence_table(clean_RACGP, c("Rec", "Qua", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Early = list(RACGP = create_evidence_table(clean_RACGP, c("Rec1", "Qua1", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Mid = list(RACGP = create_evidence_table(clean_RACGP, c("Rec2", "Qua2", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"))),
	Late = list(RACGP = create_evidence_table(clean_RACGP, c("Rec3", "Qua3", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff")))
)

evidenceTablesDetails <- list(
	Overall = list(RACGP = create_evidence_details_table(clean_RACGP)),
	Early = list(RACGP = create_evidence_details_table(clean_RACGP)),
	Mid = list(RACGP = create_evidence_details_table(clean_RACGP)),
	Late = list(RACGP = create_evidence_details_table(clean_RACGP))
)

evidenceTablesWeight <- c(RACGP = 1)

save(interventionNames, interventionTypes, interventionList,
		 evidenceTables, evidenceTablesWeight, evidenceTablesDetails,
		 attributeNames, attributeLevels, attributeWeights,
		 file = "data/evidenceTables.Rdata")