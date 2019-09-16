library(tidyverse)

load("data/data.Rdata")

create_evidence_table <- function(df) {
	out <- as.matrix(df %>% select(-Intervention))
	dimnames(out) <- list(Intervention = df$Intervention,
												Attribute = colnames(df)[-1])
	
	out
}

create_evidence_details_table <- function(df) {
	df %>%
		arrange(Intervention) %>%
		transmute(Intervention = str_c("Category: ", !!interventionTypes),
							Cost = str_c(Cost, coalesce(str_c(" ", Cost_frequency), "")),
							`Duration of Effect` = Duration,
							`Risk of Mild/Moderate Harm` = `Risk of mild to moderate side-effects`,
							`Risk of Serious Harm` = `Risk of serious harm`,
							Effectiveness = str_c("SMD (95% CI): ", Effectiveness))
}

# clean_RACGP <- clean_RACGP[complete.cases(clean_RACGP %>% 
# 																						select(c(c("Rec", "Qua", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
# 																										 c("Rec1", "Qua1", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
# 																										 c("Rec2", "Qua2", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff"),
# 																										 c("Rec3", "Qua3", "Cos", "Dur", "Acc", "Rmi", "Rse", "Eff")))), ]

attributeNames <- list(
	Recommendation = c("Strong Against", "Conditional Against", "Neutral", "Conditional For", "Strong For"),
	"Quality of Evidence" = c("Very low", "Low", "Moderate", "High"),
	"Effectiveness" = c("Low", "Medium", "High"),
	"Duration of Effect" = c("Short", "Short-Medium", "Medium", "Long"),
	"Risk of Serious Harm" = c("High", "Medium", "Low"),
	"Risk of Mild/Moderate Harm" = c("High", "Medium", "Low"),
	Cost = c("High", "Medium", "Low"),
	Accessibility = c("Inaccessible", "Neither accessible or inaccessible", "Accessible")
)
attributeLevels <- plyr::llply(names(attributeNames), function(x) (clean_weights %>% filter(attribute==x))$weight)
attributeWeights <- setNames(plyr::laply(attributeLevels, max), names(attributeNames))
attributeLevels <- plyr::llply(attributeLevels, function(x) x / max(x))

interventionNames <- clean_RACGP[[1]]$Intervention
interventionList <- list(
	"Alternative medicines" = list(
		"Avocado-soybean unsaponifiables", "Boswellia serrata extract", "Curcuma/curcuminoid", "Pycnogenol", "Glucosamine",
		"Chondroitin", "Glucosamine and chondroitin in compound form", "Vitamin D", "Omega-3 fatty acids", "Collagen",
		"Methylsulfonylmethane"
	),
	"Electrotherapies" = list(
		"Pulsed electromagnetic/shortwave therapy", "Other electrotherapy - interferential", "Other electrotherapy - laser",
		"Other electrotherapy - shockwave", "Transcutaneous electrical nerve stimulation (TENS)", "Therapeutic ultrasound", 
		"Acupuncture - electroacupuncture", "Acupuncture - laser", "Acupuncture - traditional with manual stimulation"
	),
	"Exercise interventions" = list(
		"Exercise - all land-based, muscle-strengthening, walking, Tai Chi", "Aquatic exercise/hydrotherapy", 
		"Exercise - stationary cycling only", "Exercise - stationary cycling & hatha yoga",
		"Exercise - muscle strengthening only for lower limb strengthening", 
		"Exercise - muscle strengthening only for quadriceps strengthening", "Exercise - Tai Chi only",
		"Exercise - walking only", "Exercise - yoga only"
	),
	"Injectable agents" = list(
		"Viscosupplementation injection", "Platelet-rich plasma injection", "Stem cell therapy", "Dextrose prolotherapy",
		"Fibroblast growth factor (not avaliable in NZ)", "Corticosteroid injection"
	),
	"Mechanical aids and devices" = list(
		"Knee braces - re-aligning patellofemoral braces", "Knee braces - valgus unloading/re-alignment braces",
		"Knee braces - varus unloading/re-alignment braces", 
		"Shoe orthotics - lateral wedge insoles for medial tibiofemoral knee OA",
		"Shoe orthotics - medial wedged insoles for lateral tibiofemoral OA and valgus deformity",
		"Shoe orthotics - shock absorbing insoles or arch supports", "Footwear - minimalist footwear", 
		"Footwear - rocker soled shoes", "Footwear - unloading shoes", "Taping - kinesio", "Taping - patellar",
		"Assistive walking device - cane"
	),
	"Pharmacological interventions (over the counter)" = list(
		"Paracetamol", "Topical capsaicin", "Topical NSAIDs"
	),
	"Pharmacological interventions (prescription medication only)" = list(
		"Interleukin-1 inhibitors", "Methotrexate", "Oral opioids", "Transdermal buprenorphine", "Transdermal Fentanyl",
		"Colchicine", "Anti-nerve growth factor (NGF)", "Calcitonin", "Biphosphonates", "Doxycycline",
		"Oral non-steroidal anti-inflammatory drugs (NSAIDs) including COX-2 inhibitors", "Diacerein (not available in NZ)",
		"Duloxetine (not available in NZ)", "Strontium ranelate"
	),
	"Psychological interventions" = list(
		"Cognitive behavioural therapy"
	),
	"Other physical therapies" = list(
		"Manual therapy - massage", "Manual therapy - mobilisation and manipulation"
	),
	"Self-management and education interventions" = list(
		"Self-management & education programs", "Heat therapy", "Cold therapy"
	),
	"Surgical interventions" = list(
		"Arthroscopic cartilage repair", "Arthroscopic lavage and debridement", "Arthroscopic meniscectomy", 
		"Total knee replacement"
	),
	"Weight management" = list(
		"Weight management"
	)
)
interventionTypes <- apply(sapply(interventionList, function(l) interventionNames %in% l), 1, function(x) names(x)[x])


# interventionTypes <- rep("All", length(interventionNames))
# interventionList <- setNames(lapply(unique(interventionTypes),
# 																		function(type) interventionNames[interventionTypes==type]),
# 														 unique(interventionTypes))

evidenceTables <- transpose(
	map(
		list(RACGP = clean_RACGP),
		function(evidence) map(
			list(
				Overall = map_dfc(transpose(evidence), ~do.call(pmax, .)),
				Early = evidence$EAR,
				Mid = evidence$MID,
				Late = evidence$ADV
			),
			create_evidence_table
		)
	)
)

# evidenceTables <- list(
# 	Overall = list(RACGP = create_evidence_table(clean_RACGP, c("Rec", "Qua", "Eff", "Dur", "Rse", "Rmi", "Cos", "Acc"))),
# 	Early = list(RACGP = create_evidence_table(clean_RACGP, c("Rec1", "Qua1", "Eff", "Dur", "Rse", "Rmi", "Cos", "Acc"))),
# 	Mid = list(RACGP = create_evidence_table(clean_RACGP, c("Rec2", "Qua2", "Eff", "Dur", "Rse", "Rmi", "Cos", "Acc"))),
# 	Late = list(RACGP = create_evidence_table(clean_RACGP, c("Rec3", "Qua3", "Eff", "Dur", "Rse", "Rmi", "Cos", "Acc")))
# )

evidenceTablesDetails <- transpose(
	map(
		list(RACGP = details),
		function(evidence) list(
			Overall = create_evidence_details_table(evidence),
			Early = create_evidence_details_table(evidence),
			Mid = create_evidence_details_table(evidence),
			Late = create_evidence_details_table(evidence)
		)
	)
)

evidenceTablesDetails_tibble <- evidenceTablesDetails %>% 
	map(bind_rows) %>%
	bind_rows() %>%
	rename_all(list(~paste0(., ".detail")))

evidenceTables_tibble <- evidenceTables %>% 
	map_depth(2, as_tibble, rownames = "Intervention") %>%
	map(bind_rows, .id = "source") %>%
	bind_rows(.id = "timing") %>%
	transmute(
		timing, source,
		Intervention = factor(Intervention, levels = interventionNames),
		Name = recode(
			Intervention,
			"Self-management & education programs" = "Self-management and education",
			"Exercise - all land-based, muscle-strengthening, walking, Tai Chi" = "Land-based exercise (all)",
			"Exercise - muscle strengthening only for quadriceps strengthening" = "Quadriceps strengthening",
			"Exercise - muscle strengthening only for lower limb strengthening" = "Lower limb strengthening",
			"Exercise - walking only" = "Walking",
			"Exercise - stationary cycling only" = "Stationary cycling",
			"Exercise - Tai Chi only" = "Tai chi",
			"Exercise - yoga only" = "Yoga",
			"Exercise - stationary cycling & hatha yoga" = "Stationary cycling, hatha yoga",
			"Aquatic exercise/hydrotherapy" = "Aquatic exercise",
			"Manual therapy - massage" = "Massage",
			"Manual therapy - mobilisation and manipulation" = "Knee mobilisation and manipulation",
			"Knee braces - varus unloading/re-alignment braces" = "Knee braces (varus)",
			"Knee braces - valgus unloading/re-alignment braces" = "Knee braces (valgus)",
			"Knee braces - re-aligning patellofemoral braces" = "Knee braces (patellofemoral)",
			"Shoe orthotics - shock absorbing insoles or arch supports" = "Shock absorbing insoles",
			"Shoe orthotics - lateral wedge insoles for medial tibiofemoral knee OA" = "Lateral wedge insoles",
			"Shoe orthotics - medial wedged insoles for lateral tibiofemoral OA and valgus deformity" = 
				"Medial wedged insoles",
			"Footwear - unloading shoes" = "Unloading shoes",
			"Footwear - minimalist footwear" = "Minimalist footwear",
			"Footwear - rocker soled shoes" = "Rocker soled shoes",
			"Taping - patellar" = "Patellar taping",
			"Taping - kinesio" = "Kinesio taping",
			"Other electrotherapy - laser"= "Laser electrotherapy",
			"Other electrotherapy - shockwave" = "Shockwave electrotherapy",
			"Other electrotherapy - interferential" = "Interferential electrotherapy",
			"Acupuncture - traditional with manual stimulation" = "Traditional acupuncture",
			"Acupuncture - electroacupuncture" = "Electroacupuncture",
			"Acupuncture - laser" = "Laser acupuncture",
			"Oral non-steroidal anti-inflammatory drugs (NSAIDs) including COX-2 inhibitors" = 
				"Oral NSAIDs (including COX-2 inhibitors)",
			"Glucosamine and chondroitin in compound form" = "Glucosamine and chondroitin",
			"Assistive walking device - cane" = "Assistive walking device"
		),
		`Recommendation` = factor(`Recommendation`, labels = attributeNames[["Recommendation"]]),
		`Quality of Evidence` = factor(`Quality of the evidence`, labels = attributeNames[["Quality of Evidence"]]),
		`Effectiveness` = factor(`Effectiveness`, labels = attributeNames[["Effectiveness"]]),
		`Duration of Effect` = factor(`Duration`, labels = attributeNames[["Duration of Effect"]]),
		`Risk of Serious Harm` = factor(`Risk of serious harm`, labels = attributeNames[["Risk of Serious Harm"]]),
		`Risk of Mild/Moderate Harm` = factor(`Risk of mild to moderate side-effects`, 
																					labels = attributeNames[["Risk of Mild/Moderate Harm"]]),
		`Cost` = factor(`Cost`, labels = attributeNames[["Cost"]]),
		`Accessibility` = factor(`Accessibility`, labels = attributeNames[["Accessibility"]])
	) %>%
	bind_cols(evidenceTablesDetails_tibble) %>%
	group_by(Intervention)

evidenceTablesWeight <- c(RACGP = 1)

costsTable <- clean_CE %>%
	select(Intervention, cost = `Incremental Costs`) %>%
	filter(!is.na(cost))

qalysTable <- clean_CE %>%
	select(Intervention, sf6d = `Incremental QALYs (SF-6D)`, womac = `Incremental QALYs (WOMAC)`) %>%
	gather("outcome", "qalys", sf6d:womac) %>%
	filter(!is.na(qalys))

cePlotTable <- tibble(
	Intervention = costsTable$Intervention,
	Name = recode(
		Intervention,
		"Self-management & education programs" = "Self-management and education",
		"Exercise - all land-based, muscle-strengthening, walking, Tai Chi" = "Land-based exercise (all)",
		"Exercise - muscle strengthening only for quadriceps strengthening" = "Quadriceps strengthening",
		"Exercise - muscle strengthening only for lower limb strengthening" = "Lower limb strengthening",
		"Exercise - walking only" = "Walking",
		"Exercise - stationary cycling only" = "Stationary cycling",
		"Exercise - Tai Chi only" = "Tai chi",
		"Exercise - yoga only" = "Yoga",
		"Exercise - stationary cycling & hatha yoga" = "Stationary cycling, hatha yoga",
		"Aquatic exercise/hydrotherapy" = "Aquatic exercise",
		"Manual therapy - massage" = "Massage",
		"Manual therapy - mobilisation and manipulation" = "Knee mobilisation and manipulation",
		"Knee braces - varus unloading/re-alignment braces" = "Knee braces (varus)",
		"Knee braces - valgus unloading/re-alignment braces" = "Knee braces (valgus)",
		"Knee braces - re-aligning patellofemoral braces" = "Knee braces (patellofemoral)",
		"Shoe orthotics - shock absorbing insoles or arch supports" = "Shock absorbing insoles",
		"Shoe orthotics - lateral wedge insoles for medial tibiofemoral knee OA" = "Lateral wedge insoles",
		"Shoe orthotics - medial wedged insoles for lateral tibiofemoral OA and valgus deformity" = 
			"Medial wedged insoles",
		"Footwear - unloading shoes" = "Unloading shoes",
		"Footwear - minimalist footwear" = "Minimalist footwear",
		"Footwear - rocker soled shoes" = "Rocker soled shoes",
		"Taping - patellar" = "Patellar taping",
		"Taping - kinesio" = "Kinesio taping",
		"Other electrotherapy - laser"= "Laser electrotherapy",
		"Other electrotherapy - shockwave" = "Shockwave electrotherapy",
		"Other electrotherapy - interferential" = "Interferential electrotherapy",
		"Acupuncture - traditional with manual stimulation" = "Traditional acupuncture",
		"Acupuncture - electroacupuncture" = "Electroacupuncture",
		"Acupuncture - laser" = "Laser acupuncture",
		"Oral non-steroidal anti-inflammatory drugs (NSAIDs) including COX-2 inhibitors" = 
			"Oral NSAIDs (including COX-2 inhibitors)",
		"Glucosamine and chondroitin in compound form" = "Glucosamine and chondroitin",
		"Assistive walking device - cane" = "Assistive walking device"
	),
	hjust = recode(
		Name,
		"Cognitive behavioural therapy" = 0, "Aquatic exercise" = 1, "Massage" = 0.5, "Assistive walking device" = 1,
		"Oral NSAIDs (including COX-2 inhibitors)" = 0, "Topical NSAIDs" = 1, "Duloxetine (not available in NZ)" = 0,
		"Corticosteroid injection" = 1, "Heat therapy" = 0,
		.default = 0
	),
	vjust = recode(
		Name,
		"Cognitive behavioural therapy" = 1, "Aquatic exercise" = 0, "Massage" = 1, 
		"Assistive walking device" = 0, "Oral NSAIDs (including COX-2 inhibitors)" = 0, "Topical NSAIDs" = 1, 
		"Duloxetine (not available in NZ)" = -0.1, "Corticosteroid injection" = 0, "Heat therapy" = 1,
		.default = 0
	),
	shape = if_else(Name %in% c("Massage"), "triangle", "diamond"),
	colour = recode(
		Name,
		"Cognitive behavioural therapy" = "#a6761d", "Aquatic exercise" = "#e7298a", "Massage" = "#666666", 
		"Heat therapy" = "#e41a1c", "Assistive walking device" = "#1b9e77", 
		"Oral NSAIDs (including COX-2 inhibitors)" = "#7570b3", "Topical NSAIDs" = "#d95f02", 
		"Duloxetine (not available in NZ)" = "#66a61e", "Corticosteroid injection" = "#e6ab02",
		.default = "#000000"
	)
)

save(interventionNames, interventionTypes, interventionList, evidenceTables_tibble,
		 evidenceTables, evidenceTablesWeight, evidenceTablesDetails,
		 attributeNames, attributeLevels, attributeWeights,
		 costsTable, qalysTable, cePlotTable,
		 file = "data/evidenceTables.Rdata")