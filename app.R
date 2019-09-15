##################
# Load libraries #
##################
library(shiny)
library(DT)
library(plyr)
library(tidyverse)
library(abind)

###############################
# Load data (evidence tables) #
###############################
load("data/evidenceTables.Rdata")

##################
# Source scripts #
##################
source("scripts/plot-utils.R")
summarise_attributes <- function(level, source) {
	if (length(unique(level)) == 1) return(level[[1]])
	paste(source, level, sep = ": ", collapse = "\n")
}

#######################
# Define server logic #
#######################
server <- function(input, output, session) {
	################################################
	# Reactive expressions (based on input values) #
	################################################
	selected <- reactive({
		if (isTruthy(input$interventions)) {
			rownames(evidenceTables[[input$diseaseStage]][[1]]) %in% input$interventions
		} else {
			interventionTypes %in% input$interventionTypes
		}
	})
	
	evidenceTables_timing <- reactive({
		evidenceTables_tibble[evidenceTables_tibble$timing == input$diseaseStage, , drop = FALSE]
	})
	
	sourceTables <- reactive({ 
		evidenceTables_timing()[evidenceTables_timing()$source %in% input$evidence, , drop = FALSE]
	})
	
	interventionTables <- reactive({
		sourceTables <- req(sourceTables())
		
		sourceTables %>%
			summarise(
				Name = Name[[1]],
				`Recommendation.score` = mean(attributeLevels[[1]][`Recommendation`]),
				`Quality of Evidence.score` = mean(attributeLevels[[2]][`Quality of Evidence`]),
				`Effectiveness.score` = mean(attributeLevels[[3]][`Effectiveness`]),
				`Duration of Effect.score` = mean(attributeLevels[[4]][`Duration of Effect`]),
				`Risk of Serious Harm.score` = mean(attributeLevels[[5]][`Risk of Serious Harm`]),
				`Risk of Mild/Moderate Harm.score` = mean(attributeLevels[[6]][`Risk of Mild/Moderate Harm`]),
				`Cost.score` = mean(attributeLevels[[7]][`Cost`]),
				`Accessibility.score` = mean(attributeLevels[[8]][`Accessibility`]),
				`Recommendation` = summarise_attributes(`Recommendation`, source),
				`Quality of Evidence` = summarise_attributes(`Quality of Evidence`, source),
				`Effectiveness` = summarise_attributes(`Effectiveness`, source),
				`Duration of Effect` = summarise_attributes(`Duration of Effect`, source),
				`Risk of Serious Harm` = summarise_attributes(`Risk of Serious Harm`, source),
				`Risk of Mild/Moderate Harm` = summarise_attributes(`Risk of Mild/Moderate Harm`, source),
				`Cost` = summarise_attributes(`Cost`, source),
				`Accessibility` = summarise_attributes(`Accessibility`, source),
				Intervention.detail = Intervention.detail[[1]],
				`Effectiveness.detail` = summarise_attributes(`Effectiveness.detail`, source),
				`Duration of Effect.detail` = summarise_attributes(`Duration of Effect.detail`, source),
				`Risk of Serious Harm.detail` = summarise_attributes(`Risk of Serious Harm.detail`, source),
				`Risk of Mild/Moderate Harm.detail` = summarise_attributes(`Risk of Mild/Moderate Harm.detail`, source),
				`Cost.detail` = summarise_attributes(`Cost.detail`, source)
			)
	})
	
	filteredTables <- reactive({
		interventionTables <- req(interventionTables())
		
		interventionTables[selected(), , drop = FALSE]
	})
	
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_rec, input$wgt_qua, input$wgt_eff, input$wgt_dur, input$wgt_rse, input$wgt_rmi,
											input$wgt_cos, input$wgt_acc),
										names(attributeNames))
		
		out <- out[order(out)]
		out / sum(out) * 100
	})
	
	preferenceScores <- reactive({
		filteredTables <- req(filteredTables())
		preferenceWeights <- preferenceWeights()
		
		filteredTables %>%
			transmute(
				Intervention, Name,
				`Recommendation` = `Recommendation.score` * preferenceWeights[["Recommendation"]],
				`Quality of Evidence` = `Quality of Evidence.score` * preferenceWeights[["Quality of Evidence"]],
				`Effectiveness` = `Effectiveness.score` * preferenceWeights[["Effectiveness"]],
				`Duration of Effect` = `Duration of Effect.score` * preferenceWeights[["Duration of Effect"]],
				`Risk of Serious Harm` = `Risk of Serious Harm.score` * preferenceWeights[["Risk of Serious Harm"]],
				`Risk of Mild/Moderate Harm` = `Risk of Mild/Moderate Harm.score` * preferenceWeights[["Risk of Mild/Moderate Harm"]],
				`Cost` = `Cost.score` * preferenceWeights[["Cost"]],
				`Accessibility` = `Accessibility.score` * preferenceWeights[["Accessibility"]],
				PreferenceScores = Recommendation + `Quality of Evidence` + Cost + `Duration of Effect` + Accessibility + 
					                   `Risk of Mild/Moderate Harm` + `Risk of Serious Harm` + Effectiveness
			)
	})
	
	plotdata <- reactive({
		preferenceScores <- req(preferenceScores())
		preferenceWeights <- preferenceWeights()
		
		preferenceScores <- preferenceScores %>% 
			mutate(rank = rank(-PreferenceScores)) %>%
			filter(rank <= as.integer(input$plot_n))
		
		if (nrow(preferenceScores) > 0) 
			preferenceScores <- preferenceScores %>% 
			  mutate(Intervention = fct_reorder(factor(Intervention), PreferenceScores),
			  			 Name = fct_reorder(factor(Name), PreferenceScores))
		
		preferenceScores %>%
			select("Intervention", "Name", "rank", names(preferenceWeights), "PreferenceScores") %>%
			gather("attribute", "value", -c(Intervention, Name, rank, PreferenceScores), factor_key = TRUE)
	})

	printTable <- reactive({
		filteredTables <- req(filteredTables())
		preferenceScores <- req(preferenceScores())
		
		filteredTables %>%
			transmute(
				Name.sort = as.character(Name), Cost.sort = Cost, `Duration of Effect.sort` = `Duration of Effect`,
				`Risk of Mild/Moderate Harm.sort`= `Risk of Mild/Moderate Harm`, 
				`Risk of Serious Harm.sort` = `Risk of Serious Harm`, `Effectiveness.sort` = Effectiveness, 
				`Preference Score.sort` = !!preferenceScores$PreferenceScores,
				Name = paste0("<span title = \"", `Intervention.detail`, "\"> ", Name, "</span>"), 
				Recommendation, `Quality of Evidence`, 
				Effectiveness = paste0("<span title = \"", `Effectiveness.detail`, "\"> ", Effectiveness, "</span>"),
				`Duration of Effect` = paste0("<span title = \"", `Duration of Effect.detail`, "\"> ",
																			`Duration of Effect`, "</span>"),
			  `Risk of Serious Harm` = paste0("<span title = \"", `Risk of Serious Harm.detail`, "\"> ",
			  																`Risk of Serious Harm`, "</span>"),
				`Risk of Mild/Moderate Harm` = 
					paste0("<span title = \"", `Risk of Mild/Moderate Harm.detail`, "\"> ",
								 `Risk of Mild/Moderate Harm`, "</span>"),
				Accessibility, Cost = paste0("<span title = \"", `Cost.detail`, "\"> ", Cost, "</span>"),
				`Preference Score` = paste0("<span title = \"", `Preference Score.sort`, "\"> ",
																		round(`Preference Score.sort`, 1), "</span>")
			) %>%
			arrange(desc(`Preference Score.sort`))
	})

	selectedIntervention <- reactiveValues( # For storing which point to show label for
		name = character(0), 
		label = character(0), 
		x = numeric(0), 
		y = numeric(0)
	)
	
	observeEvent(plotdata(), {
		selectedIntervention$name <- character(0)
		selectedIntervention$label <- character(0)
		selectedIntervention$x <- numeric(0)
		selectedIntervention$y <- numeric(0)
	})
	
	observeEvent(input$plot_click, {
		sel <- nearBars(plotdata() %>% spread(attribute, value) %>% 
											transmute(Name, wgt = rowSums(select(., names(attributeNames)))), 
										input$plot_click, xvar = "wgt",
										threshold = 0, maxpoints = 1)$Name
		labels <- filteredTables() %>% filter(Name %in% sel)
		values <- plotdata() %>% filter(Name %in% sel) %>% spread(attribute, value)
		if (length(sel)>0) values <- values %>% mutate_at(names(attributeNames), round, 1)
		
		selectedIntervention$name <- sel
		selectedIntervention$label <- 
			paste0(
				sel, ":",
				"\nRecommendation = ", labels$Recommendation, " (", values$Recommendation, ")",
				"\nQuality of Evidence = ", labels$`Quality of Evidence`, " (", values$`Quality of Evidence`, ")",
				"\nEffectiveness = ", labels$Effectiveness, " (", values$Effectiveness, ")",
				"\nDuration of Effect = ", labels$`Duration of Effect`, " (", values$`Duration of Effect`, ")",
				"\nRisk of Serious Harm = ", labels$`Risk of Serious Harm`, " (", values$`Risk of Serious Harm`, ")",
				"\nRisk of Mild/Moderate Harm = ", labels$`Risk of Mild/Moderate Harm`, 
				  " (", values$`Risk of Mild/Moderate Harm`, ")",
				"\nCost = ", labels$Cost, " (", values$Cost, ")", 
				"\nAccessibility = ", labels$Accessibility, " (", values$Accessibility, ")"
			)
		selectedIntervention$x <- if(is.null(input$plot_click$y)) numeric(0) else input$plot_click$y
		selectedIntervention$y <- if(is.null(input$plot_click$x)) numeric(0) else input$plot_click$x
	}, ignoreNULL = TRUE)
	
	costs <- reactive({
		costsTable[costsTable$Intervention %in% input$interventionsCE, , drop = FALSE]
	})
	
	qalys <- reactive({
		qalysTable[qalysTable$Intervention %in% input$interventionsCE, , drop = FALSE] %>%
			filter(outcome == input$hrqol)
	})
	
	ceTable <- reactive({
		full_join(costs(), qalys(), by = "Intervention") %>%
			left_join(cePlotTable, by = "Intervention")
	})
	
	ceData <- reactive({
		ceTable() %>%
			mutate(wtp = input$wtp,
						 inb = qalys * input$wtp - cost)
	})
	
	cePlotData <- reactive({
		ceData <- req(ceData())
		prefData <- req(printTable())
		
		ceData %>%
			left_join(prefData, by = c("Name" = "Name.sort")) %>%
			mutate(pref_score = (`Preference Score.sort` - min(`Preference Score.sort`)) / (max(`Preference Score.sort`) - min(`Preference Score.sort`)),
						 inb_score = (inb - min(inb)) / (max(inb) - min(inb)),
						 weighted_score = pref_score * (100 - input$ce_wgt) + inb_score * input$ce_wgt)
	})
	
	#################################################
	# Output values (based on reactive expressions) #
	#################################################
	output$showplot <- reactive({ any(selected()) })
	outputOptions(output, "showplot", suspendWhenHidden = FALSE)
	
	output$selectedEvidenceTable <- renderDT({
		printTable <- req(printTable())
		
		sort_cols <- str_detect(names(printTable), ".*\\.sort$")
		sort_indices <- which(sort_cols)
		vars_sort <- names(printTable)[sort_indices]
		vars_details <- str_remove(vars_sort, "\\.sort$")
		details_indices <- match(vars_details, names(printTable))
		
		datatable(printTable,
							autoHideNavigation = TRUE,
							escape = FALSE,
							class = "display compact",
							options = list(
								pageLength = 25,
								columnDefs = c(map(sort_indices, ~list(targets = ., visible = FALSE, searchable = FALSE)),
															 map2(details_indices, sort_indices, ~list(targets = .x, orderData = .y)))
							))
	})
	
	output$preferencePlot <- renderPlot({
		
		plotdata <- req(plotdata())
		labdata <- tibble(x = selectedIntervention$x,
											y = selectedIntervention$y,
											name = selectedIntervention$name,
											label = selectedIntervention$label)

		ggplot(plotdata, aes(Name, value)) + 
			geom_col(aes(fill = attribute), colour = NA, width = 0.7) + 
			geom_text(aes(y = PreferenceScores, label = format(PreferenceScores, digits = 0, nsmall = 1)),
								data = partial(filter, ... = , attribute == "Recommendation"),
								size = 5, hjust = -0.3) +
			geom_col(aes(group = attribute), fill = NA, width = 0.7, size = 1, colour = "black",
							 data = filter(plotdata, Name %in% labdata$name), show.legend = FALSE) +
			geom_label(aes(x, y, label = label), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward") +
			geom_hline(yintercept = 100) +
			coord_flip() +
			scale_fill_manual(NULL,
												values = c("Recommendation" = "#fa68b4", "Quality of Evidence" = "#ef8080", 
																	 "Effectiveness" = "#b954d2", "Duration of Effect" = "#99cc31",
																	 "Risk of Serious Harm" = "#4581b4", "Risk of Mild/Moderate Harm" = "#3fe0cf",
																	 "Cost" = "#86cdeb", "Accessibility" = "#31cd31")) +
			scale_y_continuous(NULL, limits = c(0, 100), expand = c(0, 0), sec.axis = dup_axis(), 
												 breaks = seq(0, 100, 10), minor_breaks = seq(10, 90, 10)) +
			scale_x_discrete(NULL) +
			guides(fill = guide_legend(reverse = TRUE, label.position = "bottom", 
																 byrow = ifelse(session$clientData$output_preferencePlot_width >= 768, FALSE, TRUE),
																 nrow = ifelse(session$clientData$output_preferencePlot_width >= 1000, 1, 
																 							ifelse(session$clientData$output_preferencePlot_width >= 768, 2, 4)),
																 keywidth = unit(ifelse(session$clientData$output_preferencePlot_width >= 1050, 3, 1), 
																 								"lines"))) +
			theme_classic() +
			theme(legend.position = "bottom",
						legend.justification = c(0, 0),
						legend.background = element_rect(fill = NA, linetype = 0),
						legend.key = element_rect(fill = NA, linetype = 0),
						plot.margin = margin(0, 10, 0, 0, "pt"),
						plot.background = element_rect(fill = "#fdfaf1"),
						panel.grid.major.x = element_line(colour = "#eee6c6"),
						panel.background = element_rect(fill = "#fdfaf1"),
						axis.text.y = element_text(size = 16))
	}, height = function() max(280, 90 + 30 * nrow(plotdata()) / 8))
	
	output$selectedCETable <- renderTable({
		dat <- req(cePlotData())

		dat %>%
			arrange(desc(weighted_score)) %>%
			transmute(" " = as.integer(rank(-weighted_score)), 
								Intervention = recode(Name,
																			"Cognitive behavioural therapy" = "CBT",
																			"Assistive walking device" = "Walking cane",
																			"Oral NSAIDs (including COX-2 inhibitors)" = "Oral NSAIDs",
																			"Corticosteroid injection" = "Corticosteroids",
																			"Duloxetine (not available in NZ)" = "Duloxetine"),
								"Preference Score (0-100)" = format(`Preference Score.sort`, digits = 0, nsmall = 1),
								"Incremental Net Benefit (NZD per capita)" = format(inb, digits = 0, nsmall = 0, scientific = FALSE),
								"Weighted Score (0-100)" = format(weighted_score, digits = 0, nsmall = 0))
	},
	striped = TRUE, hover = TRUE, bordered = TRUE, align = "llccc")
	
	output$cePlot <- renderPlot({
		plotdata <- req(cePlotData())
		
		plotdata <- plotdata %>% 
			mutate(
				name = recode(Name,
											"Cognitive behavioural therapy" = "CBT",
											# "Aquatic exercise" = "Aquatic\nexercise",
											Massage = "Massage\n(incremental cost = $4 100)",
											"Heat therapy" = "Heat therapy\n(incremental cost = $45 600)",
											"Assistive walking device" = "Walking cane",
											"Oral NSAIDs (including COX-2 inhibitors)" = "Oral NSAIDs",
											# "Topical NSAIDs" = "Topical\nNSAIDs",
											"Corticosteroid injection" = "Corticosteroids",
											"Duloxetine (not available in NZ)" = "Duloxetine"),
				cost = case_when(Intervention == "Manual therapy - massage" & "Heat therapy" %in% ceTable()$Intervention ~ 2700,
												 Intervention == "Manual therapy - massage" ~ 1900,
												 Intervention == "Heat therapy" ~ 3200, TRUE ~ cost),
				qalys = case_when(Intervention == "Heat therapy" & input$hrqol == "sf6d" ~ 0.018, TRUE ~ qalys)
		  )
		
		ggplot(plotdata, aes(qalys, cost, colour = colour)) +
			geom_point(aes(shape = shape), size = 6) +
			geom_label(aes(label = name, size = rank(weighted_score)), hjust = "inward", vjust = "inward", lineheight = 0.9, fill = NA, label.size = 0) +
			scale_x_continuous("Lifetime incremental QALYs (per-capita)", breaks = scales::pretty_breaks()) +
			scale_y_continuous("Lifetime incremental costs (per-capita, 2013 NZD)", breaks = scales::pretty_breaks()) +
			scale_shape_identity() + scale_colour_identity() +
			scale_size_continuous(range = c(8, 16), guide = FALSE) +
			geom_hline(yintercept = 0, size = 1, colour = "grey50") +
			geom_vline(xintercept = 0, size = 1, colour = "grey50") +
			geom_abline(slope = 52373, size = 2, alpha = 0.1, colour = "#377eb8") +
			geom_abline(slope = 52373 * 2, size = 2, alpha = 0.1, colour = "#377eb8") +
			geom_abline(slope = 52373 * 3, size = 2, alpha = 0.1, colour = "#377eb8") +
			geom_abline(slope = input$wtp, size = 2, alpha = 0.2, colour = "red") +
			theme_minimal(base_size = 20) +
			theme(text = element_text(colour = "grey50"),
						axis.text = element_text(colour = "grey50"),
						plot.margin = margin(0, 10, 0, 0, "pt"),
						plot.background = element_rect(fill = "#fdfaf1", linetype = 0))
	}, height = 500)
	
	########################
	# Update input widgets #
	########################
	observe({
		input$resetWeights
		
		updateSliderInput(session, "wgt_rec", value = attributeWeights[["Recommendation"]])
		updateSliderInput(session, "wgt_qua", value = attributeWeights[["Quality of Evidence"]])
		updateSliderInput(session, "wgt_eff", value = attributeWeights[["Effectiveness"]])
		updateSliderInput(session, "wgt_dur", value = attributeWeights[["Duration of Effect"]])
		updateSliderInput(session, "wgt_rse", value = attributeWeights[["Risk of Serious Harm"]])
		updateSliderInput(session, "wgt_rmi", value = attributeWeights[["Risk of Mild/Moderate Harm"]])
		updateSliderInput(session, "wgt_cos", value = attributeWeights[["Cost"]])
		updateSliderInput(session, "wgt_acc", value = attributeWeights[["Accessibility"]])
	})
	
	observeEvent(input$normaliseWeights, {
		wgt <- preferenceWeights()
		
		updateSliderInput(session, "wgt_rec", value = wgt[["Recommendation"]])
		updateSliderInput(session, "wgt_qua", value = wgt[["Quality of Evidence"]])
		updateSliderInput(session, "wgt_eff", value = wgt[["Effectiveness"]])
		updateSliderInput(session, "wgt_dur", value = wgt[["Duration of Effect"]])
		updateSliderInput(session, "wgt_rse", value = wgt[["Risk of Serious Harm"]])
		updateSliderInput(session, "wgt_rmi", value = wgt[["Risk of Mild/Moderate Harm"]])
		updateSliderInput(session, "wgt_cos", value = wgt[["Cost"]])
		updateSliderInput(session, "wgt_acc", value = wgt[["Accessibility"]])
	})

	observe({
		input$resetWeights_ce
		
		updateSliderInput(session, "wgt_rec_ce", value = attributeWeights[["Recommendation"]])
		updateSliderInput(session, "wgt_qua_ce", value = attributeWeights[["Quality of Evidence"]])
		updateSliderInput(session, "wgt_eff_ce", value = attributeWeights[["Effectiveness"]])
		updateSliderInput(session, "wgt_dur_ce", value = attributeWeights[["Duration of Effect"]])
		updateSliderInput(session, "wgt_rse_ce", value = attributeWeights[["Risk of Serious Harm"]])
		updateSliderInput(session, "wgt_rmi_ce", value = attributeWeights[["Risk of Mild/Moderate Harm"]])
		updateSliderInput(session, "wgt_cos_ce", value = attributeWeights[["Cost"]])
		updateSliderInput(session, "wgt_acc_ce", value = attributeWeights[["Accessibility"]])
	})
	
	observeEvent(input$normaliseWeights_ce, {
		wgt <- preferenceWeights()
		
		updateSliderInput(session, "wgt_rec_ce", value = wgt[["Recommendation"]])
		updateSliderInput(session, "wgt_qua_ce", value = wgt[["Quality of Evidence"]])
		updateSliderInput(session, "wgt_eff_ce", value = wgt[["Effectiveness"]])
		updateSliderInput(session, "wgt_dur_ce", value = wgt[["Duration of Effect"]])
		updateSliderInput(session, "wgt_rse_ce", value = wgt[["Risk of Serious Harm"]])
		updateSliderInput(session, "wgt_rmi_ce", value = wgt[["Risk of Mild/Moderate Harm"]])
		updateSliderInput(session, "wgt_cos_ce", value = wgt[["Cost"]])
		updateSliderInput(session, "wgt_acc_ce", value = wgt[["Accessibility"]])
	})
	
	observe({ updateSliderInput(session, "wgt_rec", value = input$wgt_rec_val) })
	observe({ updateSliderInput(session, "wgt_qua", value = input$wgt_qua_val) })
	observe({ updateSliderInput(session, "wgt_cos", value = input$wgt_cos_val) })
	observe({ updateSliderInput(session, "wgt_dur", value = input$wgt_dur_val) })
	observe({ updateSliderInput(session, "wgt_acc", value = input$wgt_acc_val) })
	observe({ updateSliderInput(session, "wgt_rmi", value = input$wgt_rmi_val) })
	observe({ updateSliderInput(session, "wgt_rse", value = input$wgt_rse_val) })
	observe({ updateSliderInput(session, "wgt_eff", value = input$wgt_eff_val) })

	observe({ updateSliderInput(session, "wgt_rec", value = input$wgt_rec_ce) })
	observe({ updateSliderInput(session, "wgt_qua", value = input$wgt_qua_ce) })
	observe({ updateSliderInput(session, "wgt_cos", value = input$wgt_cos_ce) })
	observe({ updateSliderInput(session, "wgt_dur", value = input$wgt_dur_ce) })
	observe({ updateSliderInput(session, "wgt_acc", value = input$wgt_acc_ce) })
	observe({ updateSliderInput(session, "wgt_rmi", value = input$wgt_rmi_ce) })
	observe({ updateSliderInput(session, "wgt_rse", value = input$wgt_rse_ce) })
	observe({ updateSliderInput(session, "wgt_eff", value = input$wgt_eff_ce) })
	
	observe({ updateSliderInput(session, "wgt_rec_ce", value = input$wgt_rec) })
	observe({ updateSliderInput(session, "wgt_qua_ce", value = input$wgt_qua) })
	observe({ updateSliderInput(session, "wgt_cos_ce", value = input$wgt_cos) })
	observe({ updateSliderInput(session, "wgt_dur_ce", value = input$wgt_dur) })
	observe({ updateSliderInput(session, "wgt_acc_ce", value = input$wgt_acc) })
	observe({ updateSliderInput(session, "wgt_rmi_ce", value = input$wgt_rmi) })
	observe({ updateSliderInput(session, "wgt_rse_ce", value = input$wgt_rse) })
	observe({ updateSliderInput(session, "wgt_eff_ce", value = input$wgt_eff) })
	
	observe({ updateSliderInput(session, "wgt_rec_ce", value = input$wgt_rec_val_ce) })
	observe({ updateSliderInput(session, "wgt_qua_ce", value = input$wgt_qua_val_ce) })
	observe({ updateSliderInput(session, "wgt_cos_ce", value = input$wgt_cos_val_ce) })
	observe({ updateSliderInput(session, "wgt_dur_ce", value = input$wgt_dur_val_ce) })
	observe({ updateSliderInput(session, "wgt_acc_ce", value = input$wgt_acc_val_ce) })
	observe({ updateSliderInput(session, "wgt_rmi_ce", value = input$wgt_rmi_val_ce) })
	observe({ updateSliderInput(session, "wgt_rse_ce", value = input$wgt_rse_val_ce) })
	observe({ updateSliderInput(session, "wgt_eff_ce", value = input$wgt_eff_val_ce) })
	
	observeEvent(input$interventionTypes, { 
		updateSelectInput(session, "interventions", 
											choices = if (isTruthy(input$interventionTypes)) {
												c("(All)" = "", interventionList[input$interventionTypes]) 
											} else c("None Selected!" = ""),
											selected = "")},
		ignoreInit = TRUE, ignoreNULL = FALSE)
	
	observeEvent(input$clearFilter, { updateSelectInput(session, "interventions", selected = "")},
							 ignoreInit = TRUE)
	
	observeEvent(input$allInterventions, {
		updateCheckboxGroupInput(session, "interventionTypes", selected = unique(interventionTypes))
	})
	
	observeEvent(input$noInterventions, {
		updateCheckboxGroupInput(session, "interventionTypes", selected = "")
	})
	
	observeEvent(input$allInterventions_ce, {
		updateCheckboxGroupInput(session, "interventionsCE", selected = costsTable$Intervention)
	})
	
	observeEvent(input$noInterventions_ce, {
		updateCheckboxGroupInput(session, "interventionsCE", selected = "")
	})
}

###########
# Run app #
###########
shinyApp(ui = htmlTemplate("www/index.html"), server = server)
