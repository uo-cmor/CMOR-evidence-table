# Load libraries
library(shiny)
library(DT)
library(plyr)
library(tidyverse)
library(abind)

# Load data (evidence tables)
load("data/evidenceTables.Rdata")


# Source scripts
source("scripts/plot-utils.R")
summarise_attributes <- function(level, source) {
	if (length(unique(level)) == 1) return(level[[1]])
	paste(source, level, sep = ": ", collapse = "\n")
}

# Define server logic
server <- function(input, output, session) {
	# Reactive expressions (based on input values)
	selected <- reactive({
		if (isTruthy(input$interventions)) {
			rownames(evidenceTables[[input$diseaseStage]][[1]]) %in% input$interventions
		} else interventionTypes %in% input$interventionTypes
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
				`Recommendation.score` = mean(attributeLevels[[1]][`Recommendation`]),
				`Quality of Evidence.score` = mean(attributeLevels[[2]][`Quality of Evidence`]),
				`Cost.score` = mean(attributeLevels[[3]][`Cost`]),
				`Duration of Effect.score` = mean(attributeLevels[[4]][`Duration of Effect`]),
				`Accessibility.score` = mean(attributeLevels[[5]][`Accessibility`]),
				`Risk of Mild/Moderate Harm.score` = mean(attributeLevels[[6]][`Risk of Mild/Moderate Harm`]),
				`Risk of Serious Harm.score` = mean(attributeLevels[[7]][`Risk of Serious Harm`]),
				`Effectiveness.score` = mean(attributeLevels[[8]][`Effectiveness`]),
				`Recommendation` = summarise_attributes(`Recommendation`, source),
				`Quality of Evidence` = summarise_attributes(`Quality of Evidence`, source),
				`Cost` = summarise_attributes(`Cost`, source),
				`Duration of Effect` = summarise_attributes(`Duration of Effect`, source),
				`Accessibility` = summarise_attributes(`Accessibility`, source),
				`Risk of Mild/Moderate Harm` = summarise_attributes(`Risk of Mild/Moderate Harm`, source),
				`Risk of Serious Harm` = summarise_attributes(`Risk of Serious Harm`, source),
				`Effectiveness` = summarise_attributes(`Effectiveness`, source),
				Intervention.detail = Intervention.detail[[1]],
				`Cost.detail` = summarise_attributes(`Cost.detail`, source),
				`Duration of Effect.detail` = summarise_attributes(`Duration of Effect.detail`, source),
				`Risk of Mild/Moderate Harm.detail` = summarise_attributes(`Risk of Mild/Moderate Harm.detail`, source),
				`Risk of Serious Harm.detail` = summarise_attributes(`Risk of Serious Harm.detail`, source),
				`Effectiveness.detail` = summarise_attributes(`Effectiveness.detail`, source)
			)
	})
	
	filteredTables <- reactive({
		interventionTables <- req(interventionTables())
		
		interventionTables[selected(), , drop = FALSE]
	})
	
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_rec, input$wgt_qua, input$wgt_cos, input$wgt_dur, input$wgt_acc, 
											input$wgt_rmi, input$wgt_rse, input$wgt_eff), names(attributeNames))
		
		out / sum(out) * 100
	})
	
	preferenceScores <- reactive({
		filteredTables <- req(filteredTables())
		preferenceWeights <- preferenceWeights()
		
		filteredTables %>%
			transmute(
				Intervention,
				`Recommendation` = `Recommendation.score` * preferenceWeights[[1]],
				`Quality of Evidence` = `Quality of Evidence.score` * preferenceWeights[[2]],
				`Cost` = `Cost.score` * preferenceWeights[[3]],
				`Duration of Effect` = `Duration of Effect.score` * preferenceWeights[[4]],
				`Accessibility` = `Accessibility.score` * preferenceWeights[[5]],
				`Risk of Mild/Moderate Harm` = `Risk of Mild/Moderate Harm.score` * preferenceWeights[[6]],
				`Risk of Serious Harm` = `Risk of Serious Harm.score` * preferenceWeights[[7]],
				`Effectiveness` = `Effectiveness.score` * preferenceWeights[[8]],
				PreferenceScores = Recommendation + `Quality of Evidence` + Cost + `Duration of Effect` + Accessibility + 
					                   `Risk of Mild/Moderate Harm` + `Risk of Serious Harm` + Effectiveness
			)
	})
	
	plotdata <- reactive({
		preferenceScores <- req(preferenceScores())
		
		preferenceScores <- preferenceScores %>% 
			mutate(rank = rank(-PreferenceScores)) %>%
			filter(rank <= as.integer(input$plot_n))
		
		if (nrow(preferenceScores) > 0) 
			preferenceScores <- preferenceScores %>% 
			  mutate(Intervention = fct_reorder(factor(Intervention), PreferenceScores))
		
		preferenceScores %>%
			select(-PreferenceScores) %>%
			gather("attribute", "value", -c(Intervention, rank), factor_key = TRUE)
	})

	printTable <- reactive({
		filteredTables <- req(filteredTables())
		preferenceScores <- req(preferenceScores())
		
		filteredTables %>%
			transmute(
				Cost.sort = Cost, `Duration of Effect.sort` = `Duration of Effect`,
				`Risk of Mild/Moderate Harm.sort`= `Risk of Mild/Moderate Harm`, 
				`Risk of Serious Harm.sort` = `Risk of Serious Harm`, `Effectiveness.sort` = Effectiveness,
				Intervention = as.character(Intervention), Recommendation, `Quality of Evidence`,
				Cost = paste0("<span title = \"", `Cost.detail`, "\"> ", Cost, "</span>"),
				`Duration of Effect` = paste0("<span title = \"", `Duration of Effect.detail`, "\"> ",
																			`Duration of Effect`, "</span>"),
				Accessibility,
				`Risk of Mild/Moderate Harm` = 
					paste0("<span title = \"", `Risk of Mild/Moderate Harm.detail`, "\"> ",
								 `Risk of Mild/Moderate Harm`, "</span>"),
			  `Risk of Serious Harm` = paste0("<span title = \"", `Risk of Serious Harm.detail`, "\"> ",
			  																`Risk of Serious Harm`, "</span>"),
				Effectiveness = paste0("<span title = \"", `Effectiveness.detail`, "\"> ", Effectiveness, "</span>"),
				`Preference Score` = !!preferenceScores$PreferenceScores
			) %>%
			arrange(desc(`Preference Score`))
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
											transmute(Intervention, wgt = rowSums(select(., names(attributeNames)))), 
										input$plot_click, xvar = "wgt",
										threshold = 0, maxpoints = 1)$Intervention
		#print(input$plot_click)
		# print(plotdata() %>% spread(attribute, value) %>% 
		# 				transmute(Intervention, wgt = rowSums(select(., names(attributeNames)))))
		labels <- filteredTables() %>% filter(Intervention %in% sel)
		#print(labels)
		values <- plotdata() %>% filter(Intervention %in% sel) %>% spread(attribute, value)
		#print(values)
		if (length(sel)>0) values <- values %>% mutate_at(names(attributeNames), round, 1)
		
		selectedIntervention$name <- sel
		selectedIntervention$label <- 
			paste0(
				"Recommendation = ", labels$Recommendation, " (", values$Recommendation, ")",
				"\nQuality of Evidence = ", labels$`Quality of Evidence`, " (", values$`Quality of Evidence`, ")",
				"\nCost = ", labels$Cost, " (", values$Cost, ")",
				"\nDuration of Effect = ", labels$`Duration of Effect`, " (", values$`Duration of Effect`, ")",
				"\nAccessibility = ", labels$Accessibility, " (", values$Accessibility, ")",
				"\nRisk of Mild/Moderate Harm = ", 
				labels$`Risk of Mild/Moderate Harm`, " (", values$`Risk of Mild/Moderate Harm`, ")",
				"\nRisk of Serious Harm = ", labels$`Risk of Serious Harm`, " (", values$`Risk of Serious Harm`, ")",
				"\nEffectiveness = ", labels$Effectiveness, " (", values$Effectiveness, ")"
			)
		selectedIntervention$x <- input$plot_click$y
		selectedIntervention$y <- input$plot_click$x
	}, ignoreNULL = TRUE)
	
	# Output values (based on reactive expressions)
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
		
		ggplot(plotdata, aes(Intervention, value)) + 
			geom_col(aes(fill = attribute), colour = NA, width = 0.7) + 
			geom_col(aes(group = attribute), fill = NA, width = 0.7, size = 1, colour = "black",
							 data = filter(plotdata, Intervention %in% labdata$name), show.legend = FALSE) +
			geom_label(aes(x, y, label = label), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward") +
			coord_flip() +
			scale_fill_manual(NULL, values = c("#31cd31", "#86cdeb", "#3fe0cf", "#4581b4", "#99cc31", "#b954d2", "#ef8080", "#fa68b4")) +
			scale_y_continuous(NULL, limits = c(0, 100), expand = c(0, 0), sec.axis = dup_axis(), 
												 breaks = seq(0, 90, 10), minor_breaks = NULL) +
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
						plot.margin = margin(0, 0, 0, 5, "pt"),
						plot.background = element_rect(fill = "#FDFAF1"),
						panel.background = element_rect(fill = "#FDFAF1"))
	},
	height = function() max(280, 90 + 30 * nrow(plotdata()) / 8))
	
	# Update input widgets
	observe({
		input$resetWeights
		
		updateSliderInput(session, "wgt_rec", value = attributeWeights[[1]])
		updateSliderInput(session, "wgt_qua", value = attributeWeights[[2]])
		updateSliderInput(session, "wgt_cos", value = attributeWeights[[3]])
		updateSliderInput(session, "wgt_dur", value = attributeWeights[[4]])
		updateSliderInput(session, "wgt_acc", value = attributeWeights[[5]])
		updateSliderInput(session, "wgt_rmi", value = attributeWeights[[6]])
		updateSliderInput(session, "wgt_rse", value = attributeWeights[[7]])
		updateSliderInput(session, "wgt_eff", value = attributeWeights[[8]])
	})
	
	observeEvent(input$normaliseWeights, {
		wgt <- preferenceWeights()
		
		updateSliderInput(session, "wgt_rec", value = wgt[[1]])
		updateSliderInput(session, "wgt_qua", value = wgt[[2]])
		updateSliderInput(session, "wgt_cos", value = wgt[[3]])
		updateSliderInput(session, "wgt_dur", value = wgt[[4]])
		updateSliderInput(session, "wgt_acc", value = wgt[[5]])
		updateSliderInput(session, "wgt_rmi", value = wgt[[6]])
		updateSliderInput(session, "wgt_rse", value = wgt[[7]])
		updateSliderInput(session, "wgt_eff", value = wgt[[8]])
	})
	
	observe({ updateSliderInput(session, "wgt_rec", value = input$wgt_rec_val) })
	observe({ updateSliderInput(session, "wgt_qua", value = input$wgt_qua_val) })
	observe({ updateSliderInput(session, "wgt_cos", value = input$wgt_cos_val) })
	observe({ updateSliderInput(session, "wgt_dur", value = input$wgt_dur_val) })
	observe({ updateSliderInput(session, "wgt_acc", value = input$wgt_acc_val) })
	observe({ updateSliderInput(session, "wgt_rmi", value = input$wgt_rmi_val) })
	observe({ updateSliderInput(session, "wgt_rse", value = input$wgt_rse_val) })
	observe({ updateSliderInput(session, "wgt_eff", value = input$wgt_eff_val) })
	
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
}


# Run app
shinyApp(ui = htmlTemplate("www/index.html"), server = server)
