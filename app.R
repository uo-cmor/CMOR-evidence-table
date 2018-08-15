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

# Define UI
ui <- fluidPage(
	titlePanel("CMOR Evidence Table and Preference Ranking for OA Interventions", windowTitle = "CMOR Evidence Table"),
	p("This application provides access to evidence tables for osteoarthritis interventions used in the project",
		"'The impact and management of rising osteoarthritis burden'.",
		"These data are preliminary and should not be cited as is."),
	sidebarLayout(
		sidebarPanel(width = 3,
			fluidRow(
				column(4, 
					strong("Disease Stage:"), actionLink("helpDiseaseStage", NULL, icon("info-circle")),
					radioButtons("diseaseStage", NULL,
											 choices = list("Overall", "Early", "Mid", "Late"),
											 selected = "Overall")
				),
				column(8,
					fluidRow(
						column(6,
							strong("Source of Evidence:"), actionLink("helpEvidence", NULL, icon("info-circle")),
							checkboxGroupInput("evidence", NULL, choices = list("RACGP" = 1), selected = 1)
						),
						column(6,
							strong("Interventions:"), actionLink("helpInterventions", NULL, icon("info-circle")),
							checkboxGroupInput("interventionTypes", NULL, choices = list("All"), selected = c("All"))
						)
					),
					fluidRow(
						column(8, strong("Filter:")), column(2, actionLink("clearFilter", "(Clear)"))
					),
					selectInput("interventions", NULL, multiple = TRUE, 
											choices = c("(All)" = "", interventionList))
				)
			),
			hr(),
			strong("Cost-effectiveness results:"), actionLink("helpCostEffectiveness", NULL, icon("info-circle")),
			checkboxGroupInput("cost_effectiveness", NULL,
												 choices = list("NZ health system perspective" = 1, "NZ societal perspective" = 2)),
			hr(),
			fluidRow(
				column(7, strong("Preference Weights:"), actionLink("helpPreferenceWeights", NULL, icon("info-circle")),
							 br(), actionLink("resetWeights", "(Reset)")),
				column(5, style = "text-align:right",
					actionLink("normaliseWeights", HTML("Normalise Weights<br>(sum to 100)"))
				)
			), br(),
			fluidRow(
				column(9, sliderInput("wgt_rec", names(attributeNames)[[1]], 
															0, 100, attributeWeights[[names(attributeNames)[[1]]]], 0.1)),
				column(3, br(), numericInput("wgt_rec_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[1]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_qua", names(attributeNames)[[2]], 
															0, 100, attributeWeights[[names(attributeNames)[[2]]]], 0.1)),
				column(3, br(), numericInput("wgt_qua_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[2]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_cos", names(attributeNames)[[3]], 
															0, 100, attributeWeights[[names(attributeNames)[[3]]]], 0.1)),
				column(3, br(), numericInput("wgt_cos_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[3]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_dur", names(attributeNames)[[4]], 
															0, 100, attributeWeights[[names(attributeNames)[[4]]]], 0.1)),
				column(3, br(), numericInput("wgt_dur_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[4]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_acc", names(attributeNames)[[5]], 
															0, 100, attributeWeights[[names(attributeNames)[[5]]]], 0.1)),
				column(3, br(), numericInput("wgt_acc_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[5]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_rmi", names(attributeNames)[[6]], 
															0, 100, attributeWeights[[names(attributeNames)[[6]]]], 0.1)),
				column(3, br(), numericInput("wgt_rmi_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[6]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_rse", names(attributeNames)[[7]], 
															0, 100, attributeWeights[[names(attributeNames)[[7]]]], 0.1)),
				column(3, br(), numericInput("wgt_rse_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[7]]]], 0, 100, 0.1))
			),
			fluidRow(
				column(9, sliderInput("wgt_eff", names(attributeNames)[[8]], 
															0, 100, attributeWeights[[names(attributeNames)[[8]]]], 0.1)),
				column(3, br(), numericInput("wgt_eff_val", NULL, 
																		 attributeWeights[[names(attributeNames)[[8]]]], 0, 100, 0.1))
			)
		),
		mainPanel(width = 9,
			tabsetPanel(type="tabs",
									tabPanel("Evidence Table", DTOutput("selectedEvidenceTable")),
									tabPanel("Plot", plotOutput("preferencePlot", click = clickOpts("plot_click", clip = FALSE))))
		)
	)
)


# Define server logic
server <- function(input, output, session) {
	# Reactive expressions (based on input values)
	evidence <- reactive({
		as.integer(input$evidence)
	})
	selected <- reactive({
		if (isTruthy(input$interventions)) {
			rownames(evidenceTables[[input$diseaseStage]][[1]]) %in% input$interventions
		} else interventionTypes %in% input$interventionTypes
	})
	sourceTables <- reactive({
		abind(evidenceTables[[input$diseaseStage]][evidence()], rev.along = 0)[selected(), , , drop = FALSE]
	})
	printTable <- reactive({
		req(sourceTables())
		
		out <- as.data.frame(round(apply(sourceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]))) %>%
			rownames_to_column("Intervention")
		l_ply(names(attributeNames), function(nm) out[[nm]] <<- factor(out[[nm]], levels = seq_along(attributeNames[[nm]]), 
																																	 labels = attributeNames[[nm]]))
		out <- out %>%
			mutate("Preference Score" = preferenceScores()) %>%
			arrange(desc(`Preference Score`)) %>%
			mutate("Preference Score" = round(`Preference Score`, 1))

		# out <- out[order(preferenceScores(), decreasing = TRUE), ]
		# 
		# out <- matrix(sapply(1:ncol(out), function(d) attributeNames[[d]][out[, d]]), ncol = length(attributeNames))
		# out <- cbind(interventionNames[selected()], out, round(preferenceScores(), 1))
		# colnames(out) <- c("Intervention", names(attributeNames), "Preference score")
		# rownames(out) <- as.character(1:nrow(out))
		
		out
	})
	preferenceTables <- reactive({
		req(sourceTables())
		out <- apply(sourceTables(), 3,
								 function(t) sapply(1:length(attributeNames), function(d) attributeLevels[[d]][t[, d]]))
		dim(out) <- dim(sourceTables())
		dimnames(out) <- dimnames(sourceTables())
		
		out
	})
	preferenceWeights <- reactive({
		out <- setNames(c(input$wgt_rec, input$wgt_qua, input$wgt_cos, input$wgt_dur, input$wgt_acc, 
											input$wgt_rmi, input$wgt_rse, input$wgt_eff), names(attributeNames))
		
		out / sum(out) * 100
	})
	preferenceScores <- reactive({
		pref <- apply(preferenceTables(), 3, function(x) x %*% preferenceWeights())
		dim(pref) <- dim(preferenceTables())[c(1, 3)]

		setNames(rowMeans(pref), interventionNames[selected()])
	})
	
	plotdata <- reactive({
		req(preferenceTables())
		
		apply(preferenceTables(), c(1, 2), weighted.mean, w = evidenceTablesWeight[evidence()]) %>%
			plyr::aaply(1, function(x) x * preferenceWeights(), .drop = FALSE) %>%
			as_tibble(rownames = "Intervention") %>%
			gather("attribute", "value", -1) %>%
			mutate(Intervention = fct_reorder(factor(Intervention), value, sum),
						 attribute = factor(attribute, levels = names(attributeNames)))
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
		sel <- printTable() %>%
			filter(Intervention %in% nearBars(plotdata() %>% spread(attribute, value) %>% 
																				transmute(Intervention, wgt = rowSums(select(., names(attributeNames)))), 
																			input$plot_click, xvar = "wgt",
																			threshold = 0, maxpoints = 1)$Intervention)
		selectedIntervention$name <- sel$Intervention
		selectedIntervention$label <- 
			with(sel,
					 paste0("Recommendation = ", Recommendation,
					 			 "\nQuality of Evidence = ", `Quality of Evidence`,
					 			 "\nCost = ", Cost,
					 			 "\nDuration of Effect = ", `Duration of Effect`,
					 			 "\nAccessibility = ", Accessibility,
					 			 "\nRisk of Mild/Moderate Harm = ", `Risk of Mild/Moderate Harm`,
					 			 "\nRisk of Serious Harm = ", `Risk of Serious Harm`,
					 			 "\nEffectiveness = ", `Effectiveness`))
		selectedIntervention$x <- input$plot_click$y
		selectedIntervention$y <- input$plot_click$x
	}, ignoreNULL = TRUE)

	# Output values (based on reactive expressions)
	output$selectedEvidenceTable <- renderDT({
		printTable()
	},
	autoHideNavigation = TRUE,
	style = 'bootstrap',
	options = list(pageLength = 25))
	
	output$preferencePlot <- renderPlot({
		plotdata <- req(plotdata())
		labdata <- tibble(x = selectedIntervention$x,
											y = selectedIntervention$y,
											name = selectedIntervention$name,
											label = selectedIntervention$label)

		ggplot(plotdata, aes(Intervention, value)) + 
			geom_col(aes(fill = attribute), colour = "white") + 
			geom_label(aes(x, y, label = name), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward", fontface="bold", colour = NA, alpha = 0.8) +
			geom_text(aes(x, y, label = name), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward", fontface="bold", nudge_x = -0.1, nudge_y = 0.1) +
			geom_label(aes(x, y, label = label), data = labdata, show.legend = FALSE,
								 hjust = "inward", vjust = "inward", nudge_x = -1.3) +
			coord_flip() +
			scale_fill_brewer("Attribute", type = "qual", palette = "Paired") +
			scale_y_continuous(NULL, limits = c(0, 100), expand = c(0, 0)) +
			scale_x_discrete(NULL)
	},
	height = function() max(200, 12 * dim(preferenceTables())[[1]]))

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
	
	observe({
		input$normaliseWeights
		wgt <- isolate(preferenceWeights())
		
		updateSliderInput(session, "wgt_rec", value = wgt[[1]])
		updateSliderInput(session, "wgt_qua", value = wgt[[2]])
		updateSliderInput(session, "wgt_cos", value = wgt[[3]])
		updateSliderInput(session, "wgt_dur", value = wgt[[4]])
		updateSliderInput(session, "wgt_acc", value = wgt[[5]])
		updateSliderInput(session, "wgt_rmi", value = wgt[[6]])
		updateSliderInput(session, "wgt_rse", value = wgt[[7]])
		updateSliderInput(session, "wgt_eff", value = wgt[[8]])
	})
	
	observe({ updateNumericInput(session, "wgt_rec_val", value = input$wgt_rec) })
	observe({ updateSliderInput(session, "wgt_rec", value = input$wgt_rec_val) })
	observe({ updateNumericInput(session, "wgt_qua_val", value = input$wgt_qua) })
	observe({ updateSliderInput(session, "wgt_qua", value = input$wgt_qua_val) })
	observe({ updateNumericInput(session, "wgt_cos_val", value = input$wgt_cos) })
	observe({ updateSliderInput(session, "wgt_cos", value = input$wgt_cos_val) })
	observe({ updateNumericInput(session, "wgt_dur_val", value = input$wgt_dur) })
	observe({ updateSliderInput(session, "wgt_dur", value = input$wgt_dur_val) })
	observe({ updateNumericInput(session, "wgt_acc_val", value = input$wgt_acc) })
	observe({ updateSliderInput(session, "wgt_acc", value = input$wgt_acc_val) })
	observe({ updateNumericInput(session, "wgt_rmi_val", value = input$wgt_rmi) })
	observe({ updateSliderInput(session, "wgt_rmi", value = input$wgt_rmi_val) })
	observe({ updateNumericInput(session, "wgt_rse_val", value = input$wgt_rse) })
	observe({ updateSliderInput(session, "wgt_rse", value = input$wgt_rse_val) })
	observe({ updateNumericInput(session, "wgt_eff_val", value = input$wgt_eff) })
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
	
	# Help dialog boxes
	observeEvent(input$helpDiseaseStage, {
		showModal(modalDialog(
			title = "Disease Stage:",
			"Select the disease stage for which to show evidence.", br(), br(),
			"Some interventions have stronger expert recommendation or quality of evidence at different stages of
			 the disease course",
			easyClose = TRUE, footer = NULL))
	})
	
	observeEvent(input$helpEvidence, {
		showModal(modalDialog(
			title = "Source of Evidence",
			"Different sources of evidence (e.g clinical practice guidelines, systematic reviews) can be selected.", 
			br(), br(),
			tags$dl(
				tags$dt("RACGP:"), tags$dd("Royal Australian College of General Practitioners clinical guidelines")
			),
			easyClose = TRUE, footer = NULL))
	})
	
	observeEvent(input$helpInterventions, {
		showModal(modalDialog(
			title = "Interventions",
			"Interventions can be filtered by type using these check boxes,",
			"and if desired further filtered by specific intervention in the filter selection box below.", br(), br(),
			"If no filter is applied, all interventions (of the selected types) will be shown.", br(), br(),
			"At this stage, no filters are implemented.",
			easyClose = TRUE, footer = NULL))
	})
	
	observeEvent(input$helpCostEffectiveness, {
		showModal(modalDialog(
			title = "Cost-effectiveness Results",
			"These are not yet available",
			easyClose = TRUE, footer = NULL))
	})
	
	observeEvent(input$helpPreferenceWeights, {
		showModal(modalDialog(
			title = "Preference Weights",
			"The attributes are aggregated, by default, using the New Zealand stakeholder preference weights elicited as 
			   part of our project 'The impact and management of rising osteoarthritis burden'.", br(), br(),
			"If you want to use alternative preference weights, these can be set using the sliders or numeric input boxes in
			   this section.", br(), 
			"All preference weights must sum to 100. This is done automatically for the table and plot; to see the 
         recalculated values in the input fields, use the 'Normalize Weights' button", br(),
			"Weights can be reset to the NZ stakeholder weights using the 'Reset' button",
			easyClose = TRUE, footer = NULL))
	})
	
	
}


# Run app
shinyApp(ui = ui, server = server)
