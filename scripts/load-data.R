library(tidyverse)
library(readxl)

raw_RACGP <- read_xlsx("data/raw/Performance-Matrix-RW.xlsx", skip = 1)
raw_weights <- read_xlsx("data/raw/Attribute-Weights.xlsx", skip = 1)

clean_RACGP <- raw_RACGP %>%
	filter(`1000minds variable names` %in% c(1, 10)) %>%
	transmute(Intervention = 
							str_remove_all(Alternative, 
														 paste0("( . knee( ((\\(same as hip\\))|(and/or hip \\(same for both\\))|(\\(not available in NZ\\))))*)|",
														 			  "( \\(same ((as)|(for)) hip\\))|",
														 			  "( . same for knee and hip.*)|( . same for hip and knee)|",
														 			  "(\\. Should be considered as an investigational medication only)")),
						Rec = case_when(Rec=="\u2191\u2191" ~ 5L, Rec=="\u2191" ~ 4L, Rec=="\u2194" ~ 3L, Rec=="\u2193" ~ 2L, 
														Rec=="\u2193\u2193" ~ 1L),
						Rec1 = as.integer(Rec1), Rec2 = as.integer(Rec2), Rec3 = as.integer(Rec3),
						Qua = case_when(Qua=="\u2713\u2713\u2713\u2713" ~ 4L, Qua=="\u2713\u2713\u2713" ~ 3L, 
														Qua=="\u2713\u2713" ~ 2L, Qua=="\u2713" ~ 1L,
														Intervention=="Total Joint Replacement" ~ 2L),
						Qua1 = if_else(Intervention=="Total Joint Replacement", 1L, Qua),
						Qua2 = if_else(Intervention=="Total Joint Replacement", 1L, Qua),
						Qua3 = if_else(Intervention=="Total Joint Replacement", 4L, Qua),
						Cos = case_when(Cos=="$" ~ 3, Cos=="$$" ~ 2, Cos=="$$$" ~ 1),
						Dur = case_when(`Dur\r\n`=="\u29d7\u29d7\u29d7\u29d7" ~ 4, `Dur\r\n`=="\u29d7\u29d7\u29d7" ~ 3, 
														`Dur\r\n`=="\u29d7\u29d7" ~ 2, `Dur\r\n`=="\u29d7" ~ 1),
						Acc = case_when(Acc=="\u267f\u267f\u267f" ~ 3, Acc=="\u267f\u267f" ~ 2, Acc=="\u267f" ~ 1),
						Rmi = case_when(Rmi=="\u26a0" ~ 3, Rmi=="\u26a0\u26a0" ~ 2, Rmi=="\u26a0\u26a0\u26a0" ~ 1),
						Rse = case_when(Rse=="\u26a0" ~ 3, Rse=="\u26a0\u26a0" ~ 2, Rse=="\u26a0\u26a0\u26a0" ~ 1),
						Eff = case_when(Eff=="\u2695\u2695\u2695" ~ 3, Eff=="\u2695\u2695" ~ 2, Eff=="\u2695" ~ 1),
						"Cost value" = `Cost value`, "Cost duration" = `Cost duration`, "Duration value" = `Duration value`,
						"Rmi value" = `Rmi value`, "Rse value" = `Rse value`, "Effectiveness value" = `Effectiveness value`)

clean_weights <- raw_weights %>%
	gather("level", "weight", 2:6, na.rm = TRUE) %>%
	transmute(attribute = str_replace_all(X__1, c("Risk of mild to moderate side-effects" = "Risk of Mild/Moderate Harm",
																								"Risk of serious harm" = "Risk of Serious Harm",
																								"Duration" = "Duration of Effect",
																								"Quality of the evidence" = "Quality of Evidence")),
						level = as.integer(level), weight)

save(clean_RACGP, clean_weights, file = "data/data.Rdata")
