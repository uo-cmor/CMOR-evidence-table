// Define variables
var options_panel = document.querySelector("#options");
var interventions_panel = document.querySelector("#interventions");
var weights_panel = document.querySelector("#weights");
var options_tab = document.querySelector("#options-tab");
var weights_tab = document.querySelector("#weights-tab");
var interventions_tab = document.querySelector("#interventions-tab");
var panel_button = document.querySelector("#panel-button");

//Expand plot
var button_expand = document.querySelector("#button-expand");
var pref_plot = document.querySelector("#preferencePlot");
var expand_text = document.querySelector("#expand-text");
var arrow_icon = document.querySelectorAll(".fa-angle-down");

button_expand.addEventListener("click", function(){
  if (pref_plot.style.height === "auto") {
    pref_plot.style.height = "285px";
    expand_text.textContent = "Expand ";
    arrow_icon[3].className = "fas fa-angle-down";
  } else {
    pref_plot.style.height = "auto";
    expand_text.textContent = "Collapse ";
    arrow_icon[3].className = "fas fa-angle-down fa-flip-vertical";
  }
});

//Toggle options and weights panels
function togglePanel(tab, panel) {
  tab.addEventListener("click", function(){
    if (panel.style.display === "none") {
          panel.style.display = "block";
      } else {
          panel.style.display = "none";
      }
  });
}

togglePanel(options_tab, options_panel);
togglePanel(weights_tab, weights_panel);
togglePanel(interventions_tab, interventions_panel);

//Connect slider with numeric inputs
function updateWeights(input, output) {
  document.getElementById(input).onchange = function() {
    document.getElementById(output).value = this.value;
  }
}

updateWeights("wgt_rec", "wgt_rec_val");
updateWeights("wgt_qua", "wgt_qua_val");
updateWeights("wgt_cos", "wgt_cos_val");
updateWeights("wgt_dur", "wgt_dur_val");
updateWeights("wgt_acc", "wgt_acc_val");
updateWeights("wgt_rmi", "wgt_rmi_val");
updateWeights("wgt_rse", "wgt_rse_val");
updateWeights("wgt_eff", "wgt_eff_val");
