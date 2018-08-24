// Define variables
var arrow_icon = document.querySelectorAll(".fa-angle-down");

//Assign behavior to option buttons
var button_options = document.querySelector("#btn-options");
var button_weights = document.querySelector("#btn-weights");
var button_interventions = document.querySelector("#btn-interventions");

function assignButtons(button) {
  button.addEventListener("click", function(){
    if (this.children[1].className === "fas fa-angle-down") {
      this.children[1].className = "fas fa-angle-down fa-flip-vertical";
    } else {
      this.children[1].className = "fas fa-angle-down";
    }
  });
}
assignButtons(button_options);
assignButtons(button_weights);
assignButtons(button_interventions);

//Expand plot
var button_expand = document.querySelector("#button-expand");
var pref_plot = document.querySelector("#preferencePlot");
var expand_text = document.querySelector("#expand-text");

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
