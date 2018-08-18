// Define variables
var options_panel = document.querySelector(".options");
var weights_panel = document.querySelector(".weights");
var options_tab = document.querySelector("#options-tab");
var weights_tab = document.querySelector("#weights-tab");

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
