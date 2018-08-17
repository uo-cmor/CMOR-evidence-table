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
options_tab.addEventListener("click", function(){
  // if (options_panel.style.display === "none") {
  //       options_panel.style.display = "block";
  //   } else {
  //       options_panel.style.display = "none";
  //   }
    alert("yo");
});
