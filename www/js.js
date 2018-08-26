//---------- Define variables --------------------//
var arrow_icon = document.querySelectorAll(".fa-angle-down");

//---------- Assign behavior to option buttons ---//
var dropdown = document.querySelector(".option-buttons-inner");

document.body.addEventListener("click", function(evt){
  if(dropdown.contains(evt.target)) return;
  else turnArrowsDown();
});

function assignButtons(button) {
  button.addEventListener("click", function(ev){
    if (this.getAttribute("aria-expanded") === "false") {
      turnArrowsDown();
      this.children[1].className = "fas fa-angle-down fa-flip-vertical";
    } else {
      this.children[1].className = "fas fa-angle-down";
    }
  });
}
assignButtons(document.querySelector("#btn-options"));
assignButtons(document.querySelector("#btn-weights"));
assignButtons(document.querySelector("#btn-interventions"));

function turnArrowsDown() {
  arrow_icon.forEach(function(icon){
    icon.className = "fas fa-angle-down";
  });
}
//---------- Expand plot ---------------------------//
var button_expand = document.querySelector("#button-expand");
var pref_plot = document.querySelector("#preferencePlot");
var navbar = document.querySelector(".navbar");

button_expand.addEventListener("click", function(){
  pref_plot.style.height = "auto";
  this.style.display = "none";
  navbar.style.display = "block";
});

//---------- Connect slider with numeric inputs ----//
function updateWeights(input, output) {
  document.getElementById(input).onchange = function() {
    document.getElementById(output).value = this.value;
  }
};
updateWeights("wgt_rec", "wgt_rec_val");
updateWeights("wgt_qua", "wgt_qua_val");
updateWeights("wgt_cos", "wgt_cos_val");
updateWeights("wgt_dur", "wgt_dur_val");
updateWeights("wgt_acc", "wgt_acc_val");
updateWeights("wgt_rmi", "wgt_rmi_val");
updateWeights("wgt_rse", "wgt_rse_val");
updateWeights("wgt_eff", "wgt_eff_val");
