function toggle_visibility(id) {
  var e = document.getElementById(id);
  if(e.style.display == 'block')
    e.style.display = 'none';
  else
    e.style.display = 'block';
}

var training = 0;
function change_example(images) {
  document.getElementById("training").src=images[training];
  training = (training+1)%2;
}
