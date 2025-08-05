(function() {
  var password = prompt("Enter password:");
  if (password !== "uhsm") {
    document.body.style.display = "none";
    alert("Incorrect password");
    window.location.reload();
  }
})();