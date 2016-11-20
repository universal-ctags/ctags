function scrollEditBox() {
	var editBoxEl = document.getElementById("wpTextbox1");
	var scrollTopEl = document.getElementById("wpScrolltop");
	var editFormEl = document.getElementById("editform");

	if (editBoxEl && scrollTopEl) {
		if (scrollTopEl.value) editBoxEl.scrollTop = scrollTopEl.value;
		editFormEl.onsubmit = function() {
			document.getElementById("wpScrolltop").value = document.getElementById("wpTextbox1").scrollTop;
		}
	}
}
