function ts_resortTable(lnk) {
    if (span.getAttribute("sortdir") == 'down') {
        span.setAttribute('sortdir','up');
    } else {
        span.setAttribute('sortdir','down');
    }
}
function getParent(el, pTagName) {
	if (el == null) return null;
	else if (el.nodeType == 1 && el.tagName.toLowerCase() == pTagName.toLowerCase())	// Gecko bug, supposed to be uppercase
		return el;
	else
		return getParent(el.parentNode, pTagName);
}
function ts_sort_currency(a,b) { 
    aa = ts_getInnerText(a.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
    bb = ts_getInnerText(b.cells[SORT_COLUMN_INDEX]).replace(/[^0-9.]/g,'');
    return parseFloat(aa) - parseFloat(bb);
}
function checkForUpdate() {
    if( 1==1 ) {
        document.write("hello from checkForUpdate<br>")
    }
    return 1;
}
function checkForUpdate2() {
    if( 1==1 ) {
        document.write("hello from checkForUpdate<br>");
    }
    return 2;
}

