
function toggleStyle(cell) {
    var existing = cell.style.display;
    if (existing === "none") {
        cell.style.display = null;
    } else {
        cell.style.display = "none";
    }
}
function toggleTickerCompanyVisibility() {
    // in html find all tables in the current document
    var tables = document.getElementsByTagName("table");

    console.log("Found " + tables.length + " tables");

    // go over each header row and determine if they have title ticker or company
    for (var i = 0; i < tables.length; i++) {
        var table = tables[i];
        var headerRow = table.rows[0];
        var headerCells = headerRow.cells;
        var tickerIndex = -1;
        var companyIndex = -1;
        for (var j = 0; j < headerCells.length; j++) {
            var headerCell = headerCells[j];
            if (headerCell.innerHTML == "Ticker") {
                tickerIndex = j;
            }
            if (headerCell.innerHTML == "Company") {
                companyIndex = j;
            }
        }

        // if both ticker and company are found, then hide the company column
        if (tickerIndex != -1 && companyIndex != -1) {
            for (var j = 0; j < table.rows.length; j++) {
                var row = table.rows[j];
                var tickerCell = row.cells[tickerIndex];
                toggleStyle(tickerCell);
                var companyCell = row.cells[companyIndex];
                toggleStyle(companyCell);
            }
        }
    }
}