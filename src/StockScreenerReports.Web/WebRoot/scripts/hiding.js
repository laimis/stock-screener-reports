
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

function toggleEarningsVisibility() {
    // in html find all tables in the current document
    var tables = document.getElementsByTagName("table");

    // for each table, see if a row has a cell with an i element with class set to fa-solid fa-e
    for (var i = 0; i < tables.length; i++) {
        var table = tables[i];
        var rows = table.rows;
        for (var j = 0; j < rows.length; j++) {
            var row = rows[j];
            var cells = row.cells;
            for (var k = 0; k < cells.length; k++) {
                var cell = cells[k];
                var iElements = cell.getElementsByTagName("i");
                for (var l = 0; l < iElements.length; l++) {
                    var iElement = iElements[l];
                    if (iElement.classList.contains("fa-solid") && iElement.classList.contains("fa-e")) {
                        toggleStyle(row);
                    }
                }
            }
        }
    }
}

selectedIndustry = ""
function industryClicked(event) {
    
    var td = event.target
    var ahref = td.children[0]
    var text = ahref.innerText

    if (selectedIndustry === text) {
        selectedIndustry = ""
    }
    else {
        selectedIndustry = text
    }

    toggleTableRowsForMatchingCategory("Industry", selectedIndustry)
}

selectedSector = ""
function sectorClicked(event) {
    
    var td = event.target
    var ahref = td.children[0]
    var text = ahref.innerText

    if (selectedSector === text) {
        selectedSector = ""
    }
    else {
        selectedSector = text
    }

    toggleTableRowsForMatchingCategory("Sector", selectedSector)
}

selectedCountry = ""
function countryClicked(event) {
    
    var td = event.target
    var ahref = td.children[0]
    var text = ahref.innerText

    if (selectedCountry === text) {
        selectedCountry = ""
    }
    else {
        selectedCountry = text
    }

    toggleTableRowsForMatchingCategory("Country", selectedCountry)
}

function toggleTableRowsForMatchingCategory(category, selectedValue) {
    

    // first, find all tables
    var tables = document.getElementsByTagName("table")

    // see if it has a header row with cell called "Industry"
    for (var i = 0; i < tables.length; i++) {
        var table = tables[i]
        var header = table.tHead
        var headerRow = header.rows[0]

        var industryCellIndex = -1
        for (var j = 0; j < headerRow.cells.length; j++) {
            var headerCell = headerRow.cells[j]

            if (headerCell.innerText == category) {
                industryCellIndex = j
                break
            }
        }

        if (industryCellIndex == -1) {
            continue
        }

        var rows = table.rows
        // hide rows that don't match selected industry
        for (var j = 1; j < rows.length; j++) {
            var row = rows[j]
            var td = row.cells[industryCellIndex]
            var ahref = td.children[0]
            var text = ahref.innerText

            if (text == selectedValue || selectedValue == "") {
                row.classList.remove("is-hidden")
            } else {
                row.classList.add("is-hidden")
            }
        }
    }
}