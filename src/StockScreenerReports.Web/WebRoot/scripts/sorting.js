// took this from
// https://lage.us/Javascript-Sort-HTML-Table-by-Column.html

/*
A few requirements for configuring the table:
1. The table must have id="sortable", i.e. <table id="sortable">
2. The table needs to have a table header, and the table header must have
   onclick="sortBy(n)", where n is the column number starting with 0
   i.e. <th onclick="sortBy(0)">Title of First Column</th>
*/

prevColumnIndex = -1; // global var saves the previous c, used to
            // determine if the same column is clicked again

function sortBy(header) {
    // first, determine the index being sorted
    var parentTr = header.parentElement;
    
    var index = -1;
    for (i=0; i<parentTr.children.length; i++) {
        if (parentTr.children[i] == header) {
            index = i;
            break;
        }
    }

    var table = parentTr.parentElement;
    
    rows = table.rows.length; // num of rows
    columns = table.rows[0].cells.length; // num of columns
    arrTable = [...Array(rows)].map(e => Array(columns)); // create an empty 2d array

    for (ro=0; ro<rows; ro++) { // cycle through rows
        for (co=0; co<columns; co++) { // cycle through columns
            arrTable[ro][co] = table.rows[ro].cells[co].innerHTML;
        }
    }

    th = arrTable.shift(); // remove the header row from the array, and save it
    
    if (index !== prevColumnIndex) { // different column is clicked, so sort by the new column
        arrTable.sort(
            function (a, b) {
                var aVal = a[index];
                var bVal = b[index];

                var aParsed = parseFloat(aVal);
                var bParsed = parseFloat(bVal);

                aVal = isNaN(aParsed) ? aVal : aParsed;
                bVal = isNaN(bParsed) ? bVal : bParsed;

                if (aVal === bVal) {
                    return 0;
                } else {
                    return (aVal < bVal) ? 1 : -1;
                }
            }
        );
    } else { // if the same column is clicked then reverse the array
        arrTable.reverse();
    }
    
    prevColumnIndex = index; // save in previous c

    arrTable.unshift(th); // put the header back in to the array

    // cycle through rows-columns placing values from the array back into the html table
    for (ro=0; ro<rows; ro++) {
        for (co=0; co<columns; co++) {
            table.rows[ro].cells[co].innerHTML = arrTable[ro][co];
        }
    }
}