function acknowledgeDateAlert(alertIdentifiers, rowIds) {
    
    for(let i = 0; i < alertIdentifiers.length; i++) {
        const alertIdentifier = alertIdentifiers[i];
        const rowId = rowIds[i];
        acknowledgeAlert(alertIdentifier, rowId);
    }
}

function acknowledgeAlert(alertIdentifier, rowId) {
    const url = `/alerts/acknowledge/${alertIdentifier}`;

    fetch(url, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({})
    })
        .then(response => {
            if (response.ok) {
                // If the acknowledgment is successful, remove the row from the UI
                const rowElement = document.getElementById(rowId);
                if (rowElement) {
                    rowElement.remove();
                }
            } else {
                // If there's an error, throw an alert with the error message
                return response.text().then(errorMessage => {
                    throw new Error(errorMessage);
                });
            }
        })
        .catch(error => {
            alert(`Error: ${error.message}`);
        });
}