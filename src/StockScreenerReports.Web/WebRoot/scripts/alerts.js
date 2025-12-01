async function acknowledgeDateAlert(alertIdentifiers, rowIds) {
    
    for(let i = 0; i < alertIdentifiers.length; i++) {
        const alertIdentifier = alertIdentifiers[i];
        const rowId = rowIds[i];
        await acknowledgeAlert(alertIdentifier, rowId);
    }
}

async function acknowledgeAlert(alertIdentifier, rowId) {
    const url = `/alerts/acknowledge/${alertIdentifier}`;

    try {
        const response = await fetch(url, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body: JSON.stringify({})
        });

        if (response.ok) {
            // If the acknowledgment is successful, remove the row from the UI
            const rowElement = document.getElementById(rowId);
            if (rowElement) {
                rowElement.remove();
            }
        } else {
            // If there's an error, throw an alert with the error message
            const errorMessage = await response.text();
            throw new Error(errorMessage);
        }
    } catch (error) {
        alert(`Error: ${error.message}`);
    }
}