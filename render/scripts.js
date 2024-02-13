var currentId;

console.log("scripts.js loaded");

const highlightRow = () => {
    // Get the fragment identifier from the URL
    var id = location.hash.substring(1);

    // If a row is currently highlighted, remove the highlight
    if (currentId) {
        var currentRow = document.getElementById(currentId);
        if (currentRow) {
            currentRow.style.backgroundColor = '';
        }
    }

    // Get the row with the corresponding id
    var row = document.getElementById(id);

    // Change the background color of the row
    if (row) {
        row.style.backgroundColor = 'yellow';
        // Update the currently highlighted row
        currentId = id;
    }
}

const updateURL = (href) => (event) => {
    console.log("updateURL called");
    // Get the fragment identifier from the href
    var id = href.split('#')[1];

    // Get the page from the href
    var page = href.split('#')[0].replace("http://", "").replace("https://", "").replace("file://", "");

    console.log("called updateURL with href: " + href + " id: " + id + " page: " + page);
    console.log("page: " + page);
    console.log("location.pathname: " + location.pathname);
    console.log(page === location.pathname);
    // Check if the link is to the current page
    if (page === location.pathname) {
        console.log("page is the same");

        event.preventDefault();
        // Manually set the location.hash property
        location.hash = id;
        console.log("location: " + location);
        // Call the highlightRow function to update the row style
        highlightRow();
    }
}