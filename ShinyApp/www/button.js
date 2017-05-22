$(document).keyup(function(event) {
    if ($("#mainQueryField input:first-of-type").is(":focus") && (event.keyCode == 13)) {
        $("#mainQueryField button:first-of-type").click();
    }
});