$(document).keyup(function(event) {
    if ($("#queryMain-searchterm").is(":focus") && (event.keyCode == 13)) {
        $("#queryMain-submitButton").click();
    }
});