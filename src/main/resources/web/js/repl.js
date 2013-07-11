var url = "/query";
var query = function() {
//    $('#results').html('<img src="spinner.gif"/>');
    var q = $('#inputbox').val();
    $.getJSON(url, {'q': q}, function(data) {
        console.log("HI!")
        console.log(data)
        $('#results').html(data)
    });
};
var initialize = function() {
    $('#inputbox').keypress(function(e) {
        code = (e.keyCode ? e.keyCode : e.which);
        if (code == 13) {
            e.preventDefault();
            query();
        }
    });
    $('#inputbox').focus();
}
