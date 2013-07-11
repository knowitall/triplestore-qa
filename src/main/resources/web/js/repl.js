var url = "/query";
var query = function() {
    $('#results').html('<img src="spinner.gif"/>');
    var q = $('#inputbox').val();
    $.get(url, {'q': q}, function(data) {
        console.log(data)
        $('#results').html(data)
    })
    .fail(function() { $('#results').html('Error.') });
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
