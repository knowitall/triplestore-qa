var url = "/query";

var query = function() {
    $('#results').html('<img src="spinner.gif"/>');
    var q = $('#inputbox').val();
    $.getJSON(url, {'q': q}, function(data) {
        $('#results').html(writeResults(data))
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

var addExample = function(ex) {
    var $link = $('<a href="#">'+ex.q+'</a>');
    $link.click(function(e) {
        e.preventDefault();
        $('#inputbox').val(ex.q);
        query();
    });
    var $item = $('<tr/>').appendTo($('#examples')).append("<td>" + ex.note + "</td>").append("<td>").append($link).append("</td>");
};

var writeResults = function(data) {
    var $r = $('#results');
    $r.html('');
    $.each(data, function(i, group) {
        $r.append(displayGroup(group));
    });
};

var displayGroup = function(group) {
    var $g = $('<div class="answerGroup"/>');
    $g.append($('<h2>' + group.alternates[0] + '</h2>'));
    $.each(group.uqueries , function(i, uqueryGroup) {
        $g.append(displayUQueryGroup(uqueryGroup));
    });
    return $g;
};

var displayUQueryGroup = function(uqueryGroup) {
    var $group = $('<div class="uqueryGroup"/>');
    $group.append(displayConjQuery(uqueryGroup.uquery));
    $.each(uqueryGroup.equeries, function(i, equeryGroup) {
        $group.append(displayEQueryGroup(equeryGroup));
    });
    return $group;
};

var displayEQueryGroup = function(equeryGroup) {
    var tuples = equeryGroup.tuples;
    var cols = getCols(equeryGroup);
    var attr = equeryGroup.attr;
    var $ts = displayTuples(cols, tuples, attr);
    return $ts;
};

var displayTuples = function(cols, tuples, attr) {
    var $t = $('<table class="tuples"></table>');
    var $h = $('<tr/>').appendTo($t);
    $.each(cols, function(i, col) { 
        $h.append('<th>' + col + '</th>');
    });
    $.each(tuples, function(i, tuple) {
        $t.append(tupleToRow(cols, tuple, attr));
    });
    dedupTable($t);
    return $t;
};

var getCols = function(equeryGroup) {
    var uquery = equeryGroup.equery.uquery;
    var conjs = uquery.conjuncts;
    var arr = new Array();
    $.each(conjs, function(i, conj) {
        var tbl = conj.name;
        arr.push(tbl + ".arg1");
        arr.push(tbl + ".rel");
        arr.push(tbl + ".arg2");
        arr.push(tbl + ".namespace");
    });
    return arr;
};

var tupleToRow = function(cols, tuple, attr) {
    var $row = $('<tr/>')
    $.each(cols, function(i, col) {
        var $cell = $('<td>' + tuple[col] + '</td>');
        if (col == attr) {
            $cell.addClass("highlight");
        }
        $row.append($cell);
    });
    return $row;
};

var displayConjQuery = function(query) {
    var $cq = $('<div class="conjunctiveQuery"/>')
    $.each(query.conjuncts, function(i, conj) { 
        $cq.append(displayConjunct(conj)) 
    });
    return $cq;
};

var displayConjunct = function(c) {
    var v = c.values;
    var s = "(" + v.arg1 + ", " + v.rel + ", " + v.arg2 + ")"
    return $('<div class="conjunct">' + s + '</div>')
};

String.prototype.endsWith = function(suffix) {
    return this.indexOf(suffix, this.length - suffix.length) !== -1;
};

var isTripleAttr = function(a) {
    return a.endsWith(".rel") || a.endsWith(".arg1") || a.endsWith(".arg2")
};

var dedupTable = function($t) {
    var seen = {};
    $t.find('tr').each(function() {
        var txt = $(this).text();
        if (seen[txt])
            $(this).remove();
        else
            seen[txt] = true;
    });
};
