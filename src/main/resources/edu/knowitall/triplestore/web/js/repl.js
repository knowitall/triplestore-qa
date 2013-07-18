var url = "/query";
var maxRows = 50;

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
    var $a = $('<div class="answer"/>').appendTo($g);
    $a.append($('<h1>' + group.alternates[0] + '</h1>'));
    $a.append($('<div class="score">Score = ' + group.score + '</div>'));
    $.each(group.uqueries , function(i, uqueryGroup) {
        $.each(uqueryGroup.equeries, function(j, equeryGroup) {
            $g.append(displayEQueryGroup(equeryGroup));
        });
    });
    return $g;
};

var displayEQueryGroup = function(equeryGroup) {
    var $group = $('<div class="equeryGroup"/>');
    var tuples = equeryGroup.tuples;
    var cols = getCols(equeryGroup);
    var colNames = getColNames(equeryGroup);
    var attr = equeryGroup.attr;
    var $ts = displayTuples(cols, colNames, tuples, attr);
    $ts.appendTo($group);
    return $group;
};

var displayTuples = function(cols, colNames, tuples, attr) {
    var $tc = $('<div class="tuplesContainer"/>');
    var $t = $('<table class="tuples"></table>').appendTo($tc);
    var $h = $('<tr/>').appendTo($t);
    $.each(colNames, function(i, col) { 
        $h.append('<th>' + col + '</th>');
    });
    $.each(tuples, function(i, tuple) {
        if (i >= maxRows) { return; }
        $t.append(tupleToRow(cols, tuple, attr));
    });
    dedupTable($t);
    $t.find('tr:even').addClass('even');
    $t.find('tr:odd').addClass('odd');
    return $tc;
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
var getColNames = function(equeryGroup) {
    var conjs = equeryGroup.equery.query.conjuncts;
    var arr = new Array();
    $.each(conjs, function(i, conj) {
        var tbl = conj.name;
        arr.push(conj.values.arg1);
        arr.push(conj.values.rel);
        arr.push(conj.values.arg2);
        arr.push('namespace');
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
        if (i % 4 == 3 && i < cols.length - 1 && i > 0) {
            $cell.addClass("endTuple");
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
