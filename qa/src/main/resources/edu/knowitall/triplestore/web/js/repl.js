var url = "/query";
var maxRows = 50;

var query = function() {
    $('#results').html('<img src="spinner.gif"/>');
    var q = $('#inputbox').val();
    var opts = $('#config select').serializeObject();
    opts.q = q;
    $.getJSON(url, opts, function(data) {
        console.log(data);
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
    initConfig();
    $('#inputbox').focus();
}

var initConfig = function() {
    var $conf = $('#config');
    $conf.html('<img src="spinner.gif"/>');
    var $t = $('<table></table>');
    $.getJSON('/listComponents', function(data) {
        $conf.html($t);
        var defaults = data.defaults;
        var components = data.components;
        $.each(components, function(k, val) {
            var $r = $('<tr/>').appendTo($t);
            $r.append('<td style="text-align: right">' + val.name + ':</td>');
            var $td = $('<td/>').appendTo($r);
            var $sel = $('<select name="' + k + '"></select>').appendTo($td);
            $.each(val.options, function(i, v) {
                var $o = $('<option value="'+v+'">'+v+'</option>');
                $o.appendTo($sel);
            });
            $sel.val(defaults[k]);
            $sel.appendTo($r);
        });
    });
};

var addExample = function(ex) {
    var $link = $('<a href="#">'+ex.q+'</a>');
    $link.click(function(e) {
        e.preventDefault();
        $('#inputbox').val(ex.q);
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
    $a.append($('<h1>' + group.answer + '</h1>'));
    $a.append($('<div class="score">Score = ' + group.score + '</div>'));
    $a.append($('<div class="score">' + group.derivations.length + ' derivation(s)</div>'));
    var $derivs = $('<div class="derivationGroup"/>').appendTo($g);
    $.each(group.derivations, function(i, derivation) {
        var $deriv = $('<div class="derivation"/>').appendTo($derivs);
        $deriv.append($('<h3>Derivation ' + (i+1) + '</h3>'));
        $deriv.append(displayDerivInfo(derivation));
        $deriv.append(displayDerivationTables(derivation.tables));
    });
    return $g;
};

var displayDerivInfo = function(derivation) {
    var $table = $('<table class="derivInfo"/>');
    var $tr2 = $('<tr/>').appendTo($table);
    $tr2.append('<td><b>Paraphrases:</b></td>');
    $tr2.append($('<td/>').append(displayParaphrases(derivation.paraphrases)));
    var $tr3 = $('<tr/>').appendTo($table);
    $tr3.append('<td><b>Parsed Queries:</b></td>');
    $tr3.append($('<td/>').append(displayParserQueries(derivation.parserQueries)));
    var $tr1 = $('<tr/>').appendTo($table);
    $tr1.append('<td><b>Executed Query:</b></td>');
    $tr1.append($('<td/>').append(displayConjQuery(derivation.executedQuery)));
    return $table;
};

var displayParserQueries = function(queries) {
    var $queries = $('<span/>');
    $.each(queries, function(i, query) {
        var $q = displayConjQuery(query).append('<br/>');
        $q.appendTo($queries);
    });
    return $queries;
};

var displayDerivationTables = function(tables) {
    var $tables = $('<div class="derivationTables"/>');
    $.each(tables, function(i, table) {
        $tables.append(displayDerivationTable(table));
    });
    return $tables;
};

var displayDerivationTable = function(table) {
    var cols = getCols(table);
    var colNames = getColNames(table);
    var $ts = displayTuples(cols, colNames, table.tuples);
    return $ts;
};

var displayParaphrases = function(paraphrases) {
    return paraphrases.join("<br/>");
}

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

var displayTuples = function(cols, colNames, tuples) {
    var $tc = $('<div class="tuplesContainer"/>');
    var $t = $('<table class="tuples"></table>').appendTo($tc);
    var $h = $('<tr/>').appendTo($t);
    $.each(colNames, function(i, col) { 
        $h.append('<th>' + col + '</th>');
    });
    $.each(tuples, function(i, tuple) {
        if (i >= maxRows) { return; }
        $t.append(tupleToRow(cols, tuple));
    });
    dedupTable($t);
    $t.find('tr:even').addClass('even');
    $t.find('tr:odd').addClass('odd');
    return $tc;
};

var getCols = function(table) {
    var conj = table.conjunct;
    var arr = new Array();
    var tbl = conj.name;
    arr.push(tbl + ".arg1");
    arr.push(tbl + ".rel");
    arr.push(tbl + ".arg2");
    arr.push(tbl + ".namespace");
    return arr;
};
var getColNames = function(table) {
    var conj = table.conjunct;
    var tbl = conj.name;
    var arr = new Array();
    arr.push(conj.values.arg1);
    arr.push(conj.values.rel);
    arr.push(conj.values.arg2);
    arr.push('namespace');
    return arr;
};

var tupleToRow = function(cols, tuple) {
    var $row = $('<tr/>')
    $.each(cols, function(i, col) {
        var $cell = $('<td>' + tuple.attrs[col] + '</td>');
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

$.fn.serializeObject = function()
{
    var o = {};
    var a = this.serializeArray();
    $.each(a, function() {
        if (o[this.name] !== undefined) {
            if (!o[this.name].push) {
                o[this.name] = [o[this.name]];
            }
            o[this.name].push(this.value || '');
        } else {
            o[this.name] = this.value || '';
        }
    });
    return o;
};
