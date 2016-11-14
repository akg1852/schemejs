// a subset of scheme implemented in javascript

var scheme = {};
scheme.cell = function(car, cdr) { this.car = car; this.cdr = cdr; };

scheme.reader = function(data) {
    function buildAST(tokens) {
        var token = tokens.shift();
        if (token === undefined) throw 'unexpected end of input expression';
        else if (token === '(') {
            var list = new scheme.cell(null, null), cell = list;
            while (tokens[0] !== ')') cell = cell.cdr = new scheme.cell(buildAST(tokens), null);
            tokens.shift();
            return list.cdr;
        }
        else if (token === ')') throw 'unmatched ) encountered in input expression';
        else {
            var num = Number(token);
            return isNaN(num) ? token : num;
        }
    };

    var tokens = data.match(/(?:\(|\)|[^\(\)\s]+)/g) || [];
    try { return buildAST(tokens); }
    catch (e) { console.error(e); }
};

scheme.display = function(expr) {
    if (expr instanceof scheme.cell) {
        var listDisplay = '(' + scheme.display(expr.car);
        while (expr.cdr instanceof scheme.cell) {
            expr = expr.cdr
            listDisplay += ' ' + scheme.display(expr.car);
        }
        if (expr.cdr !== null) listDisplay += ' . ' + scheme.display(expr.cdr);
        return listDisplay + ')';
    }
    else return expr;
}

scheme.eval = function(expr, env) {
    return expr;
};

scheme.env = {};

// repl
process.stdin.setEncoding('utf8');
process.stdin.on('data', function (data) {
    var expr = scheme.reader(data);
    if (expr !== undefined) console.log(scheme.display(scheme.eval(expr, scheme.env)));
});
