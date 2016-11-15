// a subset of scheme implemented in javascript

var scheme = {};
scheme.cell = function(car, cdr) { this.car = car; this.cdr = cdr; };
scheme.symbol = function(name) { this.name = name; };
scheme.primitive = function(apply) { this.apply = apply; };
scheme.lambda = function(params, body, env) {
    this.params = params; this.body = body; this.env = env; };

scheme.map = function(func, list) {
    if (list === null) return null;
    return new scheme.cell(func(list.car), scheme.map(func, list.cdr));
};

scheme.plus = function (args) {
    return args.cdr === null ? args.car : args.car + scheme.plus(args.cdr);
}

scheme.reader = function(data) {
    function buildAST(tokens) {
        var token = tokens.shift();
        if (token === '\'') {
            return new scheme.cell(new scheme.symbol('quote'),
                new scheme.cell(buildAST(tokens), null));
        }
        if (token === undefined) throw 'unexpected end of input expression';
        else if (token === '(') {
            var list = new scheme.cell(null, null), cell = list;
            while (tokens[0] !== ')') {
                if (tokens[0] === '.') {
                    tokens.shift();
                    cell.cdr = buildAST(tokens);
                }
                else cell = cell.cdr = new scheme.cell(buildAST(tokens), null);
            }
            tokens.shift();
            return list.cdr;
        }
        else if (token === ')') throw 'unmatched ) encountered in input expression';
        else {
            var atom;
            if (token === '#t') return true;
            if (token === '#f') return false;
            if (atom = Number(token), !isNaN(atom)) return atom; // number
            if ((atom = token.match(/^"([^"]*)"$/)) !== null) return atom[1]; // string
            if ((atom = token.match(/^[\w!%&:<>=@_~$^.*?/+-]+$/)) !== null)
                return new scheme.symbol(atom[0]); // symbol
            throw 'invalid token: ' + token;
        }
    };
    var tokens = data.match(/(?:\(|\)|'|[^\(\)'\s]+)/g) || [], exprs = [];
    while (tokens.length) exprs.push(buildAST(tokens));
    return exprs;
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
    else {
        if (expr === null) return '()';
        if (expr === true) return '#t';
        if (expr === false) return '#f';
        if (typeof expr === 'string') return '"' + expr + '"';
        if (expr instanceof scheme.symbol) return expr.name;
        if (expr instanceof scheme.primitive) return '<primitive procedure>';
        if (expr instanceof scheme.lambda)
            return scheme.display(new scheme.cell(new scheme.symbol('lambda'),
                new scheme.cell(expr.params, expr.body)));
        return expr;
    }
}

scheme.eval = function(expr, env) {
    if (expr instanceof scheme.cell) {
        if (expr.car instanceof scheme.symbol) {
            var symbol = expr.car.name;

            if (symbol === 'quote') return expr.cdr.car;
            if (symbol === 'lambda') {
                var params = expr.cdr.car, body = expr.cdr.cdr;
                return new scheme.lambda(params, body, env);
            }
            if (symbol === 'if') {
                if (scheme.eval(expr.cdr.car, env))
                    return scheme.eval(expr.cdr.cdr.car, env);
                if (expr.cdr.cdr.cdr !== null)
                    return scheme.eval(expr.cdr.cdr.cdr.car, env);
                return undefined;
            }
            if (symbol === 'define') {
                env.car[expr.cdr.car.name] = scheme.eval(expr.cdr.cdr.car, env);
                return undefined;
            }
        }
        return scheme.apply(scheme.eval(expr.car, env),
            scheme.map(function(arg) { return scheme.eval(arg, env); }, expr.cdr));
    }
    if (expr instanceof scheme.symbol) {
        while (env instanceof scheme.cell) {
            var value = env.car[expr.name];
            if (value !== undefined) return value;
            env = env.cdr;
        }
        throw 'symbol not assigned a value: ' + expr.name;
    }
    return expr;
};

scheme.apply = function(proc, args) {
    if (proc instanceof scheme.primitive) {
        return proc.apply(args);
    }
    if (proc instanceof scheme.lambda) {
        var params = proc.params, env = {};
        while (params !== null) {
            if (params instanceof scheme.symbol) env[params.name] = args;
            if (!(params instanceof scheme.cell && args instanceof scheme.cell)) break;
            env[params.car.name] = args.car;
            params = params.cdr; args = args.cdr;
        }
        var body = proc.body, value = undefined;
        while (body instanceof scheme.cell) {
            value = scheme.eval(body.car, new scheme.cell(env, proc.env));
            body = body.cdr;
        }
        return value;
    }
    throw 'not a procedure: ' + scheme.display(proc);
}

scheme.env = new scheme.cell({
    'load': new scheme.primitive(function(args) { scheme.load(args.car); }),
    'display': new scheme.primitive(function(args) {
        console.log(scheme.display(args.car)); }),
    'boolean?': new scheme.primitive(function(args) { return typeof args.car === 'boolean'; }),
    'number?': new scheme.primitive(function(args) { return typeof args.car === 'number'; }),
    'string?': new scheme.primitive(function(args) { return typeof args.car === 'string'; }),
    'null?': new scheme.primitive(function(args) { return args.car === null; }),
    'pair?': new scheme.primitive(function(args) { return args.car instanceof scheme.cell; }),
    'symbol?': new scheme.primitive(function(args) { return args.car instanceof scheme.symbol; }),
    'procedure?': new scheme.primitive(function(args) {
        return args.car instanceof scheme.primitive || args.car instanceof scheme.lambda; }),
    'cons': new scheme.primitive(function(args) {
        return new scheme.cell(args.car, args.cdr.car); }),
    'car': new scheme.primitive(function(args) { return args.car.car; }),
    'cdr': new scheme.primitive(function(args) { return args.car.cdr; }),
    '+': new scheme.primitive(scheme.plus),
    '-': new scheme.primitive(function(args) {
        return args.cdr === null ? -args.car : args.car - scheme.plus(args.cdr); }),
    'not': new scheme.primitive(function(args) { return !args.car; }),
    'string-append': new scheme.primitive(scheme.plus),
    'equal?': new scheme.primitive(function(args) {
        return scheme.display(args.car) === scheme.display(args.cdr.car); }),
}, null);

scheme.load = function(file, callback) {
    require('fs').readFile(file,'utf-8', function(err, data) {
        if (err) console.error('error loading file: ' + file);
        else {
            scheme.reader(data).forEach(function(expr) {
                scheme.eval(expr, scheme.env);
            });
        }
        if (callback) callback();
    });
};

// repl
scheme.load('lib.scm', function () {
    process.stdin.setEncoding('utf8');
    process.stdin.on('data', function (data) {
        try {
            scheme.reader(data).forEach(function(expr) {
                var result = scheme.eval(expr, scheme.env);
                var print = scheme.display(result);
                if (print !== undefined) console.log(print);
            });
        }
        catch (e) { console.error(e); }
    });
});

