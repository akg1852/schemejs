// a subset of scheme implemented in javascript

var scheme = {};
scheme.cell = function(car, cdr) { this.car = car; this.cdr = cdr; };
scheme.symbol = function(name) { this.name = name; };
scheme.lambda = function(params, body, env) {
    this.params = params; this.body = body; this.env = env; };

scheme.reader = function(data) {
    function buildAST(tokens) {
        var token = tokens.shift();
        if (token === '\'') {
            return new scheme.cell(new scheme.symbol('quote'),
                new scheme.cell(buildAST(tokens), null));
        }
        if (token === undefined) throw 'unexpected end of input expression';
        else if (token === '(' || token === '[') {
            var list = new scheme.cell(null, null), cell = list;
            while (tokens[0] !== ')' && tokens[0] !== ']') {
                if (tokens[0] === '.') {
                    tokens.shift();
                    cell.cdr = buildAST(tokens);
                }
                else cell = cell.cdr = new scheme.cell(buildAST(tokens), null);
            }
            tokens.shift();
            return list.cdr;
        }
        else if (token === ')' || token === ']')
            throw 'unmatched ) encountered in input expression';
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
    data = data.replace(/;[^\r\n]*/g, ''); // remove inline comments
    var tokens = data.match(/(?:\(|\)|\[|\]|'|[^\(\)\[\]'\s]+)/g) || [], exprs = [];
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
        if (typeof expr === 'function') return '<primitive procedure>';
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
    if (typeof proc === 'function') return proc(args);
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

scheme.map = function(func, list) {
    if (list === null) return null;
    return new scheme.cell(func(list.car), scheme.map(func, list.cdr));
};

scheme.fold = function(func, init, list) {
    if (list === null) return init;
    return scheme.fold(func, func(init, list.car), list.cdr);
};

scheme.listToArray = function(list) {
    var array = [];
    while (list instanceof scheme.cell) {
        array.push(list.car);
        list = list.cdr;
    }
    if (list !== null) array.push(list);
    return array;
};

scheme.primitive = function(f) {
    return function(args) {
        return f.apply(null, scheme.listToArray(args));
    };
};

scheme.env = new scheme.cell({
    'apply': function(args) {
        var f = args.car; args = args.cdr;
        var list = new scheme.cell(null, null), cell = list;
        while (args.cdr !== null) {
            cell = cell.cdr = new scheme.cell(args.car, null);
            args = args.cdr;
        }
        cell.cdr = args.car;
        return scheme.apply(f, list.cdr);
    },
    'eval': function(args) { return scheme.eval(args.car, scheme.env); },
    'load': function(args) { scheme.load(args.car); },
    'display': function(args) { console.log(scheme.display(args.car)); },
    'boolean?': function(args) { return typeof args.car === 'boolean'; },
    'number?': function(args) { return typeof args.car === 'number'; },
    'string?': function(args) { return typeof args.car === 'string'; },
    'null?': function(args) { return args.car === null; },
    'pair?': function(args) { return args.car instanceof scheme.cell; },
    'symbol?': function(args) { return args.car instanceof scheme.symbol; },
    'procedure?': function(args) {
        return args.car instanceof scheme.primitive || args.car instanceof scheme.lambda; },
    'not': scheme.primitive(function(b) { return !b; }),
    'string-append': scheme.plus,
    'equal?': function(args) {
        return scheme.display(args.car) === scheme.display(args.cdr.car); },
    'cons': function(args) { return new scheme.cell(args.car, args.cdr.car); },
    'car': function(args) { return args.car.car; },
    'cdr': function(args) { return args.car.cdr; },
    '+': function(args) { return scheme.fold(function(a, b) { return a + b; }, 0, args); },
    '-': function(args) { return args.cdr === null ? -args.car :
        scheme.fold(function(a, b) { return a - b; }, args.car, args.cdr); },
    '*': function(args) { return scheme.fold(function(a, b) { return a * b; }, 1, args); },
    '/': function(args) { return args.cdr === null? 1 / args.car :
        scheme.fold(function(a, b) { return a / b; }, args.car, args.cdr); },
    '>': function(args) { return false !== scheme.fold(function(a, b) {
        return a === false ? false : a > b ? b : false; }, Infinity, args); },
    '<': function(args) { return false !== scheme.fold(function(a, b) {
        return a === false ? false : a < b ? b : false; }, -Infinity, args); },
    '>=': function(args) { return false !== scheme.fold(function(a, b) {
        return a === false ? false : a >= b ? b : false; }, Infinity, args); },
    '<=': function(args) { return false !== scheme.fold(function(a, b) {
        return a === false ? false : a <= b ? b : false; }, -Infinity, args); },
    'expt': scheme.primitive(Math.pow),
    'log': scheme.primitive(Math.log), 'exp': scheme.primitive(Math.exp),
    'floor': scheme.primitive(Math.floor), 'ceiling': scheme.primitive(Math.ceil),
    'random': scheme.primitive(function(n) { return Math.floor(Math.random() * n); }),
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

