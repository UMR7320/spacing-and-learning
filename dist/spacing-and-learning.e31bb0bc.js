// modules are defined as an array
// [ module function, map of requires ]
//
// map of requires is short require name -> numeric require
//
// anything defined in a previous bundle is accessed via the
// orig method which is the require for previous bundles
parcelRequire = (function (modules, cache, entry, globalName) {
  // Save the require from previous bundle to this closure if any
  var previousRequire = typeof parcelRequire === 'function' && parcelRequire;
  var nodeRequire = typeof require === 'function' && require;

  function newRequire(name, jumped) {
    if (!cache[name]) {
      if (!modules[name]) {
        // if we cannot find the module within our internal map or
        // cache jump to the current global require ie. the last bundle
        // that was added to the page.
        var currentRequire = typeof parcelRequire === 'function' && parcelRequire;
        if (!jumped && currentRequire) {
          return currentRequire(name, true);
        }

        // If there are other bundles on this page the require from the
        // previous one is saved to 'previousRequire'. Repeat this as
        // many times as there are bundles until the module is found or
        // we exhaust the require chain.
        if (previousRequire) {
          return previousRequire(name, true);
        }

        // Try the node require function if it exists.
        if (nodeRequire && typeof name === 'string') {
          return nodeRequire(name);
        }

        var err = new Error('Cannot find module \'' + name + '\'');
        err.code = 'MODULE_NOT_FOUND';
        throw err;
      }

      localRequire.resolve = resolve;
      localRequire.cache = {};

      var module = cache[name] = new newRequire.Module(name);

      modules[name][0].call(module.exports, localRequire, module, module.exports, this);
    }

    return cache[name].exports;

    function localRequire(x){
      return newRequire(localRequire.resolve(x));
    }

    function resolve(x){
      return modules[name][1][x] || x;
    }
  }

  function Module(moduleName) {
    this.id = moduleName;
    this.bundle = newRequire;
    this.exports = {};
  }

  newRequire.isParcelRequire = true;
  newRequire.Module = Module;
  newRequire.modules = modules;
  newRequire.cache = cache;
  newRequire.parent = previousRequire;
  newRequire.register = function (id, exports) {
    modules[id] = [function (require, module) {
      module.exports = exports;
    }, {}];
  };

  var error;
  for (var i = 0; i < entry.length; i++) {
    try {
      newRequire(entry[i]);
    } catch (e) {
      // Save first error but execute all entries
      if (!error) {
        error = e;
      }
    }
  }

  if (entry.length) {
    // Expose entry point to Node, AMD or browser globals
    // Based on https://github.com/ForbesLindesay/umd/blob/master/template.js
    var mainExports = newRequire(entry[entry.length - 1]);

    // CommonJS
    if (typeof exports === "object" && typeof module !== "undefined") {
      module.exports = mainExports;

    // RequireJS
    } else if (typeof define === "function" && define.amd) {
     define(function () {
       return mainExports;
     });

    // <script>
    } else if (globalName) {
      this[globalName] = mainExports;
    }
  }

  // Override the current require with this new one
  parcelRequire = newRequire;

  if (error) {
    // throw error from earlier, _after updating parcelRequire_
    throw error;
  }

  return newRequire;
})({"src/Main.elm":[function(require,module,exports) {
(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEBUG mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// HELPERS


function _Debugger_unsafeCoerce(value)
{
	return value;
}



// PROGRAMS


var _Debugger_element = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init),
		$elm$browser$Debugger$Main$wrapUpdate(impl.update),
		$elm$browser$Debugger$Main$wrapSubs(impl.subscriptions),
		function(sendToApp, initialModel)
		{
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			var currNode = _VirtualDom_virtualize(domNode);
			var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
			var currPopout;

			var cornerNode = _VirtualDom_doc.createElement('div');
			domNode.parentNode.insertBefore(cornerNode, domNode.nextSibling);
			var cornerCurr = _VirtualDom_virtualize(cornerNode);

			initialModel.popout.a = sendToApp;

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = A2(_VirtualDom_map, $elm$browser$Debugger$Main$UserMsg, view($elm$browser$Debugger$Main$getUserModel(model)));
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;

				// update blocker

				var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
				_Debugger_updateBlocker(currBlocker, nextBlocker);
				currBlocker = nextBlocker;

				// view corner

				var cornerNext = $elm$browser$Debugger$Main$cornerView(model);
				var cornerPatches = _VirtualDom_diff(cornerCurr, cornerNext);
				cornerNode = _VirtualDom_applyPatches(cornerNode, cornerCurr, cornerPatches, sendToApp);
				cornerCurr = cornerNext;

				if (!model.popout.b)
				{
					currPopout = undefined;
					return;
				}

				// view popout

				_VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
				currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
				var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
				var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
				_VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
				currPopout = nextPopout;
				_VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
			});
		}
	);
});


var _Debugger_document = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		A3($elm$browser$Debugger$Main$wrapInit, _Json_wrap(debugMetadata), _Debugger_popout(), impl.init),
		$elm$browser$Debugger$Main$wrapUpdate(impl.update),
		$elm$browser$Debugger$Main$wrapSubs(impl.subscriptions),
		function(sendToApp, initialModel)
		{
			var divertHrefToApp = impl.setup && impl.setup(function(x) { return sendToApp($elm$browser$Debugger$Main$UserMsg(x)); });
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			var currBlocker = $elm$browser$Debugger$Main$toBlockerType(initialModel);
			var currPopout;

			initialModel.popout.a = sendToApp;

			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view($elm$browser$Debugger$Main$getUserModel(model));
				var nextNode = _VirtualDom_node('body')(_List_Nil)(
					_Utils_ap(
						A2($elm$core$List$map, _VirtualDom_map($elm$browser$Debugger$Main$UserMsg), doc.body),
						_List_Cons($elm$browser$Debugger$Main$cornerView(model), _List_Nil)
					)
				);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);

				// update blocker

				var nextBlocker = $elm$browser$Debugger$Main$toBlockerType(model);
				_Debugger_updateBlocker(currBlocker, nextBlocker);
				currBlocker = nextBlocker;

				// view popout

				if (!model.popout.b) { currPopout = undefined; return; }

				_VirtualDom_doc = model.popout.b; // SWITCH TO POPOUT DOC
				currPopout || (currPopout = _VirtualDom_virtualize(model.popout.b));
				var nextPopout = $elm$browser$Debugger$Main$popoutView(model);
				var popoutPatches = _VirtualDom_diff(currPopout, nextPopout);
				_VirtualDom_applyPatches(model.popout.b.body, currPopout, popoutPatches, sendToApp);
				currPopout = nextPopout;
				_VirtualDom_doc = document; // SWITCH BACK TO NORMAL DOC
			});
		}
	);
});


function _Debugger_popout()
{
	return {
		b: undefined,
		a: undefined
	};
}

function _Debugger_isOpen(popout)
{
	return !!popout.b;
}

function _Debugger_open(popout)
{
	return _Scheduler_binding(function(callback)
	{
		_Debugger_openWindow(popout);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}

function _Debugger_openWindow(popout)
{
	var w = $elm$browser$Debugger$Main$initialWindowWidth,
		h = $elm$browser$Debugger$Main$initialWindowHeight,
	 	x = screen.width - w,
		y = screen.height - h;

	var debuggerWindow = window.open('', '', 'width=' + w + ',height=' + h + ',left=' + x + ',top=' + y);
	var doc = debuggerWindow.document;
	doc.title = 'Elm Debugger';

	// handle arrow keys
	doc.addEventListener('keydown', function(event) {
		event.metaKey && event.which === 82 && window.location.reload();
		event.key === 'ArrowUp'   && (popout.a($elm$browser$Debugger$Main$Up  ), event.preventDefault());
		event.key === 'ArrowDown' && (popout.a($elm$browser$Debugger$Main$Down), event.preventDefault());
	});

	// handle window close
	window.addEventListener('unload', close);
	debuggerWindow.addEventListener('unload', function() {
		popout.b = undefined;
		popout.a($elm$browser$Debugger$Main$NoOp);
		window.removeEventListener('unload', close);
	});

	function close() {
		popout.b = undefined;
		popout.a($elm$browser$Debugger$Main$NoOp);
		debuggerWindow.close();
	}

	// register new window
	popout.b = doc;
}



// SCROLL


function _Debugger_scroll(popout)
{
	return _Scheduler_binding(function(callback)
	{
		if (popout.b)
		{
			var msgs = popout.b.getElementById('elm-debugger-sidebar');
			if (msgs && msgs.scrollTop !== 0)
			{
				msgs.scrollTop = 0;
			}
		}
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


var _Debugger_scrollTo = F2(function(id, popout)
{
	return _Scheduler_binding(function(callback)
	{
		if (popout.b)
		{
			var msg = popout.b.getElementById(id);
			if (msg)
			{
				msg.scrollIntoView(false);
			}
		}
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});



// UPLOAD


function _Debugger_upload(popout)
{
	return _Scheduler_binding(function(callback)
	{
		var doc = popout.b || document;
		var element = doc.createElement('input');
		element.setAttribute('type', 'file');
		element.setAttribute('accept', 'text/json');
		element.style.display = 'none';
		element.addEventListener('change', function(event)
		{
			var fileReader = new FileReader();
			fileReader.onload = function(e)
			{
				callback(_Scheduler_succeed(e.target.result));
			};
			fileReader.readAsText(event.target.files[0]);
			doc.body.removeChild(element);
		});
		doc.body.appendChild(element);
		element.click();
	});
}



// DOWNLOAD


var _Debugger_download = F2(function(historyLength, json)
{
	return _Scheduler_binding(function(callback)
	{
		var fileName = 'history-' + historyLength + '.txt';
		var jsonString = JSON.stringify(json);
		var mime = 'text/plain;charset=utf-8';
		var done = _Scheduler_succeed(_Utils_Tuple0);

		// for IE10+
		if (navigator.msSaveBlob)
		{
			navigator.msSaveBlob(new Blob([jsonString], {type: mime}), fileName);
			return callback(done);
		}

		// for HTML5
		var element = document.createElement('a');
		element.setAttribute('href', 'data:' + mime + ',' + encodeURIComponent(jsonString));
		element.setAttribute('download', fileName);
		element.style.display = 'none';
		document.body.appendChild(element);
		element.click();
		document.body.removeChild(element);
		callback(done);
	});
});



// POPOUT CONTENT


function _Debugger_messageToString(value)
{
	if (typeof value === 'boolean')
	{
		return value ? 'True' : 'False';
	}

	if (typeof value === 'number')
	{
		return value + '';
	}

	if (typeof value === 'string')
	{
		return '"' + _Debugger_addSlashes(value, false) + '"';
	}

	if (value instanceof String)
	{
		return "'" + _Debugger_addSlashes(value, true) + "'";
	}

	if (typeof value !== 'object' || value === null || !('$' in value))
	{
		return '…';
	}

	if (typeof value.$ === 'number')
	{
		return '…';
	}

	var code = value.$.charCodeAt(0);
	if (code === 0x23 /* # */ || /* a */ 0x61 <= code && code <= 0x7A /* z */)
	{
		return '…';
	}

	if (['Array_elm_builtin', 'Set_elm_builtin', 'RBNode_elm_builtin', 'RBEmpty_elm_builtin'].indexOf(value.$) >= 0)
	{
		return '…';
	}

	var keys = Object.keys(value);
	switch (keys.length)
	{
		case 1:
			return value.$;
		case 2:
			return value.$ + ' ' + _Debugger_messageToString(value.a);
		default:
			return value.$ + ' … ' + _Debugger_messageToString(value[keys[keys.length - 1]]);
	}
}


function _Debugger_init(value)
{
	if (typeof value === 'boolean')
	{
		return A3($elm$browser$Debugger$Expando$Constructor, $elm$core$Maybe$Just(value ? 'True' : 'False'), true, _List_Nil);
	}

	if (typeof value === 'number')
	{
		return $elm$browser$Debugger$Expando$Primitive(value + '');
	}

	if (typeof value === 'string')
	{
		return $elm$browser$Debugger$Expando$S('"' + _Debugger_addSlashes(value, false) + '"');
	}

	if (value instanceof String)
	{
		return $elm$browser$Debugger$Expando$S("'" + _Debugger_addSlashes(value, true) + "'");
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (tag === '::' || tag === '[]')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ListSeq, true,
				A2($elm$core$List$map, _Debugger_init, value)
			);
		}

		if (tag === 'Set_elm_builtin')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$SetSeq, true,
				A3($elm$core$Set$foldr, _Debugger_initCons, _List_Nil, value)
			);
		}

		if (tag === 'RBNode_elm_builtin' || tag == 'RBEmpty_elm_builtin')
		{
			return A2($elm$browser$Debugger$Expando$Dictionary, true,
				A3($elm$core$Dict$foldr, _Debugger_initKeyValueCons, _List_Nil, value)
			);
		}

		if (tag === 'Array_elm_builtin')
		{
			return A3($elm$browser$Debugger$Expando$Sequence, $elm$browser$Debugger$Expando$ArraySeq, true,
				A3($elm$core$Array$foldr, _Debugger_initCons, _List_Nil, value)
			);
		}

		if (typeof tag === 'number')
		{
			return $elm$browser$Debugger$Expando$Primitive('<internals>');
		}

		var char = tag.charCodeAt(0);
		if (char === 35 || 65 <= char && char <= 90)
		{
			var list = _List_Nil;
			for (var i in value)
			{
				if (i === '$') continue;
				list = _List_Cons(_Debugger_init(value[i]), list);
			}
			return A3($elm$browser$Debugger$Expando$Constructor, char === 35 ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(tag), true, $elm$core$List$reverse(list));
		}

		return $elm$browser$Debugger$Expando$Primitive('<internals>');
	}

	if (typeof value === 'object')
	{
		var dict = $elm$core$Dict$empty;
		for (var i in value)
		{
			dict = A3($elm$core$Dict$insert, i, _Debugger_init(value[i]), dict);
		}
		return A2($elm$browser$Debugger$Expando$Record, true, dict);
	}

	return $elm$browser$Debugger$Expando$Primitive('<internals>');
}

var _Debugger_initCons = F2(function initConsHelp(value, list)
{
	return _List_Cons(_Debugger_init(value), list);
});

var _Debugger_initKeyValueCons = F3(function(key, value, list)
{
	return _List_Cons(
		_Utils_Tuple2(_Debugger_init(key), _Debugger_init(value)),
		list
	);
});

function _Debugger_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');
	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}



// BLOCK EVENTS


function _Debugger_updateBlocker(oldBlocker, newBlocker)
{
	if (oldBlocker === newBlocker) return;

	var oldEvents = _Debugger_blockerToEvents(oldBlocker);
	var newEvents = _Debugger_blockerToEvents(newBlocker);

	// remove old blockers
	for (var i = 0; i < oldEvents.length; i++)
	{
		document.removeEventListener(oldEvents[i], _Debugger_blocker, true);
	}

	// add new blockers
	for (var i = 0; i < newEvents.length; i++)
	{
		document.addEventListener(newEvents[i], _Debugger_blocker, true);
	}
}


function _Debugger_blocker(event)
{
	if (event.type === 'keydown' && event.metaKey && event.which === 82)
	{
		return;
	}

	var isScroll = event.type === 'scroll' || event.type === 'wheel';
	for (var node = event.target; node; node = node.parentNode)
	{
		if (isScroll ? node.id === 'elm-debugger-details' : node.id === 'elm-debugger-overlay')
		{
			return;
		}
	}

	event.stopPropagation();
	event.preventDefault();
}

function _Debugger_blockerToEvents(blocker)
{
	return blocker === $elm$browser$Debugger$Overlay$BlockNone
		? []
		: blocker === $elm$browser$Debugger$Overlay$BlockMost
			? _Debugger_mostEvents
			: _Debugger_allEvents;
}

var _Debugger_mostEvents = [
	'click', 'dblclick', 'mousemove',
	'mouseup', 'mousedown', 'mouseenter', 'mouseleave',
	'touchstart', 'touchend', 'touchcancel', 'touchmove',
	'pointerdown', 'pointerup', 'pointerover', 'pointerout',
	'pointerenter', 'pointerleave', 'pointermove', 'pointercancel',
	'dragstart', 'drag', 'dragend', 'dragenter', 'dragover', 'dragleave', 'drop',
	'keyup', 'keydown', 'keypress',
	'input', 'change',
	'focus', 'blur'
];

var _Debugger_allEvents = _Debugger_mostEvents.concat('wheel', 'scroll');




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };
key['elm-hot-nav-key'] = true

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}




// VIRTUAL-DOM WIDGETS


var _Markdown_toHtml = F3(function(options, factList, rawMarkdown)
{
	return _VirtualDom_custom(
		factList,
		{
			a: options,
			b: rawMarkdown
		},
		_Markdown_render,
		_Markdown_diff
	);
});



// WIDGET IMPLEMENTATION


function _Markdown_render(model)
{
	return A2(_Markdown_replace, model, _VirtualDom_doc.createElement('div'));
}


function _Markdown_diff(x, y)
{
	return x.b === y.b && x.a === y.a
		? false
		: _Markdown_replace(y);
}


var _Markdown_replace = F2(function(model, div)
{
	div.innerHTML = _Markdown_marked(model.b, _Markdown_formatOptions(model.a));
	return div;
});



// ACTUAL MARKDOWN PARSER


var _Markdown_marked = function() {
	// catch the `marked` object regardless of the outer environment.
	// (ex. a CommonJS module compatible environment.)
	// note that this depends on marked's implementation of environment detection.
	var module = {};
	var exports = module.exports = {};

	/**
	 * marked - a markdown parser
	 * Copyright (c) 2011-2014, Christopher Jeffrey. (MIT Licensed)
	 * https://github.com/chjj/marked
	 * commit cd2f6f5b7091154c5526e79b5f3bfb4d15995a51
	 */
	(function(){var block={newline:/^\n+/,code:/^( {4}[^\n]+\n*)+/,fences:noop,hr:/^( *[-*_]){3,} *(?:\n+|$)/,heading:/^ *(#{1,6}) *([^\n]+?) *#* *(?:\n+|$)/,nptable:noop,lheading:/^([^\n]+)\n *(=|-){2,} *(?:\n+|$)/,blockquote:/^( *>[^\n]+(\n(?!def)[^\n]+)*\n*)+/,list:/^( *)(bull) [\s\S]+?(?:hr|def|\n{2,}(?! )(?!\1bull )\n*|\s*$)/,html:/^ *(?:comment *(?:\n|\s*$)|closed *(?:\n{2,}|\s*$)|closing *(?:\n{2,}|\s*$))/,def:/^ *\[([^\]]+)\]: *<?([^\s>]+)>?(?: +["(]([^\n]+)[")])? *(?:\n+|$)/,table:noop,paragraph:/^((?:[^\n]+\n?(?!hr|heading|lheading|blockquote|tag|def))+)\n*/,text:/^[^\n]+/};block.bullet=/(?:[*+-]|\d+\.)/;block.item=/^( *)(bull) [^\n]*(?:\n(?!\1bull )[^\n]*)*/;block.item=replace(block.item,"gm")(/bull/g,block.bullet)();block.list=replace(block.list)(/bull/g,block.bullet)("hr","\\n+(?=\\1?(?:[-*_] *){3,}(?:\\n+|$))")("def","\\n+(?="+block.def.source+")")();block.blockquote=replace(block.blockquote)("def",block.def)();block._tag="(?!(?:"+"a|em|strong|small|s|cite|q|dfn|abbr|data|time|code"+"|var|samp|kbd|sub|sup|i|b|u|mark|ruby|rt|rp|bdi|bdo"+"|span|br|wbr|ins|del|img)\\b)\\w+(?!:/|[^\\w\\s@]*@)\\b";block.html=replace(block.html)("comment",/<!--[\s\S]*?-->/)("closed",/<(tag)[\s\S]+?<\/\1>/)("closing",/<tag(?:"[^"]*"|'[^']*'|[^'">])*?>/)(/tag/g,block._tag)();block.paragraph=replace(block.paragraph)("hr",block.hr)("heading",block.heading)("lheading",block.lheading)("blockquote",block.blockquote)("tag","<"+block._tag)("def",block.def)();block.normal=merge({},block);block.gfm=merge({},block.normal,{fences:/^ *(`{3,}|~{3,})[ \.]*(\S+)? *\n([\s\S]*?)\s*\1 *(?:\n+|$)/,paragraph:/^/,heading:/^ *(#{1,6}) +([^\n]+?) *#* *(?:\n+|$)/});block.gfm.paragraph=replace(block.paragraph)("(?!","(?!"+block.gfm.fences.source.replace("\\1","\\2")+"|"+block.list.source.replace("\\1","\\3")+"|")();block.tables=merge({},block.gfm,{nptable:/^ *(\S.*\|.*)\n *([-:]+ *\|[-| :]*)\n((?:.*\|.*(?:\n|$))*)\n*/,table:/^ *\|(.+)\n *\|( *[-:]+[-| :]*)\n((?: *\|.*(?:\n|$))*)\n*/});function Lexer(options){this.tokens=[];this.tokens.links={};this.options=options||marked.defaults;this.rules=block.normal;if(this.options.gfm){if(this.options.tables){this.rules=block.tables}else{this.rules=block.gfm}}}Lexer.rules=block;Lexer.lex=function(src,options){var lexer=new Lexer(options);return lexer.lex(src)};Lexer.prototype.lex=function(src){src=src.replace(/\r\n|\r/g,"\n").replace(/\t/g,"    ").replace(/\u00a0/g," ").replace(/\u2424/g,"\n");return this.token(src,true)};Lexer.prototype.token=function(src,top,bq){var src=src.replace(/^ +$/gm,""),next,loose,cap,bull,b,item,space,i,l;while(src){if(cap=this.rules.newline.exec(src)){src=src.substring(cap[0].length);if(cap[0].length>1){this.tokens.push({type:"space"})}}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);cap=cap[0].replace(/^ {4}/gm,"");this.tokens.push({type:"code",text:!this.options.pedantic?cap.replace(/\n+$/,""):cap});continue}if(cap=this.rules.fences.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"code",lang:cap[2],text:cap[3]||""});continue}if(cap=this.rules.heading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[1].length,text:cap[2]});continue}if(top&&(cap=this.rules.nptable.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].split(/ *\| */)}this.tokens.push(item);continue}if(cap=this.rules.lheading.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"heading",depth:cap[2]==="="?1:2,text:cap[1]});continue}if(cap=this.rules.hr.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"hr"});continue}if(cap=this.rules.blockquote.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"blockquote_start"});cap=cap[0].replace(/^ *> ?/gm,"");this.token(cap,top,true);this.tokens.push({type:"blockquote_end"});continue}if(cap=this.rules.list.exec(src)){src=src.substring(cap[0].length);bull=cap[2];this.tokens.push({type:"list_start",ordered:bull.length>1});cap=cap[0].match(this.rules.item);next=false;l=cap.length;i=0;for(;i<l;i++){item=cap[i];space=item.length;item=item.replace(/^ *([*+-]|\d+\.) +/,"");if(~item.indexOf("\n ")){space-=item.length;item=!this.options.pedantic?item.replace(new RegExp("^ {1,"+space+"}","gm"),""):item.replace(/^ {1,4}/gm,"")}if(this.options.smartLists&&i!==l-1){b=block.bullet.exec(cap[i+1])[0];if(bull!==b&&!(bull.length>1&&b.length>1)){src=cap.slice(i+1).join("\n")+src;i=l-1}}loose=next||/\n\n(?!\s*$)/.test(item);if(i!==l-1){next=item.charAt(item.length-1)==="\n";if(!loose)loose=next}this.tokens.push({type:loose?"loose_item_start":"list_item_start"});this.token(item,false,bq);this.tokens.push({type:"list_item_end"})}this.tokens.push({type:"list_end"});continue}if(cap=this.rules.html.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:this.options.sanitize?"paragraph":"html",pre:!this.options.sanitizer&&(cap[1]==="pre"||cap[1]==="script"||cap[1]==="style"),text:cap[0]});continue}if(!bq&&top&&(cap=this.rules.def.exec(src))){src=src.substring(cap[0].length);this.tokens.links[cap[1].toLowerCase()]={href:cap[2],title:cap[3]};continue}if(top&&(cap=this.rules.table.exec(src))){src=src.substring(cap[0].length);item={type:"table",header:cap[1].replace(/^ *| *\| *$/g,"").split(/ *\| */),align:cap[2].replace(/^ *|\| *$/g,"").split(/ *\| */),cells:cap[3].replace(/(?: *\| *)?\n$/,"").split("\n")};for(i=0;i<item.align.length;i++){if(/^ *-+: *$/.test(item.align[i])){item.align[i]="right"}else if(/^ *:-+: *$/.test(item.align[i])){item.align[i]="center"}else if(/^ *:-+ *$/.test(item.align[i])){item.align[i]="left"}else{item.align[i]=null}}for(i=0;i<item.cells.length;i++){item.cells[i]=item.cells[i].replace(/^ *\| *| *\| *$/g,"").split(/ *\| */)}this.tokens.push(item);continue}if(top&&(cap=this.rules.paragraph.exec(src))){src=src.substring(cap[0].length);this.tokens.push({type:"paragraph",text:cap[1].charAt(cap[1].length-1)==="\n"?cap[1].slice(0,-1):cap[1]});continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);this.tokens.push({type:"text",text:cap[0]});continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return this.tokens};var inline={escape:/^\\([\\`*{}\[\]()#+\-.!_>])/,autolink:/^<([^ >]+(@|:\/)[^ >]+)>/,url:noop,tag:/^<!--[\s\S]*?-->|^<\/?\w+(?:"[^"]*"|'[^']*'|[^'">])*?>/,link:/^!?\[(inside)\]\(href\)/,reflink:/^!?\[(inside)\]\s*\[([^\]]*)\]/,nolink:/^!?\[((?:\[[^\]]*\]|[^\[\]])*)\]/,strong:/^_\_([\s\S]+?)_\_(?!_)|^\*\*([\s\S]+?)\*\*(?!\*)/,em:/^\b_((?:[^_]|_\_)+?)_\b|^\*((?:\*\*|[\s\S])+?)\*(?!\*)/,code:/^(`+)\s*([\s\S]*?[^`])\s*\1(?!`)/,br:/^ {2,}\n(?!\s*$)/,del:noop,text:/^[\s\S]+?(?=[\\<!\[_*`]| {2,}\n|$)/};inline._inside=/(?:\[[^\]]*\]|[^\[\]]|\](?=[^\[]*\]))*/;inline._href=/\s*<?([\s\S]*?)>?(?:\s+['"]([\s\S]*?)['"])?\s*/;inline.link=replace(inline.link)("inside",inline._inside)("href",inline._href)();inline.reflink=replace(inline.reflink)("inside",inline._inside)();inline.normal=merge({},inline);inline.pedantic=merge({},inline.normal,{strong:/^_\_(?=\S)([\s\S]*?\S)_\_(?!_)|^\*\*(?=\S)([\s\S]*?\S)\*\*(?!\*)/,em:/^_(?=\S)([\s\S]*?\S)_(?!_)|^\*(?=\S)([\s\S]*?\S)\*(?!\*)/});inline.gfm=merge({},inline.normal,{escape:replace(inline.escape)("])","~|])")(),url:/^(https?:\/\/[^\s<]+[^<.,:;"')\]\s])/,del:/^~~(?=\S)([\s\S]*?\S)~~/,text:replace(inline.text)("]|","~]|")("|","|https?://|")()});inline.breaks=merge({},inline.gfm,{br:replace(inline.br)("{2,}","*")(),text:replace(inline.gfm.text)("{2,}","*")()});function InlineLexer(links,options){this.options=options||marked.defaults;this.links=links;this.rules=inline.normal;this.renderer=this.options.renderer||new Renderer;this.renderer.options=this.options;if(!this.links){throw new Error("Tokens array requires a `links` property.")}if(this.options.gfm){if(this.options.breaks){this.rules=inline.breaks}else{this.rules=inline.gfm}}else if(this.options.pedantic){this.rules=inline.pedantic}}InlineLexer.rules=inline;InlineLexer.output=function(src,links,options){var inline=new InlineLexer(links,options);return inline.output(src)};InlineLexer.prototype.output=function(src){var out="",link,text,href,cap;while(src){if(cap=this.rules.escape.exec(src)){src=src.substring(cap[0].length);out+=cap[1];continue}if(cap=this.rules.autolink.exec(src)){src=src.substring(cap[0].length);if(cap[2]==="@"){text=cap[1].charAt(6)===":"?this.mangle(cap[1].substring(7)):this.mangle(cap[1]);href=this.mangle("mailto:")+text}else{text=escape(cap[1]);href=text}out+=this.renderer.link(href,null,text);continue}if(!this.inLink&&(cap=this.rules.url.exec(src))){src=src.substring(cap[0].length);text=escape(cap[1]);href=text;out+=this.renderer.link(href,null,text);continue}if(cap=this.rules.tag.exec(src)){if(!this.inLink&&/^<a /i.test(cap[0])){this.inLink=true}else if(this.inLink&&/^<\/a>/i.test(cap[0])){this.inLink=false}src=src.substring(cap[0].length);out+=this.options.sanitize?this.options.sanitizer?this.options.sanitizer(cap[0]):escape(cap[0]):cap[0];continue}if(cap=this.rules.link.exec(src)){src=src.substring(cap[0].length);this.inLink=true;out+=this.outputLink(cap,{href:cap[2],title:cap[3]});this.inLink=false;continue}if((cap=this.rules.reflink.exec(src))||(cap=this.rules.nolink.exec(src))){src=src.substring(cap[0].length);link=(cap[2]||cap[1]).replace(/\s+/g," ");link=this.links[link.toLowerCase()];if(!link||!link.href){out+=cap[0].charAt(0);src=cap[0].substring(1)+src;continue}this.inLink=true;out+=this.outputLink(cap,link);this.inLink=false;continue}if(cap=this.rules.strong.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.strong(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.em.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.em(this.output(cap[2]||cap[1]));continue}if(cap=this.rules.code.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.codespan(escape(cap[2],true));continue}if(cap=this.rules.br.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.br();continue}if(cap=this.rules.del.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.del(this.output(cap[1]));continue}if(cap=this.rules.text.exec(src)){src=src.substring(cap[0].length);out+=this.renderer.text(escape(this.smartypants(cap[0])));continue}if(src){throw new Error("Infinite loop on byte: "+src.charCodeAt(0))}}return out};InlineLexer.prototype.outputLink=function(cap,link){var href=escape(link.href),title=link.title?escape(link.title):null;return cap[0].charAt(0)!=="!"?this.renderer.link(href,title,this.output(cap[1])):this.renderer.image(href,title,escape(cap[1]))};InlineLexer.prototype.smartypants=function(text){if(!this.options.smartypants)return text;return text.replace(/---/g,"—").replace(/--/g,"–").replace(/(^|[-\u2014\/(\[{"\s])'/g,"$1‘").replace(/'/g,"’").replace(/(^|[-\u2014\/(\[{\u2018\s])"/g,"$1“").replace(/"/g,"”").replace(/\.{3}/g,"…")};InlineLexer.prototype.mangle=function(text){if(!this.options.mangle)return text;var out="",l=text.length,i=0,ch;for(;i<l;i++){ch=text.charCodeAt(i);if(Math.random()>.5){ch="x"+ch.toString(16)}out+="&#"+ch+";"}return out};function Renderer(options){this.options=options||{}}Renderer.prototype.code=function(code,lang,escaped){if(this.options.highlight){var out=this.options.highlight(code,lang);if(out!=null&&out!==code){escaped=true;code=out}}if(!lang){return"<pre><code>"+(escaped?code:escape(code,true))+"\n</code></pre>"}return'<pre><code class="'+this.options.langPrefix+escape(lang,true)+'">'+(escaped?code:escape(code,true))+"\n</code></pre>\n"};Renderer.prototype.blockquote=function(quote){return"<blockquote>\n"+quote+"</blockquote>\n"};Renderer.prototype.html=function(html){return html};Renderer.prototype.heading=function(text,level,raw){return"<h"+level+' id="'+this.options.headerPrefix+raw.toLowerCase().replace(/[^\w]+/g,"-")+'">'+text+"</h"+level+">\n"};Renderer.prototype.hr=function(){return this.options.xhtml?"<hr/>\n":"<hr>\n"};Renderer.prototype.list=function(body,ordered){var type=ordered?"ol":"ul";return"<"+type+">\n"+body+"</"+type+">\n"};Renderer.prototype.listitem=function(text){return"<li>"+text+"</li>\n"};Renderer.prototype.paragraph=function(text){return"<p>"+text+"</p>\n"};Renderer.prototype.table=function(header,body){return"<table>\n"+"<thead>\n"+header+"</thead>\n"+"<tbody>\n"+body+"</tbody>\n"+"</table>\n"};Renderer.prototype.tablerow=function(content){return"<tr>\n"+content+"</tr>\n"};Renderer.prototype.tablecell=function(content,flags){var type=flags.header?"th":"td";var tag=flags.align?"<"+type+' style="text-align:'+flags.align+'">':"<"+type+">";return tag+content+"</"+type+">\n"};Renderer.prototype.strong=function(text){return"<strong>"+text+"</strong>"};Renderer.prototype.em=function(text){return"<em>"+text+"</em>"};Renderer.prototype.codespan=function(text){return"<code>"+text+"</code>"};Renderer.prototype.br=function(){return this.options.xhtml?"<br/>":"<br>"};Renderer.prototype.del=function(text){return"<del>"+text+"</del>"};Renderer.prototype.link=function(href,title,text){if(this.options.sanitize){try{var prot=decodeURIComponent(unescape(href)).replace(/[^\w:]/g,"").toLowerCase()}catch(e){return""}if(prot.indexOf("javascript:")===0||prot.indexOf("vbscript:")===0||prot.indexOf("data:")===0){return""}}var out='<a href="'+href+'"';if(title){out+=' title="'+title+'"'}out+=">"+text+"</a>";return out};Renderer.prototype.image=function(href,title,text){var out='<img src="'+href+'" alt="'+text+'"';if(title){out+=' title="'+title+'"'}out+=this.options.xhtml?"/>":">";return out};Renderer.prototype.text=function(text){return text};function Parser(options){this.tokens=[];this.token=null;this.options=options||marked.defaults;this.options.renderer=this.options.renderer||new Renderer;this.renderer=this.options.renderer;this.renderer.options=this.options}Parser.parse=function(src,options,renderer){var parser=new Parser(options,renderer);return parser.parse(src)};Parser.prototype.parse=function(src){this.inline=new InlineLexer(src.links,this.options,this.renderer);this.tokens=src.reverse();var out="";while(this.next()){out+=this.tok()}return out};Parser.prototype.next=function(){return this.token=this.tokens.pop()};Parser.prototype.peek=function(){return this.tokens[this.tokens.length-1]||0};Parser.prototype.parseText=function(){var body=this.token.text;while(this.peek().type==="text"){body+="\n"+this.next().text}return this.inline.output(body)};Parser.prototype.tok=function(){switch(this.token.type){case"space":{return""}case"hr":{return this.renderer.hr()}case"heading":{return this.renderer.heading(this.inline.output(this.token.text),this.token.depth,this.token.text)}case"code":{return this.renderer.code(this.token.text,this.token.lang,this.token.escaped)}case"table":{var header="",body="",i,row,cell,flags,j;cell="";for(i=0;i<this.token.header.length;i++){flags={header:true,align:this.token.align[i]};cell+=this.renderer.tablecell(this.inline.output(this.token.header[i]),{header:true,align:this.token.align[i]})}header+=this.renderer.tablerow(cell);for(i=0;i<this.token.cells.length;i++){row=this.token.cells[i];cell="";for(j=0;j<row.length;j++){cell+=this.renderer.tablecell(this.inline.output(row[j]),{header:false,align:this.token.align[j]})}body+=this.renderer.tablerow(cell)}return this.renderer.table(header,body)}case"blockquote_start":{var body="";while(this.next().type!=="blockquote_end"){body+=this.tok()}return this.renderer.blockquote(body)}case"list_start":{var body="",ordered=this.token.ordered;while(this.next().type!=="list_end"){body+=this.tok()}return this.renderer.list(body,ordered)}case"list_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.token.type==="text"?this.parseText():this.tok()}return this.renderer.listitem(body)}case"loose_item_start":{var body="";while(this.next().type!=="list_item_end"){body+=this.tok()}return this.renderer.listitem(body)}case"html":{var html=!this.token.pre&&!this.options.pedantic?this.inline.output(this.token.text):this.token.text;return this.renderer.html(html)}case"paragraph":{return this.renderer.paragraph(this.inline.output(this.token.text))}case"text":{return this.renderer.paragraph(this.parseText())}}};function escape(html,encode){return html.replace(!encode?/&(?!#?\w+;)/g:/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/"/g,"&quot;").replace(/'/g,"&#39;")}function unescape(html){return html.replace(/&(#(?:\d+)|(?:#x[0-9A-Fa-f]+)|(?:\w+));?/g,function(_,n){n=n.toLowerCase();if(n==="colon")return":";if(n.charAt(0)==="#"){return n.charAt(1)==="x"?String.fromCharCode(parseInt(n.substring(2),16)):String.fromCharCode(+n.substring(1))}return""})}function replace(regex,opt){regex=regex.source;opt=opt||"";return function self(name,val){if(!name)return new RegExp(regex,opt);val=val.source||val;val=val.replace(/(^|[^\[])\^/g,"$1");regex=regex.replace(name,val);return self}}function noop(){}noop.exec=noop;function merge(obj){var i=1,target,key;for(;i<arguments.length;i++){target=arguments[i];for(key in target){if(Object.prototype.hasOwnProperty.call(target,key)){obj[key]=target[key]}}}return obj}function marked(src,opt,callback){if(callback||typeof opt==="function"){if(!callback){callback=opt;opt=null}opt=merge({},marked.defaults,opt||{});var highlight=opt.highlight,tokens,pending,i=0;try{tokens=Lexer.lex(src,opt)}catch(e){return callback(e)}pending=tokens.length;var done=function(err){if(err){opt.highlight=highlight;return callback(err)}var out;try{out=Parser.parse(tokens,opt)}catch(e){err=e}opt.highlight=highlight;return err?callback(err):callback(null,out)};if(!highlight||highlight.length<3){return done()}delete opt.highlight;if(!pending)return done();for(;i<tokens.length;i++){(function(token){if(token.type!=="code"){return--pending||done()}return highlight(token.text,token.lang,function(err,code){if(err)return done(err);if(code==null||code===token.text){return--pending||done()}token.text=code;token.escaped=true;--pending||done()})})(tokens[i])}return}try{if(opt)opt=merge({},marked.defaults,opt);return Parser.parse(Lexer.lex(src,opt),opt)}catch(e){e.message+="\nPlease report this to https://github.com/chjj/marked.";if((opt||marked.defaults).silent){return"<p>An error occured:</p><pre>"+escape(e.message+"",true)+"</pre>"}throw e}}marked.options=marked.setOptions=function(opt){merge(marked.defaults,opt);return marked};marked.defaults={gfm:true,tables:true,breaks:false,pedantic:false,sanitize:false,sanitizer:null,mangle:true,smartLists:false,silent:false,highlight:null,langPrefix:"lang-",smartypants:false,headerPrefix:"",renderer:new Renderer,xhtml:false};marked.Parser=Parser;marked.parser=Parser.parse;marked.Renderer=Renderer;marked.Lexer=Lexer;marked.lexer=Lexer.lex;marked.InlineLexer=InlineLexer;marked.inlineLexer=InlineLexer.output;marked.parse=marked;if(typeof module!=="undefined"&&typeof exports==="object"){module.exports=marked}else if(typeof define==="function"&&define.amd){define(function(){return marked})}else{this.marked=marked}}).call(function(){return this||(typeof window!=="undefined"?window:global)}());

	return module.exports;
}();


// FORMAT OPTIONS FOR MARKED IMPLEMENTATION

function _Markdown_formatOptions(options)
{
	function toHighlight(code, lang)
	{
		if (!lang && $elm$core$Maybe$isJust(options.defaultHighlighting))
		{
			lang = options.defaultHighlighting.a;
		}

		if (typeof hljs !== 'undefined' && lang && hljs.listLanguages().indexOf(lang) >= 0)
		{
			return hljs.highlight(lang, code, true).value;
		}

		return code;
	}

	var gfm = options.githubFlavored.a;

	return {
		highlight: toHighlight,
		gfm: gfm,
		tables: gfm && gfm.tables,
		breaks: gfm && gfm.breaks,
		sanitize: options.sanitize,
		smartypants: options.smartypants
	};
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var $author$project$Main$BrowserChangedUrl = function (a) {
	return {$: 'BrowserChangedUrl', a: a};
};
var $author$project$Main$UserClickedLink = function (a) {
	return {$: 'UserClickedLink', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Debugger$Expando$ArraySeq = {$: 'ArraySeq'};
var $elm$browser$Debugger$Overlay$BlockMost = {$: 'BlockMost'};
var $elm$browser$Debugger$Overlay$BlockNone = {$: 'BlockNone'};
var $elm$browser$Debugger$Expando$Constructor = F3(
	function (a, b, c) {
		return {$: 'Constructor', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$Dictionary = F2(
	function (a, b) {
		return {$: 'Dictionary', a: a, b: b};
	});
var $elm$browser$Debugger$Main$Down = {$: 'Down'};
var $elm$browser$Debugger$Expando$ListSeq = {$: 'ListSeq'};
var $elm$browser$Debugger$Main$NoOp = {$: 'NoOp'};
var $elm$browser$Debugger$Expando$Primitive = function (a) {
	return {$: 'Primitive', a: a};
};
var $elm$browser$Debugger$Expando$Record = F2(
	function (a, b) {
		return {$: 'Record', a: a, b: b};
	});
var $elm$browser$Debugger$Expando$S = function (a) {
	return {$: 'S', a: a};
};
var $elm$browser$Debugger$Expando$Sequence = F3(
	function (a, b, c) {
		return {$: 'Sequence', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$SetSeq = {$: 'SetSeq'};
var $elm$browser$Debugger$Main$Up = {$: 'Up'};
var $elm$browser$Debugger$Main$UserMsg = function (a) {
	return {$: 'UserMsg', a: a};
};
var $elm$browser$Debugger$Main$Export = {$: 'Export'};
var $elm$browser$Debugger$Main$Import = {$: 'Import'};
var $elm$browser$Debugger$Main$Open = {$: 'Open'};
var $elm$browser$Debugger$Main$OverlayMsg = function (a) {
	return {$: 'OverlayMsg', a: a};
};
var $elm$browser$Debugger$Main$Resume = {$: 'Resume'};
var $elm$browser$Debugger$Main$isPaused = function (state) {
	if (state.$ === 'Running') {
		return false;
	} else {
		return true;
	}
};
var $elm$browser$Debugger$History$size = function (history) {
	return history.numMessages;
};
var $elm$browser$Debugger$Overlay$Accept = function (a) {
	return {$: 'Accept', a: a};
};
var $elm$browser$Debugger$Overlay$Choose = F2(
	function (a, b) {
		return {$: 'Choose', a: a, b: b};
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$browser$Debugger$Overlay$goodNews1 = '\nThe good news is that having values like this in your message type is not\nso great in the long run. You are better off using simpler data, like\n';
var $elm$browser$Debugger$Overlay$goodNews2 = '\nfunction can pattern match on that data and call whatever functions, JSON\ndecoders, etc. you need. This makes the code much more explicit and easy to\nfollow for other readers (or you in a few months!)\n';
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $elm$html$Html$code = _VirtualDom_node('code');
var $elm$browser$Debugger$Overlay$viewCode = function (name) {
	return A2(
		$elm$html$Html$code,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(name)
			]));
};
var $elm$browser$Debugger$Overlay$addCommas = function (items) {
	if (!items.b) {
		return '';
	} else {
		if (!items.b.b) {
			var item = items.a;
			return item;
		} else {
			if (!items.b.b.b) {
				var item1 = items.a;
				var _v1 = items.b;
				var item2 = _v1.a;
				return item1 + (' and ' + item2);
			} else {
				var lastItem = items.a;
				var otherItems = items.b;
				return A2(
					$elm$core$String$join,
					', ',
					_Utils_ap(
						otherItems,
						_List_fromArray(
							[' and ' + lastItem])));
			}
		}
	}
};
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$browser$Debugger$Overlay$problemToString = function (problem) {
	switch (problem.$) {
		case 'Function':
			return 'functions';
		case 'Decoder':
			return 'JSON decoders';
		case 'Task':
			return 'tasks';
		case 'Process':
			return 'processes';
		case 'Socket':
			return 'web sockets';
		case 'Request':
			return 'HTTP requests';
		case 'Program':
			return 'programs';
		default:
			return 'virtual DOM values';
	}
};
var $elm$browser$Debugger$Overlay$viewProblemType = function (_v0) {
	var name = _v0.name;
	var problems = _v0.problems;
	return A2(
		$elm$html$Html$li,
		_List_Nil,
		_List_fromArray(
			[
				$elm$browser$Debugger$Overlay$viewCode(name),
				$elm$html$Html$text(
				' can contain ' + ($elm$browser$Debugger$Overlay$addCommas(
					A2($elm$core$List$map, $elm$browser$Debugger$Overlay$problemToString, problems)) + '.'))
			]));
};
var $elm$browser$Debugger$Overlay$viewBadMetadata = function (_v0) {
	var message = _v0.message;
	var problems = _v0.problems;
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('The '),
					$elm$browser$Debugger$Overlay$viewCode(message),
					$elm$html$Html$text(' type of your program cannot be reliably serialized for history files.')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Functions cannot be serialized, nor can values that contain functions. This is a problem in these places:')
				])),
			A2(
			$elm$html$Html$ul,
			_List_Nil,
			A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewProblemType, problems)),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews1),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://guide.elm-lang.org/types/custom_types.html')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('custom types')
						])),
					$elm$html$Html$text(', in your messages. From there, your '),
					$elm$browser$Debugger$Overlay$viewCode('update'),
					$elm$html$Html$text($elm$browser$Debugger$Overlay$goodNews2)
				]))
		]);
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$browser$Debugger$Overlay$Cancel = {$: 'Cancel'};
var $elm$browser$Debugger$Overlay$Proceed = {$: 'Proceed'};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$browser$Debugger$Overlay$viewButtons = function (buttons) {
	var btn = F2(
		function (msg, string) {
			return A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'margin-right', '20px'),
						$elm$html$Html$Events$onClick(msg)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(string)
					]));
		});
	var buttonNodes = function () {
		if (buttons.$ === 'Accept') {
			var proceed = buttons.a;
			return _List_fromArray(
				[
					A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
				]);
		} else {
			var cancel = buttons.a;
			var proceed = buttons.b;
			return _List_fromArray(
				[
					A2(btn, $elm$browser$Debugger$Overlay$Cancel, cancel),
					A2(btn, $elm$browser$Debugger$Overlay$Proceed, proceed)
				]);
		}
	}();
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'height', '60px'),
				A2($elm$html$Html$Attributes$style, 'line-height', '60px'),
				A2($elm$html$Html$Attributes$style, 'text-align', 'right'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
			]),
		buttonNodes);
};
var $elm$browser$Debugger$Overlay$viewMessage = F4(
	function (config, title, details, buttons) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('elm-debugger-overlay'),
					A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
					A2($elm$html$Html$Attributes$style, 'top', '0'),
					A2($elm$html$Html$Attributes$style, 'left', '0'),
					A2($elm$html$Html$Attributes$style, 'width', '100vw'),
					A2($elm$html$Html$Attributes$style, 'height', '100vh'),
					A2($elm$html$Html$Attributes$style, 'color', 'white'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
					A2($elm$html$Html$Attributes$style, 'font-family', '\'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif'),
					A2($elm$html$Html$Attributes$style, 'z-index', '2147483647')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
							A2($elm$html$Html$Attributes$style, 'width', '600px'),
							A2($elm$html$Html$Attributes$style, 'height', '100vh'),
							A2($elm$html$Html$Attributes$style, 'padding-left', 'calc(50% - 300px)'),
							A2($elm$html$Html$Attributes$style, 'padding-right', 'calc(50% - 300px)'),
							A2($elm$html$Html$Attributes$style, 'background-color', 'rgba(200, 200, 200, 0.7)'),
							A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '36px'),
									A2($elm$html$Html$Attributes$style, 'height', '80px'),
									A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)'),
									A2($elm$html$Html$Attributes$style, 'padding-left', '22px'),
									A2($elm$html$Html$Attributes$style, 'vertical-align', 'middle'),
									A2($elm$html$Html$Attributes$style, 'line-height', '80px')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(title)
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('elm-debugger-details'),
									A2($elm$html$Html$Attributes$style, 'padding', ' 8px 20px'),
									A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
									A2($elm$html$Html$Attributes$style, 'max-height', 'calc(100vh - 156px)'),
									A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(61, 61, 61)')
								]),
							details),
							A2(
							$elm$html$Html$map,
							config.wrap,
							$elm$browser$Debugger$Overlay$viewButtons(buttons))
						]))
				]));
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$virtual_dom$VirtualDom$nodeNS = function (tag) {
	return _VirtualDom_nodeNS(
		_VirtualDom_noScript(tag));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$browser$Debugger$Overlay$viewShape = F4(
	function (x, y, angle, coordinates) {
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			'http://www.w3.org/2000/svg',
			'polygon',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'points', coordinates),
					A2(
					$elm$virtual_dom$VirtualDom$attribute,
					'transform',
					'translate(' + ($elm$core$String$fromFloat(x) + (' ' + ($elm$core$String$fromFloat(y) + (') rotate(' + ($elm$core$String$fromFloat(-angle) + ')'))))))
				]),
			_List_Nil);
	});
var $elm$browser$Debugger$Overlay$elmLogo = A4(
	$elm$virtual_dom$VirtualDom$nodeNS,
	'http://www.w3.org/2000/svg',
	'svg',
	_List_fromArray(
		[
			A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '-300 -300 600 600'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'currentColor'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'width', '24px'),
			A2($elm$virtual_dom$VirtualDom$attribute, 'height', '24px')
		]),
	_List_fromArray(
		[
			A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			'http://www.w3.org/2000/svg',
			'g',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'transform', 'scale(1 -1)')
				]),
			_List_fromArray(
				[
					A4($elm$browser$Debugger$Overlay$viewShape, 0, -210, 0, '-280,-90 0,190 280,-90'),
					A4($elm$browser$Debugger$Overlay$viewShape, -210, 0, 90, '-280,-90 0,190 280,-90'),
					A4($elm$browser$Debugger$Overlay$viewShape, 207, 207, 45, '-198,-66 0,132 198,-66'),
					A4($elm$browser$Debugger$Overlay$viewShape, 150, 0, 0, '-130,0 0,-130 130,0 0,130'),
					A4($elm$browser$Debugger$Overlay$viewShape, -89, 239, 0, '-191,61 69,61 191,-61 -69,-61'),
					A4($elm$browser$Debugger$Overlay$viewShape, 0, 106, 180, '-130,-44 0,86  130,-44'),
					A4($elm$browser$Debugger$Overlay$viewShape, 256, -150, 270, '-130,-44 0,86  130,-44')
				]))
		]));
var $elm$core$String$length = _String_length;
var $elm$browser$Debugger$Overlay$viewMiniControls = F2(
	function (config, numMsgs) {
		var string = $elm$core$String$fromInt(numMsgs);
		var width = $elm$core$String$fromInt(
			2 + $elm$core$String$length(string));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
					A2($elm$html$Html$Attributes$style, 'bottom', '2em'),
					A2($elm$html$Html$Attributes$style, 'right', '2em'),
					A2($elm$html$Html$Attributes$style, 'width', 'calc(42px + ' + (width + 'ch)')),
					A2($elm$html$Html$Attributes$style, 'height', '36px'),
					A2($elm$html$Html$Attributes$style, 'background-color', '#1293D8'),
					A2($elm$html$Html$Attributes$style, 'color', 'white'),
					A2($elm$html$Html$Attributes$style, 'font-family', 'monospace'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto'),
					A2($elm$html$Html$Attributes$style, 'z-index', '2147483647'),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					$elm$html$Html$Events$onClick(config.open)
				]),
			_List_fromArray(
				[
					$elm$browser$Debugger$Overlay$elmLogo,
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding-left', 'calc(1ch + 6px)'),
							A2($elm$html$Html$Attributes$style, 'padding-right', '1ch')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(string)
						]))
				]));
	});
var $elm$browser$Debugger$Overlay$explanationBad = '\nThe messages in this history do not match the messages handled by your\nprogram. I noticed changes in the following types:\n';
var $elm$browser$Debugger$Overlay$explanationRisky = '\nThis history seems old. It will work with this program, but some\nmessages have been added since the history was created:\n';
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$browser$Debugger$Overlay$viewMention = F2(
	function (tags, verbed) {
		var _v0 = A2(
			$elm$core$List$map,
			$elm$browser$Debugger$Overlay$viewCode,
			$elm$core$List$reverse(tags));
		if (!_v0.b) {
			return $elm$html$Html$text('');
		} else {
			if (!_v0.b.b) {
				var tag = _v0.a;
				return A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(verbed),
							tag,
							$elm$html$Html$text('.')
						]));
			} else {
				if (!_v0.b.b.b) {
					var tag2 = _v0.a;
					var _v1 = _v0.b;
					var tag1 = _v1.a;
					return A2(
						$elm$html$Html$li,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(verbed),
								tag1,
								$elm$html$Html$text(' and '),
								tag2,
								$elm$html$Html$text('.')
							]));
				} else {
					var lastTag = _v0.a;
					var otherTags = _v0.b;
					return A2(
						$elm$html$Html$li,
						_List_Nil,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text(verbed),
							_Utils_ap(
								A2(
									$elm$core$List$intersperse,
									$elm$html$Html$text(', '),
									$elm$core$List$reverse(otherTags)),
								_List_fromArray(
									[
										$elm$html$Html$text(', and '),
										lastTag,
										$elm$html$Html$text('.')
									]))));
				}
			}
		}
	});
var $elm$browser$Debugger$Overlay$viewChange = function (change) {
	return A2(
		$elm$html$Html$li,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'margin', '8px 0')
			]),
		function () {
			if (change.$ === 'AliasChange') {
				var name = change.a;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-size', '1.5em')
							]),
						_List_fromArray(
							[
								$elm$browser$Debugger$Overlay$viewCode(name)
							]))
					]);
			} else {
				var name = change.a;
				var removed = change.b.removed;
				var changed = change.b.changed;
				var added = change.b.added;
				var argsMatch = change.b.argsMatch;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'font-size', '1.5em')
							]),
						_List_fromArray(
							[
								$elm$browser$Debugger$Overlay$viewCode(name)
							])),
						A2(
						$elm$html$Html$ul,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'list-style-type', 'disc'),
								A2($elm$html$Html$Attributes$style, 'padding-left', '2em')
							]),
						_List_fromArray(
							[
								A2($elm$browser$Debugger$Overlay$viewMention, removed, 'Removed '),
								A2($elm$browser$Debugger$Overlay$viewMention, changed, 'Changed '),
								A2($elm$browser$Debugger$Overlay$viewMention, added, 'Added ')
							])),
						argsMatch ? $elm$html$Html$text('') : $elm$html$Html$text('This may be due to the fact that the type variable names changed.')
					]);
			}
		}());
};
var $elm$browser$Debugger$Overlay$viewReport = F2(
	function (isBad, report) {
		switch (report.$) {
			case 'CorruptHistory':
				return _List_fromArray(
					[
						$elm$html$Html$text('Looks like this history file is corrupt. I cannot understand it.')
					]);
			case 'VersionChanged':
				var old = report.a;
				var _new = report.b;
				return _List_fromArray(
					[
						$elm$html$Html$text('This history was created with Elm ' + (old + (', but you are using Elm ' + (_new + ' right now.'))))
					]);
			case 'MessageChanged':
				var old = report.a;
				var _new = report.b;
				return _List_fromArray(
					[
						$elm$html$Html$text('To import some other history, the overall message type must' + ' be the same. The old history has '),
						$elm$browser$Debugger$Overlay$viewCode(old),
						$elm$html$Html$text(' messages, but the new program works with '),
						$elm$browser$Debugger$Overlay$viewCode(_new),
						$elm$html$Html$text(' messages.')
					]);
			default:
				var changes = report.a;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								isBad ? $elm$browser$Debugger$Overlay$explanationBad : $elm$browser$Debugger$Overlay$explanationRisky)
							])),
						A2(
						$elm$html$Html$ul,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'list-style-type', 'none'),
								A2($elm$html$Html$Attributes$style, 'padding-left', '20px')
							]),
						A2($elm$core$List$map, $elm$browser$Debugger$Overlay$viewChange, changes))
					]);
		}
	});
var $elm$browser$Debugger$Overlay$view = F5(
	function (config, isPaused, isOpen, numMsgs, state) {
		switch (state.$) {
			case 'None':
				return isOpen ? $elm$html$Html$text('') : (isPaused ? A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('elm-debugger-overlay'),
							A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
							A2($elm$html$Html$Attributes$style, 'top', '0'),
							A2($elm$html$Html$Attributes$style, 'left', '0'),
							A2($elm$html$Html$Attributes$style, 'width', '100vw'),
							A2($elm$html$Html$Attributes$style, 'height', '100vh'),
							A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
							A2($elm$html$Html$Attributes$style, 'display', 'flex'),
							A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
							A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
							A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto'),
							A2($elm$html$Html$Attributes$style, 'background-color', 'rgba(200, 200, 200, 0.7)'),
							A2($elm$html$Html$Attributes$style, 'color', 'white'),
							A2($elm$html$Html$Attributes$style, 'font-family', '\'Trebuchet MS\', \'Lucida Grande\', \'Bitstream Vera Sans\', \'Helvetica Neue\', sans-serif'),
							A2($elm$html$Html$Attributes$style, 'z-index', '2147483646'),
							$elm$html$Html$Events$onClick(config.resume)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', '80px')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Click to Resume')
								])),
							A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs)
						])) : A2($elm$browser$Debugger$Overlay$viewMiniControls, config, numMsgs));
			case 'BadMetadata':
				var badMetadata_ = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Cannot use Import or Export',
					$elm$browser$Debugger$Overlay$viewBadMetadata(badMetadata_),
					$elm$browser$Debugger$Overlay$Accept('Ok'));
			case 'BadImport':
				var report = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Cannot Import History',
					A2($elm$browser$Debugger$Overlay$viewReport, true, report),
					$elm$browser$Debugger$Overlay$Accept('Ok'));
			default:
				var report = state.a;
				return A4(
					$elm$browser$Debugger$Overlay$viewMessage,
					config,
					'Warning',
					A2($elm$browser$Debugger$Overlay$viewReport, false, report),
					A2($elm$browser$Debugger$Overlay$Choose, 'Cancel', 'Import Anyway'));
		}
	});
var $elm$browser$Debugger$Main$cornerView = function (model) {
	return A5(
		$elm$browser$Debugger$Overlay$view,
		{exportHistory: $elm$browser$Debugger$Main$Export, importHistory: $elm$browser$Debugger$Main$Import, open: $elm$browser$Debugger$Main$Open, resume: $elm$browser$Debugger$Main$Resume, wrap: $elm$browser$Debugger$Main$OverlayMsg},
		$elm$browser$Debugger$Main$isPaused(model.state),
		_Debugger_isOpen(model.popout),
		$elm$browser$Debugger$History$size(model.history),
		model.overlay);
};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Set$foldr = F3(
	function (func, initialState, _v0) {
		var dict = _v0.a;
		return A3(
			$elm$core$Dict$foldr,
			F3(
				function (key, _v1, state) {
					return A2(func, key, state);
				}),
			initialState,
			dict);
	});
var $elm$browser$Debugger$Main$getCurrentModel = function (state) {
	if (state.$ === 'Running') {
		var model = state.a;
		return model;
	} else {
		var model = state.b;
		return model;
	}
};
var $elm$browser$Debugger$Main$getUserModel = function (model) {
	return $elm$browser$Debugger$Main$getCurrentModel(model.state);
};
var $elm$browser$Debugger$Main$initialWindowHeight = 420;
var $elm$browser$Debugger$Main$initialWindowWidth = 900;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$browser$Debugger$Main$cachedHistory = function (model) {
	var _v0 = model.state;
	if (_v0.$ === 'Running') {
		return model.history;
	} else {
		var history = _v0.e;
		return history;
	}
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$browser$Debugger$Main$DragEnd = {$: 'DragEnd'};
var $elm$browser$Debugger$Main$getDragStatus = function (layout) {
	if (layout.$ === 'Horizontal') {
		var status = layout.a;
		return status;
	} else {
		var status = layout.a;
		return status;
	}
};
var $elm$browser$Debugger$Main$Drag = function (a) {
	return {$: 'Drag', a: a};
};
var $elm$browser$Debugger$Main$DragInfo = F5(
	function (x, y, down, width, height) {
		return {down: down, height: height, width: width, x: x, y: y};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$browser$Debugger$Main$decodeDimension = function (field) {
	return A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['currentTarget', 'ownerDocument', 'defaultView', field]),
		$elm$json$Json$Decode$float);
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map5 = _Json_map5;
var $elm$browser$Debugger$Main$onMouseMove = A2(
	$elm$html$Html$Events$on,
	'mousemove',
	A2(
		$elm$json$Json$Decode$map,
		$elm$browser$Debugger$Main$Drag,
		A6(
			$elm$json$Json$Decode$map5,
			$elm$browser$Debugger$Main$DragInfo,
			A2($elm$json$Json$Decode$field, 'pageX', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'pageY', $elm$json$Json$Decode$float),
			A2(
				$elm$json$Json$Decode$field,
				'buttons',
				A2(
					$elm$json$Json$Decode$map,
					function (v) {
						return v === 1;
					},
					$elm$json$Json$Decode$int)),
			$elm$browser$Debugger$Main$decodeDimension('innerWidth'),
			$elm$browser$Debugger$Main$decodeDimension('innerHeight'))));
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$browser$Debugger$Main$toDragListeners = function (layout) {
	var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
	if (_v0.$ === 'Static') {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				$elm$browser$Debugger$Main$onMouseMove,
				$elm$html$Html$Events$onMouseUp($elm$browser$Debugger$Main$DragEnd)
			]);
	}
};
var $elm$browser$Debugger$Main$toFlexDirection = function (layout) {
	if (layout.$ === 'Horizontal') {
		return 'row';
	} else {
		return 'column-reverse';
	}
};
var $elm$browser$Debugger$Main$DragStart = {$: 'DragStart'};
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$browser$Debugger$Main$toPercent = function (fraction) {
	return $elm$core$String$fromFloat(100 * fraction) + '%';
};
var $elm$browser$Debugger$Main$viewDragZone = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2($elm$html$Html$Attributes$style, 'top', '0'),
					A2(
					$elm$html$Html$Attributes$style,
					'left',
					$elm$browser$Debugger$Main$toPercent(x)),
					A2($elm$html$Html$Attributes$style, 'margin-left', '-5px'),
					A2($elm$html$Html$Attributes$style, 'width', '10px'),
					A2($elm$html$Html$Attributes$style, 'height', '100%'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'col-resize'),
					$elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
				]),
			_List_Nil);
	} else {
		var y = layout.c;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
					A2(
					$elm$html$Html$Attributes$style,
					'top',
					$elm$browser$Debugger$Main$toPercent(y)),
					A2($elm$html$Html$Attributes$style, 'left', '0'),
					A2($elm$html$Html$Attributes$style, 'margin-top', '-5px'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '10px'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'row-resize'),
					$elm$html$Html$Events$onMouseDown($elm$browser$Debugger$Main$DragStart)
				]),
			_List_Nil);
	}
};
var $elm$browser$Debugger$Main$TweakExpandoModel = function (a) {
	return {$: 'TweakExpandoModel', a: a};
};
var $elm$browser$Debugger$Main$TweakExpandoMsg = function (a) {
	return {$: 'TweakExpandoMsg', a: a};
};
var $elm$browser$Debugger$Main$toExpandoPercents = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return _Utils_Tuple2(
			$elm$browser$Debugger$Main$toPercent(1 - x),
			'100%');
	} else {
		var y = layout.c;
		return _Utils_Tuple2(
			'100%',
			$elm$browser$Debugger$Main$toPercent(y));
	}
};
var $elm$browser$Debugger$Main$toMouseBlocker = function (layout) {
	var _v0 = $elm$browser$Debugger$Main$getDragStatus(layout);
	if (_v0.$ === 'Static') {
		return 'auto';
	} else {
		return 'none';
	}
};
var $elm$browser$Debugger$Expando$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$browser$Debugger$Expando$Index = F3(
	function (a, b, c) {
		return {$: 'Index', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Expando$Key = {$: 'Key'};
var $elm$browser$Debugger$Expando$None = {$: 'None'};
var $elm$browser$Debugger$Expando$Toggle = {$: 'Toggle'};
var $elm$browser$Debugger$Expando$Value = {$: 'Value'};
var $elm$browser$Debugger$Expando$blue = A2($elm$html$Html$Attributes$style, 'color', 'rgb(28, 0, 207)');
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Debugger$Expando$leftPad = function (maybeKey) {
	if (maybeKey.$ === 'Nothing') {
		return _List_Nil;
	} else {
		return _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'padding-left', '4ch')
			]);
	}
};
var $elm$browser$Debugger$Expando$makeArrow = function (arrow) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'color', '#777'),
				A2($elm$html$Html$Attributes$style, 'padding-left', '2ch'),
				A2($elm$html$Html$Attributes$style, 'width', '2ch'),
				A2($elm$html$Html$Attributes$style, 'display', 'inline-block')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(arrow)
			]));
};
var $elm$browser$Debugger$Expando$purple = A2($elm$html$Html$Attributes$style, 'color', 'rgb(136, 19, 145)');
var $elm$browser$Debugger$Expando$lineStarter = F3(
	function (maybeKey, maybeIsClosed, description) {
		var arrow = function () {
			if (maybeIsClosed.$ === 'Nothing') {
				return $elm$browser$Debugger$Expando$makeArrow('');
			} else {
				if (maybeIsClosed.a) {
					return $elm$browser$Debugger$Expando$makeArrow('▸');
				} else {
					return $elm$browser$Debugger$Expando$makeArrow('▾');
				}
			}
		}();
		if (maybeKey.$ === 'Nothing') {
			return A2($elm$core$List$cons, arrow, description);
		} else {
			var key = maybeKey.a;
			return A2(
				$elm$core$List$cons,
				arrow,
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$purple]),
						_List_fromArray(
							[
								$elm$html$Html$text(key)
							])),
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(' = '),
						description)));
		}
	});
var $elm$browser$Debugger$Expando$red = A2($elm$html$Html$Attributes$style, 'color', 'rgb(196, 26, 22)');
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$browser$Debugger$Expando$seqTypeToString = F2(
	function (n, seqType) {
		switch (seqType.$) {
			case 'ListSeq':
				return 'List(' + ($elm$core$String$fromInt(n) + ')');
			case 'SetSeq':
				return 'Set(' + ($elm$core$String$fromInt(n) + ')');
			default:
				return 'Array(' + ($elm$core$String$fromInt(n) + ')');
		}
	});
var $elm$core$String$slice = _String_slice;
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $elm$browser$Debugger$Expando$elideMiddle = function (str) {
	return ($elm$core$String$length(str) <= 18) ? str : (A2($elm$core$String$left, 8, str) + ('...' + A2($elm$core$String$right, 8, str)));
};
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $elm$browser$Debugger$Expando$viewExtraTinyRecord = F3(
	function (length, starter, entries) {
		if (!entries.b) {
			return _Utils_Tuple2(
				length + 1,
				_List_fromArray(
					[
						$elm$html$Html$text('}')
					]));
		} else {
			var field = entries.a;
			var rest = entries.b;
			var nextLength = (length + $elm$core$String$length(field)) + 1;
			if (nextLength > 18) {
				return _Utils_Tuple2(
					length + 2,
					_List_fromArray(
						[
							$elm$html$Html$text('…}')
						]));
			} else {
				var _v1 = A3($elm$browser$Debugger$Expando$viewExtraTinyRecord, nextLength, ',', rest);
				var finalLength = _v1.a;
				var otherHtmls = _v1.b;
				return _Utils_Tuple2(
					finalLength,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(starter),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$purple]),
								_List_fromArray(
									[
										$elm$html$Html$text(field)
									])),
							otherHtmls)));
			}
		}
	});
var $elm$browser$Debugger$Expando$viewTinyHelp = function (str) {
	return _Utils_Tuple2(
		$elm$core$String$length(str),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $elm$browser$Debugger$Expando$viewExtraTiny = function (value) {
	if (value.$ === 'Record') {
		var record = value.b;
		return A3(
			$elm$browser$Debugger$Expando$viewExtraTinyRecord,
			0,
			'{',
			$elm$core$Dict$keys(record));
	} else {
		return $elm$browser$Debugger$Expando$viewTiny(value);
	}
};
var $elm$browser$Debugger$Expando$viewTiny = function (value) {
	switch (value.$) {
		case 'S':
			var stringRep = value.a;
			var str = $elm$browser$Debugger$Expando$elideMiddle(stringRep);
			return _Utils_Tuple2(
				$elm$core$String$length(str),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$red]),
						_List_fromArray(
							[
								$elm$html$Html$text(str)
							]))
					]));
		case 'Primitive':
			var stringRep = value.a;
			return _Utils_Tuple2(
				$elm$core$String$length(stringRep),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[$elm$browser$Debugger$Expando$blue]),
						_List_fromArray(
							[
								$elm$html$Html$text(stringRep)
							]))
					]));
		case 'Sequence':
			var seqType = value.a;
			var valueList = value.c;
			return $elm$browser$Debugger$Expando$viewTinyHelp(
				A2(
					$elm$browser$Debugger$Expando$seqTypeToString,
					$elm$core$List$length(valueList),
					seqType));
		case 'Dictionary':
			var keyValuePairs = value.b;
			return $elm$browser$Debugger$Expando$viewTinyHelp(
				'Dict(' + ($elm$core$String$fromInt(
					$elm$core$List$length(keyValuePairs)) + ')'));
		case 'Record':
			var record = value.b;
			return $elm$browser$Debugger$Expando$viewTinyRecord(record);
		default:
			if (!value.c.b) {
				var maybeName = value.a;
				return $elm$browser$Debugger$Expando$viewTinyHelp(
					A2($elm$core$Maybe$withDefault, 'Unit', maybeName));
			} else {
				var maybeName = value.a;
				var valueList = value.c;
				return $elm$browser$Debugger$Expando$viewTinyHelp(
					function () {
						if (maybeName.$ === 'Nothing') {
							return 'Tuple(' + ($elm$core$String$fromInt(
								$elm$core$List$length(valueList)) + ')');
						} else {
							var name = maybeName.a;
							return name + ' …';
						}
					}());
			}
	}
};
var $elm$browser$Debugger$Expando$viewTinyRecord = function (record) {
	return $elm$core$Dict$isEmpty(record) ? _Utils_Tuple2(
		2,
		_List_fromArray(
			[
				$elm$html$Html$text('{}')
			])) : A3(
		$elm$browser$Debugger$Expando$viewTinyRecordHelp,
		0,
		'{ ',
		$elm$core$Dict$toList(record));
};
var $elm$browser$Debugger$Expando$viewTinyRecordHelp = F3(
	function (length, starter, entries) {
		if (!entries.b) {
			return _Utils_Tuple2(
				length + 2,
				_List_fromArray(
					[
						$elm$html$Html$text(' }')
					]));
		} else {
			var _v1 = entries.a;
			var field = _v1.a;
			var value = _v1.b;
			var rest = entries.b;
			var fieldLen = $elm$core$String$length(field);
			var _v2 = $elm$browser$Debugger$Expando$viewExtraTiny(value);
			var valueLen = _v2.a;
			var valueHtmls = _v2.b;
			var newLength = ((length + fieldLen) + valueLen) + 5;
			if (newLength > 60) {
				return _Utils_Tuple2(
					length + 4,
					_List_fromArray(
						[
							$elm$html$Html$text(', … }')
						]));
			} else {
				var _v3 = A3($elm$browser$Debugger$Expando$viewTinyRecordHelp, newLength, ', ', rest);
				var finalLength = _v3.a;
				var otherHtmls = _v3.b;
				return _Utils_Tuple2(
					finalLength,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$text(starter),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$purple]),
								_List_fromArray(
									[
										$elm$html$Html$text(field)
									])),
							A2(
								$elm$core$List$cons,
								$elm$html$Html$text(' = '),
								A2(
									$elm$core$List$cons,
									A2($elm$html$Html$span, _List_Nil, valueHtmls),
									otherHtmls)))));
			}
		}
	});
var $elm$browser$Debugger$Expando$view = F2(
	function (maybeKey, expando) {
		switch (expando.$) {
			case 'S':
				var stringRep = expando.a;
				return A2(
					$elm$html$Html$div,
					$elm$browser$Debugger$Expando$leftPad(maybeKey),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Nothing,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$red]),
								_List_fromArray(
									[
										$elm$html$Html$text(stringRep)
									]))
							])));
			case 'Primitive':
				var stringRep = expando.a;
				return A2(
					$elm$html$Html$div,
					$elm$browser$Debugger$Expando$leftPad(maybeKey),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Nothing,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[$elm$browser$Debugger$Expando$blue]),
								_List_fromArray(
									[
										$elm$html$Html$text(stringRep)
									]))
							])));
			case 'Sequence':
				var seqType = expando.a;
				var isClosed = expando.b;
				var valueList = expando.c;
				return A4($elm$browser$Debugger$Expando$viewSequence, maybeKey, seqType, isClosed, valueList);
			case 'Dictionary':
				var isClosed = expando.a;
				var keyValuePairs = expando.b;
				return A3($elm$browser$Debugger$Expando$viewDictionary, maybeKey, isClosed, keyValuePairs);
			case 'Record':
				var isClosed = expando.a;
				var valueDict = expando.b;
				return A3($elm$browser$Debugger$Expando$viewRecord, maybeKey, isClosed, valueDict);
			default:
				var maybeName = expando.a;
				var isClosed = expando.b;
				var valueList = expando.c;
				return A4($elm$browser$Debugger$Expando$viewConstructor, maybeKey, maybeName, isClosed, valueList);
		}
	});
var $elm$browser$Debugger$Expando$viewConstructor = F4(
	function (maybeKey, maybeName, isClosed, valueList) {
		var tinyArgs = A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeL, $elm$core$Tuple$second, $elm$browser$Debugger$Expando$viewExtraTiny),
			valueList);
		var description = function () {
			var _v7 = _Utils_Tuple2(maybeName, tinyArgs);
			if (_v7.a.$ === 'Nothing') {
				if (!_v7.b.b) {
					var _v8 = _v7.a;
					return _List_fromArray(
						[
							$elm$html$Html$text('()')
						]);
				} else {
					var _v9 = _v7.a;
					var _v10 = _v7.b;
					var x = _v10.a;
					var xs = _v10.b;
					return A2(
						$elm$core$List$cons,
						$elm$html$Html$text('( '),
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$span, _List_Nil, x),
							A3(
								$elm$core$List$foldr,
								F2(
									function (args, rest) {
										return A2(
											$elm$core$List$cons,
											$elm$html$Html$text(', '),
											A2(
												$elm$core$List$cons,
												A2($elm$html$Html$span, _List_Nil, args),
												rest));
									}),
								_List_fromArray(
									[
										$elm$html$Html$text(' )')
									]),
								xs)));
				}
			} else {
				if (!_v7.b.b) {
					var name = _v7.a.a;
					return _List_fromArray(
						[
							$elm$html$Html$text(name)
						]);
				} else {
					var name = _v7.a.a;
					var _v11 = _v7.b;
					var x = _v11.a;
					var xs = _v11.b;
					return A2(
						$elm$core$List$cons,
						$elm$html$Html$text(name + ' '),
						A2(
							$elm$core$List$cons,
							A2($elm$html$Html$span, _List_Nil, x),
							A3(
								$elm$core$List$foldr,
								F2(
									function (args, rest) {
										return A2(
											$elm$core$List$cons,
											$elm$html$Html$text(' '),
											A2(
												$elm$core$List$cons,
												A2($elm$html$Html$span, _List_Nil, args),
												rest));
									}),
								_List_Nil,
								xs)));
				}
			}
		}();
		var _v4 = function () {
			if (!valueList.b) {
				return _Utils_Tuple2(
					$elm$core$Maybe$Nothing,
					A2($elm$html$Html$div, _List_Nil, _List_Nil));
			} else {
				if (!valueList.b.b) {
					var entry = valueList.a;
					switch (entry.$) {
						case 'S':
							return _Utils_Tuple2(
								$elm$core$Maybe$Nothing,
								A2($elm$html$Html$div, _List_Nil, _List_Nil));
						case 'Primitive':
							return _Utils_Tuple2(
								$elm$core$Maybe$Nothing,
								A2($elm$html$Html$div, _List_Nil, _List_Nil));
						case 'Sequence':
							var subValueList = entry.c;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewSequenceOpen(subValueList)));
						case 'Dictionary':
							var keyValuePairs = entry.b;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)));
						case 'Record':
							var record = entry.b;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewRecordOpen(record)));
						default:
							var subValueList = entry.c;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(isClosed),
								isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
									$elm$html$Html$map,
									A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, 0),
									$elm$browser$Debugger$Expando$viewConstructorOpen(subValueList)));
					}
				} else {
					return _Utils_Tuple2(
						$elm$core$Maybe$Just(isClosed),
						isClosed ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : $elm$browser$Debugger$Expando$viewConstructorOpen(valueList));
				}
			}
		}();
		var maybeIsClosed = _v4.a;
		var openHtml = _v4.b;
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3($elm$browser$Debugger$Expando$lineStarter, maybeKey, maybeIsClosed, description)),
					openHtml
				]));
	});
var $elm$browser$Debugger$Expando$viewConstructorEntry = F2(
	function (index, value) {
		return A2(
			$elm$html$Html$map,
			A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$None, index),
			A2(
				$elm$browser$Debugger$Expando$view,
				$elm$core$Maybe$Just(
					$elm$core$String$fromInt(index)),
				value));
	});
var $elm$browser$Debugger$Expando$viewConstructorOpen = function (valueList) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, valueList));
};
var $elm$browser$Debugger$Expando$viewDictionary = F3(
	function (maybeKey, isClosed, keyValuePairs) {
		var starter = 'Dict(' + ($elm$core$String$fromInt(
			$elm$core$List$length(keyValuePairs)) + ')');
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						_List_fromArray(
							[
								$elm$html$Html$text(starter)
							]))),
					isClosed ? $elm$html$Html$text('') : $elm$browser$Debugger$Expando$viewDictionaryOpen(keyValuePairs)
				]));
	});
var $elm$browser$Debugger$Expando$viewDictionaryEntry = F2(
	function (index, _v2) {
		var key = _v2.a;
		var value = _v2.b;
		switch (key.$) {
			case 'S':
				var stringRep = key.a;
				return A2(
					$elm$html$Html$map,
					A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
					A2(
						$elm$browser$Debugger$Expando$view,
						$elm$core$Maybe$Just(stringRep),
						value));
			case 'Primitive':
				var stringRep = key.a;
				return A2(
					$elm$html$Html$map,
					A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
					A2(
						$elm$browser$Debugger$Expando$view,
						$elm$core$Maybe$Just(stringRep),
						value));
			default:
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$map,
							A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Key, index),
							A2(
								$elm$browser$Debugger$Expando$view,
								$elm$core$Maybe$Just('key'),
								key)),
							A2(
							$elm$html$Html$map,
							A2($elm$browser$Debugger$Expando$Index, $elm$browser$Debugger$Expando$Value, index),
							A2(
								$elm$browser$Debugger$Expando$view,
								$elm$core$Maybe$Just('value'),
								value))
						]));
		}
	});
var $elm$browser$Debugger$Expando$viewDictionaryOpen = function (keyValuePairs) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewDictionaryEntry, keyValuePairs));
};
var $elm$browser$Debugger$Expando$viewRecord = F3(
	function (maybeKey, isClosed, record) {
		var _v1 = isClosed ? _Utils_Tuple3(
			$elm$browser$Debugger$Expando$viewTinyRecord(record).b,
			$elm$html$Html$text(''),
			$elm$html$Html$text('')) : _Utils_Tuple3(
			_List_fromArray(
				[
					$elm$html$Html$text('{')
				]),
			$elm$browser$Debugger$Expando$viewRecordOpen(record),
			A2(
				$elm$html$Html$div,
				$elm$browser$Debugger$Expando$leftPad(
					$elm$core$Maybe$Just(_Utils_Tuple0)),
				_List_fromArray(
					[
						$elm$html$Html$text('}')
					])));
		var start = _v1.a;
		var middle = _v1.b;
		var end = _v1.c;
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						start)),
					middle,
					end
				]));
	});
var $elm$browser$Debugger$Expando$viewRecordEntry = function (_v0) {
	var field = _v0.a;
	var value = _v0.b;
	return A2(
		$elm$html$Html$map,
		$elm$browser$Debugger$Expando$Field(field),
		A2(
			$elm$browser$Debugger$Expando$view,
			$elm$core$Maybe$Just(field),
			value));
};
var $elm$browser$Debugger$Expando$viewRecordOpen = function (record) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2(
			$elm$core$List$map,
			$elm$browser$Debugger$Expando$viewRecordEntry,
			$elm$core$Dict$toList(record)));
};
var $elm$browser$Debugger$Expando$viewSequence = F4(
	function (maybeKey, seqType, isClosed, valueList) {
		var starter = A2(
			$elm$browser$Debugger$Expando$seqTypeToString,
			$elm$core$List$length(valueList),
			seqType);
		return A2(
			$elm$html$Html$div,
			$elm$browser$Debugger$Expando$leftPad(maybeKey),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($elm$browser$Debugger$Expando$Toggle)
						]),
					A3(
						$elm$browser$Debugger$Expando$lineStarter,
						maybeKey,
						$elm$core$Maybe$Just(isClosed),
						_List_fromArray(
							[
								$elm$html$Html$text(starter)
							]))),
					isClosed ? $elm$html$Html$text('') : $elm$browser$Debugger$Expando$viewSequenceOpen(valueList)
				]));
	});
var $elm$browser$Debugger$Expando$viewSequenceOpen = function (values) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2($elm$core$List$indexedMap, $elm$browser$Debugger$Expando$viewConstructorEntry, values));
};
var $elm$browser$Debugger$Main$viewExpando = F3(
	function (expandoMsg, expandoModel, layout) {
		var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
		var _v0 = $elm$browser$Debugger$Main$toExpandoPercents(layout);
		var w = _v0.a;
		var h = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'block'),
					A2($elm$html$Html$Attributes$style, 'width', 'calc(' + (w + ' - 4em)')),
					A2($elm$html$Html$Attributes$style, 'height', 'calc(' + (h + ' - 4em)')),
					A2($elm$html$Html$Attributes$style, 'padding', '2em'),
					A2($elm$html$Html$Attributes$style, 'margin', '0'),
					A2($elm$html$Html$Attributes$style, 'overflow', 'auto'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', block),
					A2($elm$html$Html$Attributes$style, '-webkit-user-select', block),
					A2($elm$html$Html$Attributes$style, '-moz-user-select', block),
					A2($elm$html$Html$Attributes$style, '-ms-user-select', block),
					A2($elm$html$Html$Attributes$style, 'user-select', block)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#ccc'),
							A2($elm$html$Html$Attributes$style, 'padding', '0 0 1em 0')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('-- MESSAGE')
						])),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$TweakExpandoMsg,
					A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoMsg)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'color', '#ccc'),
							A2($elm$html$Html$Attributes$style, 'padding', '1em 0')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('-- MODEL')
						])),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$TweakExpandoModel,
					A2($elm$browser$Debugger$Expando$view, $elm$core$Maybe$Nothing, expandoModel))
				]));
	});
var $elm$browser$Debugger$Main$Jump = function (a) {
	return {$: 'Jump', a: a};
};
var $elm$virtual_dom$VirtualDom$lazy = _VirtualDom_lazy;
var $elm$html$Html$Lazy$lazy = $elm$virtual_dom$VirtualDom$lazy;
var $elm$browser$Debugger$Main$toHistoryPercents = function (layout) {
	if (layout.$ === 'Horizontal') {
		var x = layout.b;
		return _Utils_Tuple2(
			$elm$browser$Debugger$Main$toPercent(x),
			'100%');
	} else {
		var y = layout.c;
		return _Utils_Tuple2(
			'100%',
			$elm$browser$Debugger$Main$toPercent(1 - y));
	}
};
var $elm$virtual_dom$VirtualDom$lazy3 = _VirtualDom_lazy3;
var $elm$html$Html$Lazy$lazy3 = $elm$virtual_dom$VirtualDom$lazy3;
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$browser$Debugger$History$idForMessageIndex = function (index) {
	return 'msg-' + $elm$core$String$fromInt(index);
};
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $elm$browser$Debugger$History$viewMessage = F3(
	function (currentIndex, index, msg) {
		var messageName = _Debugger_messageToString(msg);
		var className = _Utils_eq(currentIndex, index) ? 'elm-debugger-entry elm-debugger-entry-selected' : 'elm-debugger-entry';
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id(
					$elm$browser$Debugger$History$idForMessageIndex(index)),
					$elm$html$Html$Attributes$class(className),
					$elm$html$Html$Events$onClick(index)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$title(messageName),
							$elm$html$Html$Attributes$class('elm-debugger-entry-content')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(messageName)
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('elm-debugger-entry-index')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(index))
						]))
				]));
	});
var $elm$browser$Debugger$History$consMsg = F3(
	function (currentIndex, msg, _v0) {
		var index = _v0.a;
		var rest = _v0.b;
		return _Utils_Tuple2(
			index + 1,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$elm$core$String$fromInt(index),
					A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewMessage, currentIndex, index, msg)),
				rest));
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$browser$Debugger$History$maxSnapshotSize = 31;
var $elm$browser$Debugger$History$showMoreButton = function (numMessages) {
	var nextIndex = (numMessages - 1) - ($elm$browser$Debugger$History$maxSnapshotSize * 2);
	var labelText = 'View more messages';
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('elm-debugger-entry'),
				$elm$html$Html$Events$onClick(nextIndex)
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$title(labelText),
						$elm$html$Html$Attributes$class('elm-debugger-entry-content')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(labelText)
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elm-debugger-entry-index')
					]),
				_List_Nil)
			]));
};
var $elm$browser$Debugger$History$styles = A3(
	$elm$html$Html$node,
	'style',
	_List_Nil,
	_List_fromArray(
		[
			$elm$html$Html$text('\n\n.elm-debugger-entry {\n  cursor: pointer;\n  width: 100%;\n  box-sizing: border-box;\n  padding: 8px;\n}\n\n.elm-debugger-entry:hover {\n  background-color: rgb(41, 41, 41);\n}\n\n.elm-debugger-entry-selected, .elm-debugger-entry-selected:hover {\n  background-color: rgb(10, 10, 10);\n}\n\n.elm-debugger-entry-content {\n  width: calc(100% - 40px);\n  padding: 0 5px;\n  box-sizing: border-box;\n  text-overflow: ellipsis;\n  white-space: nowrap;\n  overflow: hidden;\n  display: inline-block;\n}\n\n.elm-debugger-entry-index {\n  color: #666;\n  width: 40px;\n  text-align: right;\n  display: block;\n  float: right;\n}\n\n')
		]));
var $elm$core$Basics$ge = _Utils_ge;
var $elm$browser$Debugger$History$viewSnapshot = F3(
	function (selectedIndex, index, _v0) {
		var messages = _v0.messages;
		return A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_Nil,
			A3(
				$elm$core$Array$foldr,
				$elm$browser$Debugger$History$consMsg(selectedIndex),
				_Utils_Tuple2(index, _List_Nil),
				messages).b);
	});
var $elm$browser$Debugger$History$consSnapshot = F3(
	function (selectedIndex, snapshot, _v0) {
		var index = _v0.a;
		var rest = _v0.b;
		var nextIndex = index + $elm$core$Array$length(snapshot.messages);
		var selectedIndexHelp = ((_Utils_cmp(nextIndex, selectedIndex) > 0) && (_Utils_cmp(selectedIndex, index) > -1)) ? selectedIndex : (-1);
		return _Utils_Tuple2(
			nextIndex,
			A2(
				$elm$core$List$cons,
				A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewSnapshot, selectedIndexHelp, index, snapshot),
				rest));
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $elm$browser$Debugger$History$viewAllSnapshots = F3(
	function (selectedIndex, startIndex, snapshots) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			A3(
				$elm$core$Array$foldl,
				$elm$browser$Debugger$History$consSnapshot(selectedIndex),
				_Utils_Tuple2(startIndex, _List_Nil),
				snapshots).b);
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $elm$browser$Debugger$History$viewRecentSnapshots = F3(
	function (selectedIndex, recentMessagesNum, snapshots) {
		var messagesToFill = $elm$browser$Debugger$History$maxSnapshotSize - recentMessagesNum;
		var arrayLength = $elm$core$Array$length(snapshots);
		var snapshotsToRender = function () {
			var _v0 = _Utils_Tuple2(
				A2($elm$core$Array$get, arrayLength - 2, snapshots),
				A2($elm$core$Array$get, arrayLength - 1, snapshots));
			if ((_v0.a.$ === 'Just') && (_v0.b.$ === 'Just')) {
				var fillerSnapshot = _v0.a.a;
				var recentSnapshot = _v0.b.a;
				return $elm$core$Array$fromList(
					_List_fromArray(
						[
							{
							messages: A3($elm$core$Array$slice, 0, messagesToFill, fillerSnapshot.messages),
							model: fillerSnapshot.model
						},
							recentSnapshot
						]));
			} else {
				return snapshots;
			}
		}();
		var startingIndex = ((arrayLength * $elm$browser$Debugger$History$maxSnapshotSize) - $elm$browser$Debugger$History$maxSnapshotSize) - messagesToFill;
		return A3($elm$browser$Debugger$History$viewAllSnapshots, selectedIndex, startingIndex, snapshotsToRender);
	});
var $elm$browser$Debugger$History$view = F2(
	function (maybeIndex, _v0) {
		var snapshots = _v0.snapshots;
		var recent = _v0.recent;
		var numMessages = _v0.numMessages;
		var recentMessageStartIndex = numMessages - recent.numMessages;
		var index = A2($elm$core$Maybe$withDefault, -1, maybeIndex);
		var newStuff = A3(
			$elm$html$Html$Keyed$node,
			'div',
			_List_Nil,
			A3(
				$elm$core$List$foldr,
				$elm$browser$Debugger$History$consMsg(index),
				_Utils_Tuple2(recentMessageStartIndex, _List_Nil),
				recent.messages).b);
		var onlyRenderRecentMessages = (!_Utils_eq(index, -1)) || ($elm$core$Array$length(snapshots) < 2);
		var oldStuff = onlyRenderRecentMessages ? A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewAllSnapshots, index, 0, snapshots) : A4($elm$html$Html$Lazy$lazy3, $elm$browser$Debugger$History$viewRecentSnapshots, index, recent.numMessages, snapshots);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$id('elm-debugger-sidebar'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
					A2($elm$html$Html$Attributes$style, 'height', 'calc(100% - 72px)')
				]),
			A2(
				$elm$core$List$cons,
				$elm$browser$Debugger$History$styles,
				A2(
					$elm$core$List$cons,
					newStuff,
					A2(
						$elm$core$List$cons,
						oldStuff,
						onlyRenderRecentMessages ? _List_Nil : _List_fromArray(
							[
								$elm$browser$Debugger$History$showMoreButton(numMessages)
							])))));
	});
var $elm$browser$Debugger$Main$SwapLayout = {$: 'SwapLayout'};
var $elm$browser$Debugger$Main$toHistoryIcon = function (layout) {
	if (layout.$ === 'Horizontal') {
		return 'M13 1a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M13 3h-10a1 1 0 0 0-1 1v5h12v-5a1 1 0 0 0-1-1z M14 10h-12v2a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1z';
	} else {
		return 'M0 4a3 3 0 0 1 3-3h10a3 3 0 0 1 3 3v8a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3z M2 4v8a1 1 0 0 0 1 1h2v-10h-2a1 1 0 0 0-1 1z M6 3v10h7a1 1 0 0 0 1-1v-8a1 1 0 0 0-1-1z';
	}
};
var $elm$browser$Debugger$Main$icon = function (path) {
	return A4(
		$elm$virtual_dom$VirtualDom$nodeNS,
		'http://www.w3.org/2000/svg',
		'svg',
		_List_fromArray(
			[
				A2($elm$virtual_dom$VirtualDom$attribute, 'viewBox', '0 0 16 16'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'xmlns', 'http://www.w3.org/2000/svg'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'fill', 'currentColor'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'width', '16px'),
				A2($elm$virtual_dom$VirtualDom$attribute, 'height', '16px')
			]),
		_List_fromArray(
			[
				A4(
				$elm$virtual_dom$VirtualDom$nodeNS,
				'http://www.w3.org/2000/svg',
				'path',
				_List_fromArray(
					[
						A2($elm$virtual_dom$VirtualDom$attribute, 'd', path)
					]),
				_List_Nil)
			]));
};
var $elm$browser$Debugger$Main$viewHistoryButton = F3(
	function (label, msg, path) {
		return A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'background', 'none'),
					A2($elm$html$Html$Attributes$style, 'border', 'none'),
					A2($elm$html$Html$Attributes$style, 'color', 'inherit'),
					A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
					$elm$html$Html$Events$onClick(msg)
				]),
			_List_fromArray(
				[
					$elm$browser$Debugger$Main$icon(path),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'padding-left', '6px')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						]))
				]));
	});
var $elm$browser$Debugger$Main$viewHistoryOptions = function (layout) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'width', '100%'),
				A2($elm$html$Html$Attributes$style, 'height', '36px'),
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
				A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
				A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between'),
				A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
			]),
		_List_fromArray(
			[
				A3(
				$elm$browser$Debugger$Main$viewHistoryButton,
				'Swap Layout',
				$elm$browser$Debugger$Main$SwapLayout,
				$elm$browser$Debugger$Main$toHistoryIcon(layout)),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
						A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
						A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between')
					]),
				_List_fromArray(
					[
						A3($elm$browser$Debugger$Main$viewHistoryButton, 'Import', $elm$browser$Debugger$Main$Import, 'M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M10 2a1 1 0 0 0 -2 0v6a1 1 0 0 0 1 1h6a1 1 0 0 0 0-2h-3.586l4.293-4.293a1 1 0 0 0-1.414-1.414l-4.293 4.293z'),
						A3($elm$browser$Debugger$Main$viewHistoryButton, 'Export', $elm$browser$Debugger$Main$Export, 'M5 1a1 1 0 0 1 0 2h-2a1 1 0 0 0-1 1v8a1 1 0 0 0 1 1h10a1 1 0 0 0 1-1 a1 1 0 0 1 2 0a3 3 0 0 1-3 3h-10a3 3 0 0 1-3-3v-8a3 3 0 0 1 3-3z M9 3a1 1 0 1 1 0-2h6a1 1 0 0 1 1 1v6a1 1 0 1 1-2 0v-3.586l-5.293 5.293 a1 1 0 0 1-1.414-1.414l5.293 -5.293z')
					]))
			]));
};
var $elm$browser$Debugger$Main$SliderJump = function (a) {
	return {$: 'SliderJump', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$browser$Debugger$Main$isPlaying = function (maybeIndex) {
	if (maybeIndex.$ === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var $elm$html$Html$Attributes$max = $elm$html$Html$Attributes$stringProperty('max');
var $elm$html$Html$Attributes$min = $elm$html$Html$Attributes$stringProperty('min');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$core$String$toInt = _String_toInt;
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $elm$browser$Debugger$Main$viewPlayButton = function (playing) {
	return A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'background', '#1293D8'),
				A2($elm$html$Html$Attributes$style, 'border', 'none'),
				A2($elm$html$Html$Attributes$style, 'color', 'white'),
				A2($elm$html$Html$Attributes$style, 'cursor', 'pointer'),
				A2($elm$html$Html$Attributes$style, 'width', '36px'),
				A2($elm$html$Html$Attributes$style, 'height', '36px'),
				$elm$html$Html$Events$onClick($elm$browser$Debugger$Main$Resume)
			]),
		_List_fromArray(
			[
				playing ? $elm$browser$Debugger$Main$icon('M2 2h4v12h-4v-12z M10 2h4v12h-4v-12z') : $elm$browser$Debugger$Main$icon('M2 2l12 7l-12 7z')
			]));
};
var $elm$browser$Debugger$Main$viewHistorySlider = F2(
	function (history, maybeIndex) {
		var lastIndex = $elm$browser$Debugger$History$size(history) - 1;
		var selectedIndex = A2($elm$core$Maybe$withDefault, lastIndex, maybeIndex);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'row'),
					A2($elm$html$Html$Attributes$style, 'align-items', 'center'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '36px'),
					A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(50, 50, 50)')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Lazy$lazy,
					$elm$browser$Debugger$Main$viewPlayButton,
					$elm$browser$Debugger$Main$isPlaying(maybeIndex)),
					A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('range'),
							A2($elm$html$Html$Attributes$style, 'width', 'calc(100% - 56px)'),
							A2($elm$html$Html$Attributes$style, 'height', '36px'),
							A2($elm$html$Html$Attributes$style, 'margin', '0 10px'),
							$elm$html$Html$Attributes$min('0'),
							$elm$html$Html$Attributes$max(
							$elm$core$String$fromInt(lastIndex)),
							$elm$html$Html$Attributes$value(
							$elm$core$String$fromInt(selectedIndex)),
							$elm$html$Html$Events$onInput(
							A2(
								$elm$core$Basics$composeR,
								$elm$core$String$toInt,
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$withDefault(lastIndex),
									$elm$browser$Debugger$Main$SliderJump)))
						]),
					_List_Nil)
				]));
	});
var $elm$browser$Debugger$Main$viewHistory = F3(
	function (maybeIndex, history, layout) {
		var block = $elm$browser$Debugger$Main$toMouseBlocker(layout);
		var _v0 = $elm$browser$Debugger$Main$toHistoryPercents(layout);
		var w = _v0.a;
		var h = _v0.b;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'width', w),
					A2($elm$html$Html$Attributes$style, 'height', h),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2($elm$html$Html$Attributes$style, 'flex-direction', 'column'),
					A2($elm$html$Html$Attributes$style, 'color', '#DDDDDD'),
					A2($elm$html$Html$Attributes$style, 'background-color', 'rgb(61, 61, 61)'),
					A2($elm$html$Html$Attributes$style, 'pointer-events', block),
					A2($elm$html$Html$Attributes$style, 'user-select', block)
				]),
			_List_fromArray(
				[
					A2($elm$browser$Debugger$Main$viewHistorySlider, history, maybeIndex),
					A2(
					$elm$html$Html$map,
					$elm$browser$Debugger$Main$Jump,
					A2($elm$browser$Debugger$History$view, maybeIndex, history)),
					A2($elm$html$Html$Lazy$lazy, $elm$browser$Debugger$Main$viewHistoryOptions, layout)
				]));
	});
var $elm$browser$Debugger$Main$popoutView = function (model) {
	var maybeIndex = function () {
		var _v0 = model.state;
		if (_v0.$ === 'Running') {
			return $elm$core$Maybe$Nothing;
		} else {
			var index = _v0.a;
			return $elm$core$Maybe$Just(index);
		}
	}();
	var historyToRender = $elm$browser$Debugger$Main$cachedHistory(model);
	return A3(
		$elm$html$Html$node,
		'body',
		_Utils_ap(
			$elm$browser$Debugger$Main$toDragListeners(model.layout),
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'margin', '0'),
					A2($elm$html$Html$Attributes$style, 'padding', '0'),
					A2($elm$html$Html$Attributes$style, 'width', '100%'),
					A2($elm$html$Html$Attributes$style, 'height', '100%'),
					A2($elm$html$Html$Attributes$style, 'font-family', 'monospace'),
					A2($elm$html$Html$Attributes$style, 'display', 'flex'),
					A2(
					$elm$html$Html$Attributes$style,
					'flex-direction',
					$elm$browser$Debugger$Main$toFlexDirection(model.layout))
				])),
		_List_fromArray(
			[
				A3($elm$browser$Debugger$Main$viewHistory, maybeIndex, historyToRender, model.layout),
				$elm$browser$Debugger$Main$viewDragZone(model.layout),
				A3($elm$browser$Debugger$Main$viewExpando, model.expandoMsg, model.expandoModel, model.layout)
			]));
};
var $elm$browser$Debugger$Overlay$BlockAll = {$: 'BlockAll'};
var $elm$browser$Debugger$Overlay$toBlockerType = F2(
	function (isPaused, state) {
		switch (state.$) {
			case 'None':
				return isPaused ? $elm$browser$Debugger$Overlay$BlockAll : $elm$browser$Debugger$Overlay$BlockNone;
			case 'BadMetadata':
				return $elm$browser$Debugger$Overlay$BlockMost;
			case 'BadImport':
				return $elm$browser$Debugger$Overlay$BlockMost;
			default:
				return $elm$browser$Debugger$Overlay$BlockMost;
		}
	});
var $elm$browser$Debugger$Main$toBlockerType = function (model) {
	return A2(
		$elm$browser$Debugger$Overlay$toBlockerType,
		$elm$browser$Debugger$Main$isPaused(model.state),
		model.overlay);
};
var $elm$browser$Debugger$Main$Horizontal = F3(
	function (a, b, c) {
		return {$: 'Horizontal', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Main$Running = function (a) {
	return {$: 'Running', a: a};
};
var $elm$browser$Debugger$Main$Static = {$: 'Static'};
var $elm$browser$Debugger$Metadata$Error = F2(
	function (message, problems) {
		return {message: message, problems: problems};
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$browser$Debugger$Metadata$Metadata = F2(
	function (versions, types) {
		return {types: types, versions: versions};
	});
var $elm$browser$Debugger$Metadata$Types = F3(
	function (message, aliases, unions) {
		return {aliases: aliases, message: message, unions: unions};
	});
var $elm$browser$Debugger$Metadata$Alias = F2(
	function (args, tipe) {
		return {args: args, tipe: tipe};
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$browser$Debugger$Metadata$decodeAlias = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Alias,
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $elm$browser$Debugger$Metadata$Union = F2(
	function (args, tags) {
		return {args: args, tags: tags};
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$browser$Debugger$Metadata$decodeUnion = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Union,
	A2(
		$elm$json$Json$Decode$field,
		'args',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'tags',
		$elm$json$Json$Decode$dict(
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))));
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$browser$Debugger$Metadata$decodeTypes = A4(
	$elm$json$Json$Decode$map3,
	$elm$browser$Debugger$Metadata$Types,
	A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'aliases',
		$elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeAlias)),
	A2(
		$elm$json$Json$Decode$field,
		'unions',
		$elm$json$Json$Decode$dict($elm$browser$Debugger$Metadata$decodeUnion)));
var $elm$browser$Debugger$Metadata$Versions = function (elm) {
	return {elm: elm};
};
var $elm$browser$Debugger$Metadata$decodeVersions = A2(
	$elm$json$Json$Decode$map,
	$elm$browser$Debugger$Metadata$Versions,
	A2($elm$json$Json$Decode$field, 'elm', $elm$json$Json$Decode$string));
var $elm$browser$Debugger$Metadata$decoder = A3(
	$elm$json$Json$Decode$map2,
	$elm$browser$Debugger$Metadata$Metadata,
	A2($elm$json$Json$Decode$field, 'versions', $elm$browser$Debugger$Metadata$decodeVersions),
	A2($elm$json$Json$Decode$field, 'types', $elm$browser$Debugger$Metadata$decodeTypes));
var $elm$browser$Debugger$Metadata$ProblemType = F2(
	function (name, problems) {
		return {name: name, problems: problems};
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$String$contains = _String_contains;
var $elm$browser$Debugger$Metadata$hasProblem = F2(
	function (tipe, _v0) {
		var problem = _v0.a;
		var token = _v0.b;
		return A2($elm$core$String$contains, token, tipe) ? $elm$core$Maybe$Just(problem) : $elm$core$Maybe$Nothing;
	});
var $elm$browser$Debugger$Metadata$Decoder = {$: 'Decoder'};
var $elm$browser$Debugger$Metadata$Function = {$: 'Function'};
var $elm$browser$Debugger$Metadata$Process = {$: 'Process'};
var $elm$browser$Debugger$Metadata$Program = {$: 'Program'};
var $elm$browser$Debugger$Metadata$Request = {$: 'Request'};
var $elm$browser$Debugger$Metadata$Socket = {$: 'Socket'};
var $elm$browser$Debugger$Metadata$Task = {$: 'Task'};
var $elm$browser$Debugger$Metadata$VirtualDom = {$: 'VirtualDom'};
var $elm$browser$Debugger$Metadata$problemTable = _List_fromArray(
	[
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Function, '->'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Decoder, 'Json.Decode.Decoder'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Task, 'Task.Task'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Process, 'Process.Id'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Socket, 'WebSocket.LowLevel.WebSocket'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Request, 'Http.Request'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$Program, 'Platform.Program'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, 'VirtualDom.Node'),
		_Utils_Tuple2($elm$browser$Debugger$Metadata$VirtualDom, 'VirtualDom.Attribute')
	]);
var $elm$browser$Debugger$Metadata$findProblems = function (tipe) {
	return A2(
		$elm$core$List$filterMap,
		$elm$browser$Debugger$Metadata$hasProblem(tipe),
		$elm$browser$Debugger$Metadata$problemTable);
};
var $elm$browser$Debugger$Metadata$collectBadAliases = F3(
	function (name, _v0, list) {
		var tipe = _v0.tipe;
		var _v1 = $elm$browser$Debugger$Metadata$findProblems(tipe);
		if (!_v1.b) {
			return list;
		} else {
			var problems = _v1;
			return A2(
				$elm$core$List$cons,
				A2($elm$browser$Debugger$Metadata$ProblemType, name, problems),
				list);
		}
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm$browser$Debugger$Metadata$collectBadUnions = F3(
	function (name, _v0, list) {
		var tags = _v0.tags;
		var _v1 = A2(
			$elm$core$List$concatMap,
			$elm$browser$Debugger$Metadata$findProblems,
			$elm$core$List$concat(
				$elm$core$Dict$values(tags)));
		if (!_v1.b) {
			return list;
		} else {
			var problems = _v1;
			return A2(
				$elm$core$List$cons,
				A2($elm$browser$Debugger$Metadata$ProblemType, name, problems),
				list);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$browser$Debugger$Metadata$isPortable = function (_v0) {
	var types = _v0.types;
	var badAliases = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadAliases, _List_Nil, types.aliases);
	var _v1 = A3($elm$core$Dict$foldl, $elm$browser$Debugger$Metadata$collectBadUnions, badAliases, types.unions);
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var problems = _v1;
		return $elm$core$Maybe$Just(
			A2($elm$browser$Debugger$Metadata$Error, types.message, problems));
	}
};
var $elm$browser$Debugger$Metadata$decode = function (value) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $elm$browser$Debugger$Metadata$decoder, value);
	if (_v0.$ === 'Err') {
		return $elm$core$Result$Err(
			A2($elm$browser$Debugger$Metadata$Error, 'The compiler is generating bad metadata. This is a compiler bug!', _List_Nil));
	} else {
		var metadata = _v0.a;
		var _v1 = $elm$browser$Debugger$Metadata$isPortable(metadata);
		if (_v1.$ === 'Nothing') {
			return $elm$core$Result$Ok(metadata);
		} else {
			var error = _v1.a;
			return $elm$core$Result$Err(error);
		}
	}
};
var $elm$browser$Debugger$History$History = F3(
	function (snapshots, recent, numMessages) {
		return {numMessages: numMessages, recent: recent, snapshots: snapshots};
	});
var $elm$browser$Debugger$History$RecentHistory = F3(
	function (model, messages, numMessages) {
		return {messages: messages, model: model, numMessages: numMessages};
	});
var $elm$browser$Debugger$History$empty = function (model) {
	return A3(
		$elm$browser$Debugger$History$History,
		$elm$core$Array$empty,
		A3($elm$browser$Debugger$History$RecentHistory, model, _List_Nil, 0),
		0);
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$browser$Debugger$Expando$initHelp = F2(
	function (isOuter, expando) {
		switch (expando.$) {
			case 'S':
				return expando;
			case 'Primitive':
				return expando;
			case 'Sequence':
				var seqType = expando.a;
				var isClosed = expando.b;
				var items = expando.c;
				return isOuter ? A3(
					$elm$browser$Debugger$Expando$Sequence,
					seqType,
					false,
					A2(
						$elm$core$List$map,
						$elm$browser$Debugger$Expando$initHelp(false),
						items)) : (($elm$core$List$length(items) <= 8) ? A3($elm$browser$Debugger$Expando$Sequence, seqType, false, items) : expando);
			case 'Dictionary':
				var isClosed = expando.a;
				var keyValuePairs = expando.b;
				return isOuter ? A2(
					$elm$browser$Debugger$Expando$Dictionary,
					false,
					A2(
						$elm$core$List$map,
						function (_v1) {
							var k = _v1.a;
							var v = _v1.b;
							return _Utils_Tuple2(
								k,
								A2($elm$browser$Debugger$Expando$initHelp, false, v));
						},
						keyValuePairs)) : (($elm$core$List$length(keyValuePairs) <= 8) ? A2($elm$browser$Debugger$Expando$Dictionary, false, keyValuePairs) : expando);
			case 'Record':
				var isClosed = expando.a;
				var entries = expando.b;
				return isOuter ? A2(
					$elm$browser$Debugger$Expando$Record,
					false,
					A2(
						$elm$core$Dict$map,
						F2(
							function (_v2, v) {
								return A2($elm$browser$Debugger$Expando$initHelp, false, v);
							}),
						entries)) : (($elm$core$Dict$size(entries) <= 4) ? A2($elm$browser$Debugger$Expando$Record, false, entries) : expando);
			default:
				var maybeName = expando.a;
				var isClosed = expando.b;
				var args = expando.c;
				return isOuter ? A3(
					$elm$browser$Debugger$Expando$Constructor,
					maybeName,
					false,
					A2(
						$elm$core$List$map,
						$elm$browser$Debugger$Expando$initHelp(false),
						args)) : (($elm$core$List$length(args) <= 4) ? A3($elm$browser$Debugger$Expando$Constructor, maybeName, false, args) : expando);
		}
	});
var $elm$browser$Debugger$Expando$init = function (value) {
	return A2(
		$elm$browser$Debugger$Expando$initHelp,
		true,
		_Debugger_init(value));
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $elm$browser$Debugger$Overlay$None = {$: 'None'};
var $elm$browser$Debugger$Overlay$none = $elm$browser$Debugger$Overlay$None;
var $elm$browser$Debugger$Main$wrapInit = F4(
	function (metadata, popout, init, flags) {
		var _v0 = init(flags);
		var userModel = _v0.a;
		var userCommands = _v0.b;
		return _Utils_Tuple2(
			{
				expandoModel: $elm$browser$Debugger$Expando$init(userModel),
				expandoMsg: $elm$browser$Debugger$Expando$init(_Utils_Tuple0),
				history: $elm$browser$Debugger$History$empty(userModel),
				layout: A3($elm$browser$Debugger$Main$Horizontal, $elm$browser$Debugger$Main$Static, 0.3, 0.5),
				metadata: $elm$browser$Debugger$Metadata$decode(metadata),
				overlay: $elm$browser$Debugger$Overlay$none,
				popout: popout,
				state: $elm$browser$Debugger$Main$Running(userModel)
			},
			A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCommands));
	});
var $elm$browser$Debugger$Main$getLatestModel = function (state) {
	if (state.$ === 'Running') {
		var model = state.a;
		return model;
	} else {
		var model = state.c;
		return model;
	}
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $elm$browser$Debugger$Main$wrapSubs = F2(
	function (subscriptions, model) {
		return A2(
			$elm$core$Platform$Sub$map,
			$elm$browser$Debugger$Main$UserMsg,
			subscriptions(
				$elm$browser$Debugger$Main$getLatestModel(model.state)));
	});
var $elm$browser$Debugger$Main$Moving = {$: 'Moving'};
var $elm$browser$Debugger$Main$Paused = F5(
	function (a, b, c, d, e) {
		return {$: 'Paused', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$browser$Debugger$History$Snapshot = F2(
	function (model, messages) {
		return {messages: messages, model: model};
	});
var $elm$browser$Debugger$History$addRecent = F3(
	function (msg, newModel, _v0) {
		var model = _v0.model;
		var messages = _v0.messages;
		var numMessages = _v0.numMessages;
		return _Utils_eq(numMessages, $elm$browser$Debugger$History$maxSnapshotSize) ? _Utils_Tuple2(
			$elm$core$Maybe$Just(
				A2(
					$elm$browser$Debugger$History$Snapshot,
					model,
					$elm$core$Array$fromList(messages))),
			A3(
				$elm$browser$Debugger$History$RecentHistory,
				newModel,
				_List_fromArray(
					[msg]),
				1)) : _Utils_Tuple2(
			$elm$core$Maybe$Nothing,
			A3(
				$elm$browser$Debugger$History$RecentHistory,
				model,
				A2($elm$core$List$cons, msg, messages),
				numMessages + 1));
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $elm$browser$Debugger$History$add = F3(
	function (msg, model, _v0) {
		var snapshots = _v0.snapshots;
		var recent = _v0.recent;
		var numMessages = _v0.numMessages;
		var _v1 = A3($elm$browser$Debugger$History$addRecent, msg, model, recent);
		if (_v1.a.$ === 'Just') {
			var snapshot = _v1.a.a;
			var newRecent = _v1.b;
			return A3(
				$elm$browser$Debugger$History$History,
				A2($elm$core$Array$push, snapshot, snapshots),
				newRecent,
				numMessages + 1);
		} else {
			var _v2 = _v1.a;
			var newRecent = _v1.b;
			return A3($elm$browser$Debugger$History$History, snapshots, newRecent, numMessages + 1);
		}
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$browser$Debugger$Overlay$BadImport = function (a) {
	return {$: 'BadImport', a: a};
};
var $elm$browser$Debugger$Overlay$RiskyImport = F2(
	function (a, b) {
		return {$: 'RiskyImport', a: a, b: b};
	});
var $elm$browser$Debugger$Report$VersionChanged = F2(
	function (a, b) {
		return {$: 'VersionChanged', a: a, b: b};
	});
var $elm$browser$Debugger$Report$MessageChanged = F2(
	function (a, b) {
		return {$: 'MessageChanged', a: a, b: b};
	});
var $elm$browser$Debugger$Report$SomethingChanged = function (a) {
	return {$: 'SomethingChanged', a: a};
};
var $elm$browser$Debugger$Report$AliasChange = function (a) {
	return {$: 'AliasChange', a: a};
};
var $elm$browser$Debugger$Metadata$checkAlias = F4(
	function (name, old, _new, changes) {
		return (_Utils_eq(old.tipe, _new.tipe) && _Utils_eq(old.args, _new.args)) ? changes : A2(
			$elm$core$List$cons,
			$elm$browser$Debugger$Report$AliasChange(name),
			changes);
	});
var $elm$browser$Debugger$Report$UnionChange = F2(
	function (a, b) {
		return {$: 'UnionChange', a: a, b: b};
	});
var $elm$browser$Debugger$Metadata$addTag = F3(
	function (tag, _v0, changes) {
		return _Utils_update(
			changes,
			{
				added: A2($elm$core$List$cons, tag, changes.added)
			});
	});
var $elm$browser$Debugger$Metadata$checkTag = F4(
	function (tag, old, _new, changes) {
		return _Utils_eq(old, _new) ? changes : _Utils_update(
			changes,
			{
				changed: A2($elm$core$List$cons, tag, changes.changed)
			});
	});
var $elm$browser$Debugger$Report$TagChanges = F4(
	function (removed, changed, added, argsMatch) {
		return {added: added, argsMatch: argsMatch, changed: changed, removed: removed};
	});
var $elm$browser$Debugger$Report$emptyTagChanges = function (argsMatch) {
	return A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, argsMatch);
};
var $elm$browser$Debugger$Report$hasTagChanges = function (tagChanges) {
	return _Utils_eq(
		tagChanges,
		A4($elm$browser$Debugger$Report$TagChanges, _List_Nil, _List_Nil, _List_Nil, true));
};
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Debugger$Metadata$removeTag = F3(
	function (tag, _v0, changes) {
		return _Utils_update(
			changes,
			{
				removed: A2($elm$core$List$cons, tag, changes.removed)
			});
	});
var $elm$browser$Debugger$Metadata$checkUnion = F4(
	function (name, old, _new, changes) {
		var tagChanges = A6(
			$elm$core$Dict$merge,
			$elm$browser$Debugger$Metadata$removeTag,
			$elm$browser$Debugger$Metadata$checkTag,
			$elm$browser$Debugger$Metadata$addTag,
			old.tags,
			_new.tags,
			$elm$browser$Debugger$Report$emptyTagChanges(
				_Utils_eq(old.args, _new.args)));
		return $elm$browser$Debugger$Report$hasTagChanges(tagChanges) ? changes : A2(
			$elm$core$List$cons,
			A2($elm$browser$Debugger$Report$UnionChange, name, tagChanges),
			changes);
	});
var $elm$browser$Debugger$Metadata$ignore = F3(
	function (key, value, report) {
		return report;
	});
var $elm$browser$Debugger$Metadata$checkTypes = F2(
	function (old, _new) {
		return (!_Utils_eq(old.message, _new.message)) ? A2($elm$browser$Debugger$Report$MessageChanged, old.message, _new.message) : $elm$browser$Debugger$Report$SomethingChanged(
			A6(
				$elm$core$Dict$merge,
				$elm$browser$Debugger$Metadata$ignore,
				$elm$browser$Debugger$Metadata$checkUnion,
				$elm$browser$Debugger$Metadata$ignore,
				old.unions,
				_new.unions,
				A6($elm$core$Dict$merge, $elm$browser$Debugger$Metadata$ignore, $elm$browser$Debugger$Metadata$checkAlias, $elm$browser$Debugger$Metadata$ignore, old.aliases, _new.aliases, _List_Nil)));
	});
var $elm$browser$Debugger$Metadata$check = F2(
	function (old, _new) {
		return (!_Utils_eq(old.versions.elm, _new.versions.elm)) ? A2($elm$browser$Debugger$Report$VersionChanged, old.versions.elm, _new.versions.elm) : A2($elm$browser$Debugger$Metadata$checkTypes, old.types, _new.types);
	});
var $elm$browser$Debugger$Report$CorruptHistory = {$: 'CorruptHistory'};
var $elm$browser$Debugger$Overlay$corruptImport = $elm$browser$Debugger$Overlay$BadImport($elm$browser$Debugger$Report$CorruptHistory);
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$browser$Debugger$Report$Fine = {$: 'Fine'};
var $elm$browser$Debugger$Report$Impossible = {$: 'Impossible'};
var $elm$browser$Debugger$Report$Risky = {$: 'Risky'};
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$browser$Debugger$Report$some = function (list) {
	return !$elm$core$List$isEmpty(list);
};
var $elm$browser$Debugger$Report$evaluateChange = function (change) {
	if (change.$ === 'AliasChange') {
		return $elm$browser$Debugger$Report$Impossible;
	} else {
		var removed = change.b.removed;
		var changed = change.b.changed;
		var added = change.b.added;
		var argsMatch = change.b.argsMatch;
		return ((!argsMatch) || ($elm$browser$Debugger$Report$some(changed) || $elm$browser$Debugger$Report$some(removed))) ? $elm$browser$Debugger$Report$Impossible : ($elm$browser$Debugger$Report$some(added) ? $elm$browser$Debugger$Report$Risky : $elm$browser$Debugger$Report$Fine);
	}
};
var $elm$browser$Debugger$Report$worstCase = F2(
	function (status, statusList) {
		worstCase:
		while (true) {
			if (!statusList.b) {
				return status;
			} else {
				switch (statusList.a.$) {
					case 'Impossible':
						var _v1 = statusList.a;
						return $elm$browser$Debugger$Report$Impossible;
					case 'Risky':
						var _v2 = statusList.a;
						var rest = statusList.b;
						var $temp$status = $elm$browser$Debugger$Report$Risky,
							$temp$statusList = rest;
						status = $temp$status;
						statusList = $temp$statusList;
						continue worstCase;
					default:
						var _v3 = statusList.a;
						var rest = statusList.b;
						var $temp$status = status,
							$temp$statusList = rest;
						status = $temp$status;
						statusList = $temp$statusList;
						continue worstCase;
				}
			}
		}
	});
var $elm$browser$Debugger$Report$evaluate = function (report) {
	switch (report.$) {
		case 'CorruptHistory':
			return $elm$browser$Debugger$Report$Impossible;
		case 'VersionChanged':
			return $elm$browser$Debugger$Report$Impossible;
		case 'MessageChanged':
			return $elm$browser$Debugger$Report$Impossible;
		default:
			var changes = report.a;
			return A2(
				$elm$browser$Debugger$Report$worstCase,
				$elm$browser$Debugger$Report$Fine,
				A2($elm$core$List$map, $elm$browser$Debugger$Report$evaluateChange, changes));
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $elm$browser$Debugger$Overlay$uploadDecoder = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (x, y) {
			return _Utils_Tuple2(x, y);
		}),
	A2($elm$json$Json$Decode$field, 'metadata', $elm$browser$Debugger$Metadata$decoder),
	A2($elm$json$Json$Decode$field, 'history', $elm$json$Json$Decode$value));
var $elm$browser$Debugger$Overlay$assessImport = F2(
	function (metadata, jsonString) {
		var _v0 = A2($elm$json$Json$Decode$decodeString, $elm$browser$Debugger$Overlay$uploadDecoder, jsonString);
		if (_v0.$ === 'Err') {
			return $elm$core$Result$Err($elm$browser$Debugger$Overlay$corruptImport);
		} else {
			var _v1 = _v0.a;
			var foreignMetadata = _v1.a;
			var rawHistory = _v1.b;
			var report = A2($elm$browser$Debugger$Metadata$check, foreignMetadata, metadata);
			var _v2 = $elm$browser$Debugger$Report$evaluate(report);
			switch (_v2.$) {
				case 'Impossible':
					return $elm$core$Result$Err(
						$elm$browser$Debugger$Overlay$BadImport(report));
				case 'Risky':
					return $elm$core$Result$Err(
						A2($elm$browser$Debugger$Overlay$RiskyImport, report, rawHistory));
				default:
					return $elm$core$Result$Ok(rawHistory);
			}
		}
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$browser$Debugger$Overlay$close = F2(
	function (msg, state) {
		switch (state.$) {
			case 'None':
				return $elm$core$Maybe$Nothing;
			case 'BadMetadata':
				return $elm$core$Maybe$Nothing;
			case 'BadImport':
				return $elm$core$Maybe$Nothing;
			default:
				var rawHistory = state.b;
				if (msg.$ === 'Cancel') {
					return $elm$core$Maybe$Nothing;
				} else {
					return $elm$core$Maybe$Just(rawHistory);
				}
		}
	});
var $elm$browser$Debugger$History$elmToJs = A2($elm$core$Basics$composeR, _Json_wrap, _Debugger_unsafeCoerce);
var $elm$browser$Debugger$History$encodeHelp = F2(
	function (snapshot, allMessages) {
		return A3($elm$core$Array$foldl, $elm$core$List$cons, allMessages, snapshot.messages);
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$browser$Debugger$History$encode = function (_v0) {
	var snapshots = _v0.snapshots;
	var recent = _v0.recent;
	return A2(
		$elm$json$Json$Encode$list,
		$elm$browser$Debugger$History$elmToJs,
		A3(
			$elm$core$Array$foldr,
			$elm$browser$Debugger$History$encodeHelp,
			$elm$core$List$reverse(recent.messages),
			snapshots));
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$browser$Debugger$Metadata$encodeAlias = function (_v0) {
	var args = _v0.args;
	var tipe = _v0.tipe;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'args',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
				_Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(tipe))
			]));
};
var $elm$browser$Debugger$Metadata$encodeDict = F2(
	function (f, dict) {
		return $elm$json$Json$Encode$object(
			$elm$core$Dict$toList(
				A2(
					$elm$core$Dict$map,
					F2(
						function (key, value) {
							return f(value);
						}),
					dict)));
	});
var $elm$browser$Debugger$Metadata$encodeUnion = function (_v0) {
	var args = _v0.args;
	var tags = _v0.tags;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'args',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, args)),
				_Utils_Tuple2(
				'tags',
				A2(
					$elm$browser$Debugger$Metadata$encodeDict,
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
					tags))
			]));
};
var $elm$browser$Debugger$Metadata$encodeTypes = function (_v0) {
	var message = _v0.message;
	var unions = _v0.unions;
	var aliases = _v0.aliases;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'message',
				$elm$json$Json$Encode$string(message)),
				_Utils_Tuple2(
				'aliases',
				A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeAlias, aliases)),
				_Utils_Tuple2(
				'unions',
				A2($elm$browser$Debugger$Metadata$encodeDict, $elm$browser$Debugger$Metadata$encodeUnion, unions))
			]));
};
var $elm$browser$Debugger$Metadata$encodeVersions = function (_v0) {
	var elm = _v0.elm;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'elm',
				$elm$json$Json$Encode$string(elm))
			]));
};
var $elm$browser$Debugger$Metadata$encode = function (_v0) {
	var versions = _v0.versions;
	var types = _v0.types;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'versions',
				$elm$browser$Debugger$Metadata$encodeVersions(versions)),
				_Utils_Tuple2(
				'types',
				$elm$browser$Debugger$Metadata$encodeTypes(types))
			]));
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Debugger$Main$download = F2(
	function (metadata, history) {
		var historyLength = $elm$browser$Debugger$History$size(history);
		return A2(
			$elm$core$Task$perform,
			function (_v0) {
				return $elm$browser$Debugger$Main$NoOp;
			},
			A2(
				_Debugger_download,
				historyLength,
				_Json_unwrap(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'metadata',
								$elm$browser$Debugger$Metadata$encode(metadata)),
								_Utils_Tuple2(
								'history',
								$elm$browser$Debugger$History$encode(history))
							])))));
	});
var $elm$browser$Debugger$Main$Vertical = F3(
	function (a, b, c) {
		return {$: 'Vertical', a: a, b: b, c: c};
	});
var $elm$browser$Debugger$Main$drag = F2(
	function (info, layout) {
		if (layout.$ === 'Horizontal') {
			var status = layout.a;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Horizontal, status, info.x / info.width, y);
		} else {
			var status = layout.a;
			var x = layout.b;
			return A3($elm$browser$Debugger$Main$Vertical, status, x, info.y / info.height);
		}
	});
var $elm$browser$Debugger$History$Stepping = F2(
	function (a, b) {
		return {$: 'Stepping', a: a, b: b};
	});
var $elm$browser$Debugger$History$Done = F2(
	function (a, b) {
		return {$: 'Done', a: a, b: b};
	});
var $elm$browser$Debugger$History$getHelp = F3(
	function (update, msg, getResult) {
		if (getResult.$ === 'Done') {
			return getResult;
		} else {
			var n = getResult.a;
			var model = getResult.b;
			return (!n) ? A2(
				$elm$browser$Debugger$History$Done,
				msg,
				A2(update, msg, model).a) : A2(
				$elm$browser$Debugger$History$Stepping,
				n - 1,
				A2(update, msg, model).a);
		}
	});
var $elm$browser$Debugger$History$undone = function (getResult) {
	undone:
	while (true) {
		if (getResult.$ === 'Done') {
			var msg = getResult.a;
			var model = getResult.b;
			return _Utils_Tuple2(model, msg);
		} else {
			var $temp$getResult = getResult;
			getResult = $temp$getResult;
			continue undone;
		}
	}
};
var $elm$browser$Debugger$History$get = F3(
	function (update, index, history) {
		get:
		while (true) {
			var recent = history.recent;
			var snapshotMax = history.numMessages - recent.numMessages;
			if (_Utils_cmp(index, snapshotMax) > -1) {
				return $elm$browser$Debugger$History$undone(
					A3(
						$elm$core$List$foldr,
						$elm$browser$Debugger$History$getHelp(update),
						A2($elm$browser$Debugger$History$Stepping, index - snapshotMax, recent.model),
						recent.messages));
			} else {
				var _v0 = A2($elm$core$Array$get, (index / $elm$browser$Debugger$History$maxSnapshotSize) | 0, history.snapshots);
				if (_v0.$ === 'Nothing') {
					var $temp$update = update,
						$temp$index = index,
						$temp$history = history;
					update = $temp$update;
					index = $temp$index;
					history = $temp$history;
					continue get;
				} else {
					var model = _v0.a.model;
					var messages = _v0.a.messages;
					return $elm$browser$Debugger$History$undone(
						A3(
							$elm$core$Array$foldr,
							$elm$browser$Debugger$History$getHelp(update),
							A2($elm$browser$Debugger$History$Stepping, index % $elm$browser$Debugger$History$maxSnapshotSize, model),
							messages));
				}
			}
		}
	});
var $elm$browser$Debugger$History$getRecentMsg = function (history) {
	getRecentMsg:
	while (true) {
		var _v0 = history.recent.messages;
		if (!_v0.b) {
			var $temp$history = history;
			history = $temp$history;
			continue getRecentMsg;
		} else {
			var first = _v0.a;
			return first;
		}
	}
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$browser$Debugger$Expando$mergeDictHelp = F3(
	function (oldDict, key, value) {
		var _v12 = A2($elm$core$Dict$get, key, oldDict);
		if (_v12.$ === 'Nothing') {
			return value;
		} else {
			var oldValue = _v12.a;
			return A2($elm$browser$Debugger$Expando$mergeHelp, oldValue, value);
		}
	});
var $elm$browser$Debugger$Expando$mergeHelp = F2(
	function (old, _new) {
		var _v3 = _Utils_Tuple2(old, _new);
		_v3$6:
		while (true) {
			switch (_v3.b.$) {
				case 'S':
					return _new;
				case 'Primitive':
					return _new;
				case 'Sequence':
					if (_v3.a.$ === 'Sequence') {
						var _v4 = _v3.a;
						var isClosed = _v4.b;
						var oldValues = _v4.c;
						var _v5 = _v3.b;
						var seqType = _v5.a;
						var newValues = _v5.c;
						return A3(
							$elm$browser$Debugger$Expando$Sequence,
							seqType,
							isClosed,
							A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
					} else {
						break _v3$6;
					}
				case 'Dictionary':
					if (_v3.a.$ === 'Dictionary') {
						var _v6 = _v3.a;
						var isClosed = _v6.a;
						var _v7 = _v3.b;
						var keyValuePairs = _v7.b;
						return A2($elm$browser$Debugger$Expando$Dictionary, isClosed, keyValuePairs);
					} else {
						break _v3$6;
					}
				case 'Record':
					if (_v3.a.$ === 'Record') {
						var _v8 = _v3.a;
						var isClosed = _v8.a;
						var oldDict = _v8.b;
						var _v9 = _v3.b;
						var newDict = _v9.b;
						return A2(
							$elm$browser$Debugger$Expando$Record,
							isClosed,
							A2(
								$elm$core$Dict$map,
								$elm$browser$Debugger$Expando$mergeDictHelp(oldDict),
								newDict));
					} else {
						break _v3$6;
					}
				default:
					if (_v3.a.$ === 'Constructor') {
						var _v10 = _v3.a;
						var isClosed = _v10.b;
						var oldValues = _v10.c;
						var _v11 = _v3.b;
						var maybeName = _v11.a;
						var newValues = _v11.c;
						return A3(
							$elm$browser$Debugger$Expando$Constructor,
							maybeName,
							isClosed,
							A2($elm$browser$Debugger$Expando$mergeListHelp, oldValues, newValues));
					} else {
						break _v3$6;
					}
			}
		}
		return _new;
	});
var $elm$browser$Debugger$Expando$mergeListHelp = F2(
	function (olds, news) {
		var _v0 = _Utils_Tuple2(olds, news);
		if (!_v0.a.b) {
			return news;
		} else {
			if (!_v0.b.b) {
				return news;
			} else {
				var _v1 = _v0.a;
				var x = _v1.a;
				var xs = _v1.b;
				var _v2 = _v0.b;
				var y = _v2.a;
				var ys = _v2.b;
				return A2(
					$elm$core$List$cons,
					A2($elm$browser$Debugger$Expando$mergeHelp, x, y),
					A2($elm$browser$Debugger$Expando$mergeListHelp, xs, ys));
			}
		}
	});
var $elm$browser$Debugger$Expando$merge = F2(
	function (value, expando) {
		return A2(
			$elm$browser$Debugger$Expando$mergeHelp,
			expando,
			_Debugger_init(value));
	});
var $elm$browser$Debugger$Main$jumpUpdate = F3(
	function (update, index, model) {
		var history = $elm$browser$Debugger$Main$cachedHistory(model);
		var currentMsg = $elm$browser$Debugger$History$getRecentMsg(history);
		var currentModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
		var _v0 = A3($elm$browser$Debugger$History$get, update, index, history);
		var indexModel = _v0.a;
		var indexMsg = _v0.b;
		return _Utils_update(
			model,
			{
				expandoModel: A2($elm$browser$Debugger$Expando$merge, indexModel, model.expandoModel),
				expandoMsg: A2($elm$browser$Debugger$Expando$merge, indexMsg, model.expandoMsg),
				state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, currentModel, currentMsg, history)
			});
	});
var $elm$browser$Debugger$History$jsToElm = A2($elm$core$Basics$composeR, _Json_unwrap, _Debugger_unsafeCoerce);
var $elm$browser$Debugger$History$decoder = F2(
	function (initialModel, update) {
		var addMessage = F2(
			function (rawMsg, _v0) {
				var model = _v0.a;
				var history = _v0.b;
				var msg = $elm$browser$Debugger$History$jsToElm(rawMsg);
				return _Utils_Tuple2(
					A2(update, msg, model),
					A3($elm$browser$Debugger$History$add, msg, model, history));
			});
		var updateModel = function (rawMsgs) {
			return A3(
				$elm$core$List$foldl,
				addMessage,
				_Utils_Tuple2(
					initialModel,
					$elm$browser$Debugger$History$empty(initialModel)),
				rawMsgs);
		};
		return A2(
			$elm$json$Json$Decode$map,
			updateModel,
			$elm$json$Json$Decode$list($elm$json$Json$Decode$value));
	});
var $elm$browser$Debugger$History$getInitialModel = function (_v0) {
	var snapshots = _v0.snapshots;
	var recent = _v0.recent;
	var _v1 = A2($elm$core$Array$get, 0, snapshots);
	if (_v1.$ === 'Just') {
		var model = _v1.a.model;
		return model;
	} else {
		return recent.model;
	}
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$browser$Debugger$Main$loadNewHistory = F3(
	function (rawHistory, update, model) {
		var pureUserUpdate = F2(
			function (msg, userModel) {
				return A2(update, msg, userModel).a;
			});
		var initialUserModel = $elm$browser$Debugger$History$getInitialModel(model.history);
		var decoder = A2($elm$browser$Debugger$History$decoder, initialUserModel, pureUserUpdate);
		var _v0 = A2($elm$json$Json$Decode$decodeValue, decoder, rawHistory);
		if (_v0.$ === 'Err') {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{overlay: $elm$browser$Debugger$Overlay$corruptImport}),
				$elm$core$Platform$Cmd$none);
		} else {
			var _v1 = _v0.a;
			var latestUserModel = _v1.a;
			var newHistory = _v1.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						expandoModel: $elm$browser$Debugger$Expando$init(latestUserModel),
						expandoMsg: $elm$browser$Debugger$Expando$init(
							$elm$browser$Debugger$History$getRecentMsg(newHistory)),
						history: newHistory,
						overlay: $elm$browser$Debugger$Overlay$none,
						state: $elm$browser$Debugger$Main$Running(latestUserModel)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $elm$browser$Debugger$Main$scroll = function (popout) {
	return A2(
		$elm$core$Task$perform,
		$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
		_Debugger_scroll(popout));
};
var $elm$browser$Debugger$Main$scrollTo = F2(
	function (id, popout) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
			A2(_Debugger_scrollTo, id, popout));
	});
var $elm$browser$Debugger$Main$setDragStatus = F2(
	function (status, layout) {
		if (layout.$ === 'Horizontal') {
			var x = layout.b;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Horizontal, status, x, y);
		} else {
			var x = layout.b;
			var y = layout.c;
			return A3($elm$browser$Debugger$Main$Vertical, status, x, y);
		}
	});
var $elm$browser$Debugger$Main$swapLayout = function (layout) {
	if (layout.$ === 'Horizontal') {
		var s = layout.a;
		var x = layout.b;
		var y = layout.c;
		return A3($elm$browser$Debugger$Main$Vertical, s, x, y);
	} else {
		var s = layout.a;
		var x = layout.b;
		var y = layout.c;
		return A3($elm$browser$Debugger$Main$Horizontal, s, x, y);
	}
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$browser$Debugger$Expando$updateIndex = F3(
	function (n, func, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			var x = list.a;
			var xs = list.b;
			return (n <= 0) ? A2(
				$elm$core$List$cons,
				func(x),
				xs) : A2(
				$elm$core$List$cons,
				x,
				A3($elm$browser$Debugger$Expando$updateIndex, n - 1, func, xs));
		}
	});
var $elm$browser$Debugger$Expando$update = F2(
	function (msg, value) {
		switch (value.$) {
			case 'S':
				return value;
			case 'Primitive':
				return value;
			case 'Sequence':
				var seqType = value.a;
				var isClosed = value.b;
				var valueList = value.c;
				switch (msg.$) {
					case 'Toggle':
						return A3($elm$browser$Debugger$Expando$Sequence, seqType, !isClosed, valueList);
					case 'Index':
						if (msg.a.$ === 'None') {
							var _v3 = msg.a;
							var index = msg.b;
							var subMsg = msg.c;
							return A3(
								$elm$browser$Debugger$Expando$Sequence,
								seqType,
								isClosed,
								A3(
									$elm$browser$Debugger$Expando$updateIndex,
									index,
									$elm$browser$Debugger$Expando$update(subMsg),
									valueList));
						} else {
							return value;
						}
					default:
						return value;
				}
			case 'Dictionary':
				var isClosed = value.a;
				var keyValuePairs = value.b;
				switch (msg.$) {
					case 'Toggle':
						return A2($elm$browser$Debugger$Expando$Dictionary, !isClosed, keyValuePairs);
					case 'Index':
						var redirect = msg.a;
						var index = msg.b;
						var subMsg = msg.c;
						switch (redirect.$) {
							case 'None':
								return value;
							case 'Key':
								return A2(
									$elm$browser$Debugger$Expando$Dictionary,
									isClosed,
									A3(
										$elm$browser$Debugger$Expando$updateIndex,
										index,
										function (_v6) {
											var k = _v6.a;
											var v = _v6.b;
											return _Utils_Tuple2(
												A2($elm$browser$Debugger$Expando$update, subMsg, k),
												v);
										},
										keyValuePairs));
							default:
								return A2(
									$elm$browser$Debugger$Expando$Dictionary,
									isClosed,
									A3(
										$elm$browser$Debugger$Expando$updateIndex,
										index,
										function (_v7) {
											var k = _v7.a;
											var v = _v7.b;
											return _Utils_Tuple2(
												k,
												A2($elm$browser$Debugger$Expando$update, subMsg, v));
										},
										keyValuePairs));
						}
					default:
						return value;
				}
			case 'Record':
				var isClosed = value.a;
				var valueDict = value.b;
				switch (msg.$) {
					case 'Toggle':
						return A2($elm$browser$Debugger$Expando$Record, !isClosed, valueDict);
					case 'Index':
						return value;
					default:
						var field = msg.a;
						var subMsg = msg.b;
						return A2(
							$elm$browser$Debugger$Expando$Record,
							isClosed,
							A3(
								$elm$core$Dict$update,
								field,
								$elm$browser$Debugger$Expando$updateField(subMsg),
								valueDict));
				}
			default:
				var maybeName = value.a;
				var isClosed = value.b;
				var valueList = value.c;
				switch (msg.$) {
					case 'Toggle':
						return A3($elm$browser$Debugger$Expando$Constructor, maybeName, !isClosed, valueList);
					case 'Index':
						if (msg.a.$ === 'None') {
							var _v10 = msg.a;
							var index = msg.b;
							var subMsg = msg.c;
							return A3(
								$elm$browser$Debugger$Expando$Constructor,
								maybeName,
								isClosed,
								A3(
									$elm$browser$Debugger$Expando$updateIndex,
									index,
									$elm$browser$Debugger$Expando$update(subMsg),
									valueList));
						} else {
							return value;
						}
					default:
						return value;
				}
		}
	});
var $elm$browser$Debugger$Expando$updateField = F2(
	function (msg, maybeExpando) {
		if (maybeExpando.$ === 'Nothing') {
			return maybeExpando;
		} else {
			var expando = maybeExpando.a;
			return $elm$core$Maybe$Just(
				A2($elm$browser$Debugger$Expando$update, msg, expando));
		}
	});
var $elm$browser$Debugger$Main$Upload = function (a) {
	return {$: 'Upload', a: a};
};
var $elm$browser$Debugger$Main$upload = function (popout) {
	return A2(
		$elm$core$Task$perform,
		$elm$browser$Debugger$Main$Upload,
		_Debugger_upload(popout));
};
var $elm$browser$Debugger$Overlay$BadMetadata = function (a) {
	return {$: 'BadMetadata', a: a};
};
var $elm$browser$Debugger$Overlay$badMetadata = $elm$browser$Debugger$Overlay$BadMetadata;
var $elm$browser$Debugger$Main$withGoodMetadata = F2(
	function (model, func) {
		var _v0 = model.metadata;
		if (_v0.$ === 'Ok') {
			var metadata = _v0.a;
			return func(metadata);
		} else {
			var error = _v0.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						overlay: $elm$browser$Debugger$Overlay$badMetadata(error)
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $elm$browser$Debugger$Main$wrapUpdate = F3(
	function (update, msg, model) {
		wrapUpdate:
		while (true) {
			switch (msg.$) {
				case 'NoOp':
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				case 'UserMsg':
					var userMsg = msg.a;
					var userModel = $elm$browser$Debugger$Main$getLatestModel(model.state);
					var newHistory = A3($elm$browser$Debugger$History$add, userMsg, userModel, model.history);
					var _v1 = A2(update, userMsg, userModel);
					var newUserModel = _v1.a;
					var userCmds = _v1.b;
					var commands = A2($elm$core$Platform$Cmd$map, $elm$browser$Debugger$Main$UserMsg, userCmds);
					var _v2 = model.state;
					if (_v2.$ === 'Running') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									expandoModel: A2($elm$browser$Debugger$Expando$merge, newUserModel, model.expandoModel),
									expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
									history: newHistory,
									state: $elm$browser$Debugger$Main$Running(newUserModel)
								}),
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										commands,
										$elm$browser$Debugger$Main$scroll(model.popout)
									])));
					} else {
						var index = _v2.a;
						var indexModel = _v2.b;
						var history = _v2.e;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									history: newHistory,
									state: A5($elm$browser$Debugger$Main$Paused, index, indexModel, newUserModel, userMsg, history)
								}),
							commands);
					}
				case 'TweakExpandoMsg':
					var eMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								expandoMsg: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoMsg)
							}),
						$elm$core$Platform$Cmd$none);
				case 'TweakExpandoModel':
					var eMsg = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								expandoModel: A2($elm$browser$Debugger$Expando$update, eMsg, model.expandoModel)
							}),
						$elm$core$Platform$Cmd$none);
				case 'Resume':
					var _v3 = model.state;
					if (_v3.$ === 'Running') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var userModel = _v3.c;
						var userMsg = _v3.d;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									expandoModel: A2($elm$browser$Debugger$Expando$merge, userModel, model.expandoModel),
									expandoMsg: A2($elm$browser$Debugger$Expando$merge, userMsg, model.expandoMsg),
									state: $elm$browser$Debugger$Main$Running(userModel)
								}),
							$elm$browser$Debugger$Main$scroll(model.popout));
					}
				case 'Jump':
					var index = msg.a;
					return _Utils_Tuple2(
						A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model),
						$elm$core$Platform$Cmd$none);
				case 'SliderJump':
					var index = msg.a;
					return _Utils_Tuple2(
						A3($elm$browser$Debugger$Main$jumpUpdate, update, index, model),
						A2(
							$elm$browser$Debugger$Main$scrollTo,
							$elm$browser$Debugger$History$idForMessageIndex(index),
							model.popout));
				case 'Open':
					return _Utils_Tuple2(
						model,
						A2(
							$elm$core$Task$perform,
							$elm$core$Basics$always($elm$browser$Debugger$Main$NoOp),
							_Debugger_open(model.popout)));
				case 'Up':
					var _v4 = model.state;
					if (_v4.$ === 'Running') {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						var i = _v4.a;
						var history = _v4.e;
						var targetIndex = i + 1;
						if (_Utils_cmp(
							targetIndex,
							$elm$browser$Debugger$History$size(history)) < 0) {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$SliderJump(targetIndex),
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						} else {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$Resume,
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						}
					}
				case 'Down':
					var _v5 = model.state;
					if (_v5.$ === 'Running') {
						var $temp$update = update,
							$temp$msg = $elm$browser$Debugger$Main$Jump(
							$elm$browser$Debugger$History$size(model.history) - 1),
							$temp$model = model;
						update = $temp$update;
						msg = $temp$msg;
						model = $temp$model;
						continue wrapUpdate;
					} else {
						var index = _v5.a;
						if (index > 0) {
							var $temp$update = update,
								$temp$msg = $elm$browser$Debugger$Main$SliderJump(index - 1),
								$temp$model = model;
							update = $temp$update;
							msg = $temp$msg;
							model = $temp$model;
							continue wrapUpdate;
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					}
				case 'Import':
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (_v6) {
							return _Utils_Tuple2(
								model,
								$elm$browser$Debugger$Main$upload(model.popout));
						});
				case 'Export':
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (metadata) {
							return _Utils_Tuple2(
								model,
								A2($elm$browser$Debugger$Main$download, metadata, model.history));
						});
				case 'Upload':
					var jsonString = msg.a;
					return A2(
						$elm$browser$Debugger$Main$withGoodMetadata,
						model,
						function (metadata) {
							var _v7 = A2($elm$browser$Debugger$Overlay$assessImport, metadata, jsonString);
							if (_v7.$ === 'Err') {
								var newOverlay = _v7.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{overlay: newOverlay}),
									$elm$core$Platform$Cmd$none);
							} else {
								var rawHistory = _v7.a;
								return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
							}
						});
				case 'OverlayMsg':
					var overlayMsg = msg.a;
					var _v8 = A2($elm$browser$Debugger$Overlay$close, overlayMsg, model.overlay);
					if (_v8.$ === 'Nothing') {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{overlay: $elm$browser$Debugger$Overlay$none}),
							$elm$core$Platform$Cmd$none);
					} else {
						var rawHistory = _v8.a;
						return A3($elm$browser$Debugger$Main$loadNewHistory, rawHistory, update, model);
					}
				case 'SwapLayout':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: $elm$browser$Debugger$Main$swapLayout(model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				case 'DragStart':
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Moving, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				case 'Drag':
					var info = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$drag, info, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
				default:
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								layout: A2($elm$browser$Debugger$Main$setDragStatus, $elm$browser$Debugger$Main$Static, model.layout)
							}),
						$elm$core$Platform$Cmd$none);
			}
		}
	});
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Logic$Loading = {$: 'Loading'};
var $krisajenkins$remotedata$RemoteData$Loading = {$: 'Loading'};
var $author$project$Session$Loading = function (a) {
	return {$: 'Loading', a: a};
};
var $author$project$Session$NotAsked = {$: 'NotAsked'};
var $author$project$Route$NotFound = {$: 'NotFound'};
var $author$project$Logic$NotStarted = {$: 'NotStarted'};
var $author$project$Main$Pretest = function (a) {
	return {$: 'Pretest', a: a};
};
var $author$project$Main$Session1 = function (a) {
	return {$: 'Session1', a: a};
};
var $author$project$Main$Session2 = function (a) {
	return {$: 'Session2', a: a};
};
var $author$project$Main$Session3 = function (a) {
	return {$: 'Session3', a: a};
};
var $author$project$Pretest$Pretest$ServerRespondedWithAllPretestData = F4(
	function (a, b, c, d) {
		return {$: 'ServerRespondedWithAllPretestData', a: a, b: b, c: c, d: d};
	});
var $author$project$Pretest$Pretest$ServerRespondedWithSomeError = function (a) {
	return {$: 'ServerRespondedWithSomeError', a: a};
};
var $author$project$Pretest$Pretest$ServerRespondedWithSomePretestData = function (a) {
	return {$: 'ServerRespondedWithSomePretestData', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedA4 = function (a) {
	return {$: 'LoadedA4', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedB4 = function (a) {
	return {$: 'LoadedB4', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedC4 = function (a) {
	return {$: 'LoadedC4', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedD4 = function (a) {
	return {$: 'LoadedD4', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$State4 = F5(
	function (a, b, c, d, e) {
		return {$: 'State4', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $0ui$elm_task_parallel$Task$Parallel$routeTo = F2(
	function (successMsg, failureMsg) {
		return A2(
			$elm$core$Basics$composeR,
			$elm$core$Task$andThen(
				A2($elm$core$Basics$composeL, $elm$core$Task$succeed, $elm$core$Result$Ok)),
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Task$onError(
					A2($elm$core$Basics$composeL, $elm$core$Task$succeed, $elm$core$Result$Err)),
				$elm$core$Task$perform(
					function (result) {
						if (result.$ === 'Ok') {
							var a = result.a;
							return successMsg(a);
						} else {
							var err = result.a;
							return failureMsg(err);
						}
					})));
	});
var $0ui$elm_task_parallel$Task$Parallel$attempt4 = function (_v0) {
	var task1 = _v0.task1;
	var task2 = _v0.task2;
	var task3 = _v0.task3;
	var task4 = _v0.task4;
	var onUpdates = _v0.onUpdates;
	var onSuccess = _v0.onSuccess;
	var onFailure = _v0.onFailure;
	return _Utils_Tuple2(
		A5($0ui$elm_task_parallel$Task$Parallel$State4, onSuccess, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedA4),
					onFailure,
					task1),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedB4),
					onFailure,
					task2),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedC4),
					onFailure,
					task3),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedD4),
					onFailure,
					task4)
				])));
};
var $author$project$Data$apps = {sleep: 'appTEVHZLw3jNa7fU', spacing: 'appvKOc8FH0j48Hw1'};
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$absolute = F2(
	function (pathSegments, parameters) {
		return '/' + (A2($elm$core$String$join, '/', pathSegments) + $elm$url$Url$Builder$toQuery(parameters));
	});
var $elm$url$Url$Builder$QueryParameter = F2(
	function (a, b) {
		return {$: 'QueryParameter', a: a, b: b};
	});
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $elm$url$Url$Builder$string = F2(
	function (key, value) {
		return A2(
			$elm$url$Url$Builder$QueryParameter,
			$elm$url$Url$percentEncode(key),
			$elm$url$Url$percentEncode(value));
	});
var $author$project$Data$buildQuery = function (_v0) {
	var app = _v0.app;
	var base = _v0.base;
	var view_ = _v0.view_;
	return A2(
		$elm$url$Url$Builder$absolute,
		_List_fromArray(
			['.netlify', 'functions', 'api']),
		_List_fromArray(
			[
				A2($elm$url$Url$Builder$string, 'app', app),
				A2($elm$url$Url$Builder$string, 'base', base),
				A2($elm$url$Url$Builder$string, 'view', view_)
			]));
};
var $author$project$ExperimentInfo$Context = {$: 'Context'};
var $author$project$ExperimentInfo$Forme = {$: 'Forme'};
var $author$project$ExperimentInfo$Other = {$: 'Other'};
var $author$project$ExperimentInfo$OtherSession = {$: 'OtherSession'};
var $author$project$ExperimentInfo$Posttest = {$: 'Posttest'};
var $author$project$ExperimentInfo$Pretest = {$: 'Pretest'};
var $author$project$ExperimentInfo$Sens = {$: 'Sens'};
var $author$project$ExperimentInfo$Session1 = {$: 'Session1'};
var $author$project$ExperimentInfo$Session2 = {$: 'Session2'};
var $author$project$ExperimentInfo$Session3 = {$: 'Session3'};
var $author$project$ExperimentInfo$Task = function (uid) {
	return function (session) {
		return function (type_) {
			return function (name) {
				return function (url) {
					return function (description) {
						return function (instructions) {
							return function (instructions_short) {
								return function (feedback_correct) {
									return function (feedback_incorrect) {
										return function (end) {
											return function (trainingWheel) {
												return function (introToMain) {
													return {description: description, end: end, feedback_correct: feedback_correct, feedback_incorrect: feedback_incorrect, instructions: instructions, instructions_short: instructions_short, introToMain: introToMain, name: name, session: session, trainingWheel: trainingWheel, type_: type_, uid: uid, url: url};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $author$project$Data$decodeRecords = function (xs) {
	var decode = function (fieldsDecoder) {
		return A2($elm$json$Json$Decode$field, 'records', fieldsDecoder);
	};
	return decode(
		$elm$json$Json$Decode$list(xs));
};
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return $elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						decoder,
						$elm$json$Json$Decode$null(fallback)
					]));
		};
		var handleResult = function (input) {
			var _v0 = A2($elm$json$Json$Decode$decodeValue, pathDecoder, input);
			if (_v0.$ === 'Ok') {
				var rawValue = _v0.a;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					nullOr(valDecoder),
					rawValue);
				if (_v1.$ === 'Ok') {
					var finalResult = _v1.a;
					return $elm$json$Json$Decode$succeed(finalResult);
				} else {
					var finalErr = _v1.a;
					return $elm$json$Json$Decode$fail(
						$elm$json$Json$Decode$errorToString(finalErr));
				}
			} else {
				return $elm$json$Json$Decode$succeed(fallback);
			}
		};
		return A2($elm$json$Json$Decode$andThen, handleResult, $elm$json$Json$Decode$value);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder,
				A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $author$project$ExperimentInfo$decode = function () {
	var mapToType_ = function (str) {
		switch (str) {
			case 'Sens':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Sens);
			case 'Forme':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Forme);
			case 'Context':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Context);
			default:
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Other);
		}
	};
	var mapToSession = function (str) {
		switch (str) {
			case 'session1':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Session1);
			case 'session2':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Session2);
			case 'session3':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Session3);
			case 'Prétest':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Pretest);
			case 'Post-test':
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Posttest);
			default:
				return $elm$json$Json$Decode$succeed($author$project$ExperimentInfo$OtherSession);
		}
	};
	var decoder = A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'IntroToMain',
		$elm$json$Json$Decode$string,
		'Missing IntroToMain text',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'trainingWheels',
			$elm$json$Json$Decode$string,
			'Missing training wheel',
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'End',
				$elm$json$Json$Decode$string,
				'Missing End',
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'feedback_incorrect',
					$elm$json$Json$Decode$string,
					'Missing feedback incorrect',
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'feedback_correct',
						$elm$json$Json$Decode$string,
						'Missing feedback correct',
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'Instructions_short',
							$elm$json$Json$Decode$string,
							'Missing instructions short',
							A4(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
								'Instructions',
								$elm$json$Json$Decode$string,
								'Missing instructions',
								A4(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
									'Description',
									$elm$json$Json$Decode$string,
									'Missing Description',
									A4(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
										'Demo_Link',
										$elm$json$Json$Decode$string,
										'Missing link',
										A4(
											$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
											'Name',
											$elm$json$Json$Decode$string,
											'Missing Name',
											A2(
												$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
												A2(
													$elm$json$Json$Decode$andThen,
													mapToType_,
													A2($elm$json$Json$Decode$field, 'Type', $elm$json$Json$Decode$string)),
												A2(
													$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
													A2(
														$elm$json$Json$Decode$andThen,
														mapToSession,
														A2($elm$json$Json$Decode$field, 'Session', $elm$json$Json$Decode$string)),
													A3(
														$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
														'UID',
														$elm$json$Json$Decode$string,
														$elm$json$Json$Decode$succeed($author$project$ExperimentInfo$Task))))))))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $author$project$Data$handleJsonResponse = F2(
	function (decoder, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'BadStatus_':
				var statusCode = response.a.statusCode;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(statusCode));
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			default:
				var body = response.b;
				var _v1 = A2($elm$json$Json$Decode$decodeString, decoder, body);
				if (_v1.$ === 'Err') {
					return $elm$core$Result$Err(
						$elm$http$Http$BadBody(body));
				} else {
					var result = _v1.a;
					return $elm$core$Result$Ok(result);
				}
		}
	});
var $elm$http$Http$stringResolver = A2(_Http_expect, '', $elm$core$Basics$identity);
var $elm$core$Task$fail = _Scheduler_fail;
var $elm$http$Http$resultToTask = function (result) {
	if (result.$ === 'Ok') {
		var a = result.a;
		return $elm$core$Task$succeed(a);
	} else {
		var x = result.a;
		return $elm$core$Task$fail(x);
	}
};
var $elm$http$Http$task = function (r) {
	return A3(
		_Http_toTask,
		_Utils_Tuple0,
		$elm$http$Http$resultToTask,
		{allowCookiesFromOtherDomains: false, body: r.body, expect: r.resolver, headers: r.headers, method: r.method, timeout: r.timeout, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$ExperimentInfo$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$ExperimentInfo$decode)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'tasks', view_: 'allTasksGrid'})
	});
var $author$project$Pretest$SPR$Trial = F6(
	function (id, taggedSegments, question, isGrammatical, isTraining, feedback) {
		return {feedback: feedback, id: id, isGrammatical: isGrammatical, isTraining: isTraining, question: question, taggedSegments: taggedSegments};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Pretest$SPR$Critic = {$: 'Critic'};
var $author$project$Pretest$SPR$NoUnit = {$: 'NoUnit'};
var $author$project$Pretest$SPR$SpillOver = {$: 'SpillOver'};
var $elm$core$String$filter = _String_filter;
var $author$project$Pretest$SPR$paragraphToTaggedSegments = function (str) {
	return A2(
		$elm$core$List$map,
		function (seg) {
			return A2($elm$core$String$contains, '$', seg) ? _Utils_Tuple2(
				$author$project$Pretest$SPR$Critic,
				A2(
					$elm$core$String$filter,
					$elm$core$Basics$neq(
						_Utils_chr('$')),
					seg)) : (A2($elm$core$String$contains, '~', seg) ? _Utils_Tuple2(
				$author$project$Pretest$SPR$SpillOver,
				A2(
					$elm$core$String$filter,
					$elm$core$Basics$neq(
						_Utils_chr('~')),
					seg)) : _Utils_Tuple2($author$project$Pretest$SPR$NoUnit, seg));
		},
		A2($elm$core$String$split, '|', str));
};
var $author$project$Pretest$SPR$decodeAcceptabilityTrials = function () {
	var decoder = A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'feedback',
		$elm$json$Json$Decode$string,
		'Missing feedback',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'isTraining',
			$elm$json$Json$Decode$bool,
			false,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'isGrammatical',
				$elm$json$Json$Decode$bool,
				false,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'Question',
					$elm$json$Json$Decode$string,
					A2(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
						A2(
							$elm$json$Json$Decode$map,
							$author$project$Pretest$SPR$paragraphToTaggedSegments,
							A2($elm$json$Json$Decode$field, 'Tagged Paragraph ($CRITIC$, ~SPILL-OVER~)', $elm$json$Json$Decode$string)),
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'id',
							$elm$json$Json$Decode$string,
							$elm$json$Json$Decode$succeed($author$project$Pretest$SPR$Trial)))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Pretest$SPR$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Pretest$SPR$decodeAcceptabilityTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'SPR', view_: 'Pretest'})
	});
var $author$project$Pretest$SentenceCompletion$Trial = F7(
	function (id, context, firstAmorce, secondAmorce, isTraining, firstFeedback, secondFeedback) {
		return {context: context, firstAmorce: firstAmorce, firstFeedback: firstFeedback, id: id, isTraining: isTraining, secondAmorce: secondAmorce, secondFeedback: secondFeedback};
	});
var $author$project$Pretest$SentenceCompletion$decodeAcceptabilityTrials = function () {
	var decoder = A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'feedback2',
		$elm$json$Json$Decode$string,
		'Missing feedback 2',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'feedback1',
			$elm$json$Json$Decode$string,
			'Missing feedback',
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'isTraining',
				$elm$json$Json$Decode$bool,
				false,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'amorce2',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'amorce1',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'context',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'id',
								$elm$json$Json$Decode$string,
								$elm$json$Json$Decode$succeed($author$project$Pretest$SentenceCompletion$Trial))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Pretest$SentenceCompletion$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Pretest$SentenceCompletion$decodeAcceptabilityTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'sentence_completion', view_: 'all'})
	});
var $author$project$Pretest$VKS$Trial = F2(
	function (id, verb) {
		return {id: id, verb: verb};
	});
var $author$project$Pretest$VKS$decodeAcceptabilityTrials = function () {
	var decoder = A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'Word_Text',
		$elm$json$Json$Decode$string,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'id',
			$elm$json$Json$Decode$string,
			$elm$json$Json$Decode$succeed($author$project$Pretest$VKS$Trial)));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Pretest$VKS$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Pretest$VKS$decodeAcceptabilityTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Meaning'})
	});
var $author$project$Pretest$Pretest$attempt = $0ui$elm_task_parallel$Task$Parallel$attempt4(
	{onFailure: $author$project$Pretest$Pretest$ServerRespondedWithSomeError, onSuccess: $author$project$Pretest$Pretest$ServerRespondedWithAllPretestData, onUpdates: $author$project$Pretest$Pretest$ServerRespondedWithSomePretestData, task1: $author$project$Pretest$SPR$getRecords, task2: $author$project$Pretest$SentenceCompletion$getRecords, task3: $author$project$ExperimentInfo$getRecords, task4: $author$project$Pretest$VKS$getRecords});
var $author$project$Session3$Session$ServerRespondedWithAllSession3Data = F4(
	function (a, b, c, d) {
		return {$: 'ServerRespondedWithAllSession3Data', a: a, b: b, c: c, d: d};
	});
var $author$project$Session3$Session$ServerRespondedWithSomeError = function (a) {
	return {$: 'ServerRespondedWithSomeError', a: a};
};
var $author$project$Session3$Session$ServerRespondedWithSomeSession3Data = function (a) {
	return {$: 'ServerRespondedWithSomeSession3Data', a: a};
};
var $author$project$Data$AudioFile = F2(
	function (url, type_) {
		return {type_: type_, url: url};
	});
var $author$project$Session3$CU3$Trial = F7(
	function (uid, writtenWord, audioSentence, context, amorce, feedback, isTraining) {
		return {amorce: amorce, audioSentence: audioSentence, context: context, feedback: feedback, isTraining: isTraining, uid: uid, writtenWord: writtenWord};
	});
var $author$project$Data$audioDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Data$AudioFile,
	A2($elm$json$Json$Decode$field, 'url', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string));
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Data$decodeAudioFiles = A2(
	$elm$json$Json$Decode$map,
	A2(
		$elm$core$Basics$composeL,
		$elm$core$Maybe$withDefault(
			A2($author$project$Data$AudioFile, '', '')),
		$elm$core$List$head),
	$elm$json$Json$Decode$list($author$project$Data$audioDecoder));
var $author$project$Data$decodeBool = function (fieldname) {
	var stringToBoolDecoder = function (str) {
		if (str === 'true') {
			return $elm$json$Json$Decode$succeed(true);
		} else {
			return $elm$json$Json$Decode$succeed(false);
		}
	};
	return $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom(
		A2(
			$elm$json$Json$Decode$andThen,
			stringToBoolDecoder,
			A2($elm$json$Json$Decode$field, fieldname, $elm$json$Json$Decode$string)));
};
var $author$project$Session3$CU3$decodeTranslationInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'CU_Lvl3_Feedback',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'CU_Lvl3_TextToComplete_amorce',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'CU_Lvl3_Presentation',
					$elm$json$Json$Decode$string,
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'CU_Lv3_Audio',
						$author$project$Data$decodeAudioFiles,
						A2($author$project$Data$AudioFile, '', ''),
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'Word_Text',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'UID',
								$elm$json$Json$Decode$string,
								$elm$json$Json$Decode$succeed($author$project$Session3$CU3$Trial))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session3$CU3$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session3$CU3$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session3$Spelling3$Trial = F4(
	function (uid, writtenWord, audioSentence, isTraining) {
		return {audioSentence: audioSentence, isTraining: isTraining, uid: uid, writtenWord: writtenWord};
	});
var $author$project$Session3$Spelling3$decodeTranslationInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'Word_Audio',
			$author$project$Data$decodeAudioFiles,
			A2($author$project$Data$AudioFile, '', ''),
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'Word_Text',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'UID',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Session3$Spelling3$Trial)))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session3$Spelling3$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session3$Spelling3$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session3$Synonym$Trial = F7(
	function (uid, target, pre, stimulus, post, isTraining, radical) {
		return {isTraining: isTraining, post: post, pre: pre, radical: radical, stimulus: stimulus, target: target, uid: uid};
	});
var $author$project$Session3$Synonym$decodeSynonymTrials = function () {
	var stringToBoolDecoder = function (str) {
		if (str === 'true') {
			return $elm$json$Json$Decode$succeed(true);
		} else {
			return $elm$json$Json$Decode$succeed(false);
		}
	};
	var decoder = A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'radical',
		$elm$json$Json$Decode$string,
		A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2(
				$elm$json$Json$Decode$andThen,
				stringToBoolDecoder,
				A2($elm$json$Json$Decode$field, 'isTraining', $elm$json$Json$Decode$string)),
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'post',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'stim',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'pre',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'Word_Text',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'UID',
								$elm$json$Json$Decode$string,
								$elm$json$Json$Decode$succeed($author$project$Session3$Synonym$Trial))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session3$Synonym$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session3$Synonym$decodeSynonymTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session3$Session$attempt = $0ui$elm_task_parallel$Task$Parallel$attempt4(
	{onFailure: $author$project$Session3$Session$ServerRespondedWithSomeError, onSuccess: $author$project$Session3$Session$ServerRespondedWithAllSession3Data, onUpdates: $author$project$Session3$Session$ServerRespondedWithSomeSession3Data, task1: $author$project$Session3$CU3$getRecords, task2: $author$project$Session3$Spelling3$getRecords, task3: $author$project$Session3$Synonym$getRecords, task4: $author$project$ExperimentInfo$getRecords});
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Route$AcceptabilityEnd = {$: 'AcceptabilityEnd'};
var $author$project$Route$AcceptabilityInstructions = {$: 'AcceptabilityInstructions'};
var $author$project$Route$AcceptabilityStart = {$: 'AcceptabilityStart'};
var $author$project$Route$AuthenticatedSession2 = F2(
	function (a, b) {
		return {$: 'AuthenticatedSession2', a: a, b: b};
	});
var $author$project$Route$AuthenticatedSession3 = F2(
	function (a, b) {
		return {$: 'AuthenticatedSession3', a: a, b: b};
	});
var $author$project$Route$CU = {$: 'CU'};
var $author$project$Route$CU1 = {$: 'CU1'};
var $author$project$Route$CU3 = {$: 'CU3'};
var $author$project$Route$EmailSent = {$: 'EmailSent'};
var $author$project$Route$GeneralInfos = {$: 'GeneralInfos'};
var $author$project$Route$Home = {$: 'Home'};
var $author$project$Route$Meaning = {$: 'Meaning'};
var $author$project$Route$Pilote = F2(
	function (a, b) {
		return {$: 'Pilote', a: a, b: b};
	});
var $author$project$Route$Presentation = {$: 'Presentation'};
var $author$project$Route$Pretest = function (a) {
	return {$: 'Pretest', a: a};
};
var $author$project$Route$SPR = {$: 'SPR'};
var $author$project$Route$SentenceCompletion = {$: 'SentenceCompletion'};
var $author$project$Route$Session1 = F2(
	function (a, b) {
		return {$: 'Session1', a: a, b: b};
	});
var $author$project$Route$Spelling = {$: 'Spelling'};
var $author$project$Route$Spelling3 = {$: 'Spelling3'};
var $author$project$Route$SpellingLevel1 = {$: 'SpellingLevel1'};
var $author$project$Route$Synonym = {$: 'Synonym'};
var $author$project$Route$TopSession1 = {$: 'TopSession1'};
var $author$project$Route$Translation = {$: 'Translation'};
var $author$project$Route$VKS = {$: 'VKS'};
var $author$project$Route$YN = {$: 'YN'};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var visited = _v0.visited;
				var unvisited = _v0.unvisited;
				var params = _v0.params;
				var frag = _v0.frag;
				var value = _v0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $elm$url$Url$Parser$string = A2($elm$url$Url$Parser$custom, 'STRING', $elm$core$Maybe$Just);
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Route$parser = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Route$Home, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Pretest,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('pretest'),
				$elm$url$Url$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$SPR,
							$elm$url$Url$Parser$s('spr')),
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$YN,
							$elm$url$Url$Parser$s('yesno-task')),
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$GeneralInfos,
							$elm$url$Url$Parser$s('informations')),
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$EmailSent,
							$elm$url$Url$Parser$s('email-sent')),
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$SentenceCompletion,
							$elm$url$Url$Parser$s('sentence-completion')),
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$VKS,
							$elm$url$Url$Parser$s('vks'))
						])))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Pilote,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('user'),
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$string,
					A2(
						$elm$url$Url$Parser$slash,
						$elm$url$Url$Parser$s('pilote'),
						A2(
							$elm$url$Url$Parser$slash,
							$elm$url$Url$Parser$s('acceptability'),
							$elm$url$Url$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$url$Url$Parser$map,
										$author$project$Route$AcceptabilityInstructions,
										$elm$url$Url$Parser$s('instructions')),
										A2(
										$elm$url$Url$Parser$map,
										$author$project$Route$AcceptabilityStart,
										$elm$url$Url$Parser$s('start')),
										A2(
										$elm$url$Url$Parser$map,
										$author$project$Route$AcceptabilityEnd,
										$elm$url$Url$Parser$s('end'))
									]))))))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Pretest,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('posttest'),
				$elm$url$Url$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$url$Url$Parser$map,
							$author$project$Route$YN,
							$elm$url$Url$Parser$s('cloud-words'))
						])))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$Session1,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('user'),
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$string,
					A2(
						$elm$url$Url$Parser$slash,
						$elm$url$Url$Parser$s('session1'),
						$elm$url$Url$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Meaning,
									$elm$url$Url$Parser$s('meaning')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$SpellingLevel1,
									$elm$url$Url$Parser$s('spelling')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Presentation,
									$elm$url$Url$Parser$s('presentation')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$CU1,
									$elm$url$Url$Parser$s('context-understanding')),
									A2($elm$url$Url$Parser$map, $author$project$Route$TopSession1, $elm$url$Url$Parser$top)
								])))))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$AuthenticatedSession2,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('user'),
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$string,
					A2(
						$elm$url$Url$Parser$slash,
						$elm$url$Url$Parser$s('session2'),
						$elm$url$Url$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Spelling,
									$elm$url$Url$Parser$s('spelling')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Translation,
									$elm$url$Url$Parser$s('translation')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$CU,
									$elm$url$Url$Parser$s('context-understanding'))
								])))))),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Route$AuthenticatedSession3,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('user'),
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$string,
					A2(
						$elm$url$Url$Parser$slash,
						$elm$url$Url$Parser$s('session3'),
						$elm$url$Url$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$CU3,
									$elm$url$Url$Parser$s('context-understanding')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Spelling3,
									$elm$url$Url$Parser$s('spelling')),
									A2(
									$elm$url$Url$Parser$map,
									$author$project$Route$Synonym,
									$elm$url$Url$Parser$s('synonym'))
								]))))))
		]));
var $author$project$Route$fromUrl = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Route$NotFound,
		A2($elm$url$Url$Parser$parse, $author$project$Route$parser, url));
};
var $author$project$Session1$Session$ServerRespondedWithAllData = F5(
	function (a, b, c, d, e) {
		return {$: 'ServerRespondedWithAllData', a: a, b: b, c: c, d: d, e: e};
	});
var $author$project$Session1$Session$ServerRespondedWithSomeData = function (a) {
	return {$: 'ServerRespondedWithSomeData', a: a};
};
var $author$project$Session1$Session$ServerRespondedWithSomeError = function (a) {
	return {$: 'ServerRespondedWithSomeError', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedA5 = function (a) {
	return {$: 'LoadedA5', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedB5 = function (a) {
	return {$: 'LoadedB5', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedC5 = function (a) {
	return {$: 'LoadedC5', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedD5 = function (a) {
	return {$: 'LoadedD5', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$LoadedE5 = function (a) {
	return {$: 'LoadedE5', a: a};
};
var $0ui$elm_task_parallel$Task$Parallel$State5 = F6(
	function (a, b, c, d, e, f) {
		return {$: 'State5', a: a, b: b, c: c, d: d, e: e, f: f};
	});
var $0ui$elm_task_parallel$Task$Parallel$attempt5 = function (_v0) {
	var task1 = _v0.task1;
	var task2 = _v0.task2;
	var task3 = _v0.task3;
	var task4 = _v0.task4;
	var task5 = _v0.task5;
	var onUpdates = _v0.onUpdates;
	var onSuccess = _v0.onSuccess;
	var onFailure = _v0.onFailure;
	return _Utils_Tuple2(
		A6($0ui$elm_task_parallel$Task$Parallel$State5, onSuccess, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing),
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedA5),
					onFailure,
					task1),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedB5),
					onFailure,
					task2),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedC5),
					onFailure,
					task3),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedD5),
					onFailure,
					task4),
					A3(
					$0ui$elm_task_parallel$Task$Parallel$routeTo,
					A2($elm$core$Basics$composeL, onUpdates, $0ui$elm_task_parallel$Task$Parallel$LoadedE5),
					onFailure,
					task5)
				])));
};
var $author$project$Session1$ContextUnderstanding$Trial = F8(
	function (uid, text, target, distractor1, distractor2, distractor3, definition, isTraining) {
		return {definition: definition, distractor1: distractor1, distractor2: distractor2, distractor3: distractor3, isTraining: isTraining, target: target, text: text, uid: uid};
	});
var $author$project$Session1$ContextUnderstanding$decodeTranslationInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'Definition',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'Distractor_3_CU_Lvl1',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'Distractor_2_CU_Lvl1',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'Distractor_1_CU_Lvl1',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'Word_Text',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'Text_To_Complete',
								$elm$json$Json$Decode$string,
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'UID',
									$elm$json$Json$Decode$string,
									$elm$json$Json$Decode$succeed($author$project$Session1$ContextUnderstanding$Trial)))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session1$ContextUnderstanding$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session1$ContextUnderstanding$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session1$Meaning$Trial = F9(
	function (uid, writtenWord, target, distractor1, distractor2, distractor3, feedbackCorrect, feedbackIncorrect, isTraining) {
		return {distractor1: distractor1, distractor2: distractor2, distractor3: distractor3, feedbackCorrect: feedbackCorrect, feedbackIncorrect: feedbackIncorrect, isTraining: isTraining, target: target, uid: uid, writtenWord: writtenWord};
	});
var $author$project$Session1$Meaning$decodeMeaningInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'Feedback_Correct_Meaning',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'Feedback_Incorrect_Meaning',
				$elm$json$Json$Decode$string,
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'Distractor_3_Meaning',
					$elm$json$Json$Decode$string,
					'MISSING',
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'Distractor_2_Meaning',
						$elm$json$Json$Decode$string,
						'MISSING',
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'Distractor_1_Meaning',
							$elm$json$Json$Decode$string,
							'MISSING',
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'Definition',
								$elm$json$Json$Decode$string,
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'Word_Text',
									$elm$json$Json$Decode$string,
									A3(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'UID',
										$elm$json$Json$Decode$string,
										$elm$json$Json$Decode$succeed($author$project$Session1$Meaning$Trial))))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session1$Meaning$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session1$Meaning$decodeMeaningInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session1$Presentation$Trial = F8(
	function (uid, text, definition, example, translation1, translation2, audio, isTraining) {
		return {audio: audio, definition: definition, example: example, isTraining: isTraining, text: text, translation1: translation1, translation2: translation2, uid: uid};
	});
var $author$project$Session1$Presentation$decodeTranslationInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'Word_Audio',
			$author$project$Data$decodeAudioFiles,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'Translation_2',
				$elm$json$Json$Decode$string,
				'missing',
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'Translation_1',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'Example',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'Definition',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'Word_Text',
								$elm$json$Json$Decode$string,
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'UID',
									$elm$json$Json$Decode$string,
									$elm$json$Json$Decode$succeed($author$project$Session1$Presentation$Trial)))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session1$Presentation$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session1$Presentation$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session1$Spelling$Trial = F7(
	function (uid, target, distractor1, distractor2, distractor3, isTraining, audio) {
		return {audio: audio, distractor1: distractor1, distractor2: distractor2, distractor3: distractor3, isTraining: isTraining, target: target, uid: uid};
	});
var $author$project$Session1$Spelling$decodeTrials = function () {
	var stringToBoolDecoder = function (str) {
		if (str === 'true') {
			return $elm$json$Json$Decode$succeed(true);
		} else {
			return $elm$json$Json$Decode$succeed(false);
		}
	};
	var decoder = A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'Word_Audio',
		$author$project$Data$decodeAudioFiles,
		A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2(
				$elm$json$Json$Decode$andThen,
				stringToBoolDecoder,
				A2($elm$json$Json$Decode$field, 'isTraining', $elm$json$Json$Decode$string)),
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'Distractor_3_CCS',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'Distractor_2_CCS',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'Distractor_1_CCS',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'Word_Text',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'UID',
								$elm$json$Json$Decode$string,
								$elm$json$Json$Decode$succeed($author$project$Session1$Spelling$Trial))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session1$Spelling$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session1$Spelling$decodeTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session1$Session$getAll = $0ui$elm_task_parallel$Task$Parallel$attempt5(
	{onFailure: $author$project$Session1$Session$ServerRespondedWithSomeError, onSuccess: $author$project$Session1$Session$ServerRespondedWithAllData, onUpdates: $author$project$Session1$Session$ServerRespondedWithSomeData, task1: $author$project$Session1$Meaning$getRecords, task2: $author$project$Session1$Spelling$getRecords, task3: $author$project$Session1$ContextUnderstanding$getRecords, task4: $author$project$Session1$Presentation$getRecords, task5: $author$project$ExperimentInfo$getRecords});
var $author$project$Session2$Session$ServerRespondedWithAllData = F4(
	function (a, b, c, d) {
		return {$: 'ServerRespondedWithAllData', a: a, b: b, c: c, d: d};
	});
var $author$project$Session2$Session$ServerRespondedWithSomeData = function (a) {
	return {$: 'ServerRespondedWithSomeData', a: a};
};
var $author$project$Session2$Session$ServerRespondedWithSomeError = function (a) {
	return {$: 'ServerRespondedWithSomeError', a: a};
};
var $author$project$Session2$CU2$Trial = function (uid) {
	return function (writtenWord) {
		return function (audioSentence) {
			return function (context) {
				return function (target) {
					return function (distractor1) {
						return function (distractor2) {
							return function (distractor3) {
								return function (feedback) {
									return function (isTraining) {
										return {audioSentence: audioSentence, context: context, distractor1: distractor1, distractor2: distractor2, distractor3: distractor3, feedback: feedback, isTraining: isTraining, target: target, uid: uid, writtenWord: writtenWord};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Session2$CU2$decodeTranslationInput = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'Feedback_CU_Lvl2',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'CU_Lvl2_Distractor_3',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'CU_Lvl2_Distractor_2',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'CU_Lvl2_Distractor_1',
						$elm$json$Json$Decode$string,
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'CU_Lvl2_target',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'CU_Lvl1_Context',
								$elm$json$Json$Decode$string,
								A4(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
									'Audio_Understanding',
									$author$project$Data$decodeAudioFiles,
									A2($author$project$Data$AudioFile, '', ''),
									A3(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'Word_Text',
										$elm$json$Json$Decode$string,
										A3(
											$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
											'UID',
											$elm$json$Json$Decode$string,
											$elm$json$Json$Decode$succeed($author$project$Session2$CU2$Trial)))))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session2$CU2$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session2$CU2$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session2$Spelling$Trial = F5(
	function (uid, writtenWord, audioWord, isTraining, target) {
		return {audioWord: audioWord, isTraining: isTraining, target: target, uid: uid, writtenWord: writtenWord};
	});
var $author$project$Session2$Spelling$decodeTranslationInput = function () {
	var decoder = A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'Word_Text',
		$elm$json$Json$Decode$string,
		A2(
			$author$project$Data$decodeBool,
			'isTraining',
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'Word_Audio',
				$author$project$Data$decodeAudioFiles,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'Word_Text',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'UID',
						$elm$json$Json$Decode$string,
						$elm$json$Json$Decode$succeed($author$project$Session2$Spelling$Trial))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session2$Spelling$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session2$Spelling$decodeTranslationInput)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session2$Translation$Trial = F9(
	function (uid, question, target, translation2, distractor1, distractor2, distractor3, word, isTraining) {
		return {distractor1: distractor1, distractor2: distractor2, distractor3: distractor3, isTraining: isTraining, question: question, target: target, translation2: translation2, uid: uid, word: word};
	});
var $author$project$Session2$Translation$decodeTrials = function () {
	var decoder = A2(
		$author$project$Data$decodeBool,
		'isTraining',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'Word_Text',
			$elm$json$Json$Decode$string,
			'MISSING',
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'Distractor_3_Translation',
				$elm$json$Json$Decode$string,
				'missing distractor',
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'Distractor_2_Translation',
					$elm$json$Json$Decode$string,
					'Missing distractor',
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'Distractor_1_Translation',
						$elm$json$Json$Decode$string,
						'Missing distractor',
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'Translation_2',
							$elm$json$Json$Decode$string,
							'MISSING_TRANS_2',
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'Translation_1',
								$elm$json$Json$Decode$string,
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'Question_Translation',
									$elm$json$Json$Decode$string,
									A3(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'UID',
										$elm$json$Json$Decode$string,
										$elm$json$Json$Decode$succeed($author$project$Session2$Translation$Trial))))))))));
	return $author$project$Data$decodeRecords(decoder);
}();
var $author$project$Session2$Translation$getRecords = $elm$http$Http$task(
	{
		body: $elm$http$Http$emptyBody,
		headers: _List_Nil,
		method: 'GET',
		resolver: $elm$http$Http$stringResolver(
			$author$project$Data$handleJsonResponse($author$project$Session2$Translation$decodeTrials)),
		timeout: $elm$core$Maybe$Just(5000),
		url: $author$project$Data$buildQuery(
			{app: $author$project$Data$apps.spacing, base: 'input', view_: 'Presentation'})
	});
var $author$project$Session2$Session$getAll = $0ui$elm_task_parallel$Task$Parallel$attempt4(
	{onFailure: $author$project$Session2$Session$ServerRespondedWithSomeError, onSuccess: $author$project$Session2$Session$ServerRespondedWithAllData, onUpdates: $author$project$Session2$Session$ServerRespondedWithSomeData, task1: $author$project$Session2$CU2$getRecords, task2: $author$project$Session2$Spelling$getRecords, task3: $author$project$Session2$Translation$getRecords, task4: $author$project$ExperimentInfo$getRecords});
var $author$project$Main$pure = function (model) {
	return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
};
var $author$project$Session2$Spelling$UserDragsLetter = function (a) {
	return {$: 'UserDragsLetter', a: a};
};
var $annaghi$dnd_list$DnDList$Free = {$: 'Free'};
var $annaghi$dnd_list$DnDList$OnDrag = {$: 'OnDrag'};
var $annaghi$dnd_list$DnDList$Swap = {$: 'Swap'};
var $author$project$Session2$Spelling$config = {
	beforeUpdate: F3(
		function (_v0, _v1, list) {
			return list;
		}),
	listen: $annaghi$dnd_list$DnDList$OnDrag,
	movement: $annaghi$dnd_list$DnDList$Free,
	operation: $annaghi$dnd_list$DnDList$Swap
};
var $annaghi$dnd_list$DnDList$Model = function (a) {
	return {$: 'Model', a: a};
};
var $annaghi$dnd_list$DnDList$GotDragElement = function (a) {
	return {$: 'GotDragElement', a: a};
};
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $elm$browser$Browser$Dom$getElement = _Browser_getElement;
var $annaghi$dnd_list$DnDList$dragElementCommands = F2(
	function (stepMsg, state) {
		var _v0 = state.dragElement;
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$core$Task$attempt,
				A2($elm$core$Basics$composeL, stepMsg, $annaghi$dnd_list$DnDList$GotDragElement),
				$elm$browser$Browser$Dom$getElement(state.dragElementId));
		} else {
			return $elm$core$Platform$Cmd$none;
		}
	});
var $annaghi$dnd_list$DnDList$GotDropElement = function (a) {
	return {$: 'GotDropElement', a: a};
};
var $annaghi$dnd_list$DnDList$dropElementCommands = F2(
	function (stepMsg, state) {
		return (!state.dragCounter) ? A2(
			$elm$core$Task$attempt,
			A2($elm$core$Basics$composeL, stepMsg, $annaghi$dnd_list$DnDList$GotDropElement),
			$elm$browser$Browser$Dom$getElement(state.dropElementId)) : $elm$core$Platform$Cmd$none;
	});
var $annaghi$dnd_list$DnDList$commands = F2(
	function (stepMsg, _v0) {
		var model = _v0.a;
		if (model.$ === 'Nothing') {
			return $elm$core$Platform$Cmd$none;
		} else {
			var state = model.a;
			return $elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($annaghi$dnd_list$DnDList$dragElementCommands, stepMsg, state),
						A2($annaghi$dnd_list$DnDList$dropElementCommands, stepMsg, state)
					]));
		}
	});
var $annaghi$dnd_list$DnDList$DragStart = F3(
	function (a, b, c) {
		return {$: 'DragStart', a: a, b: b, c: c};
	});
var $annaghi$dnd_list$Internal$Common$Utils$Position = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $annaghi$dnd_list$Internal$Common$Utils$clientX = A2($elm$json$Json$Decode$field, 'clientX', $elm$json$Json$Decode$float);
var $annaghi$dnd_list$Internal$Common$Utils$clientY = A2($elm$json$Json$Decode$field, 'clientY', $elm$json$Json$Decode$float);
var $annaghi$dnd_list$Internal$Common$Utils$decodeCoordinates = A3($elm$json$Json$Decode$map2, $annaghi$dnd_list$Internal$Common$Utils$Position, $annaghi$dnd_list$Internal$Common$Utils$clientX, $annaghi$dnd_list$Internal$Common$Utils$clientY);
var $annaghi$dnd_list$Internal$Common$Utils$decodeMainMouseButton = function (decoder) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (button) {
			return (!button) ? decoder : $elm$json$Json$Decode$fail('Event is only relevant when the main mouse button was pressed.');
		},
		A2($elm$json$Json$Decode$field, 'button', $elm$json$Json$Decode$int));
};
var $annaghi$dnd_list$Internal$Common$Utils$decodeCoordinatesWithButtonCheck = $annaghi$dnd_list$Internal$Common$Utils$decodeMainMouseButton($annaghi$dnd_list$Internal$Common$Utils$decodeCoordinates);
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $annaghi$dnd_list$DnDList$dragEvents = F3(
	function (stepMsg, dragIndex, dragElementId) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$Events$preventDefaultOn,
				'mousedown',
				A2(
					$elm$json$Json$Decode$map,
					function (msg) {
						return _Utils_Tuple2(msg, true);
					},
					A2(
						$elm$json$Json$Decode$map,
						A2(
							$elm$core$Basics$composeL,
							stepMsg,
							A2($annaghi$dnd_list$DnDList$DragStart, dragIndex, dragElementId)),
						$annaghi$dnd_list$Internal$Common$Utils$decodeCoordinatesWithButtonCheck)))
			]);
	});
var $annaghi$dnd_list$DnDList$DragEnter = function (a) {
	return {$: 'DragEnter', a: a};
};
var $annaghi$dnd_list$DnDList$DragLeave = {$: 'DragLeave'};
var $annaghi$dnd_list$DnDList$DragOver = F2(
	function (a, b) {
		return {$: 'DragOver', a: a, b: b};
	});
var $elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseenter',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseOver = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		$elm$json$Json$Decode$succeed(msg));
};
var $annaghi$dnd_list$DnDList$dropEvents = F3(
	function (stepMsg, dropIndex, dropElementId) {
		return _List_fromArray(
			[
				$elm$html$Html$Events$onMouseOver(
				stepMsg(
					A2($annaghi$dnd_list$DnDList$DragOver, dropIndex, dropElementId))),
				$elm$html$Html$Events$onMouseEnter(
				stepMsg(
					$annaghi$dnd_list$DnDList$DragEnter(dropIndex))),
				$elm$html$Html$Events$onMouseLeave(
				stepMsg($annaghi$dnd_list$DnDList$DragLeave))
			]);
	});
var $annaghi$dnd_list$Internal$Common$Utils$px = function (n) {
	return $elm$core$String$fromInt(n) + 'px';
};
var $elm$core$Basics$round = _Basics_round;
var $annaghi$dnd_list$Internal$Common$Utils$translate = F2(
	function (x, y) {
		return 'translate3d(' + ($annaghi$dnd_list$Internal$Common$Utils$px(x) + (', ' + ($annaghi$dnd_list$Internal$Common$Utils$px(y) + ', 0)')));
	});
var $annaghi$dnd_list$DnDList$ghostStyles = F2(
	function (movement, _v0) {
		var model = _v0.a;
		if (model.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var state = model.a;
			var _v2 = state.dragElement;
			if (_v2.$ === 'Just') {
				var element = _v2.a.element;
				var viewport = _v2.a.viewport;
				var transform = function () {
					switch (movement.$) {
						case 'Horizontal':
							return A2(
								$elm$html$Html$Attributes$style,
								'transform',
								A2(
									$annaghi$dnd_list$Internal$Common$Utils$translate,
									$elm$core$Basics$round(((state.currentPosition.x - state.startPosition.x) + element.x) - viewport.x),
									$elm$core$Basics$round(element.y - viewport.y)));
						case 'Vertical':
							return A2(
								$elm$html$Html$Attributes$style,
								'transform',
								A2(
									$annaghi$dnd_list$Internal$Common$Utils$translate,
									$elm$core$Basics$round(element.x - viewport.x),
									$elm$core$Basics$round(((state.currentPosition.y - state.startPosition.y) + element.y) - viewport.y)));
						default:
							return A2(
								$elm$html$Html$Attributes$style,
								'transform',
								A2(
									$annaghi$dnd_list$Internal$Common$Utils$translate,
									$elm$core$Basics$round(((state.currentPosition.x - state.startPosition.x) + element.x) - viewport.x),
									$elm$core$Basics$round(((state.currentPosition.y - state.startPosition.y) + element.y) - viewport.y)));
					}
				}();
				var baseStyles = _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
						A2($elm$html$Html$Attributes$style, 'top', '0'),
						A2($elm$html$Html$Attributes$style, 'left', '0'),
						A2(
						$elm$html$Html$Attributes$style,
						'height',
						$annaghi$dnd_list$Internal$Common$Utils$px(
							$elm$core$Basics$round(element.height))),
						A2(
						$elm$html$Html$Attributes$style,
						'width',
						$annaghi$dnd_list$Internal$Common$Utils$px(
							$elm$core$Basics$round(element.width))),
						A2($elm$html$Html$Attributes$style, 'pointer-events', 'none')
					]);
				return A2($elm$core$List$cons, transform, baseStyles);
			} else {
				return _List_Nil;
			}
		}
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $annaghi$dnd_list$DnDList$info = function (_v0) {
	var model = _v0.a;
	return A2(
		$elm$core$Maybe$andThen,
		function (state) {
			return A3(
				$elm$core$Maybe$map2,
				F2(
					function (dragElement, dropElement) {
						return {currentPosition: state.currentPosition, dragElement: dragElement, dragElementId: state.dragElementId, dragIndex: state.dragIndex, dropElement: dropElement, dropElementId: state.dropElementId, dropIndex: state.dropIndex, startPosition: state.startPosition};
					}),
				state.dragElement,
				state.dropElement);
		},
		model);
};
var $annaghi$dnd_list$DnDList$Drag = function (a) {
	return {$: 'Drag', a: a};
};
var $annaghi$dnd_list$DnDList$DragEnd = {$: 'DragEnd'};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onMouseMove = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mousemove');
var $elm$browser$Browser$Events$onMouseUp = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'mouseup');
var $annaghi$dnd_list$DnDList$subscriptions = F2(
	function (stepMsg, _v0) {
		var model = _v0.a;
		if (model.$ === 'Nothing') {
			return $elm$core$Platform$Sub$none;
		} else {
			return $elm$core$Platform$Sub$batch(
				_List_fromArray(
					[
						$elm$browser$Browser$Events$onMouseMove(
						A2(
							$elm$json$Json$Decode$map,
							A2($elm$core$Basics$composeL, stepMsg, $annaghi$dnd_list$DnDList$Drag),
							$annaghi$dnd_list$Internal$Common$Utils$decodeCoordinates)),
						$elm$browser$Browser$Events$onMouseUp(
						$elm$json$Json$Decode$succeed(
							stepMsg($annaghi$dnd_list$DnDList$DragEnd)))
					]));
		}
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $annaghi$dnd_list$Internal$Common$Operations$splitAt = F2(
	function (i, list) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, i, list),
			A2($elm$core$List$drop, i, list));
	});
var $annaghi$dnd_list$Internal$Common$Operations$afterBackward = F3(
	function (i, j, list) {
		var _v0 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, j + 1, list);
		var beginning = _v0.a;
		var rest = _v0.b;
		var _v1 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, (i - j) - 1, rest);
		var middle = _v1.a;
		var end = _v1.b;
		var _v2 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, 1, end);
		var head = _v2.a;
		var tail = _v2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				head,
				_Utils_ap(middle, tail)));
	});
var $annaghi$dnd_list$Internal$Common$Operations$afterForward = F3(
	function (i, j, list) {
		var _v0 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, i, list);
		var beginning = _v0.a;
		var rest = _v0.b;
		var _v1 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, (j - i) + 1, rest);
		var middle = _v1.a;
		var end = _v1.b;
		var _v2 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, 1, middle);
		var head = _v2.a;
		var tail = _v2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				tail,
				_Utils_ap(head, end)));
	});
var $annaghi$dnd_list$Internal$Common$Operations$insertAfter = F3(
	function (dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$afterForward, dragIndex, dropIndex, list) : ((_Utils_cmp(dropIndex, dragIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$afterBackward, dragIndex, dropIndex, list) : list);
	});
var $annaghi$dnd_list$Internal$Common$Operations$beforeBackward = F3(
	function (i, j, list) {
		var _v0 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, j, list);
		var beginning = _v0.a;
		var rest = _v0.b;
		var _v1 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, i - j, rest);
		var middle = _v1.a;
		var end = _v1.b;
		var _v2 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, 1, end);
		var head = _v2.a;
		var tail = _v2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				head,
				_Utils_ap(middle, tail)));
	});
var $annaghi$dnd_list$Internal$Common$Operations$beforeForward = F3(
	function (i, j, list) {
		var _v0 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, i, list);
		var beginning = _v0.a;
		var rest = _v0.b;
		var _v1 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, j - i, rest);
		var middle = _v1.a;
		var end = _v1.b;
		var _v2 = A2($annaghi$dnd_list$Internal$Common$Operations$splitAt, 1, middle);
		var head = _v2.a;
		var tail = _v2.b;
		return _Utils_ap(
			beginning,
			_Utils_ap(
				tail,
				_Utils_ap(head, end)));
	});
var $annaghi$dnd_list$Internal$Common$Operations$insertBefore = F3(
	function (dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$beforeForward, dragIndex, dropIndex, list) : ((_Utils_cmp(dropIndex, dragIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$beforeBackward, dragIndex, dropIndex, list) : list);
	});
var $annaghi$dnd_list$Internal$Common$Operations$rotate = F3(
	function (dragIndex, dropIndex, list) {
		return (_Utils_cmp(dragIndex, dropIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$afterForward, dragIndex, dropIndex, list) : ((_Utils_cmp(dropIndex, dragIndex) < 0) ? A3($annaghi$dnd_list$Internal$Common$Operations$beforeBackward, dragIndex, dropIndex, list) : list);
	});
var $annaghi$dnd_list$Internal$Common$Operations$swapAt = F3(
	function (i, j, list) {
		var item_j = A2(
			$elm$core$List$take,
			1,
			A2($elm$core$List$drop, j, list));
		var item_i = A2(
			$elm$core$List$take,
			1,
			A2($elm$core$List$drop, i, list));
		return $elm$core$List$concat(
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (index, item) {
						return _Utils_eq(index, i) ? item_j : (_Utils_eq(index, j) ? item_i : _List_fromArray(
							[item]));
					}),
				list));
	});
var $annaghi$dnd_list$Internal$Common$Operations$swap = F3(
	function (dragIndex, dropIndex, list) {
		return (!_Utils_eq(dragIndex, dropIndex)) ? A3($annaghi$dnd_list$Internal$Common$Operations$swapAt, dragIndex, dropIndex, list) : list;
	});
var $annaghi$dnd_list$DnDList$listUpdate = F4(
	function (operation, dragIndex, dropIndex, list) {
		switch (operation.$) {
			case 'InsertAfter':
				return A3($annaghi$dnd_list$Internal$Common$Operations$insertAfter, dragIndex, dropIndex, list);
			case 'InsertBefore':
				return A3($annaghi$dnd_list$Internal$Common$Operations$insertBefore, dragIndex, dropIndex, list);
			case 'Rotate':
				return A3($annaghi$dnd_list$Internal$Common$Operations$rotate, dragIndex, dropIndex, list);
			case 'Swap':
				return A3($annaghi$dnd_list$Internal$Common$Operations$swap, dragIndex, dropIndex, list);
			default:
				return list;
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $annaghi$dnd_list$DnDList$stateUpdate = F3(
	function (operation, dropIndex, state) {
		switch (operation.$) {
			case 'InsertAfter':
				return _Utils_update(
					state,
					{
						dragCounter: 0,
						dragIndex: (_Utils_cmp(dropIndex, state.dragIndex) < 0) ? (dropIndex + 1) : dropIndex
					});
			case 'InsertBefore':
				return _Utils_update(
					state,
					{
						dragCounter: 0,
						dragIndex: (_Utils_cmp(state.dragIndex, dropIndex) < 0) ? (dropIndex - 1) : dropIndex
					});
			case 'Rotate':
				return _Utils_update(
					state,
					{dragCounter: 0, dragIndex: dropIndex});
			case 'Swap':
				return _Utils_update(
					state,
					{dragCounter: 0, dragIndex: dropIndex});
			default:
				return _Utils_update(
					state,
					{dragCounter: 0});
		}
	});
var $annaghi$dnd_list$DnDList$update = F4(
	function (_v0, msg, _v1, list) {
		var beforeUpdate = _v0.beforeUpdate;
		var listen = _v0.listen;
		var operation = _v0.operation;
		var model = _v1.a;
		switch (msg.$) {
			case 'DragStart':
				var dragIndex = msg.a;
				var dragElementId = msg.b;
				var xy = msg.c;
				return _Utils_Tuple2(
					$annaghi$dnd_list$DnDList$Model(
						$elm$core$Maybe$Just(
							{currentPosition: xy, dragCounter: 0, dragElement: $elm$core$Maybe$Nothing, dragElementId: dragElementId, dragIndex: dragIndex, dropElement: $elm$core$Maybe$Nothing, dropElementId: dragElementId, dropIndex: dragIndex, startPosition: xy})),
					list);
			case 'Drag':
				var xy = msg.a;
				return _Utils_Tuple2(
					$annaghi$dnd_list$DnDList$Model(
						A2(
							$elm$core$Maybe$map,
							function (state) {
								return _Utils_update(
									state,
									{currentPosition: xy, dragCounter: state.dragCounter + 1});
							},
							model)),
					list);
			case 'DragOver':
				var dropIndex = msg.a;
				var dropElementId = msg.b;
				return _Utils_Tuple2(
					$annaghi$dnd_list$DnDList$Model(
						A2(
							$elm$core$Maybe$map,
							function (state) {
								return _Utils_update(
									state,
									{dropElementId: dropElementId, dropIndex: dropIndex});
							},
							model)),
					list);
			case 'DragEnter':
				var dropIndex = msg.a;
				var _v3 = _Utils_Tuple2(model, listen);
				if ((_v3.a.$ === 'Just') && (_v3.b.$ === 'OnDrag')) {
					var state = _v3.a.a;
					var _v4 = _v3.b;
					return ((state.dragCounter > 1) && (!_Utils_eq(state.dragIndex, dropIndex))) ? _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(
							$elm$core$Maybe$Just(
								A3($annaghi$dnd_list$DnDList$stateUpdate, operation, dropIndex, state))),
						A4(
							$annaghi$dnd_list$DnDList$listUpdate,
							operation,
							state.dragIndex,
							dropIndex,
							A3(beforeUpdate, state.dragIndex, dropIndex, list))) : _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(model),
						list);
				} else {
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(
							A2(
								$elm$core$Maybe$map,
								function (state) {
									return _Utils_update(
										state,
										{dragCounter: 0});
								},
								model)),
						list);
				}
			case 'DragLeave':
				return _Utils_Tuple2(
					$annaghi$dnd_list$DnDList$Model(
						A2(
							$elm$core$Maybe$map,
							function (state) {
								return _Utils_update(
									state,
									{dropIndex: state.dragIndex});
							},
							model)),
					list);
			case 'DragEnd':
				var _v5 = _Utils_Tuple2(model, listen);
				if ((_v5.a.$ === 'Just') && (_v5.b.$ === 'OnDrop')) {
					var state = _v5.a.a;
					var _v6 = _v5.b;
					return (!_Utils_eq(state.dragIndex, state.dropIndex)) ? _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model($elm$core$Maybe$Nothing),
						A4(
							$annaghi$dnd_list$DnDList$listUpdate,
							operation,
							state.dragIndex,
							state.dropIndex,
							A3(beforeUpdate, state.dragIndex, state.dropIndex, list))) : _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model($elm$core$Maybe$Nothing),
						list);
				} else {
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model($elm$core$Maybe$Nothing),
						list);
				}
			case 'GotDragElement':
				if (msg.a.$ === 'Err') {
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(model),
						list);
				} else {
					var dragElement = msg.a.a;
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(
							A2(
								$elm$core$Maybe$map,
								function (state) {
									return _Utils_update(
										state,
										{
											dragElement: $elm$core$Maybe$Just(dragElement),
											dropElement: $elm$core$Maybe$Just(dragElement)
										});
								},
								model)),
						list);
				}
			default:
				if (msg.a.$ === 'Err') {
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(model),
						list);
				} else {
					var dropElement = msg.a.a;
					return _Utils_Tuple2(
						$annaghi$dnd_list$DnDList$Model(
							A2(
								$elm$core$Maybe$map,
								function (state) {
									return _Utils_update(
										state,
										{
											dropElement: $elm$core$Maybe$Just(dropElement)
										});
								},
								model)),
						list);
				}
		}
	});
var $annaghi$dnd_list$DnDList$create = F2(
	function (config, stepMsg) {
		return {
			commands: $annaghi$dnd_list$DnDList$commands(stepMsg),
			dragEvents: $annaghi$dnd_list$DnDList$dragEvents(stepMsg),
			dropEvents: $annaghi$dnd_list$DnDList$dropEvents(stepMsg),
			ghostStyles: $annaghi$dnd_list$DnDList$ghostStyles(config.movement),
			info: $annaghi$dnd_list$DnDList$info,
			model: $annaghi$dnd_list$DnDList$Model($elm$core$Maybe$Nothing),
			subscriptions: $annaghi$dnd_list$DnDList$subscriptions(stepMsg),
			update: $annaghi$dnd_list$DnDList$update(config)
		};
	});
var $author$project$Session2$Spelling$system = A2($annaghi$dnd_list$DnDList$create, $author$project$Session2$Spelling$config, $author$project$Session2$Spelling$UserDragsLetter);
var $author$project$Postest$CloudWords$NotKnown = {$: 'NotKnown'};
var $author$project$Postest$CloudWords$words = _List_fromArray(
	[
		_Utils_Tuple2('crave', $author$project$Postest$CloudWords$NotKnown)
	]);
var $author$project$Main$init = F3(
	function (_v0, url, key) {
		var route = $author$project$Route$fromUrl(url);
		var defaultInit = {
			acceptabilityTask: $author$project$Logic$NotStarted,
			cloudWords: $elm$core$Dict$fromList($author$project$Postest$CloudWords$words),
			cu1: $author$project$Logic$Loading,
			cu3: $author$project$Logic$NotStarted,
			cuLvl2: $author$project$Logic$NotStarted,
			dnd: $author$project$Session2$Spelling$system.model,
			endAcceptabilityDuration: 6000,
			errorTracking: _List_Nil,
			informations: '',
			infos: $krisajenkins$remotedata$RemoteData$Loading,
			key: key,
			meaning: $author$project$Logic$NotStarted,
			optionsOrder: _List_fromArray(
				[0, 1, 2, 3]),
			packetsSended: 0,
			pilote: $author$project$Session$NotAsked,
			presentation: $author$project$Logic$NotStarted,
			pretest: $author$project$Pretest$Pretest$attempt.a,
			route: route,
			scrabbleTask: $author$project$Logic$NotStarted,
			sentenceCompletion: $author$project$Logic$NotStarted,
			session1: $author$project$Session$NotAsked,
			session2: $author$project$Session$NotAsked,
			session3: $author$project$Session$NotAsked,
			spelling3: $author$project$Logic$NotStarted,
			spellingLvl1: $author$project$Logic$NotStarted,
			spr: $author$project$Logic$NotStarted,
			synonymTask: $author$project$Logic$NotStarted,
			translationTask: $author$project$Logic$NotStarted,
			user: url.query,
			vks: $author$project$Logic$NotStarted,
			yn: $author$project$Logic$Loading
		};
		var _v1 = $author$project$Session3$Session$attempt;
		var loadingStateSession3 = _v1.a;
		var fetchSession3 = _v1.b;
		var _v2 = $author$project$Session2$Session$getAll;
		var loadingStateSession2 = _v2.a;
		var fetchSession2 = _v2.b;
		var _v3 = $author$project$Session1$Session$getAll;
		var loadingStateSession1 = _v3.a;
		var fetchSession1 = _v3.b;
		switch (route.$) {
			case 'Session1':
				var userId = route.a;
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{
							cu1: $author$project$Logic$Loading,
							meaning: $author$project$Logic$Loading,
							presentation: $author$project$Logic$Loading,
							session1: $author$project$Session$Loading(loadingStateSession1),
							spellingLvl1: $author$project$Logic$Loading,
							user: $elm$core$Maybe$Just(userId)
						}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session1, fetchSession1));
			case 'Home':
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{
							cu1: $author$project$Logic$Loading,
							meaning: $author$project$Logic$Loading,
							presentation: $author$project$Logic$Loading,
							session1: $author$project$Session$Loading(loadingStateSession1),
							spellingLvl1: $author$project$Logic$Loading,
							user: $elm$core$Maybe$Nothing
						}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session1, fetchSession1));
			case 'AuthenticatedSession2':
				var userid = route.a;
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{
							cuLvl2: $author$project$Logic$Loading,
							scrabbleTask: $author$project$Logic$Loading,
							session2: $author$project$Session$Loading(loadingStateSession2),
							translationTask: $author$project$Logic$Loading,
							user: $elm$core$Maybe$Just(userid)
						}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session2, fetchSession2));
			case 'AuthenticatedSession3':
				var userid = route.a;
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{
							cu3: $author$project$Logic$Loading,
							session3: $author$project$Session$Loading(loadingStateSession3),
							spelling3: $author$project$Logic$Loading,
							synonymTask: $author$project$Logic$Loading,
							user: $elm$core$Maybe$Just(userid)
						}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session3, fetchSession3));
			case 'Pretest':
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{sentenceCompletion: $author$project$Logic$Loading, spr: $author$project$Logic$Loading}),
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Pretest, $author$project$Pretest$Pretest$attempt.b));
			case 'Pilote':
				var userid = route.a;
				return _Utils_Tuple2(
					_Utils_update(
						defaultInit,
						{
							acceptabilityTask: $author$project$Logic$Loading,
							pilote: $author$project$Session$NotAsked,
							user: $elm$core$Maybe$Just(userid)
						}),
					$elm$core$Platform$Cmd$none);
			case 'Posttest':
				return $author$project$Main$pure(defaultInit);
			default:
				return $author$project$Main$pure(
					_Utils_update(
						defaultInit,
						{route: $author$project$Route$NotFound}));
		}
	});
var $author$project$Pretest$Acceptability$Answering = {$: 'Answering'};
var $author$project$Pretest$Acceptability$Init = {$: 'Init'};
var $author$project$Main$SPR = function (a) {
	return {$: 'SPR', a: a};
};
var $author$project$Main$Spelling2 = function (a) {
	return {$: 'Spelling2', a: a};
};
var $author$project$Main$audioEnded = _Platform_incomingPort(
	'audioEnded',
	A2(
		$elm$json$Json$Decode$andThen,
		function (timestamp) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (name) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (eventType) {
							return $elm$json$Json$Decode$succeed(
								{eventType: eventType, name: name, timestamp: timestamp});
						},
						A2($elm$json$Json$Decode$field, 'eventType', $elm$json$Json$Decode$string));
				},
				A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$int)));
var $author$project$Main$Acceptability = function (a) {
	return {$: 'Acceptability', a: a};
};
var $author$project$Pretest$Acceptability$NextStepCinematic = function (a) {
	return {$: 'NextStepCinematic', a: a};
};
var $author$project$Main$NoOp = {$: 'NoOp'};
var $author$project$Pretest$Acceptability$Start = {$: 'Start'};
var $author$project$Main$decodeSpace = A2(
	$elm$json$Json$Decode$map,
	function (k) {
		if (k === ' ') {
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$NextStepCinematic($author$project$Pretest$Acceptability$Start));
		} else {
			return $author$project$Main$NoOp;
		}
	},
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $author$project$Logic$getState = function (task) {
	if (task.$ === 'Running') {
		var state = task.b.state;
		return $elm$core$Maybe$Just(state);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Pretest$Acceptability$UserPressedButton = function (a) {
	return {$: 'UserPressedButton', a: a};
};
var $author$project$Main$toEvaluation = function (x) {
	switch (x) {
		case 'j':
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$UserPressedButton(
					$elm$core$Maybe$Just(true)));
		case 'f':
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$UserPressedButton(
					$elm$core$Maybe$Just(false)));
		default:
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$UserPressedButton($elm$core$Maybe$Nothing));
	}
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$toEvaluation,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keydown');
var $author$project$Pretest$SPR$TimestampedMsg = F2(
	function (a, b) {
		return {$: 'TimestampedMsg', a: a, b: b};
	});
var $author$project$Pretest$SPR$UserPressedSpaceToReadNextSegment = {$: 'UserPressedSpaceToReadNextSegment'};
var $author$project$Pretest$SPR$UserPressedSpaceToStartParagraph = {$: 'UserPressedSpaceToStartParagraph'};
var $author$project$Pretest$SPR$NoOp = {$: 'NoOp'};
var $author$project$Pretest$SPR$decodeSpace = function (msg) {
	return A2(
		$elm$json$Json$Decode$map,
		function (k) {
			if (k === ' ') {
				return msg;
			} else {
				return $author$project$Pretest$SPR$NoOp;
			}
		},
		A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
};
var $author$project$Pretest$SPR$No = {$: 'No'};
var $author$project$Pretest$SPR$Unsure = {$: 'Unsure'};
var $author$project$Pretest$SPR$UserClickedNextTrial = function (a) {
	return {$: 'UserClickedNextTrial', a: a};
};
var $author$project$Pretest$SPR$Yes = {$: 'Yes'};
var $author$project$Pretest$SPR$decodeYesNoUnsure = A2(
	$elm$json$Json$Decode$map,
	function (k) {
		switch (k) {
			case 'y':
				return $author$project$Pretest$SPR$UserClickedNextTrial($author$project$Pretest$SPR$Yes);
			case 'n':
				return $author$project$Pretest$SPR$UserClickedNextTrial($author$project$Pretest$SPR$No);
			case 'k':
				return $author$project$Pretest$SPR$UserClickedNextTrial($author$project$Pretest$SPR$Unsure);
			default:
				return $author$project$Pretest$SPR$NoOp;
		}
	},
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $author$project$Pretest$SPR$UserConfirmedChoice = function (a) {
	return {$: 'UserConfirmedChoice', a: a};
};
var $author$project$Pretest$SPR$decodeYesNoUnsureInTraining = A2(
	$elm$json$Json$Decode$map,
	function (k) {
		switch (k) {
			case 'y':
				return $author$project$Pretest$SPR$UserConfirmedChoice($author$project$Pretest$SPR$Yes);
			case 'n':
				return $author$project$Pretest$SPR$UserConfirmedChoice($author$project$Pretest$SPR$No);
			case 'k':
				return $author$project$Pretest$SPR$UserConfirmedChoice($author$project$Pretest$SPR$Unsure);
			default:
				return $author$project$Pretest$SPR$NoOp;
		}
	},
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $author$project$Pretest$SPR$subscriptions = function (task) {
	_v0$2:
	while (true) {
		if (task.$ === 'Running') {
			switch (task.a.$) {
				case 'Training':
					var _v1 = task.a;
					var data = task.b;
					var _v2 = data.state.step;
					switch (_v2.$) {
						case 'SPR':
							var step = _v2.a;
							if (step.$ === 'Start') {
								return $elm$browser$Browser$Events$onKeyDown(
									$author$project$Pretest$SPR$decodeSpace(
										A2($author$project$Pretest$SPR$TimestampedMsg, $author$project$Pretest$SPR$UserPressedSpaceToStartParagraph, $elm$core$Maybe$Nothing)));
							} else {
								return $elm$browser$Browser$Events$onKeyDown(
									$author$project$Pretest$SPR$decodeSpace(
										A2($author$project$Pretest$SPR$TimestampedMsg, $author$project$Pretest$SPR$UserPressedSpaceToReadNextSegment, $elm$core$Maybe$Nothing)));
							}
						case 'Feedback':
							return $elm$browser$Browser$Events$onKeyDown(
								$author$project$Pretest$SPR$decodeSpace(
									A2($author$project$Pretest$SPR$TimestampedMsg, $author$project$Pretest$SPR$UserPressedSpaceToStartParagraph, $elm$core$Maybe$Nothing)));
						default:
							return $elm$browser$Browser$Events$onKeyDown($author$project$Pretest$SPR$decodeYesNoUnsureInTraining);
					}
				case 'Main':
					var _v4 = task.a;
					var data = task.b;
					var _v5 = data.state.step;
					switch (_v5.$) {
						case 'SPR':
							var step = _v5.a;
							if (step.$ === 'Start') {
								return $elm$browser$Browser$Events$onKeyDown(
									$author$project$Pretest$SPR$decodeSpace(
										A2($author$project$Pretest$SPR$TimestampedMsg, $author$project$Pretest$SPR$UserPressedSpaceToStartParagraph, $elm$core$Maybe$Nothing)));
							} else {
								return $elm$browser$Browser$Events$onKeyDown(
									$author$project$Pretest$SPR$decodeSpace(
										A2($author$project$Pretest$SPR$TimestampedMsg, $author$project$Pretest$SPR$UserPressedSpaceToReadNextSegment, $elm$core$Maybe$Nothing)));
							}
						case 'Question':
							return $elm$browser$Browser$Events$onKeyDown($author$project$Pretest$SPR$decodeYesNoUnsure);
						default:
							return $elm$core$Platform$Sub$none;
					}
				default:
					break _v0$2;
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Platform$Sub$none;
};
var $author$project$Session2$Spelling$subscriptions = function (model) {
	return $author$project$Session2$Spelling$system.subscriptions(model.dnd);
};
var $author$project$Pretest$Acceptability$AudioEnded = function (a) {
	return {$: 'AudioEnded', a: a};
};
var $author$project$Pretest$Acceptability$AudioStarted = function (a) {
	return {$: 'AudioStarted', a: a};
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $author$project$Main$toAcceptabilityMessage = function (_v0) {
	var eventType = _v0.eventType;
	var name = _v0.name;
	var timestamp = _v0.timestamp;
	switch (eventType) {
		case 'SoundStarted':
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$AudioStarted(
					_Utils_Tuple2(
						name,
						$elm$time$Time$millisToPosix(timestamp))));
		case 'SoundEnded':
			return $author$project$Main$Acceptability(
				$author$project$Pretest$Acceptability$AudioEnded(
					_Utils_Tuple2(
						name,
						$elm$time$Time$millisToPosix(timestamp))));
		default:
			return $author$project$Main$NoOp;
	}
};
var $author$project$Main$subscriptions = function (model) {
	var acceptabilityState = $author$project$Logic$getState(model.acceptabilityTask);
	var listenToInput = function () {
		if (acceptabilityState.$ === 'Just') {
			var state = acceptabilityState.a;
			return _Utils_eq(state.step, $author$project$Pretest$Acceptability$Answering) ? $elm$browser$Browser$Events$onKeyDown($author$project$Main$keyDecoder) : (_Utils_eq(state.step, $author$project$Pretest$Acceptability$Init) ? $elm$browser$Browser$Events$onKeyDown($author$project$Main$decodeSpace) : $elm$core$Platform$Sub$none);
		} else {
			return $elm$core$Platform$Sub$none;
		}
	}();
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$Spelling2,
				$author$project$Session2$Spelling$subscriptions(model)),
				listenToInput,
				$author$project$Main$audioEnded($author$project$Main$toAcceptabilityMessage),
				A2(
				$elm$core$Platform$Sub$map,
				$author$project$Main$SPR,
				$author$project$Pretest$SPR$subscriptions(model.spr))
			]));
};
var $author$project$Main$CU1 = function (a) {
	return {$: 'CU1', a: a};
};
var $author$project$Main$CU2 = function (a) {
	return {$: 'CU2', a: a};
};
var $author$project$Main$CU3 = function (a) {
	return {$: 'CU3', a: a};
};
var $author$project$Pretest$Acceptability$Distractor = {$: 'Distractor'};
var $author$project$Pretest$Acceptability$End = {$: 'End'};
var $author$project$Logic$Err = function (a) {
	return {$: 'Err', a: a};
};
var $author$project$Pretest$Acceptability$Listening = {$: 'Listening'};
var $author$project$Main$Meaning = function (a) {
	return {$: 'Meaning', a: a};
};
var $author$project$Main$PlaysoundInJS = function (a) {
	return {$: 'PlaysoundInJS', a: a};
};
var $author$project$Main$Presentation = function (a) {
	return {$: 'Presentation', a: a};
};
var $author$project$Pretest$Acceptability$RuntimeShuffledTrials = F2(
	function (a, b) {
		return {$: 'RuntimeShuffledTrials', a: a, b: b};
	});
var $author$project$Main$SentenceCompletion = function (a) {
	return {$: 'SentenceCompletion', a: a};
};
var $author$project$Main$ServerRespondedWithAllPretestData = F2(
	function (a, b) {
		return {$: 'ServerRespondedWithAllPretestData', a: a, b: b};
	});
var $author$project$Pretest$Acceptability$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Main$Spelling1 = function (a) {
	return {$: 'Spelling1', a: a};
};
var $author$project$Main$Spelling3 = function (a) {
	return {$: 'Spelling3', a: a};
};
var $krisajenkins$remotedata$RemoteData$Success = function (a) {
	return {$: 'Success', a: a};
};
var $author$project$Main$Synonym = function (a) {
	return {$: 'Synonym', a: a};
};
var $author$project$Pretest$Acceptability$Target = {$: 'Target'};
var $author$project$Pretest$Acceptability$Training = {$: 'Training'};
var $author$project$Main$Translation = function (a) {
	return {$: 'Translation', a: a};
};
var $author$project$Pretest$Acceptability$UserPressedButtonWithTimestamp = F2(
	function (a, b) {
		return {$: 'UserPressedButtonWithTimestamp', a: a, b: b};
	});
var $author$project$Main$VKS = function (a) {
	return {$: 'VKS', a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $andrewMacmurray$elm_delay$Delay$after = F2(
	function (time, msg) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$always(msg),
			$elm$core$Process$sleep(time));
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$andThen = F2(
	function (callback, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				var _v1 = genA(seed);
				var result = _v1.a;
				var newSeed = _v1.b;
				var _v2 = callback(result);
				var genB = _v2.a;
				return genB(newSeed);
			});
	});
var $author$project$Main$beep = 'https://dl.airtable.com/.attachments/b000c72585c5f5145828b1cf3916c38d/88d9c821/beep.mp3';
var $author$project$Data$buildErrorMessage = function (httpError) {
	switch (httpError.$) {
		case 'BadUrl':
			var message = httpError.a;
			return message;
		case 'Timeout':
			return 'Server is taking too long to respond. Please try again later.';
		case 'NetworkError':
			return 'Unable to reach server.';
		case 'BadStatus':
			var statusCode = httpError.a;
			return 'Request failed with status code: ' + $elm$core$String$fromInt(statusCode);
		default:
			var message = httpError.a;
			return message;
	}
};
var $elm$random$Random$constant = function (value) {
	return $elm$random$Random$Generator(
		function (seed) {
			return _Utils_Tuple2(value, seed);
		});
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $author$project$Logic$getTrial = function (task) {
	if (task.$ === 'Running') {
		var current = task.b.current;
		return current;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Postest$YN$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Postest$YN$initState = A2($author$project$Postest$YN$State, 'DefaultUid', '');
var $author$project$Pretest$Acceptability$NoEvaluation = {$: 'NoEvaluation'};
var $author$project$Pretest$Acceptability$initState = {audioEndedAt: $elm$core$Maybe$Nothing, audioStartedAt: $elm$core$Maybe$Nothing, beepEndedAt: $elm$core$Maybe$Nothing, beepStartedAt: $elm$core$Maybe$Nothing, evaluation: $author$project$Pretest$Acceptability$NoEvaluation, step: $author$project$Pretest$Acceptability$Init, trialuid: 'defaulttrialuid', userAnsweredAt: $elm$core$Maybe$Nothing};
var $author$project$Session1$ContextUnderstanding$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session1$ContextUnderstanding$initState = A2($author$project$Session1$ContextUnderstanding$State, 'DefaultUid', '');
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $author$project$Pretest$Acceptability$EvaluationTimeOut = {$: 'EvaluationTimeOut'};
var $author$project$Pretest$Acceptability$SentenceCorrect = {$: 'SentenceCorrect'};
var $author$project$Pretest$Acceptability$SentenceIncorrect = {$: 'SentenceIncorrect'};
var $author$project$Pretest$Acceptability$maybeBoolToEvaluation = function (maybeBool) {
	if (maybeBool.$ === 'Nothing') {
		return $author$project$Pretest$Acceptability$EvaluationTimeOut;
	} else {
		if (maybeBool.a) {
			return $author$project$Pretest$Acceptability$SentenceCorrect;
		} else {
			return $author$project$Pretest$Acceptability$SentenceIncorrect;
		}
	}
};
var $author$project$Pretest$Acceptability$newLoop = _Utils_update(
	$author$project$Pretest$Acceptability$initState,
	{step: $author$project$Pretest$Acceptability$Start});
var $author$project$Logic$Main = {$: 'Main'};
var $author$project$Logic$Running = F2(
	function (a, b) {
		return {$: 'Running', a: a, b: b};
	});
var $author$project$Logic$Training = {$: 'Training'};
var $author$project$Logic$next = F2(
	function (resetedState, task) {
		_v0$3:
		while (true) {
			switch (task.$) {
				case 'Running':
					switch (task.a.$) {
						case 'Training':
							var _v1 = task.a;
							var data = task.b;
							var _v2 = data.trainingTrials;
							if (_v2.b) {
								if (!_v2.b.b) {
									var last = _v2.a;
									return A2(
										$author$project$Logic$Running,
										$author$project$Logic$Training,
										_Utils_update(
											data,
											{
												current: $elm$core$Maybe$Nothing,
												feedback: !data.feedback,
												history: A2(
													$elm$core$List$cons,
													_Utils_Tuple2(last, data.state),
													data.history),
												next: $elm$core$Maybe$Nothing,
												state: resetedState,
												trainingTrials: _List_Nil
											}));
								} else {
									if (_v2.b.b.b) {
										var x = _v2.a;
										var _v3 = _v2.b;
										var y = _v3.a;
										var _v4 = _v3.b;
										var z = _v4.a;
										var zs = _v4.b;
										return A2(
											$author$project$Logic$Running,
											$author$project$Logic$Training,
											_Utils_update(
												data,
												{
													current: $elm$core$Maybe$Just(y),
													feedback: !data.feedback,
													history: A2(
														$elm$core$List$cons,
														_Utils_Tuple2(x, data.state),
														data.history),
													next: $elm$core$Maybe$Just(z),
													state: resetedState,
													trainingTrials: A2(
														$elm$core$List$cons,
														y,
														A2($elm$core$List$cons, z, zs))
												}));
									} else {
										var x = _v2.a;
										var _v5 = _v2.b;
										var y = _v5.a;
										return A2(
											$author$project$Logic$Running,
											$author$project$Logic$Training,
											_Utils_update(
												data,
												{
													current: $elm$core$Maybe$Just(y),
													feedback: !data.feedback,
													history: A2(
														$elm$core$List$cons,
														_Utils_Tuple2(x, data.state),
														data.history),
													next: $elm$core$Maybe$Nothing,
													state: resetedState,
													trainingTrials: _List_fromArray(
														[y])
												}));
									}
								}
							} else {
								return A2(
									$author$project$Logic$Running,
									$author$project$Logic$Training,
									_Utils_update(
										data,
										{current: $elm$core$Maybe$Nothing, feedback: data.feedback, history: data.history, next: $elm$core$Maybe$Nothing, state: resetedState, trainingTrials: _List_Nil}));
							}
						case 'Main':
							var _v6 = task.a;
							var data = task.b;
							var _v7 = data.mainTrials;
							if (_v7.b) {
								if (!_v7.b.b) {
									var last = _v7.a;
									return A2(
										$author$project$Logic$Running,
										$author$project$Logic$Main,
										_Utils_update(
											data,
											{
												current: $elm$core$Maybe$Nothing,
												feedback: !data.feedback,
												history: A2(
													$elm$core$List$cons,
													_Utils_Tuple2(last, data.state),
													data.history),
												mainTrials: _List_Nil,
												next: $elm$core$Maybe$Nothing,
												state: resetedState
											}));
								} else {
									if (_v7.b.b.b) {
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var zs = _v9.b;
										return A2(
											$author$project$Logic$Running,
											$author$project$Logic$Main,
											_Utils_update(
												data,
												{
													current: $elm$core$Maybe$Just(y),
													feedback: !data.feedback,
													history: A2(
														$elm$core$List$cons,
														_Utils_Tuple2(x, data.state),
														data.history),
													mainTrials: A2(
														$elm$core$List$cons,
														y,
														A2($elm$core$List$cons, z, zs)),
													next: $elm$core$Maybe$Just(z),
													state: resetedState
												}));
									} else {
										var x = _v7.a;
										var _v10 = _v7.b;
										var y = _v10.a;
										return A2(
											$author$project$Logic$Running,
											$author$project$Logic$Main,
											_Utils_update(
												data,
												{
													current: $elm$core$Maybe$Just(y),
													feedback: !data.feedback,
													history: A2(
														$elm$core$List$cons,
														_Utils_Tuple2(x, data.state),
														data.history),
													mainTrials: _List_fromArray(
														[y]),
													next: $elm$core$Maybe$Nothing,
													state: resetedState
												}));
									}
								}
							} else {
								return A2(
									$author$project$Logic$Running,
									$author$project$Logic$Main,
									_Utils_update(
										data,
										{current: $elm$core$Maybe$Nothing, feedback: data.feedback, history: data.history, mainTrials: _List_Nil, next: $elm$core$Maybe$Nothing, state: resetedState}));
							}
						default:
							break _v0$3;
					}
				case 'Err':
					var reason = task.a;
					return $author$project$Logic$Err(reason);
				default:
					break _v0$3;
			}
		}
		return $author$project$Logic$Err('There is no next trial to access');
	});
var $author$project$Pretest$Acceptability$FirstDistractorMissing = function (a) {
	return {$: 'FirstDistractorMissing', a: a};
};
var $author$project$Pretest$Acceptability$SecondDistractorMissing = function (a) {
	return {$: 'SecondDistractorMissing', a: a};
};
var $author$project$Pretest$Acceptability$ThirdDistractorMissing = function (a) {
	return {$: 'ThirdDistractorMissing', a: a};
};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $author$project$Main$getSentenceTypes = function (sentences) {
	return A2(
		$elm$core$List$map,
		function ($) {
			return $.sentenceType;
		},
		sentences);
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$Main$removesItemsHelp = F3(
	function (items, ls, acc) {
		removesItemsHelp:
		while (true) {
			if (!ls.b) {
				return $elm$core$List$reverse(acc);
			} else {
				var x = ls.a;
				var xs = ls.b;
				if (A2($elm$core$List$member, x, items)) {
					var $temp$items = items,
						$temp$ls = xs,
						$temp$acc = acc;
					items = $temp$items;
					ls = $temp$ls;
					acc = $temp$acc;
					continue removesItemsHelp;
				} else {
					var $temp$items = items,
						$temp$ls = xs,
						$temp$acc = A2($elm$core$List$cons, x, acc);
					items = $temp$items;
					ls = $temp$ls;
					acc = $temp$acc;
					continue removesItemsHelp;
				}
			}
		}
	});
var $author$project$Main$removesItems = F2(
	function (items, ls) {
		return A3($author$project$Main$removesItemsHelp, items, ls, _List_Nil);
	});
var $author$project$Main$organizeAcceptabilityTrialsHelper = F3(
	function (targets, distractors, output) {
		organizeAcceptabilityTrialsHelper:
		while (true) {
			var nextUngrammaticalSentence = F2(
				function (buff, dis) {
					return (!dis.isGrammatical) && (!A2(
						$elm$core$List$member,
						dis.sentenceType,
						$author$project$Main$getSentenceTypes(buff)));
				});
			var nextGrammaticalSentence = F2(
				function (buff, dis) {
					return dis.isGrammatical && (!A2(
						$elm$core$List$member,
						dis.sentenceType,
						$author$project$Main$getSentenceTypes(buff)));
				});
			var firstUnGrammaticalDistractor = A2(
				$elm_community$list_extra$List$Extra$find,
				nextUngrammaticalSentence(_List_Nil),
				distractors);
			var findThirdUnGrammaticalDistractor = F2(
				function (firstDistractor, secondDistractor) {
					return A2(
						$elm_community$list_extra$List$Extra$find,
						nextUngrammaticalSentence(
							_List_fromArray(
								[firstDistractor, secondDistractor])),
						A2(
							$author$project$Main$removesItems,
							_List_fromArray(
								[firstDistractor, secondDistractor]),
							distractors));
				});
			var findThirdGrammaticalDistractor = F2(
				function (firstDistractor, secondDistractor) {
					return A2(
						$elm_community$list_extra$List$Extra$find,
						nextGrammaticalSentence(
							_List_fromArray(
								[firstDistractor, secondDistractor])),
						A2(
							$author$project$Main$removesItems,
							_List_fromArray(
								[firstDistractor, secondDistractor]),
							distractors));
				});
			var findSecondUnGrammaticalDistractor = function (firstDistractor) {
				return A2(
					$elm_community$list_extra$List$Extra$find,
					nextUngrammaticalSentence(
						_List_fromArray(
							[firstDistractor])),
					A2(
						$author$project$Main$removesItems,
						_List_fromArray(
							[firstDistractor]),
						distractors));
			};
			var findSecondGrammaticalDistractor = function (firstDistractor) {
				return A2(
					$elm_community$list_extra$List$Extra$find,
					nextGrammaticalSentence(firstDistractor),
					A2($author$project$Main$removesItems, firstDistractor, distractors));
			};
			var findFirstGrammaticalDistractor = A2(
				$elm_community$list_extra$List$Extra$find,
				nextGrammaticalSentence(_List_Nil),
				distractors);
			var buildBlock = function (target) {
				return target.isGrammatical ? A2(
					$elm$core$Result$andThen,
					function (distractorFound) {
						return A2(
							$elm$core$Result$andThen,
							function (secondDistractorFound) {
								return A2(
									$elm$core$Result$andThen,
									function (thirdDistractorFound) {
										return $elm$core$Result$Ok(
											{
												firstDistractor: distractorFound,
												remainingDistractors: A2(
													$author$project$Main$removesItems,
													_List_fromArray(
														[distractorFound, secondDistractorFound, thirdDistractorFound]),
													distractors),
												secondDistractor: secondDistractorFound,
												target: target,
												thirdDistractor: thirdDistractorFound
											});
									},
									A2(
										$elm$core$Result$fromMaybe,
										_Utils_Tuple2(
											$author$project$Pretest$Acceptability$ThirdDistractorMissing(false),
											_List_fromArray(
												[target, distractorFound, secondDistractorFound])),
										A2(findThirdUnGrammaticalDistractor, distractorFound, secondDistractorFound)));
							},
							A2(
								$elm$core$Result$fromMaybe,
								_Utils_Tuple2(
									$author$project$Pretest$Acceptability$SecondDistractorMissing(true),
									_List_fromArray(
										[target, distractorFound])),
								findSecondGrammaticalDistractor(
									_List_fromArray(
										[distractorFound]))));
					},
					A2(
						$elm$core$Result$fromMaybe,
						_Utils_Tuple2(
							$author$project$Pretest$Acceptability$FirstDistractorMissing(false),
							_List_fromArray(
								[target])),
						firstUnGrammaticalDistractor)) : A2(
					$elm$core$Result$andThen,
					function (distractorFound) {
						return A2(
							$elm$core$Result$andThen,
							function (secondDistractorFound) {
								return A2(
									$elm$core$Result$andThen,
									function (thirdDistractorFound) {
										return $elm$core$Result$Ok(
											{
												firstDistractor: distractorFound,
												remainingDistractors: A2(
													$author$project$Main$removesItems,
													_List_fromArray(
														[distractorFound, secondDistractorFound, thirdDistractorFound]),
													distractors),
												secondDistractor: secondDistractorFound,
												target: target,
												thirdDistractor: thirdDistractorFound
											});
									},
									A2(
										$elm$core$Result$fromMaybe,
										_Utils_Tuple2(
											$author$project$Pretest$Acceptability$ThirdDistractorMissing(true),
											_List_fromArray(
												[target, distractorFound, secondDistractorFound])),
										A2(findThirdGrammaticalDistractor, distractorFound, secondDistractorFound)));
							},
							A2(
								$elm$core$Result$fromMaybe,
								_Utils_Tuple2(
									$author$project$Pretest$Acceptability$SecondDistractorMissing(false),
									_List_fromArray(
										[target, distractorFound])),
								findSecondUnGrammaticalDistractor(distractorFound)));
					},
					A2(
						$elm$core$Result$fromMaybe,
						_Utils_Tuple2(
							$author$project$Pretest$Acceptability$FirstDistractorMissing(true),
							_List_fromArray(
								[target])),
						findFirstGrammaticalDistractor));
			};
			if (!targets.b) {
				return $elm$core$Result$Ok(
					_Utils_ap(
						output,
						_List_fromArray(
							[distractors])));
			} else {
				var x = targets.a;
				var xs = targets.b;
				var _v1 = buildBlock(x);
				if (_v1.$ === 'Err') {
					var _v2 = _v1.a;
					var blockSoFar = _v2.b;
					var $temp$targets = xs,
						$temp$distractors = A2($author$project$Main$removesItems, blockSoFar, distractors),
						$temp$output = A2($elm$core$List$cons, blockSoFar, output);
					targets = $temp$targets;
					distractors = $temp$distractors;
					output = $temp$output;
					continue organizeAcceptabilityTrialsHelper;
				} else {
					var target = _v1.a.target;
					var firstDistractor = _v1.a.firstDistractor;
					var secondDistractor = _v1.a.secondDistractor;
					var thirdDistractor = _v1.a.thirdDistractor;
					var remainingDistractors = _v1.a.remainingDistractors;
					var block = _List_fromArray(
						[target, firstDistractor, secondDistractor, thirdDistractor]);
					var $temp$targets = xs,
						$temp$distractors = remainingDistractors,
						$temp$output = A2($elm$core$List$cons, block, output);
					targets = $temp$targets;
					distractors = $temp$distractors;
					output = $temp$output;
					continue organizeAcceptabilityTrialsHelper;
				}
			}
		}
	});
var $author$project$Main$organizeAcceptabilityTrials = F2(
	function (targets, distractors) {
		return A3($author$project$Main$organizeAcceptabilityTrialsHelper, targets, distractors, _List_Nil);
	});
var $author$project$Ports$playAudio = _Platform_outgoingPort('playAudio', $elm$json$Json$Encode$string);
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $author$project$Pretest$Acceptability$evalToString = function (_eval) {
	switch (_eval.$) {
		case 'NoEvaluation':
			return 'No Eval';
		case 'SentenceCorrect':
			return 'Correct';
		case 'SentenceIncorrect':
			return 'Incorrect';
		default:
			return 'Timeout';
	}
};
var $author$project$Logic$getHistory = function (task) {
	if (task.$ === 'Running') {
		var history = task.b.history;
		return history;
	} else {
		return _List_Nil;
	}
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $author$project$Data$resolve = F2(
	function (bodyParser, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					bodyParser(body));
		}
	});
var $author$project$Data$postRecordsBatch = function (payload) {
	return $elm$http$Http$task(
		{
			body: payload,
			headers: _List_Nil,
			method: 'POST',
			resolver: $elm$http$Http$stringResolver(
				$author$project$Data$resolve(
					$elm$core$Basics$always(
						$elm$core$Result$Ok(_Utils_Tuple0)))),
			timeout: $elm$core$Maybe$Just(5000),
			url: $author$project$Data$buildQuery(
				{app: $author$project$Data$apps.spacing, base: 'output', view_: 'all'})
		});
};
var $author$project$Data$splitIn = F2(
	function (k, xs) {
		return (!k) ? _List_fromArray(
			[_List_Nil]) : ((k < 0) ? _List_Nil : ((_Utils_cmp(
			$elm$core$List$length(xs),
			k) > 0) ? A2(
			$elm$core$List$cons,
			A2($elm$core$List$take, k, xs),
			A2(
				$author$project$Data$splitIn,
				k,
				A2($elm$core$List$drop, k, xs))) : _List_fromArray(
			[xs])));
	});
var $author$project$Data$updateUserCompletedTasks = function (payload) {
	return $elm$http$Http$task(
		{
			body: payload,
			headers: _List_Nil,
			method: 'PATCH',
			resolver: $elm$http$Http$stringResolver(
				$author$project$Data$resolve(
					$elm$core$Basics$always(
						$elm$core$Result$Ok(_Utils_Tuple0)))),
			timeout: $elm$core$Maybe$Just(5000),
			url: $author$project$Data$buildQuery(
				{app: $author$project$Data$apps.spacing, base: 'users', view_: 'all'})
		});
};
var $author$project$Data$sendInBatch = F4(
	function (historyEncoder, taskId, userId, history) {
		var fieldsToUpdate = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'tasks',
					A2(
						$elm$json$Json$Encode$list,
						$elm$json$Json$Encode$string,
						_List_fromArray(
							[taskId])))
				]));
		var chuncks = A2($author$project$Data$splitIn, 10, history);
		return $elm$core$Task$sequence(
			A2(
				$elm$core$List$cons,
				$author$project$Data$updateUserCompletedTasks(
					$elm$http$Http$jsonBody(
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'id',
									$elm$json$Json$Encode$string(userId)),
									_Utils_Tuple2('fields', fieldsToUpdate)
								])))),
				A2(
					$elm$core$List$map,
					function (sublist) {
						return A2(
							$elm$core$Task$andThen,
							function (_v0) {
								return $author$project$Data$postRecordsBatch(
									$elm$http$Http$jsonBody(
										historyEncoder(sublist)));
							},
							$elm$core$Process$sleep(0));
					},
					chuncks)));
	});
var $author$project$Pretest$Acceptability$taskId = 'recR8areYkKRvQ6lU';
var $author$project$Pretest$Acceptability$saveAcceptabilityData = F3(
	function (responseHandler, maybeUserId, task) {
		var whenNothing = $elm$time$Time$millisToPosix(1000000000);
		var userId = A2($elm$core$Maybe$withDefault, 'recd18l2IBRQNI05y', maybeUserId);
		var taskId_ = $author$project$Pretest$Acceptability$taskId;
		var intFromMillis = function (posix) {
			return $elm$json$Json$Encode$int(
				$elm$time$Time$posixToMillis(
					A2($elm$core$Maybe$withDefault, whenNothing, posix)));
		};
		var summarizedTrialEncoder = $elm$json$Json$Encode$list(
			function (_v0) {
				var t = _v0.a;
				var s = _v0.b;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'fields',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'trialUid',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[t.uid]))),
										_Utils_Tuple2(
										'userUid',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[userId]))),
										_Utils_Tuple2(
										'Task_UID',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[$author$project$Pretest$Acceptability$taskId]))),
										_Utils_Tuple2(
										'audioStartedAt',
										intFromMillis(s.audioStartedAt)),
										_Utils_Tuple2(
										'beepStartedAt',
										intFromMillis(s.beepStartedAt)),
										_Utils_Tuple2(
										'audioEndedAt',
										$elm$json$Json$Encode$int(
											$elm$time$Time$posixToMillis(
												A2($elm$core$Maybe$withDefault, whenNothing, s.audioEndedAt)))),
										_Utils_Tuple2(
										'beepEndedAt',
										$elm$json$Json$Encode$int(
											$elm$time$Time$posixToMillis(
												A2($elm$core$Maybe$withDefault, whenNothing, s.beepEndedAt)))),
										_Utils_Tuple2(
										'userAnsweredAt',
										$elm$json$Json$Encode$int(
											$elm$time$Time$posixToMillis(
												A2($elm$core$Maybe$withDefault, whenNothing, s.userAnsweredAt)))),
										_Utils_Tuple2(
										'evaluation',
										$elm$json$Json$Encode$string(
											$author$project$Pretest$Acceptability$evalToString(s.evaluation)))
									])))
						]));
			});
		var history = $author$project$Logic$getHistory(task);
		var sendInBatch_ = A4($author$project$Data$sendInBatch, summarizedTrialEncoder, taskId_, userId, history);
		var callbackHandler = responseHandler;
		return A2($elm$core$Task$attempt, callbackHandler, sendInBatch_);
	});
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0.a;
		var genB = _v1.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v2 = genA(seed0);
				var a = _v2.a;
				var seed1 = _v2.b;
				var _v3 = genB(seed1);
				var b = _v3.a;
				var seed2 = _v3.b;
				return _Utils_Tuple2(
					A2(func, a, b),
					seed2);
			});
	});
var $elm_community$random_extra$Random$Extra$sequence = A2(
	$elm$core$List$foldr,
	$elm$random$Random$map2($elm$core$List$cons),
	$elm$random$Random$constant(_List_Nil));
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
			});
	});
var $owanturist$elm_union_find$UnionFind$findFast = F2(
	function (id, dict) {
		findFast:
		while (true) {
			var _v0 = A2($elm$core$Dict$get, id, dict);
			if (_v0.$ === 'Nothing') {
				return id;
			} else {
				var cursor = _v0.a;
				if (_Utils_eq(id, cursor)) {
					return id;
				} else {
					var $temp$id = cursor,
						$temp$dict = dict;
					id = $temp$id;
					dict = $temp$dict;
					continue findFast;
				}
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$find = F2(
	function (id, _v0) {
		var dict = _v0.b;
		return A2($owanturist$elm_union_find$UnionFind$findFast, id, dict);
	});
var $elm$core$Array$isEmpty = function (_v0) {
	var len = _v0.a;
	return !len;
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $owanturist$elm_union_find$UnionFind$QuickUnionPathCompression = F2(
	function (a, b) {
		return {$: 'QuickUnionPathCompression', a: a, b: b};
	});
var $owanturist$elm_union_find$UnionFind$quickUnionPathCompression = A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, 0, $elm$core$Dict$empty);
var $owanturist$elm_union_find$UnionFind$findCompressed = F2(
	function (id, dict) {
		var _v0 = A2($elm$core$Dict$get, id, dict);
		if (_v0.$ === 'Nothing') {
			return _Utils_Tuple2(
				id,
				A3($elm$core$Dict$insert, id, id, dict));
		} else {
			var cursor = _v0.a;
			if (_Utils_eq(id, cursor)) {
				return _Utils_Tuple2(id, dict);
			} else {
				var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, cursor, dict);
				var parent = _v1.a;
				var nextDict = _v1.b;
				return _Utils_Tuple2(
					parent,
					A3($elm$core$Dict$insert, id, parent, nextDict));
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$union = F3(
	function (left, right, _v0) {
		var count_ = _v0.a;
		var dict = _v0.b;
		var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, left, dict);
		var leftRoot = _v1.a;
		var leftDict = _v1.b;
		var _v2 = A2($owanturist$elm_union_find$UnionFind$findCompressed, right, leftDict);
		var rightRoot = _v2.a;
		var rightDict = _v2.b;
		return _Utils_eq(leftRoot, rightRoot) ? A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, count_, rightDict) : A2(
			$owanturist$elm_union_find$UnionFind$QuickUnionPathCompression,
			count_ + 1,
			A3($elm$core$Dict$insert, leftRoot, rightRoot, rightDict));
	});
var $elm_community$random_extra$Utils$selectUniqByIndexes = F2(
	function (values, randomIndexes) {
		var modByLength = $elm$core$Basics$modBy(
			$elm$core$Array$length(values));
		var step = F2(
			function (randomIndex, _v1) {
				var uf = _v1.a;
				var acc = _v1.b;
				var leaderOfElement = A2($owanturist$elm_union_find$UnionFind$find, randomIndex, uf);
				var leaderOfNextElement = A2(
					$owanturist$elm_union_find$UnionFind$find,
					modByLength(leaderOfElement + 1),
					uf);
				var _v0 = A2($elm$core$Array$get, leaderOfElement, values);
				if (_v0.$ === 'Nothing') {
					return _Utils_Tuple2(uf, acc);
				} else {
					var value = _v0.a;
					return _Utils_Tuple2(
						A3($owanturist$elm_union_find$UnionFind$union, leaderOfElement, leaderOfNextElement, uf),
						A2($elm$core$List$cons, value, acc));
				}
			});
		return $elm$core$Array$isEmpty(values) ? _List_Nil : A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2($owanturist$elm_union_find$UnionFind$quickUnionPathCompression, _List_Nil),
			randomIndexes).b;
	});
var $elm_community$random_extra$Random$List$shuffle = function (list) {
	var values = $elm$core$Array$fromList(list);
	var length = $elm$core$Array$length(values);
	return A2(
		$elm$random$Random$map,
		$elm_community$random_extra$Utils$selectUniqByIndexes(values),
		A2(
			$elm$random$Random$list,
			length,
			A2($elm$random$Random$int, 0, length - 1)));
};
var $author$project$Logic$Instructions = {$: 'Instructions'};
var $author$project$Logic$startIntro = F4(
	function (info, trainingTrials, mainTrials, initStat) {
		if (info.$ === 'Ok') {
			var info_ = info.a;
			if (!trainingTrials.b) {
				return A2(
					$author$project$Logic$Running,
					$author$project$Logic$Instructions,
					{current: $elm$core$Maybe$Nothing, feedback: false, history: _List_Nil, infos: info_, mainTrials: mainTrials, next: $elm$core$Maybe$Nothing, state: initStat, trainingTrials: trainingTrials});
			} else {
				if (trainingTrials.b.b) {
					var x = trainingTrials.a;
					var _v2 = trainingTrials.b;
					var y = _v2.a;
					return A2(
						$author$project$Logic$Running,
						$author$project$Logic$Instructions,
						{
							current: $elm$core$Maybe$Just(x),
							feedback: false,
							history: _List_Nil,
							infos: info_,
							mainTrials: mainTrials,
							next: $elm$core$Maybe$Just(y),
							state: initStat,
							trainingTrials: trainingTrials
						});
				} else {
					var x = trainingTrials.a;
					return A2(
						$author$project$Logic$Running,
						$author$project$Logic$Instructions,
						{
							current: $elm$core$Maybe$Just(x),
							feedback: false,
							history: _List_Nil,
							infos: info_,
							mainTrials: mainTrials,
							next: $elm$core$Maybe$Nothing,
							state: initStat,
							trainingTrials: trainingTrials
						});
				}
			}
		} else {
			var error = info.a;
			return $author$project$Logic$Err('I tried to start the intro of this task but I stumbled into an error : ' + error);
		}
	});
var $author$project$ExperimentInfo$toDict = function (newInfos) {
	return $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (info) {
				return _Utils_Tuple2(
					info.uid,
					$elm$core$Basics$identity(info));
			},
			newInfos));
};
var $author$project$Pretest$Acceptability$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Pretest$Acceptability$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Pretest$Acceptability$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return _Utils_eq(datum.trialType, $author$project$Pretest$Acceptability$Training);
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !_Utils_eq(datum.trialType, $author$project$Pretest$Acceptability$Training);
				},
				trials),
			$author$project$Pretest$Acceptability$initState);
	});
var $author$project$Logic$startMain = F2(
	function (task, initState) {
		if (task.$ === 'Running') {
			var data = task.b;
			var _v1 = data.mainTrials;
			if (_v1.b) {
				if (_v1.b.b) {
					var x = _v1.a;
					var _v2 = _v1.b;
					var y = _v2.a;
					return A2(
						$author$project$Logic$Running,
						$author$project$Logic$Main,
						{
							current: $elm$core$Maybe$Just(x),
							feedback: false,
							history: data.history,
							infos: data.infos,
							mainTrials: data.mainTrials,
							next: $elm$core$Maybe$Just(y),
							state: initState,
							trainingTrials: _List_Nil
						});
				} else {
					var x = _v1.a;
					return A2(
						$author$project$Logic$Running,
						$author$project$Logic$Main,
						{
							current: $elm$core$Maybe$Just(x),
							feedback: false,
							history: data.history,
							infos: data.infos,
							mainTrials: data.mainTrials,
							next: $elm$core$Maybe$Nothing,
							state: initState,
							trainingTrials: _List_Nil
						});
				}
			} else {
				return A2(
					$author$project$Logic$Running,
					$author$project$Logic$Main,
					{current: $elm$core$Maybe$Nothing, feedback: false, history: data.history, infos: data.infos, mainTrials: data.mainTrials, next: $elm$core$Maybe$Nothing, state: initState, trainingTrials: _List_Nil});
			}
		} else {
			return $author$project$Logic$Err('I can\'t go to Main from here');
		}
	});
var $elm_community$list_extra$List$Extra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $elm_community$list_extra$List$Extra$uncons = function (list) {
	if (!list.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var first = list.a;
		var rest = list.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(first, rest));
	}
};
var $elm_community$list_extra$List$Extra$swapAt = F3(
	function (index1, index2, l) {
		swapAt:
		while (true) {
			if (_Utils_eq(index1, index2) || (index1 < 0)) {
				return l;
			} else {
				if (_Utils_cmp(index1, index2) > 0) {
					var $temp$index1 = index2,
						$temp$index2 = index1,
						$temp$l = l;
					index1 = $temp$index1;
					index2 = $temp$index2;
					l = $temp$l;
					continue swapAt;
				} else {
					var _v0 = A2($elm_community$list_extra$List$Extra$splitAt, index1, l);
					var part1 = _v0.a;
					var tail1 = _v0.b;
					var _v1 = A2($elm_community$list_extra$List$Extra$splitAt, index2 - index1, tail1);
					var head2 = _v1.a;
					var tail2 = _v1.b;
					var _v2 = _Utils_Tuple2(
						$elm_community$list_extra$List$Extra$uncons(head2),
						$elm_community$list_extra$List$Extra$uncons(tail2));
					if ((_v2.a.$ === 'Just') && (_v2.b.$ === 'Just')) {
						var _v3 = _v2.a.a;
						var value1 = _v3.a;
						var part2 = _v3.b;
						var _v4 = _v2.b.a;
						var value2 = _v4.a;
						var part3 = _v4.b;
						return $elm$core$List$concat(
							_List_fromArray(
								[
									part1,
									A2($elm$core$List$cons, value2, part2),
									A2($elm$core$List$cons, value1, part3)
								]));
					} else {
						return l;
					}
				}
			}
		}
	});
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Logic$toggle = function (task) {
	if (task.$ === 'Running') {
		var step = task.a;
		var data = task.b;
		return A2(
			$author$project$Logic$Running,
			step,
			_Utils_update(
				data,
				{feedback: !data.feedback}));
	} else {
		return $author$project$Logic$Err('I tried to toggle the feedback but the task is still loading. Please report this error.');
	}
};
var $author$project$Postest$CloudWords$Known = {$: 'Known'};
var $author$project$Postest$CloudWords$MaybeKnown = {$: 'MaybeKnown'};
var $author$project$Postest$CloudWords$toggle = function (key) {
	return A2(
		$elm$core$Dict$update,
		key,
		function (old) {
			if (old.$ === 'Just') {
				var value = old.a;
				switch (value.$) {
					case 'NotKnown':
						return $elm$core$Maybe$Just($author$project$Postest$CloudWords$MaybeKnown);
					case 'MaybeKnown':
						return $elm$core$Maybe$Just($author$project$Postest$CloudWords$Known);
					default:
						return $elm$core$Maybe$Just($author$project$Postest$CloudWords$NotKnown);
				}
			} else {
				return $elm$core$Maybe$Nothing;
			}
		});
};
var $author$project$Logic$update = F2(
	function (newState, task) {
		if (task.$ === 'Running') {
			var step = task.a;
			var data = task.b;
			return A2(
				$author$project$Logic$Running,
				step,
				_Utils_update(
					data,
					{state: newState}));
		} else {
			return $author$project$Logic$Err('You can\'t update anything here');
		}
	});
var $author$project$Pretest$Pretest$ShuffledPretest = F4(
	function (spr, sc, infos, vks) {
		return {infos: infos, sc: sc, spr: spr, vks: vks};
	});
var $author$project$Pretest$Pretest$StartPretest = function (a) {
	return {$: 'StartPretest', a: a};
};
var $author$project$Pretest$SPR$NoAnswerYet = {$: 'NoAnswerYet'};
var $author$project$Pretest$SPR$SPR = function (a) {
	return {$: 'SPR', a: a};
};
var $author$project$Pretest$SPR$Start = {$: 'Start'};
var $author$project$Pretest$SPR$initState = {
	answer: $author$project$Pretest$SPR$NoAnswerYet,
	currentSegment: $elm$core$Maybe$Nothing,
	remainingSegments: _List_Nil,
	seenSegments: _List_Nil,
	step: $author$project$Pretest$SPR$SPR($author$project$Pretest$SPR$Start)
};
var $author$project$Pretest$SPR$taskId = 'rec7oxQBDY7rBTRDn';
var $author$project$Pretest$SPR$init = F2(
	function (infos, trials) {
		var info = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t find SPR infos',
			A2(
				$elm$core$Dict$get,
				$author$project$Pretest$SPR$taskId,
				$author$project$ExperimentInfo$toDict(infos)));
		return A4(
			$author$project$Logic$startIntro,
			info,
			A2(
				$elm$core$List$filter,
				function (trial) {
					return trial.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (trial) {
					return !trial.isTraining;
				},
				trials),
			$author$project$Pretest$SPR$initState);
	});
var $author$project$Pretest$SentenceCompletion$FirstProduction = {$: 'FirstProduction'};
var $author$project$Pretest$SentenceCompletion$initState = {firstProduction: '', order: $author$project$Pretest$SentenceCompletion$FirstProduction, secondProduction: ''};
var $author$project$Pretest$SentenceCompletion$taskId = 'reczQs5ZD6g1x5F29';
var $author$project$Pretest$SentenceCompletion$init = F2(
	function (infos, trials) {
		var info = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t find Task infos',
			A2(
				$elm$core$Dict$get,
				$author$project$Pretest$SentenceCompletion$taskId,
				$author$project$ExperimentInfo$toDict(infos)));
		return A4(
			$author$project$Logic$startIntro,
			info,
			A2(
				$elm$core$List$filter,
				function (trial) {
					return trial.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (trial) {
					return !trial.isTraining;
				},
				trials),
			$author$project$Pretest$SentenceCompletion$initState);
	});
var $author$project$Pretest$VKS$NoAnswer = {$: 'NoAnswer'};
var $author$project$Pretest$VKS$initState = {definition: '', knowledge: $author$project$Pretest$VKS$NoAnswer, usage: ''};
var $author$project$Pretest$VKS$taskId = 'recR6grI83e1so6Zl';
var $author$project$Pretest$VKS$init = F2(
	function (infos, trials) {
		var info = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t find Task infos',
			A2(
				$elm$core$Dict$get,
				$author$project$Pretest$VKS$taskId,
				$author$project$ExperimentInfo$toDict(infos)));
		return A4($author$project$Logic$startIntro, info, _List_Nil, trials, $author$project$Pretest$VKS$initState);
	});
var $elm$random$Random$map4 = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		var genD = _v3.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v4 = genA(seed0);
				var a = _v4.a;
				var seed1 = _v4.b;
				var _v5 = genB(seed1);
				var b = _v5.a;
				var seed2 = _v5.b;
				var _v6 = genC(seed2);
				var c = _v6.a;
				var seed3 = _v6.b;
				var _v7 = genD(seed3);
				var d = _v7.a;
				var seed4 = _v7.b;
				return _Utils_Tuple2(
					A4(func, a, b, c, d),
					seed4);
			});
	});
var $elm$core$Maybe$map4 = F5(
	function (func, ma, mb, mc, md) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				if (mc.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var c = mc.a;
					if (md.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var d = md.a;
						return $elm$core$Maybe$Just(
							A4(func, a, b, c, d));
					}
				}
			}
		}
	});
var $0ui$elm_task_parallel$Task$Parallel$toCmd = A2(
	$elm$core$Basics$composeR,
	$elm$core$Maybe$map(
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Task$succeed,
			$elm$core$Task$perform($elm$core$Basics$identity))),
	$elm$core$Maybe$withDefault($elm$core$Platform$Cmd$none));
var $0ui$elm_task_parallel$Task$Parallel$update4 = F2(
	function (_v0, msg) {
		var onSuccess = _v0.a;
		var a = _v0.b;
		var b = _v0.c;
		var c = _v0.d;
		var d = _v0.e;
		var next = F4(
			function (a_, b_, c_, d_) {
				return _Utils_Tuple2(
					A5($0ui$elm_task_parallel$Task$Parallel$State4, onSuccess, a_, b_, c_, d_),
					$0ui$elm_task_parallel$Task$Parallel$toCmd(
						A5($elm$core$Maybe$map4, onSuccess, a_, b_, c_, d_)));
			});
		switch (msg.$) {
			case 'LoadedA4':
				var data = msg.a;
				return A4(
					next,
					$elm$core$Maybe$Just(data),
					b,
					c,
					d);
			case 'LoadedB4':
				var data = msg.a;
				return A4(
					next,
					a,
					$elm$core$Maybe$Just(data),
					c,
					d);
			case 'LoadedC4':
				var data = msg.a;
				return A4(
					next,
					a,
					b,
					$elm$core$Maybe$Just(data),
					d);
			default:
				var data = msg.a;
				return A4(
					next,
					a,
					b,
					c,
					$elm$core$Maybe$Just(data));
		}
	});
var $author$project$Pretest$Pretest$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ServerRespondedWithSomePretestData':
				var downloaded = msg.a;
				var _v1 = A2($0ui$elm_task_parallel$Task$Parallel$update4, model.pretest, downloaded);
				var nextState = _v1.a;
				var nextCmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{pretest: nextState}),
					nextCmd);
			case 'ServerRespondedWithSomeError':
				var err = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(err)),
							spr: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(err)),
							vks: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(err))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ServerRespondedWithAllPretestData':
				var sprtrials = msg.a;
				var sctrials = msg.b;
				var infos = msg.c;
				var vksTrials = msg.d;
				var randomize = A2(
					$elm$random$Random$generate,
					$author$project$Pretest$Pretest$StartPretest,
					A5(
						$elm$random$Random$map4,
						$author$project$Pretest$Pretest$ShuffledPretest,
						$elm_community$random_extra$Random$List$shuffle(sprtrials),
						$elm_community$random_extra$Random$List$shuffle(sctrials),
						$elm$random$Random$constant(infos),
						$elm_community$random_extra$Random$List$shuffle(vksTrials)));
				return _Utils_Tuple2(model, randomize);
			default:
				var spr = msg.a.spr;
				var sc = msg.a.sc;
				var vks = msg.a.vks;
				var infos = msg.a.infos;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: A2($author$project$Pretest$SentenceCompletion$init, infos, sc),
							spr: A2($author$project$Pretest$SPR$init, infos, spr),
							vks: A2($author$project$Pretest$VKS$init, infos, vks)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pretest$SPR$Feedback = {$: 'Feedback'};
var $author$project$Pretest$SPR$Question = {$: 'Question'};
var $author$project$Pretest$SPR$Reading = function (a) {
	return {$: 'Reading', a: a};
};
var $author$project$Pretest$SPR$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Pretest$SPR$TaggedSegmentStarted = F2(
	function (taggedSegment, startedAt) {
		return {startedAt: startedAt, taggedSegment: taggedSegment};
	});
var $author$project$Pretest$SPR$answerToString = function (answer) {
	switch (answer.$) {
		case 'Yes':
			return 'Yes';
		case 'No':
			return 'No';
		case 'Unsure':
			return 'I don\'t know';
		default:
			return '';
	}
};
var $author$project$Pretest$SPR$tagToString = function (tag) {
	switch (tag.$) {
		case 'NoUnit':
			return 'No unit';
		case 'Critic':
			return 'Critic';
		default:
			return 'SpillOver';
	}
};
var $author$project$Pretest$SPR$saveSprData = F3(
	function (responseHandler, maybeUserId, task) {
		var userId = A2($elm$core$Maybe$withDefault, 'recd18l2IBRQNI05y', maybeUserId);
		var taskId_ = $author$project$Pretest$SPR$taskId;
		var summarizedTrialEncoder = $elm$json$Json$Encode$list(
			function (_v2) {
				var tag = _v2.tag;
				var segment = _v2.segment;
				var startedAt = _v2.startedAt;
				var endedAt = _v2.endedAt;
				var id = _v2.id;
				var answer = _v2.answer;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'fields',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'sprTrialId',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[id]))),
										_Utils_Tuple2(
										'answer',
										$elm$json$Json$Encode$string(answer)),
										_Utils_Tuple2(
										'sprStartedAt',
										$elm$json$Json$Encode$int(startedAt)),
										_Utils_Tuple2(
										'sprEndedAt',
										$elm$json$Json$Encode$int(endedAt)),
										_Utils_Tuple2(
										'tag',
										$elm$json$Json$Encode$string(tag)),
										_Utils_Tuple2(
										'segment',
										$elm$json$Json$Encode$string(segment)),
										_Utils_Tuple2(
										'Task_UID',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[$author$project$Pretest$SPR$taskId]))),
										_Utils_Tuple2(
										'userUid',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[userId])))
									])))
						]));
			});
		var history = $author$project$Logic$getHistory(task);
		var formattedData = A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, acc) {
					var id = _v0.a.id;
					var answer = _v0.b.answer;
					var seenSegments = _v0.b.seenSegments;
					return A2(
						$elm$core$List$append,
						acc,
						A2(
							$elm$core$List$map,
							function (_v1) {
								var taggedSegment = _v1.taggedSegment;
								var startedAt = _v1.startedAt;
								var endedAt = _v1.endedAt;
								return {
									answer: $author$project$Pretest$SPR$answerToString(answer),
									endedAt: $elm$time$Time$posixToMillis(endedAt),
									id: id,
									segment: taggedSegment.b,
									startedAt: $elm$time$Time$posixToMillis(startedAt),
									tag: $author$project$Pretest$SPR$tagToString(taggedSegment.a)
								};
							},
							seenSegments));
				}),
			_List_Nil,
			history);
		var sendInBatch_ = A4($author$project$Data$sendInBatch, summarizedTrialEncoder, taskId_, userId, formattedData);
		var callbackHandler = responseHandler;
		return A2($elm$core$Task$attempt, callbackHandler, sendInBatch_);
	});
var $author$project$Logic$newStep = F2(
	function (step, task) {
		if (task.$ === 'Running') {
			var data = task.b;
			return A2($author$project$Logic$Running, step, data);
		} else {
			return $author$project$Logic$Err('I can\'t change Step here');
		}
	});
var $author$project$Logic$startTraining = function (task) {
	return A2($author$project$Logic$newStep, $author$project$Logic$Training, task);
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Pretest$SPR$updateWithTime = F4(
	function (msg, timestamp, prevModel, newModel) {
		return _Utils_eq(timestamp, $elm$core$Maybe$Nothing) ? _Utils_Tuple2(
			prevModel,
			A2(
				$elm$core$Task$perform,
				$author$project$Pretest$SPR$TimestampedMsg(msg),
				A2($elm$core$Task$map, $elm$core$Maybe$Just, $elm$time$Time$now))) : _Utils_Tuple2(newModel, $elm$core$Platform$Cmd$none);
	});
var $author$project$Pretest$SPR$update = F2(
	function (msg, model) {
		var prevState = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Pretest$SPR$initState,
			$author$project$Logic$getState(model.spr));
		var currentTrial = $author$project$Logic$getTrial(model.spr);
		switch (msg.$) {
			case 'UserConfirmedChoice':
				var answer = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spr: A2(
								$author$project$Logic$update,
								_Utils_update(
									prevState,
									{answer: answer, step: $author$project$Pretest$SPR$Feedback}),
								model.spr)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedNextTrial':
				var newanswer = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spr: A2(
								$author$project$Logic$next,
								$author$project$Pretest$SPR$initState,
								A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{answer: newanswer}),
									model.spr))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Pretest$SPR$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{spr: $author$project$Logic$Loading}),
					A3($author$project$Pretest$SPR$saveSprData, responseHandler, model.user, model.spr));
			case 'ServerRespondedWithLastRecords':
				if (msg.a.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{spr: $author$project$Logic$NotStarted}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'StartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spr: A2($author$project$Logic$startMain, model.spr, $author$project$Pretest$SPR$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'TimestampedMsg':
				var subMsg = msg.a;
				var timestamp = msg.b;
				if (subMsg.$ === 'UserPressedSpaceToStartParagraph') {
					return A4(
						$author$project$Pretest$SPR$updateWithTime,
						subMsg,
						timestamp,
						model,
						_Utils_update(
							model,
							{
								spr: function () {
									if (currentTrial.$ === 'Nothing') {
										return model.spr;
									} else {
										var tr = currentTrial.a;
										var _v3 = tr.taggedSegments;
										if (!_v3.b) {
											return model.spr;
										} else {
											var x = _v3.a;
											var xs = _v3.b;
											return A2(
												$author$project$Logic$update,
												_Utils_update(
													prevState,
													{
														currentSegment: $elm$core$Maybe$Just(
															A2(
																$author$project$Pretest$SPR$TaggedSegmentStarted,
																x,
																A2(
																	$elm$core$Maybe$withDefault,
																	$elm$time$Time$millisToPosix(0),
																	timestamp))),
														remainingSegments: xs,
														step: $author$project$Pretest$SPR$SPR(
															$author$project$Pretest$SPR$Reading('bla)'))
													}),
												model.spr);
										}
									}
								}()
							}));
				} else {
					return A4(
						$author$project$Pretest$SPR$updateWithTime,
						subMsg,
						timestamp,
						model,
						_Utils_update(
							model,
							{
								spr: function () {
									var _v4 = prevState.remainingSegments;
									if (!_v4.b) {
										return A2(
											$author$project$Logic$update,
											_Utils_update(
												prevState,
												{
													seenSegments: function () {
														var _v5 = prevState.currentSegment;
														if (_v5.$ === 'Just') {
															var seg = _v5.a;
															return A2(
																$elm$core$List$cons,
																function (_v6) {
																	var taggedSegment = _v6.taggedSegment;
																	var startedAt = _v6.startedAt;
																	return {
																		endedAt: A2(
																			$elm$core$Maybe$withDefault,
																			$elm$time$Time$millisToPosix(0),
																			timestamp),
																		startedAt: startedAt,
																		taggedSegment: taggedSegment
																	};
																}(seg),
																prevState.seenSegments);
														} else {
															return prevState.seenSegments;
														}
													}(),
													step: $author$project$Pretest$SPR$Question
												}),
											model.spr);
									} else {
										var x = _v4.a;
										return A2(
											$author$project$Logic$update,
											_Utils_update(
												prevState,
												{
													currentSegment: $elm$core$Maybe$Just(
														A2(
															$author$project$Pretest$SPR$TaggedSegmentStarted,
															x,
															A2(
																$elm$core$Maybe$withDefault,
																$elm$time$Time$millisToPosix(0),
																timestamp))),
													remainingSegments: A2(
														$elm$core$Maybe$withDefault,
														_List_Nil,
														$elm$core$List$tail(prevState.remainingSegments)),
													seenSegments: function () {
														var _v7 = prevState.currentSegment;
														if (_v7.$ === 'Just') {
															var seg = _v7.a;
															return A2(
																$elm$core$List$cons,
																function (_v8) {
																	var taggedSegment = _v8.taggedSegment;
																	var startedAt = _v8.startedAt;
																	return {
																		endedAt: A2(
																			$elm$core$Maybe$withDefault,
																			$elm$time$Time$millisToPosix(0),
																			timestamp),
																		startedAt: startedAt,
																		taggedSegment: taggedSegment
																	};
																}(seg),
																prevState.seenSegments);
														} else {
															return prevState.seenSegments;
														}
													}(),
													step: $author$project$Pretest$SPR$SPR(
														$author$project$Pretest$SPR$Reading('blo'))
												}),
											model.spr);
									}
								}()
							}));
				}
			case 'NoOp':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spr: $author$project$Logic$startTraining(model.spr)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pretest$SentenceCompletion$RuntimeReordedAmorces = function (a) {
	return {$: 'RuntimeReordedAmorces', a: a};
};
var $author$project$Pretest$SentenceCompletion$SecondProduction = {$: 'SecondProduction'};
var $author$project$Pretest$SentenceCompletion$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Pretest$SentenceCompletion$saveData = F3(
	function (responseHandler, maybeUserId, task) {
		var userId = A2($elm$core$Maybe$withDefault, 'recd18l2IBRQNI05y', maybeUserId);
		var summarizedTrialEncoder = $elm$json$Json$Encode$list(
			function (_v0) {
				var id = _v0.a.id;
				var firstProduction = _v0.b.firstProduction;
				var secondProduction = _v0.b.secondProduction;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'fields',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'sentenceCompletionTrialId',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[id]))),
										_Utils_Tuple2(
										'firstProduction',
										$elm$json$Json$Encode$string(firstProduction)),
										_Utils_Tuple2(
										'secondProduction',
										$elm$json$Json$Encode$string(secondProduction))
									])))
						]));
			});
		var history = $author$project$Logic$getHistory(task);
		var sendInBatch_ = A4($author$project$Data$sendInBatch, summarizedTrialEncoder, $author$project$Pretest$SentenceCompletion$taskId, userId, history);
		return A2($elm$core$Task$attempt, responseHandler, sendInBatch_);
	});
var $elm$random$Random$addOne = function (value) {
	return _Utils_Tuple2(1, value);
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $elm$random$Random$uniform = F2(
	function (value, valueList) {
		return A2(
			$elm$random$Random$weighted,
			$elm$random$Random$addOne(value),
			A2($elm$core$List$map, $elm$random$Random$addOne, valueList));
	});
var $author$project$Pretest$SentenceCompletion$update = F2(
	function (msg, model) {
		var prevState = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Pretest$SentenceCompletion$initState,
			$author$project$Logic$getState(model.sentenceCompletion));
		switch (msg.$) {
			case 'RuntimeReordedAmorces':
				var field = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: A2(
								$author$project$Logic$update,
								_Utils_update(
									prevState,
									{order: field}),
								model.sentenceCompletion)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: A2(
								$author$project$Logic$next,
								$author$project$Pretest$SentenceCompletion$initState,
								$author$project$Logic$toggle(model.sentenceCompletion))
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Pretest$SentenceCompletion$RuntimeReordedAmorces,
						A2(
							$elm$random$Random$uniform,
							$author$project$Pretest$SentenceCompletion$FirstProduction,
							_List_fromArray(
								[$author$project$Pretest$SentenceCompletion$SecondProduction]))));
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: $author$project$Logic$toggle(model.sentenceCompletion)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: A2($author$project$Logic$startMain, model.sentenceCompletion, $author$project$Pretest$SentenceCompletion$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserUpdatedField':
				var fieldId = msg.a;
				var _new = msg.b;
				if (fieldId.$ === 'FirstProduction') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								sentenceCompletion: A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{firstProduction: _new}),
									model.sentenceCompletion)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								sentenceCompletion: A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{secondProduction: _new}),
									model.sentenceCompletion)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Pretest$SentenceCompletion$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sentenceCompletion: $author$project$Logic$Loading}),
					A3($author$project$Pretest$SentenceCompletion$saveData, responseHandler, model.user, model.sentenceCompletion));
			case 'ServerRespondedWithLastRecords':
				if (msg.a.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{sentenceCompletion: $author$project$Logic$NotStarted}),
						$elm$core$Platform$Cmd$none);
				} else {
					var reason = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								sentenceCompletion: $author$project$Logic$Err(
									$author$project$Data$buildErrorMessage(reason))
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sentenceCompletion: $author$project$Logic$startTraining(model.sentenceCompletion)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Pretest$VKS$FirstProduction = {$: 'FirstProduction'};
var $author$project$Pretest$VKS$RuntimeReordedAmorces = function (a) {
	return {$: 'RuntimeReordedAmorces', a: a};
};
var $author$project$Pretest$VKS$SecondProduction = {$: 'SecondProduction'};
var $author$project$Pretest$VKS$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Pretest$VKS$Known = {$: 'Known'};
var $author$project$Pretest$VKS$NeverSeen = {$: 'NeverSeen'};
var $author$project$Pretest$VKS$PreviouslySeen = {$: 'PreviouslySeen'};
var $author$project$Pretest$VKS$familiarityFromString = function (str) {
	switch (str) {
		case 'Known':
			return $author$project$Pretest$VKS$Known;
		case 'NeverSeen':
			return $author$project$Pretest$VKS$NeverSeen;
		case 'PreviouslySeen':
			return $author$project$Pretest$VKS$PreviouslySeen;
		default:
			return $author$project$Pretest$VKS$NoAnswer;
	}
};
var $author$project$Pretest$VKS$familiarityToString = function (fam) {
	switch (fam.$) {
		case 'Known':
			return 'Known';
		case 'NeverSeen':
			return 'NeverSeen';
		case 'PreviouslySeen':
			return 'PreviouslySeen';
		default:
			return '';
	}
};
var $author$project$Pretest$VKS$saveData = F3(
	function (responseHandler, maybeUserId, task) {
		var userId = A2($elm$core$Maybe$withDefault, 'recd18l2IBRQNI05y', maybeUserId);
		var summarizedTrialEncoder = $elm$json$Json$Encode$list(
			function (_v0) {
				var id = _v0.a.id;
				var knowledge = _v0.b.knowledge;
				var definition = _v0.b.definition;
				var usage = _v0.b.usage;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'fields',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'id',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[id]))),
										_Utils_Tuple2(
										'knowledge',
										$elm$json$Json$Encode$string(
											$author$project$Pretest$VKS$familiarityToString(knowledge))),
										_Utils_Tuple2(
										'definition',
										$elm$json$Json$Encode$string(definition)),
										_Utils_Tuple2(
										'usage',
										$elm$json$Json$Encode$string(usage))
									])))
						]));
			});
		var history = $author$project$Logic$getHistory(task);
		var sendInBatch_ = A4($author$project$Data$sendInBatch, summarizedTrialEncoder, $author$project$Pretest$VKS$taskId, userId, history);
		return A2($elm$core$Task$attempt, responseHandler, sendInBatch_);
	});
var $author$project$Pretest$VKS$update = F2(
	function (msg, model) {
		var prevState = A2(
			$elm$core$Maybe$withDefault,
			$author$project$Pretest$VKS$initState,
			$author$project$Logic$getState(model.vks));
		switch (msg.$) {
			case 'RuntimeReordedAmorces':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							vks: A2($author$project$Logic$update, prevState, model.vks)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							vks: A2(
								$author$project$Logic$next,
								$author$project$Pretest$VKS$initState,
								$author$project$Logic$toggle(model.vks))
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Pretest$VKS$RuntimeReordedAmorces,
						A2(
							$elm$random$Random$uniform,
							$author$project$Pretest$VKS$FirstProduction,
							_List_fromArray(
								[$author$project$Pretest$VKS$SecondProduction]))));
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							vks: A2($author$project$Logic$startMain, model.vks, $author$project$Pretest$VKS$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserUpdatedField':
				var fieldId = msg.a;
				var _new = msg.b;
				if (fieldId.$ === 'FirstProduction') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								vks: A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{definition: _new}),
									model.vks)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								vks: A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{usage: _new}),
									model.vks)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Pretest$VKS$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{vks: $author$project$Logic$Loading}),
					A3($author$project$Pretest$VKS$saveData, responseHandler, model.user, model.vks));
			case 'ServerRespondedWithLastRecords':
				if (msg.a.$ === 'Ok') {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{vks: $author$project$Logic$NotStarted}),
						$elm$core$Platform$Cmd$none);
				} else {
					var reason = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								vks: $author$project$Logic$Err(
									$author$project$Data$buildErrorMessage(reason))
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var str = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							vks: A2(
								$author$project$Logic$update,
								_Utils_update(
									prevState,
									{
										knowledge: $author$project$Pretest$VKS$familiarityFromString(str)
									}),
								model.vks)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session1$ContextUnderstanding$RuntimeShuffledOptionsOrder = function (a) {
	return {$: 'RuntimeShuffledOptionsOrder', a: a};
};
var $author$project$Session1$ContextUnderstanding$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Logic$saveData = F4(
	function (responseHandler, maybeUserId, taskId, task) {
		var userId = A2($elm$core$Maybe$withDefault, 'recd18l2IBRQNI05y', maybeUserId);
		var taskId_ = taskId;
		var summarizedTrialEncoder = $elm$json$Json$Encode$list(
			function (_v0) {
				var t = _v0.a;
				var s = _v0.b;
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'fields',
							$elm$json$Json$Encode$object(
								_List_fromArray(
									[
										_Utils_Tuple2(
										'trialUid',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[t.uid]))),
										_Utils_Tuple2(
										'userUid',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[userId]))),
										_Utils_Tuple2(
										'Task_UID',
										A2(
											$elm$json$Json$Encode$list,
											$elm$json$Json$Encode$string,
											_List_fromArray(
												[taskId]))),
										_Utils_Tuple2(
										'attempt',
										$elm$json$Json$Encode$string(s.userAnswer))
									])))
						]));
			});
		var history = $author$project$Logic$getHistory(task);
		var sendInBatch_ = A4($author$project$Data$sendInBatch, summarizedTrialEncoder, taskId_, userId, history);
		var callbackHandler = responseHandler;
		return A2($elm$core$Task$attempt, callbackHandler, sendInBatch_);
	});
var $author$project$Session1$ContextUnderstanding$taskId = 'recsN8oyy3LIC8URx';
var $author$project$Session1$ContextUnderstanding$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: A2($author$project$Logic$next, $author$project$Session1$ContextUnderstanding$initState, model.cu1)
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Session1$ContextUnderstanding$RuntimeShuffledOptionsOrder,
						$elm_community$random_extra$Random$List$shuffle(model.optionsOrder)));
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: $author$project$Logic$toggle(model.cu1)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedRadioButton':
				var newChoice = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: newChoice},
								model.cu1)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: A2($author$project$Logic$startMain, model.cu1, $author$project$Session1$ContextUnderstanding$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session1$ContextUnderstanding$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session1$ContextUnderstanding$taskId, model.cu1));
			case 'ServerRespondedWithLastRecords':
				if (msg.a.$ === 'Ok') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: $author$project$Logic$startTraining(model.cu1)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var newOrder = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{optionsOrder: newOrder}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session1$Meaning$RuntimeShuffledOptionsOrder = function (a) {
	return {$: 'RuntimeShuffledOptionsOrder', a: a};
};
var $author$project$Session1$Meaning$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session1$Meaning$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session1$Meaning$initState = A2($author$project$Session1$Meaning$State, 'DefaultTrialUID', '');
var $author$project$Session1$Meaning$taskId = 'rec9fDmVOpqDJktmQ';
var $author$project$Session1$Meaning$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							meaning: A2($author$project$Logic$next, $author$project$Session1$Meaning$initState, model.meaning)
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Session1$Meaning$RuntimeShuffledOptionsOrder,
						$elm_community$random_extra$Random$List$shuffle(model.optionsOrder)));
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							meaning: $author$project$Logic$toggle(model.meaning)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedRadioButton':
				var newChoice = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							meaning: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: newChoice},
								model.meaning)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							meaning: A2($author$project$Logic$startMain, model.meaning, $author$project$Session1$Meaning$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'SaveDataMsg':
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, $author$project$Session1$Meaning$ServerRespondedWithLastRecords, model.user, $author$project$Session1$Meaning$taskId, model.meaning));
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							meaning: $author$project$Logic$startTraining(model.meaning)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var newOrder = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{optionsOrder: newOrder}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session1$Presentation$State = F2(
	function (uid, toggledEntries) {
		return {toggledEntries: toggledEntries, uid: uid};
	});
var $author$project$Session1$Presentation$initState = A2(
	$author$project$Session1$Presentation$State,
	'DefaultUid',
	$elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2('definition', false),
				_Utils_Tuple2('example', false),
				_Utils_Tuple2('translation', false)
			])));
var $elm$core$Debug$todo = _Debug_todo;
var $author$project$Session1$Presentation$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							presentation: A2($author$project$Logic$next, $author$project$Session1$Presentation$initState, model.presentation)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							presentation: A2($author$project$Logic$startMain, model.presentation, $author$project$Session1$Presentation$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserToggleElementOfEntry':
				var id = msg.a;
				var prevState = $author$project$Logic$getState(model.presentation);
				if (prevState.$ === 'Just') {
					var state = prevState.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								presentation: A2(
									$author$project$Logic$update,
									_Utils_update(
										state,
										{
											toggledEntries: A3(
												$elm$core$Dict$update,
												id,
												$elm$core$Maybe$map($elm$core$Basics$not),
												state.toggledEntries)
										}),
									model.presentation)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'UserClickedStartAudio':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							presentation: $author$project$Logic$startTraining(model.presentation)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Debug_todo(
					'Session1.Presentation',
					{
						start: {line: 222, column: 13},
						end: {line: 222, column: 23}
					})('');
		}
	});
var $author$project$Session$Ready = {$: 'Ready'};
var $author$project$Session1$Session$ShuffledSession1 = F5(
	function (meaning, spelling, cu1, presentation, infos_) {
		return {cu1: cu1, infos_: infos_, meaning: meaning, presentation: presentation, spelling: spelling};
	});
var $author$project$Session1$Session$StartSession = function (a) {
	return {$: 'StartSession', a: a};
};
var $elm$random$Random$map5 = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		var genD = _v3.a;
		var genE = _v4.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v5 = genA(seed0);
				var a = _v5.a;
				var seed1 = _v5.b;
				var _v6 = genB(seed1);
				var b = _v6.a;
				var seed2 = _v6.b;
				var _v7 = genC(seed2);
				var c = _v7.a;
				var seed3 = _v7.b;
				var _v8 = genD(seed3);
				var d = _v8.a;
				var seed4 = _v8.b;
				var _v9 = genE(seed4);
				var e = _v9.a;
				var seed5 = _v9.b;
				return _Utils_Tuple2(
					A5(func, a, b, c, d, e),
					seed5);
			});
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $author$project$Session1$ContextUnderstanding$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session1$ContextUnderstanding$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session1$ContextUnderstanding$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session1$ContextUnderstanding$initState);
	});
var $author$project$Session1$Meaning$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session1$Meaning$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session1$Meaning$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session1$Meaning$initState);
	});
var $author$project$Session1$Presentation$taskId = 'rec8eKMwCMFFtKVKD';
var $author$project$Session1$Presentation$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session1$Presentation$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session1$Presentation$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session1$Presentation$initState);
	});
var $author$project$Session1$Spelling$State = F3(
	function (inputUid, userUID, userAnswer) {
		return {inputUid: inputUid, userAnswer: userAnswer, userUID: userUID};
	});
var $author$project$Session1$Spelling$initState = A3($author$project$Session1$Spelling$State, 'DefaultTrialUID', 'DefaultUserUID', '');
var $author$project$Session1$Spelling$start = F2(
	function (info, trials) {
		var id = 'recJOpE5pMTCHJOSV';
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + id,
			A2(
				$elm$core$Dict$get,
				id,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session1$Spelling$initState);
	});
var $elm$core$Maybe$map5 = F6(
	function (func, ma, mb, mc, md, me) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				if (mc.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var c = mc.a;
					if (md.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var d = md.a;
						if (me.$ === 'Nothing') {
							return $elm$core$Maybe$Nothing;
						} else {
							var e = me.a;
							return $elm$core$Maybe$Just(
								A5(func, a, b, c, d, e));
						}
					}
				}
			}
		}
	});
var $0ui$elm_task_parallel$Task$Parallel$update5 = F2(
	function (_v0, msg) {
		var onSuccess = _v0.a;
		var a = _v0.b;
		var b = _v0.c;
		var c = _v0.d;
		var d = _v0.e;
		var e = _v0.f;
		var next = F5(
			function (a_, b_, c_, d_, e_) {
				return _Utils_Tuple2(
					A6($0ui$elm_task_parallel$Task$Parallel$State5, onSuccess, a_, b_, c_, d_, e_),
					$0ui$elm_task_parallel$Task$Parallel$toCmd(
						A6($elm$core$Maybe$map5, onSuccess, a_, b_, c_, d_, e_)));
			});
		switch (msg.$) {
			case 'LoadedA5':
				var data = msg.a;
				return A5(
					next,
					$elm$core$Maybe$Just(data),
					b,
					c,
					d,
					e);
			case 'LoadedB5':
				var data = msg.a;
				return A5(
					next,
					a,
					$elm$core$Maybe$Just(data),
					c,
					d,
					e);
			case 'LoadedC5':
				var data = msg.a;
				return A5(
					next,
					a,
					b,
					$elm$core$Maybe$Just(data),
					d,
					e);
			case 'LoadedD5':
				var data = msg.a;
				return A5(
					next,
					a,
					b,
					c,
					$elm$core$Maybe$Just(data),
					e);
			default:
				var data = msg.a;
				return A5(
					next,
					a,
					b,
					c,
					d,
					$elm$core$Maybe$Just(data));
		}
	});
var $author$project$Session1$Session$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ServerRespondedWithSomeData':
				var downloadMsg = msg.a;
				var _v1 = function () {
					var _v2 = model.session1;
					if (_v2.$ === 'Loading') {
						var downloadState = _v2.a;
						return A2(
							$elm$core$Tuple$mapFirst,
							$author$project$Session$Loading,
							A2($0ui$elm_task_parallel$Task$Parallel$update5, downloadState, downloadMsg));
					} else {
						return _Utils_Tuple2(model.session1, $elm$core$Platform$Cmd$none);
					}
				}();
				var updte = _v1.a;
				var cmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{session1: updte}),
					cmd);
			case 'ServerRespondedWithSomeError':
				var error = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error)),
							meaning: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error)),
							spellingLvl1: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ServerRespondedWithAllData':
				var meaning = msg.a;
				var spelling = msg.b;
				var cu1 = msg.c;
				var presentation = msg.d;
				var infos_ = msg.e;
				var randomize = A2(
					$elm$random$Random$generate,
					$author$project$Session1$Session$StartSession,
					A6(
						$elm$random$Random$map5,
						$author$project$Session1$Session$ShuffledSession1,
						$elm_community$random_extra$Random$List$shuffle(meaning),
						$elm_community$random_extra$Random$List$shuffle(spelling),
						$elm_community$random_extra$Random$List$shuffle(cu1),
						$elm_community$random_extra$Random$List$shuffle(presentation),
						$elm$random$Random$constant(infos_)));
				return _Utils_Tuple2(model, randomize);
			default:
				var tasks = msg.a;
				var infos_ = tasks.infos_;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu1: A2($author$project$Session1$ContextUnderstanding$start, infos_, tasks.cu1),
							meaning: A2($author$project$Session1$Meaning$start, infos_, tasks.meaning),
							presentation: A2($author$project$Session1$Presentation$start, infos_, tasks.presentation),
							session1: $author$project$Session$Ready,
							spellingLvl1: A2($author$project$Session1$Spelling$start, infos_, tasks.spelling)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session1$Spelling$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session1$Spelling$iniState = {inputUid: '', userAnswer: '', userUID: ''};
var $author$project$Session1$Spelling$update = F2(
	function (msg, model) {
		var taskId = 'recJOpE5pMTCHJOSV';
		var currentSpellingState = $author$project$Logic$getState(model.spellingLvl1);
		switch (msg.$) {
			case 'UserClickedFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spellingLvl1: $author$project$Logic$toggle(model.spellingLvl1)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedRadioButton':
				var newChoice = msg.a;
				if (currentSpellingState.$ === 'Just') {
					var prevState = currentSpellingState.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								spellingLvl1: A2(
									$author$project$Logic$update,
									_Utils_update(
										prevState,
										{userAnswer: newChoice}),
									model.spellingLvl1)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spellingLvl1: A2($author$project$Logic$next, $author$project$Session1$Spelling$initState, model.spellingLvl1)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMainloop':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spellingLvl1: A2($author$project$Logic$startMain, model.spellingLvl1, $author$project$Session1$Spelling$iniState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSavedData':
				var responseHandler = $author$project$Session1$Spelling$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, taskId, model.spellingLvl1));
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UserClickedPlayAudio':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spellingLvl1: $author$project$Logic$startTraining(model.spellingLvl1)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session2$CU2$RuntimeShuffledOptionsOrder = function (a) {
	return {$: 'RuntimeShuffledOptionsOrder', a: a};
};
var $author$project$Session2$CU2$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session2$CU2$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session2$CU2$initState = A2($author$project$Session2$CU2$State, 'DefaultUid', '');
var $author$project$Session2$CU2$taskId = 'recwxsmowpB18bpLj';
var $author$project$Session2$CU2$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: A2($author$project$Logic$next, $author$project$Session2$CU2$initState, model.cuLvl2)
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Session2$CU2$RuntimeShuffledOptionsOrder,
						$elm_community$random_extra$Random$List$shuffle(model.optionsOrder)));
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: $author$project$Logic$toggle(model.cuLvl2)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedRadioButton':
				var newChoice = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: newChoice},
								model.cuLvl2)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: A2($author$project$Logic$startMain, model.cuLvl2, $author$project$Session2$CU2$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session2$CU2$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session2$CU2$taskId, model.cuLvl2));
			case 'UserClickedAudio':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
			case 'RuntimeShuffledOptionsOrder':
				var ls = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{optionsOrder: ls}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: $author$project$Logic$startTraining(model.cuLvl2)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session2$Session$ShuffledSession2 = F4(
	function (cu, spelling, translation, infos) {
		return {cu: cu, infos: infos, spelling: spelling, translation: translation};
	});
var $author$project$Session2$Session$StartSession = function (a) {
	return {$: 'StartSession', a: a};
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $author$project$Session2$CU2$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session2$CU2$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session2$CU2$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session2$CU2$initState);
	});
var $author$project$Session2$Spelling$State = F3(
	function (uid, userAnswer, scrambledLetter) {
		return {scrambledLetter: scrambledLetter, uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session2$Spelling$initState = A3($author$project$Session2$Spelling$State, 'DefaultUid', '', _List_Nil);
var $author$project$Session2$Spelling$taskId = 'recSL8cthViyXRx8u';
var $author$project$Session2$Spelling$dedupeHelper = F2(
	function (letters, acc) {
		var lettersInAcc = A2($elm$core$List$map, $elm$core$Tuple$first, acc);
		var countRecLetters = function (target) {
			return A3(
				$elm$core$List$foldr,
				F2(
					function (letter, acc_) {
						return _Utils_eq(target, letter) ? (acc_ + 1) : acc_;
					}),
				1,
				lettersInAcc);
		};
		if (!letters.b) {
			return $elm$core$List$reverse(acc);
		} else {
			var x = letters.a;
			var xs = letters.b;
			return A2($elm$core$List$member, x, lettersInAcc) ? A2(
				$author$project$Session2$Spelling$dedupeHelper,
				xs,
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(
						x,
						countRecLetters(x)),
					acc)) : A2(
				$author$project$Session2$Spelling$dedupeHelper,
				xs,
				A2(
					$elm$core$List$cons,
					_Utils_Tuple2(x, 1),
					acc));
		}
	});
var $author$project$Session2$Spelling$dedupe = function (letters) {
	return A2($author$project$Session2$Spelling$dedupeHelper, letters, _List_Nil);
};
var $author$project$Session2$Spelling$toKeyedItem = function (letters) {
	return A2(
		$elm$core$List$map,
		function (_v0) {
			var lett = _v0.a;
			var rec = _v0.b;
			return _Utils_Tuple2(
				'key-' + (lett + $elm$core$String$fromInt(rec)),
				lett);
		},
		$author$project$Session2$Spelling$dedupe(letters));
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$Session2$Spelling$toItems = function (string) {
	return $author$project$Session2$Spelling$toKeyedItem(
		A2(
			$elm$core$List$map,
			$elm$core$String$fromChar,
			$elm$core$String$toList(string)));
};
var $author$project$Session2$Spelling$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session2$Spelling$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session2$Spelling$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		var nextTrial = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function ($) {
					return $.isTraining;
				},
				trials));
		if (nextTrial.$ === 'Just') {
			var x = nextTrial.a;
			return A4(
				$author$project$Logic$startIntro,
				relatedInfos,
				A2(
					$elm$core$List$filter,
					function ($) {
						return $.isTraining;
					},
					trials),
				A2(
					$elm$core$List$filter,
					A2(
						$elm$core$Basics$composeL,
						$elm$core$Basics$not,
						function ($) {
							return $.isTraining;
						}),
					trials),
				_Utils_update(
					$author$project$Session2$Spelling$initState,
					{
						scrambledLetter: $author$project$Session2$Spelling$toItems(x.writtenWord),
						userAnswer: x.writtenWord
					}));
		} else {
			return $author$project$Logic$Err('I tried to initate the state with the first trial but I couldn\'t find a first trial. Please report this error.');
		}
	});
var $author$project$Session2$Translation$initState = {uid: '', userAnswer: ''};
var $author$project$Session2$Translation$taskId = 'recf5HANE632FLKbc';
var $author$project$Session2$Translation$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session2$Translation$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session2$Translation$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session2$Translation$initState);
	});
var $author$project$Session2$Session$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ServerRespondedWithSomeData':
				var dataSoFar = msg.a;
				var _v1 = function () {
					var _v2 = model.session2;
					if (_v2.$ === 'Loading') {
						var downloadState = _v2.a;
						return A2(
							$elm$core$Tuple$mapFirst,
							$author$project$Session$Loading,
							A2($0ui$elm_task_parallel$Task$Parallel$update4, downloadState, dataSoFar));
					} else {
						return _Utils_Tuple2(model.session2, $elm$core$Platform$Cmd$none);
					}
				}();
				var updte = _v1.a;
				var cmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{session2: updte}),
					cmd);
			case 'ServerRespondedWithAllData':
				var cu = msg.a;
				var spelling = msg.b;
				var translation = msg.c;
				var infos_ = msg.d;
				var shuffleLetters = A2(
					$elm$random$Random$andThen,
					$elm_community$random_extra$Random$List$shuffle,
					$elm_community$random_extra$Random$Extra$sequence(
						A2(
							$elm$core$List$map,
							function (trial) {
								return A6(
									$elm$random$Random$map5,
									$author$project$Session2$Spelling$Trial,
									$elm$random$Random$constant(trial.uid),
									A2(
										$elm$random$Random$map,
										function (letters_) {
											return $elm$core$String$concat(
												A2($elm$core$List$map, $elm$core$String$fromChar, letters_));
										},
										$elm_community$random_extra$Random$List$shuffle(
											$elm$core$String$toList(trial.writtenWord))),
									$elm$random$Random$constant(trial.audioWord),
									$elm$random$Random$constant(trial.isTraining),
									$elm$random$Random$constant(trial.writtenWord));
							},
							spelling)));
				var randomizeTrials = A2(
					$elm$random$Random$generate,
					$author$project$Session2$Session$StartSession,
					A5(
						$elm$random$Random$map4,
						$author$project$Session2$Session$ShuffledSession2,
						$elm_community$random_extra$Random$List$shuffle(cu),
						shuffleLetters,
						$elm_community$random_extra$Random$List$shuffle(translation),
						$elm$random$Random$constant(infos_)));
				return _Utils_Tuple2(
					model,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[randomizeTrials])));
			case 'ServerRespondedWithSomeError':
				return _Debug_todo(
					'Session2.Session',
					{
						start: {line: 91, column: 13},
						end: {line: 91, column: 17}
					})('');
			default:
				var cu = msg.a.cu;
				var spelling = msg.a.spelling;
				var translation = msg.a.translation;
				var infos = msg.a.infos;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cuLvl2: A2($author$project$Session2$CU2$start, infos, cu),
							scrabbleTask: A2($author$project$Session2$Spelling$start, infos, spelling),
							session2: $author$project$Session$Ready,
							translationTask: A2($author$project$Session2$Translation$start, infos, translation)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session2$Spelling$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session2$Spelling$update = F2(
	function (msg, model) {
		var currentScrabbleState = function () {
			var _v4 = $author$project$Logic$getState(model.scrabbleTask);
			if (_v4.$ === 'Just') {
				var x = _v4.a;
				return x;
			} else {
				return $author$project$Session2$Spelling$initState;
			}
		}();
		switch (msg.$) {
			case 'UserDragsLetter':
				var dndmsg = msg.a;
				var _v1 = A3($author$project$Session2$Spelling$system.update, dndmsg, model.dnd, currentScrabbleState.scrambledLetter);
				var dnd = _v1.a;
				var items = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							dnd: dnd,
							scrabbleTask: A2(
								$author$project$Logic$update,
								_Utils_update(
									currentScrabbleState,
									{
										scrambledLetter: items,
										userAnswer: $elm$core$String$concat(
											A2($elm$core$List$map, $elm$core$Tuple$second, items))
									}),
								model.scrabbleTask)
						}),
					$author$project$Session2$Spelling$system.commands(dnd));
			case 'PlayAudio':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
			case 'UserClickedFeedbackButton':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							scrabbleTask: $author$project$Logic$toggle(model.scrabbleTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedNextTrial':
				if (msg.a.$ === 'Just') {
					var nextTrial = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								scrabbleTask: A2(
									$author$project$Logic$next,
									_Utils_update(
										currentScrabbleState,
										{
											scrambledLetter: $author$project$Session2$Spelling$toItems(nextTrial.writtenWord),
											userAnswer: nextTrial.writtenWord
										}),
									model.scrabbleTask)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var _v2 = msg.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								scrabbleTask: A2($author$project$Logic$next, currentScrabbleState, model.scrabbleTask)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session2$Spelling$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session2$Spelling$taskId, model.scrabbleTask));
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UserClickedStartMainloop':
				var trials = msg.a;
				if (!trials.b) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								scrabbleTask: $author$project$Logic$Err('You gave no trial to start the main loop. Please report this error message.')
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var x = trials.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								scrabbleTask: A2(
									$author$project$Logic$startMain,
									model.scrabbleTask,
									_Utils_update(
										currentScrabbleState,
										{
											scrambledLetter: $author$project$Session2$Spelling$toItems(x.writtenWord),
											userAnswer: x.writtenWord
										}))
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							scrabbleTask: $author$project$Logic$startTraining(model.scrabbleTask)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
		}
	});
var $author$project$Session2$Translation$RuntimeShuffledOptionsOrder = function (a) {
	return {$: 'RuntimeShuffledOptionsOrder', a: a};
};
var $author$project$Session2$Translation$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session2$Translation$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							translationTask: A2($author$project$Logic$next, $author$project$Session2$Translation$initState, model.translationTask)
						}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								A2(
								$elm$random$Random$generate,
								$author$project$Session2$Translation$RuntimeShuffledOptionsOrder,
								$elm_community$random_extra$Random$List$shuffle(model.optionsOrder))
							])));
			case 'ServerRespondedWithLastRecords':
				if (msg.a.$ === 'Ok') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							translationTask: $author$project$Logic$toggle(model.translationTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedRadioButton':
				var newChoice = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							translationTask: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: newChoice},
								model.translationTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							translationTask: $author$project$Logic$startTraining(model.translationTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							translationTask: A2($author$project$Logic$startMain, model.translationTask, $author$project$Session2$Translation$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session2$Translation$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session2$Translation$taskId, model.translationTask));
			default:
				var ls = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{optionsOrder: ls}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session3$CU3$RuntimeShuffledOptionsOrder = function (a) {
	return {$: 'RuntimeShuffledOptionsOrder', a: a};
};
var $author$project$Session3$CU3$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session3$CU3$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session3$CU3$initState = A2($author$project$Session3$CU3$State, 'DefaultUid', '');
var $author$project$Session3$CU3$taskId = 'recFEtKbtuBSolHnI';
var $author$project$Session3$CU3$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: A2($author$project$Logic$next, $author$project$Session3$CU3$initState, model.cu3)
						}),
					A2(
						$elm$random$Random$generate,
						$author$project$Session3$CU3$RuntimeShuffledOptionsOrder,
						$elm_community$random_extra$Random$List$shuffle(model.optionsOrder)));
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: $author$project$Logic$toggle(model.cu3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: A2($author$project$Logic$startMain, model.cu3, $author$project$Session3$CU3$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserChangedInput':
				var _new = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: _new},
								model.cu3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session3$CU3$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session3$CU3$taskId, model.scrabbleTask));
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: $author$project$Logic$startTraining(model.cu3)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var ls = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{optionsOrder: ls}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session3$Session$ShuffledSession3 = F4(
	function (cu, spelling, synonym, infos) {
		return {cu: cu, infos: infos, spelling: spelling, synonym: synonym};
	});
var $author$project$Session3$Session$StartSession = function (a) {
	return {$: 'StartSession', a: a};
};
var $author$project$Session3$CU3$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session3$CU3$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session3$CU3$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session3$CU3$initState);
	});
var $author$project$Session3$Spelling3$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session3$Spelling3$initState = A2($author$project$Session3$Spelling3$State, 'DefaultUid', '');
var $author$project$Session3$Spelling3$taskId = 'recJucOXEZzJj6Uui';
var $author$project$Session3$Spelling3$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session3$Spelling3$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session3$Spelling3$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session3$Spelling3$initState);
	});
var $author$project$Session3$Synonym$State = F2(
	function (uid, userAnswer) {
		return {uid: uid, userAnswer: userAnswer};
	});
var $author$project$Session3$Synonym$initState = A2($author$project$Session3$Synonym$State, 'DefaultUserUID', '');
var $author$project$Session3$Synonym$taskId = 'recf5HANE632FLKbc';
var $author$project$Session3$Synonym$start = F2(
	function (info, trials) {
		var relatedInfos = A2(
			$elm$core$Result$fromMaybe,
			'I couldn\'t fetch the value associated with: ' + $author$project$Session3$Synonym$taskId,
			A2(
				$elm$core$Dict$get,
				$author$project$Session3$Synonym$taskId,
				$author$project$ExperimentInfo$toDict(info)));
		return A4(
			$author$project$Logic$startIntro,
			relatedInfos,
			A2(
				$elm$core$List$filter,
				function (datum) {
					return datum.isTraining;
				},
				trials),
			A2(
				$elm$core$List$filter,
				function (datum) {
					return !datum.isTraining;
				},
				trials),
			$author$project$Session3$Synonym$initState);
	});
var $author$project$Session3$Session$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ServerRespondedWithSomeSession3Data':
				var downloadMsg = msg.a;
				var _v1 = function () {
					var _v2 = model.session3;
					if (_v2.$ === 'Loading') {
						var downloadState = _v2.a;
						return A2(
							$elm$core$Tuple$mapFirst,
							$author$project$Session$Loading,
							A2($0ui$elm_task_parallel$Task$Parallel$update4, downloadState, downloadMsg));
					} else {
						return _Utils_Tuple2(model.session3, $elm$core$Platform$Cmd$none);
					}
				}();
				var updte = _v1.a;
				var cmd = _v1.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{session3: updte}),
					cmd);
			case 'ServerRespondedWithAllSession3Data':
				var cu = msg.a;
				var spelling = msg.b;
				var synonym = msg.c;
				var infos = msg.d;
				var randomize = A2(
					$elm$random$Random$generate,
					$author$project$Session3$Session$StartSession,
					A5(
						$elm$random$Random$map4,
						$author$project$Session3$Session$ShuffledSession3,
						$elm_community$random_extra$Random$List$shuffle(cu),
						$elm_community$random_extra$Random$List$shuffle(spelling),
						$elm_community$random_extra$Random$List$shuffle(synonym),
						$elm$random$Random$constant(infos)));
				return _Utils_Tuple2(model, randomize);
			case 'ServerRespondedWithSomeError':
				var error = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error)),
							spelling3: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error)),
							synonymTask: $author$project$Logic$Err(
								$author$project$Data$buildErrorMessage(error))
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var cu = msg.a.cu;
				var spelling = msg.a.spelling;
				var synonym = msg.a.synonym;
				var infos = msg.a.infos;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cu3: A2($author$project$Session3$CU3$start, infos, cu),
							infos: $krisajenkins$remotedata$RemoteData$Success(
								$author$project$ExperimentInfo$toDict(infos)),
							session3: $author$project$Session$Ready,
							spelling3: A2($author$project$Session3$Spelling3$start, infos, spelling),
							synonymTask: A2($author$project$Session3$Synonym$start, infos, synonym)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Session3$Spelling3$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session3$Spelling3$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spelling3: A2($author$project$Logic$next, $author$project$Session3$Spelling3$initState, model.spelling3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedToggleFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spelling3: $author$project$Logic$toggle(model.spelling3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartTraining':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spelling3: $author$project$Logic$startTraining(model.spelling3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedStartMain':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spelling3: A2($author$project$Logic$startMain, model.spelling3, $author$project$Session3$Spelling3$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserChangedInput':
				var _new = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							spelling3: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: _new},
								model.spelling3)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedSaveData':
				var responseHandler = $author$project$Session3$Spelling3$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session3$Spelling3$taskId, model.scrabbleTask));
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
		}
	});
var $author$project$Session3$Synonym$ServerRespondedWithLastRecords = function (a) {
	return {$: 'ServerRespondedWithLastRecords', a: a};
};
var $author$project$Session3$Synonym$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'UserClickedFeedback':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							synonymTask: $author$project$Logic$toggle(model.synonymTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserChangedInput':
				var newChoice = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							synonymTask: A2(
								$author$project$Logic$update,
								{uid: '', userAnswer: newChoice},
								model.synonymTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UserClickedNextTrial':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							synonymTask: A2($author$project$Logic$next, $author$project$Session3$Synonym$initState, model.synonymTask)
						}),
					$elm$core$Platform$Cmd$none);
			case 'SaveDataMsg':
				var responseHandler = $author$project$Session3$Synonym$ServerRespondedWithLastRecords;
				return _Utils_Tuple2(
					model,
					A4($author$project$Logic$saveData, responseHandler, model.user, $author$project$Session3$Synonym$taskId, model.translationTask));
			case 'UserClickedStartMainloop':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							synonymTask: A2($author$project$Logic$startMain, model.synonymTask, $author$project$Session3$Synonym$initState)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ServerRespondedWithLastRecords':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							synonymTask: $author$project$Logic$startTraining(model.synonymTask)
						}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'BrowserChangedUrl':
				var url = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							route: $author$project$Route$fromUrl(url)
						}),
					$elm$core$Platform$Cmd$none);
			case 'NoOp':
				return $author$project$Main$pure(model);
			case 'UserClickedLink':
				var urlRequest = msg.a;
				if (urlRequest.$ === 'Internal') {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.key,
							$elm$url$Url$toString(url)));
				} else {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(url));
				}
			case 'SPR':
				var submsg = msg.a;
				var _v2 = A2($author$project$Pretest$SPR$update, submsg, model);
				var newModel = _v2.a;
				var newCmd = _v2.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$SPR, newCmd));
			case 'Pretest':
				var submsg = msg.a;
				var _v3 = A2($author$project$Pretest$Pretest$update, submsg, model);
				var newModel = _v3.a;
				var newCmd = _v3.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Pretest, newCmd));
			case 'Session1':
				var submsg = msg.a;
				var _v4 = A2($author$project$Session1$Session$update, submsg, model);
				var newModel = _v4.a;
				var newCmd = _v4.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session1, newCmd));
			case 'Session2':
				var submsg = msg.a;
				var _v5 = A2($author$project$Session2$Session$update, submsg, model);
				var newModel = _v5.a;
				var newCmd = _v5.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session2, newCmd));
			case 'Session3':
				var submsg = msg.a;
				var _v6 = A2($author$project$Session3$Session$update, submsg, model);
				var newModel = _v6.a;
				var newCmd = _v6.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Session3, newCmd));
			case 'SentenceCompletion':
				var submsg = msg.a;
				var _v7 = A2($author$project$Pretest$SentenceCompletion$update, submsg, model);
				var newModel = _v7.a;
				var newCmd = _v7.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$SentenceCompletion, newCmd));
			case 'VKS':
				var submsg = msg.a;
				var _v8 = A2($author$project$Pretest$VKS$update, submsg, model);
				var newModel = _v8.a;
				var newCmd = _v8.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$VKS, newCmd));
			case 'Acceptability':
				var message = msg.a;
				var toNextStep = F2(
					function (_int, step) {
						return A2(
							$andrewMacmurray$elm_delay$Delay$after,
							_int,
							$author$project$Main$Acceptability(
								$author$project$Pretest$Acceptability$NextStepCinematic(step)));
					});
				var prevState = $author$project$Logic$getState(model.acceptabilityTask);
				var getTrial = $author$project$Logic$getTrial(model.acceptabilityTask);
				var _v9 = _Utils_Tuple2(prevState, getTrial);
				if ((_v9.a.$ === 'Just') && (_v9.b.$ === 'Just')) {
					var pState = _v9.a.a;
					var trial = _v9.b.a;
					switch (message.$) {
						case 'NextStepCinematic':
							var step = message.a;
							switch (step.$) {
								case 'Listening':
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												acceptabilityTask: A2(
													$author$project$Logic$update,
													_Utils_update(
														pState,
														{step: $author$project$Pretest$Acceptability$Listening}),
													model.acceptabilityTask)
											}),
										A2(
											$andrewMacmurray$elm_delay$Delay$after,
											500,
											$author$project$Main$PlaysoundInJS(trial.audio.url)));
								case 'Answering':
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												acceptabilityTask: A2(
													$author$project$Logic$update,
													_Utils_update(
														pState,
														{step: $author$project$Pretest$Acceptability$Answering}),
													model.acceptabilityTask)
											}),
										A2(
											$andrewMacmurray$elm_delay$Delay$after,
											trial.timeout,
											$author$project$Main$Acceptability(
												$author$project$Pretest$Acceptability$UserPressedButton($elm$core$Maybe$Nothing))));
								case 'End':
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												acceptabilityTask: A2(
													$author$project$Logic$next,
													pState,
													A2(
														$author$project$Logic$update,
														_Utils_update(
															pState,
															{step: $author$project$Pretest$Acceptability$End}),
														model.acceptabilityTask))
											}),
										A2(toNextStep, 0, $author$project$Pretest$Acceptability$Start));
								case 'Start':
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												acceptabilityTask: A2($author$project$Logic$update, $author$project$Pretest$Acceptability$newLoop, model.acceptabilityTask)
											}),
										A2(
											$andrewMacmurray$elm_delay$Delay$after,
											0,
											$author$project$Main$PlaysoundInJS($author$project$Main$beep)));
								default:
									return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
							}
						case 'StartMain':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2($author$project$Logic$startMain, model.acceptabilityTask, $author$project$Pretest$Acceptability$initState)
									}),
								$elm$core$Platform$Cmd$none);
						case 'UserPressedButton':
							var maybeBool = message.a;
							var forward = _Utils_eq(pState.step, $author$project$Pretest$Acceptability$Answering) ? A2(
								$elm$core$Task$perform,
								function (timestamp) {
									return $author$project$Main$Acceptability(
										A2($author$project$Pretest$Acceptability$UserPressedButtonWithTimestamp, maybeBool, timestamp));
								},
								$elm$time$Time$now) : $elm$core$Platform$Cmd$none;
							return _Utils_Tuple2(model, forward);
						case 'UserPressedButtonWithTimestamp':
							var maybeBool = message.a;
							var timestamp = message.b;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2(
											$author$project$Logic$update,
											_Utils_update(
												pState,
												{
													evaluation: $author$project$Pretest$Acceptability$maybeBoolToEvaluation(maybeBool),
													step: $author$project$Pretest$Acceptability$End,
													userAnsweredAt: $elm$core$Maybe$Just(timestamp)
												}),
											model.acceptabilityTask)
									}),
								A2(toNextStep, model.endAcceptabilityDuration, $author$project$Pretest$Acceptability$End));
						case 'AudioEnded':
							var _v12 = message.a;
							var name = _v12.a;
							var timestamp = _v12.b;
							return _Utils_eq(name, $author$project$Main$beep) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2(
											$author$project$Logic$update,
											_Utils_update(
												pState,
												{
													beepEndedAt: $elm$core$Maybe$Just(timestamp)
												}),
											model.acceptabilityTask)
									}),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2(
											$author$project$Logic$update,
											_Utils_update(
												pState,
												{
													audioEndedAt: $elm$core$Maybe$Just(timestamp)
												}),
											model.acceptabilityTask)
									}),
								A2(toNextStep, 0, $author$project$Pretest$Acceptability$Answering));
						case 'AudioStarted':
							var _v13 = message.a;
							var name = _v13.a;
							var timestamp = _v13.b;
							return _Utils_eq(name, $author$project$Main$beep) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2(
											$author$project$Logic$update,
											_Utils_update(
												pState,
												{
													beepStartedAt: $elm$core$Maybe$Just(timestamp)
												}),
											model.acceptabilityTask)
									}),
								A2(toNextStep, 0, $author$project$Pretest$Acceptability$Listening)) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2(
											$author$project$Logic$update,
											_Utils_update(
												pState,
												{
													audioStartedAt: $elm$core$Maybe$Just(timestamp)
												}),
											model.acceptabilityTask)
									}),
								$elm$core$Platform$Cmd$none);
						case 'StartTraining':
							return _Utils_Tuple2(
								model,
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											A2($elm$browser$Browser$Navigation$pushUrl, model.key, 'start')
										])));
						default:
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					switch (message.$) {
						case 'StartMain':
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										acceptabilityTask: A2($author$project$Logic$startMain, model.acceptabilityTask, $author$project$Pretest$Acceptability$initState),
										endAcceptabilityDuration: 500
									}),
								A2(toNextStep, 0, $author$project$Pretest$Acceptability$Init));
						case 'RuntimeShuffledTrials':
							var info = message.a;
							var trials = message.b;
							if (trials.$ === 'Ok') {
								var shuffledTrials = trials.a;
								return _Utils_eq(
									A2(
										$elm$core$List$filter,
										function (block) {
											return $elm$core$List$length(block) < 4;
										},
										shuffledTrials),
									_List_fromArray(
										[_List_Nil])) ? _Utils_Tuple2(
									_Utils_update(
										model,
										{
											acceptabilityTask: A2(
												$author$project$Pretest$Acceptability$start,
												info,
												$elm$core$List$concat(shuffledTrials))
										}),
									$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
									model,
									A2(
										$andrewMacmurray$elm_delay$Delay$after,
										0,
										A2(
											$author$project$Main$ServerRespondedWithAllPretestData,
											$elm$core$List$concat(shuffledTrials),
											info)));
							} else {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											acceptabilityTask: $author$project$Logic$Err('Error whhen I tried to shuffle trials')
										}),
									$elm$core$Platform$Cmd$none);
							}
						case 'UserClickedSaveMsg':
							var responseHandler = function (records) {
								return $author$project$Main$Acceptability(
									$author$project$Pretest$Acceptability$ServerRespondedWithLastRecords(records));
							};
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{acceptabilityTask: $author$project$Logic$Loading}),
								A3($author$project$Pretest$Acceptability$saveAcceptabilityData, responseHandler, model.user, model.acceptabilityTask));
						case 'ServerRespondedWithLastRecords':
							if (message.a.$ === 'Ok') {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{acceptabilityTask: $author$project$Logic$Loading}),
									A2($elm$browser$Browser$Navigation$pushUrl, model.key, 'end'));
							} else {
								var reason = message.a.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											acceptabilityTask: $author$project$Logic$Err(
												$author$project$Data$buildErrorMessage(reason) + 'Please report this error message to yacourt@unice.fr with a nice screenshot!')
										}),
									$elm$core$Platform$Cmd$none);
							}
						default:
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				}
			case 'ServerRespondedWithAllPretestData':
				var trials = msg.a;
				var info = msg.b;
				var trainingTrials = A2(
					$elm$core$List$filter,
					function (datum) {
						return _Utils_eq(datum.trialType, $author$project$Pretest$Acceptability$Training);
					},
					trials);
				var swapTargetWithOneDistractor = function (tr) {
					return A2(
						$elm$core$List$map,
						function (block) {
							return A2(
								$elm$random$Random$andThen,
								function (position) {
									return $elm$random$Random$constant(
										A3($elm_community$list_extra$List$Extra$swapAt, 0, position, block));
								},
								A2(
									$elm$random$Random$int,
									1,
									$elm$core$List$length(block) - 1));
						},
						tr);
				};
				var generateOrganizedTrials = A2(
					$elm$random$Random$generate,
					function (st) {
						return $author$project$Main$Acceptability(
							A2($author$project$Pretest$Acceptability$RuntimeShuffledTrials, info, st));
					},
					A2(
						$elm$random$Random$andThen,
						function (shuffledTrials) {
							var targets = A2(
								$elm$core$List$filter,
								function (datum) {
									return _Utils_eq(datum.trialType, $author$project$Pretest$Acceptability$Target);
								},
								shuffledTrials);
							var distractors = A2(
								$elm$core$List$filter,
								function (datum) {
									return _Utils_eq(datum.trialType, $author$project$Pretest$Acceptability$Distractor);
								},
								shuffledTrials);
							return A2(
								$elm$random$Random$andThen,
								function (organizedTrials_) {
									if (organizedTrials_.$ === 'Err') {
										var reason = organizedTrials_.a;
										return $elm$random$Random$constant(
											$elm$core$Result$Err(reason));
									} else {
										var tr = organizedTrials_.a;
										return A2(
											$elm$random$Random$andThen,
											function (swapedTargets) {
												return $elm$random$Random$constant(
													$elm$core$Result$Ok(
														A2($elm$core$List$cons, trainingTrials, swapedTargets)));
											},
											$elm_community$random_extra$Random$Extra$sequence(
												swapTargetWithOneDistractor(tr)));
									}
								},
								$elm$random$Random$constant(
									A2($author$project$Main$organizeAcceptabilityTrials, targets, distractors)));
						},
						$elm_community$random_extra$Random$List$shuffle(trials)));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							infos: $krisajenkins$remotedata$RemoteData$Success(
								$author$project$ExperimentInfo$toDict(info))
						}),
					generateOrganizedTrials);
			case 'UserToggledInCloudWords':
				var word = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							cloudWords: A2($author$project$Postest$CloudWords$toggle, word, model.cloudWords)
						}),
					$elm$core$Platform$Cmd$none);
			case 'PlaysoundInJS':
				var url = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Ports$playAudio(url));
			case 'Spelling1':
				var message = msg.a;
				var _v17 = A2($author$project$Session1$Spelling$update, message, model);
				var newModel = _v17.a;
				var newCmd = _v17.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Spelling1, newCmd));
			case 'CU2':
				var message = msg.a;
				var _v18 = A2($author$project$Session2$CU2$update, message, model);
				var newModel = _v18.a;
				var newCmd = _v18.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$CU2, newCmd));
			case 'Spelling2':
				var message = msg.a;
				var _v19 = A2($author$project$Session2$Spelling$update, message, model);
				var newModel = _v19.a;
				var newCmd = _v19.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Spelling2, newCmd));
			case 'CU1':
				var submsg = msg.a;
				var _v20 = A2($author$project$Session1$ContextUnderstanding$update, submsg, model);
				var newModel = _v20.a;
				var newCmd = _v20.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$CU1, newCmd));
			case 'CU3':
				var message = msg.a;
				var _v21 = A2($author$project$Session3$CU3$update, message, model);
				var newModel = _v21.a;
				var newCmd = _v21.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$CU3, newCmd));
			case 'Spelling3':
				var message = msg.a;
				var _v22 = A2($author$project$Session3$Spelling3$update, message, model);
				var newModel = _v22.a;
				var newCmd = _v22.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Spelling3, newCmd));
			case 'YN':
				var message = msg.a;
				switch (message.$) {
					case 'UserClickedNextTrial':
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									yn: A2($author$project$Logic$next, $author$project$Session1$ContextUnderstanding$initState, model.yn)
								}),
							$elm$core$Platform$Cmd$none);
					case 'UserClickedToggleFeedback':
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									yn: $author$project$Logic$toggle(model.yn)
								}),
							$elm$core$Platform$Cmd$none);
					case 'UserClickedStartIntro':
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					case 'UserClickedStartMain':
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									yn: A2($author$project$Logic$startMain, model.yn, $author$project$Postest$YN$initState)
								}),
							$elm$core$Platform$Cmd$none);
					default:
						var _new = message.a;
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{
									yn: A2(
										$author$project$Logic$update,
										{uid: '', userAnswer: _new},
										model.yn)
								}),
							$elm$core$Platform$Cmd$none);
				}
			case 'Presentation':
				var message = msg.a;
				var _v24 = A2($author$project$Session1$Presentation$update, message, model);
				var subModel = _v24.a;
				var subCmd = _v24.b;
				return _Utils_Tuple2(
					subModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Presentation, subCmd));
			case 'Synonym':
				var message = msg.a;
				var _v25 = A2($author$project$Session3$Synonym$update, message, model);
				var newModel = _v25.a;
				var newCmd = _v25.b;
				return _Utils_Tuple2(
					newModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Synonym, newCmd));
			case 'Meaning':
				var submsg = msg.a;
				var _v26 = A2($author$project$Session1$Meaning$update, submsg, model);
				var subModel = _v26.a;
				var subCmd = _v26.b;
				return _Utils_Tuple2(
					subModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Meaning, subCmd));
			default:
				var submsg = msg.a;
				var _v27 = A2($author$project$Session2$Translation$update, submsg, model);
				var subModel = _v27.a;
				var subCmd = _v27.b;
				return _Utils_Tuple2(
					subModel,
					A2($elm$core$Platform$Cmd$map, $author$project$Main$Translation, subCmd));
		}
	});
var $author$project$Pretest$Acceptability$StartMain = F2(
	function (a, b) {
		return {$: 'StartMain', a: a, b: b};
	});
var $author$project$Pretest$Acceptability$StartTraining = {$: 'StartTraining'};
var $author$project$Postest$YN$UserChangedInput = function (a) {
	return {$: 'UserChangedInput', a: a};
};
var $author$project$Postest$YN$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Pretest$Acceptability$UserClickedSaveMsg = {$: 'UserClickedSaveMsg'};
var $author$project$Postest$YN$UserClickedStartMain = F2(
	function (a, b) {
		return {$: 'UserClickedStartMain', a: a, b: b};
	});
var $author$project$Postest$YN$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$Main$YN = function (a) {
	return {$: 'YN', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$Node = F3(
	function (a, b, c) {
		return {$: 'Node', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$Node;
var $rtfeldman$elm_css$Html$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$node;
var $rtfeldman$elm_css$Html$Styled$a = $rtfeldman$elm_css$Html$Styled$node('a');
var $rtfeldman$elm_css$Html$Styled$button = $rtfeldman$elm_css$Html$Styled$node('button');
var $rtfeldman$elm_css$VirtualDom$Styled$Attribute = F3(
	function (a, b, c) {
		return {$: 'Attribute', a: a, b: b, c: c};
	});
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$property = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$property, key, value),
			_List_Nil,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$class = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('className');
var $elm$json$Json$Encode$bool = _Json_wrap;
var $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$disabled = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('disabled');
var $rtfeldman$elm_css$VirtualDom$Styled$on = F2(
	function (eventName, handler) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$on, eventName, handler),
			_List_Nil,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Events$on = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$onClick = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $rtfeldman$elm_css$VirtualDom$Styled$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$text = function (str) {
	return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
		$elm$virtual_dom$VirtualDom$text(str));
};
var $rtfeldman$elm_css$Html$Styled$text = $rtfeldman$elm_css$VirtualDom$Styled$text;
var $author$project$View$button = function (_v0) {
	var message = _v0.message;
	var txt = _v0.txt;
	var isDisabled = _v0.isDisabled;
	return A2(
		$rtfeldman$elm_css$Html$Styled$button,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-xl w-full mt-6 mb-4'),
				$rtfeldman$elm_css$Html$Styled$Events$onClick(message),
				$rtfeldman$elm_css$Html$Styled$Attributes$disabled(isDisabled),
				$rtfeldman$elm_css$Html$Styled$Attributes$class(
				isDisabled ? 'invisible' : 'visible')
			]),
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$text(txt)
			]));
};
var $rtfeldman$elm_css$VirtualDom$Styled$attribute = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$attribute, key, value),
			_List_Nil,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$attribute = $rtfeldman$elm_css$VirtualDom$Styled$attribute;
var $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 'UniversalSelectorSequence', a: a};
};
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $rtfeldman$elm_css$Css$Structure$compactHelp = F2(
	function (declaration, _v0) {
		var keyframesByName = _v0.a;
		var declarations = _v0.b;
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var _v2 = declaration.a;
				var properties = _v2.c;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'MediaRule':
				var styleBlocks = declaration.b;
				return A2(
					$elm$core$List$all,
					function (_v3) {
						var properties = _v3.c;
						return $elm$core$List$isEmpty(properties);
					},
					styleBlocks) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'SupportsRule':
				var otherDeclarations = declaration.b;
				return $elm$core$List$isEmpty(otherDeclarations) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'DocumentRule':
				return _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'PageRule':
				var properties = declaration.b;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'FontFace':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'Keyframes':
				var record = declaration.a;
				return $elm$core$String$isEmpty(record.declaration) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					A3($elm$core$Dict$insert, record.name, record.declaration, keyframesByName),
					declarations);
			case 'Viewport':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'CounterStyle':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			default:
				var tuples = declaration.a;
				return A2(
					$elm$core$List$all,
					function (_v4) {
						var properties = _v4.b;
						return $elm$core$List$isEmpty(properties);
					},
					tuples) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
		}
	});
var $rtfeldman$elm_css$Css$Structure$Keyframes = function (a) {
	return {$: 'Keyframes', a: a};
};
var $rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations = F2(
	function (keyframesByName, compactedDeclarations) {
		return A2(
			$elm$core$List$append,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var decl = _v0.b;
					return $rtfeldman$elm_css$Css$Structure$Keyframes(
						{declaration: decl, name: name});
				},
				$elm$core$Dict$toList(keyframesByName)),
			compactedDeclarations);
	});
var $rtfeldman$elm_css$Css$Structure$compactStylesheet = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	var _v1 = A3(
		$elm$core$List$foldr,
		$rtfeldman$elm_css$Css$Structure$compactHelp,
		_Utils_Tuple2($elm$core$Dict$empty, _List_Nil),
		declarations);
	var keyframesByName = _v1.a;
	var compactedDeclarations = _v1.b;
	var finalDeclarations = A2($rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations, keyframesByName, compactedDeclarations);
	return {charset: charset, declarations: finalDeclarations, imports: imports, namespaces: namespaces};
};
var $rtfeldman$elm_css$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.feature + (A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			$elm$core$Basics$append(': '),
			expression.value)) + ')'));
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'print';
		case 'Screen':
			return 'screen';
		default:
			return 'speech';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				$elm$core$String$join,
				' and ',
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString(mediaType),
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 'AllQuery':
			var expressions = mediaQuery.a;
			return A2(
				$elm$core$String$join,
				' and ',
				A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions));
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString = F2(
	function (name, mediaQuery) {
		return '@import \"' + (name + ($rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString(mediaQuery) + '\"'));
	});
var $rtfeldman$elm_css$Css$Structure$Output$importToString = function (_v0) {
	var name = _v0.a;
	var mediaQueries = _v0.b;
	return A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString(name),
			mediaQueries));
};
var $rtfeldman$elm_css$Css$Structure$Output$namespaceToString = function (_v0) {
	var prefix = _v0.a;
	var str = _v0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var $rtfeldman$elm_css$Css$Structure$Output$spaceIndent = '    ';
var $rtfeldman$elm_css$Css$Structure$Output$indent = function (str) {
	return _Utils_ap($rtfeldman$elm_css$Css$Structure$Output$spaceIndent, str);
};
var $rtfeldman$elm_css$Css$Structure$Output$noIndent = '';
var $rtfeldman$elm_css$Css$Structure$Output$emitProperty = function (str) {
	return str + ';';
};
var $rtfeldman$elm_css$Css$Structure$Output$emitProperties = function (properties) {
	return A2(
		$elm$core$String$join,
		'\n',
		A2(
			$elm$core$List$map,
			A2($elm$core$Basics$composeL, $rtfeldman$elm_css$Css$Structure$Output$indent, $rtfeldman$elm_css$Css$Structure$Output$emitProperty),
			properties));
};
var $elm$core$String$append = _String_append;
var $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString = function (_v0) {
	var str = _v0.a;
	return '::' + str;
};
var $rtfeldman$elm_css$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator.$) {
		case 'AdjacentSibling':
			return '+';
		case 'GeneralSibling':
			return '~';
		case 'Child':
			return '>';
		default:
			return '';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 'ClassSelector':
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 'IdSelector':
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		case 'PseudoClassSelector':
			var str = repeatableSimpleSelector.a;
			return ':' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return '[' + (str + ']');
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 'TypeSelectorSequence':
			var str = simpleSelectorSequence.a.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$cons,
					str,
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
		case 'UniversalSelectorSequence':
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return $elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A2(
				$elm$core$String$join,
				'',
				A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors));
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return A2(
				$elm$core$String$join,
				'',
				A2(
					$elm$core$List$cons,
					str,
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, repeatableSimpleSelectors)));
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString = function (_v0) {
	var combinator = _v0.a;
	var sequence = _v0.b;
	return A2(
		$elm$core$String$join,
		' ',
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$Structure$Output$combinatorToString(combinator),
				$rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(sequence)
			]));
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorToString = function (_v0) {
	var simpleSelectorSequence = _v0.a;
	var chain = _v0.b;
	var pseudoElement = _v0.c;
	var segments = A2(
		$elm$core$List$cons,
		$rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence),
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		$elm$core$String$join,
		'',
		_List_fromArray(
			[
				A2(
				$elm$core$Maybe$withDefault,
				'',
				A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString, pseudoElement))
			]));
	return A2(
		$elm$core$String$append,
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$filter,
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
				segments)),
		pseudoElementsString);
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock = F2(
	function (indentLevel, _v0) {
		var firstSelector = _v0.a;
		var otherSelectors = _v0.b;
		var properties = _v0.c;
		var selectorStr = A2(
			$elm$core$String$join,
			', ',
			A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Css$Structure$Output$selectorToString,
				A2($elm$core$List$cons, firstSelector, otherSelectors)));
		return A2(
			$elm$core$String$join,
			'',
			_List_fromArray(
				[
					selectorStr,
					' {\n',
					indentLevel,
					$rtfeldman$elm_css$Css$Structure$Output$emitProperties(properties),
					'\n',
					indentLevel,
					'}'
				]));
	});
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration = function (decl) {
	switch (decl.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = decl.a;
			return A2($rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock, $rtfeldman$elm_css$Css$Structure$Output$noIndent, styleBlock);
		case 'MediaRule':
			var mediaQueries = decl.a;
			var styleBlocks = decl.b;
			var query = A2(
				$elm$core$String$join,
				',\n',
				A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString, mediaQueries));
			var blocks = A2(
				$elm$core$String$join,
				'\n\n',
				A2(
					$elm$core$List$map,
					A2(
						$elm$core$Basics$composeL,
						$rtfeldman$elm_css$Css$Structure$Output$indent,
						$rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock($rtfeldman$elm_css$Css$Structure$Output$spaceIndent)),
					styleBlocks));
			return '@media ' + (query + (' {\n' + (blocks + '\n}')));
		case 'SupportsRule':
			return 'TODO';
		case 'DocumentRule':
			return 'TODO';
		case 'PageRule':
			return 'TODO';
		case 'FontFace':
			return 'TODO';
		case 'Keyframes':
			var name = decl.a.name;
			var declaration = decl.a.declaration;
			return '@keyframes ' + (name + (' {\n' + (declaration + '\n}')));
		case 'Viewport':
			return 'TODO';
		case 'CounterStyle':
			return 'TODO';
		default:
			return 'TODO';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrint = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var declarations = _v0.declarations;
	return A2(
		$elm$core$String$join,
		'\n\n',
		A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$Output$charsetToString(charset),
					A2(
					$elm$core$String$join,
					'\n',
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$importToString, imports)),
					A2(
					$elm$core$String$join,
					'\n',
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$namespaceToString, namespaces)),
					A2(
					$elm$core$String$join,
					'\n\n',
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration, declarations))
				])));
};
var $rtfeldman$elm_css$Css$Structure$CounterStyle = function (a) {
	return {$: 'CounterStyle', a: a};
};
var $rtfeldman$elm_css$Css$Structure$FontFace = function (a) {
	return {$: 'FontFace', a: a};
};
var $rtfeldman$elm_css$Css$Structure$PageRule = F2(
	function (a, b) {
		return {$: 'PageRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 'Selector', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 'SupportsRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$Viewport = function (a) {
	return {$: 'Viewport', a: a};
};
var $rtfeldman$elm_css$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 'MediaRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$mapLast, update, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$withPropertyAppended = F2(
	function (property, _v0) {
		var firstSelector = _v0.a;
		var otherSelectors = _v0.b;
		var properties = _v0.c;
		return A3(
			$rtfeldman$elm_css$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var $rtfeldman$elm_css$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 'StyleBlockDeclaration':
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
								A2($rtfeldman$elm_css$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 'MediaRule':
						var _v1 = declarations.a;
						var mediaQueries = _v1.a;
						var styleBlocks = _v1.b;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									$rtfeldman$elm_css$Css$Structure$mapLast,
									$rtfeldman$elm_css$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					$rtfeldman$elm_css$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2($elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, rest, properties),
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var $rtfeldman$elm_css$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _v0) {
		var sequence = _v0.a;
		var selectors = _v0.b;
		return A3(
			$rtfeldman$elm_css$Css$Structure$Selector,
			sequence,
			selectors,
			$elm$core$Maybe$Just(pseudo));
	});
var $rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 'CustomSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 'TypeSelectorSequence', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 'TypeSelectorSequence':
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 'UniversalSelectorSequence':
				var list = sequence.a;
				return $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _v1 = list.a;
				var combinator = _v1.a;
				var sequence = _v1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				firstSelector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _v1 = declarations.a.a;
				var firstSelector = _v1.a;
				var otherSelectors = _v1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2($elm$core$List$cons, firstSelector, otherSelectors),
					$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 'DocumentRule', a: a, b: b, c: c, d: d, e: e};
	});
var $rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_v0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 'StyleBlockDeclaration':
							var styleBlock = declarations.a.a;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 'MediaRule':
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _v1 = declarations.a;
									var mediaQueries = _v1.a;
									var _v2 = _v1.b;
									var styleBlock = _v2.a;
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _v3 = declarations.a;
									var mediaQueries = _v3.a;
									var _v4 = _v3.b;
									var first = _v4.a;
									var rest = _v4.b;
									var _v5 = A2(
										$rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2($rtfeldman$elm_css$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_v5.b && (_v5.a.$ === 'MediaRule')) && (!_v5.b.b)) {
										var _v6 = _v5.a;
										var newMediaQueries = _v6.a;
										var newStyleBlocks = _v6.b;
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Css$Structure$MediaRule,
												newMediaQueries,
												A2($elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var newDeclarations = _v5;
										return newDeclarations;
									}
								}
							} else {
								break _v0$12;
							}
						case 'SupportsRule':
							var _v7 = declarations.a;
							var str = _v7.a;
							var nestedDeclarations = _v7.b;
							return _List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Structure$SupportsRule,
									str,
									A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 'DocumentRule':
							var _v8 = declarations.a;
							var str1 = _v8.a;
							var str2 = _v8.b;
							var str3 = _v8.c;
							var str4 = _v8.d;
							var styleBlock = _v8.e;
							return A2(
								$elm$core$List$map,
								A4($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 'PageRule':
							var _v9 = declarations.a;
							return declarations;
						case 'FontFace':
							return declarations;
						case 'Keyframes':
							return declarations;
						case 'Viewport':
							return declarations;
						case 'CounterStyle':
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _v0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			$elm$core$List$cons,
			first,
			A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$HashData = F4(
	function (shift, seed, hash, charsProcessed) {
		return {charsProcessed: charsProcessed, hash: hash, seed: seed, shift: shift};
	});
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$c1 = 3432918353;
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$c2 = 461845907;
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy = F2(
	function (b, a) {
		return ((a & 65535) * b) + ((((a >>> 16) * b) & 65535) << 16);
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$rotlBy = F2(
	function (b, a) {
		return (a << b) | (a >>> (32 - b));
	});
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$finalize = function (data) {
	var acc = (!(!data.hash)) ? (data.seed ^ A2(
		$rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy,
		$rtfeldman$elm_css$ElmCssVendor$Murmur3$c2,
		A2(
			$rtfeldman$elm_css$ElmCssVendor$Murmur3$rotlBy,
			15,
			A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy, $rtfeldman$elm_css$ElmCssVendor$Murmur3$c1, data.hash)))) : data.seed;
	var h0 = acc ^ data.charsProcessed;
	var h1 = A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy, 2246822507, h0 ^ (h0 >>> 16));
	var h2 = A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy, 3266489909, h1 ^ (h1 >>> 13));
	return (h2 ^ (h2 >>> 16)) >>> 0;
};
var $elm$core$String$foldl = _String_foldl;
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$mix = F2(
	function (h1, k1) {
		return A2(
			$rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy,
			5,
			A2(
				$rtfeldman$elm_css$ElmCssVendor$Murmur3$rotlBy,
				13,
				h1 ^ A2(
					$rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy,
					$rtfeldman$elm_css$ElmCssVendor$Murmur3$c2,
					A2(
						$rtfeldman$elm_css$ElmCssVendor$Murmur3$rotlBy,
						15,
						A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$multiplyBy, $rtfeldman$elm_css$ElmCssVendor$Murmur3$c1, k1))))) + 3864292196;
	});
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$hashFold = F2(
	function (c, data) {
		var res = data.hash | ((255 & $elm$core$Char$toCode(c)) << data.shift);
		var _v0 = data.shift;
		if (_v0 === 24) {
			return {
				charsProcessed: data.charsProcessed + 1,
				hash: 0,
				seed: A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$mix, data.seed, res),
				shift: 0
			};
		} else {
			return {charsProcessed: data.charsProcessed + 1, hash: res, seed: data.seed, shift: data.shift + 8};
		}
	});
var $rtfeldman$elm_css$ElmCssVendor$Murmur3$hashString = F2(
	function (seed, str) {
		return $rtfeldman$elm_css$ElmCssVendor$Murmur3$finalize(
			A3(
				$elm$core$String$foldl,
				$rtfeldman$elm_css$ElmCssVendor$Murmur3$hashFold,
				A4($rtfeldman$elm_css$ElmCssVendor$Murmur3$HashData, 0, seed, 0, 0),
				str));
	});
var $rtfeldman$elm_css$Hash$murmurSeed = 15739;
var $elm$core$String$fromList = _String_fromList;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return _Utils_chr('0');
			case 1:
				return _Utils_chr('1');
			case 2:
				return _Utils_chr('2');
			case 3:
				return _Utils_chr('3');
			case 4:
				return _Utils_chr('4');
			case 5:
				return _Utils_chr('5');
			case 6:
				return _Utils_chr('6');
			case 7:
				return _Utils_chr('7');
			case 8:
				return _Utils_chr('8');
			case 9:
				return _Utils_chr('9');
			case 10:
				return _Utils_chr('a');
			case 11:
				return _Utils_chr('b');
			case 12:
				return _Utils_chr('c');
			case 13:
				return _Utils_chr('d');
			case 14:
				return _Utils_chr('e');
			case 15:
				return _Utils_chr('f');
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			_Utils_chr('-'),
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $rtfeldman$elm_css$Hash$fromString = function (str) {
	return A2(
		$elm$core$String$cons,
		_Utils_chr('_'),
		$rtfeldman$elm_hex$Hex$toString(
			A2($rtfeldman$elm_css$ElmCssVendor$Murmur3$hashString, $rtfeldman$elm_css$Hash$murmurSeed, str)));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$last = function (list) {
	last:
	while (true) {
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return $elm$core$Maybe$Just(singleton);
			} else {
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue last;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return $elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 'Nothing') {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$FontFeatureValues = function (a) {
	return {$: 'FontFeatureValues', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _List_Nil;
		} else {
			var properties = tuplesToExpand.a;
			var rest = tuplesToExpand.b;
			return A2(
				$elm$core$List$cons,
				properties,
				expandTuples(rest));
		}
	};
	var newTuples = expandTuples(tuples);
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$FontFeatureValues(newTuples)
		]);
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule = F2(
	function (mediaQueries, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var styleBlock = declaration.a;
			return A2(
				$rtfeldman$elm_css$Css$Structure$MediaRule,
				mediaQueries,
				_List_fromArray(
					[styleBlock]));
		} else {
			return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var structureStyleBlock = declaration.a;
			return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var structureStyleBlock = declaration.a;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 'MediaRule':
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 'SupportsRule':
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$SupportsRule,
					str,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 'DocumentRule':
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 'PageRule':
				return declaration;
			case 'FontFace':
				return declaration;
			case 'Keyframes':
				return declaration;
			case 'Viewport':
				return declaration;
			case 'CounterStyle':
				return declaration;
			default:
				return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet = function (_v0) {
	var declarations = _v0.a;
	return declarations;
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(decls));
		};
		var nextResult = A2(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _v14 = _Utils_Tuple2(
				$elm$core$List$head(nextResult),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$last(declarations));
			if ((_v14.a.$ === 'Just') && (_v14.b.$ === 'Just')) {
				var nextResultParent = _v14.a.a;
				var originalParent = _v14.b.a;
				return _Utils_ap(
					A2(
						$elm$core$List$take,
						$elm$core$List$length(declarations) - 1,
						declarations),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var insertStylesToNestedDecl = function (lastDecl) {
			return $elm$core$List$concat(
				A2(
					$rtfeldman$elm_css$Css$Structure$mapLast,
					$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles(nestedStyles),
					A2(
						$elm$core$List$map,
						$elm$core$List$singleton,
						A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				insertStylesToNestedDecl,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return _Utils_ap(
			newDeclarations,
			_Utils_ap(
				withoutParent(initialResult),
				withoutParent(nextResult)));
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return declarations;
		} else {
			switch (styles.a.$) {
				case 'AppendProperty':
					var property = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, declarations));
				case 'ExtendSelector':
					var _v4 = styles.a;
					var selector = _v4.a;
					var nestedStyles = _v4.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 'NestSnippet':
					var _v5 = styles.a;
					var selectorCombinator = _v5.a;
					var snippets = _v5.b;
					var rest = styles.b;
					var chain = F2(
						function (_v9, _v10) {
							var originalSequence = _v9.a;
							var originalTuples = _v9.b;
							var originalPseudoElement = _v9.c;
							var newSequence = _v10.a;
							var newTuples = _v10.b;
							var newPseudoElement = _v10.c;
							return A3(
								$rtfeldman$elm_css$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								$rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 'StyleBlockDeclaration':
								var _v7 = declaration.a;
								var firstSelector = _v7.a;
								var otherSelectors = _v7.b;
								var nestedStyles = _v7.c;
								var newSelectors = A2(
									$elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											$elm$core$List$map,
											chain(originalSelector),
											A2($elm$core$List$cons, firstSelector, otherSelectors));
									},
									$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remainder = newSelectors.b;
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
												A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, remainder, _List_Nil))
											]);
									}
								}();
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations);
							case 'MediaRule':
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 'SupportsRule':
								var str = declaration.a;
								var otherSnippets = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, otherSnippets);
							case 'DocumentRule':
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A2(
									$elm$core$List$map,
									A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
									$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
							case 'PageRule':
								var str = declaration.a;
								var properties = declaration.b;
								return _List_fromArray(
									[
										A2($rtfeldman$elm_css$Css$Structure$PageRule, str, properties)
									]);
							case 'FontFace':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$FontFace(properties)
									]);
							case 'Viewport':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$Viewport(properties)
									]);
							case 'CounterStyle':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
									]);
							default:
								var tuples = declaration.a;
								return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return $elm$core$List$concat(
						_Utils_ap(
							_List_fromArray(
								[
									A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								$elm$core$List$map,
								expandDeclaration,
								A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets))));
				case 'WithPseudoElement':
					var _v11 = styles.a;
					var pseudoElement = _v11.a;
					var nestedStyles = _v11.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 'WithKeyframes':
					var str = styles.a.a;
					var rest = styles.b;
					var name = $rtfeldman$elm_css$Hash$fromString(str);
					var newProperty = 'animation-name:' + name;
					var newDeclarations = A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, newProperty, declarations));
					return A2(
						$elm$core$List$append,
						newDeclarations,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$Keyframes(
								{declaration: str, name: name})
							]));
				case 'WithMedia':
					var _v12 = styles.a;
					var mediaQueries = _v12.a;
					var nestedStyles = _v12.b;
					var rest = styles.b;
					var extraDeclarations = function () {
						var _v13 = $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_v13.b) {
							return _List_Nil;
						} else {
							var firstSelector = _v13.a;
							var otherSelectors = _v13.b;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule(mediaQueries),
								A2(
									$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
									nestedStyles,
									$elm$core$List$singleton(
										$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
											A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)))));
						}
					}();
					return _Utils_ap(
						A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations),
						extraDeclarations);
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock = function (_v2) {
	var firstSelector = _v2.a;
	var otherSelectors = _v2.b;
	var styles = _v2.c;
	return A2(
		$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
				A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return _List_Nil;
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		return _Utils_ap(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations(first),
			$rtfeldman$elm_css$Css$Preprocess$Resolve$extract(rest));
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			return A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		};
		return A2($elm$core$List$concatMap, handleStyleBlock, styleBlocks);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
			A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
		return _List_fromArray(
			[
				A2($rtfeldman$elm_css$Css$Structure$SupportsRule, str, declarations)
			]);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 'SupportsRule':
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 'DocumentRule':
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A2(
				$elm$core$List$map,
				A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		case 'PageRule':
			var str = snippetDeclaration.a;
			var properties = snippetDeclaration.b;
			return _List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$Structure$PageRule, str, properties)
				]);
		case 'FontFace':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$FontFace(properties)
				]);
		case 'Viewport':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$Viewport(properties)
				]);
		case 'CounterStyle':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
				]);
		default:
			var tuples = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure = function (_v0) {
	var charset = _v0.charset;
	var imports = _v0.imports;
	var namespaces = _v0.namespaces;
	var snippets = _v0.snippets;
	var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
		A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
	return {charset: charset, declarations: declarations, imports: imports, namespaces: namespaces};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$compileHelp = function (sheet) {
	return $rtfeldman$elm_css$Css$Structure$Output$prettyPrint(
		$rtfeldman$elm_css$Css$Structure$compactStylesheet(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure(sheet)));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$compile = function (styles) {
	return A2(
		$elm$core$String$join,
		'\n\n',
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Preprocess$Resolve$compileHelp, styles));
};
var $rtfeldman$elm_css$Css$Preprocess$Snippet = function (a) {
	return {$: 'Snippet', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$makeSnippet = F2(
	function (styles, sequence) {
		var selector = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$murmurSeed = 15739;
var $rtfeldman$elm_css$Css$Preprocess$stylesheet = function (snippets) {
	return {charset: $elm$core$Maybe$Nothing, imports: _List_Nil, namespaces: _List_Nil, snippets: snippets};
};
var $rtfeldman$elm_css$VirtualDom$Styled$getClassname = function (styles) {
	return $elm$core$List$isEmpty(styles) ? 'unstyled' : A2(
		$elm$core$String$cons,
		_Utils_chr('_'),
		$rtfeldman$elm_hex$Hex$toString(
			A2(
				$rtfeldman$elm_css$ElmCssVendor$Murmur3$hashString,
				$rtfeldman$elm_css$VirtualDom$Styled$murmurSeed,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
					$elm$core$List$singleton(
						$rtfeldman$elm_css$Css$Preprocess$stylesheet(
							$elm$core$List$singleton(
								A2(
									$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
									styles,
									$rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(_List_Nil)))))))));
};
var $rtfeldman$elm_css$Html$Styled$Internal$css = function (styles) {
	var classname = $rtfeldman$elm_css$VirtualDom$Styled$getClassname(styles);
	var classProperty = A2(
		$elm$virtual_dom$VirtualDom$property,
		'className',
		$elm$json$Json$Encode$string(classname));
	return A3($rtfeldman$elm_css$VirtualDom$Styled$Attribute, classProperty, styles, classname);
};
var $rtfeldman$elm_css$Html$Styled$Attributes$css = $rtfeldman$elm_css$Html$Styled$Internal$css;
var $rtfeldman$elm_css$Html$Styled$div = $rtfeldman$elm_css$Html$Styled$node('div');
var $rtfeldman$elm_css$Css$Preprocess$AppendProperty = function (a) {
	return {$: 'AppendProperty', a: a};
};
var $rtfeldman$elm_css$Css$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(key + (':' + value));
	});
var $rtfeldman$elm_css$Css$prop1 = F2(
	function (key, arg) {
		return A2($rtfeldman$elm_css$Css$property, key, arg.value);
	});
var $rtfeldman$elm_css$Css$marginTop = $rtfeldman$elm_css$Css$prop1('margin-top');
var $rtfeldman$elm_css$Css$RemUnits = {$: 'RemUnits'};
var $rtfeldman$elm_css$Css$Structure$Compatible = {$: 'Compatible'};
var $rtfeldman$elm_css$Css$Internal$lengthConverter = F3(
	function (units, unitLabel, numericValue) {
		return {
			absoluteLength: $rtfeldman$elm_css$Css$Structure$Compatible,
			calc: $rtfeldman$elm_css$Css$Structure$Compatible,
			flexBasis: $rtfeldman$elm_css$Css$Structure$Compatible,
			fontSize: $rtfeldman$elm_css$Css$Structure$Compatible,
			length: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
			lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
			numericValue: numericValue,
			textIndent: $rtfeldman$elm_css$Css$Structure$Compatible,
			unitLabel: unitLabel,
			units: units,
			value: _Utils_ap(
				$elm$core$String$fromFloat(numericValue),
				unitLabel)
		};
	});
var $rtfeldman$elm_css$Css$rem = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$RemUnits, 'rem');
var $author$project$View$theme = {
	headerHeight: $rtfeldman$elm_css$Css$rem(4)
};
var $author$project$View$container = function (content) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				A2($rtfeldman$elm_css$Html$Styled$Attributes$attribute, 'data-test', 'content'),
				$rtfeldman$elm_css$Html$Styled$Attributes$class('container mx-auto py-10 px-4'),
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$marginTop($author$project$View$theme.headerHeight)
					]))
			]),
		content);
};
var $elm_explorations$markdown$Markdown$defaultOptions = {
	defaultHighlighting: $elm$core$Maybe$Nothing,
	githubFlavored: $elm$core$Maybe$Just(
		{breaks: false, tables: false}),
	sanitize: true,
	smartypants: false
};
var $author$project$View$def = $elm_explorations$markdown$Markdown$defaultOptions;
var $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode = $rtfeldman$elm_css$VirtualDom$Styled$Unstyled;
var $rtfeldman$elm_css$Html$Styled$fromUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$unstyledNode;
var $elm_explorations$markdown$Markdown$toHtmlWith = _Markdown_toHtml;
var $author$project$View$fromMarkdown = A2(
	$elm$core$Basics$composeL,
	$rtfeldman$elm_css$Html$Styled$fromUnstyled,
	A2(
		$elm_explorations$markdown$Markdown$toHtmlWith,
		_Utils_update(
			$author$project$View$def,
			{sanitize: false}),
		_List_Nil));
var $rtfeldman$elm_css$Html$Styled$h1 = $rtfeldman$elm_css$Html$Styled$node('h1');
var $rtfeldman$elm_css$Css$height = $rtfeldman$elm_css$Css$prop1('height');
var $rtfeldman$elm_css$Html$Styled$Attributes$href = function (url) {
	return A2($rtfeldman$elm_css$Html$Styled$Attributes$stringProperty, 'href', url);
};
var $rtfeldman$elm_css$Html$Styled$p = $rtfeldman$elm_css$Html$Styled$node('p');
var $rtfeldman$elm_css$Html$Styled$ul = $rtfeldman$elm_css$Html$Styled$node('ul');
var $author$project$View$header = function (items) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('fixed z-7 top-0 inset-x-0 bg-white border-b border-gray-300'),
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$height($author$project$View$theme.headerHeight)
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('container mx-auto h-full'),
						$rtfeldman$elm_css$Html$Styled$Attributes$class('flex items-center px-6')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$a,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Html$Styled$Attributes$attribute, 'data-test', 'logo'),
								$rtfeldman$elm_css$Html$Styled$Attributes$class('flex items-center'),
								$rtfeldman$elm_css$Html$Styled$Attributes$href('/')
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('👩\u200D🎓️')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$p,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold uppercase text-sm text-gray-800')
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Lex Learn')
									]))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$ul,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Html$Styled$Attributes$attribute, 'data-test', 'menu'),
								$rtfeldman$elm_css$Html$Styled$Attributes$class('flex-grow'),
								$rtfeldman$elm_css$Html$Styled$Attributes$class('flex justify-end')
							]),
						items)
					]))
			]));
};
var $rtfeldman$elm_css$VirtualDom$Styled$KeyedNode = F3(
	function (a, b, c) {
		return {$: 'KeyedNode', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$KeyedNodeNS = F4(
	function (a, b, c, d) {
		return {$: 'KeyedNodeNS', a: a, b: b, c: c, d: d};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$NodeNS = F4(
	function (a, b, c, d) {
		return {$: 'NodeNS', a: a, b: b, c: c, d: d};
	});
var $elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
var $rtfeldman$elm_css$VirtualDom$Styled$mapAttribute = F2(
	function (transform, _v0) {
		var prop = _v0.a;
		var styles = _v0.b;
		var classname = _v0.c;
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$mapAttribute, transform, prop),
			styles,
			classname);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$map = F2(
	function (transform, vdomNode) {
		switch (vdomNode.$) {
			case 'Node':
				var elemType = vdomNode.a;
				var properties = vdomNode.b;
				var children = vdomNode.c;
				return A3(
					$rtfeldman$elm_css$VirtualDom$Styled$Node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$map(transform),
						children));
			case 'NodeNS':
				var ns = vdomNode.a;
				var elemType = vdomNode.b;
				var properties = vdomNode.c;
				var children = vdomNode.d;
				return A4(
					$rtfeldman$elm_css$VirtualDom$Styled$NodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$map(transform),
						children));
			case 'KeyedNode':
				var elemType = vdomNode.a;
				var properties = vdomNode.b;
				var children = vdomNode.c;
				return A3(
					$rtfeldman$elm_css$VirtualDom$Styled$KeyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						function (_v1) {
							var key = _v1.a;
							var child = _v1.b;
							return _Utils_Tuple2(
								key,
								A2($rtfeldman$elm_css$VirtualDom$Styled$map, transform, child));
						},
						children));
			case 'KeyedNodeNS':
				var ns = vdomNode.a;
				var elemType = vdomNode.b;
				var properties = vdomNode.c;
				var children = vdomNode.d;
				return A4(
					$rtfeldman$elm_css$VirtualDom$Styled$KeyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						function (_v2) {
							var key = _v2.a;
							var child = _v2.b;
							return _Utils_Tuple2(
								key,
								A2($rtfeldman$elm_css$VirtualDom$Styled$map, transform, child));
						},
						children));
			default:
				var vdom = vdomNode.a;
				return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
					A2($elm$virtual_dom$VirtualDom$map, transform, vdom));
		}
	});
var $rtfeldman$elm_css$Html$Styled$map = $rtfeldman$elm_css$VirtualDom$Styled$map;
var $rtfeldman$elm_css$Html$Styled$li = $rtfeldman$elm_css$Html$Styled$node('li');
var $author$project$View$item = F2(
	function (name, attributes) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$li,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('mr-6')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$a,
					attributes,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(name)
						]))
				]));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$target = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('target');
var $author$project$View$navOut = F2(
	function (name, url) {
		return A2(
			$author$project$View$item,
			name,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$href(url),
					$rtfeldman$elm_css$Html$Styled$Attributes$target('_blank'),
					$rtfeldman$elm_css$Html$Styled$Attributes$class('external')
				]));
	});
var $author$project$View$notFound = _List_fromArray(
	[
		A2(
		$rtfeldman$elm_css$Html$Styled$h1,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('text-2xl text-gray-500')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('404')
					])),
				$rtfeldman$elm_css$Html$Styled$text('Sorry, we could not find this page.')
			])),
		A2(
		$rtfeldman$elm_css$Html$Styled$p,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$a,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('btn'),
						$rtfeldman$elm_css$Html$Styled$Attributes$href('/')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Home')
					]))
			]))
	]);
var $rtfeldman$elm_css$Html$Styled$Attributes$checked = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('checked');
var $rtfeldman$elm_css$Html$Styled$fieldset = $rtfeldman$elm_css$Html$Styled$node('fieldset');
var $rtfeldman$elm_css$Html$Styled$Attributes$for = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('htmlFor');
var $rtfeldman$elm_css$Html$Styled$Attributes$id = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('id');
var $rtfeldman$elm_css$Html$Styled$input = $rtfeldman$elm_css$Html$Styled$node('input');
var $rtfeldman$elm_css$Html$Styled$label = $rtfeldman$elm_css$Html$Styled$node('label');
var $rtfeldman$elm_css$Html$Styled$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rtfeldman$elm_css$Html$Styled$Events$onInput = function (tagger) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$rtfeldman$elm_css$Html$Styled$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $rtfeldman$elm_css$Html$Styled$Events$targetValue)));
};
var $rtfeldman$elm_css$Html$Styled$span = $rtfeldman$elm_css$Html$Styled$node('span');
var $rtfeldman$elm_css$Html$Styled$Attributes$type_ = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('type');
var $rtfeldman$elm_css$Html$Styled$Attributes$value = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('value');
var $author$project$View$trainingBox = $rtfeldman$elm_css$Html$Styled$div(
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col items-center justify-center w-full h-full border-4 border-green-500 border-rounded-lg border-dashed ')
		]));
var $rtfeldman$elm_css$Html$Styled$h2 = $rtfeldman$elm_css$Html$Styled$node('h2');
var $rtfeldman$elm_css$Html$Styled$pre = $rtfeldman$elm_css$Html$Styled$node('pre');
var $author$project$View$viewInstructions = function (instructions_) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col')
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Instructions')
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$p,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('pt-8 pb-8 font-medium')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$pre,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$View$fromMarkdown(instructions_)
							]))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg text-green-500 font-bold pb-2')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$span,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Practice here !')
							]))
					]))
			]));
};
var $author$project$View$viewTraining = F2(
	function (instructions_, content) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col')
				]),
			_List_fromArray(
				[
					$author$project$View$viewInstructions(instructions_),
					$author$project$View$trainingBox(content)
				]));
	});
var $author$project$Postest$YN$view = F2(
	function (exp, _v0) {
		var toggleFeedback = _v0.toggleFeedback;
		var nextTrialMsg = _v0.nextTrialMsg;
		var startMainMsg = _v0.startMainMsg;
		var userChangedInput = _v0.userChangedInput;
		switch (exp.$) {
			case 'NotStarted':
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('experiment did not start yet')
						]));
			case 'Loading':
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Loading...')
						]));
			case 'Err':
				var reason = exp.a;
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(reason)
						]));
			default:
				switch (exp.a.$) {
					case 'Instructions':
						var _v2 = exp.a;
						return A2($rtfeldman$elm_css$Html$Styled$div, _List_Nil, _List_Nil);
					case 'Training':
						var _v3 = exp.a;
						var mainTrials = exp.b.mainTrials;
						var current = exp.b.current;
						var feedback = exp.b.feedback;
						var infos = exp.b.infos;
						if (current.$ === 'Just') {
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$author$project$View$viewTraining,
										infos.instructions,
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_Nil,
												feedback ? _List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('I\'m feedback'),
														$author$project$View$button(
														{isDisabled: false, message: nextTrialMsg, txt: 'Next Training Item'})
													]) : _List_fromArray(
													[
														$author$project$View$button(
														{isDisabled: false, message: toggleFeedback, txt: 'togglefeedback'})
													]))
											]))
									]));
						} else {
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Intro is over'),
										$author$project$View$button(
										{
											isDisabled: false,
											message: A2(startMainMsg, mainTrials, infos),
											txt: 'Start'
										})
									]));
						}
					default:
						var _v5 = exp.a;
						var current = exp.b.current;
						var state = exp.b.state;
						var infos = exp.b.infos;
						if (current.$ === 'Just') {
							var trial = current.a;
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$span,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.word)
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$fieldset,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col')
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$input,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
																$rtfeldman$elm_css$Html$Styled$Attributes$value('true'),
																$rtfeldman$elm_css$Html$Styled$Attributes$checked(state.userAnswer === 'true'),
																$rtfeldman$elm_css$Html$Styled$Events$onInput(userChangedInput),
																$rtfeldman$elm_css$Html$Styled$Attributes$id('truecb')
															]),
														_List_Nil),
														A2(
														$rtfeldman$elm_css$Html$Styled$label,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$for('truecb'),
																$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-4 hover:underline')
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('Exists')
															]))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$input,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
																$rtfeldman$elm_css$Html$Styled$Attributes$value('false'),
																$rtfeldman$elm_css$Html$Styled$Attributes$checked(state.userAnswer === 'false'),
																$rtfeldman$elm_css$Html$Styled$Events$onInput(userChangedInput),
																$rtfeldman$elm_css$Html$Styled$Attributes$id('falsecb')
															]),
														_List_Nil),
														A2(
														$rtfeldman$elm_css$Html$Styled$label,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$for('falsecb'),
																$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-4 hover:underline')
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('Does not exist')
															]))
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_Nil,
										_List_fromArray(
											[
												$author$project$View$button(
												{isDisabled: false, message: nextTrialMsg, txt: 'Next Item'})
											]))
									]));
						} else {
							return $rtfeldman$elm_css$Html$Styled$text(infos.end);
						}
				}
		}
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$height = function (n) {
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$attribute,
		'height',
		$elm$core$String$fromInt(n));
};
var $rtfeldman$elm_css$Html$Styled$img = $rtfeldman$elm_css$Html$Styled$node('img');
var $rtfeldman$elm_css$Html$Styled$Attributes$src = function (url) {
	return A2($rtfeldman$elm_css$Html$Styled$Attributes$stringProperty, 'src', url);
};
var $author$project$Pretest$Acceptability$viewTransition = F3(
	function (infos, msg, buttontext) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center justify-center')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$View$fromMarkdown(infos)
						])),
					$author$project$View$button(
					{isDisabled: false, message: msg, txt: buttontext})
				]));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$width = function (n) {
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $author$project$Pretest$Acceptability$view = F2(
	function (task, _v0) {
		var startMainMsg = _v0.startMainMsg;
		var startTraining = _v0.startTraining;
		var saveDataMsg = _v0.saveDataMsg;
		var prompt = A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center justify-center')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$img,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$src('/acceptability.png'),
							$rtfeldman$elm_css$Html$Styled$Attributes$class('items-center justify-center'),
							$rtfeldman$elm_css$Html$Styled$Attributes$width(500),
							$rtfeldman$elm_css$Html$Styled$Attributes$height(500)
						]),
					_List_Nil)
				]));
		switch (task.$) {
			case 'Loading':
				return _List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading... Please don\'t exit this page, data may be lost.')
					]);
			case 'NotStarted':
				return _List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('not started')
					]);
			case 'Err':
				var reason = task.a;
				return _List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Oups, I ran into the following error: ' + reason)
							]))
					]);
			default:
				switch (task.a.$) {
					case 'Training':
						var _v2 = task.a;
						var data = task.b;
						var _v3 = data.current;
						if (_v3.$ === 'Nothing') {
							return _List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
										]),
									_List_fromArray(
										[
											$author$project$View$button(
											{
												isDisabled: false,
												message: A2(startMainMsg, data.infos, data.mainTrials),
												txt: 'That\'s it for the practice items'
											})
										]))
								]);
						} else {
							var trial = _v3.a;
							var _v4 = data.state.step;
							switch (_v4.$) {
								case 'Init':
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$p,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col  text-center ')
												]),
											_List_fromArray(
												[
													$author$project$View$fromMarkdown(data.infos.instructions_short)
												]))
										]);
								case 'End':
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center justify-center')
												]),
											_List_fromArray(
												[
													$author$project$View$fromMarkdown(trial.feedback)
												]))
										]);
								default:
									return _List_fromArray(
										[prompt]);
							}
						}
					case 'Main':
						var _v5 = task.a;
						var data = task.b;
						var _v6 = data.current;
						if (_v6.$ === 'Nothing') {
							return _List_fromArray(
								[
									A3($author$project$Pretest$Acceptability$viewTransition, data.infos.end, saveDataMsg, 'Click to save your answers')
								]);
						} else {
							var _v7 = data.state.step;
							switch (_v7.$) {
								case 'Init':
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$p,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col  text-center ')
												]),
											_List_fromArray(
												[
													$author$project$View$fromMarkdown(data.infos.trainingWheel)
												]))
										]);
								case 'End':
									return _List_Nil;
								default:
									return _List_fromArray(
										[prompt]);
							}
						}
					default:
						var _v8 = task.a;
						return _List_Nil;
				}
		}
	});
var $author$project$Pretest$SPR$StartMain = F2(
	function (a, b) {
		return {$: 'StartMain', a: a, b: b};
	});
var $author$project$Pretest$SPR$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Pretest$SPR$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$View$instructions = F2(
	function (content, msgToTraining) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h1,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Instructions')
						])),
					$author$project$View$fromMarkdown(content),
					$author$project$View$button(
					{isDisabled: false, message: msgToTraining, txt: 'Start Training'})
				]));
	});
var $author$project$Pretest$SPR$viewTask = F3(
	function (data, trial, endTrialMsg) {
		var _v0 = _Utils_Tuple2(data.state.step, data.state.currentSegment);
		switch (_v0.a.$) {
			case 'SPR':
				if (_v0.b.$ === 'Just') {
					var s = _v0.a.a;
					var taggedSegment = _v0.b.a.taggedSegment;
					if (s.$ === 'Start') {
						return A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('text-bold')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('Press the space bar to start reading')
								]));
					} else {
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('w-max h-max flex flex-col items-center pt-16 pb-16 border-2 text-bold text-lg')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg items-center')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(taggedSegment.b)
										]))
								]));
					}
				} else {
					var _v2 = _v0.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Press the space to start')
							]));
				}
			case 'Question':
				var _v3 = _v0.a;
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('w-max h-max flex flex-col items-center pt-16 pb-16 border-2')
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(trial.question)
								])),
							$rtfeldman$elm_css$Html$Styled$text('Press Y for Yes, N for No and K for I don\'t know')
						]));
			default:
				var _v4 = _v0.a;
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('w-max h-max flex flex-col items-center pt-16 pb-16 border-2')
						]),
					_List_fromArray(
						[
							$author$project$View$fromMarkdown(trial.feedback)
						]));
		}
	});
var $author$project$Pretest$SPR$view = function (task) {
	switch (task.$) {
		case 'Loading':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('Loading... Please don\'t exit or data may be lost')
				]);
		case 'Err':
			var reason = task.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('I encountered the following error: ' + reason)
				]);
		case 'NotStarted':
			return _List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Thanks for your participation !')
						]))
				]);
		default:
			switch (task.a.$) {
				case 'Training':
					var _v1 = task.a;
					var data = task.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						var trial = _v2.a;
						return _List_fromArray(
							[
								A3($author$project$Pretest$SPR$viewTask, data, trial, $author$project$Pretest$SPR$UserConfirmedChoice)
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										$author$project$View$fromMarkdown(data.infos.introToMain),
										$author$project$View$button(
										{
											isDisabled: false,
											message: A2($author$project$Pretest$SPR$StartMain, data.infos, data.mainTrials),
											txt: 'Start'
										})
									]))
							]);
					}
				case 'Main':
					var _v3 = task.a;
					var data = task.b;
					var _v4 = data.current;
					if (_v4.$ === 'Just') {
						var trial = _v4.a;
						return _List_fromArray(
							[
								A3($author$project$Pretest$SPR$viewTask, data, trial, $author$project$Pretest$SPR$UserClickedNextTrial)
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										$author$project$View$fromMarkdown(data.infos.end),
										$author$project$View$button(
										{isDisabled: false, message: $author$project$Pretest$SPR$UserClickedSaveData, txt: 'Click here to save your data'})
									]))
							]);
					}
				default:
					var _v5 = task.a;
					var data = task.b;
					return _List_fromArray(
						[
							A2($author$project$View$instructions, data.infos.instructions, $author$project$Pretest$SPR$UserClickedStartTraining)
						]);
			}
	}
};
var $author$project$Pretest$SentenceCompletion$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Pretest$SentenceCompletion$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Pretest$SentenceCompletion$UserClickedStartMain = F2(
	function (a, b) {
		return {$: 'UserClickedStartMain', a: a, b: b};
	});
var $author$project$Pretest$SentenceCompletion$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Pretest$SentenceCompletion$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$Pretest$SentenceCompletion$UserUpdatedField = F2(
	function (a, b) {
		return {$: 'UserUpdatedField', a: a, b: b};
	});
var $author$project$View$end = F3(
	function (endInfo, saveDataMsg, linkToNextTask) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col w-full items-center')
				]),
			_List_fromArray(
				[
					$author$project$View$fromMarkdown(endInfo),
					A2(
					$rtfeldman$elm_css$Html$Styled$a,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$href(linkToNextTask)
						]),
					_List_fromArray(
						[
							$author$project$View$button(
							{isDisabled: false, message: saveDataMsg, txt: 'Click here when you are ready !'})
						]))
				]));
	});
var $author$project$View$navigationButton = F3(
	function (toggleFeedbackMsg, nextTrialMsg, feedback) {
		return $author$project$View$button(
			(!feedback) ? {isDisabled: false, message: toggleFeedbackMsg, txt: 'Check my answer'} : {isDisabled: false, message: nextTrialMsg, txt: 'Next item '});
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$placeholder = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('placeholder');
var $rtfeldman$elm_css$Html$Styled$Attributes$spellcheck = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('spellcheck');
var $rtfeldman$elm_css$Html$Styled$textarea = $rtfeldman$elm_css$Html$Styled$node('textarea');
var $author$project$Pretest$SentenceCompletion$view = function (task) {
	switch (task.$) {
		case 'Err':
			var reason = task.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(reason)
				]);
		case 'Loading':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('Loading... Please don\'t quit or data may be lost')
				]);
		case 'NotStarted':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('C\'est tout bon!')
				]);
		default:
			switch (task.a.$) {
				case 'Training':
					var _v1 = task.a;
					var data = task.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						var trial = _v2.a;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg  m-4 p-2')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.context)
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$textarea,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$id('firstProd'),
												$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2'),
												$rtfeldman$elm_css$Html$Styled$Events$onInput(
												$author$project$Pretest$SentenceCompletion$UserUpdatedField($author$project$Pretest$SentenceCompletion$FirstProduction)),
												$rtfeldman$elm_css$Html$Styled$Attributes$spellcheck(false),
												$rtfeldman$elm_css$Html$Styled$Attributes$placeholder(trial.firstAmorce)
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.firstAmorce)
											])),
										data.feedback ? $author$project$View$fromMarkdown(trial.firstFeedback) : $rtfeldman$elm_css$Html$Styled$text(''),
										A2(
										$rtfeldman$elm_css$Html$Styled$textarea,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$id('secondProd'),
												$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2'),
												$rtfeldman$elm_css$Html$Styled$Events$onInput(
												$author$project$Pretest$SentenceCompletion$UserUpdatedField($author$project$Pretest$SentenceCompletion$SecondProduction)),
												$rtfeldman$elm_css$Html$Styled$Attributes$spellcheck(false)
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.secondAmorce)
											])),
										data.feedback ? $author$project$View$fromMarkdown(trial.secondFeedback) : $rtfeldman$elm_css$Html$Styled$text('')
									])),
								A3($author$project$View$navigationButton, $author$project$Pretest$SentenceCompletion$UserClickedToggleFeedback, $author$project$Pretest$SentenceCompletion$UserClickedNextTrial, data.feedback)
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										$author$project$View$fromMarkdown(data.infos.introToMain),
										$author$project$View$button(
										{
											isDisabled: false,
											message: A2($author$project$Pretest$SentenceCompletion$UserClickedStartMain, data.infos, data.mainTrials),
											txt: 'Start'
										})
									]))
							]);
					}
				case 'Main':
					var _v3 = task.a;
					var data = task.b;
					var _v4 = data.current;
					if (_v4.$ === 'Just') {
						var trial = _v4.a;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('text-center text-lg m-2 p-2')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.context)
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class(
												'flex flex-col ' + (_Utils_eq(data.state.order, $author$project$Pretest$SentenceCompletion$FirstProduction) ? 'order-2' : 'order-3'))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$textarea,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$id('firstProd'),
														$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2'),
														$rtfeldman$elm_css$Html$Styled$Events$onInput(
														$author$project$Pretest$SentenceCompletion$UserUpdatedField($author$project$Pretest$SentenceCompletion$FirstProduction)),
														$rtfeldman$elm_css$Html$Styled$Attributes$spellcheck(false)
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(trial.firstAmorce)
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class(
												'flex flex-col ' + (_Utils_eq(data.state.order, $author$project$Pretest$SentenceCompletion$SecondProduction) ? 'order-2' : 'order-3'))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$textarea,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$id('secondProd'),
														$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2'),
														$rtfeldman$elm_css$Html$Styled$Events$onInput(
														$author$project$Pretest$SentenceCompletion$UserUpdatedField($author$project$Pretest$SentenceCompletion$SecondProduction)),
														$rtfeldman$elm_css$Html$Styled$Attributes$spellcheck(false)
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(trial.secondAmorce)
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('order-last')
											]),
										_List_fromArray(
											[
												$author$project$View$button(
												{isDisabled: false, message: $author$project$Pretest$SentenceCompletion$UserClickedNextTrial, txt: 'Next Item'})
											]))
									]))
							]);
					} else {
						return _List_fromArray(
							[
								A3($author$project$View$end, data.infos.end, $author$project$Pretest$SentenceCompletion$UserClickedSaveData, '')
							]);
					}
				default:
					var _v5 = task.a;
					var data = task.b;
					return _List_fromArray(
						[
							A2($author$project$View$instructions, data.infos.instructions, $author$project$Pretest$SentenceCompletion$UserClickedStartTraining)
						]);
			}
	}
};
var $author$project$Pretest$VKS$UserClickedNewKnowledge = function (a) {
	return {$: 'UserClickedNewKnowledge', a: a};
};
var $author$project$Pretest$VKS$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Pretest$VKS$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Pretest$VKS$UserClickedStartMain = {$: 'UserClickedStartMain'};
var $author$project$Pretest$VKS$UserUpdatedField = F2(
	function (a, b) {
		return {$: 'UserUpdatedField', a: a, b: b};
	});
var $author$project$Pretest$VKS$view = function (task) {
	switch (task.$) {
		case 'Err':
			var reason = task.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(reason)
				]);
		case 'Loading':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('Loading... Please don\'t quit or data may be lost')
				]);
		case 'NotStarted':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('C\'est tout bon!')
				]);
		default:
			switch (task.a.$) {
				case 'Training':
					var _v1 = task.a;
					var data = task.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						return _List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('vks')
							]);
					} else {
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										$author$project$View$fromMarkdown(data.infos.introToMain),
										$author$project$View$button(
										{isDisabled: false, message: $author$project$Pretest$VKS$UserClickedStartMain, txt: 'Start'})
									]))
							]);
					}
				case 'Main':
					var _v3 = task.a;
					var data = task.b;
					var _v4 = data.current;
					if (_v4.$ === 'Just') {
						var trial = _v4.a;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$span,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg font-bold')
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(trial.verb)
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$fieldset,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col m-2')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
														$rtfeldman$elm_css$Html$Styled$Attributes$id('ns'),
														$rtfeldman$elm_css$Html$Styled$Attributes$value('NeverSeen'),
														$rtfeldman$elm_css$Html$Styled$Attributes$checked(
														_Utils_eq(data.state.knowledge, $author$project$Pretest$VKS$NeverSeen)),
														$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Pretest$VKS$UserClickedNewKnowledge)
													]),
												_List_Nil),
												A2(
												$rtfeldman$elm_css$Html$Styled$span,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$class('p-2')
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('I don’t remember having seen this verb before')
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
														$rtfeldman$elm_css$Html$Styled$Attributes$value('PreviouslySeen'),
														$rtfeldman$elm_css$Html$Styled$Attributes$checked(
														_Utils_eq(data.state.knowledge, $author$project$Pretest$VKS$PreviouslySeen)),
														$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Pretest$VKS$UserClickedNewKnowledge)
													]),
												_List_Nil),
												A2(
												$rtfeldman$elm_css$Html$Styled$span,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$class('p-2')
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('I have seen this verb before, but I don’t know what it means')
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
														$rtfeldman$elm_css$Html$Styled$Attributes$value('Known'),
														$rtfeldman$elm_css$Html$Styled$Attributes$checked(
														_Utils_eq(data.state.knowledge, $author$project$Pretest$VKS$Known)),
														$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Pretest$VKS$UserClickedNewKnowledge)
													]),
												_List_Nil),
												A2(
												$rtfeldman$elm_css$Html$Styled$span,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$class('p-2')
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('I have seen this verb before, and I think I know what it means')
													]))
											]))
									])),
								_Utils_eq(data.state.knowledge, $author$project$Pretest$VKS$Known) ? A2(
								$rtfeldman$elm_css$Html$Styled$fieldset,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col p-2')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('What do you think this verb means? (please provide a translation, synonym or definition or all meanings of this verb that you know):'),
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
														$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2'),
														$rtfeldman$elm_css$Html$Styled$Events$onInput(
														$author$project$Pretest$VKS$UserUpdatedField($author$project$Pretest$VKS$FirstProduction))
													]),
												_List_Nil)
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Please use this verb in a sentence. The sentence should show that you know what the word means.'),
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
														$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2'),
														$rtfeldman$elm_css$Html$Styled$Events$onInput(
														$author$project$Pretest$VKS$UserUpdatedField($author$project$Pretest$VKS$SecondProduction))
													]),
												_List_Nil)
											]))
									])) : $rtfeldman$elm_css$Html$Styled$text(''),
								$author$project$View$button(
								{isDisabled: false, message: $author$project$Pretest$VKS$UserClickedNextTrial, txt: 'Next Item'})
							]);
					} else {
						return _List_fromArray(
							[
								A3($author$project$View$end, data.infos.end, $author$project$Pretest$VKS$UserClickedSaveData, '')
							]);
					}
				default:
					var _v5 = task.a;
					var data = task.b;
					return _List_fromArray(
						[
							A2($author$project$View$instructions, data.infos.instructions, $author$project$Pretest$VKS$UserClickedStartMain)
						]);
			}
	}
};
var $author$project$Session1$ContextUnderstanding$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session1$ContextUnderstanding$UserClickedRadioButton = function (a) {
	return {$: 'UserClickedRadioButton', a: a};
};
var $author$project$Session1$ContextUnderstanding$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session1$ContextUnderstanding$UserClickedStartMain = F2(
	function (a, b) {
		return {$: 'UserClickedStartMain', a: a, b: b};
	});
var $author$project$Session1$ContextUnderstanding$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session1$ContextUnderstanding$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$View$bold = function (string) {
	return '**' + (string + '**');
};
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$polyline = $elm$svg$Svg$trustedNode('polyline');
var $elm$svg$Svg$Attributes$class = _VirtualDom_attribute('class');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeLinecap = _VirtualDom_attribute('stroke-linecap');
var $elm$svg$Svg$Attributes$strokeLinejoin = _VirtualDom_attribute('stroke-linejoin');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $author$project$Icons$svgFeatherIcon = function (className) {
	return $elm$svg$Svg$svg(
		_List_fromArray(
			[
				$elm$svg$Svg$Attributes$class('feather feather-' + className),
				$elm$svg$Svg$Attributes$fill('none'),
				$elm$svg$Svg$Attributes$stroke('currentColor'),
				$elm$svg$Svg$Attributes$strokeLinecap('round'),
				$elm$svg$Svg$Attributes$strokeLinejoin('round'),
				$elm$svg$Svg$Attributes$strokeWidth('2'),
				$elm$svg$Svg$Attributes$viewBox('0 0 24 24'),
				$elm$svg$Svg$Attributes$width('100%')
			]));
};
var $author$project$Icons$checkCircle = A2(
	$author$project$Icons$svgFeatherIcon,
	'check-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M22 11.08V12a10 10 0 1 1-5.93-9.14')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polyline,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('22 4 12 14.01 9 11.01')
				]),
			_List_Nil)
		]));
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $lukewestby$elm_string_interpolate$String$Interpolate$applyInterpolation = F2(
	function (replacements, _v0) {
		var match = _v0.match;
		var ordinalString = A2(
			$elm$core$Basics$composeL,
			$elm$core$String$dropLeft(1),
			$elm$core$String$dropRight(1))(match);
		return A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$andThen,
				function (value) {
					return A2($elm$core$Array$get, value, replacements);
				},
				$elm$core$String$toInt(ordinalString)));
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $lukewestby$elm_string_interpolate$String$Interpolate$interpolationRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('\\{\\d+\\}'));
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $lukewestby$elm_string_interpolate$String$Interpolate$interpolate = F2(
	function (string, args) {
		var asArray = $elm$core$Array$fromList(args);
		return A3(
			$elm$regex$Regex$replace,
			$lukewestby$elm_string_interpolate$String$Interpolate$interpolationRegex,
			$lukewestby$elm_string_interpolate$String$Interpolate$applyInterpolation(asArray),
			string);
	});
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Icons$xCircle = A2(
	$author$project$Icons$svgFeatherIcon,
	'x-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('15'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('9'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('9'),
					$elm$svg$Svg$Attributes$y1('9'),
					$elm$svg$Svg$Attributes$x2('15'),
					$elm$svg$Svg$Attributes$y2('15')
				]),
			_List_Nil)
		]));
var $author$project$View$genericSingleChoiceFeedback = function (data) {
	var feedback_Correct = data.feedback_Correct;
	var feedback_Incorrect = data.feedback_Incorrect;
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class(
				' w-full max-w-2xl rounded-md text-center object-center  mb-8 ' + ((data.isVisible && _Utils_eq(data.userAnswer, data.target)) ? 'bg-green-700' : ((data.isVisible && (!_Utils_eq(data.userAnswer, data.target))) ? 'bg-red-700' : ((!data.isVisible) ? '' : ''))))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$p,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class(
						'text-xl py-4 w-full flex flex-col items-center justify-center  text-white p-2' + (' ' + (data.isVisible ? 'visible' : 'invisible')))
					]),
				_Utils_eq(data.userAnswer, data.target) ? _List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('w-12 h-12')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$checkCircle)
							])),
						$author$project$View$fromMarkdown(
						A2($lukewestby$elm_string_interpolate$String$Interpolate$interpolate, feedback_Correct.a, feedback_Correct.b))
					]) : _List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('w-12 h-12')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$xCircle)
							])),
						$author$project$View$fromMarkdown(
						A2($lukewestby$elm_string_interpolate$String$Interpolate$interpolate, feedback_Incorrect.a, feedback_Incorrect.b))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('p-4')
					]),
				_List_fromArray(
					[data.button]))
			]));
};
var $rtfeldman$elm_css$Html$Styled$h3 = $rtfeldman$elm_css$Html$Styled$node('h3');
var $author$project$View$introToMain = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col w-full items-center justify-center')
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h3,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Now you understand the activity, let\'s try our target words.')
					])),
				$author$project$View$button(
				{isDisabled: false, message: msg, txt: 'Start'})
			]));
};
var $author$project$Session1$ContextUnderstanding$paragraphWithInput = F3(
	function (pre, userAnswer, post) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$p,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-3xl text-lg p-4')
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text(pre),
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('border-4 w-24 h-2 pl-4 pr-4 font-bold')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(userAnswer)
						])),
					$rtfeldman$elm_css$Html$Styled$text(post)
				]));
	});
var $rtfeldman$elm_css$Css$PercentageUnits = {$: 'PercentageUnits'};
var $rtfeldman$elm_css$Css$pct = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PercentageUnits, '%');
var $author$project$View$pct = F2(
	function (trialn, trials) {
		return (trialn / $elm$core$List$length(trials)) * 100;
	});
var $rtfeldman$elm_css$Css$width = $rtfeldman$elm_css$Css$prop1('width');
var $author$project$Progressbar$progressBar = F2(
	function (history, remainingTrials) {
		var trialNumber = $elm$core$List$length(history) + 1;
		var pct_ = A2(
			$author$project$View$pct,
			trialNumber,
			_Utils_ap(
				remainingTrials,
				A2($elm$core$List$map, $elm$core$Tuple$first, history)));
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('shadow max-w-xl w-full bg-gray-300 mt-2 mb-12 transition duration-500')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('bg-indigo-600 text-sm font-bold w-full leading-none py-1 text-center text-white '),
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$width(
									$rtfeldman$elm_css$Css$pct(pct_))
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$elm$core$String$fromInt(
								$elm$core$Basics$round(pct_)) + '%')
						]))
				]));
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$name = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('name');
var $author$project$View$radio = F5(
	function (value, isChecked, isCorrect, feedbackMode, msg) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$label,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('group block text-gray-70 font-medium '),
					$rtfeldman$elm_css$Html$Styled$Attributes$id(value)
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class(
							'border-solid border-2 px-4 py-4 mb-1 ' + function () {
								var _v0 = _Utils_Tuple3(feedbackMode, isChecked, isCorrect);
								if (!_v0.a) {
									if (_v0.b) {
										return 'border-indigo-600';
									} else {
										return 'border-grey-500';
									}
								} else {
									if (_v0.b) {
										if (!_v0.c) {
											return 'border-red-500';
										} else {
											return 'border-green-500';
										}
									} else {
										return 'border-grey-500';
									}
								}
							}()),
							(!feedbackMode) ? $rtfeldman$elm_css$Html$Styled$Attributes$class('group-hover:border-indigo-600 hover:underline cursor-pointer') : $rtfeldman$elm_css$Html$Styled$Attributes$class(''),
							$rtfeldman$elm_css$Html$Styled$Attributes$class('active:border-indigo-400')
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$input,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$type_('radio'),
									$rtfeldman$elm_css$Html$Styled$Attributes$checked(isChecked),
									$rtfeldman$elm_css$Html$Styled$Attributes$name('definition-choice'),
									$rtfeldman$elm_css$Html$Styled$Attributes$class('form-radio text-indigo-500'),
									$rtfeldman$elm_css$Html$Styled$Events$onClick(msg),
									$rtfeldman$elm_css$Html$Styled$Attributes$disabled(feedbackMode)
								]),
							_List_Nil),
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-4 ')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(value)
								]))
						]))
				]));
	});
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$View$shuffledOptions = F5(
	function (state, fb, radioMsg, trial, optionsOrder) {
		var isCorrect = function (optionN) {
			return _Utils_eq(optionN, trial.target);
		};
		var option = function (id) {
			return A5(
				$author$project$View$radio,
				id,
				_Utils_eq(state.userAnswer, id),
				isCorrect(id),
				fb,
				radioMsg(id));
		};
		var options = _List_fromArray(
			[
				option(trial.distractor1),
				option(trial.distractor2),
				option(trial.distractor3),
				option(trial.target)
			]);
		var ordoredOptions = A2(
			$elm$core$List$map,
			$elm$core$Tuple$second,
			A2(
				$elm$core$List$sortBy,
				$elm$core$Tuple$first,
				A3($elm$core$List$map2, $elm$core$Tuple$pair, optionsOrder, options)));
		return ordoredOptions;
	});
var $rtfeldman$elm_css$Css$absolute = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'absolute'};
var $rtfeldman$elm_css$Css$Structure$PseudoElement = function (a) {
	return {$: 'PseudoElement', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement = F2(
	function (a, b) {
		return {$: 'WithPseudoElement', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$pseudoElement = function (element) {
	return $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement(
		$rtfeldman$elm_css$Css$Structure$PseudoElement(element));
};
var $rtfeldman$elm_css$Css$after = $rtfeldman$elm_css$Css$pseudoElement('after');
var $rtfeldman$elm_css$Css$backgroundColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'background-color', c.value);
};
var $rtfeldman$elm_css$Css$prop3 = F4(
	function (key, argA, argB, argC) {
		return A2(
			$rtfeldman$elm_css$Css$property,
			key,
			A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					[argA.value, argB.value, argC.value])));
	});
var $rtfeldman$elm_css$Css$border3 = $rtfeldman$elm_css$Css$prop3('border');
var $rtfeldman$elm_css$Css$borderTopColor = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'border-top-color', c.value);
};
var $rtfeldman$elm_css$Css$bottom = $rtfeldman$elm_css$Css$prop1('bottom');
var $rtfeldman$elm_css$Css$color = function (c) {
	return A2($rtfeldman$elm_css$Css$property, 'color', c.value);
};
var $rtfeldman$elm_css$Css$Structure$Descendant = {$: 'Descendant'};
var $rtfeldman$elm_css$Css$Preprocess$NestSnippet = F2(
	function (a, b) {
		return {$: 'NestSnippet', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Global$descendants = $rtfeldman$elm_css$Css$Preprocess$NestSnippet($rtfeldman$elm_css$Css$Structure$Descendant);
var $author$project$Icons$helpCircle = A2(
	$author$project$Icons$svgFeatherIcon,
	'help-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$path,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$d('M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$line,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x1('12'),
					$elm$svg$Svg$Attributes$y1('17'),
					$elm$svg$Svg$Attributes$x2('12.01'),
					$elm$svg$Svg$Attributes$y2('17')
				]),
			_List_Nil)
		]));
var $rtfeldman$elm_css$Css$withPrecedingHash = function (str) {
	return A2($elm$core$String$startsWith, '#', str) ? str : A2(
		$elm$core$String$cons,
		_Utils_chr('#'),
		str);
};
var $rtfeldman$elm_css$Css$erroneousHex = function (str) {
	return {
		alpha: 1,
		blue: 0,
		color: $rtfeldman$elm_css$Css$Structure$Compatible,
		green: 0,
		red: 0,
		value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
	};
};
var $elm$core$Basics$pow = _Basics_pow;
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char.valueOf()) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $elm$core$String$toLower = _String_toLower;
var $rtfeldman$elm_css$Css$validHex = F5(
	function (str, _v0, _v1, _v2, _v3) {
		var r1 = _v0.a;
		var r2 = _v0.b;
		var g1 = _v1.a;
		var g2 = _v1.b;
		var b1 = _v2.a;
		var b2 = _v2.b;
		var a1 = _v3.a;
		var a2 = _v3.b;
		var toResult = A2(
			$elm$core$Basics$composeR,
			$elm$core$String$fromList,
			A2($elm$core$Basics$composeR, $elm$core$String$toLower, $rtfeldman$elm_hex$Hex$fromString));
		var results = _Utils_Tuple2(
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[r1, r2])),
				toResult(
					_List_fromArray(
						[g1, g2]))),
			_Utils_Tuple2(
				toResult(
					_List_fromArray(
						[b1, b2])),
				toResult(
					_List_fromArray(
						[a1, a2]))));
		if ((((results.a.a.$ === 'Ok') && (results.a.b.$ === 'Ok')) && (results.b.a.$ === 'Ok')) && (results.b.b.$ === 'Ok')) {
			var _v5 = results.a;
			var red = _v5.a.a;
			var green = _v5.b.a;
			var _v6 = results.b;
			var blue = _v6.a.a;
			var alpha = _v6.b.a;
			return {
				alpha: alpha / 255,
				blue: blue,
				color: $rtfeldman$elm_css$Css$Structure$Compatible,
				green: green,
				red: red,
				value: $rtfeldman$elm_css$Css$withPrecedingHash(str)
			};
		} else {
			return $rtfeldman$elm_css$Css$erroneousHex(str);
		}
	});
var $rtfeldman$elm_css$Css$hex = function (str) {
	var withoutHash = A2($elm$core$String$startsWith, '#', str) ? A2($elm$core$String$dropLeft, 1, str) : str;
	var _v0 = $elm$core$String$toList(withoutHash);
	_v0$4:
	while (true) {
		if ((_v0.b && _v0.b.b) && _v0.b.b.b) {
			if (!_v0.b.b.b.b) {
				var r = _v0.a;
				var _v1 = _v0.b;
				var g = _v1.a;
				var _v2 = _v1.b;
				var b = _v2.a;
				return A5(
					$rtfeldman$elm_css$Css$validHex,
					str,
					_Utils_Tuple2(r, r),
					_Utils_Tuple2(g, g),
					_Utils_Tuple2(b, b),
					_Utils_Tuple2(
						_Utils_chr('f'),
						_Utils_chr('f')));
			} else {
				if (!_v0.b.b.b.b.b) {
					var r = _v0.a;
					var _v3 = _v0.b;
					var g = _v3.a;
					var _v4 = _v3.b;
					var b = _v4.a;
					var _v5 = _v4.b;
					var a = _v5.a;
					return A5(
						$rtfeldman$elm_css$Css$validHex,
						str,
						_Utils_Tuple2(r, r),
						_Utils_Tuple2(g, g),
						_Utils_Tuple2(b, b),
						_Utils_Tuple2(a, a));
				} else {
					if (_v0.b.b.b.b.b.b) {
						if (!_v0.b.b.b.b.b.b.b) {
							var r1 = _v0.a;
							var _v6 = _v0.b;
							var r2 = _v6.a;
							var _v7 = _v6.b;
							var g1 = _v7.a;
							var _v8 = _v7.b;
							var g2 = _v8.a;
							var _v9 = _v8.b;
							var b1 = _v9.a;
							var _v10 = _v9.b;
							var b2 = _v10.a;
							return A5(
								$rtfeldman$elm_css$Css$validHex,
								str,
								_Utils_Tuple2(r1, r2),
								_Utils_Tuple2(g1, g2),
								_Utils_Tuple2(b1, b2),
								_Utils_Tuple2(
									_Utils_chr('f'),
									_Utils_chr('f')));
						} else {
							if (_v0.b.b.b.b.b.b.b.b && (!_v0.b.b.b.b.b.b.b.b.b)) {
								var r1 = _v0.a;
								var _v11 = _v0.b;
								var r2 = _v11.a;
								var _v12 = _v11.b;
								var g1 = _v12.a;
								var _v13 = _v12.b;
								var g2 = _v13.a;
								var _v14 = _v13.b;
								var b1 = _v14.a;
								var _v15 = _v14.b;
								var b2 = _v15.a;
								var _v16 = _v15.b;
								var a1 = _v16.a;
								var _v17 = _v16.b;
								var a2 = _v17.a;
								return A5(
									$rtfeldman$elm_css$Css$validHex,
									str,
									_Utils_Tuple2(r1, r2),
									_Utils_Tuple2(g1, g2),
									_Utils_Tuple2(b1, b2),
									_Utils_Tuple2(a1, a2));
							} else {
								break _v0$4;
							}
						}
					} else {
						break _v0$4;
					}
				}
			}
		} else {
			break _v0$4;
		}
	}
	return $rtfeldman$elm_css$Css$erroneousHex(str);
};
var $rtfeldman$elm_css$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 'ExtendSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 'PseudoClassSelector', a: a};
};
var $rtfeldman$elm_css$Css$pseudoClass = function (_class) {
	return $rtfeldman$elm_css$Css$Preprocess$ExtendSelector(
		$rtfeldman$elm_css$Css$Structure$PseudoClassSelector(_class));
};
var $rtfeldman$elm_css$Css$hover = $rtfeldman$elm_css$Css$pseudoClass('hover');
var $rtfeldman$elm_css$Css$UnitlessFloat = {$: 'UnitlessFloat'};
var $rtfeldman$elm_css$Css$num = function (val) {
	return {
		lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible,
		lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible,
		number: $rtfeldman$elm_css$Css$Structure$Compatible,
		numberOrInfinite: $rtfeldman$elm_css$Css$Structure$Compatible,
		numericValue: val,
		unitLabel: '',
		units: $rtfeldman$elm_css$Css$UnitlessFloat,
		value: $elm$core$String$fromFloat(val)
	};
};
var $rtfeldman$elm_css$Css$opacity = $rtfeldman$elm_css$Css$prop1('opacity');
var $rtfeldman$elm_css$Css$Transitions$Opacity = {$: 'Opacity'};
var $rtfeldman$elm_css$Css$Transitions$Transition = function (a) {
	return {$: 'Transition', a: a};
};
var $rtfeldman$elm_css$Css$Transitions$durationTransition = F2(
	function (animation, duration) {
		return $rtfeldman$elm_css$Css$Transitions$Transition(
			{animation: animation, delay: $elm$core$Maybe$Nothing, duration: duration, timing: $elm$core$Maybe$Nothing});
	});
var $rtfeldman$elm_css$Css$Transitions$opacity = $rtfeldman$elm_css$Css$Transitions$durationTransition($rtfeldman$elm_css$Css$Transitions$Opacity);
var $rtfeldman$elm_css$Css$pointerEventsAll = A2($rtfeldman$elm_css$Css$property, 'pointer-events', 'all');
var $rtfeldman$elm_css$Css$position = $rtfeldman$elm_css$Css$prop1('position');
var $rtfeldman$elm_css$Css$PxUnits = {$: 'PxUnits'};
var $rtfeldman$elm_css$Css$px = A2($rtfeldman$elm_css$Css$Internal$lengthConverter, $rtfeldman$elm_css$Css$PxUnits, 'px');
var $rtfeldman$elm_css$Css$relative = {position: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'relative'};
var $rtfeldman$elm_css$Css$Global$selector = F2(
	function (selectorStr, styles) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
			styles,
			A2($rtfeldman$elm_css$Css$Structure$CustomSelector, selectorStr, _List_Nil));
	});
var $rtfeldman$elm_css$Css$solid = {borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationStyle: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'solid'};
var $rtfeldman$elm_css$Css$Transitions$propToString = function (prop) {
	switch (prop.$) {
		case 'Background':
			return 'background';
		case 'BackgroundColor':
			return 'background-color';
		case 'BackgroundPosition':
			return 'background-position';
		case 'BackgroundSize':
			return 'background-size';
		case 'Border':
			return 'border';
		case 'BorderBottom':
			return 'border-bottom';
		case 'BorderBottomColor':
			return 'border-bottom-color';
		case 'BorderBottomLeftRadius':
			return 'border-bottom-left-radius';
		case 'BorderBottomRightRadius':
			return 'border-bottom-right-radius';
		case 'BorderBottomWidth':
			return 'border-bottom-width';
		case 'BorderColor':
			return 'border-color';
		case 'BorderLeft':
			return 'border-left';
		case 'BorderLeftColor':
			return 'border-left-color';
		case 'BorderLeftWidth':
			return 'border-left-width';
		case 'BorderRadius':
			return 'border-radius';
		case 'BorderRight':
			return 'border-right';
		case 'BorderRightColor':
			return 'border-right-color';
		case 'BorderRightWidth':
			return 'border-right-width';
		case 'BorderTop':
			return 'border-top';
		case 'BorderTopColor':
			return 'border-top-color';
		case 'BorderTopLeftRadius':
			return 'border-top-left-radius';
		case 'BorderTopRightRadius':
			return 'border-top-right-radius';
		case 'BorderTopWidth':
			return 'border-top-width';
		case 'BorderWidth':
			return 'border-width';
		case 'Bottom':
			return 'bottom';
		case 'BoxShadow':
			return 'box-shadow';
		case 'CaretColor':
			return 'caret-color';
		case 'Clip':
			return 'clip';
		case 'ClipPath':
			return 'clip-path';
		case 'Color':
			return 'color';
		case 'ColumnCount':
			return 'column-count';
		case 'ColumnGap':
			return 'column-gap';
		case 'ColumnRule':
			return 'column-rule';
		case 'ColumnRuleColor':
			return 'column-rule-color';
		case 'ColumnRuleWidth':
			return 'column-rule-width';
		case 'ColumnWidth':
			return 'column-width';
		case 'Columns':
			return 'columns';
		case 'Filter':
			return 'filter';
		case 'Flex':
			return 'flex';
		case 'FlexBasis':
			return 'flex-basis';
		case 'FlexGrow':
			return 'flex-grow';
		case 'FlexShrink':
			return 'flex-shrink';
		case 'Font':
			return 'font';
		case 'FontSize':
			return 'font-size';
		case 'FontSizeAdjust':
			return 'font-size-adjust';
		case 'FontStretch':
			return 'font-stretch';
		case 'FontVariationSettings':
			return 'font-variation-settings';
		case 'FontWeight':
			return 'font-weight';
		case 'GridColumnGap':
			return 'grid-column-gap';
		case 'GridGap':
			return 'grid-gap';
		case 'GridRowGap':
			return 'grid-row-gap';
		case 'Height':
			return 'height';
		case 'Left':
			return 'left';
		case 'LetterSpacing':
			return 'letter-spacing';
		case 'LineHeight':
			return 'line-height';
		case 'Margin':
			return 'margin';
		case 'MarginBottom':
			return 'margin-bottom';
		case 'MarginLeft':
			return 'margin-left';
		case 'MarginRight':
			return 'margin-right';
		case 'MarginTop':
			return 'margin-top';
		case 'Mask':
			return 'mask';
		case 'MaskPosition':
			return 'mask-position';
		case 'MaskSize':
			return 'mask-size';
		case 'MaxHeight':
			return 'max-height';
		case 'MaxWidth':
			return 'max-width';
		case 'MinHeight':
			return 'min-height';
		case 'MinWidth':
			return 'min-width';
		case 'ObjectPosition':
			return 'object-position';
		case 'Offset':
			return 'offset';
		case 'OffsetAnchor':
			return 'offset-anchor';
		case 'OffsetDistance':
			return 'offset-distance';
		case 'OffsetPath':
			return 'offset-path';
		case 'OffsetRotate':
			return 'offset-rotate';
		case 'Opacity':
			return 'opacity';
		case 'Order':
			return 'order';
		case 'Outline':
			return 'outline';
		case 'OutlineColor':
			return 'outline-color';
		case 'OutlineOffset':
			return 'outline-offset';
		case 'OutlineWidth':
			return 'outline-width';
		case 'Padding':
			return 'padding';
		case 'PaddingBottom':
			return 'padding-bottom';
		case 'PaddingLeft':
			return 'padding-left';
		case 'PaddingRight':
			return 'padding-right';
		case 'PaddingTop':
			return 'padding-top';
		case 'Right':
			return 'right';
		case 'TabSize':
			return 'tab-size';
		case 'TextIndent':
			return 'text-indent';
		case 'TextShadow':
			return 'text-shadow';
		case 'Top':
			return 'top';
		case 'Transform':
			return 'transform';
		case 'TransformOrigin':
			return 'transform-origin';
		case 'VerticalAlign':
			return 'vertical-align';
		case 'Visibility':
			return 'visibility';
		case 'Width':
			return 'width';
		case 'WordSpacing':
			return 'word-spacing';
		default:
			return 'z-index';
	}
};
var $rtfeldman$elm_css$Css$Transitions$timeToString = function (time) {
	return $elm$core$String$fromFloat(time) + 'ms';
};
var $rtfeldman$elm_css$Css$Transitions$timingFunctionToString = function (tf) {
	switch (tf.$) {
		case 'Ease':
			return 'ease';
		case 'Linear':
			return 'linear';
		case 'EaseIn':
			return 'ease-in';
		case 'EaseOut':
			return 'ease-out';
		case 'EaseInOut':
			return 'ease-in-out';
		case 'StepStart':
			return 'step-start';
		case 'StepEnd':
			return 'step-end';
		default:
			var _float = tf.a;
			var float2 = tf.b;
			var float3 = tf.c;
			var float4 = tf.d;
			return 'cubic-bezier(' + ($elm$core$String$fromFloat(_float) + (' , ' + ($elm$core$String$fromFloat(float2) + (' , ' + ($elm$core$String$fromFloat(float3) + (' , ' + ($elm$core$String$fromFloat(float4) + ')')))))));
	}
};
var $rtfeldman$elm_css$Css$Transitions$transition = function (options) {
	var v = A3(
		$elm$core$String$slice,
		0,
		-1,
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, s) {
					var animation = _v0.a.animation;
					var duration = _v0.a.duration;
					var delay = _v0.a.delay;
					var timing = _v0.a.timing;
					return s + (A2(
						$elm$core$String$join,
						' ',
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Transitions$propToString(animation),
								$rtfeldman$elm_css$Css$Transitions$timeToString(duration),
								A2(
								$elm$core$Maybe$withDefault,
								'',
								A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Transitions$timeToString, delay)),
								A2(
								$elm$core$Maybe$withDefault,
								'',
								A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Transitions$timingFunctionToString, timing))
							])) + ',');
				}),
			'',
			options));
	return A2($rtfeldman$elm_css$Css$property, 'transition', v);
};
var $rtfeldman$elm_css$Css$transparent = {color: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'transparent'};
var $rtfeldman$elm_css$Css$UnitlessInteger = {$: 'UnitlessInteger'};
var $rtfeldman$elm_css$Css$zero = {length: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAuto: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrAutoOrCoverOrContain: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumber: $rtfeldman$elm_css$Css$Structure$Compatible, number: $rtfeldman$elm_css$Css$Structure$Compatible, numericValue: 0, outline: $rtfeldman$elm_css$Css$Structure$Compatible, unitLabel: '', units: $rtfeldman$elm_css$Css$UnitlessInteger, value: '0'};
var $author$project$View$tooltip = function (text_) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
						$rtfeldman$elm_css$Css$hover(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Global$descendants(
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Css$Global$selector,
										'.tooltip__content',
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$opacity(
												$rtfeldman$elm_css$Css$num(1)),
												$rtfeldman$elm_css$Css$pointerEventsAll
											]))
									]))
							]))
					])),
				$rtfeldman$elm_css$Html$Styled$Attributes$class('h-6 w-6')
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('tooltip__content'),
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$opacity($rtfeldman$elm_css$Css$zero),
								$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
								$rtfeldman$elm_css$Css$backgroundColor(
								$rtfeldman$elm_css$Css$hex('fffff')),
								$rtfeldman$elm_css$Css$width(
								$rtfeldman$elm_css$Css$px(200)),
								$rtfeldman$elm_css$Css$color(
								$rtfeldman$elm_css$Css$hex('333333')),
								$rtfeldman$elm_css$Css$bottom(
								$rtfeldman$elm_css$Css$pct(100)),
								$rtfeldman$elm_css$Css$Transitions$transition(
								_List_fromArray(
									[
										$rtfeldman$elm_css$Css$Transitions$opacity(200)
									])),
								$rtfeldman$elm_css$Css$after(
								_List_fromArray(
									[
										A2($rtfeldman$elm_css$Css$property, 'content', ' '),
										$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
										A3(
										$rtfeldman$elm_css$Css$border3,
										$rtfeldman$elm_css$Css$px(8),
										$rtfeldman$elm_css$Css$solid,
										$rtfeldman$elm_css$Css$transparent),
										$rtfeldman$elm_css$Css$borderTopColor(
										$rtfeldman$elm_css$Css$hex('333333')),
										$rtfeldman$elm_css$Css$bottom($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$height($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$width($rtfeldman$elm_css$Css$zero),
										$rtfeldman$elm_css$Css$Transitions$transition(
										_List_fromArray(
											[
												$rtfeldman$elm_css$Css$Transitions$opacity(200)
											]))
									]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2 bg-white')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(text_)
							]))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('tooltip__trigger opacity-75')
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$helpCircle)
							]))
					]))
			]));
};
var $author$project$View$trainingWheelsGeneric = F3(
	function (trialn, pattern_, variables) {
		var helpSentence = A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col pt-4 pb-4 italic text-xl max-w-xl')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_Nil,
					_List_fromArray(
						[
							$author$project$View$fromMarkdown(
							A2($lukewestby$elm_string_interpolate$String$Interpolate$interpolate, pattern_, variables))
						]))
				]));
		if (!trialn) {
			return helpSentence;
		} else {
			return A2($rtfeldman$elm_css$Html$Styled$div, _List_Nil, _List_Nil);
		}
	});
var $author$project$Session1$ContextUnderstanding$view = function (task) {
	var _v0 = task.task;
	switch (_v0.$) {
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('experiment did not start yet')
					]));
		case 'Err':
			var reason = _v0.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(reason)
					]));
		case 'Loading':
			return $rtfeldman$elm_css$Html$Styled$text('Loading...');
		default:
			switch (_v0.a.$) {
				case 'Training':
					var _v1 = _v0.a;
					var data = _v0.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						var trial = _v2.a;
						var _v3 = function () {
							var _v4 = A2($elm$core$String$split, '/', trial.text);
							if (_v4.b) {
								if (_v4.b.b) {
									var x = _v4.a;
									var _v5 = _v4.b;
									var y = _v5.a;
									return _Utils_Tuple2(x, y);
								} else {
									var x = _v4.a;
									return _Utils_Tuple2(x, 'defaultPost');
								}
							} else {
								return _Utils_Tuple2('defautpre', 'defaultpOst');
							}
						}();
						var pre = _v3.a;
						var post = _v3.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$author$project$View$viewTraining,
									data.infos.instructions,
									_List_fromArray(
										[
											A3(
											$author$project$View$trainingWheelsGeneric,
											$elm$core$List$length(data.history),
											data.infos.trainingWheel,
											_List_fromArray(
												[
													$author$project$View$bold(trial.target)
												])),
											A3($author$project$Session1$ContextUnderstanding$paragraphWithInput, pre, data.state.userAnswer, post),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('w-full max-w-2xl')
												]),
											A5($author$project$View$shuffledOptions, data.state, data.feedback, $author$project$Session1$ContextUnderstanding$UserClickedRadioButton, trial, task.optionsOrder)),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('col-start-2 col-span-4')
												]),
											_List_fromArray(
												[
													$author$project$View$genericSingleChoiceFeedback(
													{
														button: A3($author$project$View$navigationButton, $author$project$Session1$ContextUnderstanding$UserClickedToggleFeedback, $author$project$Session1$ContextUnderstanding$UserClickedNextTrial, data.feedback),
														feedback_Correct: _Utils_Tuple2(
															data.infos.feedback_correct,
															_List_fromArray(
																[
																	$author$project$View$bold(trial.target),
																	$author$project$View$bold(trial.definition)
																])),
														feedback_Incorrect: _Utils_Tuple2(
															data.infos.feedback_incorrect,
															_List_fromArray(
																[
																	$author$project$View$bold(trial.target),
																	$author$project$View$bold(trial.definition)
																])),
														isVisible: data.feedback,
														target: trial.target,
														userAnswer: data.state.userAnswer
													})
												]))
										]))
								]));
					} else {
						return $author$project$View$introToMain(
							A2($author$project$Session1$ContextUnderstanding$UserClickedStartMain, data.mainTrials, data.infos));
					}
				case 'Main':
					var _v6 = _v0.a;
					var data = _v0.b;
					var _v7 = data.current;
					if (_v7.$ === 'Just') {
						var trial = _v7.a;
						var _v8 = function () {
							var _v9 = A2($elm$core$String$split, '/', trial.text);
							if (_v9.b) {
								if (_v9.b.b) {
									var x = _v9.a;
									var _v10 = _v9.b;
									var y = _v10.a;
									return _Utils_Tuple2(x, y);
								} else {
									var x = _v9.a;
									return _Utils_Tuple2(x, 'defaultPost');
								}
							} else {
								return _Utils_Tuple2('defautpre', 'defaultpOst');
							}
						}();
						var pre = _v8.a;
						var post = _v8.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col w-full w-max-3xl items-center justify-center ')
								]),
							_List_fromArray(
								[
									A2($author$project$Progressbar$progressBar, data.history, data.mainTrials),
									$author$project$View$tooltip(data.infos.instructions_short),
									A3($author$project$Session1$ContextUnderstanding$paragraphWithInput, pre, data.state.userAnswer, post),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('w-full max-w-xl')
										]),
									A5($author$project$View$shuffledOptions, data.state, data.feedback, $author$project$Session1$ContextUnderstanding$UserClickedRadioButton, trial, task.optionsOrder)),
									$author$project$View$genericSingleChoiceFeedback(
									{
										button: A3($author$project$View$navigationButton, $author$project$Session1$ContextUnderstanding$UserClickedToggleFeedback, $author$project$Session1$ContextUnderstanding$UserClickedNextTrial, data.feedback),
										feedback_Correct: _Utils_Tuple2(
											data.infos.feedback_correct,
											_List_fromArray(
												[
													$author$project$View$bold(trial.target),
													$author$project$View$bold(trial.definition)
												])),
										feedback_Incorrect: _Utils_Tuple2(
											data.infos.feedback_incorrect,
											_List_fromArray(
												[
													$author$project$View$bold(trial.target),
													$author$project$View$bold(trial.definition)
												])),
										isVisible: data.feedback,
										target: trial.target,
										userAnswer: data.state.userAnswer
									})
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session1$ContextUnderstanding$UserClickedSaveData, './');
					}
				default:
					var _v11 = _v0.a;
					var data = _v0.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$View$instructions, data.infos.instructions, $author$project$Session1$ContextUnderstanding$UserClickedStartTraining)
							]));
			}
	}
};
var $author$project$Session1$Meaning$SaveDataMsg = {$: 'SaveDataMsg'};
var $author$project$Session1$Meaning$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session1$Meaning$UserClickedRadioButton = function (a) {
	return {$: 'UserClickedRadioButton', a: a};
};
var $author$project$Session1$Meaning$UserClickedStartMain = {$: 'UserClickedStartMain'};
var $author$project$Session1$Meaning$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session1$Meaning$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$Session1$Meaning$viewQuestion = F2(
	function (word, trialn) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$h3,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('italic')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(word)
								]))
						]))
				]));
	});
var $author$project$Session1$Meaning$view = function (task) {
	var _v0 = task.task;
	switch (_v0.$) {
		case 'Loading':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading... ')
					]));
		case 'Err':
			var reason = _v0.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(reason)
					]));
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('I did not start yet.')
					]));
		default:
			switch (_v0.a.$) {
				case 'Training':
					var _v1 = _v0.a;
					var data = _v0.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						var trial = _v2.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											A3(
											$author$project$View$trainingWheelsGeneric,
											$elm$core$List$length(data.history),
											data.infos.trainingWheel,
											_List_fromArray(
												[
													$author$project$View$bold(trial.writtenWord),
													$author$project$View$bold(trial.target)
												]))
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$author$project$Session1$Meaning$viewQuestion,
											trial.writtenWord,
											$elm$core$List$length(data.history))
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('pt-6 max-w-xl '),
											$rtfeldman$elm_css$Html$Styled$Attributes$disabled(data.feedback)
										]),
									A5($author$project$View$shuffledOptions, data.state, data.feedback, $author$project$Session1$Meaning$UserClickedRadioButton, trial, task.optionsOrder)),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_Nil,
									_List_fromArray(
										[
											$author$project$View$genericSingleChoiceFeedback(
											{
												button: A3($author$project$View$navigationButton, $author$project$Session1$Meaning$UserClickedToggleFeedback, $author$project$Session1$Meaning$UserClickedNextTrial, data.feedback),
												feedback_Correct: _Utils_Tuple2(trial.feedbackIncorrect, _List_Nil),
												feedback_Incorrect: _Utils_Tuple2(trial.feedbackCorrect, _List_Nil),
												isVisible: data.feedback,
												target: trial.target,
												userAnswer: data.state.userAnswer
											})
										]))
								]));
					} else {
						return $author$project$View$introToMain($author$project$Session1$Meaning$UserClickedStartMain);
					}
				case 'Main':
					var _v3 = _v0.a;
					var data = _v0.b;
					var _v4 = data.current;
					if (_v4.$ === 'Just') {
						var trial = _v4.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col items-center justify-center')
								]),
							_List_fromArray(
								[
									A2($author$project$Progressbar$progressBar, data.history, data.mainTrials),
									$author$project$View$tooltip(
									A2(
										$lukewestby$elm_string_interpolate$String$Interpolate$interpolate,
										data.infos.instructions_short,
										_List_fromArray(
											[trial.writtenWord]))),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('mr-8 w-full max-w-xl')
										]),
									_List_fromArray(
										[
											A2(
											$author$project$Session1$Meaning$viewQuestion,
											trial.writtenWord,
											$elm$core$List$length(data.history)),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('pt-6 center-items justify-center max-w-xl w-full mt-6 '),
													$rtfeldman$elm_css$Html$Styled$Attributes$disabled(data.feedback)
												]),
											A5($author$project$View$shuffledOptions, data.state, data.feedback, $author$project$Session1$Meaning$UserClickedRadioButton, trial, task.optionsOrder)),
											$author$project$View$genericSingleChoiceFeedback(
											{
												button: A3($author$project$View$navigationButton, $author$project$Session1$Meaning$UserClickedToggleFeedback, $author$project$Session1$Meaning$UserClickedNextTrial, data.feedback),
												feedback_Correct: _Utils_Tuple2(trial.feedbackIncorrect, _List_Nil),
												feedback_Incorrect: _Utils_Tuple2(trial.feedbackCorrect, _List_Nil),
												isVisible: data.feedback,
												target: trial.target,
												userAnswer: data.state.userAnswer
											})
										]))
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session1$Meaning$SaveDataMsg, 'spelling');
					}
				default:
					var _v5 = _v0.a;
					var data = _v0.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$h1,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Instructions')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$p,
								_List_Nil,
								_List_fromArray(
									[
										$author$project$View$fromMarkdown(data.infos.instructions)
									])),
								$author$project$View$button(
								{isDisabled: false, message: $author$project$Session1$Meaning$UserClickedStartTraining, txt: 'Start training'})
							]));
			}
	}
};
var $author$project$Session1$Presentation$NoOp = {$: 'NoOp'};
var $author$project$Session1$Presentation$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session1$Presentation$UserClickedStartAudio = function (a) {
	return {$: 'UserClickedStartAudio', a: a};
};
var $author$project$Session1$Presentation$UserClickedStartMain = F2(
	function (a, b) {
		return {$: 'UserClickedStartMain', a: a, b: b};
	});
var $author$project$Session1$Presentation$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session1$Presentation$UserToggleElementOfEntry = function (a) {
	return {$: 'UserToggleElementOfEntry', a: a};
};
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $author$project$Icons$music = A2(
	$author$project$Icons$svgFeatherIcon,
	'play-circle',
	_List_fromArray(
		[
			A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx('12'),
					$elm$svg$Svg$Attributes$cy('12'),
					$elm$svg$Svg$Attributes$r('10')
				]),
			_List_Nil),
			A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$points('10 8 16 12 10 16 10 8')
				]),
			_List_Nil)
		]));
var $author$project$View$audioButton = F3(
	function (msg, url, audioName) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('px-4 py-4 mt-4 mb-4 hover:opacity-75 cursor-pointer border-2 flex flex-row items-center justify-center bg-green-500 rounded-md shadow-sm'),
					$rtfeldman$elm_css$Html$Styled$Events$onClick(
					msg(url))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('h-6 w-6 text-white')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$music)
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-4 text-lg font-bold text-white')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Listen to the ' + audioName)
						]))
				]));
	});
var $author$project$Session1$Presentation$sep = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$class('w-32 h-1 mt-4 mb-4')
		]),
	_List_Nil);
var $author$project$Session1$Presentation$viewEntry = F3(
	function (key, _v0, toggledEntries) {
		var txt = _v0.txt;
		var elements = _v0.elements;
		return A2(
			$elm$core$Maybe$withDefault,
			false,
			A2($elm$core$Dict$get, key, toggledEntries)) ? A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col')
				]),
			A2(
				$elm$core$List$map,
				function (el) {
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('p-4')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(el)
							]));
				},
				elements)) : A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_Nil,
			A2($elm$core$List$map, $rtfeldman$elm_css$Html$Styled$text, _List_Nil));
	});
var $author$project$Session1$Presentation$entries = F5(
	function (d, e, t, msg, toggledEntries) {
		var arrow = function (key) {
			return A2(
				$elm$core$Maybe$withDefault,
				false,
				A2($elm$core$Dict$get, key, toggledEntries)) ? A2(
				$rtfeldman$elm_css$Html$Styled$span,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg font-bold')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('⌄')
					])) : A2(
				$rtfeldman$elm_css$Html$Styled$span,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg font-bold')
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('›')
					]));
		};
		return A2(
			$elm$core$List$intersperse,
			$author$project$Session1$Presentation$sep,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var key = _v0.a;
					var val = _v0.b;
					var txt = val.txt;
					return A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col'),
								$rtfeldman$elm_css$Html$Styled$Events$onClick(
								msg(key))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg hover:underline cursor-pointer')
									]),
								_List_fromArray(
									[
										arrow(key),
										A2(
										$rtfeldman$elm_css$Html$Styled$span,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-2')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(txt)
											]))
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$span,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('p-2')
									]),
								_List_fromArray(
									[
										A3($author$project$Session1$Presentation$viewEntry, key, val, toggledEntries)
									]))
							]));
				},
				_List_fromArray(
					[
						_Utils_Tuple2(
						'definition',
						{elements: d, txt: 'Definition: '}),
						_Utils_Tuple2(
						'example',
						{elements: e, txt: 'Example: '}),
						_Utils_Tuple2(
						'translation',
						{
							elements: A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq('missing'),
								t),
							txt: 'Translation: '
						})
					])));
	});
var $author$project$Session1$Presentation$view = function (task) {
	switch (task.$) {
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('experiment did not start yet')
					]));
		case 'Err':
			var reason = task.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(reason)
					]));
		case 'Loading':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading...')
					]));
		default:
			switch (task.a.$) {
				case 'Instructions':
					var _v1 = task.a;
					var data = task.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$View$instructions, data.infos.instructions, $author$project$Session1$Presentation$UserClickedStartTraining)
							]));
				case 'Training':
					var _v2 = task.a;
					var data = task.b;
					var _v3 = data.current;
					if (_v3.$ === 'Just') {
						var trial = _v3.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('pb-4 pt-4 text-3xl font-bold flex flex-row')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(trial.text)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_Nil,
									_List_fromArray(
										[
											A3($author$project$View$audioButton, $author$project$Session1$Presentation$UserClickedStartAudio, trial.audio.url, 'Pronunciation')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('w-56 pt-8')
										]),
									A5(
										$author$project$Session1$Presentation$entries,
										_List_fromArray(
											[trial.definition]),
										_List_fromArray(
											[trial.example]),
										_List_fromArray(
											[trial.translation1, trial.translation2]),
										$author$project$Session1$Presentation$UserToggleElementOfEntry,
										data.state.toggledEntries)),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('pb-8')
										]),
									_List_fromArray(
										[
											$author$project$View$button(
											{isDisabled: false, message: $author$project$Session1$Presentation$UserClickedNextTrial, txt: 'Next Item'})
										]))
								]));
					} else {
						return $author$project$View$introToMain(
							A2($author$project$Session1$Presentation$UserClickedStartMain, data.mainTrials, data.infos));
					}
				default:
					var _v4 = task.a;
					var data = task.b;
					var _v5 = data.current;
					if (_v5.$ === 'Just') {
						var trial = _v5.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A2($author$project$Progressbar$progressBar, data.history, data.mainTrials),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('pb-4 text-3xl font-bold flex flex-row')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(trial.text)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('w-1/3')
										]),
									A2(
										$elm$core$List$cons,
										A3($author$project$View$audioButton, $author$project$Session1$Presentation$UserClickedStartAudio, trial.audio.url, 'Pronunciation'),
										A5(
											$author$project$Session1$Presentation$entries,
											_List_fromArray(
												[trial.definition]),
											_List_fromArray(
												[trial.example]),
											_List_fromArray(
												[trial.translation1, trial.translation2]),
											$author$project$Session1$Presentation$UserToggleElementOfEntry,
											data.state.toggledEntries))),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('')
										]),
									_List_fromArray(
										[
											$author$project$View$button(
											{isDisabled: false, message: $author$project$Session1$Presentation$UserClickedNextTrial, txt: 'Next Item'})
										]))
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session1$Presentation$NoOp, 'meaning');
					}
			}
	}
};
var $author$project$Session1$Spelling$UserClickedRadioButton = function (a) {
	return {$: 'UserClickedRadioButton', a: a};
};
var $author$project$Session1$Spelling$UserClickedSavedData = {$: 'UserClickedSavedData'};
var $author$project$Session1$Spelling$UserClickedStartMainloop = {$: 'UserClickedStartMainloop'};
var $author$project$Session1$Spelling$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session1$Spelling$UserClickedFeedback = {$: 'UserClickedFeedback'};
var $author$project$Session1$Spelling$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session1$Spelling$UserClickedPlayAudio = function (a) {
	return {$: 'UserClickedPlayAudio', a: a};
};
var $author$project$Session1$Spelling$viewTask = F3(
	function (data, currentTrial, ordoredOptions) {
		return _List_fromArray(
			[
				A3($author$project$View$audioButton, $author$project$Session1$Spelling$UserClickedPlayAudio, currentTrial.audio.url, 'word'),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('pt-6 center-items justify-center max-w-xl w-full mt-6 '),
						$rtfeldman$elm_css$Html$Styled$Attributes$disabled(data.feedback)
					]),
				_List_fromArray(
					[
						A2($rtfeldman$elm_css$Html$Styled$fieldset, _List_Nil, ordoredOptions),
						$author$project$View$genericSingleChoiceFeedback(
						{
							button: A3($author$project$View$navigationButton, $author$project$Session1$Spelling$UserClickedFeedback, $author$project$Session1$Spelling$UserClickedNextTrial, data.feedback),
							feedback_Correct: _Utils_Tuple2(
								data.infos.feedback_correct,
								_List_fromArray(
									[
										$author$project$View$bold(currentTrial.target)
									])),
							feedback_Incorrect: _Utils_Tuple2(
								data.infos.feedback_incorrect,
								_List_fromArray(
									[
										$author$project$View$bold(currentTrial.target)
									])),
							isVisible: data.feedback,
							target: currentTrial.target,
							userAnswer: data.state.userAnswer
						})
					]))
			]);
	});
var $author$project$Session1$Spelling$view = F2(
	function (exp, optionsOrder) {
		switch (exp.$) {
			case 'Loading':
				return $rtfeldman$elm_css$Html$Styled$text('Loading...');
			case 'NotStarted':
				return $rtfeldman$elm_css$Html$Styled$text('I\'m not started yet');
			case 'Err':
				var reason = exp.a;
				return $rtfeldman$elm_css$Html$Styled$text('Error: ' + reason);
			default:
				switch (exp.a.$) {
					case 'Instructions':
						var _v1 = exp.a;
						var data = exp.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A2($author$project$View$instructions, data.infos.instructions, $author$project$Session1$Spelling$UserClickedStartTraining)
								]));
					case 'Training':
						var _v2 = exp.a;
						var data = exp.b;
						var trainingTrials = data.trainingTrials;
						var mainTrials = data.mainTrials;
						var current = data.current;
						var state = data.state;
						var feedback = data.feedback;
						var history = data.history;
						var infos = data.infos;
						if (current.$ === 'Just') {
							var x = current.a;
							var isCorrect = function (optionN) {
								return _Utils_eq(optionN, x.target);
							};
							var option = function (id) {
								return A5(
									$author$project$View$radio,
									id,
									_Utils_eq(state.userAnswer, id),
									isCorrect(id),
									feedback,
									$author$project$Session1$Spelling$UserClickedRadioButton(id));
							};
							var options = _List_fromArray(
								[
									option(x.distractor1),
									option(x.distractor2),
									option(x.distractor3),
									option(x.target)
								]);
							var ordoredOptions = A2(
								$elm$core$List$map,
								$elm$core$Tuple$second,
								A2(
									$elm$core$List$sortBy,
									$elm$core$Tuple$first,
									A3($elm$core$List$map2, $elm$core$Tuple$pair, optionsOrder, options)));
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								A2(
									$elm$core$List$cons,
									A3(
										$author$project$View$trainingWheelsGeneric,
										$elm$core$List$length(history),
										data.infos.trainingWheel,
										_List_fromArray(
											[
												$author$project$View$bold(x.target)
											])),
									A3($author$project$Session1$Spelling$viewTask, data, x, ordoredOptions)));
						} else {
							return $author$project$View$introToMain($author$project$Session1$Spelling$UserClickedStartMainloop);
						}
					default:
						var _v4 = exp.a;
						var data = exp.b;
						var mainTrials = data.mainTrials;
						var current = data.current;
						var state = data.state;
						var feedback = data.feedback;
						var history = data.history;
						var infos = data.infos;
						if (current.$ === 'Just') {
							var trial = current.a;
							var isCorrect = function (optionN) {
								return _Utils_eq(optionN, trial.target);
							};
							var option = function (id) {
								return A5(
									$author$project$View$radio,
									id,
									_Utils_eq(state.userAnswer, id),
									isCorrect(id),
									feedback,
									$author$project$Session1$Spelling$UserClickedRadioButton(id));
							};
							var options = _List_fromArray(
								[
									option(trial.distractor1),
									option(trial.distractor2),
									option(trial.distractor3),
									option(trial.target)
								]);
							var ordoredOptions = A2(
								$elm$core$List$map,
								$elm$core$Tuple$second,
								A2(
									$elm$core$List$sortBy,
									$elm$core$Tuple$first,
									A3($elm$core$List$map2, $elm$core$Tuple$pair, optionsOrder, options)));
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('container w-full flex flex-col justify-center items-center')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('mr-8 w-full max-w-xl')
											]),
										A2(
											$elm$core$List$cons,
											A2($author$project$Progressbar$progressBar, history, mainTrials),
											A3($author$project$Session1$Spelling$viewTask, data, trial, ordoredOptions)))
									]));
						} else {
							return A3($author$project$View$end, infos.end, $author$project$Session1$Spelling$UserClickedSavedData, 'context-understanding');
						}
				}
		}
	});
var $rtfeldman$elm_css$Html$Styled$article = $rtfeldman$elm_css$Html$Styled$node('article');
var $rtfeldman$elm_css$Html$Styled$footer = $rtfeldman$elm_css$Html$Styled$node('footer');
var $rtfeldman$elm_css$Html$Styled$header = $rtfeldman$elm_css$Html$Styled$node('header');
var $author$project$ExperimentInfo$sessionToString = function (str) {
	switch (str.$) {
		case 'Session1':
			return 'Session1';
		case 'Session2':
			return 'Session 2';
		case 'Session3':
			return 'Session 3';
		case 'Pretest':
			return 'Pretest';
		case 'Posttest':
			return 'Post-test';
		default:
			return 'Other';
	}
};
var $author$project$ExperimentInfo$typeToString = function (t) {
	switch (t.$) {
		case 'Sens':
			return 'Sens';
		case 'Forme':
			return 'Forme';
		case 'Context':
			return 'Context';
		default:
			return 'Other';
	}
};
var $author$project$Session1$Top$view = function (infos) {
	var toCard = function (info) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$href(info.url),
					$rtfeldman$elm_css$Html$Styled$Attributes$class(' my-1 px-1 w-full md:w-1/2 lg:my-4 lg:px-4 lg:w-13 transform scale-100 transition duration-150 hover:scale-105 cursor-pointer')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$article,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('overflow-hidden rounded-lg shadow-md hover:shadow-xl')
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$a,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$href(info.url)
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$img,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('block h-auto w-full'),
											$rtfeldman$elm_css$Html$Styled$Attributes$src('https://picsum.photos/600/400/?random')
										]),
									_List_Nil)
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$header,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex items-center justify-between leading-tight p-2 md:p4')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h1,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('text-lg')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(info.name)
										]))
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('px-2 overflow-ellipsis')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(info.description)
								])),
							A2(
							$rtfeldman$elm_css$Html$Styled$footer,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex items-center justify-between leading-none p-2 md:p-4')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('bg-green-500 rounded-lg p-2 text-white')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$author$project$ExperimentInfo$typeToString(info.type_))
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('bg-blue-500 rounded-lg p-2 text-white')
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(
											$author$project$ExperimentInfo$sessionToString(info.session))
										]))
								]))
						]))
				]));
	};
	var viewCard = function (_with) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-wrap -mx-1 px-4 md:px-12')
				]),
			A2(
				$elm$core$List$map,
				toCard,
				A2(
					$elm$core$List$filter,
					_with,
					A2(
						$elm$core$List$map,
						$elm$core$Tuple$second,
						$elm$core$Dict$toList(infos)))));
	};
	return _List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col items-center justify-center w-full max-w-2-xl')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h1,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Lex Learn 👩\u200D🎓️')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-2xl text-xl text-center mb-8')
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n Sapien et ligula ullamcorper malesuada proin libero nunc consequat. Sed sed risus pretium quam vulputate dignissim. Aliquam sem fringilla ut morbi tincidunt augue interdum velit euismod. Ultrices tincidunt arcu non sodales neque.')
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$h2,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Session 1')
						])),
					viewCard(
					function (info) {
						return _Utils_eq(info.session, $author$project$ExperimentInfo$Session1);
					})
				]))
		]);
};
var $author$project$Session2$CU2$UserClickedAudio = function (a) {
	return {$: 'UserClickedAudio', a: a};
};
var $author$project$Session2$CU2$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session2$CU2$UserClickedRadioButton = function (a) {
	return {$: 'UserClickedRadioButton', a: a};
};
var $author$project$Session2$CU2$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session2$CU2$UserClickedStartMain = F2(
	function (a, b) {
		return {$: 'UserClickedStartMain', a: a, b: b};
	});
var $author$project$Session2$CU2$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session2$CU2$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$Session2$CU2$view = F2(
	function (exp, optionsOrder) {
		switch (exp.$) {
			case 'NotStarted':
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('experiment did not start yet')
						]));
			case 'Loading':
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Loading...')
						]));
			case 'Err':
				var reason = exp.a;
				return A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(reason)
						]));
			default:
				switch (exp.a.$) {
					case 'Instructions':
						var _v1 = exp.a;
						var data = exp.b;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2($author$project$View$instructions, data.infos.instructions, $author$project$Session2$CU2$UserClickedStartTraining)
								]));
					case 'Training':
						var _v2 = exp.a;
						var data = exp.b;
						var mainTrials = data.mainTrials;
						var current = data.current;
						var state = data.state;
						var feedback = data.feedback;
						if (current.$ === 'Just') {
							var trial = current.a;
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('p-4')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.context)
											])),
										A3($author$project$View$audioButton, $author$project$Session2$CU2$UserClickedAudio, trial.audioSentence.url, 'dialog'),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_Nil,
										A5($author$project$View$shuffledOptions, state, feedback, $author$project$Session2$CU2$UserClickedRadioButton, trial, optionsOrder)),
										$author$project$View$genericSingleChoiceFeedback(
										{
											button: A3($author$project$View$navigationButton, $author$project$Session2$CU2$UserClickedToggleFeedback, $author$project$Session2$CU2$UserClickedNextTrial, feedback),
											feedback_Correct: _Utils_Tuple2(trial.feedback, _List_Nil),
											feedback_Incorrect: _Utils_Tuple2(trial.feedback, _List_Nil),
											isVisible: feedback,
											target: trial.target,
											userAnswer: state.userAnswer
										})
									]));
						} else {
							return $author$project$View$introToMain(
								A2($author$project$Session2$CU2$UserClickedStartMain, mainTrials, data.infos));
						}
					default:
						var _v4 = exp.a;
						var data = exp.b;
						var mainTrials = data.mainTrials;
						var current = data.current;
						var state = data.state;
						var feedback = data.feedback;
						var history = data.history;
						if (current.$ === 'Just') {
							var trial = current.a;
							return A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col w-full items-center')
									]),
								_List_fromArray(
									[
										$author$project$View$tooltip(data.infos.instructions_short),
										A2($author$project$Progressbar$progressBar, history, mainTrials),
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('p-8 text-lg')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(trial.context)
											])),
										A3($author$project$View$audioButton, $author$project$Session2$CU2$UserClickedAudio, trial.audioSentence.url, 'dialog'),
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-2xl pt-4')
											]),
										A5($author$project$View$shuffledOptions, state, feedback, $author$project$Session2$CU2$UserClickedRadioButton, trial, optionsOrder)),
										$author$project$View$genericSingleChoiceFeedback(
										{
											button: A3($author$project$View$navigationButton, $author$project$Session2$CU2$UserClickedToggleFeedback, $author$project$Session2$CU2$UserClickedNextTrial, feedback),
											feedback_Correct: _Utils_Tuple2(trial.feedback, _List_Nil),
											feedback_Incorrect: _Utils_Tuple2(trial.feedback, _List_Nil),
											isVisible: feedback,
											target: trial.target,
											userAnswer: state.userAnswer
										})
									]));
						} else {
							return A3($author$project$View$end, data.infos.end, $author$project$Session2$CU2$UserClickedSaveData, '/');
						}
				}
		}
	});
var $author$project$Session2$Translation$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session2$Translation$UserClickedStartMain = {$: 'UserClickedStartMain'};
var $author$project$Session2$Translation$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session2$Translation$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session2$Translation$UserClickedRadioButton = function (a) {
	return {$: 'UserClickedRadioButton', a: a};
};
var $author$project$Session2$Translation$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $author$project$Session2$Translation$renderTask = F5(
	function (task, trial, data, history, allTrials) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('text-2xl w-1/2 p-2')
				]),
			_List_fromArray(
				[
					A2($author$project$Progressbar$progressBar, history, allTrials),
					$author$project$View$fromMarkdown(trial.question),
					A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('w-full max-w-1/3 pt-8'),
							$rtfeldman$elm_css$Html$Styled$Attributes$disabled(data.feedback)
						]),
					A5($author$project$View$shuffledOptions, data.state, data.feedback, $author$project$Session2$Translation$UserClickedRadioButton, trial, task.optionsOrder)),
					$author$project$View$genericSingleChoiceFeedback(
					{
						button: A3($author$project$View$navigationButton, $author$project$Session2$Translation$UserClickedToggleFeedback, $author$project$Session2$Translation$UserClickedNextTrial, data.feedback),
						feedback_Correct: _Utils_Tuple2(
							data.infos.feedback_correct,
							_List_fromArray(
								[trial.target])),
						feedback_Incorrect: _Utils_Tuple2(
							data.infos.feedback_incorrect,
							_List_fromArray(
								[trial.target])),
						isVisible: data.feedback,
						target: trial.target,
						userAnswer: data.state.userAnswer
					})
				]));
	});
var $author$project$Session2$Translation$view = function (task) {
	var _v0 = task.task;
	switch (_v0.$) {
		case 'Loading':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading')
					]));
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('The experiment is not started yet')
					]));
		case 'Err':
			var reason = _v0.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(reason)
					]));
		default:
			switch (_v0.a.$) {
				case 'Training':
					var _v1 = _v0.a;
					var data = _v0.b;
					var _v2 = data.current;
					if (_v2.$ === 'Just') {
						var trial = _v2.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$View$trainingWheelsGeneric,
									$elm$core$List$length(data.history),
									data.infos.trainingWheel,
									_List_fromArray(
										[trial.target])),
									A5($author$project$Session2$Translation$renderTask, task, trial, data, data.history, data.trainingTrials)
								]));
					} else {
						return $author$project$View$introToMain($author$project$Session2$Translation$UserClickedStartMain);
					}
				case 'Main':
					var _v3 = _v0.a;
					var data = _v0.b;
					var _v4 = data.current;
					if (_v4.$ === 'Just') {
						var trial = _v4.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A5($author$project$Session2$Translation$renderTask, task, trial, data, data.history, data.mainTrials)
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session2$Translation$UserClickedSaveData, 'spelling');
					}
				default:
					var _v5 = _v0.a;
					var data = _v0.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$View$instructions, data.infos.instructions, $author$project$Session2$Translation$UserClickedStartTraining)
							]));
			}
	}
};
var $author$project$Session3$CU3$UserChangedInput = function (a) {
	return {$: 'UserChangedInput', a: a};
};
var $author$project$Session3$CU3$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session3$CU3$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session3$CU3$UserClickedStartMain = {$: 'UserClickedStartMain'};
var $author$project$Session3$CU3$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session3$CU3$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $rtfeldman$elm_css$Html$Styled$br = $rtfeldman$elm_css$Html$Styled$node('br');
var $rtfeldman$elm_css$Css$borderRadius = $rtfeldman$elm_css$Css$prop1('border-radius');
var $rtfeldman$elm_css$Css$left = $rtfeldman$elm_css$Css$prop1('left');
var $rtfeldman$elm_css$Css$none = {backgroundImage: $rtfeldman$elm_css$Css$Structure$Compatible, blockAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, borderStyle: $rtfeldman$elm_css$Css$Structure$Compatible, cursor: $rtfeldman$elm_css$Css$Structure$Compatible, display: $rtfeldman$elm_css$Css$Structure$Compatible, hoverCapability: $rtfeldman$elm_css$Css$Structure$Compatible, inlineAxisOverflow: $rtfeldman$elm_css$Css$Structure$Compatible, keyframes: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNone: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNoneOrMinMaxDimension: $rtfeldman$elm_css$Css$Structure$Compatible, lengthOrNumberOrAutoOrNoneOrContent: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleType: $rtfeldman$elm_css$Css$Structure$Compatible, listStyleTypeOrPositionOrImage: $rtfeldman$elm_css$Css$Structure$Compatible, none: $rtfeldman$elm_css$Css$Structure$Compatible, outline: $rtfeldman$elm_css$Css$Structure$Compatible, pointerDevice: $rtfeldman$elm_css$Css$Structure$Compatible, pointerEvents: $rtfeldman$elm_css$Css$Structure$Compatible, resize: $rtfeldman$elm_css$Css$Structure$Compatible, scriptingSupport: $rtfeldman$elm_css$Css$Structure$Compatible, textDecorationLine: $rtfeldman$elm_css$Css$Structure$Compatible, textTransform: $rtfeldman$elm_css$Css$Structure$Compatible, touchAction: $rtfeldman$elm_css$Css$Structure$Compatible, transform: $rtfeldman$elm_css$Css$Structure$Compatible, updateFrequency: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'none'};
var $rtfeldman$elm_css$Css$padding = $rtfeldman$elm_css$Css$prop1('padding');
var $rtfeldman$elm_css$Css$pointerEvents = $rtfeldman$elm_css$Css$prop1('pointer-events');
var $rtfeldman$elm_css$Html$Styled$Attributes$readonly = $rtfeldman$elm_css$Html$Styled$Attributes$boolProperty('readOnly');
var $rtfeldman$elm_css$Css$top = $rtfeldman$elm_css$Css$prop1('top');
var $rtfeldman$elm_css$Css$valuesOrNone = function (list) {
	return $elm$core$List$isEmpty(list) ? {value: 'none'} : {
		value: A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				function ($) {
					return $.value;
				},
				list))
	};
};
var $rtfeldman$elm_css$Css$transforms = A2(
	$elm$core$Basics$composeL,
	$rtfeldman$elm_css$Css$prop1('transform'),
	$rtfeldman$elm_css$Css$valuesOrNone);
var $rtfeldman$elm_css$Css$transform = function (only) {
	return $rtfeldman$elm_css$Css$transforms(
		_List_fromArray(
			[only]));
};
var $rtfeldman$elm_css$Css$Transitions$Transform = {$: 'Transform'};
var $rtfeldman$elm_css$Css$Transitions$transform = $rtfeldman$elm_css$Css$Transitions$durationTransition($rtfeldman$elm_css$Css$Transitions$Transform);
var $rtfeldman$elm_css$Css$cssFunction = F2(
	function (funcName, args) {
		return funcName + ('(' + (A2($elm$core$String$join, ', ', args) + ')'));
	});
var $rtfeldman$elm_css$Css$translate2 = F2(
	function (tx, ty) {
		return {
			transform: $rtfeldman$elm_css$Css$Structure$Compatible,
			value: A2(
				$rtfeldman$elm_css$Css$cssFunction,
				'translate',
				_List_fromArray(
					[tx.value, ty.value]))
		};
	});
var $author$project$View$floatingLabel = F4(
	function (stim, val, msg, givenIsFeedback) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$relative),
							$rtfeldman$elm_css$Css$Global$descendants(
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Global$selector,
									'.floating-label__input:not(:placeholder-shown) + label',
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$backgroundColor(
											$rtfeldman$elm_css$Css$hex('ffffff')),
											$rtfeldman$elm_css$Css$transform(
											A2(
												$rtfeldman$elm_css$Css$translate2,
												$rtfeldman$elm_css$Css$zero,
												$rtfeldman$elm_css$Css$pct(-50))),
											$rtfeldman$elm_css$Css$opacity(
											$rtfeldman$elm_css$Css$num(1))
										]))
								]))
						]))
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$input,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('floating-label__input'),
							$rtfeldman$elm_css$Html$Styled$Attributes$placeholder(stim),
							$rtfeldman$elm_css$Html$Styled$Attributes$readonly(givenIsFeedback),
							$rtfeldman$elm_css$Html$Styled$Attributes$value(val),
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$padding(
									$rtfeldman$elm_css$Css$px(8)),
									$rtfeldman$elm_css$Css$borderRadius(
									$rtfeldman$elm_css$Css$px(4)),
									A3(
									$rtfeldman$elm_css$Css$border3,
									$rtfeldman$elm_css$Css$px(1),
									$rtfeldman$elm_css$Css$solid,
									$rtfeldman$elm_css$Css$hex('efefef'))
								])),
							$rtfeldman$elm_css$Html$Styled$Events$onInput(msg)
						]),
					_List_Nil),
					A2(
					$rtfeldman$elm_css$Html$Styled$label,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('floating-label__label'),
							$rtfeldman$elm_css$Html$Styled$Attributes$css(
							_List_fromArray(
								[
									$rtfeldman$elm_css$Css$position($rtfeldman$elm_css$Css$absolute),
									$rtfeldman$elm_css$Css$left(
									$rtfeldman$elm_css$Css$px(8)),
									$rtfeldman$elm_css$Css$top(
									$rtfeldman$elm_css$Css$px(0)),
									$rtfeldman$elm_css$Css$opacity($rtfeldman$elm_css$Css$zero),
									$rtfeldman$elm_css$Css$pointerEvents($rtfeldman$elm_css$Css$none),
									$rtfeldman$elm_css$Css$Transitions$transition(
									_List_fromArray(
										[
											$rtfeldman$elm_css$Css$Transitions$opacity(200),
											$rtfeldman$elm_css$Css$Transitions$transform(200)
										])),
									$rtfeldman$elm_css$Css$color(
									$rtfeldman$elm_css$Css$hex('b6b6b6'))
								]))
						]),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(stim)
						]))
				]));
	});
var $author$project$View$genericNeutralFeedback = function (data) {
	var feedback_Correct = data.feedback_Correct;
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class(
				'max-w-xl w-full rounded-md text-center object-center  mb-8 ' + (data.isVisible ? 'bg-indigo-800' : ((!data.isVisible) ? '' : '')))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$p,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class(
						'font-medium py-4 w-full text-white' + (' ' + (data.isVisible ? 'visible' : 'invisible')))
					]),
				_List_fromArray(
					[
						$author$project$View$fromMarkdown(
						A2($lukewestby$elm_string_interpolate$String$Interpolate$interpolate, feedback_Correct.a, feedback_Correct.b))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$class('p-4')
					]),
				_List_fromArray(
					[data.button]))
			]));
};
var $author$project$Session3$CU3$view = function (exp) {
	switch (exp.$) {
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Task is not started yet.')
					]));
		case 'Loading':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading...')
					]));
		case 'Err':
			var reason = exp.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('I stumbled into an error : ' + reason)
					]));
		default:
			switch (exp.a.$) {
				case 'Instructions':
					var _v1 = exp.a;
					var data = exp.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$View$instructions, data.infos.instructions, $author$project$Session3$CU3$UserClickedStartTraining)
							]));
				case 'Training':
					var _v2 = exp.a;
					var current = exp.b.current;
					var state = exp.b.state;
					var feedback = exp.b.feedback;
					if (current.$ === 'Just') {
						var trial = current.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-lg  text-lg m-4')
										]),
									_List_fromArray(
										[
											$author$project$View$fromMarkdown(trial.context),
											A2($rtfeldman$elm_css$Html$Styled$br, _List_Nil, _List_Nil)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-row p-4 text-lg items-center')
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$span,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('pr-4')
												]),
											_List_fromArray(
												[
													$author$project$View$fromMarkdown(trial.amorce)
												])),
											A4($author$project$View$floatingLabel, '', state.userAnswer, $author$project$Session3$CU3$UserChangedInput, feedback)
										])),
									$author$project$View$genericNeutralFeedback(
									{
										button: A3($author$project$View$navigationButton, $author$project$Session3$CU3$UserClickedToggleFeedback, $author$project$Session3$CU3$UserClickedNextTrial, feedback),
										feedback_Correct: _Utils_Tuple2(trial.feedback, _List_Nil),
										isVisible: feedback
									})
								]));
					} else {
						return $author$project$View$introToMain($author$project$Session3$CU3$UserClickedStartMain);
					}
				default:
					var _v4 = exp.a;
					var data = exp.b;
					var mainTrials = data.mainTrials;
					var current = data.current;
					var state = data.state;
					var feedback = data.feedback;
					var history = data.history;
					if (current.$ === 'Just') {
						var trial = current.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									$author$project$View$tooltip(data.infos.instructions_short),
									A2($author$project$Progressbar$progressBar, history, mainTrials),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-lg  text-lg m-4')
										]),
									_List_fromArray(
										[
											$author$project$View$fromMarkdown(trial.context),
											A2($rtfeldman$elm_css$Html$Styled$br, _List_Nil, _List_Nil)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-row p-4 text-lg items-center')
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$span,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('pr-4')
												]),
											_List_fromArray(
												[
													$author$project$View$fromMarkdown(trial.amorce)
												])),
											A4($author$project$View$floatingLabel, '', state.userAnswer, $author$project$Session3$CU3$UserChangedInput, feedback)
										])),
									$author$project$View$genericNeutralFeedback(
									{
										button: A3($author$project$View$navigationButton, $author$project$Session3$CU3$UserClickedToggleFeedback, $author$project$Session3$CU3$UserClickedNextTrial, feedback),
										feedback_Correct: _Utils_Tuple2(trial.feedback, _List_Nil),
										isVisible: feedback
									})
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session3$CU3$UserClickedSaveData, '/');
					}
			}
	}
};
var $author$project$Session3$Spelling3$UserChangedInput = function (a) {
	return {$: 'UserChangedInput', a: a};
};
var $author$project$Session3$Spelling3$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session3$Spelling3$UserClickedPlayAudio = function (a) {
	return {$: 'UserClickedPlayAudio', a: a};
};
var $author$project$Session3$Spelling3$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session3$Spelling3$UserClickedStartMain = {$: 'UserClickedStartMain'};
var $author$project$Session3$Spelling3$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $author$project$Session3$Spelling3$UserClickedToggleFeedback = {$: 'UserClickedToggleFeedback'};
var $elm$core$String$trim = _String_trim;
var $author$project$Session3$Spelling3$view = function (exp) {
	switch (exp.$) {
		case 'NotStarted':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('experiment did not start yet')
					]));
		case 'Running':
			switch (exp.a.$) {
				case 'Instructions':
					var _v1 = exp.a;
					var data = exp.b;
					return A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$View$instructions, data.infos.instructions, $author$project$Session3$Spelling3$UserClickedStartTraining)
							]));
				case 'Training':
					var _v2 = exp.a;
					var data = exp.b;
					var current = data.current;
					var state = data.state;
					var feedback = data.feedback;
					var history = data.history;
					if (current.$ === 'Just') {
						var trial = current.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center')
								]),
							_List_fromArray(
								[
									A3(
									$author$project$View$trainingWheelsGeneric,
									$elm$core$List$length(history),
									data.infos.trainingWheel,
									_List_Nil),
									A3($author$project$View$audioButton, $author$project$Session3$Spelling3$UserClickedPlayAudio, trial.audioSentence.url, 'word'),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('p-8')
										]),
									_List_fromArray(
										[
											A4($author$project$View$floatingLabel, '', state.userAnswer, $author$project$Session3$Spelling3$UserChangedInput, feedback)
										])),
									$author$project$View$genericSingleChoiceFeedback(
									{
										button: A3($author$project$View$navigationButton, $author$project$Session3$Spelling3$UserClickedToggleFeedback, $author$project$Session3$Spelling3$UserClickedNextTrial, feedback),
										feedback_Correct: _Utils_Tuple2(
											data.infos.feedback_correct,
											_List_fromArray(
												[trial.writtenWord])),
										feedback_Incorrect: _Utils_Tuple2(
											data.infos.feedback_incorrect,
											_List_fromArray(
												[trial.writtenWord])),
										isVisible: feedback,
										target: trial.writtenWord,
										userAnswer: $elm$core$String$toLower(
											$elm$core$String$trim(state.userAnswer))
									})
								]));
					} else {
						return $author$project$View$introToMain($author$project$Session3$Spelling3$UserClickedStartMain);
					}
				default:
					var _v4 = exp.a;
					var data = exp.b;
					var current = data.current;
					var state = data.state;
					var feedback = data.feedback;
					if (current.$ === 'Just') {
						var trial = current.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col justify-center items-center max-w-3xl m-4 p-4')
								]),
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('h-8 w-8 pb-16'),
											$rtfeldman$elm_css$Html$Styled$Events$onClick(
											$author$project$Session3$Spelling3$UserClickedPlayAudio(trial.audioSentence.url))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$music)
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('pb-8')
										]),
									_List_fromArray(
										[
											A4($author$project$View$floatingLabel, 'Type here', state.userAnswer, $author$project$Session3$Spelling3$UserChangedInput, feedback)
										])),
									$author$project$View$genericSingleChoiceFeedback(
									{
										button: A3($author$project$View$navigationButton, $author$project$Session3$Spelling3$UserClickedToggleFeedback, $author$project$Session3$Spelling3$UserClickedNextTrial, feedback),
										feedback_Correct: _Utils_Tuple2(
											data.infos.feedback_correct,
											_List_fromArray(
												[trial.writtenWord])),
										feedback_Incorrect: _Utils_Tuple2(
											data.infos.feedback_incorrect,
											_List_fromArray(
												[trial.writtenWord])),
										isVisible: feedback,
										target: trial.writtenWord,
										userAnswer: $elm$core$String$toLower(
											$elm$core$String$trim(state.userAnswer))
									})
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session3$Spelling3$UserClickedSaveData, 'context-understanding');
					}
			}
		case 'Err':
			var reason = exp.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('I stumbled into an error : ' + reason)
					]));
		default:
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_Nil,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Loading...')
					]));
	}
};
var $author$project$Main$UserToggledInCloudWords = function (a) {
	return {$: 'UserToggledInCloudWords', a: a};
};
var $author$project$Main$viewCloud = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$class('grid grid-flow-col grid-rows-4 auto-cols-max gap-4 ')
			]),
		A2(
			$elm$core$List$map,
			function (word) {
				return A2(
					$rtfeldman$elm_css$Html$Styled$label,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$Attributes$class('border-2 p-2 text-black align-baseline flex flex-row')
						]),
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$input,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$type_('checkbox'),
									$rtfeldman$elm_css$Html$Styled$Events$onClick(
									$author$project$Main$UserToggledInCloudWords(word))
								]),
							_List_Nil),
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('pl-4')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(word)
								]))
						]));
			},
			$elm$core$Dict$keys(model.cloudWords)));
};
var $author$project$Session2$Spelling$PlayAudio = function (a) {
	return {$: 'PlayAudio', a: a};
};
var $author$project$Session2$Spelling$UserClickedFeedbackButton = {$: 'UserClickedFeedbackButton'};
var $author$project$Session2$Spelling$UserClickedNextTrial = function (a) {
	return {$: 'UserClickedNextTrial', a: a};
};
var $author$project$Session2$Spelling$UserClickedSaveData = {$: 'UserClickedSaveData'};
var $author$project$Session2$Spelling$UserClickedStartAudio = function (a) {
	return {$: 'UserClickedStartAudio', a: a};
};
var $author$project$Session2$Spelling$UserClickedStartMainloop = function (a) {
	return {$: 'UserClickedStartMainloop', a: a};
};
var $author$project$Session2$Spelling$UserClickedStartTraining = {$: 'UserClickedStartTraining'};
var $rtfeldman$elm_css$VirtualDom$Styled$style = F2(
	function (key, val) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$style, key, val),
			_List_Nil,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$style = $rtfeldman$elm_css$VirtualDom$Styled$style;
var $author$project$Session2$Spelling$containerStyles = _List_fromArray(
	[
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'display', 'flex'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'flex-wrap', 'wrap'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'align-items', 'center'),
		A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'padding-top', '2em'),
		$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold text-lg')
	]);
var $rtfeldman$elm_css$VirtualDom$Styled$unstyledAttribute = function (prop) {
	return A3($rtfeldman$elm_css$VirtualDom$Styled$Attribute, prop, _List_Nil, '');
};
var $rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$unstyledAttribute;
var $author$project$Session2$Spelling$ghostGreen = '#2f804e';
var $author$project$Session2$Spelling$itemStyles = function (color) {
	return _List_fromArray(
		[
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'width', '5rem'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'height', '5rem'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'background-color', color),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'border-radius', '8px'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'color', 'white'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'cursor', 'pointer'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'margin', '0 2em 2em 0'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'display', 'flex'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'align-items', 'center'),
			A2($rtfeldman$elm_css$Html$Styled$Attributes$style, 'justify-content', 'center')
		]);
};
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$Session2$Spelling$ghostView = F2(
	function (dnd, items) {
		var maybeDragItem = A2(
			$elm$core$Maybe$andThen,
			function (_v2) {
				var dragIndex = _v2.dragIndex;
				return $elm$core$List$head(
					A2($elm$core$List$drop, dragIndex, items));
			},
			$author$project$Session2$Spelling$system.info(dnd));
		if (maybeDragItem.$ === 'Just') {
			var _v1 = maybeDragItem.a;
			var item = _v1.b;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_Utils_ap(
					$author$project$Session2$Spelling$itemStyles($author$project$Session2$Spelling$ghostGreen),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
						$author$project$Session2$Spelling$system.ghostStyles(dnd))),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text(
						$elm$core$String$toUpper(item))
					]));
		} else {
			return $rtfeldman$elm_css$Html$Styled$text('');
		}
	});
var $author$project$Session2$Spelling$green = '#3da565';
var $author$project$Session2$Spelling$itemView = F3(
	function (dnd, index, _v0) {
		var key = _v0.a;
		var item = _v0.b;
		var itemId = 'id-' + key;
		var _v1 = $author$project$Session2$Spelling$system.info(dnd);
		if (_v1.$ === 'Just') {
			var dragIndex = _v1.a.dragIndex;
			return (!_Utils_eq(dragIndex, index)) ? _Utils_Tuple2(
				key,
				A2(
					$rtfeldman$elm_css$Html$Styled$div,
					A2(
						$elm$core$List$cons,
						$rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
						_Utils_ap(
							$author$project$Session2$Spelling$itemStyles($author$project$Session2$Spelling$green),
							A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
								A2($author$project$Session2$Spelling$system.dropEvents, index, itemId)))),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$elm$core$String$toUpper(item))
						]))) : _Utils_Tuple2(
				key,
				A2(
					$rtfeldman$elm_css$Html$Styled$div,
					A2(
						$elm$core$List$cons,
						$rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
						$author$project$Session2$Spelling$itemStyles('dimgray')),
					_List_Nil));
		} else {
			return _Utils_Tuple2(
				key,
				A2(
					$rtfeldman$elm_css$Html$Styled$div,
					A2(
						$elm$core$List$cons,
						$rtfeldman$elm_css$Html$Styled$Attributes$id(itemId),
						_Utils_ap(
							$author$project$Session2$Spelling$itemStyles($author$project$Session2$Spelling$green),
							A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Html$Styled$Attributes$fromUnstyled,
								A2($author$project$Session2$Spelling$system.dragEvents, index, itemId)))),
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(
							$elm$core$String$toUpper(item))
						])));
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$keyedNode = $rtfeldman$elm_css$VirtualDom$Styled$KeyedNode;
var $rtfeldman$elm_css$Html$Styled$Keyed$node = $rtfeldman$elm_css$VirtualDom$Styled$keyedNode;
var $author$project$Session2$Spelling$viewScrabbleTask = function (model) {
	var viewLetters = function (scrambledLetters) {
		return A3(
			$rtfeldman$elm_css$Html$Styled$Keyed$node,
			'div',
			$author$project$Session2$Spelling$containerStyles,
			A2(
				$elm$core$List$indexedMap,
				$author$project$Session2$Spelling$itemView(model.dnd),
				scrambledLetters));
	};
	var audioButton = function (url) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Events$onClick(
					$author$project$Session2$Spelling$PlayAudio(url)),
					$rtfeldman$elm_css$Html$Styled$Attributes$class('col-start-2 col-span-4 h-8 w-8')
				]),
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$fromUnstyled($author$project$Icons$music)
				]));
	};
	var _v0 = model.scrabbleTask;
	switch (_v0.$) {
		case 'NotStarted':
			return $rtfeldman$elm_css$Html$Styled$text('Not Asked');
		case 'Running':
			switch (_v0.a.$) {
				case 'Instructions':
					var _v1 = _v0.a;
					var data = _v0.b;
					return A2($author$project$View$instructions, data.infos.instructions, $author$project$Session2$Spelling$UserClickedStartTraining);
				case 'Main':
					var _v2 = _v0.a;
					var data = _v0.b;
					var _v3 = data.current;
					if (_v3.$ === 'Just') {
						var currentTrial = _v3.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center justify-center')
								]),
							_List_fromArray(
								[
									A2($author$project$Progressbar$progressBar, data.history, data.mainTrials),
									A3($author$project$View$audioButton, $author$project$Session2$Spelling$UserClickedStartAudio, currentTrial.audioWord.url, 'word'),
									(!data.feedback) ? A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col items-center w-full')
										]),
									_List_fromArray(
										[
											viewLetters(data.state.scrambledLetter),
											A2($author$project$Session2$Spelling$ghostView, model.dnd, data.state.scrambledLetter)
										])) : A2($rtfeldman$elm_css$Html$Styled$div, _List_Nil, _List_Nil),
									$author$project$View$genericSingleChoiceFeedback(
									{
										button: A3(
											$author$project$View$navigationButton,
											$author$project$Session2$Spelling$UserClickedFeedbackButton,
											$author$project$Session2$Spelling$UserClickedNextTrial(data.next),
											data.feedback),
										feedback_Correct: _Utils_Tuple2(
											data.infos.feedback_correct,
											_List_fromArray(
												[
													$author$project$View$bold(currentTrial.target)
												])),
										feedback_Incorrect: _Utils_Tuple2(
											data.infos.feedback_incorrect,
											_List_fromArray(
												[
													$author$project$View$bold(currentTrial.target)
												])),
										isVisible: data.feedback,
										target: currentTrial.target,
										userAnswer: data.state.userAnswer
									})
								]));
					} else {
						return A3($author$project$View$end, data.infos.end, $author$project$Session2$Spelling$UserClickedSaveData, 'context-understanding');
					}
				default:
					var _v4 = _v0.a;
					var data = _v0.b;
					var _v5 = data.current;
					if (_v5.$ === 'Just') {
						var currentTrial = _v5.a;
						return A2(
							$author$project$View$viewTraining,
							data.infos.instructions,
							_List_fromArray(
								[
									audioButton(currentTrial.audioWord.url),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('col-start-2 col-span-4')
										]),
									_List_fromArray(
										[
											viewLetters(data.state.scrambledLetter)
										])),
									A2($author$project$Session2$Spelling$ghostView, model.dnd, data.state.scrambledLetter),
									$author$project$View$genericSingleChoiceFeedback(
									{
										button: A3(
											$author$project$View$navigationButton,
											$author$project$Session2$Spelling$UserClickedFeedbackButton,
											$author$project$Session2$Spelling$UserClickedNextTrial(data.next),
											data.feedback),
										feedback_Correct: _Utils_Tuple2(
											data.infos.feedback_correct,
											_List_fromArray(
												[
													$author$project$View$bold(currentTrial.target)
												])),
										feedback_Incorrect: _Utils_Tuple2(
											data.infos.feedback_incorrect,
											_List_fromArray(
												[
													$author$project$View$bold(currentTrial.target)
												])),
										isVisible: data.feedback,
										target: currentTrial.target,
										userAnswer: data.state.userAnswer
									})
								]));
					} else {
						return $author$project$View$introToMain(
							$author$project$Session2$Spelling$UserClickedStartMainloop(data.mainTrials));
					}
			}
		case 'Loading':
			return $rtfeldman$elm_css$Html$Styled$text('Loading...');
		default:
			var reason = _v0.a;
			return $rtfeldman$elm_css$Html$Styled$text(reason);
	}
};
var $author$project$Session3$Synonym$SaveDataMsg = {$: 'SaveDataMsg'};
var $author$project$Session3$Synonym$UserCLickedStartTraining = {$: 'UserCLickedStartTraining'};
var $author$project$Session3$Synonym$UserChangedInput = function (a) {
	return {$: 'UserChangedInput', a: a};
};
var $author$project$Session3$Synonym$UserClickedFeedback = {$: 'UserClickedFeedback'};
var $author$project$Session3$Synonym$UserClickedNextTrial = {$: 'UserClickedNextTrial'};
var $author$project$Session3$Synonym$UserClickedStartMainloop = {$: 'UserClickedStartMainloop'};
var $rtfeldman$elm_css$Html$Styled$h4 = $rtfeldman$elm_css$Html$Styled$node('h4');
var $author$project$View$sentenceInSynonym = F4(
	function (t, state, msg, feedback_) {
		return A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex w-full border-2 p-4  space-x-4 text-xl text-center items-center')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(t.pre)
						])),
					A4($author$project$View$floatingLabel, t.stimulus, state.userAnswer, msg, feedback_),
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text(t.post)
						]))
				]));
	});
var $author$project$Session3$Synonym$trainingWheels = F3(
	function (trialn, radical, target) {
		var helpSentence = A2(
			$rtfeldman$elm_css$Html$Styled$div,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col pt-4 italic text-xl ')
				]),
			_List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$p,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('The synonym of '),
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(radical)
								])),
							$rtfeldman$elm_css$Html$Styled$text(' is '),
							A2(
							$rtfeldman$elm_css$Html$Styled$span,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold')
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(target)
								]))
						])),
					A2(
					$rtfeldman$elm_css$Html$Styled$span,
					_List_Nil,
					_List_fromArray(
						[
							$rtfeldman$elm_css$Html$Styled$text('Type it here and you\'re good to go!')
						]))
				]));
		if (!trialn) {
			return helpSentence;
		} else {
			return A2($rtfeldman$elm_css$Html$Styled$div, _List_Nil, _List_Nil);
		}
	});
var $author$project$Session3$Synonym$viewTask = function (experiment) {
	switch (experiment.$) {
		case 'Err':
			var reason = experiment.a;
			return _List_fromArray(
				[
					A2(
					$rtfeldman$elm_css$Html$Styled$h4,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_Nil,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('Failure' + reason)
								]))
						]))
				]);
		case 'NotStarted':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('I\'m not started yet.')
				]);
		case 'Loading':
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('Loading...')
				]);
		default:
			switch (experiment.a.$) {
				case 'Instructions':
					var _v1 = experiment.a;
					var data = experiment.b;
					return _List_fromArray(
						[
							A2($author$project$View$instructions, data.infos.instructions, $author$project$Session3$Synonym$UserCLickedStartTraining)
						]);
				case 'Training':
					var _v2 = experiment.a;
					var task = experiment.b;
					var trainingBox = $rtfeldman$elm_css$Html$Styled$div(
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$class('container w-full h-full border-4 border-green-500 border-rounded-lg border-dashed text-center object-center ')
							]));
					var toggleFeedback = $author$project$View$button(
						{isDisabled: false, message: $author$project$Session3$Synonym$UserClickedFeedback, txt: 'Check my answer'});
					var _v3 = _Utils_Tuple2(task.current, task.feedback);
					if (_v3.a.$ === 'Just') {
						if (!_v3.b) {
							var x = _v3.a.a;
							return _List_fromArray(
								[
									trainingBox(
									_List_fromArray(
										[
											A3(
											$author$project$Session3$Synonym$trainingWheels,
											$elm$core$List$length(task.history),
											x.radical,
											x.target),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('p-8')
												]),
											_List_fromArray(
												[
													A4($author$project$View$sentenceInSynonym, x, task.state, $author$project$Session3$Synonym$UserChangedInput, task.feedback)
												])),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('m-8')
												]),
											_List_fromArray(
												[toggleFeedback]))
										]))
								]);
						} else {
							var x = _v3.a.a;
							return _List_fromArray(
								[
									trainingBox(
									_List_fromArray(
										[
											A3(
											$author$project$Session3$Synonym$trainingWheels,
											$elm$core$List$length(task.history),
											x.stimulus,
											x.target),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('m-8')
												]),
											_List_fromArray(
												[
													A4($author$project$View$sentenceInSynonym, x, task.state, $author$project$Session3$Synonym$UserChangedInput, task.feedback)
												])),
											A2(
											$rtfeldman$elm_css$Html$Styled$div,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class(' rounded-md text-center object-center bg-green-300 m-8')
												]),
											_List_fromArray(
												[
													A2(
													$rtfeldman$elm_css$Html$Styled$p,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$Attributes$class('p-6 text-xl text-white')
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$text('The correct synonym for '),
															$rtfeldman$elm_css$Html$Styled$text(x.radical),
															$rtfeldman$elm_css$Html$Styled$text(' is '),
															A2(
															$rtfeldman$elm_css$Html$Styled$span,
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold')
																]),
															_List_fromArray(
																[
																	$rtfeldman$elm_css$Html$Styled$text(x.target)
																]))
														])),
													A2(
													$rtfeldman$elm_css$Html$Styled$div,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$Attributes$class('pb-4')
														]),
													_List_fromArray(
														[
															$author$project$View$button(
															{isDisabled: false, message: $author$project$Session3$Synonym$UserClickedNextTrial, txt: 'Next'})
														]))
												]))
										]))
								]);
						}
					} else {
						var _v4 = _v3.a;
						return _List_fromArray(
							[
								$author$project$View$introToMain($author$project$Session3$Synonym$UserClickedStartMainloop)
							]);
					}
				default:
					var _v5 = experiment.a;
					var task = experiment.b;
					var _v6 = _Utils_Tuple2(task.current, task.feedback);
					if (_v6.a.$ === 'Just') {
						if (!_v6.b) {
							var t = _v6.a.a;
							return _List_fromArray(
								[
									$author$project$View$tooltip('Type the synonym of the word in the box'),
									A4($author$project$View$sentenceInSynonym, t, task.state, $author$project$Session3$Synonym$UserChangedInput, task.feedback),
									$author$project$View$button(
									{isDisabled: false, message: $author$project$Session3$Synonym$UserClickedFeedback, txt: 'Check my answer'})
								]);
						} else {
							var t = _v6.a.a;
							return _List_fromArray(
								[
									A4($author$project$View$sentenceInSynonym, t, task.state, $author$project$Session3$Synonym$UserChangedInput, task.feedback),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('p-4')
										]),
									_List_Nil),
									A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$class('flex flex-col w-full rounded-lg h-48 bg-green-300 items-center text-center')
										]),
									_List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Html$Styled$p,
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$Attributes$class('pt-8 text-lg text-white')
												]),
											_List_fromArray(
												[
													$rtfeldman$elm_css$Html$Styled$text('The best synonym for ' + (t.radical + ' is ')),
													A2(
													$rtfeldman$elm_css$Html$Styled$span,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$Attributes$class('font-bold')
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$text(t.target)
														]))
												])),
											$author$project$View$button(
											{isDisabled: false, message: $author$project$Session3$Synonym$UserClickedNextTrial, txt: 'Next'})
										]))
								]);
						}
					} else {
						var _v7 = _v6.a;
						return _List_fromArray(
							[
								A3($author$project$View$end, task.infos.end, $author$project$Session3$Synonym$SaveDataMsg, 'spelling')
							]);
					}
			}
	}
};
var $author$project$Main$body = function (model) {
	var infos = function () {
		var _v8 = model.infos;
		if (_v8.$ === 'Success') {
			var infos_ = _v8.a;
			return infos_;
		} else {
			return $elm$core$Dict$empty;
		}
	}();
	return _List_fromArray(
		[
			$author$project$View$header(
			_List_fromArray(
				[
					A2($author$project$View$navOut, 'BCL', 'https://bcl.cnrs.fr/'),
					A2($author$project$View$navOut, 'L\'équipe', 'https://bcl.cnrs.fr/rubrique225')
				])),
			$author$project$View$container(
			function () {
				var _v0 = model.route;
				switch (_v0.$) {
					case 'Session1':
						var task = _v0.b;
						switch (task.$) {
							case 'TopSession1':
								return $author$project$Session1$Top$view(infos);
							case 'Meaning':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Meaning,
										$author$project$Session1$Meaning$view(
											{optionsOrder: model.optionsOrder, task: model.meaning}))
									]);
							case 'Presentation':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Presentation,
										$author$project$Session1$Presentation$view(model.presentation))
									]);
							case 'SpellingLevel1':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Spelling1,
										A2($author$project$Session1$Spelling$view, model.spellingLvl1, model.optionsOrder))
									]);
							default:
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$CU1,
										$author$project$Session1$ContextUnderstanding$view(
											{optionsOrder: model.optionsOrder, task: model.cu1}))
									]);
						}
					case 'AuthenticatedSession2':
						var task = _v0.b;
						switch (task.$) {
							case 'CU':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$CU2,
										A2($author$project$Session2$CU2$view, model.cuLvl2, model.optionsOrder))
									]);
							case 'Translation':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Translation,
										$author$project$Session2$Translation$view(
											{optionsOrder: model.optionsOrder, task: model.translationTask}))
									]);
							default:
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Spelling2,
										$author$project$Session2$Spelling$viewScrabbleTask(model))
									]);
						}
					case 'AuthenticatedSession3':
						var task = _v0.b;
						switch (task.$) {
							case 'CU3':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$CU3,
										$author$project$Session3$CU3$view(model.cu3))
									]);
							case 'Synonym':
								return A2(
									$elm$core$List$map,
									$rtfeldman$elm_css$Html$Styled$map($author$project$Main$Synonym),
									$author$project$Session3$Synonym$viewTask(model.synonymTask));
							default:
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$map,
										$author$project$Main$Spelling3,
										$author$project$Session3$Spelling3$view(model.spelling3))
									]);
						}
					case 'Posttest':
						var task = _v0.a;
						return _List_fromArray(
							[
								$author$project$Main$viewCloud(model)
							]);
					case 'Pilote':
						var task = _v0.b;
						switch (task.$) {
							case 'AcceptabilityEnd':
								return _List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Thanks again for your time and your help with this project. \nContact shona.whyte@univ-cotedazur.fr if you have any questions or comments.')
											]))
									]);
							case 'AcceptabilityStart':
								return A2(
									$author$project$Pretest$Acceptability$view,
									model.acceptabilityTask,
									{
										saveDataMsg: $author$project$Main$Acceptability($author$project$Pretest$Acceptability$UserClickedSaveMsg),
										startMainMsg: F2(
											function (informations, trials) {
												return $author$project$Main$Acceptability(
													A2($author$project$Pretest$Acceptability$StartMain, trials, informations));
											}),
										startTraining: $author$project$Main$Acceptability($author$project$Pretest$Acceptability$StartTraining)
									});
							default:
								var _v6 = model.infos;
								switch (_v6.$) {
									case 'Success':
										var informations = _v6.a;
										var taskInfo = A2(
											$elm$core$Maybe$withDefault,
											'I couldn\'t find the infos of the task : recR8areYkKRvQ6lU ',
											A2(
												$elm$core$Maybe$map,
												function ($) {
													return $.instructions;
												},
												A2($elm$core$Dict$get, 'recR8areYkKRvQ6lU', informations)));
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col items-center justify-center w-full max-w-2-xl')
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$h1,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$class('')
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('Instructions')
															])),
														A2(
														$rtfeldman$elm_css$Html$Styled$p,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-2xl text-xl text-center mb-8')
															]),
														_List_fromArray(
															[
																$author$project$View$fromMarkdown(taskInfo)
															])),
														$author$project$View$button(
														{
															isDisabled: false,
															message: $author$project$Main$Acceptability($author$project$Pretest$Acceptability$StartTraining),
															txt: 'start'
														})
													]))
											]);
									case 'Failure':
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$p,
												_List_Nil,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('I couldn\'t find the tasks infos. Please report this error.')
													]))
											]);
									case 'Loading':
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Loading...')
											]);
									default:
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Info not asked')
											]);
								}
						}
					case 'Pretest':
						var task = _v0.a;
						switch (task.$) {
							case 'YN':
								return _List_fromArray(
									[
										A2(
										$author$project$Postest$YN$view,
										model.yn,
										{
											nextTrialMsg: $author$project$Main$YN($author$project$Postest$YN$UserClickedNextTrial),
											startMainMsg: F2(
												function (trials, informations) {
													return $author$project$Main$YN(
														A2($author$project$Postest$YN$UserClickedStartMain, trials, informations));
												}),
											toggleFeedback: $author$project$Main$YN($author$project$Postest$YN$UserClickedToggleFeedback),
											userChangedInput: function (str) {
												return $author$project$Main$YN(
													$author$project$Postest$YN$UserChangedInput(str));
											}
										})
									]);
							case 'EmailSent':
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Un email a été envoyé. Veuillez cliquer sur le lien pour continuer l\'expérience.')
									]);
							case 'SPR':
								return A2(
									$elm$core$List$map,
									$rtfeldman$elm_css$Html$Styled$map($author$project$Main$SPR),
									$author$project$Pretest$SPR$view(model.spr));
							case 'SentenceCompletion':
								return A2(
									$elm$core$List$map,
									$rtfeldman$elm_css$Html$Styled$map($author$project$Main$SentenceCompletion),
									$author$project$Pretest$SentenceCompletion$view(model.sentenceCompletion));
							case 'GeneralInfos':
								return _List_Nil;
							default:
								return A2(
									$elm$core$List$map,
									$rtfeldman$elm_css$Html$Styled$map($author$project$Main$VKS),
									$author$project$Pretest$VKS$view(model.vks));
						}
					case 'Home':
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('container flex flex-col items-center justify-center w-full max-w-2-xl')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$h1,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Lex Learn 👩\u200D🎓️')
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$p,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$class('max-w-2xl text-xl text-center mb-8')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n Sapien et ligula ullamcorper malesuada proin libero nunc consequat. Sed sed risus pretium quam vulputate dignissim. Aliquam sem fringilla ut morbi tincidunt augue interdum velit euismod. Ultrices tincidunt arcu non sodales neque.')
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$a,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$href('/pretest/informations')
											]),
										_List_fromArray(
											[
												$author$project$View$button(
												{isDisabled: false, message: $author$project$Main$NoOp, txt: 'Commencer les prétests'})
											]))
									]))
							]);
					default:
						return $author$project$View$notFound;
				}
			}())
		]);
};
var $author$project$Main$project = {
	description: '\n        Une expérience visant à mieux comprendre l\'acquisition de nouvelles structures grammaticales en langue anglaise. \n      ',
	title: 'Apprentissage et espacement',
	url: A2(
		$elm$url$Url$Builder$absolute,
		_List_fromArray(
			['start']),
		_List_Nil)
};
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles = F2(
	function (_v0, styles) {
		var newStyles = _v0.b;
		var classname = _v0.c;
		return $elm$core$List$isEmpty(newStyles) ? styles : A3($elm$core$Dict$insert, classname, newStyles, styles);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute = function (_v0) {
	var val = _v0.a;
	return val;
};
var $elm$virtual_dom$VirtualDom$keyedNodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_keyedNodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml = F2(
	function (_v6, _v7) {
		var key = _v6.a;
		var html = _v6.b;
		var pairs = _v7.a;
		var styles = _v7.b;
		switch (html.$) {
			case 'Unstyled':
				var vdom = html.a;
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v9 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v9.a;
				var finalStyles = _v9.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v10 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v10.a;
				var finalStyles = _v10.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v11 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v11.a;
				var finalStyles = _v11.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v12 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v12.a;
				var finalStyles = _v12.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml = F2(
	function (html, _v0) {
		var nodes = _v0.a;
		var styles = _v0.b;
		switch (html.$) {
			case 'Unstyled':
				var vdomNode = html.a;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v2 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v2.a;
				var finalStyles = _v2.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v3 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v3.a;
				var finalStyles = _v3.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v4 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v4.a;
				var finalStyles = _v4.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v5 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v5.a;
				var finalStyles = _v5.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
		}
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$stylesFromPropertiesHelp = F2(
	function (candidate, properties) {
		stylesFromPropertiesHelp:
		while (true) {
			if (!properties.b) {
				return candidate;
			} else {
				var _v1 = properties.a;
				var styles = _v1.b;
				var classname = _v1.c;
				var rest = properties.b;
				if ($elm$core$String$isEmpty(classname)) {
					var $temp$candidate = candidate,
						$temp$properties = rest;
					candidate = $temp$candidate;
					properties = $temp$properties;
					continue stylesFromPropertiesHelp;
				} else {
					var $temp$candidate = $elm$core$Maybe$Just(
						_Utils_Tuple2(classname, styles)),
						$temp$properties = rest;
					candidate = $temp$candidate;
					properties = $temp$properties;
					continue stylesFromPropertiesHelp;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties = function (properties) {
	var _v0 = A2($rtfeldman$elm_css$VirtualDom$Styled$stylesFromPropertiesHelp, $elm$core$Maybe$Nothing, properties);
	if (_v0.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var _v1 = _v0.a;
		var classname = _v1.a;
		var styles = _v1.b;
		return A2($elm$core$Dict$singleton, classname, styles);
	}
};
var $rtfeldman$elm_css$Css$Structure$ClassSelector = function (a) {
	return {$: 'ClassSelector', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$snippetFromPair = function (_v0) {
	var classname = _v0.a;
	var styles = _v0.b;
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$makeSnippet,
		styles,
		$rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$ClassSelector(classname)
				])));
};
var $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration = function (dict) {
	return $rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
		$elm$core$List$singleton(
			$rtfeldman$elm_css$Css$Preprocess$stylesheet(
				A2(
					$elm$core$List$map,
					$rtfeldman$elm_css$VirtualDom$Styled$snippetFromPair,
					$elm$core$Dict$toList(dict)))));
};
var $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode = function (styles) {
	return A3(
		$elm$virtual_dom$VirtualDom$node,
		'style',
		_List_Nil,
		$elm$core$List$singleton(
			$elm$virtual_dom$VirtualDom$text(
				$rtfeldman$elm_css$VirtualDom$Styled$toDeclaration(styles))));
};
var $rtfeldman$elm_css$VirtualDom$Styled$unstyle = F3(
	function (elemType, properties, children) {
		var unstyledProperties = A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = $rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(styles);
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$containsKey = F2(
	function (key, pairs) {
		containsKey:
		while (true) {
			if (!pairs.b) {
				return false;
			} else {
				var _v1 = pairs.a;
				var str = _v1.a;
				var rest = pairs.b;
				if (_Utils_eq(key, str)) {
					return true;
				} else {
					var $temp$key = key,
						$temp$pairs = rest;
					key = $temp$key;
					pairs = $temp$pairs;
					continue containsKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey = F2(
	function (_default, pairs) {
		getUnusedKey:
		while (true) {
			if (!pairs.b) {
				return _default;
			} else {
				var _v1 = pairs.a;
				var firstKey = _v1.a;
				var rest = pairs.b;
				var newKey = '_' + firstKey;
				if (A2($rtfeldman$elm_css$VirtualDom$Styled$containsKey, newKey, rest)) {
					var $temp$default = newKey,
						$temp$pairs = rest;
					_default = $temp$default;
					pairs = $temp$pairs;
					continue getUnusedKey;
				} else {
					return newKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode = F2(
	function (allStyles, keyedChildNodes) {
		var styleNodeKey = A2($rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey, '_', keyedChildNodes);
		var finalNode = $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(allStyles);
		return _Utils_Tuple2(styleNodeKey, finalNode);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed = F3(
	function (elemType, properties, keyedChildren) {
		var unstyledProperties = A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = $rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, styles, keyedChildNodes);
		return A3(
			$elm$virtual_dom$VirtualDom$keyedNode,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS = F4(
	function (ns, elemType, properties, keyedChildren) {
		var unstyledProperties = A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = $rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode, styles, keyedChildNodes);
		return A4(
			$elm$virtual_dom$VirtualDom$keyedNodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleNS = F4(
	function (ns, elemType, properties, children) {
		var unstyledProperties = A2($elm$core$List$map, $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute, properties);
		var initialStyles = $rtfeldman$elm_css$VirtualDom$Styled$stylesFromProperties(properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode(styles);
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled = function (vdom) {
	switch (vdom.$) {
		case 'Unstyled':
			var plainNode = vdom.a;
			return plainNode;
		case 'Node':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A3($rtfeldman$elm_css$VirtualDom$Styled$unstyle, elemType, properties, children);
		case 'NodeNS':
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyleNS, ns, elemType, properties, children);
		case 'KeyedNode':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A3($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed, elemType, properties, children);
		default:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS, ns, elemType, properties, children);
	}
};
var $rtfeldman$elm_css$Html$Styled$toUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled;
var $author$project$Main$view = function (model) {
	return {
		body: _List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$toUnstyled(
				A2(
					$rtfeldman$elm_css$Html$Styled$div,
					_List_Nil,
					$author$project$Main$body(model)))
			]),
		title: $author$project$Main$project.title
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$BrowserChangedUrl, onUrlRequest: $author$project$Main$UserClickedLink, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(
		{}))({"versions":{"elm":"0.19.1"},"types":{"message":"Main.Msg","aliases":{"Data.AudioFile":{"args":[],"type":"{ url : String.String, type_ : String.String }"},"ExperimentInfo.Task":{"args":[],"type":"{ uid : String.String, session : ExperimentInfo.Session, type_ : ExperimentInfo.Type_, name : String.String, url : String.String, description : String.String, instructions : String.String, instructions_short : String.String, feedback_correct : String.String, feedback_incorrect : String.String, end : String.String, trainingWheel : String.String, introToMain : String.String }"},"Pretest.Acceptability.Trial":{"args":[],"type":"{ uid : String.String, sentence : String.String, sentenceType : Pretest.Acceptability.SentenceType, trialType : Pretest.Acceptability.TrialType, isGrammatical : Basics.Bool, audio : Data.AudioFile, feedback : String.String, timeout : Basics.Int }"},"Url.Url":{"args":[],"type":"{ protocol : Url.Protocol, host : String.String, port_ : Maybe.Maybe Basics.Int, path : String.String, query : Maybe.Maybe String.String, fragment : Maybe.Maybe String.String }"},"Session1.Session.LoadingMsg":{"args":[],"type":"Task.Parallel.Msg5 (List.List Session1.Meaning.Trial) (List.List Session1.Spelling.Trial) (List.List Session1.ContextUnderstanding.Trial) (List.List Session1.Presentation.Trial) (List.List ExperimentInfo.Task)"},"Pretest.Pretest.ParaMsg":{"args":[],"type":"Task.Parallel.Msg4 (List.List Pretest.SPR.Trial) (List.List Pretest.SentenceCompletion.Trial) (List.List ExperimentInfo.Task) (List.List Pretest.VKS.Trial)"},"Pretest.Pretest.ShuffledPretest":{"args":[],"type":"{ spr : List.List Pretest.SPR.Trial, sc : List.List Pretest.SentenceCompletion.Trial, infos : List.List ExperimentInfo.Task, vks : List.List Pretest.VKS.Trial }"},"Session1.Session.ShuffledSession1":{"args":[],"type":"{ meaning : List.List Session1.Meaning.Trial, spelling : List.List Session1.Spelling.Trial, cu1 : List.List Session1.ContextUnderstanding.Trial, presentation : List.List Session1.Presentation.Trial, infos_ : List.List ExperimentInfo.Task }"},"Session2.Session.ShuffledSession2":{"args":[],"type":"{ cu : List.List Session2.CU2.Trial, spelling : List.List Session2.Spelling.Trial, translation : List.List Session2.Translation.Trial, infos : List.List ExperimentInfo.Task }"},"Session3.Session.ShuffledSession3":{"args":[],"type":"{ cu : List.List Session3.CU3.Trial, spelling : List.List Session3.Spelling3.Trial, synonym : List.List Session3.Synonym.Trial, infos : List.List ExperimentInfo.Task }"},"Pretest.SPR.TaggedSegment":{"args":[],"type":"( Pretest.SPR.Tag, String.String )"},"Postest.YN.Trial":{"args":[],"type":"{ uid : String.String, word : String.String, exists : Basics.Bool }"},"Pretest.SPR.Trial":{"args":[],"type":"{ id : String.String, taggedSegments : List.List Pretest.SPR.TaggedSegment, question : String.String, isGrammatical : Basics.Bool, isTraining : Basics.Bool, feedback : String.String }"},"Pretest.SentenceCompletion.Trial":{"args":[],"type":"{ id : String.String, context : String.String, firstAmorce : String.String, secondAmorce : String.String, isTraining : Basics.Bool, firstFeedback : String.String, secondFeedback : String.String }"},"Pretest.VKS.Trial":{"args":[],"type":"{ id : String.String, verb : String.String }"},"Session1.ContextUnderstanding.Trial":{"args":[],"type":"{ uid : String.String, text : String.String, target : String.String, distractor1 : String.String, distractor2 : String.String, distractor3 : String.String, definition : String.String, isTraining : Basics.Bool }"},"Session1.Meaning.Trial":{"args":[],"type":"{ uid : String.String, writtenWord : String.String, target : String.String, distractor1 : String.String, distractor2 : String.String, distractor3 : String.String, feedbackCorrect : String.String, feedbackIncorrect : String.String, isTraining : Basics.Bool }"},"Session1.Presentation.Trial":{"args":[],"type":"{ uid : String.String, text : String.String, definition : String.String, example : String.String, translation1 : String.String, translation2 : String.String, audio : Data.AudioFile, isTraining : Basics.Bool }"},"Session1.Spelling.Trial":{"args":[],"type":"{ uid : String.String, target : String.String, distractor1 : String.String, distractor2 : String.String, distractor3 : String.String, isTraining : Basics.Bool, audio : Data.AudioFile }"},"Session2.CU2.Trial":{"args":[],"type":"{ uid : String.String, writtenWord : String.String, audioSentence : Data.AudioFile, context : String.String, target : String.String, distractor1 : String.String, distractor2 : String.String, distractor3 : String.String, feedback : String.String, isTraining : Basics.Bool }"},"Session2.Spelling.Trial":{"args":[],"type":"{ uid : String.String, writtenWord : String.String, audioWord : Data.AudioFile, isTraining : Basics.Bool, target : String.String }"},"Session2.Translation.Trial":{"args":[],"type":"{ uid : String.String, question : String.String, target : String.String, translation2 : String.String, distractor1 : String.String, distractor2 : String.String, distractor3 : String.String, word : String.String, isTraining : Basics.Bool }"},"Session3.CU3.Trial":{"args":[],"type":"{ uid : String.String, writtenWord : String.String, audioSentence : Data.AudioFile, context : String.String, amorce : String.String, feedback : String.String, isTraining : Basics.Bool }"},"Session3.Spelling3.Trial":{"args":[],"type":"{ uid : String.String, writtenWord : String.String, audioSentence : Data.AudioFile, isTraining : Basics.Bool }"},"Session3.Synonym.Trial":{"args":[],"type":"{ uid : String.String, target : String.String, pre : String.String, stimulus : String.String, post : String.String, isTraining : Basics.Bool, radical : String.String }"},"DnDList.DragElementId":{"args":[],"type":"String.String"},"DnDList.DragIndex":{"args":[],"type":"Basics.Int"},"DnDList.DropElementId":{"args":[],"type":"String.String"},"DnDList.DropIndex":{"args":[],"type":"Basics.Int"},"Browser.Dom.Element":{"args":[],"type":"{ scene : { width : Basics.Float, height : Basics.Float }, viewport : { x : Basics.Float, y : Basics.Float, width : Basics.Float, height : Basics.Float }, element : { x : Basics.Float, y : Basics.Float, width : Basics.Float, height : Basics.Float } }"},"DnDList.Position":{"args":[],"type":"{ x : Basics.Float, y : Basics.Float }"}},"unions":{"Main.Msg":{"args":[],"tags":{"UserToggledInCloudWords":["String.String"],"PlaysoundInJS":["String.String"],"UserClickedLink":["Browser.UrlRequest"],"BrowserChangedUrl":["Url.Url"],"NoOp":[],"Acceptability":["Pretest.Acceptability.Msg"],"Pretest":["Pretest.Pretest.Msg"],"SentenceCompletion":["Pretest.SentenceCompletion.Msg"],"ServerRespondedWithAllPretestData":["List.List Pretest.Acceptability.Trial","List.List ExperimentInfo.Task"],"SPR":["Pretest.SPR.Msg"],"VKS":["Pretest.VKS.Msg"],"CU1":["Session1.ContextUnderstanding.Msg"],"Presentation":["Session1.Presentation.Msg"],"Meaning":["Session1.Meaning.Msg"],"Spelling1":["Session1.Spelling.Msg"],"Session1":["Session1.Session.Msg"],"CU2":["Session2.CU2.CU2Msg"],"Spelling2":["Session2.Spelling.Msg"],"Translation":["Session2.Translation.Msg"],"Session2":["Session2.Session.Msg"],"CU3":["Session3.CU3.Msg"],"Spelling3":["Session3.Spelling3.Msg"],"YN":["Postest.YN.Msg"],"Synonym":["Session3.Synonym.Msg"],"Session3":["Session3.Session.Msg"]}},"Basics.Bool":{"args":[],"tags":{"True":[],"False":[]}},"Session2.CU2.CU2Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedRadioButton":["String.String"],"UserClickedStartMain":["List.List Session2.CU2.Trial","ExperimentInfo.Task"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedAudio":["String.String"],"RuntimeShuffledOptionsOrder":["List.List Basics.Int"],"UserClickedStartTraining":[]}},"Basics.Int":{"args":[],"tags":{"Int":[]}},"List.List":{"args":["a"],"tags":{}},"Maybe.Maybe":{"args":["a"],"tags":{"Just":["a"],"Nothing":[]}},"Postest.YN.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedStartIntro":["List.List Postest.YN.Trial"],"UserClickedStartMain":["List.List Postest.YN.Trial","ExperimentInfo.Task"],"UserChangedInput":["String.String"]}},"Pretest.Acceptability.Msg":{"args":[],"tags":{"UserPressedButton":["Maybe.Maybe Basics.Bool"],"UserPressedButtonWithTimestamp":["Maybe.Maybe Basics.Bool","Time.Posix"],"NextStepCinematic":["Pretest.Acceptability.Step"],"AudioEnded":["( String.String, Time.Posix )"],"AudioStarted":["( String.String, Time.Posix )"],"StartTraining":[],"UserClickedSaveMsg":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"StartMain":["List.List Pretest.Acceptability.Trial","ExperimentInfo.Task"],"RuntimeShuffledTrials":["List.List ExperimentInfo.Task","Result.Result ( Pretest.Acceptability.ErrorBlock, List.List Pretest.Acceptability.Trial ) (List.List (List.List Pretest.Acceptability.Trial))"]}},"Pretest.Pretest.Msg":{"args":[],"tags":{"ServerRespondedWithSomePretestData":["Pretest.Pretest.ParaMsg"],"ServerRespondedWithSomeError":["Http.Error"],"ServerRespondedWithAllPretestData":["List.List Pretest.SPR.Trial","List.List Pretest.SentenceCompletion.Trial","List.List ExperimentInfo.Task","List.List Pretest.VKS.Trial"],"StartPretest":["Pretest.Pretest.ShuffledPretest"]}},"Pretest.SPR.Msg":{"args":[],"tags":{"NoOp":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"StartMain":["ExperimentInfo.Task","List.List Pretest.SPR.Trial"],"TimestampedMsg":["Pretest.SPR.TimedMsg","Maybe.Maybe Time.Posix"],"UserClickedNextTrial":["Pretest.SPR.Answer"],"UserClickedSaveData":[],"UserConfirmedChoice":["Pretest.SPR.Answer"],"UserClickedStartTraining":[]}},"Pretest.SentenceCompletion.Msg":{"args":[],"tags":{"UserClickedToggleFeedback":[],"UserClickedNextTrial":[],"UserClickedStartMain":["ExperimentInfo.Task","List.List Pretest.SentenceCompletion.Trial"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserUpdatedField":["Pretest.SentenceCompletion.Field","String.String"],"RuntimeReordedAmorces":["Pretest.SentenceCompletion.Field"],"UserClickedStartTraining":[]}},"Pretest.VKS.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedStartMain":[],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserUpdatedField":["Pretest.VKS.Field","String.String"],"RuntimeReordedAmorces":["Pretest.VKS.Field"],"UserClickedNewKnowledge":["String.String"]}},"Session1.ContextUnderstanding.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedRadioButton":["String.String"],"UserClickedStartMain":["List.List Session1.ContextUnderstanding.Trial","ExperimentInfo.Task"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedStartTraining":[],"RuntimeShuffledOptionsOrder":["List.List Basics.Int"]}},"Session1.Meaning.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedRadioButton":["String.String"],"UserClickedStartMain":[],"SaveDataMsg":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedStartTraining":[],"RuntimeShuffledOptionsOrder":["List.List Basics.Int"]}},"Session1.Presentation.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedStartMain":["List.List Session1.Presentation.Trial","ExperimentInfo.Task"],"UserToggleElementOfEntry":["String.String"],"UserClickedStartAudio":["String.String"],"UserClickedStartTraining":[],"NoOp":[]}},"Session1.Session.Msg":{"args":[],"tags":{"ServerRespondedWithSomeData":["Session1.Session.LoadingMsg"],"ServerRespondedWithSomeError":["Http.Error"],"ServerRespondedWithAllData":["List.List Session1.Meaning.Trial","List.List Session1.Spelling.Trial","List.List Session1.ContextUnderstanding.Trial","List.List Session1.Presentation.Trial","List.List ExperimentInfo.Task"],"StartSession":["Session1.Session.ShuffledSession1"]}},"Session1.Spelling.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedFeedback":[],"UserClickedRadioButton":["String.String"],"UserClickedStartMainloop":[],"UserClickedSavedData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedPlayAudio":["String.String"],"UserClickedStartTraining":[]}},"Session2.Session.Msg":{"args":[],"tags":{"ServerRespondedWithSomeData":["Task.Parallel.Msg4 (List.List Session2.CU2.Trial) (List.List Session2.Spelling.Trial) (List.List Session2.Translation.Trial) (List.List ExperimentInfo.Task)"],"ServerRespondedWithAllData":["List.List Session2.CU2.Trial","List.List Session2.Spelling.Trial","List.List Session2.Translation.Trial","List.List ExperimentInfo.Task"],"ServerRespondedWithSomeError":["Http.Error"],"StartSession":["Session2.Session.ShuffledSession2"]}},"Session2.Spelling.Msg":{"args":[],"tags":{"UserDragsLetter":["DnDList.Msg"],"PlayAudio":["String.String"],"UserClickedFeedbackButton":[],"UserClickedNextTrial":["Maybe.Maybe Session2.Spelling.Trial"],"UserClickedStartMainloop":["List.List Session2.Spelling.Trial"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedStartTraining":[],"UserClickedStartAudio":["String.String"]}},"Session2.Translation.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedSaveData":[],"UserClickedRadioButton":["String.String"],"UserClickedStartTraining":[],"UserClickedStartMain":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"RuntimeShuffledOptionsOrder":["List.List Basics.Int"]}},"Session3.CU3.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedStartMain":[],"UserChangedInput":["String.String"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedStartTraining":[],"RuntimeShuffledOptionsOrder":["List.List Basics.Int"]}},"Session3.Session.Msg":{"args":[],"tags":{"ServerRespondedWithSomeSession3Data":["Task.Parallel.Msg4 (List.List Session3.CU3.Trial) (List.List Session3.Spelling3.Trial) (List.List Session3.Synonym.Trial) (List.List ExperimentInfo.Task)"],"ServerRespondedWithAllSession3Data":["List.List Session3.CU3.Trial","List.List Session3.Spelling3.Trial","List.List Session3.Synonym.Trial","List.List ExperimentInfo.Task"],"ServerRespondedWithSomeError":["Http.Error"],"StartSession":["Session3.Session.ShuffledSession3"]}},"Session3.Spelling3.Msg":{"args":[],"tags":{"UserClickedNextTrial":[],"UserClickedToggleFeedback":[],"UserClickedStartTraining":[],"UserClickedStartMain":[],"UserChangedInput":["String.String"],"UserClickedSaveData":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserClickedPlayAudio":["String.String"]}},"Session3.Synonym.Msg":{"args":[],"tags":{"UserClickedFeedback":[],"UserChangedInput":["String.String"],"UserClickedNextTrial":[],"UserClickedStartMainloop":[],"SaveDataMsg":[],"ServerRespondedWithLastRecords":["Result.Result Http.Error (List.List ())"],"UserCLickedStartTraining":[]}},"Url.Protocol":{"args":[],"tags":{"Http":[],"Https":[]}},"Pretest.Acceptability.SentenceType":{"args":[],"tags":{"EmbeddedQuestion":[],"ZeroArticle":[],"AdjectiveAgreement":[],"PresentPerfectOrSimplePast":[],"Conditional":[],"Question":[],"RelativeClause":[]}},"ExperimentInfo.Session":{"args":[],"tags":{"Session1":[],"Session2":[],"Session3":[],"Pretest":[],"Posttest":[],"OtherSession":[]}},"String.String":{"args":[],"tags":{"String":[]}},"Pretest.Acceptability.TrialType":{"args":[],"tags":{"Target":[],"Training":[],"Distractor":[]}},"ExperimentInfo.Type_":{"args":[],"tags":{"Sens":[],"Forme":[],"Context":[],"Other":[]}},"Browser.UrlRequest":{"args":[],"tags":{"Internal":["Url.Url"],"External":["String.String"]}},"Pretest.SPR.Answer":{"args":[],"tags":{"Yes":[],"No":[],"Unsure":[],"NoAnswerYet":[]}},"Http.Error":{"args":[],"tags":{"BadUrl":["String.String"],"Timeout":[],"NetworkError":[],"BadStatus":["Basics.Int"],"BadBody":["String.String"]}},"Pretest.Acceptability.ErrorBlock":{"args":[],"tags":{"FirstDistractorMissing":["Basics.Bool"],"SecondDistractorMissing":["Basics.Bool"],"ThirdDistractorMissing":["Basics.Bool"]}},"Pretest.SentenceCompletion.Field":{"args":[],"tags":{"FirstProduction":[],"SecondProduction":[]}},"Pretest.VKS.Field":{"args":[],"tags":{"FirstProduction":[],"SecondProduction":[]}},"DnDList.Msg":{"args":[],"tags":{"DragStart":["DnDList.DragIndex","DnDList.DragElementId","DnDList.Position"],"Drag":["DnDList.Position"],"DragOver":["DnDList.DropIndex","DnDList.DropElementId"],"DragEnter":["DnDList.DropIndex"],"DragLeave":[],"DragEnd":[],"GotDragElement":["Result.Result Browser.Dom.Error Browser.Dom.Element"],"GotDropElement":["Result.Result Browser.Dom.Error Browser.Dom.Element"]}},"Task.Parallel.Msg4":{"args":["a","b","c","d"],"tags":{"LoadedA4":["a"],"LoadedB4":["b"],"LoadedC4":["c"],"LoadedD4":["d"]}},"Task.Parallel.Msg5":{"args":["a","b","c","d","e"],"tags":{"LoadedA5":["a"],"LoadedB5":["b"],"LoadedC5":["c"],"LoadedD5":["d"],"LoadedE5":["e"]}},"Time.Posix":{"args":[],"tags":{"Posix":["Basics.Int"]}},"Result.Result":{"args":["error","value"],"tags":{"Ok":["value"],"Err":["error"]}},"Pretest.Acceptability.Step":{"args":[],"tags":{"Start":[],"Listening":[],"Answering":[],"End":[],"Init":[]}},"Pretest.SPR.Tag":{"args":[],"tags":{"NoUnit":[],"Critic":[],"SpillOver":[]}},"Pretest.SPR.TimedMsg":{"args":[],"tags":{"UserPressedSpaceToStartParagraph":[],"UserPressedSpaceToReadNextSegment":[]}},"Browser.Dom.Error":{"args":[],"tags":{"NotFound":["String.String"]}},"Basics.Float":{"args":[],"tags":{"Float":[]}}}}})}});

//////////////////// HMR BEGIN ////////////////////

/*
  MIT License http://www.opensource.org/licenses/mit-license.php
  Original Author: Flux Xu @fluxxu
*/

/*
    A note about the environment that this code runs in...

    assumed globals:
        - `module` (from Node.js module system and webpack)

    assumed in scope after injection into the Elm IIFE:
        - `scope` (has an 'Elm' property which contains the public Elm API)
        - various functions defined by Elm which we have to hook such as `_Platform_initialize` and `_Scheduler_binding`
 */

if (module.hot) {
    (function () {
        "use strict";

        //polyfill for IE: https://github.com/fluxxu/elm-hot-loader/issues/16
        if (typeof Object.assign != 'function') {
            Object.assign = function (target) {
                'use strict';
                if (target == null) {
                    throw new TypeError('Cannot convert undefined or null to object');
                }

                target = Object(target);
                for (var index = 1; index < arguments.length; index++) {
                    var source = arguments[index];
                    if (source != null) {
                        for (var key in source) {
                            if (Object.prototype.hasOwnProperty.call(source, key)) {
                                target[key] = source[key];
                            }
                        }
                    }
                }
                return target;
            };
        }

        // Elm 0.19.1 introduced a '$' prefix at the beginning of the symbols it emits,
        // and we check for `Maybe.Just` because we expect it to be present in all Elm programs.
        var elmVersion;
        if (typeof elm$core$Maybe$Just !== 'undefined')
            elmVersion = '0.19.0';
        else if (typeof $elm$core$Maybe$Just !== 'undefined')
            elmVersion = '0.19.1';
        else
            throw new Error("Could not determine Elm version");

        function elmSymbol(symbol) {
            try {
                switch (elmVersion) {
                    case '0.19.0':
                        return eval(symbol);
                    case '0.19.1':
                        return eval('$' + symbol);
                    default:
                        throw new Error('Cannot resolve ' + symbol + '. Elm version unknown!')
                }
            } catch (e) {
                if (e instanceof ReferenceError) {
                    return undefined;
                } else {
                    throw e;
                }
            }
        }

        var instances = module.hot.data
            ? module.hot.data.instances || {}
            : {};
        var uid = module.hot.data
            ? module.hot.data.uid || 0
            : 0;

        if (Object.keys(instances).length === 0) {
            log("[elm-hot] Enabled");
        }

        var cancellers = [];

        // These 2 variables act as dynamically-scoped variables which are set only when the
        // Elm module's hooked init function is called.
        var initializingInstance = null;
        var swappingInstance = null;

        module.hot.accept();
        module.hot.dispose(function (data) {
            data.instances = instances;
            data.uid = uid;

            // Cleanup pending async tasks

            // First, make sure that no new tasks can be started until we finish replacing the code
            _Scheduler_binding = function () {
                return _Scheduler_fail(new Error('[elm-hot] Inactive Elm instance.'))
            };

            // Second, kill pending tasks belonging to the old instance
            if (cancellers.length) {
                log('[elm-hot] Killing ' + cancellers.length + ' running processes...');
                try {
                    cancellers.forEach(function (cancel) {
                        cancel();
                    });
                } catch (e) {
                    console.warn('[elm-hot] Kill process error: ' + e.message);
                }
            }
        });

        function log(message) {
            if (module.hot.verbose) {
                console.log(message)
            }
        }

        function getId() {
            return ++uid;
        }

        function findPublicModules(parent, path) {
            var modules = [];
            for (var key in parent) {
                var child = parent[key];
                var currentPath = path ? path + '.' + key : key;
                if ('init' in child) {
                    modules.push({
                        path: currentPath,
                        module: child
                    });
                } else {
                    modules = modules.concat(findPublicModules(child, currentPath));
                }
            }
            return modules;
        }

        function registerInstance(domNode, flags, path, portSubscribes, portSends) {
            var id = getId();

            var instance = {
                id: id,
                path: path,
                domNode: domNode,
                flags: flags,
                portSubscribes: portSubscribes,
                portSends: portSends,
                lastState: null // last Elm app state (root model)
            };

            return instances[id] = instance
        }

        function isFullscreenApp() {
            // Returns true if the Elm app will take over the entire DOM body.
            return typeof elmSymbol("elm$browser$Browser$application") !== 'undefined'
                || typeof elmSymbol("elm$browser$Browser$document") !== 'undefined';
        }

        function wrapDomNode(node) {
            // When embedding an Elm app into a specific DOM node, Elm will replace the provided
            // DOM node with the Elm app's content. When the Elm app is compiled normally, the
            // original DOM node is reused (its attributes and content changes, but the object
            // in memory remains the same). But when compiled using `--debug`, Elm will completely
            // destroy the original DOM node and instead replace it with 2 brand new nodes: one
            // for your Elm app's content and the other for the Elm debugger UI. In this case,
            // if you held a reference to the DOM node provided for embedding, it would be orphaned
            // after Elm module initialization.
            //
            // So in order to make both cases consistent and isolate us from changes in how Elm
            // does this, we will insert a dummy node to wrap the node for embedding and hold
            // a reference to the dummy node.
            //
            // We will also put a tag on the dummy node so that the Elm developer knows who went
            // behind their back and rudely put stuff in their DOM.
            var dummyNode = document.createElement("div");
            dummyNode.setAttribute("data-elm-hot", "true");
            dummyNode.style.height = "inherit";
            var parentNode = node.parentNode;
            parentNode.replaceChild(dummyNode, node);
            dummyNode.appendChild(node);
            return dummyNode;
        }

        function wrapPublicModule(path, module) {
            var originalInit = module.init;
            if (originalInit) {
                module.init = function (args) {
                    var elm;
                    var portSubscribes = {};
                    var portSends = {};
                    var domNode = null;
                    var flags = null;
                    if (typeof args !== 'undefined') {
                        // normal case
                        domNode = args['node'] && !isFullscreenApp()
                            ? wrapDomNode(args['node'])
                            : document.body;
                        flags = args['flags'];
                    } else {
                        // rare case: Elm allows init to be called without any arguments at all
                        domNode = document.body;
                        flags = undefined
                    }
                    initializingInstance = registerInstance(domNode, flags, path, portSubscribes, portSends);
                    elm = originalInit(args);
                    wrapPorts(elm, portSubscribes, portSends);
                    initializingInstance = null;
                    return elm;
                };
            } else {
                console.error("Could not find a public module to wrap at path " + path)
            }
        }

        function swap(Elm, instance) {
            log('[elm-hot] Hot-swapping module: ' + instance.path);

            swappingInstance = instance;

            // remove from the DOM everything that had been created by the old Elm app
            var containerNode = instance.domNode;
            while (containerNode.lastChild) {
                containerNode.removeChild(containerNode.lastChild);
            }

            var m = getAt(instance.path.split('.'), Elm);
            var elm;
            if (m) {
                // prepare to initialize the new Elm module
                var args = {flags: instance.flags};
                if (containerNode === document.body) {
                    // fullscreen case: no additional args needed
                } else {
                    // embed case: provide a new node for Elm to use
                    var nodeForEmbed = document.createElement("div");
                    containerNode.appendChild(nodeForEmbed);
                    args['node'] = nodeForEmbed;
                }

                elm = m.init(args);

                Object.keys(instance.portSubscribes).forEach(function (portName) {
                    if (portName in elm.ports && 'subscribe' in elm.ports[portName]) {
                        var handlers = instance.portSubscribes[portName];
                        if (!handlers.length) {
                            return;
                        }
                        log('[elm-hot] Reconnect ' + handlers.length + ' handler(s) to port \''
                            + portName + '\' (' + instance.path + ').');
                        handlers.forEach(function (handler) {
                            elm.ports[portName].subscribe(handler);
                        });
                    } else {
                        delete instance.portSubscribes[portName];
                        log('[elm-hot] Port was removed: ' + portName);
                    }
                });

                Object.keys(instance.portSends).forEach(function (portName) {
                    if (portName in elm.ports && 'send' in elm.ports[portName]) {
                        log('[elm-hot] Replace old port send with the new send');
                        instance.portSends[portName] = elm.ports[portName].send;
                    } else {
                        delete instance.portSends[portName];
                        log('[elm-hot] Port was removed: ' + portName);
                    }
                });
            } else {
                log('[elm-hot] Module was removed: ' + instance.path);
            }

            swappingInstance = null;
        }

        function wrapPorts(elm, portSubscribes, portSends) {
            var portNames = Object.keys(elm.ports || {});
            //hook ports
            if (portNames.length) {
                // hook outgoing ports
                portNames
                    .filter(function (name) {
                        return 'subscribe' in elm.ports[name];
                    })
                    .forEach(function (portName) {
                        var port = elm.ports[portName];
                        var subscribe = port.subscribe;
                        var unsubscribe = port.unsubscribe;
                        elm.ports[portName] = Object.assign(port, {
                            subscribe: function (handler) {
                                log('[elm-hot] ports.' + portName + '.subscribe called.');
                                if (!portSubscribes[portName]) {
                                    portSubscribes[portName] = [handler];
                                } else {
                                    //TODO handle subscribing to single handler more than once?
                                    portSubscribes[portName].push(handler);
                                }
                                return subscribe.call(port, handler);
                            },
                            unsubscribe: function (handler) {
                                log('[elm-hot] ports.' + portName + '.unsubscribe called.');
                                var list = portSubscribes[portName];
                                if (list && list.indexOf(handler) !== -1) {
                                    list.splice(list.lastIndexOf(handler), 1);
                                } else {
                                    console.warn('[elm-hot] ports.' + portName + '.unsubscribe: handler not subscribed');
                                }
                                return unsubscribe.call(port, handler);
                            }
                        });
                    });

                // hook incoming ports
                portNames
                    .filter(function (name) {
                        return 'send' in elm.ports[name];
                    })
                    .forEach(function (portName) {
                        var port = elm.ports[portName];
                        portSends[portName] = port.send;
                        elm.ports[portName] = Object.assign(port, {
                            send: function (val) {
                                return portSends[portName].call(port, val);
                            }
                        });
                    });
            }
            return portSubscribes;
        }

        /*
        Breadth-first search for a `Browser.Navigation.Key` in the user's app model.
        Returns the key and keypath or null if not found.
        */
        function findNavKey(rootModel) {
            var queue = [];
            if (isDebuggerModel(rootModel)) {
                /*
                 Extract the user's app model from the Elm Debugger's model. The Elm debugger
                 can hold multiple references to the user's model (e.g. in its "history"). So
                 we must be careful to only search within the "state" part of the Debugger.
                */
                queue.push({value: rootModel['state'], keypath: ['state']});
            } else {
                queue.push({value: rootModel, keypath: []});
            }

            while (queue.length !== 0) {
                var item = queue.shift();

                if (typeof item.value === "undefined" || item.value === null) {
                    continue;
                }

                // The nav key is identified by a runtime tag added by the elm-hot injector.
                if (item.value.hasOwnProperty("elm-hot-nav-key")) {
                    // found it!
                    return item;
                }

                if (typeof item.value !== "object") {
                    continue;
                }

                for (var propName in item.value) {
                    if (!item.value.hasOwnProperty(propName)) continue;
                    var newKeypath = item.keypath.slice();
                    newKeypath.push(propName);
                    queue.push({value: item.value[propName], keypath: newKeypath})
                }
            }

            return null;
        }


        function isDebuggerModel(model) {
            // Up until elm/browser 1.0.2, the Elm debugger could be identified by a
            // property named "expando". But in version 1.0.2 that was renamed to "expandoModel"
            return model
                && (model.hasOwnProperty("expando") || model.hasOwnProperty("expandoModel"))
                && model.hasOwnProperty("state");
        }

        function getAt(keyPath, obj) {
            return keyPath.reduce(function (xs, x) {
                return (xs && xs[x]) ? xs[x] : null
            }, obj)
        }

        function removeNavKeyListeners(navKey) {
            window.removeEventListener('popstate', navKey.value);
            window.navigator.userAgent.indexOf('Trident') < 0 || window.removeEventListener('hashchange', navKey.value);
        }

        // hook program creation
        var initialize = _Platform_initialize;
        _Platform_initialize = function (flagDecoder, args, init, update, subscriptions, stepperBuilder) {
            var instance = initializingInstance || swappingInstance;
            var tryFirstRender = !!swappingInstance;

            var hookedInit = function (args) {
                var initialStateTuple = init(args);
                if (swappingInstance) {
                    var oldModel = swappingInstance.lastState;
                    var newModel = initialStateTuple.a;

                    if (typeof elmSymbol("elm$browser$Browser$application") !== 'undefined') {
                        var oldKeyLoc = findNavKey(oldModel);

                        // attempt to find the Browser.Navigation.Key in the newly-constructed model
                        // and bring it along with the rest of the old data.
                        var newKeyLoc = findNavKey(newModel);
                        var error = null;
                        if (newKeyLoc === null) {
                            error = "could not find Browser.Navigation.Key in the new app model";
                        } else if (oldKeyLoc === null) {
                            error = "could not find Browser.Navigation.Key in the old app model.";
                        } else if (newKeyLoc.keypath.toString() !== oldKeyLoc.keypath.toString()) {
                            error = "the location of the Browser.Navigation.Key in the model has changed.";
                        } else {
                            // remove event listeners attached to the old nav key
                            removeNavKeyListeners(oldKeyLoc.value);

                            // insert the new nav key into the old model in the exact same location
                            var parentKeyPath = oldKeyLoc.keypath.slice(0, -1);
                            var lastSegment = oldKeyLoc.keypath.slice(-1)[0];
                            var oldParent = getAt(parentKeyPath, oldModel);
                            oldParent[lastSegment] = newKeyLoc.value;
                        }

                        if (error !== null) {
                            console.error("[elm-hot] Hot-swapping " + instance.path + " not possible: " + error);
                            oldModel = newModel;
                        }
                    }

                    // the heart of the app state hot-swap
                    initialStateTuple.a = oldModel;

                    // ignore any Cmds returned by the init during hot-swap
                    initialStateTuple.b = elmSymbol("elm$core$Platform$Cmd$none");
                } else {
                    // capture the initial state for later
                    initializingInstance.lastState = initialStateTuple.a;
                }

                return initialStateTuple
            };

            var hookedStepperBuilder = function (sendToApp, model) {
                var result;
                // first render may fail if shape of model changed too much
                if (tryFirstRender) {
                    tryFirstRender = false;
                    try {
                        result = stepperBuilder(sendToApp, model)
                    } catch (e) {
                        throw new Error('[elm-hot] Hot-swapping ' + instance.path +
                            ' is not possible, please reload page. Error: ' + e.message)
                    }
                } else {
                    result = stepperBuilder(sendToApp, model)
                }

                return function (nextModel, isSync) {
                    if (instance) {
                        // capture the state after every step so that later we can restore from it during a hot-swap
                        instance.lastState = nextModel
                    }
                    return result(nextModel, isSync)
                }
            };

            return initialize(flagDecoder, args, hookedInit, update, subscriptions, hookedStepperBuilder)
        };

        // hook process creation
        var originalBinding = _Scheduler_binding;
        _Scheduler_binding = function (originalCallback) {
            return originalBinding(function () {
                // start the scheduled process, which may return a cancellation function.
                var cancel = originalCallback.apply(this, arguments);
                if (cancel) {
                    cancellers.push(cancel);
                    return function () {
                        cancellers.splice(cancellers.indexOf(cancel), 1);
                        return cancel();
                    };
                }
                return cancel;
            });
        };

        scope['_elm_hot_loader_init'] = function (Elm) {
            // swap instances
            var removedInstances = [];
            for (var id in instances) {
                var instance = instances[id];
                if (instance.domNode.parentNode) {
                    swap(Elm, instance);
                } else {
                    removedInstances.push(id);
                }
            }

            removedInstances.forEach(function (id) {
                delete instance[id];
            });

            // wrap all public modules
            var publicModules = findPublicModules(Elm);
            publicModules.forEach(function (m) {
                wrapPublicModule(m.path, m.module);
            });
        }
    })();

    scope['_elm_hot_loader_init'](scope['Elm']);
}
//////////////////// HMR END ////////////////////


}(this));
},{}],"node_modules/parcel-bundler/src/builtins/bundle-url.js":[function(require,module,exports) {
var bundleURL = null;

function getBundleURLCached() {
  if (!bundleURL) {
    bundleURL = getBundleURL();
  }

  return bundleURL;
}

function getBundleURL() {
  // Attempt to find the URL of the current script and use that as the base URL
  try {
    throw new Error();
  } catch (err) {
    var matches = ('' + err.stack).match(/(https?|file|ftp|chrome-extension|moz-extension):\/\/[^)\n]+/g);

    if (matches) {
      return getBaseURL(matches[0]);
    }
  }

  return '/';
}

function getBaseURL(url) {
  return ('' + url).replace(/^((?:https?|file|ftp|chrome-extension|moz-extension):\/\/.+)?\/[^/]+(?:\?.*)?$/, '$1') + '/';
}

exports.getBundleURL = getBundleURLCached;
exports.getBaseURL = getBaseURL;
},{}],"node_modules/parcel-bundler/src/builtins/css-loader.js":[function(require,module,exports) {
var bundle = require('./bundle-url');

function updateLink(link) {
  var newLink = link.cloneNode();

  newLink.onload = function () {
    link.remove();
  };

  newLink.href = link.href.split('?')[0] + '?' + Date.now();
  link.parentNode.insertBefore(newLink, link.nextSibling);
}

var cssTimeout = null;

function reloadCSS() {
  if (cssTimeout) {
    return;
  }

  cssTimeout = setTimeout(function () {
    var links = document.querySelectorAll('link[rel="stylesheet"]');

    for (var i = 0; i < links.length; i++) {
      if (bundle.getBaseURL(links[i].href) === bundle.getBundleURL()) {
        updateLink(links[i]);
      }
    }

    cssTimeout = null;
  }, 50);
}

module.exports = reloadCSS;
},{"./bundle-url":"node_modules/parcel-bundler/src/builtins/bundle-url.js"}],"scss/style.scss":[function(require,module,exports) {
var reloadCSS = require('_css_loader');

module.hot.dispose(reloadCSS);
module.hot.accept(reloadCSS);
},{"_css_loader":"node_modules/parcel-bundler/src/builtins/css-loader.js"}],"node_modules/howler/dist/howler.js":[function(require,module,exports) {
var define;
var global = arguments[3];
/*!
 *  howler.js v2.2.1
 *  howlerjs.com
 *
 *  (c) 2013-2020, James Simpson of GoldFire Studios
 *  goldfirestudios.com
 *
 *  MIT License
 */

(function() {

  'use strict';

  /** Global Methods **/
  /***************************************************************************/

  /**
   * Create the global controller. All contained methods and properties apply
   * to all sounds that are currently playing or will be in the future.
   */
  var HowlerGlobal = function() {
    this.init();
  };
  HowlerGlobal.prototype = {
    /**
     * Initialize the global Howler object.
     * @return {Howler}
     */
    init: function() {
      var self = this || Howler;

      // Create a global ID counter.
      self._counter = 1000;

      // Pool of unlocked HTML5 Audio objects.
      self._html5AudioPool = [];
      self.html5PoolSize = 10;

      // Internal properties.
      self._codecs = {};
      self._howls = [];
      self._muted = false;
      self._volume = 1;
      self._canPlayEvent = 'canplaythrough';
      self._navigator = (typeof window !== 'undefined' && window.navigator) ? window.navigator : null;

      // Public properties.
      self.masterGain = null;
      self.noAudio = false;
      self.usingWebAudio = true;
      self.autoSuspend = true;
      self.ctx = null;

      // Set to false to disable the auto audio unlocker.
      self.autoUnlock = true;

      // Setup the various state values for global tracking.
      self._setup();

      return self;
    },

    /**
     * Get/set the global volume for all sounds.
     * @param  {Float} vol Volume from 0.0 to 1.0.
     * @return {Howler/Float}     Returns self or current volume.
     */
    volume: function(vol) {
      var self = this || Howler;
      vol = parseFloat(vol);

      // If we don't have an AudioContext created yet, run the setup.
      if (!self.ctx) {
        setupAudioContext();
      }

      if (typeof vol !== 'undefined' && vol >= 0 && vol <= 1) {
        self._volume = vol;

        // Don't update any of the nodes if we are muted.
        if (self._muted) {
          return self;
        }

        // When using Web Audio, we just need to adjust the master gain.
        if (self.usingWebAudio) {
          self.masterGain.gain.setValueAtTime(vol, Howler.ctx.currentTime);
        }

        // Loop through and change volume for all HTML5 audio nodes.
        for (var i=0; i<self._howls.length; i++) {
          if (!self._howls[i]._webAudio) {
            // Get all of the sounds in this Howl group.
            var ids = self._howls[i]._getSoundIds();

            // Loop through all sounds and change the volumes.
            for (var j=0; j<ids.length; j++) {
              var sound = self._howls[i]._soundById(ids[j]);

              if (sound && sound._node) {
                sound._node.volume = sound._volume * vol;
              }
            }
          }
        }

        return self;
      }

      return self._volume;
    },

    /**
     * Handle muting and unmuting globally.
     * @param  {Boolean} muted Is muted or not.
     */
    mute: function(muted) {
      var self = this || Howler;

      // If we don't have an AudioContext created yet, run the setup.
      if (!self.ctx) {
        setupAudioContext();
      }

      self._muted = muted;

      // With Web Audio, we just need to mute the master gain.
      if (self.usingWebAudio) {
        self.masterGain.gain.setValueAtTime(muted ? 0 : self._volume, Howler.ctx.currentTime);
      }

      // Loop through and mute all HTML5 Audio nodes.
      for (var i=0; i<self._howls.length; i++) {
        if (!self._howls[i]._webAudio) {
          // Get all of the sounds in this Howl group.
          var ids = self._howls[i]._getSoundIds();

          // Loop through all sounds and mark the audio node as muted.
          for (var j=0; j<ids.length; j++) {
            var sound = self._howls[i]._soundById(ids[j]);

            if (sound && sound._node) {
              sound._node.muted = (muted) ? true : sound._muted;
            }
          }
        }
      }

      return self;
    },

    /**
     * Handle stopping all sounds globally.
     */
    stop: function() {
      var self = this || Howler;

      // Loop through all Howls and stop them.
      for (var i=0; i<self._howls.length; i++) {
        self._howls[i].stop();
      }

      return self;
    },

    /**
     * Unload and destroy all currently loaded Howl objects.
     * @return {Howler}
     */
    unload: function() {
      var self = this || Howler;

      for (var i=self._howls.length-1; i>=0; i--) {
        self._howls[i].unload();
      }

      // Create a new AudioContext to make sure it is fully reset.
      if (self.usingWebAudio && self.ctx && typeof self.ctx.close !== 'undefined') {
        self.ctx.close();
        self.ctx = null;
        setupAudioContext();
      }

      return self;
    },

    /**
     * Check for codec support of specific extension.
     * @param  {String} ext Audio file extention.
     * @return {Boolean}
     */
    codecs: function(ext) {
      return (this || Howler)._codecs[ext.replace(/^x-/, '')];
    },

    /**
     * Setup various state values for global tracking.
     * @return {Howler}
     */
    _setup: function() {
      var self = this || Howler;

      // Keeps track of the suspend/resume state of the AudioContext.
      self.state = self.ctx ? self.ctx.state || 'suspended' : 'suspended';

      // Automatically begin the 30-second suspend process
      self._autoSuspend();

      // Check if audio is available.
      if (!self.usingWebAudio) {
        // No audio is available on this system if noAudio is set to true.
        if (typeof Audio !== 'undefined') {
          try {
            var test = new Audio();

            // Check if the canplaythrough event is available.
            if (typeof test.oncanplaythrough === 'undefined') {
              self._canPlayEvent = 'canplay';
            }
          } catch(e) {
            self.noAudio = true;
          }
        } else {
          self.noAudio = true;
        }
      }

      // Test to make sure audio isn't disabled in Internet Explorer.
      try {
        var test = new Audio();
        if (test.muted) {
          self.noAudio = true;
        }
      } catch (e) {}

      // Check for supported codecs.
      if (!self.noAudio) {
        self._setupCodecs();
      }

      return self;
    },

    /**
     * Check for browser support for various codecs and cache the results.
     * @return {Howler}
     */
    _setupCodecs: function() {
      var self = this || Howler;
      var audioTest = null;

      // Must wrap in a try/catch because IE11 in server mode throws an error.
      try {
        audioTest = (typeof Audio !== 'undefined') ? new Audio() : null;
      } catch (err) {
        return self;
      }

      if (!audioTest || typeof audioTest.canPlayType !== 'function') {
        return self;
      }

      var mpegTest = audioTest.canPlayType('audio/mpeg;').replace(/^no$/, '');

      // Opera version <33 has mixed MP3 support, so we need to check for and block it.
      var checkOpera = self._navigator && self._navigator.userAgent.match(/OPR\/([0-6].)/g);
      var isOldOpera = (checkOpera && parseInt(checkOpera[0].split('/')[1], 10) < 33);

      self._codecs = {
        mp3: !!(!isOldOpera && (mpegTest || audioTest.canPlayType('audio/mp3;').replace(/^no$/, ''))),
        mpeg: !!mpegTest,
        opus: !!audioTest.canPlayType('audio/ogg; codecs="opus"').replace(/^no$/, ''),
        ogg: !!audioTest.canPlayType('audio/ogg; codecs="vorbis"').replace(/^no$/, ''),
        oga: !!audioTest.canPlayType('audio/ogg; codecs="vorbis"').replace(/^no$/, ''),
        wav: !!(audioTest.canPlayType('audio/wav; codecs="1"') || audioTest.canPlayType('audio/wav')).replace(/^no$/, ''),
        aac: !!audioTest.canPlayType('audio/aac;').replace(/^no$/, ''),
        caf: !!audioTest.canPlayType('audio/x-caf;').replace(/^no$/, ''),
        m4a: !!(audioTest.canPlayType('audio/x-m4a;') || audioTest.canPlayType('audio/m4a;') || audioTest.canPlayType('audio/aac;')).replace(/^no$/, ''),
        m4b: !!(audioTest.canPlayType('audio/x-m4b;') || audioTest.canPlayType('audio/m4b;') || audioTest.canPlayType('audio/aac;')).replace(/^no$/, ''),
        mp4: !!(audioTest.canPlayType('audio/x-mp4;') || audioTest.canPlayType('audio/mp4;') || audioTest.canPlayType('audio/aac;')).replace(/^no$/, ''),
        weba: !!audioTest.canPlayType('audio/webm; codecs="vorbis"').replace(/^no$/, ''),
        webm: !!audioTest.canPlayType('audio/webm; codecs="vorbis"').replace(/^no$/, ''),
        dolby: !!audioTest.canPlayType('audio/mp4; codecs="ec-3"').replace(/^no$/, ''),
        flac: !!(audioTest.canPlayType('audio/x-flac;') || audioTest.canPlayType('audio/flac;')).replace(/^no$/, '')
      };

      return self;
    },

    /**
     * Some browsers/devices will only allow audio to be played after a user interaction.
     * Attempt to automatically unlock audio on the first user interaction.
     * Concept from: http://paulbakaus.com/tutorials/html5/web-audio-on-ios/
     * @return {Howler}
     */
    _unlockAudio: function() {
      var self = this || Howler;

      // Only run this if Web Audio is supported and it hasn't already been unlocked.
      if (self._audioUnlocked || !self.ctx) {
        return;
      }

      self._audioUnlocked = false;
      self.autoUnlock = false;

      // Some mobile devices/platforms have distortion issues when opening/closing tabs and/or web views.
      // Bugs in the browser (especially Mobile Safari) can cause the sampleRate to change from 44100 to 48000.
      // By calling Howler.unload(), we create a new AudioContext with the correct sampleRate.
      if (!self._mobileUnloaded && self.ctx.sampleRate !== 44100) {
        self._mobileUnloaded = true;
        self.unload();
      }

      // Scratch buffer for enabling iOS to dispose of web audio buffers correctly, as per:
      // http://stackoverflow.com/questions/24119684
      self._scratchBuffer = self.ctx.createBuffer(1, 1, 22050);

      // Call this method on touch start to create and play a buffer,
      // then check if the audio actually played to determine if
      // audio has now been unlocked on iOS, Android, etc.
      var unlock = function(e) {
        // Create a pool of unlocked HTML5 Audio objects that can
        // be used for playing sounds without user interaction. HTML5
        // Audio objects must be individually unlocked, as opposed
        // to the WebAudio API which only needs a single activation.
        // This must occur before WebAudio setup or the source.onended
        // event will not fire.
        while (self._html5AudioPool.length < self.html5PoolSize) {
          try {
            var audioNode = new Audio();

            // Mark this Audio object as unlocked to ensure it can get returned
            // to the unlocked pool when released.
            audioNode._unlocked = true;

            // Add the audio node to the pool.
            self._releaseHtml5Audio(audioNode);
          } catch (e) {
            self.noAudio = true;
            break;
          }
        }

        // Loop through any assigned audio nodes and unlock them.
        for (var i=0; i<self._howls.length; i++) {
          if (!self._howls[i]._webAudio) {
            // Get all of the sounds in this Howl group.
            var ids = self._howls[i]._getSoundIds();

            // Loop through all sounds and unlock the audio nodes.
            for (var j=0; j<ids.length; j++) {
              var sound = self._howls[i]._soundById(ids[j]);

              if (sound && sound._node && !sound._node._unlocked) {
                sound._node._unlocked = true;
                sound._node.load();
              }
            }
          }
        }

        // Fix Android can not play in suspend state.
        self._autoResume();

        // Create an empty buffer.
        var source = self.ctx.createBufferSource();
        source.buffer = self._scratchBuffer;
        source.connect(self.ctx.destination);

        // Play the empty buffer.
        if (typeof source.start === 'undefined') {
          source.noteOn(0);
        } else {
          source.start(0);
        }

        // Calling resume() on a stack initiated by user gesture is what actually unlocks the audio on Android Chrome >= 55.
        if (typeof self.ctx.resume === 'function') {
          self.ctx.resume();
        }

        // Setup a timeout to check that we are unlocked on the next event loop.
        source.onended = function() {
          source.disconnect(0);

          // Update the unlocked state and prevent this check from happening again.
          self._audioUnlocked = true;

          // Remove the touch start listener.
          document.removeEventListener('touchstart', unlock, true);
          document.removeEventListener('touchend', unlock, true);
          document.removeEventListener('click', unlock, true);

          // Let all sounds know that audio has been unlocked.
          for (var i=0; i<self._howls.length; i++) {
            self._howls[i]._emit('unlock');
          }
        };
      };

      // Setup a touch start listener to attempt an unlock in.
      document.addEventListener('touchstart', unlock, true);
      document.addEventListener('touchend', unlock, true);
      document.addEventListener('click', unlock, true);

      return self;
    },

    /**
     * Get an unlocked HTML5 Audio object from the pool. If none are left,
     * return a new Audio object and throw a warning.
     * @return {Audio} HTML5 Audio object.
     */
    _obtainHtml5Audio: function() {
      var self = this || Howler;

      // Return the next object from the pool if one exists.
      if (self._html5AudioPool.length) {
        return self._html5AudioPool.pop();
      }

      //.Check if the audio is locked and throw a warning.
      var testPlay = new Audio().play();
      if (testPlay && typeof Promise !== 'undefined' && (testPlay instanceof Promise || typeof testPlay.then === 'function')) {
        testPlay.catch(function() {
          console.warn('HTML5 Audio pool exhausted, returning potentially locked audio object.');
        });
      }

      return new Audio();
    },

    /**
     * Return an activated HTML5 Audio object to the pool.
     * @return {Howler}
     */
    _releaseHtml5Audio: function(audio) {
      var self = this || Howler;

      // Don't add audio to the pool if we don't know if it has been unlocked.
      if (audio._unlocked) {
        self._html5AudioPool.push(audio);
      }

      return self;
    },

    /**
     * Automatically suspend the Web Audio AudioContext after no sound has played for 30 seconds.
     * This saves processing/energy and fixes various browser-specific bugs with audio getting stuck.
     * @return {Howler}
     */
    _autoSuspend: function() {
      var self = this;

      if (!self.autoSuspend || !self.ctx || typeof self.ctx.suspend === 'undefined' || !Howler.usingWebAudio) {
        return;
      }

      // Check if any sounds are playing.
      for (var i=0; i<self._howls.length; i++) {
        if (self._howls[i]._webAudio) {
          for (var j=0; j<self._howls[i]._sounds.length; j++) {
            if (!self._howls[i]._sounds[j]._paused) {
              return self;
            }
          }
        }
      }

      if (self._suspendTimer) {
        clearTimeout(self._suspendTimer);
      }

      // If no sound has played after 30 seconds, suspend the context.
      self._suspendTimer = setTimeout(function() {
        if (!self.autoSuspend) {
          return;
        }

        self._suspendTimer = null;
        self.state = 'suspending';

        // Handle updating the state of the audio context after suspending.
        var handleSuspension = function() {
          self.state = 'suspended';

          if (self._resumeAfterSuspend) {
            delete self._resumeAfterSuspend;
            self._autoResume();
          }
        };

        // Either the state gets suspended or it is interrupted.
        // Either way, we need to update the state to suspended.
        self.ctx.suspend().then(handleSuspension, handleSuspension);
      }, 30000);

      return self;
    },

    /**
     * Automatically resume the Web Audio AudioContext when a new sound is played.
     * @return {Howler}
     */
    _autoResume: function() {
      var self = this;

      if (!self.ctx || typeof self.ctx.resume === 'undefined' || !Howler.usingWebAudio) {
        return;
      }

      if (self.state === 'running' && self.ctx.state !== 'interrupted' && self._suspendTimer) {
        clearTimeout(self._suspendTimer);
        self._suspendTimer = null;
      } else if (self.state === 'suspended' || self.state === 'running' && self.ctx.state === 'interrupted') {
        self.ctx.resume().then(function() {
          self.state = 'running';

          // Emit to all Howls that the audio has resumed.
          for (var i=0; i<self._howls.length; i++) {
            self._howls[i]._emit('resume');
          }
        });

        if (self._suspendTimer) {
          clearTimeout(self._suspendTimer);
          self._suspendTimer = null;
        }
      } else if (self.state === 'suspending') {
        self._resumeAfterSuspend = true;
      }

      return self;
    }
  };

  // Setup the global audio controller.
  var Howler = new HowlerGlobal();

  /** Group Methods **/
  /***************************************************************************/

  /**
   * Create an audio group controller.
   * @param {Object} o Passed in properties for this group.
   */
  var Howl = function(o) {
    var self = this;

    // Throw an error if no source is provided.
    if (!o.src || o.src.length === 0) {
      console.error('An array of source files must be passed with any new Howl.');
      return;
    }

    self.init(o);
  };
  Howl.prototype = {
    /**
     * Initialize a new Howl group object.
     * @param  {Object} o Passed in properties for this group.
     * @return {Howl}
     */
    init: function(o) {
      var self = this;

      // If we don't have an AudioContext created yet, run the setup.
      if (!Howler.ctx) {
        setupAudioContext();
      }

      // Setup user-defined default properties.
      self._autoplay = o.autoplay || false;
      self._format = (typeof o.format !== 'string') ? o.format : [o.format];
      self._html5 = o.html5 || false;
      self._muted = o.mute || false;
      self._loop = o.loop || false;
      self._pool = o.pool || 5;
      self._preload = (typeof o.preload === 'boolean' || o.preload === 'metadata') ? o.preload : true;
      self._rate = o.rate || 1;
      self._sprite = o.sprite || {};
      self._src = (typeof o.src !== 'string') ? o.src : [o.src];
      self._volume = o.volume !== undefined ? o.volume : 1;
      self._xhr = {
        method: o.xhr && o.xhr.method ? o.xhr.method : 'GET',
        headers: o.xhr && o.xhr.headers ? o.xhr.headers : null,
        withCredentials: o.xhr && o.xhr.withCredentials ? o.xhr.withCredentials : false,
      };

      // Setup all other default properties.
      self._duration = 0;
      self._state = 'unloaded';
      self._sounds = [];
      self._endTimers = {};
      self._queue = [];
      self._playLock = false;

      // Setup event listeners.
      self._onend = o.onend ? [{fn: o.onend}] : [];
      self._onfade = o.onfade ? [{fn: o.onfade}] : [];
      self._onload = o.onload ? [{fn: o.onload}] : [];
      self._onloaderror = o.onloaderror ? [{fn: o.onloaderror}] : [];
      self._onplayerror = o.onplayerror ? [{fn: o.onplayerror}] : [];
      self._onpause = o.onpause ? [{fn: o.onpause}] : [];
      self._onplay = o.onplay ? [{fn: o.onplay}] : [];
      self._onstop = o.onstop ? [{fn: o.onstop}] : [];
      self._onmute = o.onmute ? [{fn: o.onmute}] : [];
      self._onvolume = o.onvolume ? [{fn: o.onvolume}] : [];
      self._onrate = o.onrate ? [{fn: o.onrate}] : [];
      self._onseek = o.onseek ? [{fn: o.onseek}] : [];
      self._onunlock = o.onunlock ? [{fn: o.onunlock}] : [];
      self._onresume = [];

      // Web Audio or HTML5 Audio?
      self._webAudio = Howler.usingWebAudio && !self._html5;

      // Automatically try to enable audio.
      if (typeof Howler.ctx !== 'undefined' && Howler.ctx && Howler.autoUnlock) {
        Howler._unlockAudio();
      }

      // Keep track of this Howl group in the global controller.
      Howler._howls.push(self);

      // If they selected autoplay, add a play event to the load queue.
      if (self._autoplay) {
        self._queue.push({
          event: 'play',
          action: function() {
            self.play();
          }
        });
      }

      // Load the source file unless otherwise specified.
      if (self._preload && self._preload !== 'none') {
        self.load();
      }

      return self;
    },

    /**
     * Load the audio file.
     * @return {Howler}
     */
    load: function() {
      var self = this;
      var url = null;

      // If no audio is available, quit immediately.
      if (Howler.noAudio) {
        self._emit('loaderror', null, 'No audio support.');
        return;
      }

      // Make sure our source is in an array.
      if (typeof self._src === 'string') {
        self._src = [self._src];
      }

      // Loop through the sources and pick the first one that is compatible.
      for (var i=0; i<self._src.length; i++) {
        var ext, str;

        if (self._format && self._format[i]) {
          // If an extension was specified, use that instead.
          ext = self._format[i];
        } else {
          // Make sure the source is a string.
          str = self._src[i];
          if (typeof str !== 'string') {
            self._emit('loaderror', null, 'Non-string found in selected audio sources - ignoring.');
            continue;
          }

          // Extract the file extension from the URL or base64 data URI.
          ext = /^data:audio\/([^;,]+);/i.exec(str);
          if (!ext) {
            ext = /\.([^.]+)$/.exec(str.split('?', 1)[0]);
          }

          if (ext) {
            ext = ext[1].toLowerCase();
          }
        }

        // Log a warning if no extension was found.
        if (!ext) {
          console.warn('No file extension was found. Consider using the "format" property or specify an extension.');
        }

        // Check if this extension is available.
        if (ext && Howler.codecs(ext)) {
          url = self._src[i];
          break;
        }
      }

      if (!url) {
        self._emit('loaderror', null, 'No codec support for selected audio sources.');
        return;
      }

      self._src = url;
      self._state = 'loading';

      // If the hosting page is HTTPS and the source isn't,
      // drop down to HTML5 Audio to avoid Mixed Content errors.
      if (window.location.protocol === 'https:' && url.slice(0, 5) === 'http:') {
        self._html5 = true;
        self._webAudio = false;
      }

      // Create a new sound object and add it to the pool.
      new Sound(self);

      // Load and decode the audio data for playback.
      if (self._webAudio) {
        loadBuffer(self);
      }

      return self;
    },

    /**
     * Play a sound or resume previous playback.
     * @param  {String/Number} sprite   Sprite name for sprite playback or sound id to continue previous.
     * @param  {Boolean} internal Internal Use: true prevents event firing.
     * @return {Number}          Sound ID.
     */
    play: function(sprite, internal) {
      var self = this;
      var id = null;

      // Determine if a sprite, sound id or nothing was passed
      if (typeof sprite === 'number') {
        id = sprite;
        sprite = null;
      } else if (typeof sprite === 'string' && self._state === 'loaded' && !self._sprite[sprite]) {
        // If the passed sprite doesn't exist, do nothing.
        return null;
      } else if (typeof sprite === 'undefined') {
        // Use the default sound sprite (plays the full audio length).
        sprite = '__default';

        // Check if there is a single paused sound that isn't ended.
        // If there is, play that sound. If not, continue as usual.
        if (!self._playLock) {
          var num = 0;
          for (var i=0; i<self._sounds.length; i++) {
            if (self._sounds[i]._paused && !self._sounds[i]._ended) {
              num++;
              id = self._sounds[i]._id;
            }
          }

          if (num === 1) {
            sprite = null;
          } else {
            id = null;
          }
        }
      }

      // Get the selected node, or get one from the pool.
      var sound = id ? self._soundById(id) : self._inactiveSound();

      // If the sound doesn't exist, do nothing.
      if (!sound) {
        return null;
      }

      // Select the sprite definition.
      if (id && !sprite) {
        sprite = sound._sprite || '__default';
      }

      // If the sound hasn't loaded, we must wait to get the audio's duration.
      // We also need to wait to make sure we don't run into race conditions with
      // the order of function calls.
      if (self._state !== 'loaded') {
        // Set the sprite value on this sound.
        sound._sprite = sprite;

        // Mark this sound as not ended in case another sound is played before this one loads.
        sound._ended = false;

        // Add the sound to the queue to be played on load.
        var soundId = sound._id;
        self._queue.push({
          event: 'play',
          action: function() {
            self.play(soundId);
          }
        });

        return soundId;
      }

      // Don't play the sound if an id was passed and it is already playing.
      if (id && !sound._paused) {
        // Trigger the play event, in order to keep iterating through queue.
        if (!internal) {
          self._loadQueue('play');
        }

        return sound._id;
      }

      // Make sure the AudioContext isn't suspended, and resume it if it is.
      if (self._webAudio) {
        Howler._autoResume();
      }

      // Determine how long to play for and where to start playing.
      var seek = Math.max(0, sound._seek > 0 ? sound._seek : self._sprite[sprite][0] / 1000);
      var duration = Math.max(0, ((self._sprite[sprite][0] + self._sprite[sprite][1]) / 1000) - seek);
      var timeout = (duration * 1000) / Math.abs(sound._rate);
      var start = self._sprite[sprite][0] / 1000;
      var stop = (self._sprite[sprite][0] + self._sprite[sprite][1]) / 1000;
      sound._sprite = sprite;

      // Mark the sound as ended instantly so that this async playback
      // doesn't get grabbed by another call to play while this one waits to start.
      sound._ended = false;

      // Update the parameters of the sound.
      var setParams = function() {
        sound._paused = false;
        sound._seek = seek;
        sound._start = start;
        sound._stop = stop;
        sound._loop = !!(sound._loop || self._sprite[sprite][2]);
      };

      // End the sound instantly if seek is at the end.
      if (seek >= stop) {
        self._ended(sound);
        return;
      }

      // Begin the actual playback.
      var node = sound._node;
      if (self._webAudio) {
        // Fire this when the sound is ready to play to begin Web Audio playback.
        var playWebAudio = function() {
          self._playLock = false;
          setParams();
          self._refreshBuffer(sound);

          // Setup the playback params.
          var vol = (sound._muted || self._muted) ? 0 : sound._volume;
          node.gain.setValueAtTime(vol, Howler.ctx.currentTime);
          sound._playStart = Howler.ctx.currentTime;

          // Play the sound using the supported method.
          if (typeof node.bufferSource.start === 'undefined') {
            sound._loop ? node.bufferSource.noteGrainOn(0, seek, 86400) : node.bufferSource.noteGrainOn(0, seek, duration);
          } else {
            sound._loop ? node.bufferSource.start(0, seek, 86400) : node.bufferSource.start(0, seek, duration);
          }

          // Start a new timer if none is present.
          if (timeout !== Infinity) {
            self._endTimers[sound._id] = setTimeout(self._ended.bind(self, sound), timeout);
          }

          if (!internal) {
            setTimeout(function() {
              self._emit('play', sound._id);
              self._loadQueue();
            }, 0);
          }
        };

        if (Howler.state === 'running' && Howler.ctx.state !== 'interrupted') {
          playWebAudio();
        } else {
          self._playLock = true;

          // Wait for the audio context to resume before playing.
          self.once('resume', playWebAudio);

          // Cancel the end timer.
          self._clearTimer(sound._id);
        }
      } else {
        // Fire this when the sound is ready to play to begin HTML5 Audio playback.
        var playHtml5 = function() {
          node.currentTime = seek;
          node.muted = sound._muted || self._muted || Howler._muted || node.muted;
          node.volume = sound._volume * Howler.volume();
          node.playbackRate = sound._rate;

          // Some browsers will throw an error if this is called without user interaction.
          try {
            var play = node.play();

            // Support older browsers that don't support promises, and thus don't have this issue.
            if (play && typeof Promise !== 'undefined' && (play instanceof Promise || typeof play.then === 'function')) {
              // Implements a lock to prevent DOMException: The play() request was interrupted by a call to pause().
              self._playLock = true;

              // Set param values immediately.
              setParams();

              // Releases the lock and executes queued actions.
              play
                .then(function() {
                  self._playLock = false;
                  node._unlocked = true;
                  if (!internal) {
                    self._emit('play', sound._id);
                    self._loadQueue();
                  }
                })
                .catch(function() {
                  self._playLock = false;
                  self._emit('playerror', sound._id, 'Playback was unable to start. This is most commonly an issue ' +
                    'on mobile devices and Chrome where playback was not within a user interaction.');

                  // Reset the ended and paused values.
                  sound._ended = true;
                  sound._paused = true;
                });
            } else if (!internal) {
              self._playLock = false;
              setParams();
              self._emit('play', sound._id);
              self._loadQueue();
            }

            // Setting rate before playing won't work in IE, so we set it again here.
            node.playbackRate = sound._rate;

            // If the node is still paused, then we can assume there was a playback issue.
            if (node.paused) {
              self._emit('playerror', sound._id, 'Playback was unable to start. This is most commonly an issue ' +
                'on mobile devices and Chrome where playback was not within a user interaction.');
              return;
            }

            // Setup the end timer on sprites or listen for the ended event.
            if (sprite !== '__default' || sound._loop) {
              self._endTimers[sound._id] = setTimeout(self._ended.bind(self, sound), timeout);
            } else {
              self._endTimers[sound._id] = function() {
                // Fire ended on this audio node.
                self._ended(sound);

                // Clear this listener.
                node.removeEventListener('ended', self._endTimers[sound._id], false);
              };
              node.addEventListener('ended', self._endTimers[sound._id], false);
            }
          } catch (err) {
            self._emit('playerror', sound._id, err);
          }
        };

        // If this is streaming audio, make sure the src is set and load again.
        if (node.src === 'data:audio/wav;base64,UklGRigAAABXQVZFZm10IBIAAAABAAEARKwAAIhYAQACABAAAABkYXRhAgAAAAEA') {
          node.src = self._src;
          node.load();
        }

        // Play immediately if ready, or wait for the 'canplaythrough'e vent.
        var loadedNoReadyState = (window && window.ejecta) || (!node.readyState && Howler._navigator.isCocoonJS);
        if (node.readyState >= 3 || loadedNoReadyState) {
          playHtml5();
        } else {
          self._playLock = true;

          var listener = function() {
            // Begin playback.
            playHtml5();

            // Clear this listener.
            node.removeEventListener(Howler._canPlayEvent, listener, false);
          };
          node.addEventListener(Howler._canPlayEvent, listener, false);

          // Cancel the end timer.
          self._clearTimer(sound._id);
        }
      }

      return sound._id;
    },

    /**
     * Pause playback and save current position.
     * @param  {Number} id The sound ID (empty to pause all in group).
     * @return {Howl}
     */
    pause: function(id) {
      var self = this;

      // If the sound hasn't loaded or a play() promise is pending, add it to the load queue to pause when capable.
      if (self._state !== 'loaded' || self._playLock) {
        self._queue.push({
          event: 'pause',
          action: function() {
            self.pause(id);
          }
        });

        return self;
      }

      // If no id is passed, get all ID's to be paused.
      var ids = self._getSoundIds(id);

      for (var i=0; i<ids.length; i++) {
        // Clear the end timer.
        self._clearTimer(ids[i]);

        // Get the sound.
        var sound = self._soundById(ids[i]);

        if (sound && !sound._paused) {
          // Reset the seek position.
          sound._seek = self.seek(ids[i]);
          sound._rateSeek = 0;
          sound._paused = true;

          // Stop currently running fades.
          self._stopFade(ids[i]);

          if (sound._node) {
            if (self._webAudio) {
              // Make sure the sound has been created.
              if (!sound._node.bufferSource) {
                continue;
              }

              if (typeof sound._node.bufferSource.stop === 'undefined') {
                sound._node.bufferSource.noteOff(0);
              } else {
                sound._node.bufferSource.stop(0);
              }

              // Clean up the buffer source.
              self._cleanBuffer(sound._node);
            } else if (!isNaN(sound._node.duration) || sound._node.duration === Infinity) {
              sound._node.pause();
            }
          }
        }

        // Fire the pause event, unless `true` is passed as the 2nd argument.
        if (!arguments[1]) {
          self._emit('pause', sound ? sound._id : null);
        }
      }

      return self;
    },

    /**
     * Stop playback and reset to start.
     * @param  {Number} id The sound ID (empty to stop all in group).
     * @param  {Boolean} internal Internal Use: true prevents event firing.
     * @return {Howl}
     */
    stop: function(id, internal) {
      var self = this;

      // If the sound hasn't loaded, add it to the load queue to stop when capable.
      if (self._state !== 'loaded' || self._playLock) {
        self._queue.push({
          event: 'stop',
          action: function() {
            self.stop(id);
          }
        });

        return self;
      }

      // If no id is passed, get all ID's to be stopped.
      var ids = self._getSoundIds(id);

      for (var i=0; i<ids.length; i++) {
        // Clear the end timer.
        self._clearTimer(ids[i]);

        // Get the sound.
        var sound = self._soundById(ids[i]);

        if (sound) {
          // Reset the seek position.
          sound._seek = sound._start || 0;
          sound._rateSeek = 0;
          sound._paused = true;
          sound._ended = true;

          // Stop currently running fades.
          self._stopFade(ids[i]);

          if (sound._node) {
            if (self._webAudio) {
              // Make sure the sound's AudioBufferSourceNode has been created.
              if (sound._node.bufferSource) {
                if (typeof sound._node.bufferSource.stop === 'undefined') {
                  sound._node.bufferSource.noteOff(0);
                } else {
                  sound._node.bufferSource.stop(0);
                }

                // Clean up the buffer source.
                self._cleanBuffer(sound._node);
              }
            } else if (!isNaN(sound._node.duration) || sound._node.duration === Infinity) {
              sound._node.currentTime = sound._start || 0;
              sound._node.pause();

              // If this is a live stream, stop download once the audio is stopped.
              if (sound._node.duration === Infinity) {
                self._clearSound(sound._node);
              }
            }
          }

          if (!internal) {
            self._emit('stop', sound._id);
          }
        }
      }

      return self;
    },

    /**
     * Mute/unmute a single sound or all sounds in this Howl group.
     * @param  {Boolean} muted Set to true to mute and false to unmute.
     * @param  {Number} id    The sound ID to update (omit to mute/unmute all).
     * @return {Howl}
     */
    mute: function(muted, id) {
      var self = this;

      // If the sound hasn't loaded, add it to the load queue to mute when capable.
      if (self._state !== 'loaded'|| self._playLock) {
        self._queue.push({
          event: 'mute',
          action: function() {
            self.mute(muted, id);
          }
        });

        return self;
      }

      // If applying mute/unmute to all sounds, update the group's value.
      if (typeof id === 'undefined') {
        if (typeof muted === 'boolean') {
          self._muted = muted;
        } else {
          return self._muted;
        }
      }

      // If no id is passed, get all ID's to be muted.
      var ids = self._getSoundIds(id);

      for (var i=0; i<ids.length; i++) {
        // Get the sound.
        var sound = self._soundById(ids[i]);

        if (sound) {
          sound._muted = muted;

          // Cancel active fade and set the volume to the end value.
          if (sound._interval) {
            self._stopFade(sound._id);
          }

          if (self._webAudio && sound._node) {
            sound._node.gain.setValueAtTime(muted ? 0 : sound._volume, Howler.ctx.currentTime);
          } else if (sound._node) {
            sound._node.muted = Howler._muted ? true : muted;
          }

          self._emit('mute', sound._id);
        }
      }

      return self;
    },

    /**
     * Get/set the volume of this sound or of the Howl group. This method can optionally take 0, 1 or 2 arguments.
     *   volume() -> Returns the group's volume value.
     *   volume(id) -> Returns the sound id's current volume.
     *   volume(vol) -> Sets the volume of all sounds in this Howl group.
     *   volume(vol, id) -> Sets the volume of passed sound id.
     * @return {Howl/Number} Returns self or current volume.
     */
    volume: function() {
      var self = this;
      var args = arguments;
      var vol, id;

      // Determine the values based on arguments.
      if (args.length === 0) {
        // Return the value of the groups' volume.
        return self._volume;
      } else if (args.length === 1 || args.length === 2 && typeof args[1] === 'undefined') {
        // First check if this is an ID, and if not, assume it is a new volume.
        var ids = self._getSoundIds();
        var index = ids.indexOf(args[0]);
        if (index >= 0) {
          id = parseInt(args[0], 10);
        } else {
          vol = parseFloat(args[0]);
        }
      } else if (args.length >= 2) {
        vol = parseFloat(args[0]);
        id = parseInt(args[1], 10);
      }

      // Update the volume or return the current volume.
      var sound;
      if (typeof vol !== 'undefined' && vol >= 0 && vol <= 1) {
        // If the sound hasn't loaded, add it to the load queue to change volume when capable.
        if (self._state !== 'loaded'|| self._playLock) {
          self._queue.push({
            event: 'volume',
            action: function() {
              self.volume.apply(self, args);
            }
          });

          return self;
        }

        // Set the group volume.
        if (typeof id === 'undefined') {
          self._volume = vol;
        }

        // Update one or all volumes.
        id = self._getSoundIds(id);
        for (var i=0; i<id.length; i++) {
          // Get the sound.
          sound = self._soundById(id[i]);

          if (sound) {
            sound._volume = vol;

            // Stop currently running fades.
            if (!args[2]) {
              self._stopFade(id[i]);
            }

            if (self._webAudio && sound._node && !sound._muted) {
              sound._node.gain.setValueAtTime(vol, Howler.ctx.currentTime);
            } else if (sound._node && !sound._muted) {
              sound._node.volume = vol * Howler.volume();
            }

            self._emit('volume', sound._id);
          }
        }
      } else {
        sound = id ? self._soundById(id) : self._sounds[0];
        return sound ? sound._volume : 0;
      }

      return self;
    },

    /**
     * Fade a currently playing sound between two volumes (if no id is passed, all sounds will fade).
     * @param  {Number} from The value to fade from (0.0 to 1.0).
     * @param  {Number} to   The volume to fade to (0.0 to 1.0).
     * @param  {Number} len  Time in milliseconds to fade.
     * @param  {Number} id   The sound id (omit to fade all sounds).
     * @return {Howl}
     */
    fade: function(from, to, len, id) {
      var self = this;

      // If the sound hasn't loaded, add it to the load queue to fade when capable.
      if (self._state !== 'loaded' || self._playLock) {
        self._queue.push({
          event: 'fade',
          action: function() {
            self.fade(from, to, len, id);
          }
        });

        return self;
      }

      // Make sure the to/from/len values are numbers.
      from = Math.min(Math.max(0, parseFloat(from)), 1);
      to = Math.min(Math.max(0, parseFloat(to)), 1);
      len = parseFloat(len);

      // Set the volume to the start position.
      self.volume(from, id);

      // Fade the volume of one or all sounds.
      var ids = self._getSoundIds(id);
      for (var i=0; i<ids.length; i++) {
        // Get the sound.
        var sound = self._soundById(ids[i]);

        // Create a linear fade or fall back to timeouts with HTML5 Audio.
        if (sound) {
          // Stop the previous fade if no sprite is being used (otherwise, volume handles this).
          if (!id) {
            self._stopFade(ids[i]);
          }

          // If we are using Web Audio, let the native methods do the actual fade.
          if (self._webAudio && !sound._muted) {
            var currentTime = Howler.ctx.currentTime;
            var end = currentTime + (len / 1000);
            sound._volume = from;
            sound._node.gain.setValueAtTime(from, currentTime);
            sound._node.gain.linearRampToValueAtTime(to, end);
          }

          self._startFadeInterval(sound, from, to, len, ids[i], typeof id === 'undefined');
        }
      }

      return self;
    },

    /**
     * Starts the internal interval to fade a sound.
     * @param  {Object} sound Reference to sound to fade.
     * @param  {Number} from The value to fade from (0.0 to 1.0).
     * @param  {Number} to   The volume to fade to (0.0 to 1.0).
     * @param  {Number} len  Time in milliseconds to fade.
     * @param  {Number} id   The sound id to fade.
     * @param  {Boolean} isGroup   If true, set the volume on the group.
     */
    _startFadeInterval: function(sound, from, to, len, id, isGroup) {
      var self = this;
      var vol = from;
      var diff = to - from;
      var steps = Math.abs(diff / 0.01);
      var stepLen = Math.max(4, (steps > 0) ? len / steps : len);
      var lastTick = Date.now();

      // Store the value being faded to.
      sound._fadeTo = to;

      // Update the volume value on each interval tick.
      sound._interval = setInterval(function() {
        // Update the volume based on the time since the last tick.
        var tick = (Date.now() - lastTick) / len;
        lastTick = Date.now();
        vol += diff * tick;

        // Round to within 2 decimal points.
        vol = Math.round(vol * 100) / 100;

        // Make sure the volume is in the right bounds.
        if (diff < 0) {
          vol = Math.max(to, vol);
        } else {
          vol = Math.min(to, vol);
        }

        // Change the volume.
        if (self._webAudio) {
          sound._volume = vol;
        } else {
          self.volume(vol, sound._id, true);
        }

        // Set the group's volume.
        if (isGroup) {
          self._volume = vol;
        }

        // When the fade is complete, stop it and fire event.
        if ((to < from && vol <= to) || (to > from && vol >= to)) {
          clearInterval(sound._interval);
          sound._interval = null;
          sound._fadeTo = null;
          self.volume(to, sound._id);
          self._emit('fade', sound._id);
        }
      }, stepLen);
    },

    /**
     * Internal method that stops the currently playing fade when
     * a new fade starts, volume is changed or the sound is stopped.
     * @param  {Number} id The sound id.
     * @return {Howl}
     */
    _stopFade: function(id) {
      var self = this;
      var sound = self._soundById(id);

      if (sound && sound._interval) {
        if (self._webAudio) {
          sound._node.gain.cancelScheduledValues(Howler.ctx.currentTime);
        }

        clearInterval(sound._interval);
        sound._interval = null;
        self.volume(sound._fadeTo, id);
        sound._fadeTo = null;
        self._emit('fade', id);
      }

      return self;
    },

    /**
     * Get/set the loop parameter on a sound. This method can optionally take 0, 1 or 2 arguments.
     *   loop() -> Returns the group's loop value.
     *   loop(id) -> Returns the sound id's loop value.
     *   loop(loop) -> Sets the loop value for all sounds in this Howl group.
     *   loop(loop, id) -> Sets the loop value of passed sound id.
     * @return {Howl/Boolean} Returns self or current loop value.
     */
    loop: function() {
      var self = this;
      var args = arguments;
      var loop, id, sound;

      // Determine the values for loop and id.
      if (args.length === 0) {
        // Return the grou's loop value.
        return self._loop;
      } else if (args.length === 1) {
        if (typeof args[0] === 'boolean') {
          loop = args[0];
          self._loop = loop;
        } else {
          // Return this sound's loop value.
          sound = self._soundById(parseInt(args[0], 10));
          return sound ? sound._loop : false;
        }
      } else if (args.length === 2) {
        loop = args[0];
        id = parseInt(args[1], 10);
      }

      // If no id is passed, get all ID's to be looped.
      var ids = self._getSoundIds(id);
      for (var i=0; i<ids.length; i++) {
        sound = self._soundById(ids[i]);

        if (sound) {
          sound._loop = loop;
          if (self._webAudio && sound._node && sound._node.bufferSource) {
            sound._node.bufferSource.loop = loop;
            if (loop) {
              sound._node.bufferSource.loopStart = sound._start || 0;
              sound._node.bufferSource.loopEnd = sound._stop;
            }
          }
        }
      }

      return self;
    },

    /**
     * Get/set the playback rate of a sound. This method can optionally take 0, 1 or 2 arguments.
     *   rate() -> Returns the first sound node's current playback rate.
     *   rate(id) -> Returns the sound id's current playback rate.
     *   rate(rate) -> Sets the playback rate of all sounds in this Howl group.
     *   rate(rate, id) -> Sets the playback rate of passed sound id.
     * @return {Howl/Number} Returns self or the current playback rate.
     */
    rate: function() {
      var self = this;
      var args = arguments;
      var rate, id;

      // Determine the values based on arguments.
      if (args.length === 0) {
        // We will simply return the current rate of the first node.
        id = self._sounds[0]._id;
      } else if (args.length === 1) {
        // First check if this is an ID, and if not, assume it is a new rate value.
        var ids = self._getSoundIds();
        var index = ids.indexOf(args[0]);
        if (index >= 0) {
          id = parseInt(args[0], 10);
        } else {
          rate = parseFloat(args[0]);
        }
      } else if (args.length === 2) {
        rate = parseFloat(args[0]);
        id = parseInt(args[1], 10);
      }

      // Update the playback rate or return the current value.
      var sound;
      if (typeof rate === 'number') {
        // If the sound hasn't loaded, add it to the load queue to change playback rate when capable.
        if (self._state !== 'loaded' || self._playLock) {
          self._queue.push({
            event: 'rate',
            action: function() {
              self.rate.apply(self, args);
            }
          });

          return self;
        }

        // Set the group rate.
        if (typeof id === 'undefined') {
          self._rate = rate;
        }

        // Update one or all volumes.
        id = self._getSoundIds(id);
        for (var i=0; i<id.length; i++) {
          // Get the sound.
          sound = self._soundById(id[i]);

          if (sound) {
            // Keep track of our position when the rate changed and update the playback
            // start position so we can properly adjust the seek position for time elapsed.
            if (self.playing(id[i])) {
              sound._rateSeek = self.seek(id[i]);
              sound._playStart = self._webAudio ? Howler.ctx.currentTime : sound._playStart;
            }
            sound._rate = rate;

            // Change the playback rate.
            if (self._webAudio && sound._node && sound._node.bufferSource) {
              sound._node.bufferSource.playbackRate.setValueAtTime(rate, Howler.ctx.currentTime);
            } else if (sound._node) {
              sound._node.playbackRate = rate;
            }

            // Reset the timers.
            var seek = self.seek(id[i]);
            var duration = ((self._sprite[sound._sprite][0] + self._sprite[sound._sprite][1]) / 1000) - seek;
            var timeout = (duration * 1000) / Math.abs(sound._rate);

            // Start a new end timer if sound is already playing.
            if (self._endTimers[id[i]] || !sound._paused) {
              self._clearTimer(id[i]);
              self._endTimers[id[i]] = setTimeout(self._ended.bind(self, sound), timeout);
            }

            self._emit('rate', sound._id);
          }
        }
      } else {
        sound = self._soundById(id);
        return sound ? sound._rate : self._rate;
      }

      return self;
    },

    /**
     * Get/set the seek position of a sound. This method can optionally take 0, 1 or 2 arguments.
     *   seek() -> Returns the first sound node's current seek position.
     *   seek(id) -> Returns the sound id's current seek position.
     *   seek(seek) -> Sets the seek position of the first sound node.
     *   seek(seek, id) -> Sets the seek position of passed sound id.
     * @return {Howl/Number} Returns self or the current seek position.
     */
    seek: function() {
      var self = this;
      var args = arguments;
      var seek, id;

      // Determine the values based on arguments.
      if (args.length === 0) {
        // We will simply return the current position of the first node.
        id = self._sounds[0]._id;
      } else if (args.length === 1) {
        // First check if this is an ID, and if not, assume it is a new seek position.
        var ids = self._getSoundIds();
        var index = ids.indexOf(args[0]);
        if (index >= 0) {
          id = parseInt(args[0], 10);
        } else if (self._sounds.length) {
          id = self._sounds[0]._id;
          seek = parseFloat(args[0]);
        }
      } else if (args.length === 2) {
        seek = parseFloat(args[0]);
        id = parseInt(args[1], 10);
      }

      // If there is no ID, bail out.
      if (typeof id === 'undefined') {
        return self;
      }

      // If the sound hasn't loaded, add it to the load queue to seek when capable.
      if (typeof seek === 'number' && (self._state !== 'loaded' || self._playLock)) {
        self._queue.push({
          event: 'seek',
          action: function() {
            self.seek.apply(self, args);
          }
        });

        return self;
      }

      // Get the sound.
      var sound = self._soundById(id);

      if (sound) {
        if (typeof seek === 'number' && seek >= 0) {
          // Pause the sound and update position for restarting playback.
          var playing = self.playing(id);
          if (playing) {
            self.pause(id, true);
          }

          // Move the position of the track and cancel timer.
          sound._seek = seek;
          sound._ended = false;
          self._clearTimer(id);

          // Update the seek position for HTML5 Audio.
          if (!self._webAudio && sound._node && !isNaN(sound._node.duration)) {
            sound._node.currentTime = seek;
          }

          // Seek and emit when ready.
          var seekAndEmit = function() {
            self._emit('seek', id);

            // Restart the playback if the sound was playing.
            if (playing) {
              self.play(id, true);
            }
          };

          // Wait for the play lock to be unset before emitting (HTML5 Audio).
          if (playing && !self._webAudio) {
            var emitSeek = function() {
              if (!self._playLock) {
                seekAndEmit();
              } else {
                setTimeout(emitSeek, 0);
              }
            };
            setTimeout(emitSeek, 0);
          } else {
            seekAndEmit();
          }
        } else {
          if (self._webAudio) {
            var realTime = self.playing(id) ? Howler.ctx.currentTime - sound._playStart : 0;
            var rateSeek = sound._rateSeek ? sound._rateSeek - sound._seek : 0;
            return sound._seek + (rateSeek + realTime * Math.abs(sound._rate));
          } else {
            return sound._node.currentTime;
          }
        }
      }

      return self;
    },

    /**
     * Check if a specific sound is currently playing or not (if id is provided), or check if at least one of the sounds in the group is playing or not.
     * @param  {Number}  id The sound id to check. If none is passed, the whole sound group is checked.
     * @return {Boolean} True if playing and false if not.
     */
    playing: function(id) {
      var self = this;

      // Check the passed sound ID (if any).
      if (typeof id === 'number') {
        var sound = self._soundById(id);
        return sound ? !sound._paused : false;
      }

      // Otherwise, loop through all sounds and check if any are playing.
      for (var i=0; i<self._sounds.length; i++) {
        if (!self._sounds[i]._paused) {
          return true;
        }
      }

      return false;
    },

    /**
     * Get the duration of this sound. Passing a sound id will return the sprite duration.
     * @param  {Number} id The sound id to check. If none is passed, return full source duration.
     * @return {Number} Audio duration in seconds.
     */
    duration: function(id) {
      var self = this;
      var duration = self._duration;

      // If we pass an ID, get the sound and return the sprite length.
      var sound = self._soundById(id);
      if (sound) {
        duration = self._sprite[sound._sprite][1] / 1000;
      }

      return duration;
    },

    /**
     * Returns the current loaded state of this Howl.
     * @return {String} 'unloaded', 'loading', 'loaded'
     */
    state: function() {
      return this._state;
    },

    /**
     * Unload and destroy the current Howl object.
     * This will immediately stop all sound instances attached to this group.
     */
    unload: function() {
      var self = this;

      // Stop playing any active sounds.
      var sounds = self._sounds;
      for (var i=0; i<sounds.length; i++) {
        // Stop the sound if it is currently playing.
        if (!sounds[i]._paused) {
          self.stop(sounds[i]._id);
        }

        // Remove the source or disconnect.
        if (!self._webAudio) {
          // Set the source to 0-second silence to stop any downloading (except in IE).
          self._clearSound(sounds[i]._node);

          // Remove any event listeners.
          sounds[i]._node.removeEventListener('error', sounds[i]._errorFn, false);
          sounds[i]._node.removeEventListener(Howler._canPlayEvent, sounds[i]._loadFn, false);
          sounds[i]._node.removeEventListener('ended', sounds[i]._endFn, false);

          // Release the Audio object back to the pool.
          Howler._releaseHtml5Audio(sounds[i]._node);
        }

        // Empty out all of the nodes.
        delete sounds[i]._node;

        // Make sure all timers are cleared out.
        self._clearTimer(sounds[i]._id);
      }

      // Remove the references in the global Howler object.
      var index = Howler._howls.indexOf(self);
      if (index >= 0) {
        Howler._howls.splice(index, 1);
      }

      // Delete this sound from the cache (if no other Howl is using it).
      var remCache = true;
      for (i=0; i<Howler._howls.length; i++) {
        if (Howler._howls[i]._src === self._src || self._src.indexOf(Howler._howls[i]._src) >= 0) {
          remCache = false;
          break;
        }
      }

      if (cache && remCache) {
        delete cache[self._src];
      }

      // Clear global errors.
      Howler.noAudio = false;

      // Clear out `self`.
      self._state = 'unloaded';
      self._sounds = [];
      self = null;

      return null;
    },

    /**
     * Listen to a custom event.
     * @param  {String}   event Event name.
     * @param  {Function} fn    Listener to call.
     * @param  {Number}   id    (optional) Only listen to events for this sound.
     * @param  {Number}   once  (INTERNAL) Marks event to fire only once.
     * @return {Howl}
     */
    on: function(event, fn, id, once) {
      var self = this;
      var events = self['_on' + event];

      if (typeof fn === 'function') {
        events.push(once ? {id: id, fn: fn, once: once} : {id: id, fn: fn});
      }

      return self;
    },

    /**
     * Remove a custom event. Call without parameters to remove all events.
     * @param  {String}   event Event name.
     * @param  {Function} fn    Listener to remove. Leave empty to remove all.
     * @param  {Number}   id    (optional) Only remove events for this sound.
     * @return {Howl}
     */
    off: function(event, fn, id) {
      var self = this;
      var events = self['_on' + event];
      var i = 0;

      // Allow passing just an event and ID.
      if (typeof fn === 'number') {
        id = fn;
        fn = null;
      }

      if (fn || id) {
        // Loop through event store and remove the passed function.
        for (i=0; i<events.length; i++) {
          var isId = (id === events[i].id);
          if (fn === events[i].fn && isId || !fn && isId) {
            events.splice(i, 1);
            break;
          }
        }
      } else if (event) {
        // Clear out all events of this type.
        self['_on' + event] = [];
      } else {
        // Clear out all events of every type.
        var keys = Object.keys(self);
        for (i=0; i<keys.length; i++) {
          if ((keys[i].indexOf('_on') === 0) && Array.isArray(self[keys[i]])) {
            self[keys[i]] = [];
          }
        }
      }

      return self;
    },

    /**
     * Listen to a custom event and remove it once fired.
     * @param  {String}   event Event name.
     * @param  {Function} fn    Listener to call.
     * @param  {Number}   id    (optional) Only listen to events for this sound.
     * @return {Howl}
     */
    once: function(event, fn, id) {
      var self = this;

      // Setup the event listener.
      self.on(event, fn, id, 1);

      return self;
    },

    /**
     * Emit all events of a specific type and pass the sound id.
     * @param  {String} event Event name.
     * @param  {Number} id    Sound ID.
     * @param  {Number} msg   Message to go with event.
     * @return {Howl}
     */
    _emit: function(event, id, msg) {
      var self = this;
      var events = self['_on' + event];

      // Loop through event store and fire all functions.
      for (var i=events.length-1; i>=0; i--) {
        // Only fire the listener if the correct ID is used.
        if (!events[i].id || events[i].id === id || event === 'load') {
          setTimeout(function(fn) {
            fn.call(this, id, msg);
          }.bind(self, events[i].fn), 0);

          // If this event was setup with `once`, remove it.
          if (events[i].once) {
            self.off(event, events[i].fn, events[i].id);
          }
        }
      }

      // Pass the event type into load queue so that it can continue stepping.
      self._loadQueue(event);

      return self;
    },

    /**
     * Queue of actions initiated before the sound has loaded.
     * These will be called in sequence, with the next only firing
     * after the previous has finished executing (even if async like play).
     * @return {Howl}
     */
    _loadQueue: function(event) {
      var self = this;

      if (self._queue.length > 0) {
        var task = self._queue[0];

        // Remove this task if a matching event was passed.
        if (task.event === event) {
          self._queue.shift();
          self._loadQueue();
        }

        // Run the task if no event type is passed.
        if (!event) {
          task.action();
        }
      }

      return self;
    },

    /**
     * Fired when playback ends at the end of the duration.
     * @param  {Sound} sound The sound object to work with.
     * @return {Howl}
     */
    _ended: function(sound) {
      var self = this;
      var sprite = sound._sprite;

      // If we are using IE and there was network latency we may be clipping
      // audio before it completes playing. Lets check the node to make sure it
      // believes it has completed, before ending the playback.
      if (!self._webAudio && sound._node && !sound._node.paused && !sound._node.ended && sound._node.currentTime < sound._stop) {
        setTimeout(self._ended.bind(self, sound), 100);
        return self;
      }

      // Should this sound loop?
      var loop = !!(sound._loop || self._sprite[sprite][2]);

      // Fire the ended event.
      self._emit('end', sound._id);

      // Restart the playback for HTML5 Audio loop.
      if (!self._webAudio && loop) {
        self.stop(sound._id, true).play(sound._id);
      }

      // Restart this timer if on a Web Audio loop.
      if (self._webAudio && loop) {
        self._emit('play', sound._id);
        sound._seek = sound._start || 0;
        sound._rateSeek = 0;
        sound._playStart = Howler.ctx.currentTime;

        var timeout = ((sound._stop - sound._start) * 1000) / Math.abs(sound._rate);
        self._endTimers[sound._id] = setTimeout(self._ended.bind(self, sound), timeout);
      }

      // Mark the node as paused.
      if (self._webAudio && !loop) {
        sound._paused = true;
        sound._ended = true;
        sound._seek = sound._start || 0;
        sound._rateSeek = 0;
        self._clearTimer(sound._id);

        // Clean up the buffer source.
        self._cleanBuffer(sound._node);

        // Attempt to auto-suspend AudioContext if no sounds are still playing.
        Howler._autoSuspend();
      }

      // When using a sprite, end the track.
      if (!self._webAudio && !loop) {
        self.stop(sound._id, true);
      }

      return self;
    },

    /**
     * Clear the end timer for a sound playback.
     * @param  {Number} id The sound ID.
     * @return {Howl}
     */
    _clearTimer: function(id) {
      var self = this;

      if (self._endTimers[id]) {
        // Clear the timeout or remove the ended listener.
        if (typeof self._endTimers[id] !== 'function') {
          clearTimeout(self._endTimers[id]);
        } else {
          var sound = self._soundById(id);
          if (sound && sound._node) {
            sound._node.removeEventListener('ended', self._endTimers[id], false);
          }
        }

        delete self._endTimers[id];
      }

      return self;
    },

    /**
     * Return the sound identified by this ID, or return null.
     * @param  {Number} id Sound ID
     * @return {Object}    Sound object or null.
     */
    _soundById: function(id) {
      var self = this;

      // Loop through all sounds and find the one with this ID.
      for (var i=0; i<self._sounds.length; i++) {
        if (id === self._sounds[i]._id) {
          return self._sounds[i];
        }
      }

      return null;
    },

    /**
     * Return an inactive sound from the pool or create a new one.
     * @return {Sound} Sound playback object.
     */
    _inactiveSound: function() {
      var self = this;

      self._drain();

      // Find the first inactive node to recycle.
      for (var i=0; i<self._sounds.length; i++) {
        if (self._sounds[i]._ended) {
          return self._sounds[i].reset();
        }
      }

      // If no inactive node was found, create a new one.
      return new Sound(self);
    },

    /**
     * Drain excess inactive sounds from the pool.
     */
    _drain: function() {
      var self = this;
      var limit = self._pool;
      var cnt = 0;
      var i = 0;

      // If there are less sounds than the max pool size, we are done.
      if (self._sounds.length < limit) {
        return;
      }

      // Count the number of inactive sounds.
      for (i=0; i<self._sounds.length; i++) {
        if (self._sounds[i]._ended) {
          cnt++;
        }
      }

      // Remove excess inactive sounds, going in reverse order.
      for (i=self._sounds.length - 1; i>=0; i--) {
        if (cnt <= limit) {
          return;
        }

        if (self._sounds[i]._ended) {
          // Disconnect the audio source when using Web Audio.
          if (self._webAudio && self._sounds[i]._node) {
            self._sounds[i]._node.disconnect(0);
          }

          // Remove sounds until we have the pool size.
          self._sounds.splice(i, 1);
          cnt--;
        }
      }
    },

    /**
     * Get all ID's from the sounds pool.
     * @param  {Number} id Only return one ID if one is passed.
     * @return {Array}    Array of IDs.
     */
    _getSoundIds: function(id) {
      var self = this;

      if (typeof id === 'undefined') {
        var ids = [];
        for (var i=0; i<self._sounds.length; i++) {
          ids.push(self._sounds[i]._id);
        }

        return ids;
      } else {
        return [id];
      }
    },

    /**
     * Load the sound back into the buffer source.
     * @param  {Sound} sound The sound object to work with.
     * @return {Howl}
     */
    _refreshBuffer: function(sound) {
      var self = this;

      // Setup the buffer source for playback.
      sound._node.bufferSource = Howler.ctx.createBufferSource();
      sound._node.bufferSource.buffer = cache[self._src];

      // Connect to the correct node.
      if (sound._panner) {
        sound._node.bufferSource.connect(sound._panner);
      } else {
        sound._node.bufferSource.connect(sound._node);
      }

      // Setup looping and playback rate.
      sound._node.bufferSource.loop = sound._loop;
      if (sound._loop) {
        sound._node.bufferSource.loopStart = sound._start || 0;
        sound._node.bufferSource.loopEnd = sound._stop || 0;
      }
      sound._node.bufferSource.playbackRate.setValueAtTime(sound._rate, Howler.ctx.currentTime);

      return self;
    },

    /**
     * Prevent memory leaks by cleaning up the buffer source after playback.
     * @param  {Object} node Sound's audio node containing the buffer source.
     * @return {Howl}
     */
    _cleanBuffer: function(node) {
      var self = this;
      var isIOS = Howler._navigator && Howler._navigator.vendor.indexOf('Apple') >= 0;

      if (Howler._scratchBuffer && node.bufferSource) {
        node.bufferSource.onended = null;
        node.bufferSource.disconnect(0);
        if (isIOS) {
          try { node.bufferSource.buffer = Howler._scratchBuffer; } catch(e) {}
        }
      }
      node.bufferSource = null;

      return self;
    },

    /**
     * Set the source to a 0-second silence to stop any downloading (except in IE).
     * @param  {Object} node Audio node to clear.
     */
    _clearSound: function(node) {
      var checkIE = /MSIE |Trident\//.test(Howler._navigator && Howler._navigator.userAgent);
      if (!checkIE) {
        node.src = 'data:audio/wav;base64,UklGRigAAABXQVZFZm10IBIAAAABAAEARKwAAIhYAQACABAAAABkYXRhAgAAAAEA';
      }
    }
  };

  /** Single Sound Methods **/
  /***************************************************************************/

  /**
   * Setup the sound object, which each node attached to a Howl group is contained in.
   * @param {Object} howl The Howl parent group.
   */
  var Sound = function(howl) {
    this._parent = howl;
    this.init();
  };
  Sound.prototype = {
    /**
     * Initialize a new Sound object.
     * @return {Sound}
     */
    init: function() {
      var self = this;
      var parent = self._parent;

      // Setup the default parameters.
      self._muted = parent._muted;
      self._loop = parent._loop;
      self._volume = parent._volume;
      self._rate = parent._rate;
      self._seek = 0;
      self._paused = true;
      self._ended = true;
      self._sprite = '__default';

      // Generate a unique ID for this sound.
      self._id = ++Howler._counter;

      // Add itself to the parent's pool.
      parent._sounds.push(self);

      // Create the new node.
      self.create();

      return self;
    },

    /**
     * Create and setup a new sound object, whether HTML5 Audio or Web Audio.
     * @return {Sound}
     */
    create: function() {
      var self = this;
      var parent = self._parent;
      var volume = (Howler._muted || self._muted || self._parent._muted) ? 0 : self._volume;

      if (parent._webAudio) {
        // Create the gain node for controlling volume (the source will connect to this).
        self._node = (typeof Howler.ctx.createGain === 'undefined') ? Howler.ctx.createGainNode() : Howler.ctx.createGain();
        self._node.gain.setValueAtTime(volume, Howler.ctx.currentTime);
        self._node.paused = true;
        self._node.connect(Howler.masterGain);
      } else if (!Howler.noAudio) {
        // Get an unlocked Audio object from the pool.
        self._node = Howler._obtainHtml5Audio();

        // Listen for errors (http://dev.w3.org/html5/spec-author-view/spec.html#mediaerror).
        self._errorFn = self._errorListener.bind(self);
        self._node.addEventListener('error', self._errorFn, false);

        // Listen for 'canplaythrough' event to let us know the sound is ready.
        self._loadFn = self._loadListener.bind(self);
        self._node.addEventListener(Howler._canPlayEvent, self._loadFn, false);

        // Listen for the 'ended' event on the sound to account for edge-case where
        // a finite sound has a duration of Infinity.
        self._endFn = self._endListener.bind(self);
        self._node.addEventListener('ended', self._endFn, false);

        // Setup the new audio node.
        self._node.src = parent._src;
        self._node.preload = parent._preload === true ? 'auto' : parent._preload;
        self._node.volume = volume * Howler.volume();

        // Begin loading the source.
        self._node.load();
      }

      return self;
    },

    /**
     * Reset the parameters of this sound to the original state (for recycle).
     * @return {Sound}
     */
    reset: function() {
      var self = this;
      var parent = self._parent;

      // Reset all of the parameters of this sound.
      self._muted = parent._muted;
      self._loop = parent._loop;
      self._volume = parent._volume;
      self._rate = parent._rate;
      self._seek = 0;
      self._rateSeek = 0;
      self._paused = true;
      self._ended = true;
      self._sprite = '__default';

      // Generate a new ID so that it isn't confused with the previous sound.
      self._id = ++Howler._counter;

      return self;
    },

    /**
     * HTML5 Audio error listener callback.
     */
    _errorListener: function() {
      var self = this;

      // Fire an error event and pass back the code.
      self._parent._emit('loaderror', self._id, self._node.error ? self._node.error.code : 0);

      // Clear the event listener.
      self._node.removeEventListener('error', self._errorFn, false);
    },

    /**
     * HTML5 Audio canplaythrough listener callback.
     */
    _loadListener: function() {
      var self = this;
      var parent = self._parent;

      // Round up the duration to account for the lower precision in HTML5 Audio.
      parent._duration = Math.ceil(self._node.duration * 10) / 10;

      // Setup a sprite if none is defined.
      if (Object.keys(parent._sprite).length === 0) {
        parent._sprite = {__default: [0, parent._duration * 1000]};
      }

      if (parent._state !== 'loaded') {
        parent._state = 'loaded';
        parent._emit('load');
        parent._loadQueue();
      }

      // Clear the event listener.
      self._node.removeEventListener(Howler._canPlayEvent, self._loadFn, false);
    },

    /**
     * HTML5 Audio ended listener callback.
     */
    _endListener: function() {
      var self = this;
      var parent = self._parent;

      // Only handle the `ended`` event if the duration is Infinity.
      if (parent._duration === Infinity) {
        // Update the parent duration to match the real audio duration.
        // Round up the duration to account for the lower precision in HTML5 Audio.
        parent._duration = Math.ceil(self._node.duration * 10) / 10;

        // Update the sprite that corresponds to the real duration.
        if (parent._sprite.__default[1] === Infinity) {
          parent._sprite.__default[1] = parent._duration * 1000;
        }

        // Run the regular ended method.
        parent._ended(self);
      }

      // Clear the event listener since the duration is now correct.
      self._node.removeEventListener('ended', self._endFn, false);
    }
  };

  /** Helper Methods **/
  /***************************************************************************/

  var cache = {};

  /**
   * Buffer a sound from URL, Data URI or cache and decode to audio source (Web Audio API).
   * @param  {Howl} self
   */
  var loadBuffer = function(self) {
    var url = self._src;

    // Check if the buffer has already been cached and use it instead.
    if (cache[url]) {
      // Set the duration from the cache.
      self._duration = cache[url].duration;

      // Load the sound into this Howl.
      loadSound(self);

      return;
    }

    if (/^data:[^;]+;base64,/.test(url)) {
      // Decode the base64 data URI without XHR, since some browsers don't support it.
      var data = atob(url.split(',')[1]);
      var dataView = new Uint8Array(data.length);
      for (var i=0; i<data.length; ++i) {
        dataView[i] = data.charCodeAt(i);
      }

      decodeAudioData(dataView.buffer, self);
    } else {
      // Load the buffer from the URL.
      var xhr = new XMLHttpRequest();
      xhr.open(self._xhr.method, url, true);
      xhr.withCredentials = self._xhr.withCredentials;
      xhr.responseType = 'arraybuffer';

      // Apply any custom headers to the request.
      if (self._xhr.headers) {
        Object.keys(self._xhr.headers).forEach(function(key) {
          xhr.setRequestHeader(key, self._xhr.headers[key]);
        });
      }

      xhr.onload = function() {
        // Make sure we get a successful response back.
        var code = (xhr.status + '')[0];
        if (code !== '0' && code !== '2' && code !== '3') {
          self._emit('loaderror', null, 'Failed loading audio file with status: ' + xhr.status + '.');
          return;
        }

        decodeAudioData(xhr.response, self);
      };
      xhr.onerror = function() {
        // If there is an error, switch to HTML5 Audio.
        if (self._webAudio) {
          self._html5 = true;
          self._webAudio = false;
          self._sounds = [];
          delete cache[url];
          self.load();
        }
      };
      safeXhrSend(xhr);
    }
  };

  /**
   * Send the XHR request wrapped in a try/catch.
   * @param  {Object} xhr XHR to send.
   */
  var safeXhrSend = function(xhr) {
    try {
      xhr.send();
    } catch (e) {
      xhr.onerror();
    }
  };

  /**
   * Decode audio data from an array buffer.
   * @param  {ArrayBuffer} arraybuffer The audio data.
   * @param  {Howl}        self
   */
  var decodeAudioData = function(arraybuffer, self) {
    // Fire a load error if something broke.
    var error = function() {
      self._emit('loaderror', null, 'Decoding audio data failed.');
    };

    // Load the sound on success.
    var success = function(buffer) {
      if (buffer && self._sounds.length > 0) {
        cache[self._src] = buffer;
        loadSound(self, buffer);
      } else {
        error();
      }
    };

    // Decode the buffer into an audio source.
    if (typeof Promise !== 'undefined' && Howler.ctx.decodeAudioData.length === 1) {
      Howler.ctx.decodeAudioData(arraybuffer).then(success).catch(error);
    } else {
      Howler.ctx.decodeAudioData(arraybuffer, success, error);
    }
  }

  /**
   * Sound is now loaded, so finish setting everything up and fire the loaded event.
   * @param  {Howl} self
   * @param  {Object} buffer The decoded buffer sound source.
   */
  var loadSound = function(self, buffer) {
    // Set the duration.
    if (buffer && !self._duration) {
      self._duration = buffer.duration;
    }

    // Setup a sprite if none is defined.
    if (Object.keys(self._sprite).length === 0) {
      self._sprite = {__default: [0, self._duration * 1000]};
    }

    // Fire the loaded event.
    if (self._state !== 'loaded') {
      self._state = 'loaded';
      self._emit('load');
      self._loadQueue();
    }
  };

  /**
   * Setup the audio context when available, or switch to HTML5 Audio mode.
   */
  var setupAudioContext = function() {
    // If we have already detected that Web Audio isn't supported, don't run this step again.
    if (!Howler.usingWebAudio) {
      return;
    }

    // Check if we are using Web Audio and setup the AudioContext if we are.
    try {
      if (typeof AudioContext !== 'undefined') {
        Howler.ctx = new AudioContext();
      } else if (typeof webkitAudioContext !== 'undefined') {
        Howler.ctx = new webkitAudioContext();
      } else {
        Howler.usingWebAudio = false;
      }
    } catch(e) {
      Howler.usingWebAudio = false;
    }

    // If the audio context creation still failed, set using web audio to false.
    if (!Howler.ctx) {
      Howler.usingWebAudio = false;
    }

    // Check if a webview is being used on iOS8 or earlier (rather than the browser).
    // If it is, disable Web Audio as it causes crashing.
    var iOS = (/iP(hone|od|ad)/.test(Howler._navigator && Howler._navigator.platform));
    var appVersion = Howler._navigator && Howler._navigator.appVersion.match(/OS (\d+)_(\d+)_?(\d+)?/);
    var version = appVersion ? parseInt(appVersion[1], 10) : null;
    if (iOS && version && version < 9) {
      var safari = /safari/.test(Howler._navigator && Howler._navigator.userAgent.toLowerCase());
      if (Howler._navigator && !safari) {
        Howler.usingWebAudio = false;
      }
    }

    // Create and expose the master GainNode when using Web Audio (useful for plugins or advanced usage).
    if (Howler.usingWebAudio) {
      Howler.masterGain = (typeof Howler.ctx.createGain === 'undefined') ? Howler.ctx.createGainNode() : Howler.ctx.createGain();
      Howler.masterGain.gain.setValueAtTime(Howler._muted ? 0 : Howler._volume, Howler.ctx.currentTime);
      Howler.masterGain.connect(Howler.ctx.destination);
    }

    // Re-run the setup on Howler.
    Howler._setup();
  };

  // Add support for AMD (Asynchronous Module Definition) libraries such as require.js.
  if (typeof define === 'function' && define.amd) {
    define([], function() {
      return {
        Howler: Howler,
        Howl: Howl
      };
    });
  }

  // Add support for CommonJS libraries such as browserify.
  if (typeof exports !== 'undefined') {
    exports.Howler = Howler;
    exports.Howl = Howl;
  }

  // Add to global in Node.js (for testing, etc).
  if (typeof global !== 'undefined') {
    global.HowlerGlobal = HowlerGlobal;
    global.Howler = Howler;
    global.Howl = Howl;
    global.Sound = Sound;
  } else if (typeof window !== 'undefined') {  // Define globally in case AMD is not available or unused.
    window.HowlerGlobal = HowlerGlobal;
    window.Howler = Howler;
    window.Howl = Howl;
    window.Sound = Sound;
  }
})();


/*!
 *  Spatial Plugin - Adds support for stereo and 3D audio where Web Audio is supported.
 *  
 *  howler.js v2.2.1
 *  howlerjs.com
 *
 *  (c) 2013-2020, James Simpson of GoldFire Studios
 *  goldfirestudios.com
 *
 *  MIT License
 */

(function() {

  'use strict';

  // Setup default properties.
  HowlerGlobal.prototype._pos = [0, 0, 0];
  HowlerGlobal.prototype._orientation = [0, 0, -1, 0, 1, 0];

  /** Global Methods **/
  /***************************************************************************/

  /**
   * Helper method to update the stereo panning position of all current Howls.
   * Future Howls will not use this value unless explicitly set.
   * @param  {Number} pan A value of -1.0 is all the way left and 1.0 is all the way right.
   * @return {Howler/Number}     Self or current stereo panning value.
   */
  HowlerGlobal.prototype.stereo = function(pan) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self.ctx || !self.ctx.listener) {
      return self;
    }

    // Loop through all Howls and update their stereo panning.
    for (var i=self._howls.length-1; i>=0; i--) {
      self._howls[i].stereo(pan);
    }

    return self;
  };

  /**
   * Get/set the position of the listener in 3D cartesian space. Sounds using
   * 3D position will be relative to the listener's position.
   * @param  {Number} x The x-position of the listener.
   * @param  {Number} y The y-position of the listener.
   * @param  {Number} z The z-position of the listener.
   * @return {Howler/Array}   Self or current listener position.
   */
  HowlerGlobal.prototype.pos = function(x, y, z) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self.ctx || !self.ctx.listener) {
      return self;
    }

    // Set the defaults for optional 'y' & 'z'.
    y = (typeof y !== 'number') ? self._pos[1] : y;
    z = (typeof z !== 'number') ? self._pos[2] : z;

    if (typeof x === 'number') {
      self._pos = [x, y, z];

      if (typeof self.ctx.listener.positionX !== 'undefined') {
        self.ctx.listener.positionX.setTargetAtTime(self._pos[0], Howler.ctx.currentTime, 0.1);
        self.ctx.listener.positionY.setTargetAtTime(self._pos[1], Howler.ctx.currentTime, 0.1);
        self.ctx.listener.positionZ.setTargetAtTime(self._pos[2], Howler.ctx.currentTime, 0.1);
      } else {
        self.ctx.listener.setPosition(self._pos[0], self._pos[1], self._pos[2]);
      }
    } else {
      return self._pos;
    }

    return self;
  };

  /**
   * Get/set the direction the listener is pointing in the 3D cartesian space.
   * A front and up vector must be provided. The front is the direction the
   * face of the listener is pointing, and up is the direction the top of the
   * listener is pointing. Thus, these values are expected to be at right angles
   * from each other.
   * @param  {Number} x   The x-orientation of the listener.
   * @param  {Number} y   The y-orientation of the listener.
   * @param  {Number} z   The z-orientation of the listener.
   * @param  {Number} xUp The x-orientation of the top of the listener.
   * @param  {Number} yUp The y-orientation of the top of the listener.
   * @param  {Number} zUp The z-orientation of the top of the listener.
   * @return {Howler/Array}     Returns self or the current orientation vectors.
   */
  HowlerGlobal.prototype.orientation = function(x, y, z, xUp, yUp, zUp) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self.ctx || !self.ctx.listener) {
      return self;
    }

    // Set the defaults for optional 'y' & 'z'.
    var or = self._orientation;
    y = (typeof y !== 'number') ? or[1] : y;
    z = (typeof z !== 'number') ? or[2] : z;
    xUp = (typeof xUp !== 'number') ? or[3] : xUp;
    yUp = (typeof yUp !== 'number') ? or[4] : yUp;
    zUp = (typeof zUp !== 'number') ? or[5] : zUp;

    if (typeof x === 'number') {
      self._orientation = [x, y, z, xUp, yUp, zUp];

      if (typeof self.ctx.listener.forwardX !== 'undefined') {
        self.ctx.listener.forwardX.setTargetAtTime(x, Howler.ctx.currentTime, 0.1);
        self.ctx.listener.forwardY.setTargetAtTime(y, Howler.ctx.currentTime, 0.1);
        self.ctx.listener.forwardZ.setTargetAtTime(z, Howler.ctx.currentTime, 0.1);
        self.ctx.listener.upX.setTargetAtTime(xUp, Howler.ctx.currentTime, 0.1);
        self.ctx.listener.upY.setTargetAtTime(yUp, Howler.ctx.currentTime, 0.1);
        self.ctx.listener.upZ.setTargetAtTime(zUp, Howler.ctx.currentTime, 0.1);
      } else {
        self.ctx.listener.setOrientation(x, y, z, xUp, yUp, zUp);
      }
    } else {
      return or;
    }

    return self;
  };

  /** Group Methods **/
  /***************************************************************************/

  /**
   * Add new properties to the core init.
   * @param  {Function} _super Core init method.
   * @return {Howl}
   */
  Howl.prototype.init = (function(_super) {
    return function(o) {
      var self = this;

      // Setup user-defined default properties.
      self._orientation = o.orientation || [1, 0, 0];
      self._stereo = o.stereo || null;
      self._pos = o.pos || null;
      self._pannerAttr = {
        coneInnerAngle: typeof o.coneInnerAngle !== 'undefined' ? o.coneInnerAngle : 360,
        coneOuterAngle: typeof o.coneOuterAngle !== 'undefined' ? o.coneOuterAngle : 360,
        coneOuterGain: typeof o.coneOuterGain !== 'undefined' ? o.coneOuterGain : 0,
        distanceModel: typeof o.distanceModel !== 'undefined' ? o.distanceModel : 'inverse',
        maxDistance: typeof o.maxDistance !== 'undefined' ? o.maxDistance : 10000,
        panningModel: typeof o.panningModel !== 'undefined' ? o.panningModel : 'HRTF',
        refDistance: typeof o.refDistance !== 'undefined' ? o.refDistance : 1,
        rolloffFactor: typeof o.rolloffFactor !== 'undefined' ? o.rolloffFactor : 1
      };

      // Setup event listeners.
      self._onstereo = o.onstereo ? [{fn: o.onstereo}] : [];
      self._onpos = o.onpos ? [{fn: o.onpos}] : [];
      self._onorientation = o.onorientation ? [{fn: o.onorientation}] : [];

      // Complete initilization with howler.js core's init function.
      return _super.call(this, o);
    };
  })(Howl.prototype.init);

  /**
   * Get/set the stereo panning of the audio source for this sound or all in the group.
   * @param  {Number} pan  A value of -1.0 is all the way left and 1.0 is all the way right.
   * @param  {Number} id (optional) The sound ID. If none is passed, all in group will be updated.
   * @return {Howl/Number}    Returns self or the current stereo panning value.
   */
  Howl.prototype.stereo = function(pan, id) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self._webAudio) {
      return self;
    }

    // If the sound hasn't loaded, add it to the load queue to change stereo pan when capable.
    if (self._state !== 'loaded') {
      self._queue.push({
        event: 'stereo',
        action: function() {
          self.stereo(pan, id);
        }
      });

      return self;
    }

    // Check for PannerStereoNode support and fallback to PannerNode if it doesn't exist.
    var pannerType = (typeof Howler.ctx.createStereoPanner === 'undefined') ? 'spatial' : 'stereo';

    // Setup the group's stereo panning if no ID is passed.
    if (typeof id === 'undefined') {
      // Return the group's stereo panning if no parameters are passed.
      if (typeof pan === 'number') {
        self._stereo = pan;
        self._pos = [pan, 0, 0];
      } else {
        return self._stereo;
      }
    }

    // Change the streo panning of one or all sounds in group.
    var ids = self._getSoundIds(id);
    for (var i=0; i<ids.length; i++) {
      // Get the sound.
      var sound = self._soundById(ids[i]);

      if (sound) {
        if (typeof pan === 'number') {
          sound._stereo = pan;
          sound._pos = [pan, 0, 0];

          if (sound._node) {
            // If we are falling back, make sure the panningModel is equalpower.
            sound._pannerAttr.panningModel = 'equalpower';

            // Check if there is a panner setup and create a new one if not.
            if (!sound._panner || !sound._panner.pan) {
              setupPanner(sound, pannerType);
            }

            if (pannerType === 'spatial') {
              if (typeof sound._panner.positionX !== 'undefined') {
                sound._panner.positionX.setValueAtTime(pan, Howler.ctx.currentTime);
                sound._panner.positionY.setValueAtTime(0, Howler.ctx.currentTime);
                sound._panner.positionZ.setValueAtTime(0, Howler.ctx.currentTime);
              } else {
                sound._panner.setPosition(pan, 0, 0);
              }
            } else {
              sound._panner.pan.setValueAtTime(pan, Howler.ctx.currentTime);
            }
          }

          self._emit('stereo', sound._id);
        } else {
          return sound._stereo;
        }
      }
    }

    return self;
  };

  /**
   * Get/set the 3D spatial position of the audio source for this sound or group relative to the global listener.
   * @param  {Number} x  The x-position of the audio source.
   * @param  {Number} y  The y-position of the audio source.
   * @param  {Number} z  The z-position of the audio source.
   * @param  {Number} id (optional) The sound ID. If none is passed, all in group will be updated.
   * @return {Howl/Array}    Returns self or the current 3D spatial position: [x, y, z].
   */
  Howl.prototype.pos = function(x, y, z, id) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self._webAudio) {
      return self;
    }

    // If the sound hasn't loaded, add it to the load queue to change position when capable.
    if (self._state !== 'loaded') {
      self._queue.push({
        event: 'pos',
        action: function() {
          self.pos(x, y, z, id);
        }
      });

      return self;
    }

    // Set the defaults for optional 'y' & 'z'.
    y = (typeof y !== 'number') ? 0 : y;
    z = (typeof z !== 'number') ? -0.5 : z;

    // Setup the group's spatial position if no ID is passed.
    if (typeof id === 'undefined') {
      // Return the group's spatial position if no parameters are passed.
      if (typeof x === 'number') {
        self._pos = [x, y, z];
      } else {
        return self._pos;
      }
    }

    // Change the spatial position of one or all sounds in group.
    var ids = self._getSoundIds(id);
    for (var i=0; i<ids.length; i++) {
      // Get the sound.
      var sound = self._soundById(ids[i]);

      if (sound) {
        if (typeof x === 'number') {
          sound._pos = [x, y, z];

          if (sound._node) {
            // Check if there is a panner setup and create a new one if not.
            if (!sound._panner || sound._panner.pan) {
              setupPanner(sound, 'spatial');
            }

            if (typeof sound._panner.positionX !== 'undefined') {
              sound._panner.positionX.setValueAtTime(x, Howler.ctx.currentTime);
              sound._panner.positionY.setValueAtTime(y, Howler.ctx.currentTime);
              sound._panner.positionZ.setValueAtTime(z, Howler.ctx.currentTime);
            } else {
              sound._panner.setPosition(x, y, z);
            }
          }

          self._emit('pos', sound._id);
        } else {
          return sound._pos;
        }
      }
    }

    return self;
  };

  /**
   * Get/set the direction the audio source is pointing in the 3D cartesian coordinate
   * space. Depending on how direction the sound is, based on the `cone` attributes,
   * a sound pointing away from the listener can be quiet or silent.
   * @param  {Number} x  The x-orientation of the source.
   * @param  {Number} y  The y-orientation of the source.
   * @param  {Number} z  The z-orientation of the source.
   * @param  {Number} id (optional) The sound ID. If none is passed, all in group will be updated.
   * @return {Howl/Array}    Returns self or the current 3D spatial orientation: [x, y, z].
   */
  Howl.prototype.orientation = function(x, y, z, id) {
    var self = this;

    // Stop right here if not using Web Audio.
    if (!self._webAudio) {
      return self;
    }

    // If the sound hasn't loaded, add it to the load queue to change orientation when capable.
    if (self._state !== 'loaded') {
      self._queue.push({
        event: 'orientation',
        action: function() {
          self.orientation(x, y, z, id);
        }
      });

      return self;
    }

    // Set the defaults for optional 'y' & 'z'.
    y = (typeof y !== 'number') ? self._orientation[1] : y;
    z = (typeof z !== 'number') ? self._orientation[2] : z;

    // Setup the group's spatial orientation if no ID is passed.
    if (typeof id === 'undefined') {
      // Return the group's spatial orientation if no parameters are passed.
      if (typeof x === 'number') {
        self._orientation = [x, y, z];
      } else {
        return self._orientation;
      }
    }

    // Change the spatial orientation of one or all sounds in group.
    var ids = self._getSoundIds(id);
    for (var i=0; i<ids.length; i++) {
      // Get the sound.
      var sound = self._soundById(ids[i]);

      if (sound) {
        if (typeof x === 'number') {
          sound._orientation = [x, y, z];

          if (sound._node) {
            // Check if there is a panner setup and create a new one if not.
            if (!sound._panner) {
              // Make sure we have a position to setup the node with.
              if (!sound._pos) {
                sound._pos = self._pos || [0, 0, -0.5];
              }

              setupPanner(sound, 'spatial');
            }

            if (typeof sound._panner.orientationX !== 'undefined') {
              sound._panner.orientationX.setValueAtTime(x, Howler.ctx.currentTime);
              sound._panner.orientationY.setValueAtTime(y, Howler.ctx.currentTime);
              sound._panner.orientationZ.setValueAtTime(z, Howler.ctx.currentTime);
            } else {
              sound._panner.setOrientation(x, y, z);
            }
          }

          self._emit('orientation', sound._id);
        } else {
          return sound._orientation;
        }
      }
    }

    return self;
  };

  /**
   * Get/set the panner node's attributes for a sound or group of sounds.
   * This method can optionall take 0, 1 or 2 arguments.
   *   pannerAttr() -> Returns the group's values.
   *   pannerAttr(id) -> Returns the sound id's values.
   *   pannerAttr(o) -> Set's the values of all sounds in this Howl group.
   *   pannerAttr(o, id) -> Set's the values of passed sound id.
   *
   *   Attributes:
   *     coneInnerAngle - (360 by default) A parameter for directional audio sources, this is an angle, in degrees,
   *                      inside of which there will be no volume reduction.
   *     coneOuterAngle - (360 by default) A parameter for directional audio sources, this is an angle, in degrees,
   *                      outside of which the volume will be reduced to a constant value of `coneOuterGain`.
   *     coneOuterGain - (0 by default) A parameter for directional audio sources, this is the gain outside of the
   *                     `coneOuterAngle`. It is a linear value in the range `[0, 1]`.
   *     distanceModel - ('inverse' by default) Determines algorithm used to reduce volume as audio moves away from
   *                     listener. Can be `linear`, `inverse` or `exponential.
   *     maxDistance - (10000 by default) The maximum distance between source and listener, after which the volume
   *                   will not be reduced any further.
   *     refDistance - (1 by default) A reference distance for reducing volume as source moves further from the listener.
   *                   This is simply a variable of the distance model and has a different effect depending on which model
   *                   is used and the scale of your coordinates. Generally, volume will be equal to 1 at this distance.
   *     rolloffFactor - (1 by default) How quickly the volume reduces as source moves from listener. This is simply a
   *                     variable of the distance model and can be in the range of `[0, 1]` with `linear` and `[0, ∞]`
   *                     with `inverse` and `exponential`.
   *     panningModel - ('HRTF' by default) Determines which spatialization algorithm is used to position audio.
   *                     Can be `HRTF` or `equalpower`.
   *
   * @return {Howl/Object} Returns self or current panner attributes.
   */
  Howl.prototype.pannerAttr = function() {
    var self = this;
    var args = arguments;
    var o, id, sound;

    // Stop right here if not using Web Audio.
    if (!self._webAudio) {
      return self;
    }

    // Determine the values based on arguments.
    if (args.length === 0) {
      // Return the group's panner attribute values.
      return self._pannerAttr;
    } else if (args.length === 1) {
      if (typeof args[0] === 'object') {
        o = args[0];

        // Set the grou's panner attribute values.
        if (typeof id === 'undefined') {
          if (!o.pannerAttr) {
            o.pannerAttr = {
              coneInnerAngle: o.coneInnerAngle,
              coneOuterAngle: o.coneOuterAngle,
              coneOuterGain: o.coneOuterGain,
              distanceModel: o.distanceModel,
              maxDistance: o.maxDistance,
              refDistance: o.refDistance,
              rolloffFactor: o.rolloffFactor,
              panningModel: o.panningModel
            };
          }

          self._pannerAttr = {
            coneInnerAngle: typeof o.pannerAttr.coneInnerAngle !== 'undefined' ? o.pannerAttr.coneInnerAngle : self._coneInnerAngle,
            coneOuterAngle: typeof o.pannerAttr.coneOuterAngle !== 'undefined' ? o.pannerAttr.coneOuterAngle : self._coneOuterAngle,
            coneOuterGain: typeof o.pannerAttr.coneOuterGain !== 'undefined' ? o.pannerAttr.coneOuterGain : self._coneOuterGain,
            distanceModel: typeof o.pannerAttr.distanceModel !== 'undefined' ? o.pannerAttr.distanceModel : self._distanceModel,
            maxDistance: typeof o.pannerAttr.maxDistance !== 'undefined' ? o.pannerAttr.maxDistance : self._maxDistance,
            refDistance: typeof o.pannerAttr.refDistance !== 'undefined' ? o.pannerAttr.refDistance : self._refDistance,
            rolloffFactor: typeof o.pannerAttr.rolloffFactor !== 'undefined' ? o.pannerAttr.rolloffFactor : self._rolloffFactor,
            panningModel: typeof o.pannerAttr.panningModel !== 'undefined' ? o.pannerAttr.panningModel : self._panningModel
          };
        }
      } else {
        // Return this sound's panner attribute values.
        sound = self._soundById(parseInt(args[0], 10));
        return sound ? sound._pannerAttr : self._pannerAttr;
      }
    } else if (args.length === 2) {
      o = args[0];
      id = parseInt(args[1], 10);
    }

    // Update the values of the specified sounds.
    var ids = self._getSoundIds(id);
    for (var i=0; i<ids.length; i++) {
      sound = self._soundById(ids[i]);

      if (sound) {
        // Merge the new values into the sound.
        var pa = sound._pannerAttr;
        pa = {
          coneInnerAngle: typeof o.coneInnerAngle !== 'undefined' ? o.coneInnerAngle : pa.coneInnerAngle,
          coneOuterAngle: typeof o.coneOuterAngle !== 'undefined' ? o.coneOuterAngle : pa.coneOuterAngle,
          coneOuterGain: typeof o.coneOuterGain !== 'undefined' ? o.coneOuterGain : pa.coneOuterGain,
          distanceModel: typeof o.distanceModel !== 'undefined' ? o.distanceModel : pa.distanceModel,
          maxDistance: typeof o.maxDistance !== 'undefined' ? o.maxDistance : pa.maxDistance,
          refDistance: typeof o.refDistance !== 'undefined' ? o.refDistance : pa.refDistance,
          rolloffFactor: typeof o.rolloffFactor !== 'undefined' ? o.rolloffFactor : pa.rolloffFactor,
          panningModel: typeof o.panningModel !== 'undefined' ? o.panningModel : pa.panningModel
        };

        // Update the panner values or create a new panner if none exists.
        var panner = sound._panner;
        if (panner) {
          panner.coneInnerAngle = pa.coneInnerAngle;
          panner.coneOuterAngle = pa.coneOuterAngle;
          panner.coneOuterGain = pa.coneOuterGain;
          panner.distanceModel = pa.distanceModel;
          panner.maxDistance = pa.maxDistance;
          panner.refDistance = pa.refDistance;
          panner.rolloffFactor = pa.rolloffFactor;
          panner.panningModel = pa.panningModel;
        } else {
          // Make sure we have a position to setup the node with.
          if (!sound._pos) {
            sound._pos = self._pos || [0, 0, -0.5];
          }

          // Create a new panner node.
          setupPanner(sound, 'spatial');
        }
      }
    }

    return self;
  };

  /** Single Sound Methods **/
  /***************************************************************************/

  /**
   * Add new properties to the core Sound init.
   * @param  {Function} _super Core Sound init method.
   * @return {Sound}
   */
  Sound.prototype.init = (function(_super) {
    return function() {
      var self = this;
      var parent = self._parent;

      // Setup user-defined default properties.
      self._orientation = parent._orientation;
      self._stereo = parent._stereo;
      self._pos = parent._pos;
      self._pannerAttr = parent._pannerAttr;

      // Complete initilization with howler.js core Sound's init function.
      _super.call(this);

      // If a stereo or position was specified, set it up.
      if (self._stereo) {
        parent.stereo(self._stereo);
      } else if (self._pos) {
        parent.pos(self._pos[0], self._pos[1], self._pos[2], self._id);
      }
    };
  })(Sound.prototype.init);

  /**
   * Override the Sound.reset method to clean up properties from the spatial plugin.
   * @param  {Function} _super Sound reset method.
   * @return {Sound}
   */
  Sound.prototype.reset = (function(_super) {
    return function() {
      var self = this;
      var parent = self._parent;

      // Reset all spatial plugin properties on this sound.
      self._orientation = parent._orientation;
      self._stereo = parent._stereo;
      self._pos = parent._pos;
      self._pannerAttr = parent._pannerAttr;

      // If a stereo or position was specified, set it up.
      if (self._stereo) {
        parent.stereo(self._stereo);
      } else if (self._pos) {
        parent.pos(self._pos[0], self._pos[1], self._pos[2], self._id);
      } else if (self._panner) {
        // Disconnect the panner.
        self._panner.disconnect(0);
        self._panner = undefined;
        parent._refreshBuffer(self);
      }

      // Complete resetting of the sound.
      return _super.call(this);
    };
  })(Sound.prototype.reset);

  /** Helper Methods **/
  /***************************************************************************/

  /**
   * Create a new panner node and save it on the sound.
   * @param  {Sound} sound Specific sound to setup panning on.
   * @param {String} type Type of panner to create: 'stereo' or 'spatial'.
   */
  var setupPanner = function(sound, type) {
    type = type || 'spatial';

    // Create the new panner node.
    if (type === 'spatial') {
      sound._panner = Howler.ctx.createPanner();
      sound._panner.coneInnerAngle = sound._pannerAttr.coneInnerAngle;
      sound._panner.coneOuterAngle = sound._pannerAttr.coneOuterAngle;
      sound._panner.coneOuterGain = sound._pannerAttr.coneOuterGain;
      sound._panner.distanceModel = sound._pannerAttr.distanceModel;
      sound._panner.maxDistance = sound._pannerAttr.maxDistance;
      sound._panner.refDistance = sound._pannerAttr.refDistance;
      sound._panner.rolloffFactor = sound._pannerAttr.rolloffFactor;
      sound._panner.panningModel = sound._pannerAttr.panningModel;

      if (typeof sound._panner.positionX !== 'undefined') {
        sound._panner.positionX.setValueAtTime(sound._pos[0], Howler.ctx.currentTime);
        sound._panner.positionY.setValueAtTime(sound._pos[1], Howler.ctx.currentTime);
        sound._panner.positionZ.setValueAtTime(sound._pos[2], Howler.ctx.currentTime);
      } else {
        sound._panner.setPosition(sound._pos[0], sound._pos[1], sound._pos[2]);
      }

      if (typeof sound._panner.orientationX !== 'undefined') {
        sound._panner.orientationX.setValueAtTime(sound._orientation[0], Howler.ctx.currentTime);
        sound._panner.orientationY.setValueAtTime(sound._orientation[1], Howler.ctx.currentTime);
        sound._panner.orientationZ.setValueAtTime(sound._orientation[2], Howler.ctx.currentTime);
      } else {
        sound._panner.setOrientation(sound._orientation[0], sound._orientation[1], sound._orientation[2]);
      }
    } else {
      sound._panner = Howler.ctx.createStereoPanner();
      sound._panner.pan.setValueAtTime(sound._stereo, Howler.ctx.currentTime);
    }

    sound._panner.connect(sound._node);

    // Update the connections.
    if (!sound._paused) {
      sound._parent.pause(sound._id, true).play(sound._id, true);
    }
  };
})();

},{}],"sounds/beep.mp3":[function(require,module,exports) {
module.exports = "/beep.d0a13649.mp3";
},{}],"index.js":[function(require,module,exports) {
"use strict";

var _Main = require("./src/Main.elm");

require("./scss/style.scss");

var _howler = require("howler");

if (module.hot) {
  module.hot.dispose(function () {
    window.location.reload();
  });
}

var flags = {};

var app = _Main.Elm.Main.init({
  flags: flags
});

var beepUrl = require('./sounds/beep.mp3');

app.ports.playAudio.subscribe(function (audio) {
  var sound = new _howler.Howl({
    src: [audio]
  });
  sound.play();
  sound.once('play', function () {
    var audioCallback = {
      "eventType": "SoundStarted",
      "timestamp": Date.now(),
      "name": audio
    };
    app.ports.audioEnded.send(audioCallback);
    console.log({
      "sound started": audio,
      timestamp: Date.now()
    });
  });
  sound.once('end', function () {
    var audioCallback = {
      "eventType": "SoundEnded",
      timestamp: Date.now(),
      "name": audio
    };
    app.ports.audioEnded.send(audioCallback);
    console.log({
      'sound is over': audio,
      timestamp: Date.now()
    });
    console.log(audio);
  });
});
},{"./src/Main.elm":"src/Main.elm","./scss/style.scss":"scss/style.scss","howler":"node_modules/howler/dist/howler.js","./sounds/beep.mp3":"sounds/beep.mp3"}],"node_modules/parcel-bundler/src/builtins/hmr-runtime.js":[function(require,module,exports) {
var global = arguments[3];
var OVERLAY_ID = '__parcel__error__overlay__';
var OldModule = module.bundle.Module;

function Module(moduleName) {
  OldModule.call(this, moduleName);
  this.hot = {
    data: module.bundle.hotData,
    _acceptCallbacks: [],
    _disposeCallbacks: [],
    accept: function (fn) {
      this._acceptCallbacks.push(fn || function () {});
    },
    dispose: function (fn) {
      this._disposeCallbacks.push(fn);
    }
  };
  module.bundle.hotData = null;
}

module.bundle.Module = Module;
var checkedAssets, assetsToAccept;
var parent = module.bundle.parent;

if ((!parent || !parent.isParcelRequire) && typeof WebSocket !== 'undefined') {
  var hostname = "" || location.hostname;
  var protocol = location.protocol === 'https:' ? 'wss' : 'ws';
  var ws = new WebSocket(protocol + '://' + hostname + ':' + "55934" + '/');

  ws.onmessage = function (event) {
    checkedAssets = {};
    assetsToAccept = [];
    var data = JSON.parse(event.data);

    if (data.type === 'update') {
      var handled = false;
      data.assets.forEach(function (asset) {
        if (!asset.isNew) {
          var didAccept = hmrAcceptCheck(global.parcelRequire, asset.id);

          if (didAccept) {
            handled = true;
          }
        }
      }); // Enable HMR for CSS by default.

      handled = handled || data.assets.every(function (asset) {
        return asset.type === 'css' && asset.generated.js;
      });

      if (handled) {
        console.clear();
        data.assets.forEach(function (asset) {
          hmrApply(global.parcelRequire, asset);
        });
        assetsToAccept.forEach(function (v) {
          hmrAcceptRun(v[0], v[1]);
        });
      } else if (location.reload) {
        // `location` global exists in a web worker context but lacks `.reload()` function.
        location.reload();
      }
    }

    if (data.type === 'reload') {
      ws.close();

      ws.onclose = function () {
        location.reload();
      };
    }

    if (data.type === 'error-resolved') {
      console.log('[parcel] ✨ Error resolved');
      removeErrorOverlay();
    }

    if (data.type === 'error') {
      console.error('[parcel] 🚨  ' + data.error.message + '\n' + data.error.stack);
      removeErrorOverlay();
      var overlay = createErrorOverlay(data);
      document.body.appendChild(overlay);
    }
  };
}

function removeErrorOverlay() {
  var overlay = document.getElementById(OVERLAY_ID);

  if (overlay) {
    overlay.remove();
  }
}

function createErrorOverlay(data) {
  var overlay = document.createElement('div');
  overlay.id = OVERLAY_ID; // html encode message and stack trace

  var message = document.createElement('div');
  var stackTrace = document.createElement('pre');
  message.innerText = data.error.message;
  stackTrace.innerText = data.error.stack;
  overlay.innerHTML = '<div style="background: black; font-size: 16px; color: white; position: fixed; height: 100%; width: 100%; top: 0px; left: 0px; padding: 30px; opacity: 0.85; font-family: Menlo, Consolas, monospace; z-index: 9999;">' + '<span style="background: red; padding: 2px 4px; border-radius: 2px;">ERROR</span>' + '<span style="top: 2px; margin-left: 5px; position: relative;">🚨</span>' + '<div style="font-size: 18px; font-weight: bold; margin-top: 20px;">' + message.innerHTML + '</div>' + '<pre>' + stackTrace.innerHTML + '</pre>' + '</div>';
  return overlay;
}

function getParents(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return [];
  }

  var parents = [];
  var k, d, dep;

  for (k in modules) {
    for (d in modules[k][1]) {
      dep = modules[k][1][d];

      if (dep === id || Array.isArray(dep) && dep[dep.length - 1] === id) {
        parents.push(k);
      }
    }
  }

  if (bundle.parent) {
    parents = parents.concat(getParents(bundle.parent, id));
  }

  return parents;
}

function hmrApply(bundle, asset) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (modules[asset.id] || !bundle.parent) {
    var fn = new Function('require', 'module', 'exports', asset.generated.js);
    asset.isNew = !modules[asset.id];
    modules[asset.id] = [fn, asset.deps];
  } else if (bundle.parent) {
    hmrApply(bundle.parent, asset);
  }
}

function hmrAcceptCheck(bundle, id) {
  var modules = bundle.modules;

  if (!modules) {
    return;
  }

  if (!modules[id] && bundle.parent) {
    return hmrAcceptCheck(bundle.parent, id);
  }

  if (checkedAssets[id]) {
    return;
  }

  checkedAssets[id] = true;
  var cached = bundle.cache[id];
  assetsToAccept.push([bundle, id]);

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    return true;
  }

  return getParents(global.parcelRequire, id).some(function (id) {
    return hmrAcceptCheck(global.parcelRequire, id);
  });
}

function hmrAcceptRun(bundle, id) {
  var cached = bundle.cache[id];
  bundle.hotData = {};

  if (cached) {
    cached.hot.data = bundle.hotData;
  }

  if (cached && cached.hot && cached.hot._disposeCallbacks.length) {
    cached.hot._disposeCallbacks.forEach(function (cb) {
      cb(bundle.hotData);
    });
  }

  delete bundle.cache[id];
  bundle(id);
  cached = bundle.cache[id];

  if (cached && cached.hot && cached.hot._acceptCallbacks.length) {
    cached.hot._acceptCallbacks.forEach(function (cb) {
      cb();
    });

    return true;
  }
}
},{}]},{},["node_modules/parcel-bundler/src/builtins/hmr-runtime.js","index.js"], null)
//# sourceMappingURL=/spacing-and-learning.e31bb0bc.js.map