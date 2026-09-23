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

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
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

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
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


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
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
	if (region.b2.aT === region.ck.aT)
	{
		return 'on line ' + region.b2.aT;
	}
	return 'on lines ' + region.b2.aT + ' through ' + region.ck.aT;
}



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

	/**_UNUSED/
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

	/**/
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

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
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

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


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



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


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



/**_UNUSED/
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
				if (Object.prototype.hasOwnProperty.call(value, key))
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

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	var unwrapped = _Json_unwrap(value);
	if (!(key === 'toJSON' && typeof unwrapped === 'function'))
	{
		object[key] = unwrapped;
	}
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
		impl.dQ,
		impl.ew,
		impl.eo,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
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


function _Platform_export(exports)
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


function _Platform_export_UNUSED(exports)
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

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
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
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'outerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (
		(typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		||
		(Array.isArray(_Json_unwrap(value)) && _VirtualDom_RE_js_html.test(String(_Json_unwrap(value))))
	)
		? _Json_wrap(
			/**/''//*//**_UNUSED/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
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
		cA: func(record.cA),
		b5: record.b5,
		bP: record.bP
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
		var message = !tag ? value : tag < 3 ? value.a : value.cA;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.b5;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.bP) && event.preventDefault(),
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




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.dQ,
		impl.ew,
		impl.eo,
		function(sendToApp, initialModel) {
			var view = impl.c0;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
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
		impl.dQ,
		impl.ew,
		impl.eo,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.b$ && impl.b$(sendToApp)
			var view = impl.c0;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.am);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.F) && (_VirtualDom_doc.title = title = doc.F);
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
	var onUrlChange = impl.ea;
	var onUrlRequest = impl.eb;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		b$: function(sendToApp)
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
							&& curr.cN === next.cN
							&& curr.cs === next.cs
							&& curr.cK.a === next.cK.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		dQ: function(flags)
		{
			return A3(impl.dQ, flags, _Browser_getUrl(), key);
		},
		c0: impl.c0,
		ew: impl.ew,
		eo: impl.eo
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
		? { dN: 'hidden', dm: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { dN: 'mozHidden', dm: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { dN: 'msHidden', dm: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { dN: 'webkitHidden', dm: 'webkitvisibilitychange' }
		: { dN: 'hidden', dm: 'visibilitychange' };
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
		cT: _Browser_getScene(),
		c1: {
			c4: _Browser_window.pageXOffset,
			c5: _Browser_window.pageYOffset,
			c3: _Browser_doc.documentElement.clientWidth,
			cr: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		c3: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		cr: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			cT: {
				c3: node.scrollWidth,
				cr: node.scrollHeight
			},
			c1: {
				c4: node.scrollLeft,
				c5: node.scrollTop,
				c3: node.clientWidth,
				cr: node.clientHeight
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
			cT: _Browser_getScene(),
			c1: {
				c4: x,
				c5: y,
				c3: _Browser_doc.documentElement.clientWidth,
				cr: _Browser_doc.documentElement.clientHeight
			},
			dw: {
				c4: x + rect.left,
				c5: y + rect.top,
				c3: rect.width,
				cr: rect.height
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
}var $author$project$Main$FromJs = function (a) {
	return {$: 0, a: a};
};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
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
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
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
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
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
		'\u000A    ',
		A2($elm$core$String$split, '\u000A', str));
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
		return '\u000A\u000A(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
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
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\u0027' + (f + '\u0027]'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
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
								'\u000A\u000A',
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
							return 'Problem with the given value:\u000A\u000A';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\u000A\u000A    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\u000A\u000A' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
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
	return {$: 1, a: a};
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
	return {$: 0, a: a};
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
		if (!builder.l) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.o),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.o);
		} else {
			var treeLen = builder.l * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.p) : builder.p;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.l);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.o) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.o);
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
					{p: nodeList, l: (len / $elm$core$Array$branchFactor) | 0, o: tail});
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
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {cm: fragment, cs: host, bi: path, cK: port_, cN: protocol, c: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
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
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
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
					if (_v1.$ === 1) {
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
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
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
		var task = _v0;
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
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$fromJs = _Platform_incomingPort('fromJs', $elm$json$Json$Decode$value);
var $author$project$Main$QueryRead = 1;
var $author$project$Main$RepositoryRead = 0;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$View$Forms$Create = 0;
var $author$project$View$Forms$initial = {aw: 0, aa: '', ab: 'human', ac: '', aO: '', dd: '', ak: $elm$core$Maybe$Nothing, de: '', am: '', ax: '', bv: '', dt: false, L: '', bD: '', bJ: 'delta', aV: $elm$core$Maybe$Nothing, bQ: '', cO: '', bV: '', aY: '', bY: '', eg: false, bZ: false, aZ: $elm$core$Maybe$Nothing, bl: _List_Nil, O: '', b1: false, a_: '', Q: '', es: '', F: ''};
var $author$project$Route$Browse = 0;
var $author$project$Main$initialQuery = {a3: '', J: '', a8: 'HEAD~1', a9: 'HEAD', by: '', dF: '', bd: false, cz: 100, bJ: 'hybrid', bh: 'newest', bj: 'collapsed', aG: 'HEAD', b0: false, aH: '', b7: '', aK: '', c0: 0, ex: false};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
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
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$json$Json$Encode$null = _Json_encodeNull;
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
			_Json_emptyObject(0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Api$request = F4(
	function (requestId, method, path, body) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('request')),
					_Utils_Tuple2(
					'request_id',
					$elm$json$Json$Encode$string(requestId)),
					_Utils_Tuple2(
					'method',
					$elm$json$Json$Encode$string(method)),
					_Utils_Tuple2(
					'path',
					$elm$json$Json$Encode$string(path)),
					_Utils_Tuple2(
					'body',
					A2($elm$core$Maybe$withDefault, $elm$json$Json$Encode$null, body))
				]));
	});
var $author$project$Main$requestKey = function (kind) {
	switch (kind) {
		case 0:
			return 'repository';
		case 1:
			return 'query';
		case 2:
			return 'collapsed';
		case 3:
			return 'exploded';
		default:
			return 'mutation';
	}
};
var $author$project$Main$toJs = _Platform_outgoingPort('toJs', $elm$core$Basics$identity);
var $author$project$Main$issue = F5(
	function (kind, method, path, body, model) {
		if (model.k) {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var pending = {az: path, z: model.z, A: model.A, be: kind};
			var identifier = 'ui-' + $elm$core$String$fromInt(model.aD);
			var next = _Utils_update(
				model,
				{
					aS: A3(
						$elm$core$Dict$insert,
						$author$project$Main$requestKey(kind),
						identifier,
						model.aS),
					aD: model.aD + 1,
					W: A3($elm$core$Dict$insert, identifier, pending, model.W)
				});
			return _Utils_Tuple2(
				next,
				$author$project$Main$toJs(
					A4($author$project$Api$request, identifier, method, path, body)));
		}
	});
var $author$project$Route$bool = function (value) {
	return value ? 'true' : 'false';
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $elm$core$String$trim = _String_trim;
var $author$project$Route$optional = F2(
	function (key, value) {
		return ($elm$core$String$trim(value) === '') ? _List_Nil : _List_fromArray(
			[
				_Utils_Tuple2(key, value)
			]);
	});
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $author$project$Route$path = F2(
	function (base, fields) {
		return base + ('?' + A2(
			$elm$core$String$join,
			'&',
			A2(
				$elm$core$List$map,
				function (_v0) {
					var key = _v0.a;
					var value = _v0.b;
					return $elm$url$Url$percentEncode(key) + ('=' + $elm$url$Url$percentEncode(value));
				},
				fields)));
	});
var $author$project$Route$queryPath = function (query) {
	var revision = ($elm$core$String$trim(query.aG) === '') ? 'HEAD' : query.aG;
	var resultLimit = $elm$core$String$fromInt(
		A3($elm$core$Basics$clamp, 1, 1000, query.cz));
	var filter = _Utils_ap(
		A2($author$project$Route$optional, 'domain', query.by),
		_Utils_ap(
			A2($author$project$Route$optional, 'file', query.dF),
			_Utils_ap(
				A2($author$project$Route$optional, 'actor', query.a3),
				_Utils_ap(
					A2($author$project$Route$optional, 'since', query.aH),
					A2($author$project$Route$optional, 'until', query.aK)))));
	var at = _List_fromArray(
		[
			_Utils_Tuple2('at', revision)
		]);
	var _v0 = query.c0;
	switch (_v0) {
		case 0:
			return A2(
				$author$project$Route$path,
				'/api/v1/search',
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2('q', ''),
							_Utils_Tuple2('mode', query.bJ),
							_Utils_Tuple2('view', query.bj),
							_Utils_Tuple2('limit', resultLimit),
							_Utils_Tuple2(
							'include_obsolete',
							$author$project$Route$bool(query.bd)),
							_Utils_Tuple2(
							'shallow',
							$author$project$Route$bool(query.b0))
						]),
					_Utils_ap(at, filter)));
		case 1:
			return A2(
				$author$project$Route$path,
				'/api/v1/search',
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2('q', query.b7),
							_Utils_Tuple2('mode', query.bJ),
							_Utils_Tuple2('view', query.bj),
							_Utils_Tuple2('limit', resultLimit),
							_Utils_Tuple2(
							'include_obsolete',
							$author$project$Route$bool(query.bd)),
							_Utils_Tuple2(
							'shallow',
							$author$project$Route$bool(query.b0))
						]),
					_Utils_ap(at, filter)));
		case 2:
			return A2(
				$author$project$Route$path,
				'/api/v1/relevant',
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2('file', query.dF),
							_Utils_Tuple2(
							'limit',
							$elm$core$String$fromInt(
								A3($elm$core$Basics$clamp, 1, 100, query.cz))),
							_Utils_Tuple2(
							'include_obsolete',
							$author$project$Route$bool(query.bd)),
							_Utils_Tuple2(
							'worktree',
							$author$project$Route$bool(query.ex))
						]),
					query.ex ? _List_Nil : at));
		case 3:
			return A2(
				$author$project$Route$path,
				'/api/v1/history',
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2('limit', resultLimit),
							_Utils_Tuple2('order', query.bh)
						]),
					_Utils_ap(
						at,
						_Utils_ap(
							A2($author$project$Route$optional, 'adr', query.J),
							_Utils_ap(
								A2($author$project$Route$optional, 'actor', query.a3),
								_Utils_ap(
									A2($author$project$Route$optional, 'since', query.aH),
									A2($author$project$Route$optional, 'until', query.aK)))))));
		case 4:
			return A2(
				$author$project$Route$path,
				'/api/v1/compare',
				_List_fromArray(
					[
						_Utils_Tuple2('from', query.a8),
						_Utils_Tuple2('to', query.a9),
						_Utils_Tuple2('include_unchanged', 'false')
					]));
		case 5:
			return A2($author$project$Route$path, '/api/v1/conflicts', at);
		default:
			return A2($author$project$Route$path, '/api/v1/doctor', at);
	}
};
var $author$project$Route$repository = '/api/v1/repository';
var $author$project$Main$init = function (flags) {
	var base = {
		R: false,
		t: $elm$core$Dict$empty,
		S: false,
		ay: $elm$core$Maybe$Nothing,
		an: $elm$core$Maybe$Nothing,
		ba: $elm$core$Maybe$Nothing,
		b: $author$project$View$Forms$initial,
		z: 0,
		A: 0,
		g: $elm$core$Maybe$Nothing,
		I: false,
		m: flags.m,
		aC: $elm$core$Maybe$Nothing,
		s: $elm$core$Maybe$Nothing,
		v: $elm$core$Dict$empty,
		aS: $elm$core$Dict$empty,
		Z: $elm$core$Maybe$Nothing,
		aD: 1,
		M: $elm$core$Maybe$Nothing,
		e: '',
		j: 0,
		W: $elm$core$Dict$empty,
		c: $author$project$Main$initialQuery,
		aW: $elm$core$Maybe$Nothing,
		ag: 0,
		aX: $elm$core$Maybe$Nothing,
		N: $elm$core$Maybe$Nothing,
		q: false,
		ah: $elm$core$Maybe$Nothing,
		D: $elm$core$Maybe$Nothing,
		X: $elm$core$Maybe$Nothing,
		P: flags.m ? 'connecting' : 'unavailable',
		k: false,
		d: true,
		bs: '0'
	};
	var _v0 = A5($author$project$Main$issue, 0, 'GET', $author$project$Route$repository, $elm$core$Maybe$Nothing, base);
	var withRepository = _v0.a;
	var repositoryCommand = _v0.b;
	var _v1 = A5(
		$author$project$Main$issue,
		1,
		'GET',
		$author$project$Route$queryPath($author$project$Main$initialQuery),
		$elm$core$Maybe$Nothing,
		withRepository);
	var withQuery = _v1.a;
	var queryCommand = _v1.b;
	return _Utils_Tuple2(
		withQuery,
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[
					repositoryCommand,
					queryCommand,
					flags.m ? $author$project$Main$toJs(
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'type',
								$elm$json$Json$Encode$string('connect'))
							]))) : $elm$core$Platform$Cmd$none
				])));
};
var $author$project$View$Forms$candidateLabels = function (inspection) {
	return _Utils_ap(
		A2(
			$elm$core$List$map,
			function (candidate) {
				return candidate.bc + (' · ' + (candidate.F + (' · ' + candidate.Q)));
			},
			inspection.a4.cP),
		_Utils_ap(
			A2(
				$elm$core$List$map,
				function (candidate) {
					return candidate.bc + (' · ' + candidate.Q);
				},
				inspection.a4.O),
			A2(
				$elm$core$List$map,
				function (candidate) {
					return candidate.bc + (' · ' + candidate.Q);
				},
				inspection.a4.L)));
};
var $author$project$View$Forms$heads = function (inspection) {
	return _Utils_ap(
		inspection.bU,
		_Utils_ap(
			inspection.b_,
			_Utils_ap(inspection.bz, inspection.b4)));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$View$Forms$baseline = F2(
	function (repository, inspection) {
		return {
			de: repository.a_,
			am: A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.am;
					},
					inspection)),
			a4: A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2($elm$core$Maybe$map, $author$project$View$Forms$candidateLabels, inspection)),
			L: A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.L;
					},
					inspection)),
			dK: repository.dK,
			dL: repository.dL,
			dM: A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2($elm$core$Maybe$map, $author$project$View$Forms$heads, inspection)),
			O: A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.O;
					},
					inspection)),
			a_: A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.a_;
					},
					inspection)),
			Q: A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.Q;
					},
					inspection)),
			F: A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.F;
					},
					inspection))
		};
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$View$Forms$adopt = F3(
	function (repository, inspection, draft) {
		return (!_Utils_eq(draft.es, inspection.J)) ? $elm$core$Result$Err('Inspect the draft\u0027s target ADR before adopting tokens.') : ((!_Utils_eq(inspection.aj, repository.dK)) ? $elm$core$Result$Err('Inspect the exact current repository HEAD before adopting tokens.') : $elm$core$Result$Ok(
			_Utils_update(
				draft,
				{
					dd: repository.dK,
					ak: repository.dL,
					de: repository.a_,
					bZ: true,
					aZ: $elm$core$Maybe$Just(
						A2(
							$author$project$View$Forms$baseline,
							repository,
							$elm$core$Maybe$Just(inspection))),
					bl: $author$project$View$Forms$heads(inspection),
					b1: false,
					a_: inspection.a_
				})));
	});
var $author$project$View$Forms$adoptCreate = F2(
	function (repository, draft) {
		return _Utils_update(
			draft,
			{
				dd: repository.dK,
				ak: repository.dL,
				de: repository.a_,
				bZ: true,
				aZ: $elm$core$Maybe$Just(
					A2($author$project$View$Forms$baseline, repository, $elm$core$Maybe$Nothing)),
				b1: false
			});
	});
var $author$project$View$Forms$begin = F4(
	function (action, repository, inspection, previous) {
		var selected = function () {
			var _v0 = (!action) ? $elm$core$Maybe$Nothing : inspection;
			if (_v0.$ === 1) {
				return $author$project$View$Forms$initial;
			} else {
				var item = _v0.a;
				return _Utils_update(
					$author$project$View$Forms$initial,
					{
						am: item.am,
						L: A2($elm$core$String$join, '\u000A', item.L),
						bl: $author$project$View$Forms$heads(item),
						O: A2($elm$core$String$join, '\u000A', item.O),
						a_: item.a_,
						Q: item.Q,
						es: item.J,
						F: item.F
					});
			}
		}();
		return _Utils_update(
			selected,
			{
				aw: action,
				aa: previous.aa,
				ab: previous.ab,
				ac: previous.ac,
				dd: repository.dK,
				ak: repository.dL,
				de: repository.a_,
				aV: $elm$core$Maybe$Just(
					A2(
						$author$project$View$Forms$baseline,
						repository,
						(!action) ? $elm$core$Maybe$Nothing : inspection)),
				bZ: _Utils_eq(
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.aj;
						},
						inspection),
					$elm$core$Maybe$Just(repository.dK)) && (_Utils_eq(
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.c0;
						},
						inspection),
					$elm$core$Maybe$Just('collapsed')) && (_Utils_eq(
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.aF;
						},
						inspection),
					$elm$core$Maybe$Just(false)) && (!(!action)))),
				aZ: $elm$core$Maybe$Nothing,
				b1: (!_Utils_eq(inspection, $elm$core$Maybe$Nothing)) && ((!_Utils_eq(
					A2(
						$elm$core$Maybe$map,
						function ($) {
							return $.aj;
						},
						inspection),
					$elm$core$Maybe$Just(repository.dK))) && (!(!action)))
			});
	});
var $author$project$View$Forms$change = F3(
	function (field, content, draft) {
		var modified = function () {
			switch (field) {
				case 0:
					return _Utils_update(
						draft,
						{F: content});
				case 1:
					return _Utils_update(
						draft,
						{Q: content});
				case 2:
					return _Utils_update(
						draft,
						{am: content});
				case 3:
					return _Utils_update(
						draft,
						{ax: content});
				case 4:
					return _Utils_update(
						draft,
						{cO: content});
				case 5:
					return _Utils_update(
						draft,
						{L: content});
				case 6:
					return _Utils_update(
						draft,
						{O: content});
				case 7:
					return _Utils_update(
						draft,
						{aO: content});
				case 8:
					return _Utils_update(
						draft,
						{aY: content});
				case 9:
					return _Utils_update(
						draft,
						{bV: content});
				case 10:
					return _Utils_update(
						draft,
						{bY: content});
				case 11:
					return _Utils_update(
						draft,
						{ab: content});
				case 12:
					return _Utils_update(
						draft,
						{aa: content});
				case 13:
					return _Utils_update(
						draft,
						{ac: content});
				case 14:
					return _Utils_update(
						draft,
						{bD: content});
				case 15:
					return _Utils_update(
						draft,
						{bQ: content});
				case 16:
					return _Utils_update(
						draft,
						{bv: content});
				default:
					return _Utils_update(
						draft,
						{bJ: content});
			}
		}();
		return _Utils_update(
			modified,
			{dt: true});
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$inspectionReady = function (model) {
	return (!model.k) && (model.q && (model.S && (model.I && (_Utils_eq(
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.c0;
			},
			model.s),
		$elm$core$Maybe$Just('collapsed')) && (_Utils_eq(
		model.X,
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.dK;
			},
			model.N)) && _Utils_eq(
		model.D,
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.J;
			},
			model.s)))))));
};
var $author$project$Main$isHistorical = function (model) {
	return (model.c.aG !== 'HEAD') || ((!_Utils_eq(model.D, $elm$core$Maybe$Nothing)) && (!_Utils_eq(
		model.X,
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.dK;
			},
			model.N))));
};
var $author$project$Route$Relevant = 2;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $author$project$Route$Compare = 4;
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Main$signedDecimal = function (raw) {
	var digits = A2($elm$core$String$startsWith, '-', raw) ? A2($elm$core$String$dropLeft, 1, raw) : raw;
	return (!$elm$core$String$isEmpty(digits)) && A2(
		$elm$core$String$all,
		function (c) {
			return (c >= '0') && (c <= '9');
		},
		digits);
};
var $author$project$Main$validateQuery = function (query) {
	return ((query.cz < 1) || (_Utils_cmp(
		query.cz,
		(query.c0 === 2) ? 100 : 1000) > 0)) ? $elm$core$Maybe$Just('Window limit must be between 1 and 1000 (100 for relevance).') : (((query.c0 === 2) && ($elm$core$String$trim(query.dF) === '')) ? $elm$core$Maybe$Just('Choose a repository-relative file for relevance.') : (((query.c0 === 2) && (query.ex && (query.aG !== 'HEAD'))) ? $elm$core$Maybe$Just('Worktree relevance cannot use an explicit revision.') : (((query.c0 === 4) && (($elm$core$String$trim(query.a8) === '') || ($elm$core$String$trim(query.a9) === ''))) ? $elm$core$Maybe$Just('Choose both comparison revisions.') : (((!($elm$core$String$isEmpty(query.aH) || $author$project$Main$signedDecimal(query.aH))) || (!($elm$core$String$isEmpty(query.aK) || $author$project$Main$signedDecimal(query.aK)))) ? $elm$core$Maybe$Just('Time filters must be signed Unix milliseconds.') : $elm$core$Maybe$Nothing))));
};
var $author$project$Main$loadActive = function (model) {
	var _v0 = $author$project$Main$validateQuery(model.c);
	if (!_v0.$) {
		var problem = _v0.a;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					g: $elm$core$Maybe$Just(problem),
					d: true
				}),
			$elm$core$Platform$Cmd$none);
	} else {
		var path = $author$project$Route$queryPath(model.c);
		var interest = ((model.c.c0 === 2) && model.c.ex) ? _List_fromArray(
			[model.c.dF]) : _List_Nil;
		var _v1 = A5(
			$author$project$Main$issue,
			1,
			'GET',
			path,
			$elm$core$Maybe$Nothing,
			_Utils_update(
				model,
				{g: $elm$core$Maybe$Nothing, j: 0, d: true}));
		var next = _v1.a;
		var command = _v1.b;
		return _Utils_Tuple2(
			next,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						command,
						model.m ? $author$project$Main$toJs(
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'type',
									$elm$json$Json$Encode$string('active-files')),
									_Utils_Tuple2(
									'paths',
									A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, interest))
								]))) : $elm$core$Platform$Cmd$none
					])));
	}
};
var $author$project$Main$load = function (model) {
	return model.k ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : $author$project$Main$loadActive(model);
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $author$project$Route$pageSize = 100;
var $author$project$Route$pageCount = function (items) {
	return A2(
		$elm$core$Basics$max,
		1,
		((($elm$core$List$length(items) + $author$project$Route$pageSize) - 1) / $author$project$Route$pageSize) | 0);
};
var $author$project$Main$maxPage = function (model) {
	var _v0 = model.c.c0;
	switch (_v0) {
		case 0:
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				A2(
					$elm$core$Maybe$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.cR;
						},
						A2(
							$elm$core$Basics$composeR,
							$author$project$Route$pageCount,
							function (n) {
								return n - 1;
							})),
					model.ah));
		case 1:
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				A2(
					$elm$core$Maybe$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.cR;
						},
						A2(
							$elm$core$Basics$composeR,
							$author$project$Route$pageCount,
							function (n) {
								return n - 1;
							})),
					model.ah));
		case 3:
			return A2(
				$elm$core$Maybe$withDefault,
				0,
				A2(
					$elm$core$Maybe$map,
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.bg;
						},
						A2(
							$elm$core$Basics$composeR,
							$author$project$Route$pageCount,
							function (n) {
								return n - 1;
							})),
					model.aC));
		default:
			return 0;
	}
};
var $author$project$Main$CollapsedRead = 2;
var $author$project$Main$ExplodedRead = 3;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
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
var $author$project$Route$showPath = F3(
	function (adr, revision, projection) {
		return A2(
			$author$project$Route$path,
			'/api/v1/adrs/' + $elm$url$Url$percentEncode(adr),
			_List_fromArray(
				[
					_Utils_Tuple2('at', revision),
					_Utils_Tuple2('view', projection)
				]));
	});
var $author$project$Main$selectedPath = F2(
	function (viewMode, model) {
		var _v0 = _Utils_Tuple2(model.D, model.X);
		if ((!_v0.a.$) && (!_v0.b.$)) {
			var adr = _v0.a.a;
			var revision = _v0.b.a;
			return A3($author$project$Route$showPath, adr, revision, viewMode);
		} else {
			return '';
		}
	});
var $author$project$Main$readPendingCurrent = F3(
	function (requestId, pending, model) {
		return (!model.k) && (_Utils_eq(
			A2(
				$elm$core$Dict$get,
				$author$project$Main$requestKey(pending.be),
				model.aS),
			$elm$core$Maybe$Just(requestId)) && (_Utils_eq(pending.A, model.A) && (((pending.be !== 1) || _Utils_eq(
			pending.az,
			$author$project$Route$queryPath(model.c))) && (((pending.be !== 2) || _Utils_eq(
			pending.az,
			A2($author$project$Main$selectedPath, 'collapsed', model))) && ((pending.be !== 3) || _Utils_eq(
			pending.az,
			A2($author$project$Main$selectedPath, 'exploded', model)))))));
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$Api$Event = F4(
	function (generation, asOf, kind, facts) {
		return {aj: asOf, dD: facts, cn: generation, be: kind};
	});
var $author$project$Api$AtCommit = function (a) {
	return {$: 0, a: a};
};
var $author$project$Api$AtComparison = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$Api$Unavailable = function (a) {
	return {$: 2, a: a};
};
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Api$asOf = A2(
	$elm$json$Json$Decode$andThen,
	function (kind) {
		switch (kind) {
			case 'commit':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Api$AtCommit,
					A2($elm$json$Json$Decode$field, 'oid', $elm$json$Json$Decode$string));
			case 'comparison':
				return A3(
					$elm$json$Json$Decode$map2,
					$author$project$Api$AtComparison,
					A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$string),
					A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$string));
			case 'unavailable':
				return A2(
					$elm$json$Json$Decode$map,
					$author$project$Api$Unavailable,
					A2($elm$json$Json$Decode$field, 'reason', $elm$json$Json$Decode$string));
			default:
				return $elm$json$Json$Decode$fail('unknown as_of kind');
		}
	},
	A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $author$project$Api$validGeneration = function (raw) {
	var maxWord = '18446744073709551615';
	var digits = (!$elm$core$String$isEmpty(raw)) && A2(
		$elm$core$String$all,
		function (c) {
			return (c >= '0') && (c <= '9');
		},
		raw);
	var canonical = (raw === '0') || (!A2($elm$core$String$startsWith, '0', raw));
	return digits && (canonical && ((_Utils_cmp(
		$elm$core$String$length(raw),
		$elm$core$String$length(maxWord)) < 0) || (_Utils_eq(
		$elm$core$String$length(raw),
		$elm$core$String$length(maxWord)) && (_Utils_cmp(raw, maxWord) < 1))));
};
var $author$project$Api$generation = A2(
	$elm$json$Json$Decode$andThen,
	function (raw) {
		return $author$project$Api$validGeneration(raw) ? $elm$json$Json$Decode$succeed(raw) : $elm$json$Json$Decode$fail('generation must be a canonical unsigned decimal Word64 string');
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map4 = _Json_map4;
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
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $author$project$Api$lookupAt = F2(
	function (path, whole) {
		lookupAt:
		while (true) {
			if (!path.b) {
				return $elm$core$Result$Ok(
					$elm$core$Maybe$Just(whole));
			} else {
				var key = path.a;
				var rest = path.b;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					$elm$json$Json$Decode$null(0),
					whole);
				if (!_v1.$) {
					return $elm$core$Result$Ok($elm$core$Maybe$Nothing);
				} else {
					var _v2 = A2(
						$elm$json$Json$Decode$decodeValue,
						$elm$json$Json$Decode$dict($elm$json$Json$Decode$value),
						whole);
					if (_v2.$ === 1) {
						return $elm$core$Result$Err('invalid object before ' + key);
					} else {
						var object = _v2.a;
						var _v3 = A2($elm$core$Dict$get, key, object);
						if (_v3.$ === 1) {
							return $elm$core$Result$Ok($elm$core$Maybe$Nothing);
						} else {
							var value = _v3.a;
							var $temp$path = rest,
								$temp$whole = value;
							path = $temp$path;
							whole = $temp$whole;
							continue lookupAt;
						}
					}
				}
			}
		}
	});
var $author$project$Api$optionalAt = F3(
	function (path, decoder, fallback) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (whole) {
				var _v0 = A2($author$project$Api$lookupAt, path, whole);
				if (_v0.$ === 1) {
					var problem = _v0.a;
					return $elm$json$Json$Decode$fail(problem);
				} else {
					if (_v0.a.$ === 1) {
						var _v1 = _v0.a;
						return $elm$json$Json$Decode$succeed(fallback);
					} else {
						var raw = _v0.a.a;
						var _v2 = A2($elm$json$Json$Decode$decodeValue, decoder, raw);
						if (!_v2.$) {
							var decoded = _v2.a;
							return $elm$json$Json$Decode$succeed(decoded);
						} else {
							return $elm$json$Json$Decode$fail(
								'invalid ' + A2($elm$core$String$join, '.', path));
						}
					}
				}
			},
			$elm$json$Json$Decode$value);
	});
var $author$project$Api$event = A2(
	$elm$json$Json$Decode$andThen,
	function (schema) {
		return (schema !== 'adrai/events/v1') ? $elm$json$Json$Decode$fail('unsupported event schema') : A5(
			$elm$json$Json$Decode$map4,
			$author$project$Api$Event,
			A2($elm$json$Json$Decode$field, 'generation', $author$project$Api$generation),
			A2($elm$json$Json$Decode$field, 'as_of', $author$project$Api$asOf),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['event', 'type']),
				$elm$json$Json$Decode$string),
			A3(
				$author$project$Api$optionalAt,
				_List_fromArray(
					['event', 'facts']),
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
				_List_Nil));
	},
	A2($elm$json$Json$Decode$field, 'schema', $elm$json$Json$Decode$string));
var $author$project$Api$compareGeneration = F2(
	function (left, right) {
		var _v0 = A2(
			$elm$core$Basics$compare,
			$elm$core$String$length(left),
			$elm$core$String$length(right));
		if (_v0 === 1) {
			return A2($elm$core$Basics$compare, left, right);
		} else {
			var answer = _v0;
			return answer;
		}
	});
var $author$project$Main$fullResyncFacts = _List_fromArray(
	['repository-identity', 'head', 'index', 'sequencer', 'configuration', 'managed-source', 'common-refs', 'packed-refs', 'reflogs', 'worktree-metadata', 'relevant-worktree-file']);
var $author$project$View$Forms$markStale = function (draft) {
	return _Utils_update(
		draft,
		{bZ: false, b1: draft.dt || draft.b1});
};
var $author$project$Main$refreshActive = function (model) {
	var stale = _Utils_update(
		model,
		{
			t: $elm$core$Dict$empty,
			S: false,
			b: $author$project$View$Forms$markStale(model.b),
			A: model.A + 1,
			I: false,
			v: $elm$core$Dict$empty,
			q: false,
			d: true
		});
	var _v0 = A5($author$project$Main$issue, 0, 'GET', $author$project$Route$repository, $elm$core$Maybe$Nothing, stale);
	var withRepository = _v0.a;
	var repositoryCommand = _v0.b;
	var _v1 = $author$project$Main$load(withRepository);
	var withView = _v1.a;
	var viewCommand = _v1.b;
	return _Utils_Tuple2(
		withView,
		$elm$core$Platform$Cmd$batch(
			_List_fromArray(
				[repositoryCommand, viewCommand])));
};
var $author$project$Main$refresh = function (model) {
	return model.k ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : $author$project$Main$refreshActive(model);
};
var $author$project$Main$eventReceived = F2(
	function (event, model) {
		if (model.k || (A2($author$project$Api$compareGeneration, event.cn, model.bs) !== 2)) {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{R: false}),
				$elm$core$Platform$Cmd$none);
		} else {
			var recovered = function () {
				var _v1 = event.aj;
				if (!_v1.$) {
					return (event.be === 'repository-invalidated') && (_Utils_eq(
						event.dD,
						_List_fromArray(
							['repository-identity'])) || (model.R && _Utils_eq(event.dD, $author$project$Main$fullResyncFacts)));
				} else {
					return false;
				}
			}();
			var changed = _Utils_update(
				model,
				{
					R: false,
					b: $author$project$View$Forms$markStale(model.b),
					A: model.A + 1,
					M: recovered ? $elm$core$Maybe$Nothing : model.M,
					ag: (event.be === 'repository-invalidated') ? 0 : model.ag,
					d: true,
					bs: event.cn
				});
			if (event.be === 'observation-failed') {
				return $author$project$Main$refresh(
					_Utils_update(
						changed,
						{
							M: $elm$core$Maybe$Just('repository-observation-failed')
						}));
			} else {
				var _v0 = event.aj;
				if (_v0.$ === 2) {
					var reason = _v0.a;
					return (reason === 'repository-observation-failed') ? $author$project$Main$refresh(
						_Utils_update(
							changed,
							{
								M: $elm$core$Maybe$Just(reason)
							})) : $author$project$Main$refresh(
						_Utils_update(
							changed,
							{
								g: $elm$core$Maybe$Just('Live observation unavailable: ' + reason)
							}));
				} else {
					return $author$project$Main$refresh(changed);
				}
			}
		}
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$Main$MutationWrite = 4;
var $author$project$Main$RetryRead = F2(
	function (a, b) {
		return {$: 15, a: a, b: b};
	});
var $author$project$Api$Failure = F5(
	function (metadata, category, status, code, message) {
		return {dl: category, bu: code, cA: message, cB: metadata, at: status};
	});
var $elm$json$Json$Decode$map5 = _Json_map5;
var $author$project$Api$Metadata = F2(
	function (generation, asOf) {
		return {aj: asOf, cn: generation};
	});
var $author$project$Api$metadata = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Api$Metadata,
	A2($elm$json$Json$Decode$field, 'generation', $author$project$Api$generation),
	A2($elm$json$Json$Decode$field, 'as_of', $author$project$Api$asOf));
var $author$project$Api$failure = A2(
	$elm$json$Json$Decode$andThen,
	function (schema) {
		return (schema === 'adrai/api/v1') ? A6(
			$elm$json$Json$Decode$map5,
			$author$project$Api$Failure,
			A2($elm$json$Json$Decode$field, 'metadata', $author$project$Api$metadata),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['error', 'category']),
				$elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['error', 'status']),
				$elm$json$Json$Decode$int),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['error', 'code']),
				$elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$at,
				_List_fromArray(
					['error', 'message']),
				$elm$json$Json$Decode$string)) : $elm$json$Json$Decode$fail('unsupported API schema');
	},
	A2($elm$json$Json$Decode$field, 'schema', $elm$json$Json$Decode$string));
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
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
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Api$optional = F3(
	function (key, decoder, fallback) {
		return A3(
			$author$project$Api$optionalAt,
			_List_fromArray(
				[key]),
			decoder,
			fallback);
	});
var $author$project$Api$mutation = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A3(
			$elm$json$Json$Decode$map2,
			F2(
				function (indexError, warning) {
					return _Utils_update(
						base,
						{bC: indexError, bR: warning});
				}),
			A3(
				$author$project$Api$optional,
				'index_error',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
				$elm$core$Maybe$Nothing),
			A3(
				$author$project$Api$optional,
				'publication_warning',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
				$elm$core$Maybe$Nothing));
	},
	A6(
		$elm$json$Json$Decode$map5,
		F5(
			function (committed, operationId, commit, adr, indexed) {
				return {J: adr, a7: commit, cf: committed, bC: $elm$core$Maybe$Nothing, ct: indexed, aE: operationId, bR: $elm$core$Maybe$Nothing};
			}),
		A2($elm$json$Json$Decode$field, 'committed', $elm$json$Json$Decode$bool),
		A2($elm$json$Json$Decode$field, 'operation', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'commit', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'indexed', $elm$json$Json$Decode$bool)));
var $author$project$Api$Envelope = F2(
	function (metadata, data) {
		return {dp: data, cB: metadata};
	});
var $author$project$Api$response = function (dataDecoder) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (schema) {
			return (schema === 'adrai/api/v1') ? A3(
				$elm$json$Json$Decode$map2,
				$author$project$Api$Envelope,
				A2($elm$json$Json$Decode$field, 'metadata', $author$project$Api$metadata),
				A2($elm$json$Json$Decode$field, 'data', dataDecoder)) : $elm$json$Json$Decode$fail('unsupported API schema');
		},
		A2($elm$json$Json$Decode$field, 'schema', $elm$json$Json$Decode$string));
};
var $author$project$Main$terminalMessage = 'Event generation exhausted. Restart the web server, then reopen its new bootstrap URL.';
var $author$project$Main$terminalExhaustion = function (model) {
	var statusText = A2($elm$core$String$contains, $author$project$Main$terminalMessage, model.e) ? model.e : ($elm$core$String$isEmpty(model.e) ? $author$project$Main$terminalMessage : (model.e + (' ' + $author$project$Main$terminalMessage)));
	var draft = model.b;
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{
				R: false,
				t: $elm$core$Dict$empty,
				S: false,
				b: _Utils_update(
					draft,
					{bZ: false, b1: true}),
				A: model.A + 1,
				g: $elm$core$Maybe$Just($author$project$Main$terminalMessage),
				I: false,
				v: $elm$core$Dict$empty,
				M: $elm$core$Maybe$Nothing,
				e: statusText,
				q: false,
				P: 'unavailable',
				k: true,
				d: true
			}),
		$author$project$Main$toJs(
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('disconnect'))
					]))));
};
var $author$project$Main$mutationResponse = F5(
	function (requestId, pending, status, body, model) {
		var cleared = _Utils_update(
			model,
			{
				Z: _Utils_eq(
					model.Z,
					$elm$core$Maybe$Just(requestId)) ? $elm$core$Maybe$Nothing : model.Z
			});
		var _v0 = A2(
			$elm$json$Json$Decode$decodeValue,
			$author$project$Api$response($author$project$Api$mutation),
			body);
		if (!_v0.$) {
			var envelope = _v0.a;
			if ((status < 400) && envelope.dp.cf) {
				var outcome = envelope.dp;
				var warning = A2(
					$elm$core$String$join,
					'',
					A2(
						$elm$core$List$filterMap,
						$elm$core$Basics$identity,
						_List_fromArray(
							[
								A2(
								$elm$core$Maybe$map,
								function (value) {
									return ' Publication warning: ' + value;
								},
								outcome.bR),
								outcome.ct ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
								' Index warning: ' + A2($elm$core$Maybe$withDefault, 'index unavailable', outcome.bC))
							])));
				var statusText = 'Committed ' + (outcome.aE + (' at ' + (outcome.a7 + ('.' + warning))));
				var currentDraft = model.b;
				var safe = _Utils_update(
					cleared,
					{
						b: _Utils_eq(pending.z, model.z) ? _Utils_update(
							currentDraft,
							{bZ: false, b1: true}) : model.b,
						e: statusText,
						d: true
					});
				return (model.k || _Utils_eq(
					outcome.bR,
					$elm$core$Maybe$Just('generation-exhausted'))) ? $author$project$Main$terminalExhaustion(safe) : $author$project$Main$refresh(safe);
			} else {
				if (status >= 400) {
					return _Utils_Tuple2(
						_Utils_update(
							cleared,
							{
								b: $author$project$View$Forms$markStale(model.b),
								e: 'The response is inconsistent. Inspect operation history before another submission.'
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							cleared,
							{
								b: $author$project$View$Forms$markStale(model.b),
								e: 'Server did not confirm a commit. Inspect current repository state.'
							}),
						$elm$core$Platform$Cmd$none);
				}
			}
		} else {
			var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$Api$failure, body);
			if (!_v1.$) {
				var failure = _v1.a;
				return (!_Utils_eq(failure.at, status)) ? _Utils_Tuple2(
					_Utils_update(
						cleared,
						{
							b: $author$project$View$Forms$markStale(model.b),
							e: 'The response is inconsistent. Inspect operation history before another submission.'
						}),
					$elm$core$Platform$Cmd$none) : (((status === 503) && (failure.bu === 'generation-exhausted')) ? $author$project$Main$terminalExhaustion(
					_Utils_update(
						cleared,
						{e: 'Operation rejected (generation-exhausted): ' + failure.cA})) : ((status === 401) ? _Utils_Tuple2(
					_Utils_update(
						cleared,
						{
							b: $author$project$View$Forms$markStale(model.b),
							m: false,
							e: 'Session unavailable. Reopen the process bootstrap URL.',
							P: 'unavailable'
						}),
					$author$project$Main$toJs(
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'type',
									$elm$json$Json$Encode$string('disconnect'))
								])))) : _Utils_Tuple2(
					_Utils_update(
						cleared,
						{
							b: (status === 409) ? $author$project$View$Forms$markStale(model.b) : model.b,
							e: 'Operation rejected (' + (failure.bu + ('): ' + (failure.cA + ((status === 409) ? ' Refresh, inspect, and adopt tokens.' : ''))))
						}),
					$elm$core$Platform$Cmd$none)));
			} else {
				return _Utils_Tuple2(
					_Utils_update(
						cleared,
						{
							b: $author$project$View$Forms$markStale(model.b),
							e: 'The result is uncertain. Inspect repository and operation history before another submission.'
						}),
					$elm$core$Platform$Cmd$none);
			}
		}
	});
var $author$project$Main$readIssue = F3(
	function (kind, message, model) {
		switch (kind) {
			case 2:
				return _Utils_update(
					model,
					{
						v: A3($elm$core$Dict$insert, 'collapsed', message, model.v),
						d: true
					});
			case 3:
				return _Utils_update(
					model,
					{
						v: A3($elm$core$Dict$insert, 'exploded', message, model.v),
						d: true
					});
			default:
				return _Utils_update(
					model,
					{
						g: $elm$core$Maybe$Just(message),
						d: true
					});
		}
	});
var $author$project$Main$accept = F4(
	function (decoder, body, set, model) {
		var _v0 = A2(
			$elm$json$Json$Decode$decodeValue,
			$author$project$Api$response(decoder),
			body);
		if (!_v0.$) {
			var envelope = _v0.a;
			var _v1 = envelope.cB.aj;
			switch (_v1.$) {
				case 2:
					var reason = _v1.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								g: $elm$core$Maybe$Just('Snapshot unavailable: ' + reason),
								d: true
							}),
						$elm$core$Platform$Cmd$none);
				case 0:
					var oid = _v1.a;
					return _Utils_Tuple2(
						A2(
							set,
							envelope.dp,
							_Utils_update(
								model,
								{
									g: $elm$core$Maybe$Nothing,
									aW: $elm$core$Maybe$Just(oid)
								})),
						$elm$core$Platform$Cmd$none);
				default:
					var toOid = _v1.b;
					return _Utils_Tuple2(
						A2(
							set,
							envelope.dp,
							_Utils_update(
								model,
								{
									g: $elm$core$Maybe$Nothing,
									aW: $elm$core$Maybe$Just(toOid)
								})),
						$elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						g: $elm$core$Maybe$Just('Response has an unsupported shape.'),
						d: true
					}),
				$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Api$CompareWindow = F3(
	function (from, to, entries) {
		return {dx: entries, dJ: from, et: to};
	});
var $author$project$Api$CompareEntry = F6(
	function (adr, title, kind, before, after, changes) {
		return {J: adr, b8: after, cc: before, a5: changes, be: kind, F: title};
	});
var $author$project$Api$CompareChange = F4(
	function (field, before, after, diff) {
		return {b8: after, cc: before, ds: diff, dE: field};
	});
var $author$project$Api$readableValue = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			$elm$json$Json$Decode$string,
			A2(
			$elm$json$Json$Decode$map,
			$elm$core$String$join(', '),
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
			A2(
			$elm$json$Json$Decode$map,
			function (value) {
				return value ? 'true' : 'false';
			},
			$elm$json$Json$Decode$bool),
			A2($elm$json$Json$Decode$map, $elm$core$String$fromInt, $elm$json$Json$Decode$int),
			$elm$json$Json$Decode$null('')
		]));
var $author$project$Api$compareChange = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$CompareChange,
	A2($elm$json$Json$Decode$field, 'field', $elm$json$Json$Decode$string),
	A3($author$project$Api$optional, 'before', $author$project$Api$readableValue, ''),
	A3($author$project$Api$optional, 'after', $author$project$Api$readableValue, ''),
	A3(
		$author$project$Api$optional,
		'diff',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
		$elm$core$Maybe$Nothing));
var $author$project$Api$CompareSnapshot = F6(
	function (title, summary, body, status, domains, scopes) {
		return {am: body, L: domains, O: scopes, at: status, Q: summary, F: title};
	});
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Api$compareSnapshot = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Api$CompareSnapshot,
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'body', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'domains',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'applies_to',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
var $author$project$Api$compareEntry = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Api$CompareEntry,
	A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'before',
		$elm$json$Json$Decode$nullable($author$project$Api$compareSnapshot)),
	A2(
		$elm$json$Json$Decode$field,
		'after',
		$elm$json$Json$Decode$nullable($author$project$Api$compareSnapshot)),
	A2(
		$elm$json$Json$Decode$field,
		'changes',
		$elm$json$Json$Decode$list($author$project$Api$compareChange)));
var $author$project$Api$comparison = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Api$CompareWindow,
	A2($elm$json$Json$Decode$field, 'from', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'to', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'entries',
		$elm$json$Json$Decode$list($author$project$Api$compareEntry)));
var $author$project$Api$ConflictWindow = function (conflicts) {
	return {an: conflicts};
};
var $author$project$Api$ConflictEntry = F4(
	function (adr, code, summaries, candidates) {
		return {J: adr, a4: candidates, bu: code, ep: summaries};
	});
var $author$project$Api$ConflictCandidate = F3(
	function (axis, heads, summary) {
		return {da: axis, dM: heads, Q: summary};
	});
var $author$project$Api$conflictCandidate = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Api$ConflictCandidate,
	A2($elm$json$Json$Decode$field, 'axis', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'heads',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string));
var $author$project$Api$conflictEntry = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$ConflictEntry,
	A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'code', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'summaries',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'candidates',
		$elm$json$Json$Decode$list($author$project$Api$conflictCandidate)));
var $author$project$Api$conflicts = A2(
	$elm$json$Json$Decode$map,
	$author$project$Api$ConflictWindow,
	A2(
		$elm$json$Json$Decode$field,
		'conflicts',
		$elm$json$Json$Decode$list($author$project$Api$conflictEntry)));
var $author$project$Api$Doctor = function (issues) {
	return {dR: issues};
};
var $author$project$Api$Issue = F4(
	function (severity, code, message, path) {
		return {bu: code, cA: message, bi: path, em: severity};
	});
var $author$project$Api$issue = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$Issue,
	A2($elm$json$Json$Decode$field, 'severity', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'code', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string),
	A3(
		$author$project$Api$optional,
		'path',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
		$elm$core$Maybe$Nothing));
var $author$project$Api$doctor = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (_v0, issues) {
			return $author$project$Api$Doctor(issues);
		}),
	A2($elm$json$Json$Decode$field, 'ok', $elm$json$Json$Decode$bool),
	A2(
		$elm$json$Json$Decode$field,
		'issues',
		$elm$json$Json$Decode$list($author$project$Api$issue)));
var $author$project$Api$HistoryWindow = F4(
	function (asOf, limit, truncated, operations) {
		return {aj: asOf, cz: limit, bg: operations, eu: truncated};
	});
var $elm$json$Json$Decode$map8 = _Json_map8;
var $author$project$Api$historyItem = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A2(
			$elm$json$Json$Decode$map,
			function (changes) {
				return _Utils_update(
					base,
					{a5: changes});
			},
			A2(
				$elm$json$Json$Decode$field,
				'changes',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
	},
	A9(
		$elm$json$Json$Decode$map8,
		F8(
			function (adr, title, label, actor, claimedAt, operationId, reason, commit) {
				return {a3: actor, J: adr, a5: _List_Nil, a6: claimedAt, a7: commit, cx: label, aE: operationId, cO: reason, F: title};
			}),
		A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'label', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'actor', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'claimed_at', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'operation', $elm$json$Json$Decode$string),
		A3(
			$author$project$Api$optional,
			'reason',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
			$elm$core$Maybe$Nothing),
		A3(
			$author$project$Api$optional,
			'commit',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
			$elm$core$Maybe$Nothing)));
var $author$project$Api$history = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$HistoryWindow,
	A2($elm$json$Json$Decode$field, 'as_of', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'limit', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'truncated', $elm$json$Json$Decode$bool),
	A2(
		$elm$json$Json$Decode$field,
		'operations',
		$elm$json$Json$Decode$list($author$project$Api$historyItem)));
var $author$project$Api$CandidateRecord = F5(
	function (id, title, summary, body, path) {
		return {am: body, bc: id, bi: path, Q: summary, F: title};
	});
var $author$project$Api$candidateDomain = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (id, domains, path) {
			return A5(
				$author$project$Api$CandidateRecord,
				id,
				'Domain candidate',
				A2($elm$core$String$join, ', ', domains),
				'',
				path);
		}),
	A2($elm$json$Json$Decode$field, 'connection', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'domains',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'path', $elm$json$Json$Decode$string));
var $author$project$Api$candidateRecord = A5(
	$elm$json$Json$Decode$map4,
	F4(
		function (id, title, summary, path) {
			return A5($author$project$Api$CandidateRecord, id, title, summary, '', path);
		}),
	A2($elm$json$Json$Decode$field, 'record', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'path', $elm$json$Json$Decode$string));
var $author$project$Api$candidateScope = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (id, scopes, path) {
			return A5(
				$author$project$Api$CandidateRecord,
				id,
				'Scope candidate',
				A2($elm$core$String$join, ', ', scopes),
				'',
				path);
		}),
	A2($elm$json$Json$Decode$field, 'connection', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'applies_to',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'path', $elm$json$Json$Decode$string));
var $author$project$Api$landing = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A2(
			$elm$json$Json$Decode$map,
			function (complete) {
				return base + (' · ' + (complete ? 'complete landing' : 'partial landing'));
			},
			A2($elm$json$Json$Decode$field, 'complete', $elm$json$Json$Decode$bool));
	},
	A4(
		$elm$json$Json$Decode$map3,
		F3(
			function (ref, commit, line) {
				return ref + (' · ' + (commit + (' · ' + line)));
			}),
		A2($elm$json$Json$Decode$field, 'ref', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'commit', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'line', $elm$json$Json$Decode$string)));
var $author$project$Api$placement = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A5(
			$elm$json$Json$Decode$map4,
			F4(
				function (authored, committed, parents, reachable) {
					return base + (' · authored ' + (authored + (' · committed ' + (committed + (' · parents ' + (A2($elm$core$String$join, ', ', parents) + (' · ' + (reachable ? 'reachable' : 'unreachable'))))))));
				}),
			A2($elm$json$Json$Decode$field, 'authored_at', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'committed_at', $elm$json$Json$Decode$string),
			A2(
				$elm$json$Json$Decode$field,
				'parents',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
			A2($elm$json$Json$Decode$field, 'reachable', $elm$json$Json$Decode$bool));
	},
	A4(
		$elm$json$Json$Decode$map3,
		F3(
			function (classification, commit, subject) {
				return classification + (' · ' + (commit + (' · ' + subject)));
			}),
		A2($elm$json$Json$Decode$field, 'classification', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'commit', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'subject', $elm$json$Json$Decode$string)));
var $author$project$Api$provenance = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (extended) {
				return A5(
					$elm$json$Json$Decode$map4,
					F4(
						function (topIntroductions, whenIntroductions, topOriginals, whenOriginals) {
							return _Utils_update(
								extended,
								{
									bE: _Utils_ap(topIntroductions, whenIntroductions),
									bM: _Utils_ap(topOriginals, whenOriginals)
								});
						}),
					A3(
						$author$project$Api$optional,
						'introductions',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil),
					A3(
						$author$project$Api$optionalAt,
						_List_fromArray(
							['when', 'introductions']),
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil),
					A3(
						$author$project$Api$optional,
						'original_commits',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil),
					A3(
						$author$project$Api$optionalAt,
						_List_fromArray(
							['when', 'original_operation_commits']),
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil));
			},
			A6(
				$elm$json$Json$Decode$map5,
				F5(
					function (input, prompt, context, placements, landings) {
						return _Utils_update(
							base,
							{bv: context, bD: input, bF: landings, bO: placements, bQ: prompt});
					}),
				A3(
					$author$project$Api$optional,
					'input_digest',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
					$elm$core$Maybe$Nothing),
				A3(
					$author$project$Api$optional,
					'prompt_digest',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
					$elm$core$Maybe$Nothing),
				A3(
					$author$project$Api$optional,
					'context_digest',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
					$elm$core$Maybe$Nothing),
				A3(
					$author$project$Api$optional,
					'placements',
					$elm$json$Json$Decode$list($author$project$Api$placement),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'line_landings',
					$elm$json$Json$Decode$list($author$project$Api$landing),
					_List_Nil)));
	},
	A6(
		$elm$json$Json$Decode$map5,
		F5(
			function (actor, model, claimedAt, basis, operationId) {
				return {a3: actor, cb: basis, a6: claimedAt, bv: $elm$core$Maybe$Nothing, bD: $elm$core$Maybe$Nothing, bE: _List_Nil, bF: _List_Nil, cC: model, aE: operationId, bM: _List_Nil, bO: _List_Nil, bQ: $elm$core$Maybe$Nothing};
			}),
		A2($elm$json$Json$Decode$field, 'actor', $elm$json$Json$Decode$string),
		A3(
			$author$project$Api$optional,
			'model',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
			$elm$core$Maybe$Nothing),
		A2($elm$json$Json$Decode$field, 'claimed_at', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'basis', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'operation', $elm$json$Json$Decode$string)));
var $author$project$Api$resolutionConflict = A5(
	$elm$json$Json$Decode$map4,
	F4(
		function (_v0, _v1, _v2, _v3) {
			return 0;
		}),
	A2($elm$json$Json$Decode$field, 'kind', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'head_count', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'heads',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string));
var $author$project$Api$resolution = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (_v0, _v1, _v2) {
			return 0;
		}),
	A2($elm$json$Json$Decode$field, 'resolved', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'resolution_required', $elm$json$Json$Decode$bool),
	A2(
		$elm$json$Json$Decode$field,
		'conflicts',
		$elm$json$Json$Decode$list($author$project$Api$resolutionConflict)));
var $author$project$Api$collapsedRequired = A2(
	$elm$json$Json$Decode$andThen,
	function (_v8) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (_v17) {
				return A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (_v18, _v19, _v20) {
							return 0;
						}),
					A2($elm$json$Json$Decode$field, 'resolved', $elm$json$Json$Decode$bool),
					A2($elm$json$Json$Decode$field, 'resolution_required', $elm$json$Json$Decode$bool),
					A2($elm$json$Json$Decode$field, 'resolution', $author$project$Api$resolution));
			},
			A9(
				$elm$json$Json$Decode$map8,
				F8(
					function (_v9, _v10, _v11, _v12, _v13, _v14, _v15, _v16) {
						return 0;
					}),
				A2(
					$elm$json$Json$Decode$field,
					'domain_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'status_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'candidate_records',
					$elm$json$Json$Decode$list($author$project$Api$candidateRecord)),
				A2(
					$elm$json$Json$Decode$field,
					'candidate_scopes',
					$elm$json$Json$Decode$list($author$project$Api$candidateScope)),
				A2(
					$elm$json$Json$Decode$field,
					'candidate_domains',
					$elm$json$Json$Decode$list($author$project$Api$candidateDomain)),
				A2(
					$elm$json$Json$Decode$field,
					'conflicts',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'source_paths',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
				A2(
					$elm$json$Json$Decode$field,
					'provenance',
					$elm$json$Json$Decode$dict(
						$elm$json$Json$Decode$nullable($author$project$Api$provenance)))));
	},
	A9(
		$elm$json$Json$Decode$map8,
		F8(
			function (_v0, _v1, _v2, _v3, _v4, _v5, _v6, _v7) {
				return 0;
			}),
		A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'body', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			'domains',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'applies_to',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'record_heads',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'scope_heads',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))));
var $author$project$Api$Operation = F3(
	function (id, items, provenance) {
		return {bc: id, dT: items, bk: provenance};
	});
var $author$project$Api$parentDiff = A3(
	$elm$json$Json$Decode$map2,
	F2(
		function (parent, diff) {
			return parent + ('\u000A' + diff);
		}),
	A2($elm$json$Json$Decode$field, 'parent', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'diff', $elm$json$Json$Decode$string));
var $author$project$Api$operationItem = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (extended) {
				return A5(
					$elm$json$Json$Decode$map4,
					F4(
						function (relation, rawSemantic, parents, diffs) {
							return _Utils_update(
								extended,
								{bx: diffs, bN: parents, bT: rawSemantic, bW: relation});
						}),
					A3(
						$author$project$Api$optional,
						'relation',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'raw_semantic',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'parents',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil),
					A3(
						$author$project$Api$optional,
						'diffs',
						$elm$json$Json$Decode$list($author$project$Api$parentDiff),
						_List_Nil));
			},
			A2(
				$elm$json$Json$Decode$andThen,
				function (extended) {
					return A7(
						$elm$json$Json$Decode$map6,
						F6(
							function (scopes, state, replacement, added, removed, refinements) {
								return _Utils_update(
									extended,
									{bt: added, bV: refinements, bX: removed, bY: replacement, O: scopes, b3: state});
							}),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'applies_to']),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
							_List_Nil),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'state']),
							$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
							$elm$core$Maybe$Nothing),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'replacement']),
							$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
							$elm$core$Maybe$Nothing),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'added']),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
							_List_Nil),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'removed']),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
							_List_Nil),
						A3(
							$author$project$Api$optionalAt,
							_List_fromArray(
								['metadata', 'refinements']),
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
							_List_Nil));
				},
				A6(
					$elm$json$Json$Decode$map5,
					F5(
						function (title, summary, body, rationale, domains) {
							return _Utils_update(
								base,
								{am: body, L: domains, bS: rationale, Q: summary, F: title});
						}),
					A3(
						$author$project$Api$optional,
						'title',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'summary',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'body',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'rationale',
						$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
						$elm$core$Maybe$Nothing),
					A3(
						$author$project$Api$optional,
						'domains',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil))));
	},
	A5(
		$elm$json$Json$Decode$map4,
		F4(
			function (id, kind, eventName, path) {
				return {bt: _List_Nil, am: $elm$core$Maybe$Nothing, bx: _List_Nil, L: _List_Nil, cl: eventName, bc: id, be: kind, bN: _List_Nil, bi: path, bS: $elm$core$Maybe$Nothing, bT: $elm$core$Maybe$Nothing, bV: _List_Nil, bW: $elm$core$Maybe$Nothing, bX: _List_Nil, bY: $elm$core$Maybe$Nothing, O: _List_Nil, b3: $elm$core$Maybe$Nothing, Q: $elm$core$Maybe$Nothing, F: $elm$core$Maybe$Nothing};
			}),
		A2($elm$json$Json$Decode$field, 'item', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'event', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'path', $elm$json$Json$Decode$string)));
var $author$project$Api$operation = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Api$Operation,
	A2($elm$json$Json$Decode$field, 'operation', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'items',
		$elm$json$Json$Decode$list($author$project$Api$operationItem)),
	A2(
		$elm$json$Json$Decode$field,
		'provenance',
		$elm$json$Json$Decode$nullable($author$project$Api$provenance)));
var $author$project$Api$explodedRequired = A5(
	$elm$json$Json$Decode$map4,
	F4(
		function (_v0, _v1, _v2, _v3) {
			return 0;
		}),
	A2(
		$elm$json$Json$Decode$field,
		'operations',
		$elm$json$Json$Decode$list($author$project$Api$operation)),
	A2($elm$json$Json$Decode$field, 'resolution', $author$project$Api$resolution),
	A2($elm$json$Json$Decode$field, 'resolution_required', $elm$json$Json$Decode$bool),
	A2($elm$json$Json$Decode$field, 'resolved', $elm$json$Json$Decode$bool));
var $author$project$Api$CandidateSet = F3(
	function (records, scopes, domains) {
		return {L: domains, cP: records, O: scopes};
	});
var $author$project$Api$candidateSet = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Api$CandidateSet,
	A3(
		$author$project$Api$optional,
		'candidate_records',
		$elm$json$Json$Decode$list($author$project$Api$candidateRecord),
		_List_Nil),
	A3(
		$author$project$Api$optional,
		'candidate_scopes',
		$elm$json$Json$Decode$list($author$project$Api$candidateScope),
		_List_Nil),
	A3(
		$author$project$Api$optional,
		'candidate_domains',
		$elm$json$Json$Decode$list($author$project$Api$candidateDomain),
		_List_Nil));
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
var $author$project$Api$provenanceSet = A2(
	$elm$json$Json$Decode$map,
	A2(
		$elm$core$Basics$composeR,
		$elm$core$Dict$values,
		$elm$core$List$filterMap($elm$core$Basics$identity)),
	A3(
		$author$project$Api$optional,
		'provenance',
		$elm$json$Json$Decode$dict(
			$elm$json$Json$Decode$nullable($author$project$Api$provenance)),
		$elm$core$Dict$empty));
var $author$project$Api$inspectionFields = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (extended) {
				return A5(
					$elm$json$Json$Decode$map4,
					F4(
						function (paths, operations, originDetails, required) {
							return _Utils_update(
								extended,
								{bg: operations, bk: originDetails, aF: required, aJ: paths});
						}),
					A3(
						$author$project$Api$optional,
						'source_paths',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil),
					A3(
						$author$project$Api$optional,
						'operations',
						$elm$json$Json$Decode$list($author$project$Api$operation),
						_List_Nil),
					$author$project$Api$provenanceSet,
					A2($elm$json$Json$Decode$field, 'resolution_required', $elm$json$Json$Decode$bool));
			},
			A9(
				$elm$json$Json$Decode$map8,
				F8(
					function (domains, scopes, recordHeads, scopeHeads, domainHeads, statusHeads, candidates, conflictSummaries) {
						return _Utils_update(
							base,
							{a4: candidates, an: conflictSummaries, bz: domainHeads, L: domains, bU: recordHeads, b_: scopeHeads, O: scopes, b4: statusHeads});
					}),
				A3(
					$author$project$Api$optional,
					'domains',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'applies_to',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'record_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'scope_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'domain_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				A3(
					$author$project$Api$optional,
					'status_heads',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil),
				$author$project$Api$candidateSet,
				A3(
					$author$project$Api$optional,
					'conflicts',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil)));
	},
	A9(
		$elm$json$Json$Decode$map8,
		F8(
			function (adr, revision, view, title, summary, body, status, token) {
				return {
					J: adr,
					aj: revision,
					am: body,
					a4: {L: _List_Nil, cP: _List_Nil, O: _List_Nil},
					an: _List_Nil,
					bz: _List_Nil,
					L: _List_Nil,
					bg: _List_Nil,
					bk: _List_Nil,
					bU: _List_Nil,
					aF: false,
					b_: _List_Nil,
					O: _List_Nil,
					aJ: _List_Nil,
					a_: token,
					at: status,
					b4: _List_Nil,
					Q: summary,
					F: title,
					c0: view
				};
			}),
		A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'as_of', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'view', $elm$json$Json$Decode$string),
		A3($author$project$Api$optional, 'title', $elm$json$Json$Decode$string, ''),
		A3($author$project$Api$optional, 'summary', $elm$json$Json$Decode$string, ''),
		A3($author$project$Api$optional, 'body', $elm$json$Json$Decode$string, ''),
		A3($author$project$Api$optional, 'status', $elm$json$Json$Decode$string, ''),
		A2($elm$json$Json$Decode$field, 'state_token', $elm$json$Json$Decode$string)));
var $author$project$Api$inspection = A2(
	$elm$json$Json$Decode$andThen,
	function (_v0) {
		var schema = _v0.a;
		var viewMode = _v0.b;
		var _v1 = _Utils_Tuple2(schema, viewMode);
		_v1$2:
		while (true) {
			switch (_v1.a) {
				case 'adrai/show-collapsed/v1':
					if (_v1.b === 'collapsed') {
						return A3(
							$elm$json$Json$Decode$map2,
							F2(
								function (_v2, value) {
									return value;
								}),
							$author$project$Api$collapsedRequired,
							$author$project$Api$inspectionFields);
					} else {
						break _v1$2;
					}
				case 'adrai/show-exploded/v1':
					if (_v1.b === 'exploded') {
						return A3(
							$elm$json$Json$Decode$map2,
							F2(
								function (_v3, value) {
									return value;
								}),
							$author$project$Api$explodedRequired,
							$author$project$Api$inspectionFields);
					} else {
						break _v1$2;
					}
				default:
					break _v1$2;
			}
		}
		return $elm$json$Json$Decode$fail('unsupported inspection schema or view');
	},
	A3(
		$elm$json$Json$Decode$map2,
		$elm$core$Tuple$pair,
		A2($elm$json$Json$Decode$field, 'schema', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'view', $elm$json$Json$Decode$string)));
var $author$project$Api$SearchWindow = F3(
	function (asOf, limit, results) {
		return {aj: asOf, cz: limit, cR: results};
	});
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Api$searchHit = A2(
	$elm$json$Json$Decode$andThen,
	function (base) {
		return A7(
			$elm$json$Json$Decode$map6,
			F6(
				function (matched, fields, terms, paths, conflictSummaries, required) {
					return _Utils_update(
						base,
						{an: conflictSummaries, bG: fields, bH: terms, bI: matched, aF: required, aJ: paths});
				}),
			A3(
				$author$project$Api$optional,
				'matched_title',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
				$elm$core$Maybe$Nothing),
			A3(
				$author$project$Api$optionalAt,
				_List_fromArray(
					['matches', 'fields']),
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
				_List_Nil),
			A3(
				$author$project$Api$optionalAt,
				_List_fromArray(
					['matches', 'terms']),
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
				_List_Nil),
			A3(
				$author$project$Api$optional,
				'source_paths',
				$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
				_List_Nil),
			A3(
				$author$project$Api$optional,
				'conflicts',
				$elm$json$Json$Decode$list(
					A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string)),
				_List_Nil),
			A3($author$project$Api$optional, 'resolution_required', $elm$json$Json$Decode$bool, false));
	},
	A9(
		$elm$json$Json$Decode$map8,
		F8(
			function (adr, title, summary, status, domains, scopes, stateToken, score) {
				return {J: adr, an: _List_Nil, L: domains, bG: _List_Nil, bH: _List_Nil, bI: $elm$core$Maybe$Nothing, aF: false, O: scopes, bp: score, aJ: _List_Nil, a_: stateToken, at: status, Q: summary, F: title};
			}),
		A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
		A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			'domains',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
		A2(
			$elm$json$Json$Decode$field,
			'applies_to',
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
		A2($elm$json$Json$Decode$field, 'state_token', $elm$json$Json$Decode$string),
		A2(
			$elm$json$Json$Decode$field,
			'score',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$float))));
var $author$project$Api$search = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Api$SearchWindow,
	A2($elm$json$Json$Decode$field, 'as_of', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'limit', $elm$json$Json$Decode$int),
	A2(
		$elm$json$Json$Decode$field,
		'results',
		$elm$json$Json$Decode$list($author$project$Api$searchHit)));
var $author$project$Main$readSearch = F2(
	function (body, model) {
		return A4(
			$author$project$Main$accept,
			$author$project$Api$search,
			body,
			F2(
				function (window, state) {
					return _Utils_update(
						state,
						{
							j: 0,
							ah: $elm$core$Maybe$Just(window),
							d: false
						});
				}),
			model);
	});
var $author$project$Api$RelevantWindow = F4(
	function (asOf, file, source, results) {
		return {aj: asOf, dF: file, cR: results, en: source};
	});
var $author$project$Api$RelevantHit = F8(
	function (adr, title, summary, status, scopeMatch, confidence, score, evidence) {
		return {J: adr, $7: confidence, dz: evidence, ek: scopeMatch, bp: score, at: status, Q: summary, F: title};
	});
var $author$project$Api$Evidence = F4(
	function (fileExcerpt, adrExcerpt, section, score) {
		return {c8: adrExcerpt, dG: fileExcerpt, bp: score, el: section};
	});
var $author$project$Api$evidence = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$Evidence,
	A2($elm$json$Json$Decode$field, 'file_excerpt', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'adr_excerpt', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'adr_section', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$float));
var $author$project$Api$relevantHit = A9(
	$elm$json$Json$Decode$map8,
	$author$project$Api$RelevantHit,
	A2($elm$json$Json$Decode$field, 'adr', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'summary', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'scope_match', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'confidence', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'score', $elm$json$Json$Decode$float),
	A2(
		$elm$json$Json$Decode$field,
		'evidence',
		$elm$json$Json$Decode$list($author$project$Api$evidence)));
var $author$project$Api$relevant = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$RelevantWindow,
	A2($elm$json$Json$Decode$field, 'as_of', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['file', 'path']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['file', 'source']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'results',
		$elm$json$Json$Decode$list($author$project$Api$relevantHit)));
var $author$project$Api$Repository = F4(
	function (worktree, head, headRef, stateToken) {
		return {dK: head, dL: headRef, a_: stateToken, ex: worktree};
	});
var $author$project$Api$repository = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Api$Repository,
	A2($elm$json$Json$Decode$field, 'worktree', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'head', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'head_ref',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['repository_state', 'token']),
		$elm$json$Json$Decode$string));
var $author$project$View$Forms$seedBasis = F2(
	function (repository, draft) {
		return (draft.de === '') ? _Utils_update(
			draft,
			{
				dd: repository.dK,
				ak: repository.dL,
				de: repository.a_,
				aV: $elm$core$Maybe$Just(
					A2($author$project$View$Forms$baseline, repository, $elm$core$Maybe$Nothing))
			}) : draft;
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
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
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
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
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
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
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
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
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
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
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
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
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
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
				if (_v4.$ === -1) {
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
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
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
						if (_v7.$ === -1) {
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
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
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
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Main$clearInspectionRetries = function (retries) {
	return A2(
		$elm$core$Dict$remove,
		'collapsed',
		A2($elm$core$Dict$remove, 'exploded', retries));
};
var $author$project$Main$selectAdrAt = F3(
	function (adr, revision, model) {
		var selected = _Utils_update(
			model,
			{
				t: $author$project$Main$clearInspectionRetries(model.t),
				S: false,
				I: false,
				v: $elm$core$Dict$empty,
				D: $elm$core$Maybe$Just(adr),
				X: $elm$core$Maybe$Just(revision)
			});
		var _v0 = A5(
			$author$project$Main$issue,
			2,
			'GET',
			A3($author$project$Route$showPath, adr, revision, 'collapsed'),
			$elm$core$Maybe$Nothing,
			selected);
		var withCollapsed = _v0.a;
		var collapsedCommand = _v0.b;
		var _v1 = A5(
			$author$project$Main$issue,
			3,
			'GET',
			A3($author$project$Route$showPath, adr, revision, 'exploded'),
			$elm$core$Maybe$Nothing,
			withCollapsed);
		var withExploded = _v1.a;
		var explodedCommand = _v1.b;
		return _Utils_Tuple2(
			withExploded,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[collapsedCommand, explodedCommand])));
	});
var $author$project$Main$readResponse = F3(
	function (kind, body, model) {
		switch (kind) {
			case 0:
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					$author$project$Api$response($author$project$Api$repository),
					body);
				if (!_v1.$) {
					var envelope = _v1.a;
					var _v2 = envelope.cB.aj;
					switch (_v2.$) {
						case 2:
							var reason = _v2.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Repository observation unavailable: ' + reason),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
						case 0:
							var oid = _v2.a;
							var repository = envelope.dp;
							var updated = _Utils_update(
								model,
								{
									b: A2($author$project$View$Forms$seedBasis, repository, model.b),
									g: $elm$core$Maybe$Nothing,
									N: $elm$core$Maybe$Just(repository),
									q: true
								});
							if (!_Utils_eq(oid, repository.dK)) {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											g: $elm$core$Maybe$Just('Repository response basis disagrees with its metadata.'),
											d: true
										}),
									$elm$core$Platform$Cmd$none);
							} else {
								var _v3 = _Utils_Tuple2(model.D, model.c.aG);
								if ((!_v3.a.$) && (_v3.b === 'HEAD')) {
									var adr = _v3.a.a;
									return A3($author$project$Main$selectAdrAt, adr, repository.dK, updated);
								} else {
									return _Utils_Tuple2(updated, $elm$core$Platform$Cmd$none);
								}
							}
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Repository response has an invalid comparison basis.'),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								g: $elm$core$Maybe$Just('Repository response has an unsupported shape.'),
								d: true
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 1:
				var _v4 = model.c.c0;
				switch (_v4) {
					case 0:
						return A2($author$project$Main$readSearch, body, model);
					case 1:
						return A2($author$project$Main$readSearch, body, model);
					case 2:
						return A4(
							$author$project$Main$accept,
							$author$project$Api$relevant,
							body,
							F2(
								function (window, state) {
									return _Utils_update(
										state,
										{
											j: 0,
											aX: $elm$core$Maybe$Just(window),
											d: false
										});
								}),
							model);
					case 3:
						return A4(
							$author$project$Main$accept,
							$author$project$Api$history,
							body,
							F2(
								function (window, state) {
									return _Utils_update(
										state,
										{
											aC: $elm$core$Maybe$Just(window),
											j: 0,
											d: false
										});
								}),
							model);
					case 4:
						return A4(
							$author$project$Main$accept,
							$author$project$Api$comparison,
							body,
							F2(
								function (window, state) {
									return _Utils_update(
										state,
										{
											ay: $elm$core$Maybe$Just(window),
											j: 0,
											d: false
										});
								}),
							model);
					case 5:
						return A4(
							$author$project$Main$accept,
							$author$project$Api$conflicts,
							body,
							F2(
								function (window, state) {
									return _Utils_update(
										state,
										{
											an: $elm$core$Maybe$Just(window),
											j: 0,
											d: false
										});
								}),
							model);
					default:
						return A4(
							$author$project$Main$accept,
							$author$project$Api$doctor,
							body,
							F2(
								function (window, state) {
									return _Utils_update(
										state,
										{
											ba: $elm$core$Maybe$Just(window),
											j: 0,
											d: false
										});
								}),
							model);
				}
			case 2:
				var _v5 = A2(
					$elm$json$Json$Decode$decodeValue,
					$author$project$Api$response($author$project$Api$inspection),
					body);
				if (!_v5.$) {
					var envelope = _v5.a;
					var _v6 = envelope.cB.aj;
					switch (_v6.$) {
						case 2:
							var reason = _v6.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Inspection unavailable: ' + reason),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
						case 0:
							var oid = _v6.a;
							var previous = model.s;
							var inspection = envelope.dp;
							var merged = function () {
								if (!previous.$) {
									var prior = previous.a;
									return (_Utils_eq(prior.J, inspection.J) && _Utils_eq(prior.aj, inspection.aj)) ? _Utils_update(
										inspection,
										{bg: prior.bg}) : inspection;
								} else {
									return inspection;
								}
							}();
							return ((inspection.c0 !== 'collapsed') || ((!_Utils_eq(oid, inspection.aj)) || ((!_Utils_eq(
								model.X,
								$elm$core$Maybe$Just(oid))) || (!_Utils_eq(
								model.D,
								$elm$core$Maybe$Just(inspection.J)))))) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Inspection basis disagrees with the selected ADR and revision.'),
										d: true
									}),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										S: true,
										g: $elm$core$Maybe$Nothing,
										s: $elm$core$Maybe$Just(merged)
									}),
								$elm$core$Platform$Cmd$none);
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Inspection has an invalid comparison basis.'),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								g: $elm$core$Maybe$Just('Inspection response has an unsupported shape.'),
								d: true
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 3:
				var _v8 = A2(
					$elm$json$Json$Decode$decodeValue,
					$author$project$Api$response($author$project$Api$inspection),
					body);
				if (!_v8.$) {
					var envelope = _v8.a;
					var _v9 = envelope.cB.aj;
					switch (_v9.$) {
						case 2:
							var reason = _v9.a;
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Operation history unavailable: ' + reason),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
						case 0:
							var oid = _v9.a;
							var exploded = envelope.dp;
							if ((exploded.c0 !== 'exploded') || ((!_Utils_eq(oid, exploded.aj)) || ((!_Utils_eq(
								model.X,
								$elm$core$Maybe$Just(oid))) || (!_Utils_eq(
								model.D,
								$elm$core$Maybe$Just(exploded.J)))))) {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{
											g: $elm$core$Maybe$Just('Operation history basis disagrees with the selected ADR and revision.'),
											d: true
										}),
									$elm$core$Platform$Cmd$none);
							} else {
								var _v10 = model.s;
								if (!_v10.$) {
									var collapsed = _v10.a;
									return (_Utils_eq(collapsed.J, exploded.J) && _Utils_eq(collapsed.aj, exploded.aj)) ? _Utils_Tuple2(
										_Utils_update(
											model,
											{
												I: true,
												s: $elm$core$Maybe$Just(
													_Utils_update(
														collapsed,
														{bg: exploded.bg}))
											}),
										$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
										_Utils_update(
											model,
											{
												I: true,
												s: $elm$core$Maybe$Just(exploded)
											}),
										$elm$core$Platform$Cmd$none);
								} else {
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												I: true,
												s: $elm$core$Maybe$Just(exploded)
											}),
										$elm$core$Platform$Cmd$none);
								}
							}
						default:
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Operation history has an invalid comparison basis.'),
										d: true
									}),
								$elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								g: $elm$core$Maybe$Just('Operation history has an unsupported shape.'),
								d: true
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
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
var $author$project$Main$retryableBusy = function (body) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $author$project$Api$failure, body);
	if (!_v0.$) {
		var problem = _v0.a;
		return (problem.at === 503) && A2(
			$elm$core$List$member,
			problem.bu,
			_List_fromArray(
				['repository-busy', 'repository-lock-unavailable']));
	} else {
		return false;
	}
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Main$terminalFailure = function (body) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $author$project$Api$failure, body);
	if (!_v0.$) {
		var failure = _v0.a;
		return (failure.at === 503) && (failure.bu === 'generation-exhausted');
	} else {
		return false;
	}
};
var $author$project$Main$receiveResponse = F4(
	function (requestId, status, body, model) {
		var _v0 = A2($elm$core$Dict$get, requestId, model.W);
		if (_v0.$ === 1) {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var pending = _v0.a;
			var without = _Utils_update(
				model,
				{
					W: A2($elm$core$Dict$remove, requestId, model.W)
				});
			var current = A3($author$project$Main$readPendingCurrent, requestId, pending, model);
			if (pending.be === 4) {
				return A5($author$project$Main$mutationResponse, requestId, pending, status, body, without);
			} else {
				if ((status === 503) && $author$project$Main$terminalFailure(body)) {
					return $author$project$Main$terminalExhaustion(without);
				} else {
					if (!current) {
						return _Utils_Tuple2(without, $elm$core$Platform$Cmd$none);
					} else {
						if ((status === 503) && $author$project$Main$retryableBusy(body)) {
							var key = $author$project$Main$requestKey(pending.be);
							var attempts = A2(
								$elm$core$Maybe$withDefault,
								0,
								A2($elm$core$Dict$get, key, model.t));
							return (attempts < 2) ? _Utils_Tuple2(
								A3(
									$author$project$Main$readIssue,
									pending.be,
									'Repository is busy; retrying this read.',
									_Utils_update(
										without,
										{
											t: A3($elm$core$Dict$insert, key, attempts + 1, model.t)
										})),
								A2(
									$elm$core$Task$perform,
									function (_v1) {
										return A2($author$project$Main$RetryRead, requestId, pending);
									},
									$elm$core$Process$sleep(400))) : _Utils_Tuple2(
								A3(
									$author$project$Main$readIssue,
									pending.be,
									((pending.be === 2) || (pending.be === 3)) ? 'Repository remains busy. Retry this inspection when it settles.' : 'Repository remains busy. Refresh when it settles.',
									without),
								$elm$core$Platform$Cmd$none);
						} else {
							if (status >= 400) {
								var _v2 = A2($elm$json$Json$Decode$decodeValue, $author$project$Api$failure, body);
								if (!_v2.$) {
									var failure = _v2.a;
									return (!_Utils_eq(failure.at, status)) ? _Utils_Tuple2(
										A3($author$project$Main$readIssue, pending.be, 'The server returned an inconsistent error status.', without),
										$elm$core$Platform$Cmd$none) : ((failure.at === 401) ? _Utils_Tuple2(
										_Utils_update(
											without,
											{
												g: $elm$core$Maybe$Just('Session unavailable. Reopen the process bootstrap URL.'),
												m: false,
												P: 'unavailable',
												d: true
											}),
										$author$project$Main$toJs(
											$elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'type',
														$elm$json$Json$Encode$string('disconnect'))
													])))) : _Utils_Tuple2(
										A3($author$project$Main$readIssue, pending.be, failure.bu + (': ' + failure.cA), without),
										$elm$core$Platform$Cmd$none));
								} else {
									return _Utils_Tuple2(
										A3($author$project$Main$readIssue, pending.be, 'The server returned an unreadable error.', without),
										$elm$core$Platform$Cmd$none);
								}
							} else {
								var _v3 = A3(
									$author$project$Main$readResponse,
									pending.be,
									body,
									_Utils_update(
										without,
										{
											t: A2(
												$elm$core$Dict$remove,
												$author$project$Main$requestKey(pending.be),
												without.t),
											v: A2(
												$elm$core$Dict$remove,
												$author$project$Main$requestKey(pending.be),
												without.v)
										}));
								var received = _v3.a;
								var command = _v3.b;
								return _Utils_Tuple2(
									_Utils_update(
										received,
										{
											q: received.q && _Utils_eq(received.M, $elm$core$Maybe$Nothing),
											d: received.d || (!_Utils_eq(received.M, $elm$core$Maybe$Nothing))
										}),
									command);
							}
						}
					}
				}
			}
		}
	});
var $author$project$Main$requestFailed = F3(
	function (requestId, message, model) {
		var _v0 = A2($elm$core$Dict$get, requestId, model.W);
		if (_v0.$ === 1) {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			var pending = _v0.a;
			var without = _Utils_update(
				model,
				{
					W: A2($elm$core$Dict$remove, requestId, model.W)
				});
			return (pending.be === 4) ? _Utils_Tuple2(
				_Utils_update(
					without,
					{
						b: $author$project$View$Forms$markStale(model.b),
						Z: $elm$core$Maybe$Nothing,
						e: 'Mutation outcome is uncertain. Inspect current history before retrying.'
					}),
				$elm$core$Platform$Cmd$none) : ((!A3($author$project$Main$readPendingCurrent, requestId, pending, model)) ? _Utils_Tuple2(without, $elm$core$Platform$Cmd$none) : _Utils_Tuple2(
				A3($author$project$Main$readIssue, pending.be, message, without),
				$elm$core$Platform$Cmd$none));
		}
	});
var $author$project$Main$Reconnect = {$: 14};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$Main$socketChanged = F2(
	function (state, model) {
		if (model.k) {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		} else {
			if ((state === 'closed') && (model.m && (model.ag < 4))) {
				var attempts = model.ag + 1;
				var pause = A2(
					$elm$core$Basics$min,
					8000,
					500 * A2($elm$core$Basics$pow, 2, attempts));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							R: false,
							S: false,
							b: $author$project$View$Forms$markStale(model.b),
							I: false,
							ag: attempts,
							q: false,
							P: 'closed',
							d: true
						}),
					A2(
						$elm$core$Task$perform,
						function (_v0) {
							return $author$project$Main$Reconnect;
						},
						$elm$core$Process$sleep(pause)));
			} else {
				if ((state === 'closed') && model.m) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{R: false, S: false, I: false, q: false, P: 'unavailable', d: true}),
						$elm$core$Platform$Cmd$none);
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{R: state === 'open', P: state}),
						$elm$core$Platform$Cmd$none);
				}
			}
		}
	});
var $author$project$Main$receive = F2(
	function (raw, model) {
		var _v0 = A2(
			$elm$json$Json$Decode$decodeValue,
			A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$string),
			raw);
		_v0$4:
		while (true) {
			if (!_v0.$) {
				switch (_v0.a) {
					case 'response':
						var _v1 = A2(
							$elm$json$Json$Decode$decodeValue,
							A4(
								$elm$json$Json$Decode$map3,
								F3(
									function (requestId, status, body) {
										return _Utils_Tuple3(requestId, status, body);
									}),
								A2($elm$json$Json$Decode$field, 'request_id', $elm$json$Json$Decode$string),
								A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$int),
								A2($elm$json$Json$Decode$field, 'body', $elm$json$Json$Decode$value)),
							raw);
						if (!_v1.$) {
							var _v2 = _v1.a;
							var requestId = _v2.a;
							var status = _v2.b;
							var body = _v2.c;
							return A4($author$project$Main$receiveResponse, requestId, status, body, model);
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Malformed transport response.')
									}),
								$elm$core$Platform$Cmd$none);
						}
					case 'request-failed':
						var _v3 = A2(
							$elm$json$Json$Decode$decodeValue,
							A3(
								$elm$json$Json$Decode$map2,
								$elm$core$Tuple$pair,
								A2($elm$json$Json$Decode$field, 'request_id', $elm$json$Json$Decode$string),
								A2($elm$json$Json$Decode$field, 'message', $elm$json$Json$Decode$string)),
							raw);
						if (!_v3.$) {
							var _v4 = _v3.a;
							var requestId = _v4.a;
							var message = _v4.b;
							return A3($author$project$Main$requestFailed, requestId, message, model);
						} else {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{
										g: $elm$core$Maybe$Just('Malformed transport failure.')
									}),
								$elm$core$Platform$Cmd$none);
						}
					case 'socket-state':
						var _v5 = A2(
							$elm$json$Json$Decode$decodeValue,
							A2($elm$json$Json$Decode$field, 'state', $elm$json$Json$Decode$string),
							raw);
						if (!_v5.$) {
							var state = _v5.a;
							return ((state === 'unavailable') && _Utils_eq(
								A2(
									$elm$json$Json$Decode$decodeValue,
									A2($elm$json$Json$Decode$field, 'reason', $elm$json$Json$Decode$string),
									raw),
								$elm$core$Result$Ok('generation-exhausted'))) ? $author$project$Main$terminalExhaustion(model) : A2($author$project$Main$socketChanged, state, model);
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					case 'event':
						var _v6 = A2(
							$elm$json$Json$Decode$decodeValue,
							A2($elm$json$Json$Decode$field, 'body', $author$project$Api$event),
							raw);
						if (!_v6.$) {
							var event = _v6.a;
							return A2($author$project$Main$eventReceived, event, model);
						} else {
							return $author$project$Main$refresh(
								_Utils_update(
									model,
									{
										R: false,
										g: $elm$core$Maybe$Just('Event decoding failed; refreshing snapshots.')
									}));
						}
					default:
						break _v0$4;
				}
			} else {
				break _v0$4;
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Main$retryInspection = F3(
	function (kind, path, model) {
		return model.k ? _Utils_Tuple2(model, $elm$core$Platform$Cmd$none) : A5(
			$author$project$Main$issue,
			kind,
			'GET',
			path,
			$elm$core$Maybe$Nothing,
			_Utils_update(
				model,
				{
					t: A2(
						$elm$core$Dict$remove,
						$author$project$Main$requestKey(kind),
						model.t),
					v: A2(
						$elm$core$Dict$remove,
						$author$project$Main$requestKey(kind),
						model.v)
				}));
	});
var $author$project$Main$currentViewRevision = function (model) {
	var _v0 = model.c.c0;
	switch (_v0) {
		case 0:
			return A2(
				$elm$core$Maybe$withDefault,
				'HEAD',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.aj;
					},
					model.ah));
		case 1:
			return A2(
				$elm$core$Maybe$withDefault,
				'HEAD',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.aj;
					},
					model.ah));
		case 2:
			return A2(
				$elm$core$Maybe$withDefault,
				'HEAD',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.aj;
					},
					model.aX));
		case 3:
			return A2(
				$elm$core$Maybe$withDefault,
				'HEAD',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.aj;
					},
					model.aC));
		case 4:
			return A2(
				$elm$core$Maybe$withDefault,
				'HEAD',
				A2(
					$elm$core$Maybe$map,
					function ($) {
						return $.et;
					},
					model.ay));
		default:
			return A2($elm$core$Maybe$withDefault, 'HEAD', model.aW);
	}
};
var $author$project$Main$selectAdr = F2(
	function (adr, model) {
		var revision = $author$project$Main$currentViewRevision(model);
		var selected = _Utils_update(
			model,
			{
				t: $author$project$Main$clearInspectionRetries(model.t),
				S: false,
				g: $elm$core$Maybe$Nothing,
				I: false,
				s: $elm$core$Maybe$Nothing,
				v: $elm$core$Dict$empty,
				D: $elm$core$Maybe$Just(adr),
				X: $elm$core$Maybe$Just(revision)
			});
		var _v0 = A5(
			$author$project$Main$issue,
			2,
			'GET',
			A3($author$project$Route$showPath, adr, revision, 'collapsed'),
			$elm$core$Maybe$Nothing,
			selected);
		var withCollapsed = _v0.a;
		var collapsedCommand = _v0.b;
		var _v1 = A5(
			$author$project$Main$issue,
			3,
			'GET',
			A3($author$project$Route$showPath, adr, revision, 'exploded'),
			$elm$core$Maybe$Nothing,
			withCollapsed);
		var withExploded = _v1.a;
		var explodedCommand = _v1.b;
		return _Utils_Tuple2(
			withExploded,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[collapsedCommand, explodedCommand])));
	});
var $author$project$View$Forms$actionName = function (action) {
	switch (action) {
		case 0:
			return 'create';
		case 1:
			return 'amend';
		case 2:
			return 'scope';
		case 3:
			return 'domain';
		case 4:
			return 'obsolete';
		default:
			return 'reactivate';
	}
};
var $author$project$View$Forms$optionalDigest = F2(
	function (key, content) {
		return ($elm$core$String$trim(content) === '') ? _List_Nil : _List_fromArray(
			[
				_Utils_Tuple2(
				key,
				$elm$json$Json$Encode$string(
					$elm$core$String$trim(content)))
			]);
	});
var $author$project$View$Forms$common = function (draft) {
	return _Utils_ap(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'repository_state',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'kind',
							$elm$json$Json$Encode$string('repository')),
							_Utils_Tuple2(
							'token',
							$elm$json$Json$Encode$string(draft.de)),
							_Utils_Tuple2(
							'head',
							$elm$json$Json$Encode$string(draft.dd)),
							_Utils_Tuple2(
							'head_ref',
							A2(
								$elm$core$Maybe$withDefault,
								$elm$json$Json$Encode$null,
								A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, draft.ak)))
						]))),
				_Utils_Tuple2(
				'actor',
				$elm$json$Json$Encode$object(
					_Utils_ap(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'kind',
								$elm$json$Json$Encode$string(draft.ab)),
								_Utils_Tuple2(
								'id',
								$elm$json$Json$Encode$string(draft.aa))
							]),
						($elm$core$String$trim(draft.ac) === '') ? _List_Nil : _List_fromArray(
							[
								_Utils_Tuple2(
								'model',
								$elm$json$Json$Encode$string(draft.ac))
							]))))
			]),
		_Utils_ap(
			(!draft.aw) ? _List_Nil : _List_fromArray(
				[
					_Utils_Tuple2(
					'state_token',
					$elm$json$Json$Encode$string(draft.a_))
				]),
			_Utils_ap(
				A2($author$project$View$Forms$optionalDigest, 'input_digest', draft.bD),
				_Utils_ap(
					A2($author$project$View$Forms$optionalDigest, 'prompt_digest', draft.bQ),
					A2($author$project$View$Forms$optionalDigest, 'context_digest', draft.bv)))));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$View$Forms$canonicalBody = function (content) {
	return $elm$core$String$trim(content) + '\u000A';
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
var $author$project$View$Forms$lines = function (content) {
	return A2(
		$elm$core$List$filter,
		$elm$core$Basics$neq(''),
		A2(
			$elm$core$List$map,
			$elm$core$String$trim,
			A2($elm$core$String$split, '\u000A', content)));
};
var $author$project$View$Forms$strings = function (content) {
	return A2(
		$elm$json$Json$Encode$list,
		$elm$json$Json$Encode$string,
		$author$project$View$Forms$lines(content));
};
var $author$project$View$Forms$Domain = 3;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$View$Forms$variant = F3(
	function (draft, reviewedField, reviewedValues) {
		var _v0 = draft.bJ;
		switch (_v0) {
			case 'delta':
				return $elm$core$Result$Ok(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'reason',
							$elm$json$Json$Encode$string(draft.cO)),
							_Utils_Tuple2(
							'mode',
							$elm$json$Json$Encode$string('delta')),
							_Utils_Tuple2(
							'add',
							$author$project$View$Forms$strings(draft.aO)),
							_Utils_Tuple2(
							'remove',
							$author$project$View$Forms$strings(draft.aY))
						]));
			case 'reviewed':
				return $elm$core$Result$Ok(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'reason',
							$elm$json$Json$Encode$string(draft.cO)),
							_Utils_Tuple2(
							'mode',
							$elm$json$Json$Encode$string('reviewed')),
							_Utils_Tuple2(
							reviewedField,
							$author$project$View$Forms$strings(reviewedValues))
						]));
			case 'refine':
				return ((draft.aw !== 3) || $elm$core$List$isEmpty(
					$author$project$View$Forms$lines(draft.bV))) ? $elm$core$Result$Err('Domain refinements must contain at least one entry.') : $elm$core$Result$Ok(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'reason',
							$elm$json$Json$Encode$string(draft.cO)),
							_Utils_Tuple2(
							'mode',
							$elm$json$Json$Encode$string('refine')),
							_Utils_Tuple2(
							'refinements',
							$author$project$View$Forms$strings(draft.bV))
						]));
			default:
				return $elm$core$Result$Err('Choose a supported change mode.');
		}
	});
var $author$project$View$Forms$fields = function (draft) {
	var _v0 = draft.aw;
	switch (_v0) {
		case 0:
			return ($elm$core$String$trim(draft.F) === '') ? $elm$core$Result$Err('Title is required.') : $elm$core$Result$Ok(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'title',
						$elm$json$Json$Encode$string(draft.F)),
						_Utils_Tuple2(
						'summary',
						$elm$json$Json$Encode$string(draft.Q)),
						_Utils_Tuple2(
						'body',
						$elm$json$Json$Encode$string(
							$author$project$View$Forms$canonicalBody(draft.am))),
						_Utils_Tuple2(
						'domains',
						$author$project$View$Forms$strings(draft.L)),
						_Utils_Tuple2(
						'scopes',
						$author$project$View$Forms$strings(draft.O))
					]));
		case 1:
			return ($elm$core$String$trim(draft.ax) === '') ? $elm$core$Result$Err('Change summary is required.') : $elm$core$Result$Ok(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'change_summary',
						$elm$json$Json$Encode$string(draft.ax)),
						_Utils_Tuple2(
						'title',
						$elm$json$Json$Encode$string(draft.F)),
						_Utils_Tuple2(
						'summary',
						$elm$json$Json$Encode$string(draft.Q)),
						_Utils_Tuple2(
						'body',
						$elm$json$Json$Encode$string(
							$author$project$View$Forms$canonicalBody(draft.am)))
					]));
		case 2:
			return ($elm$core$String$trim(draft.cO) === '') ? $elm$core$Result$Err('Scope reason is required.') : A3($author$project$View$Forms$variant, draft, 'patterns', draft.O);
		case 3:
			return ($elm$core$String$trim(draft.cO) === '') ? $elm$core$Result$Err('Domain reason is required.') : A3($author$project$View$Forms$variant, draft, 'domains', draft.L);
		case 4:
			return ($elm$core$String$trim(draft.cO) === '') ? $elm$core$Result$Err('Obsolete reason is required.') : $elm$core$Result$Ok(
				_Utils_ap(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'reason',
							$elm$json$Json$Encode$string(draft.cO)),
							_Utils_Tuple2(
							'resolve',
							$elm$json$Json$Encode$bool(draft.eg))
						]),
					($elm$core$String$trim(draft.bY) === '') ? _List_Nil : _List_fromArray(
						[
							_Utils_Tuple2(
							'replacement',
							$elm$json$Json$Encode$string(
								$elm$core$String$trim(draft.bY)))
						])));
		default:
			return ($elm$core$String$trim(draft.cO) === '') ? $elm$core$Result$Err('Reactivate reason is required.') : $elm$core$Result$Ok(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'reason',
						$elm$json$Json$Encode$string(draft.cO)),
						_Utils_Tuple2(
						'resolve',
						$elm$json$Json$Encode$bool(draft.eg))
					]));
	}
};
var $author$project$View$Forms$build = function (draft) {
	if ((draft.de === '') || (draft.dd === '')) {
		return $elm$core$Result$Err('Refresh the repository before submitting.');
	} else {
		if (draft.b1 || ((!draft.bZ) && (!(!draft.aw)))) {
			return $elm$core$Result$Err('Review current heads and adopt fresh tokens before submitting.');
		} else {
			if (draft.aa === '') {
				return $elm$core$Result$Err('Actor ID is required.');
			} else {
				if (!A2(
					$elm$core$List$member,
					draft.ab,
					_List_fromArray(
						['human', 'llm', 'service']))) {
					return $elm$core$Result$Err('Actor kind must be human, llm, or service.');
				} else {
					if ((!(!draft.aw)) && ((draft.es === '') || (draft.a_ === ''))) {
						return $elm$core$Result$Err('Select and inspect an ADR before submitting.');
					} else {
						var _v0 = $author$project$View$Forms$fields(draft);
						if (_v0.$ === 1) {
							var problem = _v0.a;
							return $elm$core$Result$Err(problem);
						} else {
							var actionFields = _v0.a;
							return $elm$core$Result$Ok(
								$elm$json$Json$Encode$object(
									_Utils_ap(
										$author$project$View$Forms$common(draft),
										actionFields)));
						}
					}
				}
			}
		}
	}
};
var $author$project$Route$mutationPath = F2(
	function (action, adr) {
		return (action === 'create') ? '/api/v1/adrs' : ('/api/v1/adrs/' + ($elm$url$Url$percentEncode(adr) + ('/' + action)));
	});
var $author$project$Main$submit = function (model) {
	var historical = $author$project$Main$isHistorical(model);
	var draft = model.b;
	if (model.k) {
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{e: $author$project$Main$terminalMessage}),
			$elm$core$Platform$Cmd$none);
	} else {
		if (!model.m) {
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{e: 'Reopen the process bootstrap URL to make changes.'}),
				$elm$core$Platform$Cmd$none);
		} else {
			if ((!model.q) || ((!(!draft.aw)) && (!$author$project$Main$inspectionReady(model)))) {
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{e: 'Wait for fresh repository and both inspection views before submitting.'}),
					$elm$core$Platform$Cmd$none);
			} else {
				if (!_Utils_eq(model.Z, $elm$core$Maybe$Nothing)) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{e: 'Wait for the current operation result.'}),
						$elm$core$Platform$Cmd$none);
				} else {
					if (historical) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{e: 'Historical inspection is read-only. Return to current HEAD and review it.'}),
							$elm$core$Platform$Cmd$none);
					} else {
						if ((!_Utils_eq(
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.dK;
								},
								model.N),
							$elm$core$Maybe$Just(draft.dd))) || (!_Utils_eq(
							A2(
								$elm$core$Maybe$map,
								function ($) {
									return $.a_;
								},
								model.N),
							$elm$core$Maybe$Just(draft.de)))) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{e: 'Repository changed. Refresh, inspect, then adopt tokens.'}),
								$elm$core$Platform$Cmd$none);
						} else {
							var _v0 = $author$project$View$Forms$build(draft);
							if (_v0.$ === 1) {
								var problem = _v0.a;
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{e: problem}),
									$elm$core$Platform$Cmd$none);
							} else {
								var body = _v0.a;
								var path = A2(
									$author$project$Route$mutationPath,
									$author$project$View$Forms$actionName(draft.aw),
									draft.es);
								var identifier = 'ui-' + $elm$core$String$fromInt(model.aD);
								var _v1 = A5(
									$author$project$Main$issue,
									4,
									'POST',
									path,
									$elm$core$Maybe$Just(body),
									model);
								var next = _v1.a;
								var command = _v1.b;
								return _Utils_Tuple2(
									_Utils_update(
										next,
										{
											Z: $elm$core$Maybe$Just(identifier),
											e: 'Submitting checked operation…'
										}),
									command);
							}
						}
					}
				}
			}
		}
	}
};
var $author$project$Main$update = F2(
	function (message, model) {
		switch (message.$) {
			case 0:
				var value = message.a;
				return A2($author$project$Main$receive, value, model);
			case 1:
				var selected = message.a;
				var current = model.c;
				var query = _Utils_update(
					current,
					{c0: selected});
				return $author$project$Main$load(
					_Utils_update(
						model,
						{g: $elm$core$Maybe$Nothing, j: 0, c: query, d: true}));
			case 2:
				var key = message.a;
				var content = message.b;
				var current = model.c;
				var query = function () {
					switch (key) {
						case 'revision':
							return _Utils_update(
								current,
								{aG: content});
						case 'text':
							return _Utils_update(
								current,
								{b7: content});
						case 'mode':
							return _Utils_update(
								current,
								{bJ: content});
						case 'projection':
							return _Utils_update(
								current,
								{bj: content});
						case 'domain':
							return _Utils_update(
								current,
								{by: content});
						case 'file':
							return _Utils_update(
								current,
								{dF: content});
						case 'actor':
							return _Utils_update(
								current,
								{a3: content});
						case 'since':
							return _Utils_update(
								current,
								{aH: content});
						case 'until':
							return _Utils_update(
								current,
								{aK: content});
						case 'limit':
							return _Utils_update(
								current,
								{
									cz: A2(
										$elm$core$Maybe$withDefault,
										0,
										$elm$core$String$toInt(content))
								});
						case 'order':
							return _Utils_update(
								current,
								{bh: content});
						case 'adr':
							return _Utils_update(
								current,
								{J: content});
						case 'compareFrom':
							return _Utils_update(
								current,
								{a8: content});
						case 'compareTo':
							return _Utils_update(
								current,
								{a9: content});
						default:
							return current;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{j: 0, c: query, d: true}),
					$elm$core$Platform$Cmd$none);
			case 3:
				var key = message.a;
				var checked = message.b;
				var current = model.c;
				var query = function () {
					switch (key) {
						case 'includeObsolete':
							return _Utils_update(
								current,
								{bd: checked});
						case 'shallow':
							return _Utils_update(
								current,
								{b0: checked});
						case 'worktree':
							return _Utils_update(
								current,
								{ex: checked});
						default:
							return current;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{j: 0, c: query, d: true}),
					$elm$core$Platform$Cmd$none);
			case 4:
				return $author$project$Main$load(model);
			case 5:
				var adr = message.a;
				return A2($author$project$Main$selectAdr, adr, model);
			case 6:
				var adr = message.a;
				var revision = message.b;
				return A3(
					$author$project$Main$selectAdrAt,
					adr,
					revision,
					_Utils_update(
						model,
						{
							s: $elm$core$Maybe$Nothing,
							D: $elm$core$Maybe$Just(adr)
						}));
			case 7:
				var page = message.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							j: A3(
								$elm$core$Basics$clamp,
								0,
								$author$project$Main$maxPage(model),
								page)
						}),
					$elm$core$Platform$Cmd$none);
			case 8:
				return $author$project$Main$refresh(model);
			case 9:
				var action = message.a;
				if (model.k) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{e: $author$project$Main$terminalMessage}),
						$elm$core$Platform$Cmd$none);
				} else {
					if ($author$project$Main$isHistorical(model)) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{e: 'Historical inspection is read-only. Return to current HEAD first.'}),
							$elm$core$Platform$Cmd$none);
					} else {
						var _v3 = model.N;
						if (_v3.$ === 1) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{e: 'Refresh the repository before editing.'}),
								$elm$core$Platform$Cmd$none);
						} else {
							var repository = _v3.a;
							return ((!model.q) || ((!(!action)) && (!$author$project$Main$inspectionReady(model)))) ? _Utils_Tuple2(
								_Utils_update(
									model,
									{e: 'Wait for fresh repository and both inspection views before editing.'}),
								$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
								_Utils_update(
									model,
									{
										b: A4($author$project$View$Forms$begin, action, repository, model.s, model.b),
										z: model.z + 1,
										e: 'Review original values against current heads before submitting.'
									}),
								$elm$core$Platform$Cmd$none);
						}
					}
				}
			case 10:
				var field = message.a;
				var content = message.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							b: A3($author$project$View$Forms$change, field, content, model.b),
							z: model.z + 1
						}),
					$elm$core$Platform$Cmd$none);
			case 11:
				var checked = message.a;
				var current = model.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							b: _Utils_update(
								current,
								{dt: true, eg: checked}),
							z: model.z + 1
						}),
					$elm$core$Platform$Cmd$none);
			case 12:
				if (model.k) {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{e: $author$project$Main$terminalMessage}),
						$elm$core$Platform$Cmd$none);
				} else {
					if ($author$project$Main$isHistorical(model)) {
						return _Utils_Tuple2(
							_Utils_update(
								model,
								{e: 'Historical inspection is read-only. Return to current HEAD first.'}),
							$elm$core$Platform$Cmd$none);
					} else {
						var _v4 = model.N;
						if (_v4.$ === 1) {
							return _Utils_Tuple2(
								_Utils_update(
									model,
									{e: 'Refresh the repository first.'}),
								$elm$core$Platform$Cmd$none);
						} else {
							var repository = _v4.a;
							if (!model.q) {
								return _Utils_Tuple2(
									_Utils_update(
										model,
										{e: 'Wait for a fresh repository read before adopting tokens.'}),
									$elm$core$Platform$Cmd$none);
							} else {
								if (!model.b.aw) {
									return _Utils_Tuple2(
										_Utils_update(
											model,
											{
												b: A2($author$project$View$Forms$adoptCreate, repository, model.b),
												e: 'Current repository basis adopted.'
											}),
										$elm$core$Platform$Cmd$none);
								} else {
									if ($author$project$Main$inspectionReady(model)) {
										var _v5 = model.s;
										if (!_v5.$) {
											var inspection = _v5.a;
											var _v6 = A3($author$project$View$Forms$adopt, repository, inspection, model.b);
											if (!_v6.$) {
												var draft = _v6.a;
												return _Utils_Tuple2(
													_Utils_update(
														model,
														{b: draft, e: 'Current heads reviewed; fresh tokens adopted.'}),
													$elm$core$Platform$Cmd$none);
											} else {
												var problem = _v6.a;
												return _Utils_Tuple2(
													_Utils_update(
														model,
														{e: problem}),
													$elm$core$Platform$Cmd$none);
											}
										} else {
											return _Utils_Tuple2(
												_Utils_update(
													model,
													{e: 'Inspect the target ADR before adopting tokens.'}),
												$elm$core$Platform$Cmd$none);
										}
									} else {
										return _Utils_Tuple2(
											_Utils_update(
												model,
												{e: 'Wait for fresh collapsed and exploded inspection at current HEAD.'}),
											$elm$core$Platform$Cmd$none);
									}
								}
							}
						}
					}
				}
			case 13:
				return $author$project$Main$submit(model);
			case 14:
				return (model.k || ((!model.m) || (model.ag > 4))) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{P: 'unavailable'}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{P: 'connecting'}),
					$author$project$Main$toJs(
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'type',
									$elm$json$Json$Encode$string('connect'))
								]))));
			case 15:
				var requestId = message.a;
				var pending = message.b;
				return A3($author$project$Main$readPendingCurrent, requestId, pending, model) ? A5($author$project$Main$issue, pending.be, 'GET', pending.az, $elm$core$Maybe$Nothing, model) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			default:
				var kind = message.a;
				var _v7 = _Utils_Tuple3(kind, model.D, model.X);
				_v7$2:
				while (true) {
					if ((!_v7.b.$) && (!_v7.c.$)) {
						switch (_v7.a) {
							case 2:
								var _v8 = _v7.a;
								var adr = _v7.b.a;
								var revision = _v7.c.a;
								return A3(
									$author$project$Main$retryInspection,
									kind,
									A3($author$project$Route$showPath, adr, revision, 'collapsed'),
									model);
							case 3:
								var _v9 = _v7.a;
								var adr = _v7.b.a;
								var revision = _v7.c.a;
								return A3(
									$author$project$Main$retryInspection,
									kind,
									A3($author$project$Route$showPath, adr, revision, 'exploded'),
									model);
							default:
								break _v7$2;
						}
					} else {
						break _v7$2;
					}
				}
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$AdoptTokens = {$: 12};
var $author$project$Main$EditDraft = F2(
	function (a, b) {
		return {$: 10, a: a, b: b};
	});
var $author$project$Main$ResolveDraft = function (a) {
	return {$: 11, a: a};
};
var $author$project$Main$StartAction = function (a) {
	return {$: 9, a: a};
};
var $author$project$Main$Submit = {$: 13};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$View$Forms$ActorId = 12;
var $author$project$View$Forms$ActorKind = 11;
var $author$project$View$Forms$ActorModel = 13;
var $author$project$View$Forms$Add = 7;
var $author$project$View$Forms$Amend = 1;
var $author$project$View$Forms$Body = 2;
var $author$project$View$Forms$ChangeSummary = 3;
var $author$project$View$Forms$ContextDigest = 16;
var $author$project$View$Forms$Domains = 5;
var $author$project$View$Forms$InputDigest = 14;
var $author$project$View$Forms$Mode = 17;
var $author$project$View$Forms$Obsolete = 4;
var $author$project$View$Forms$PromptDigest = 15;
var $author$project$View$Forms$Reactivate = 5;
var $author$project$View$Forms$Reason = 4;
var $author$project$View$Forms$Refinements = 9;
var $author$project$View$Forms$Remove = 8;
var $author$project$View$Forms$Replacement = 10;
var $author$project$View$Forms$Scope = 2;
var $author$project$View$Forms$Scopes = 6;
var $author$project$View$Forms$Summary = 1;
var $author$project$View$Forms$Title = 0;
var $author$project$View$Forms$actionFrom = function (raw) {
	switch (raw) {
		case 'amend':
			return 1;
		case 'scope':
			return 2;
		case 'domain':
			return 3;
		case 'obsolete':
			return 4;
		case 'reactivate':
			return 5;
		default:
			return 0;
	}
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$fieldset = _VirtualDom_node('fieldset');
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$legend = _VirtualDom_node('legend');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$targetChecked = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'checked']),
	$elm$json$Json$Decode$bool);
var $elm$html$Html$Events$onCheck = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetChecked));
};
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
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
var $elm$html$Html$option = _VirtualDom_node('option');
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$View$Forms$reviewView = F2(
	function (label, review) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'HEAD ' + (review.dK + (' · ' + A2($elm$core$Maybe$withDefault, 'detached', review.dL))))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Repository basis token: ' + review.de)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('ADR state token: ' + review.a_)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Title: ' + review.F)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Summary: ' + review.Q)
						])),
					A2(
					$elm$html$Html$pre,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(review.am)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Domains: ' + A2($elm$core$String$join, ', ', review.L))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Scopes: ' + A2($elm$core$String$join, ', ', review.O))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Heads: ' + A2($elm$core$String$join, ', ', review.dM))
						])),
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (candidate) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(candidate)
									]));
						},
						review.a4))
				]));
	});
var $elm$html$Html$select = _VirtualDom_node('select');
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$View$Forms$view = function (controls) {
	var named = F3(
		function (field, name, content) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for(name)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(name)
							])),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id(name),
								$elm$html$Html$Attributes$value(content),
								$elm$html$Html$Events$onInput(
								controls.d6(field))
							]),
						_List_Nil)
					]));
		});
	var multiline = F3(
		function (field, name, content) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for(name)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(name)
							])),
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id(name),
								$elm$html$Html$Attributes$value(content),
								$elm$html$Html$Events$onInput(
								controls.d6(field))
							]),
						_List_Nil)
					]));
		});
	var draft = controls.b;
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Checked operation')
					])),
				A2(
				$elm$html$Html$label,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$for('action')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Action')
					])),
				A2(
				$elm$html$Html$select,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('action'),
						$elm$html$Html$Attributes$disabled(!controls.di),
						$elm$html$Html$Events$onInput(
						function (value) {
							return controls.d7(
								$author$project$View$Forms$actionFrom(value));
						})
					]),
				A2(
					$elm$core$List$map,
					function (action) {
						return A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value(
									$author$project$View$Forms$actionName(action)),
									$elm$html$Html$Attributes$selected(
									_Utils_eq(draft.aw, action)),
									$elm$html$Html$Attributes$disabled((!(!action)) && (!controls.dj))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(
									$author$project$View$Forms$actionName(action))
								]));
					},
					_List_fromArray(
						[0, 1, 2, 3, 4, 5]))),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Target: ' + ((draft.es === '') ? 'new ADR' : draft.es))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Original HEAD: ' + draft.dd)
					])),
				draft.b1 ? A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Draft is stale. Refresh the repository, inspect current heads and candidates, then adopt the new tokens.')
					])) : $elm$html$Html$text(''),
				((!draft.aw) || (draft.aw === 1)) ? A2(
				$elm$html$Html$div,
				_List_Nil,
				_Utils_ap(
					_List_fromArray(
						[
							A3(named, 0, 'Title', draft.F),
							A3(multiline, 1, 'Summary', draft.Q),
							A3(multiline, 2, 'Body', draft.am)
						]),
					(draft.aw === 1) ? _List_fromArray(
						[
							A3(named, 3, 'Change summary', draft.ax)
						]) : _List_fromArray(
						[
							A3(multiline, 5, 'Domains, one per line', draft.L),
							A3(multiline, 6, 'Scopes, one per line', draft.O)
						]))) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_Utils_ap(
					_List_fromArray(
						[
							A3(named, 4, 'Reason', draft.cO)
						]),
					_Utils_ap(
						((draft.aw === 2) || (draft.aw === 3)) ? _Utils_ap(
							_List_fromArray(
								[
									A2(
									$elm$html$Html$label,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$for('change-mode')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Change mode')
										])),
									A2(
									$elm$html$Html$select,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('change-mode'),
											$elm$html$Html$Events$onInput(
											controls.d6(17))
										]),
									A2(
										$elm$core$List$map,
										function (mode) {
											return A2(
												$elm$html$Html$option,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$value(mode),
														$elm$html$Html$Attributes$selected(
														_Utils_eq(draft.bJ, mode))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(mode)
													]));
										},
										(draft.aw === 3) ? _List_fromArray(
											['delta', 'reviewed', 'refine']) : _List_fromArray(
											['delta', 'reviewed'])))
								]),
							function () {
								var _v0 = draft.bJ;
								switch (_v0) {
									case 'delta':
										return _List_fromArray(
											[
												A3(multiline, 7, 'Add, one per line', draft.aO),
												A3(multiline, 8, 'Remove, one per line', draft.aY)
											]);
									case 'refine':
										return _List_fromArray(
											[
												A3(multiline, 9, 'Refinements, one per line', draft.bV)
											]);
									default:
										return _List_fromArray(
											[
												A3(
												multiline,
												(draft.aw === 2) ? 6 : 5,
												'Reviewed set, one per line',
												(draft.aw === 2) ? draft.O : draft.L)
											]);
								}
							}()) : _List_fromArray(
							[
								A2(
								$elm$html$Html$label,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$for('resolve')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Resolve status conflict')
									])),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$id('resolve'),
										$elm$html$Html$Attributes$type_('checkbox'),
										$elm$html$Html$Attributes$checked(draft.eg),
										$elm$html$Html$Events$onCheck(controls.d8)
									]),
								_List_Nil)
							]),
						(draft.aw === 4) ? _List_fromArray(
							[
								A3(named, 10, 'Replacement ADR (optional)', draft.bY)
							]) : _List_Nil))),
				A2(
				$elm$html$Html$fieldset,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$legend,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Actor and provenance')
							])),
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for('actor-kind')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Actor kind')
							])),
						A2(
						$elm$html$Html$select,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id('actor-kind'),
								$elm$html$Html$Events$onInput(
								controls.d6(11))
							]),
						A2(
							$elm$core$List$map,
							function (kind) {
								return A2(
									$elm$html$Html$option,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$value(kind),
											$elm$html$Html$Attributes$selected(
											_Utils_eq(draft.ab, kind))
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(kind)
										]));
							},
							_List_fromArray(
								['human', 'llm', 'service']))),
						A3(named, 12, 'Actor ID', draft.aa),
						A3(named, 13, 'Model (optional)', draft.ac),
						A3(named, 14, 'Input digest (optional)', draft.bD),
						A3(named, 15, 'Prompt digest (optional)', draft.bQ),
						A3(named, 16, 'Context digest (optional)', draft.bv)
					])),
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Review original and current state')
					])),
				function () {
				var _v1 = draft.aV;
				if (!_v1.$) {
					var original = _v1.a;
					return A2($author$project$View$Forms$reviewView, 'Original reviewed state', original);
				} else {
					return A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('No original snapshot has been reviewed yet.')
							]));
				}
			}(),
				function () {
				var _v2 = controls.dI;
				if (!_v2.$) {
					var repository = _v2.a;
					return A2(
						$author$project$View$Forms$reviewView,
						'Current freshly inspected state',
						A2($author$project$View$Forms$baseline, repository, controls.dH));
				} else {
					return A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Current state is not fresh. Refresh the repository and inspect both views.')
							]));
				}
			}(),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('button'),
						$elm$html$Html$Attributes$disabled(!controls.dh),
						$elm$html$Html$Events$onClick(controls.d5)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						(!draft.aw) ? 'Review repository and adopt current basis' : 'Review heads and adopt current tokens')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('button'),
						$elm$html$Html$Attributes$disabled(!controls.dk),
						$elm$html$Html$Events$onClick(controls.d9)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Submit ' + $author$project$View$Forms$actionName(draft.aw))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(controls.at)
					]))
			]));
};
var $author$project$Main$actionsPane = function (model) {
	var historical = $author$project$Main$isHistorical(model);
	var canSubmit = model.m && ((!model.k) && (model.q && (_Utils_eq(model.Z, $elm$core$Maybe$Nothing) && ((!historical) && ((!model.b.b1) && ((!model.b.aw) || (model.b.bZ && $author$project$Main$inspectionReady(model))))))));
	var canChooseAction = (!model.k) && ((!historical) && model.q);
	var canAdopt = model.m && ((!model.k) && ((!historical) && (model.q && ((!model.b.aw) || $author$project$Main$inspectionReady(model)))));
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				historical ? A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('notice')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Historical inspection is read-only. Return to current HEAD before submitting changes.')
					])) : $elm$html$Html$text(''),
				(!model.m) ? A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('notice')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('This cleaned-URL session can read snapshots. Reopen the process bootstrap URL for a new in-memory credential before submitting.')
					])) : $elm$html$Html$text(''),
				$author$project$View$Forms$view(
				{
					dh: canAdopt,
					di: canChooseAction,
					dj: canChooseAction && $author$project$Main$inspectionReady(model),
					dk: canSubmit,
					b: model.b,
					dH: $author$project$Main$inspectionReady(model) ? model.s : $elm$core$Maybe$Nothing,
					dI: (model.q && ((!model.b.aw) || $author$project$Main$inspectionReady(model))) ? model.N : $elm$core$Maybe$Nothing,
					d5: $author$project$Main$AdoptTokens,
					d6: $author$project$Main$EditDraft,
					d7: $author$project$Main$StartAction,
					d8: $author$project$Main$ResolveDraft,
					d9: $author$project$Main$Submit,
					at: model.e
				})
			]));
};
var $author$project$Main$ChooseView = function (a) {
	return {$: 1, a: a};
};
var $author$project$Route$Conflicts = 5;
var $author$project$Route$Doctor = 6;
var $author$project$Main$EditQuery = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $author$project$Route$History = 3;
var $author$project$Main$Load = {$: 4};
var $author$project$Main$Refresh = {$: 8};
var $author$project$Route$Search = 1;
var $author$project$Main$ToggleQuery = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $author$project$Main$SelectAdr = function (a) {
	return {$: 5, a: a};
};
var $author$project$Main$SelectCompareAdr = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $elm$html$Html$article = _VirtualDom_node('article');
var $elm$core$String$fromFloat = _String_fromNumber;
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
var $author$project$Route$pageSlice = F2(
	function (number, items) {
		return A2(
			$elm$core$List$take,
			$author$project$Route$pageSize,
			A2(
				$elm$core$List$drop,
				A2($elm$core$Basics$max, 0, number) * $author$project$Route$pageSize,
				items));
	});
var $author$project$Main$SetPage = function (a) {
	return {$: 7, a: a};
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$pager = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('pager')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('button'),
						$elm$html$Html$Attributes$disabled(model.j <= 0),
						$elm$html$Html$Events$onClick(
						$author$project$Main$SetPage(model.j - 1))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Previous page')
					])),
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Page ' + ($elm$core$String$fromInt(model.j + 1) + (' of ' + $elm$core$String$fromInt(
							$author$project$Main$maxPage(model) + 1))))
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('button'),
						$elm$html$Html$Attributes$disabled(
						_Utils_cmp(
							model.j,
							$author$project$Main$maxPage(model)) > -1),
						$elm$html$Html$Events$onClick(
						$author$project$Main$SetPage(model.j + 1))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Next page')
					]))
			]));
};
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $author$project$Main$searchResults = function (model) {
	var _v0 = model.ah;
	if (_v0.$ === 1) {
		return A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No result window loaded.')
				]));
	} else {
		var window = _v0.a;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('meta')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							'At ' + (window.aj + (' · ' + ($elm$core$String$fromInt(
								$elm$core$List$length(window.cR)) + ' results in a bounded window'))))
						])),
					(_Utils_cmp(
					$elm$core$List$length(window.cR),
					window.cz) > -1) ? A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('notice')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Result window full; more may exist.')
						])) : $elm$html$Html$text(''),
					$author$project$Main$pager(model),
					A2(
					$elm$html$Html$ul,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('result-list')
						]),
					A2(
						$elm$core$List$map,
						function (hit) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('button'),
												$elm$html$Html$Events$onClick(
												$author$project$Main$SelectAdr(hit.J))
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$strong,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(hit.F)
													])),
												$elm$html$Html$text(' · ' + hit.J)
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(hit.Q)
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('meta')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												hit.at + (' · ' + (A2($elm$core$String$join, ', ', hit.L) + (' · score ' + A2(
													$elm$core$Maybe$withDefault,
													'n/a',
													A2($elm$core$Maybe$map, $elm$core$String$fromFloat, hit.bp))))))
											])),
										$elm$core$List$isEmpty(hit.bG) ? $elm$html$Html$text('') : A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												'Matched ' + (A2($elm$core$String$join, ', ', hit.bG) + (' for ' + A2($elm$core$String$join, ', ', hit.bH))))
											])),
										hit.aF ? A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('notice')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												'Conflict: ' + A2($elm$core$String$join, '; ', hit.an))
											])) : $elm$html$Html$text('')
									]));
						},
						A2($author$project$Route$pageSlice, model.j, window.cR)))
				]));
	}
};
var $author$project$Main$snapshotView = F2(
	function (label, snapshot) {
		if (snapshot.$ === 1) {
			return A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(label + ': absent at this revision')
					]));
		} else {
			var value = snapshot.a;
			return A2(
				$elm$html$Html$article,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('candidate')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(label)
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$strong,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(value.F)
									])),
								$elm$html$Html$text(' · ' + value.at)
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(value.Q)
							])),
						A2(
						$elm$html$Html$pre,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('body-text')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(value.am)
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'Domains: ' + A2($elm$core$String$join, ', ', value.L))
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								'Scopes: ' + A2($elm$core$String$join, ', ', value.O))
							]))
					]));
		}
	});
var $author$project$Main$results = function (model) {
	var _v0 = model.c.c0;
	switch (_v0) {
		case 0:
			return $author$project$Main$searchResults(model);
		case 1:
			return $author$project$Main$searchResults(model);
		case 2:
			var _v1 = model.aX;
			if (_v1.$ === 1) {
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No relevance results loaded.')
						]));
			} else {
				var window = _v1.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('meta')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Source: ' + (window.dF + (' · ' + window.en)))
								])),
							A2(
							$elm$html$Html$ul,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('result-list')
								]),
							A2(
								$elm$core$List$map,
								function (hit) {
									return A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Events$onClick(
														$author$project$Main$SelectAdr(hit.J))
													]),
												_List_fromArray(
													[
														A2(
														$elm$html$Html$strong,
														_List_Nil,
														_List_fromArray(
															[
																$elm$html$Html$text(hit.F)
															])),
														$elm$html$Html$text(' · ' + hit.J)
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(hit.Q)
													])),
												A2(
												$elm$html$Html$p,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('meta')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(
														'Declared applicability: ' + (hit.ek + (' · Semantic evidence: ' + (hit.$7 + (' · Score ' + $elm$core$String$fromFloat(hit.bp))))))
													])),
												A2(
												$elm$html$Html$ul,
												_List_Nil,
												A2(
													$elm$core$List$map,
													function (e) {
														return A2(
															$elm$html$Html$li,
															_List_Nil,
															_List_fromArray(
																[
																	$elm$html$Html$text(e.el + (': ' + (e.dG + (' ↔ ' + e.c8))))
																]));
													},
													hit.dz))
											]));
								},
								window.cR))
						]));
			}
		case 3:
			var _v2 = model.aC;
			if (_v2.$ === 1) {
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No history loaded.')
						]));
			} else {
				var window = _v2.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							window.eu ? A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('notice')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('History window truncated by the server.')
								])) : $elm$html$Html$text(''),
							$author$project$Main$pager(model),
							A2(
							$elm$html$Html$ul,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('result-list')
								]),
							A2(
								$elm$core$List$map,
								function (item) {
									return A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Events$onClick(
														$author$project$Main$SelectAdr(item.J))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(item.F + (' · ' + item.cx))
													])),
												A2(
												$elm$html$Html$p,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('meta')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(item.a3 + (' · ' + (item.a6 + (' · ' + item.aE))))
													])),
												A2(
												$elm$html$Html$p,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(
														A2($elm$core$String$join, ', ', item.a5))
													]))
											]));
								},
								A2($author$project$Route$pageSlice, model.j, window.bg)))
						]));
			}
		case 4:
			var _v3 = model.ay;
			if (_v3.$ === 1) {
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No comparison loaded.')
						]));
			} else {
				var window = _v3.a;
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('meta')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(window.dJ + (' → ' + window.et))
								])),
							A2(
							$elm$html$Html$ul,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('result-list')
								]),
							A2(
								$elm$core$List$map,
								function (entry) {
									return A2(
										$elm$html$Html$li,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$type_('button'),
														$elm$html$Html$Events$onClick(
														A2(
															$author$project$Main$SelectCompareAdr,
															entry.J,
															_Utils_eq(entry.b8, $elm$core$Maybe$Nothing) ? window.dJ : window.et))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(entry.F + (' · ' + entry.be))
													])),
												A2($author$project$Main$snapshotView, 'Before', entry.cc),
												A2($author$project$Main$snapshotView, 'After', entry.b8),
												A2(
												$elm$html$Html$div,
												_List_Nil,
												A2(
													$elm$core$List$map,
													function (change) {
														return A2(
															$elm$html$Html$article,
															_List_fromArray(
																[
																	$elm$html$Html$Attributes$class('candidate')
																]),
															_List_fromArray(
																[
																	A2(
																	$elm$html$Html$strong,
																	_List_Nil,
																	_List_fromArray(
																		[
																			$elm$html$Html$text(change.dE)
																		])),
																	A2(
																	$elm$html$Html$p,
																	_List_Nil,
																	_List_fromArray(
																		[
																			$elm$html$Html$text('Before: ' + change.cc)
																		])),
																	A2(
																	$elm$html$Html$p,
																	_List_Nil,
																	_List_fromArray(
																		[
																			$elm$html$Html$text('After: ' + change.b8)
																		])),
																	function () {
																	var _v4 = change.ds;
																	if (!_v4.$) {
																		var difference = _v4.a;
																		return A2(
																			$elm$html$Html$pre,
																			_List_fromArray(
																				[
																					$elm$html$Html$Attributes$class('body-text')
																				]),
																			_List_fromArray(
																				[
																					$elm$html$Html$text(difference)
																				]));
																	} else {
																		return $elm$html$Html$text('');
																	}
																}()
																]));
													},
													entry.a5))
											]));
								},
								window.dx))
						]));
			}
		case 5:
			var _v5 = model.an;
			if (_v5.$ === 1) {
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No conflict index loaded.')
						]));
			} else {
				var window = _v5.a;
				return $elm$core$List$isEmpty(window.an) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No conflicts at this revision.')
						])) : A2(
					$elm$html$Html$ul,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('result-list')
						]),
					A2(
						$elm$core$List$map,
						function (entry) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('button'),
												$elm$html$Html$Events$onClick(
												$author$project$Main$SelectAdr(entry.J))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(entry.J)
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												A2($elm$core$String$join, '; ', entry.ep))
											])),
										A2(
										$elm$html$Html$ul,
										_List_Nil,
										A2(
											$elm$core$List$map,
											function (candidate) {
												return A2(
													$elm$html$Html$li,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text(
															candidate.da + (': ' + (candidate.Q + (' [' + (A2($elm$core$String$join, ', ', candidate.dM) + ']')))))
														]));
											},
											entry.a4))
									]));
						},
						window.an));
			}
		default:
			var _v6 = model.ba;
			if (_v6.$ === 1) {
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No diagnostics loaded.')
						]));
			} else {
				var doctor = _v6.a;
				return $elm$core$List$isEmpty(doctor.dR) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No issues reported.')
						])) : A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (diagnostic) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										diagnostic.em + (' · ' + (diagnostic.bu + (': ' + (diagnostic.cA + (' ' + A2($elm$core$Maybe$withDefault, '', diagnostic.bi)))))))
									]));
						},
						doctor.dR));
			}
	}
};
var $author$project$Main$searchControls = F4(
	function (model, field, check, withText) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					withText ? A3(field, 'text', 'Search terms', model.c.b7) : A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Browse the ordered result window.')
						])),
					A2(
					$elm$html$Html$label,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$for('mode')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Retrieval mode')
						])),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('mode'),
							$elm$html$Html$Events$onInput(
							$author$project$Main$EditQuery('mode'))
						]),
					A2(
						$elm$core$List$map,
						function (mode) {
							return A2(
								$elm$html$Html$option,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$value(mode),
										$elm$html$Html$Attributes$selected(
										_Utils_eq(model.c.bJ, mode))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(mode)
									]));
						},
						_List_fromArray(
							['hybrid', 'fts', 'vector']))),
					A2(
					$elm$html$Html$label,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$for('projection')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Result view')
						])),
					A2(
					$elm$html$Html$select,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id('projection'),
							$elm$html$Html$Events$onInput(
							$author$project$Main$EditQuery('projection'))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value('collapsed'),
									$elm$html$Html$Attributes$selected(model.c.bj === 'collapsed')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Collapsed')
								])),
							A2(
							$elm$html$Html$option,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$value('exploded'),
									$elm$html$Html$Attributes$selected(model.c.bj === 'exploded')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Exploded')
								]))
						])),
					A3(field, 'domain', 'Domain filter', model.c.by),
					A3(field, 'file', 'File scope filter', model.c.dF),
					A3(field, 'actor', 'Actor (kind:identifier)', model.c.a3),
					A3(field, 'since', 'Since (Unix milliseconds)', model.c.aH),
					A3(field, 'until', 'Until (Unix milliseconds)', model.c.aK),
					A3(check, 'includeObsolete', 'Include obsolete', model.c.bd),
					A3(check, 'shallow', 'Shallow history', model.c.b0),
					A3(
					field,
					'limit',
					'Window size (1–1000)',
					$elm$core$String$fromInt(model.c.cz))
				]));
	});
var $author$project$Main$contextPane = function (model) {
	var field = F3(
		function (key, title, content) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('field')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for(key)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(title)
							])),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$id(key),
								$elm$html$Html$Attributes$value(content),
								$elm$html$Html$Events$onInput(
								$author$project$Main$EditQuery(key))
							]),
						_List_Nil)
					]));
		});
	var check = F3(
		function (key, title, selected) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for(key)
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$id(key),
										$elm$html$Html$Attributes$type_('checkbox'),
										$elm$html$Html$Attributes$checked(selected),
										$elm$html$Html$Events$onCheck(
										$author$project$Main$ToggleQuery(key))
									]),
								_List_Nil),
								$elm$html$Html$text(title)
							]))
					]));
		});
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				(!model.m) ? A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('notice')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Read-only session. Reopen the process bootstrap URL to restore live updates and checked mutations.')
					])) : A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Live connection: ' + model.P)
					])),
				function () {
				var _v0 = model.M;
				if (!_v0.$) {
					var reason = _v0.a;
					return A2(
						$elm$html$Html$p,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('error')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Live observation unavailable: ' + reason)
							]));
				} else {
					var _v1 = model.g;
					if (!_v1.$) {
						var problem = _v1.a;
						return A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('error')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(problem)
								]));
					} else {
						return $elm$html$Html$text('');
					}
				}
			}(),
				(model.d || (!_Utils_eq(model.M, $elm$core$Maybe$Nothing))) ? A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('notice')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Snapshot is loading or stale.')
					])) : $elm$html$Html$text(''),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('nav')
					]),
				A2(
					$elm$core$List$map,
					function (_v2) {
						var name = _v2.a;
						var kind = _v2.b;
						return A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('button'),
									$elm$html$Html$Attributes$class(
									_Utils_eq(model.c.c0, kind) ? 'selected' : ''),
									$elm$html$Html$Events$onClick(
									$author$project$Main$ChooseView(kind))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(name)
								]));
					},
					_List_fromArray(
						[
							_Utils_Tuple2('Browse', 0),
							_Utils_Tuple2('Search', 1),
							_Utils_Tuple2('Relevant', 2),
							_Utils_Tuple2('History', 3),
							_Utils_Tuple2('Compare', 4),
							_Utils_Tuple2('Conflicts', 5),
							_Utils_Tuple2('Doctor', 6)
						]))),
				A3(field, 'revision', 'Revision (HEAD or exact commit)', model.c.aG),
				function () {
				var _v3 = model.c.c0;
				switch (_v3) {
					case 0:
						return A4($author$project$Main$searchControls, model, field, check, false);
					case 1:
						return A4($author$project$Main$searchControls, model, field, check, true);
					case 2:
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A3(field, 'file', 'Repository-relative source file', model.c.dF),
									A3(check, 'worktree', 'Use worktree source', model.c.ex),
									A3(check, 'includeObsolete', 'Include obsolete', model.c.bd),
									A3(
									field,
									'limit',
									'Result limit (1–100)',
									$elm$core$String$fromInt(model.c.cz))
								]));
					case 3:
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A3(field, 'adr', 'ADR filter', model.c.J),
									A3(field, 'actor', 'Actor (kind:identifier)', model.c.a3),
									A3(field, 'since', 'Since (Unix milliseconds)', model.c.aH),
									A3(field, 'until', 'Until (Unix milliseconds)', model.c.aK),
									A3(
									field,
									'limit',
									'Window size (1–1000)',
									$elm$core$String$fromInt(model.c.cz)),
									A2(
									$elm$html$Html$label,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$for('order')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Order')
										])),
									A2(
									$elm$html$Html$select,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('order'),
											$elm$html$Html$Events$onInput(
											$author$project$Main$EditQuery('order'))
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$option,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$value('newest'),
													$elm$html$Html$Attributes$selected(model.c.bh === 'newest')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Newest')
												])),
											A2(
											$elm$html$Html$option,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$value('oldest'),
													$elm$html$Html$Attributes$selected(model.c.bh === 'oldest')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Oldest')
												]))
										]))
								]));
					case 4:
						return A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A3(field, 'compareFrom', 'From revision', model.c.a8),
									A3(field, 'compareTo', 'To revision', model.c.a9)
								]));
					default:
						return $elm$html$Html$text('');
				}
			}(),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('actions')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$elm$html$Html$Attributes$class('primary'),
								$elm$html$Html$Events$onClick($author$project$Main$Load)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Load view')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('button'),
								$elm$html$Html$Events$onClick($author$project$Main$Refresh)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Refresh repository')
							]))
					])),
				$author$project$Main$results(model)
			]));
};
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$header = _VirtualDom_node('header');
var $author$project$Main$RetryInspection = function (a) {
	return {$: 16, a: a};
};
var $author$project$Main$inspectionIssuesView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2(
			$elm$core$List$filterMap,
			function (_v0) {
				var kind = _v0.a;
				var label = _v0.b;
				var _v1 = A2(
					$elm$core$Dict$get,
					$author$project$Main$requestKey(kind),
					model.v);
				if (!_v1.$) {
					var problem = _v1.a;
					return $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$p,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('error')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(label + (': ' + problem))
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick(
											$author$project$Main$RetryInspection(kind)),
											$elm$html$Html$Attributes$disabled(model.k)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Retry ' + label)
										]))
								])));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			_List_fromArray(
				[
					_Utils_Tuple2(2, 'decision inspection'),
					_Utils_Tuple2(3, 'operation history')
				])));
};
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
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$candidateBodies = F2(
	function (inspection, candidates) {
		return A2(
			$elm$core$List$map,
			function (candidate) {
				var _v0 = $elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (item) {
							return _Utils_eq(item.bc, candidate.bc);
						},
						A2(
							$elm$core$List$concatMap,
							function ($) {
								return $.dT;
							},
							inspection.bg)));
				if (!_v0.$) {
					var item = _v0.a;
					return _Utils_update(
						candidate,
						{
							am: A2($elm$core$Maybe$withDefault, '', item.am)
						});
				} else {
					return candidate;
				}
			},
			candidates);
	});
var $author$project$Main$candidateView = F3(
	function (axis, heads, candidates) {
		return $elm$core$List$isEmpty(heads) ? $elm$html$Html$text('') : A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(axis + ' heads')
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('meta')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A2($elm$core$String$join, ', ', heads))
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (candidate) {
							return A2(
								$elm$html$Html$article,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('candidate')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$strong,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(candidate.F)
											])),
										A2(
										$elm$html$Html$p,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(candidate.Q)
											])),
										(candidate.am === '') ? $elm$html$Html$text('') : A2(
										$elm$html$Html$pre,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('body-text')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(candidate.am)
											])),
										A2(
										$elm$html$Html$p,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('meta')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(candidate.bc + (' · ' + candidate.bi))
											]))
									]));
						},
						candidates))
				]));
	});
var $author$project$Main$compareDetail = function (model) {
	var _v0 = _Utils_Tuple3(model.c.c0, model.ay, model.D);
	if (((_v0.a === 4) && (!_v0.b.$)) && (!_v0.c.$)) {
		var _v1 = _v0.a;
		var window = _v0.b.a;
		var adr = _v0.c.a;
		var _v2 = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (entry) {
					return _Utils_eq(entry.J, adr);
				},
				window.dx));
		if (!_v2.$) {
			var entry = _v2.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h3,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Comparison: ' + (entry.be + (' · ' + entry.J)))
							])),
						A2($author$project$Main$snapshotView, 'Before', entry.cc),
						A2($author$project$Main$snapshotView, 'After', entry.b8)
					]));
		} else {
			return $elm$html$Html$text('');
		}
	} else {
		return $elm$html$Html$text('');
	}
};
var $elm$html$Html$details = _VirtualDom_node('details');
var $author$project$Main$provenanceView = function (provenance) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('candidate')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Actor: ' + (provenance.a3 + (' · Claimed: ' + provenance.a6)))
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Model: ' + (A2($elm$core$Maybe$withDefault, 'none', provenance.cC) + (' · Basis: ' + (provenance.cb + (' · Operation: ' + provenance.aE)))))
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Input digest: ' + A2($elm$core$Maybe$withDefault, 'none', provenance.bD))
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Prompt digest: ' + A2($elm$core$Maybe$withDefault, 'none', provenance.bQ))
					])),
				A2(
				$elm$html$Html$p,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('meta')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Context digest: ' + A2($elm$core$Maybe$withDefault, 'none', provenance.bv))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Introductions: ' + A2($elm$core$String$join, ', ', provenance.bE))
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Original operation commits: ' + A2($elm$core$String$join, ', ', provenance.bM))
					])),
				A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (placement) {
						return A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(placement)
								]));
					},
					provenance.bO)),
				A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (landing) {
						return A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(landing)
								]));
					},
					provenance.bF))
			]));
};
var $elm$html$Html$summary = _VirtualDom_node('summary');
var $author$project$Main$operationView = function (operation) {
	return A2(
		$elm$html$Html$article,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('candidate')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h4,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Operation ' + operation.bc)
					])),
				function () {
				var _v0 = operation.bk;
				if (_v0.$ === 1) {
					return $elm$html$Html$text('');
				} else {
					var provenance = _v0.a;
					return $author$project$Main$provenanceView(provenance);
				}
			}(),
				A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					function (item) {
						return A2(
							$elm$html$Html$li,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(item.be + (' · ' + item.cl))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$Maybe$withDefault, '', item.F) + (' ' + A2($elm$core$Maybe$withDefault, '', item.Q)))
										])),
									A2(
									$elm$html$Html$pre,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('body-text')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											A2($elm$core$Maybe$withDefault, '', item.am))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Rationale: ' + A2($elm$core$Maybe$withDefault, 'none', item.bS))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Relation: ' + A2($elm$core$Maybe$withDefault, 'none', item.bW))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Parents: ' + A2($elm$core$String$join, ', ', item.bN))
										])),
									A2(
									$elm$html$Html$div,
									_List_Nil,
									A2(
										$elm$core$List$map,
										function (difference) {
											return A2(
												$elm$html$Html$pre,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('body-text')
													]),
												_List_fromArray(
													[
														$elm$html$Html$text(difference)
													]));
										},
										item.bx)),
									function () {
									var _v1 = item.bT;
									if (!_v1.$) {
										var raw = _v1.a;
										return A2(
											$elm$html$Html$details,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('candidate')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$summary,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text('Raw semantic source')
														])),
													A2(
													$elm$html$Html$pre,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('body-text')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(raw)
														]))
												]));
									} else {
										return $elm$html$Html$text('');
									}
								}(),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Scopes: ' + (A2($elm$core$String$join, ', ', item.O) + (' · Domains: ' + A2($elm$core$String$join, ', ', item.L))))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Status: ' + (A2($elm$core$Maybe$withDefault, '', item.b3) + (' · Replacement: ' + A2($elm$core$Maybe$withDefault, '', item.bY))))
										])),
									A2(
									$elm$html$Html$p,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Added: ' + (A2($elm$core$String$join, ', ', item.bt) + (' · Removed: ' + (A2($elm$core$String$join, ', ', item.bX) + (' · Refinements: ' + A2($elm$core$String$join, ', ', item.bV))))))
										])),
									A2(
									$elm$html$Html$p,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('meta')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(item.bc + (' · ' + item.bi))
										]))
								]));
					},
					operation.dT))
			]));
};
var $author$project$Main$statusCandidates = function (inspection) {
	return A2(
		$elm$core$List$filterMap,
		function (item) {
			return A2($elm$core$List$member, item.bc, inspection.b4) ? $elm$core$Maybe$Just(
				{
					am: A2($elm$core$Maybe$withDefault, '', item.bS),
					bc: item.bc,
					bi: item.bi,
					Q: A2($elm$core$Maybe$withDefault, item.cl, item.b3),
					F: 'Status candidate'
				}) : $elm$core$Maybe$Nothing;
		},
		A2(
			$elm$core$List$concatMap,
			function ($) {
				return $.dT;
			},
			inspection.bg));
};
var $author$project$Main$inspectorPane = function (model) {
	var _v0 = model.s;
	if (_v0.$ === 1) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$compareDetail(model),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Select an ADR to inspect its decision, candidates, and operation provenance.')
						]))
				]));
	} else {
		var inspection = _v0.a;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$compareDetail(model),
					A2(
					$elm$html$Html$h3,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							(inspection.F === '') ? inspection.J : inspection.F)
						])),
					A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('meta')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(inspection.J + (' · ' + (inspection.at + (' · ' + inspection.aj))))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(inspection.Q)
						])),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Decision body')
						])),
					A2(
					$elm$html$Html$pre,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('body-text')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(inspection.am)
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Domains: ' + A2($elm$core$String$join, ', ', inspection.L))
						])),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Scopes: ' + A2($elm$core$String$join, ', ', inspection.O))
						])),
					inspection.aF ? A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('notice')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							'Review required: ' + A2($elm$core$String$join, '; ', inspection.an))
						])) : $elm$html$Html$text(''),
					A3(
					$author$project$Main$candidateView,
					'Decision',
					inspection.bU,
					A2($author$project$Main$candidateBodies, inspection, inspection.a4.cP)),
					A3($author$project$Main$candidateView, 'Scope', inspection.b_, inspection.a4.O),
					A3($author$project$Main$candidateView, 'Domain', inspection.bz, inspection.a4.L),
					A3(
					$author$project$Main$candidateView,
					'Status',
					inspection.b4,
					$author$project$Main$statusCandidates(inspection)),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Source paths')
						])),
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (path) {
							return A2(
								$elm$html$Html$li,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(path)
									]));
						},
						inspection.aJ)),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Current provenance')
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2($elm$core$List$map, $author$project$Main$provenanceView, inspection.bk)),
					A2(
					$elm$html$Html$h4,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Operations and provenance')
						])),
					$elm$core$List$isEmpty(inspection.bg) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No exploded operations loaded.')
						])) : $elm$html$Html$text(''),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A2($elm$core$List$map, $author$project$Main$operationView, inspection.bg))
				]));
	}
};
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $author$project$Main$repositoryLabel = function (model) {
	var _v0 = model.N;
	if (!_v0.$) {
		var repository = _v0.a;
		return 'HEAD ' + (repository.dK + (' · ' + A2($elm$core$Maybe$withDefault, 'detached', repository.dL)));
	} else {
		return 'Repository loading';
	}
};
var $elm$html$Html$section = _VirtualDom_node('section');
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$main_,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$header,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('masthead')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h1,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('ADRAI repository explorer')
							])),
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$Main$repositoryLabel(model))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('panes')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pane'),
								$elm$html$Html$Attributes$id('context-pane')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Context and results')
									])),
								$author$project$Main$contextPane(model)
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pane'),
								$elm$html$Html$Attributes$id('inspector-pane')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Inspector')
									])),
								$author$project$Main$inspectionIssuesView(model),
								$author$project$Main$inspectorPane(model)
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('pane'),
								$elm$html$Html$Attributes$id('actions-pane')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Actions and status')
									])),
								$author$project$Main$actionsPane(model)
							]))
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		dQ: $author$project$Main$init,
		eo: function (_v0) {
			return $author$project$Main$fromJs($author$project$Main$FromJs);
		},
		ew: $author$project$Main$update,
		c0: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (hasCredential) {
			return $elm$json$Json$Decode$succeed(
				{m: hasCredential});
		},
		A2($elm$json$Json$Decode$field, 'hasCredential', $elm$json$Json$Decode$bool)))(0)}});}(this));
;
(() => {
  "use strict";
  const credential = typeof window.__ADRAI_TOKEN__ === "string" && window.__ADRAI_TOKEN__
    ? window.__ADRAI_TOKEN__ : null;
  delete window.__ADRAI_TOKEN__;

  const readPaths = new Set([
    "/api/v1/repository", "/api/v1/search", "/api/v1/relevant",
    "/api/v1/history", "/api/v1/compare", "/api/v1/conflicts", "/api/v1/doctor"
  ]);
  const adrRead = /^\/api\/v1\/adrs\/[A-Za-z0-9-]+$/;
  const writePath = /^\/api\/v1\/adrs(?:\/[A-Za-z0-9-]+\/(?:amend|scope|domain|obsolete|reactivate))?$/;
  const socketPath = "/api/v1/events";
  let socket = null;
  let interests = [];
  let app = null;

  function report(payload) {
    if (app) app.ports.fromJs.send(payload);
  }

  function validPath(method, path) {
    if (typeof path !== "string" || path.length > 8192 ||
        !path.startsWith("/api/v1/") || path.includes("\\") ||
        path.includes("#") || path.includes("//")) return false;
    let url;
    try { url = new URL(path, window.location.origin); } catch { return false; }
    if (url.origin !== window.location.origin ||
        url.pathname !== path.split("?")[0] ||
        url.search.length > 4096) return false;
    if (method === "GET") return readPaths.has(url.pathname) || adrRead.test(url.pathname);
    return method === "POST" && !url.search && writePath.test(url.pathname);
  }

  async function request(command) {
    const id = command.request_id;
    if (typeof id !== "string" || id.length > 128 ||
        !validPath(command.method, command.path)) {
      report({ type: "request-failed", request_id: String(id || ""), message: "Invalid API request." });
      return;
    }
    if (command.method === "POST" && !credential) {
      report({ type: "request-failed", request_id: id, message: "Reopen the process bootstrap URL to submit changes." });
      return;
    }
    const headers = {};
    const options = { method: command.method, credentials: "same-origin", headers };
    if (command.method === "POST") {
      headers["Content-Type"] = "application/json";
      headers.Authorization = "Bearer " + credential;
      options.body = JSON.stringify(command.body);
    }
    try {
      const response = await fetch(command.path, options);
      const body = await response.json();
      report({ type: "response", request_id: id, status: response.status, body });
    } catch {
      report({ type: "request-failed", request_id: id, message: "Network or JSON response failed. Inspect repository state before retrying a mutation." });
    }
  }

  function validInterests(paths) {
    return Array.isArray(paths) && paths.length <= 32 &&
      paths.every(path => typeof path === "string" && path.length > 0 &&
        path.length <= 1024 && !path.startsWith("/") && !path.includes("\\") &&
        !path.split("/").some(part => !part || part === "." || part === "..")) &&
      new Set(paths).size === paths.length;
  }

  function sendInterests() {
    if (socket && socket.readyState === WebSocket.OPEN)
      socket.send(JSON.stringify({ type: "active-files", paths: interests }));
  }

  function disconnect() {
    interests = [];
    if (socket) {
      const previous = socket;
      socket = null;
      if (previous.readyState === WebSocket.OPEN)
        previous.send(JSON.stringify({ type: "active-files", paths: [] }));
      previous.close();
    }
    report({ type: "socket-state", state: "closed" });
  }

  function connect() {
    if (!credential) {
      report({ type: "socket-state", state: "unavailable" });
      return;
    }
    if (socket && (socket.readyState === WebSocket.OPEN || socket.readyState === WebSocket.CONNECTING))
      return;
    report({ type: "socket-state", state: "connecting" });
    const scheme = window.location.protocol === "https:" ? "wss:" : "ws:";
    const current = new WebSocket(scheme + "//" + window.location.host + socketPath);
    socket = current;
    current.onopen = () => {
      if (socket !== current) return;
      current.send(JSON.stringify({ type: "authenticate", credential }));
      sendInterests();
      report({ type: "socket-state", state: "open" });
    };
    current.onmessage = event => {
      if (socket !== current || typeof event.data !== "string") return;
      try { report({ type: "event", body: JSON.parse(event.data) }); }
      catch { report({ type: "socket-state", state: "unavailable" }); disconnect(); }
    };
    current.onerror = () => {
      if (socket === current) report({ type: "socket-state", state: "unavailable" });
    };
    current.onclose = event => {
      if (socket !== current) return;
      socket = null;
      if (event.reason === "generation-exhausted; restart the web server")
        report({ type: "socket-state", state: "unavailable", reason: "generation-exhausted" });
      else
        report({ type: "socket-state", state: "closed" });
    };
  }

  function command(value) {
    if (!value || typeof value !== "object") return;
    switch (value.type) {
      case "request": request(value); break;
      case "connect": connect(); break;
      case "disconnect": disconnect(); break;
      case "active-files":
        if (validInterests(value.paths)) {
          interests = value.paths.slice();
          sendInterests();
        }
        break;
    }
  }

  function mount() {
    const node = document.getElementById("adrai-app");
    if (!node || !window.Elm || !window.Elm.Main) return;
    app = window.Elm.Main.init({ node, flags: { hasCredential: !!credential } });
    app.ports.toJs.subscribe(command);
  }

  if (document.readyState === "loading")
    document.addEventListener("DOMContentLoaded", mount, { once: true });
  else
    mount();
})();

