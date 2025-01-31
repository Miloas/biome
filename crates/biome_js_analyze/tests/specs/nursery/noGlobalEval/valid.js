/* should not generate diagnostics */
var foo = "foo";
Eval(foo);

setTimeout("foo");

setInterval("foo");

window.setTimeout("foo");

window.setInterval("foo");

window.noeval("foo");

global.noeval("foo");

globalThis.noeval("foo");

this.noeval("foo");

function foo() {
	"use strict";
	this.eval("foo");
}

("use strict");
this.eval("foo");

function foo() {
	this.eval("foo");
}

var obj = {
	foo: function () {
		this.eval("foo");
	},
};

var obj = {};
obj.foo = function () {
	this.eval("foo");
};

() => {
	this.eval("foo");
};

function f() {
	"use strict";
	() => {
		this.eval("foo");
	};
}

(function f() {
	"use strict";
	() => {
		this.eval("foo");
	};
});

class A {
	foo() {
		this.eval("foo");
	}
}

class A {
	static foo() {
		this.eval();
	}
}

class A {
	field = this.eval();
}

class A {
	field = () => this.eval();
}

class A {
	static {
		this.eval();
	}
}

Array.findLast(
	function (x) {
		return this.eval.includes(x);
	},
	{ eval: ["foo", "bar"] },
);

callbacks.findLastIndex(function (cb) {
	return cb(this.eval);
}, this);

["1+1"].flatMap(function (str) {
	return this.eval(str);
}, new Evaluator());

// TODO Fix to prevent errors for these cases
// function foo() {
// 	var eval = "foo";
// 	window[eval]("foo");
// }

// function foo() {
// 	var eval = "foo";
// 	global[eval]("foo");
// }

// function foo() {
// 	var eval = "foo";
// 	globalThis[eval]("foo");
// }

// function foo(eval) {
// 	eval("var a = 0");
// }
