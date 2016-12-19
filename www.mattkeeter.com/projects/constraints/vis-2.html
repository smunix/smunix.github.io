"use strict";
var Grid = {}
var Demo = {}

function populateGrid() {
    Grid.WIDTH = 400;
    Grid.HEIGHT = 400;
    Grid.MARGINS = {top: 20, right: 20, bottom: 20, left: 30};
    Grid.toScreenX = d3.scale.linear().range([Grid.MARGINS.left, Grid.WIDTH - Grid.MARGINS.right]).domain([0,3]);
    Grid.toScreenY = d3.scale.linear().range([Grid.HEIGHT - Grid.MARGINS.top, Grid.MARGINS.bottom]).domain([-2,2]);
    Grid.fromScreenX = d3.scale.linear().range([0, 3]).domain([Grid.MARGINS.left, Grid.WIDTH - Grid.MARGINS.right]);
    Grid.fromScreenY = d3.scale.linear().range([-2,2]).domain([Grid.HEIGHT - Grid.MARGINS.top, Grid.MARGINS.bottom]);

    Grid.xAxis = d3.svg.axis() // generate an axis
        .scale(Grid.toScreenX) // set the range of the axis
        .tickPadding(10) // height of the ticks
        .innerTickSize(-Grid.HEIGHT + Grid.MARGINS.top + Grid.MARGINS.bottom) // to make Grid lines
        .tickSubdivide(true); // display ticks between text labels
    Grid.yAxis = d3.svg.axis() // generate an axis
        .scale(Grid.toScreenY) // set the range of the axis
        .tickPadding(5) // width of the ticks
        .innerTickSize(-Grid.WIDTH + Grid.MARGINS.left + Grid.MARGINS.right) // to make Grid lines
        .orient("left") // have the text labels on the left hand side
        .tickSubdivide(true); // display ticks between text labels
};

function makeAxesFor(v) {
    v.append("svg:g") // add a container for the axis
         .attr("class", "x axis") // add some classes so we can style it
         .attr("transform", "translate(0," + (Grid.HEIGHT - Grid.MARGINS.bottom) + ")") // move it into position
         .call(Grid.xAxis); // finally, add the axis to the visualisation

    v.append("svg:g")
        .attr("class", "y axis")
        .attr("transform", "translate(" + (Grid.MARGINS.left) + ",0)")
        .call(Grid.yAxis);
};

function initSearch(search) {
    makeAxesFor(search);

    search.append("defs")
        .append("marker")
        .attr("id", "end")
        .attr("markerWidth", 6)
        .attr("viewBox", "0 -3 5 6")
        .attr("markerHeight", 6)
        .attr("orient", "auto")
        .append("path")
            .attr("d", "M0,-2L5,0L0,2")
            .attr("fill", "#666");

    search.append("path")
        .attr("id", "arrow")
        .attr("d", "M2,2 L2,11 L10,6 L2,2")
        .attr("stroke", "#666")
        .attr("stroke-width", 4)
        .attr("visibility", "hidden")
        .attr("marker-end", "url(#end)");

    search.append("circle")
        .attr("id", "cursor")
        .attr("r", 8)
        .attr("visibility", "hidden");

    search.on("mousemove", function() {
        var pos = d3.mouse(search.node());

        var x = Grid.fromScreenX(pos[0]);
        var y = Grid.fromScreenY(pos[1]);

        if (x < 0 || x > 3 || y < -2 || y > 2)
        {
            search.select("#cursor").attr("visibility", "hidden");
            search.select("#arrow").attr("visibility", "hidden");
            return;
        }
        var dx = -Haste.gradX(x,y);
        var dy = -Haste.gradY(x,y);
        var len = Math.sqrt(dx*dx + dy*dy);
        var dx = dx/len;
        var dy = dy/len;

        var v = Haste.val(x,y);
        var len = Math.min(0.5, v/2);
        if (v < 0.1) {
            search.select("#cursor").attr("fill","green");
        }
        else {
            search.select("#cursor").attr("fill","red");
        }

        search.select("#cursor")
            .attr("cx", Math.round(Grid.toScreenX(x)))
            .attr("cy", Math.round(Grid.toScreenY(y)))
            .attr("visibility", "visible");

        search.select("#arrow")
            .attr("d","M" + Math.round(Grid.toScreenX(x)) +
                      "," + Math.round(Grid.toScreenY(y)) +
                     " L" + Math.round(Grid.toScreenX(x+dx*len)) +
                      "," + Math.round(Grid.toScreenY(y+dy*len)))
            .attr("visibility", "visible");
    });

    search.on("mouseleave", function() {
        search.select("#cursor").attr("visibility", "hidden");
        search.select("#arrow").attr("visibility", "hidden");
    });

};

function initSolver(solver) {
    makeAxesFor(solver);

    solver.append("path")
        .attr("id", "trace")
        .attr("stroke", "#666")
        .attr("stroke-width", 1)
        .attr("fill", "none")
        .attr("visibility", "hidden");

    solver.on("mousemove", function()
    {
        var pos = d3.mouse(solver.node());

        var x = Grid.fromScreenX(pos[0]);
        var y = Grid.fromScreenY(pos[1]);

        if (x < 0 || x > 3 || y < -2 || y > 2)
        {
            solver.select("#trace").attr("visibility", "hidden");
            return;
        }

        var path = Haste.trace(x, y);

        var d = "";
        for (var i=0; i < path.length; ++i)
        {
            if (i == 0)
            {
                d += "M";
            }
            else
            {
                d += "L";
            }
            d += Math.round(Grid.toScreenX(path[i][0])) + "," +
                 Math.round(Grid.toScreenY(path[i][1]));
        }

        var last = path[path.length - 1];
        if (last[0] > 1.6)
        {
            solver.select("#trace").attr("stroke", "orange");
        }
        else
        {
            solver.select("#trace").attr("stroke", "blue");
        }

        solver.select("#trace")
            .attr("d",d)
            .attr("visibility", "visible");
    });

    solver.on("mouseleave", function() {
        solver.select("#trace").attr("visibility", "hidden");
    });
};

////////////////////////////////////////////////////////////////////////////////

function makeWindow(svg, width, height, xmin, xmax, ymin, ymax) {
    var obj = {};
    obj.svg = svg

    svg.attr("width", width);
    svg.attr("height", height);

    obj.toScreenX = d3.scale.linear().range([0, width]).domain([xmin, xmax]);
    obj.toScreenY = d3.scale.linear().range([height, 0]).domain([ymin, ymax]);
    obj.fromScreenX = d3.scale.linear().range([xmin, xmax]).domain([0, width]);
    obj.fromScreenY = d3.scale.linear().range([ymin, ymax]).domain([height, 0]);

    return obj;
};

////////////////////////////////////////////////////////////////////////////////

function handle(win, id, r, x, y)
{
    var pt = win.svg.append("circle")
        .attr("id", id)
        .attr("r", r)
        .attr("cx", win.toScreenX(x))
        .attr("cy", win.toScreenY(y))
        .attr("class", "handle");
}

function between(svg, a, b)
{
    return svg.append("line")
        .attr("class", "between")
        .attr("start", a)
        .attr("end", b);
}

function label(svg, id)
{
    return svg.append("text").text(id)
        .attr("fill", "black")
        .attr("class", "label")
        .attr("target", id);
}


////////////////////////////////////////////////////////////////////////////////

function updateBetween(svg, line)
{
    var a = svg.select("circle#" + line.attr("start"));
    var b = svg.select("circle#" + line.attr("end"));
    line.attr("x1", a.attr("cx"))
        .attr("x2", b.attr("cx"))
        .attr("y1", a.attr("cy"))
        .attr("y2", b.attr("cy"));
}

function updateLabel(svg, label)
{
    var pt = svg.select("circle#" + label.attr("target"));

    label.attr("x", parseFloat(pt.attr("cx")) + 10)
         .attr("y", parseFloat(pt.attr("cy")) - 5);
}

function runSolver(win, solver, active, x, y, pts)
{
    var values = [];
    for (var i=0; i < pts.length; ++i)
    {
        var circle = win.svg.select("#" + pts[i]);
        values.push([pts[i] + "x", win.fromScreenX(circle.attr("cx"))]);
        values.push([pts[i] + "y", win.fromScreenY(circle.attr("cy"))]);
    }

    // Run solver
    var solution = solver(active, x, y, values);

    // Apply solution to handles
    for (var i=0; i < solution.length; ++i)
    {
        var circle = win.svg.select("#" + solution[i][0].slice(0,-1));
        var axis = solution[i][0].slice(-1);
        if (axis == "x") {
            circle.attr("cx", win.toScreenX(solution[i][1]));
        }
        else {
            circle.attr("cy", win.toScreenY(solution[i][1]));
        }
    }

    // Move lines to agree with new handle positions
    win.svg.selectAll("line.between").each(
        function(d, i) {
            updateBetween(win.svg, d3.select(this));
        });

    // Move labels to be in agreement with handle positions
    win.svg.selectAll("text.label").each(
        function(d, i) {
            updateLabel(win.svg, d3.select(this));
        });
}

function onDrag(win, solver, active, pts)
{
    var pos = d3.mouse(win.svg.node());
    runSolver(win, solver, active.attr("id"),
              win.fromScreenX(pos[0]),
              win.fromScreenY(pos[1]), pts);
}

function setDragCallback(win, solver, pts)
{
    // Callback closure
    var callback = function() {onDrag(win, solver, d3.select(this), pts)}
    var drag = d3.behavior.drag().on("drag", callback);
    win.svg.selectAll(".handle").call(drag);

    // Run the solver once to get points in valid positions
    runSolver(win, solver, "", 0, 0, pts);
}

////////////////////////////////////////////////////////////////////////////////

function initPerp(svg)
{
    var win = makeWindow(svg, 180, 180, -2.3, 2.3, -2.3, 2.3);

    between(svg, "a", "b")
            .attr("stroke", "#d33682")
            .attr("stroke-width", 4);
    between(svg, "c", "d")
            .attr("stroke", "#6c71c4")
            .attr("stroke-width", 4);

    handle(win, "a", 6, -1, -1);
    handle(win, "b", 6, 1, 1);
    handle(win, "c", 6, 0, 0);
    handle(win, "d", 6, -1, 1);

    setDragCallback(win, Haste.perp, ["a", "b", "c", "d"])
}

function initArm(svg)
{
    var win = makeWindow(svg, 180, 180, -2.3, 2.3, -2.3, 2.3);

    svg.append("line")
        .attr("stroke", "#93a1a1")
        .attr("stroke-weight", 2)
        .attr("x1", win.toScreenX(-1))
        .attr("x2", win.toScreenX(1))
        .attr("y1", win.toScreenY(0))
        .attr("y2", win.toScreenY(0));
    svg.append("line")
        .attr("stroke", "#93a1a1")
        .attr("stroke-weight", 2)
        .attr("x1", win.toScreenX(0))
        .attr("x2", win.toScreenX(0))
        .attr("y1", win.toScreenY(-1))
        .attr("y2", win.toScreenY(1));

    between(svg, "a", "b")
            .attr("stroke", "#268bd2")
            .attr("stroke-width", 4);
    between(svg, "b", "c")
            .attr("stroke", "#859900")
            .attr("stroke-width", 4);

    handle(win, "a", 6, 0, 0);
    handle(win, "b", 6, 1, 0);
    handle(win, "c", 6, 1.71, 0.71);

    setDragCallback(win, Haste.arm, ["a", "b", "c"])
}

function initTangent(svg)
{
    var win = makeWindow(svg, 180, 180, -1.5, 1.5, -1.5, 1.5);

    svg.append("circle")
        .attr("r", 60)
        .attr("fill", "none")
        .attr("stroke-weight", 2)
        .attr("stroke", "#93a1a1")
        .attr("cx", win.toScreenX(0))
        .attr("cy", win.toScreenY(0));

    between(svg, "b", "c")
            .attr("stroke", "#cb4b16")
            .attr("stroke-width", 4);

    handle(win, "b", 6, 0, 1);
    handle(win, "c", 6, 1, 1);

    setDragCallback(win, Haste.tangent, ["b", "c"])
}

function initDemo(svg)
{
    var win = makeWindow(svg, 400, 166, -4, 4, -1.666, 1.666);

    svg.append("circle")
        .attr("r", 50)
        .attr("fill", "none")
        .attr("stroke-weight", 2)
        .attr("stroke", "#93a1a1")
        .attr("cx", win.toScreenX(0))
        .attr("cy", win.toScreenY(0));
    svg.append("line")
        .attr("stroke", "#93a1a1")
        .attr("stroke-weight", 2)
        .attr("x1", win.toScreenX(-3))
        .attr("x2", win.toScreenX(3))
        .attr("y1", win.toScreenY(-0.2))
        .attr("y2", win.toScreenY(-0.2));
    svg.append("line")
        .attr("stroke", "#93a1a1")
        .attr("stroke-weight", 2)
        .attr("x1", win.toScreenX(-3))
        .attr("x2", win.toScreenX(3))
        .attr("y1", win.toScreenY(0.2))
        .attr("y2", win.toScreenY(0.2));

    between(svg, "a", "b")
            .attr("stroke", "#268bd2")
            .attr("stroke-width", 6);
    between(svg, "b", "c")
            .attr("stroke", "#859900")
            .attr("stroke-width", 6);

    handle(win, "a", 6, 0, 1);
    handle(win, "b", 6, 1, 0);
    handle(win, "c", 6, 2, 0);

    label(svg, "a");
    label(svg, "b");
    label(svg, "c");

    setDragCallback(win, Haste.demo, ["a", "b", "c"])
}

function init () {
    populateGrid();

    initSearch(d3.select("#search"));
    initSolver(d3.select("#solver"));

    initArm(d3.select("#arm"));
    initTangent(d3.select("#tangent"));
    initPerp(d3.select("#perp"));
    initDemo(d3.select("#demo"));
}

////////////////////////////////////////////////////////////////////////////////

// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof F) {
            f = E(B(f));
        }
        if(f instanceof PAP) {
            // f is a partial application
            if(args.length == f.arity) {
                // Saturated application
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                // Application is still unsaturated
                return new PAP(f.f, f.args.concat(args));
            } else {
                // Application is oversaturated; 
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else if(f instanceof Function) {
            if(args.length == f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,str.charCodeAt(i),new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1]));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(I_getBits(i,0)) + popCnt(I_getBits(i,1));
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return [0,1,0,0,0];
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return [0,1,0,0,0];
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1]));
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, ks[i], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}
window['newByteArr'] = newByteArr;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as[0] === 1; as = as[2]) {
        arr.push(as[1]);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem],new T(function(){return __arr2lst(elem+1,arr);})]
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs[0] === 1; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0=0,_1=function(_){return _0;},_2=[0],_3=function(_4,_){var _5=E(_4);if(!_5[0]){return _2;}else{var _6=B(A(_5[1],[_])),_7=B(_3(_5[2],_));return [1,_6,_7];}},_8=function(_9,_){var _a=__arr2lst(0,_9);return new F(function(){return _3(_a,_);});},_b=function(_c,_){return new F(function(){return _8(E(_c),_);});},_d=function(_e){return E(_e);},_f=[0,_d,_b],_g=function(_h,_){return new T(function(){var _i=String(E(_h));return fromJSStr(_i);});},_j=function(_k){return new F(function(){return Number(E(_k));});},_l=function(_m){return new F(function(){return _j(_m);});},_n=function(_o,_){return new T(function(){return B(_l(_o));});},_p=[0,_n,_g],_q=function(_r){return E(E(_r)[2]);},_s=function(_t,_u,_){var _v=__arr2lst(0,_u),_w=new T(function(){return B(_q(_t));}),_x=function(_y,_){var _z=E(_y);if(!_z[0]){return _2;}else{var _A=B(A(_w,[_z[1],_])),_B=B(_x(_z[2],_));return [1,_A,_B];}};return new F(function(){return _x(_v,_);});},_C=function(_D,_){return new F(function(){return _s(_p,E(_D),_);});},_E=[0,_g,_C],_F=function(_G,_){var _H=E(_G);if(!_H[0]){return _2;}else{var _I=B(_F(_H[2],_));return [1,_H[1],_I];}},_J=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_K=new T(function(){return B(unCStr("base"));}),_L=new T(function(){return B(unCStr("IOException"));}),_M=new T(function(){var _N=hs_wordToWord64(4053623282),_O=hs_wordToWord64(3693590983);return [0,_N,_O,[0,_N,_O,_K,_J,_L],_2,_2];}),_P=function(_Q){return E(_M);},_R=function(_S){return E(E(_S)[1]);},_T=function(_U,_V,_W){var _X=B(A(_U,[_])),_Y=B(A(_V,[_])),_Z=hs_eqWord64(_X[1],_Y[1]);if(!_Z){return [0];}else{var _10=hs_eqWord64(_X[2],_Y[2]);return (!_10)?[0]:[1,_W];}},_11=function(_12){var _13=E(_12);return new F(function(){return _T(B(_R(_13[1])),_P,_13[2]);});},_14=new T(function(){return B(unCStr(": "));}),_15=new T(function(){return B(unCStr(")"));}),_16=new T(function(){return B(unCStr(" ("));}),_17=function(_18,_19){var _1a=E(_18);return (_1a[0]==0)?E(_19):[1,_1a[1],new T(function(){return B(_17(_1a[2],_19));})];},_1b=new T(function(){return B(unCStr("interrupted"));}),_1c=new T(function(){return B(unCStr("system error"));}),_1d=new T(function(){return B(unCStr("unsatisified constraints"));}),_1e=new T(function(){return B(unCStr("user error"));}),_1f=new T(function(){return B(unCStr("permission denied"));}),_1g=new T(function(){return B(unCStr("illegal operation"));}),_1h=new T(function(){return B(unCStr("end of file"));}),_1i=new T(function(){return B(unCStr("resource exhausted"));}),_1j=new T(function(){return B(unCStr("resource busy"));}),_1k=new T(function(){return B(unCStr("does not exist"));}),_1l=new T(function(){return B(unCStr("already exists"));}),_1m=new T(function(){return B(unCStr("resource vanished"));}),_1n=new T(function(){return B(unCStr("timeout"));}),_1o=new T(function(){return B(unCStr("unsupported operation"));}),_1p=new T(function(){return B(unCStr("hardware fault"));}),_1q=new T(function(){return B(unCStr("inappropriate type"));}),_1r=new T(function(){return B(unCStr("invalid argument"));}),_1s=new T(function(){return B(unCStr("failed"));}),_1t=new T(function(){return B(unCStr("protocol error"));}),_1u=function(_1v,_1w){switch(E(_1v)){case 0:return new F(function(){return _17(_1l,_1w);});break;case 1:return new F(function(){return _17(_1k,_1w);});break;case 2:return new F(function(){return _17(_1j,_1w);});break;case 3:return new F(function(){return _17(_1i,_1w);});break;case 4:return new F(function(){return _17(_1h,_1w);});break;case 5:return new F(function(){return _17(_1g,_1w);});break;case 6:return new F(function(){return _17(_1f,_1w);});break;case 7:return new F(function(){return _17(_1e,_1w);});break;case 8:return new F(function(){return _17(_1d,_1w);});break;case 9:return new F(function(){return _17(_1c,_1w);});break;case 10:return new F(function(){return _17(_1t,_1w);});break;case 11:return new F(function(){return _17(_1s,_1w);});break;case 12:return new F(function(){return _17(_1r,_1w);});break;case 13:return new F(function(){return _17(_1q,_1w);});break;case 14:return new F(function(){return _17(_1p,_1w);});break;case 15:return new F(function(){return _17(_1o,_1w);});break;case 16:return new F(function(){return _17(_1n,_1w);});break;case 17:return new F(function(){return _17(_1m,_1w);});break;default:return new F(function(){return _17(_1b,_1w);});}},_1x=new T(function(){return B(unCStr("}"));}),_1y=new T(function(){return B(unCStr("{handle: "));}),_1z=function(_1A,_1B,_1C,_1D,_1E,_1F){var _1G=new T(function(){var _1H=new T(function(){var _1I=new T(function(){var _1J=E(_1D);if(!_1J[0]){return E(_1F);}else{var _1K=new T(function(){return B(_17(_1J,new T(function(){return B(_17(_15,_1F));},1)));},1);return B(_17(_16,_1K));}},1);return B(_1u(_1B,_1I));}),_1L=E(_1C);if(!_1L[0]){return E(_1H);}else{return B(_17(_1L,new T(function(){return B(_17(_14,_1H));},1)));}}),_1M=E(_1E);if(!_1M[0]){var _1N=E(_1A);if(!_1N[0]){return E(_1G);}else{var _1O=E(_1N[1]);if(!_1O[0]){var _1P=new T(function(){var _1Q=new T(function(){return B(_17(_1x,new T(function(){return B(_17(_14,_1G));},1)));},1);return B(_17(_1O[1],_1Q));},1);return new F(function(){return _17(_1y,_1P);});}else{var _1R=new T(function(){var _1S=new T(function(){return B(_17(_1x,new T(function(){return B(_17(_14,_1G));},1)));},1);return B(_17(_1O[1],_1S));},1);return new F(function(){return _17(_1y,_1R);});}}}else{return new F(function(){return _17(_1M[1],new T(function(){return B(_17(_14,_1G));},1));});}},_1T=function(_1U){var _1V=E(_1U);return new F(function(){return _1z(_1V[1],_1V[2],_1V[3],_1V[4],_1V[6],_2);});},_1W=function(_1X,_1Y,_1Z){var _20=E(_1Y);return new F(function(){return _1z(_20[1],_20[2],_20[3],_20[4],_20[6],_1Z);});},_21=function(_22,_23){var _24=E(_22);return new F(function(){return _1z(_24[1],_24[2],_24[3],_24[4],_24[6],_23);});},_25=44,_26=93,_27=91,_28=function(_29,_2a,_2b){var _2c=E(_2a);if(!_2c[0]){return new F(function(){return unAppCStr("[]",_2b);});}else{var _2d=new T(function(){var _2e=new T(function(){var _2f=function(_2g){var _2h=E(_2g);if(!_2h[0]){return [1,_26,_2b];}else{var _2i=new T(function(){return B(A(_29,[_2h[1],new T(function(){return B(_2f(_2h[2]));})]));});return [1,_25,_2i];}};return B(_2f(_2c[2]));});return B(A(_29,[_2c[1],_2e]));});return [1,_27,_2d];}},_2j=function(_2k,_2l){return new F(function(){return _28(_21,_2k,_2l);});},_2m=[0,_1W,_1T,_2j],_2n=new T(function(){return [0,_P,_2m,_2o,_11,_1T];}),_2o=function(_2p){return [0,_2n,_2p];},_2q=[0],_2r=7,_2s=new T(function(){return B(unCStr("Pattern match failure in do expression at src/Haste/Prim/Any.hs:272:5-9"));}),_2t=[0,_2q,_2r,_2,_2s,_2q,_2q],_2u=new T(function(){return B(_2o(_2t));}),_2v=function(_){return new F(function(){return die(_2u);});},_2w=function(_2x){return E(E(_2x)[1]);},_2y=function(_2z,_2A,_2B,_){var _2C=__arr2lst(0,_2B),_2D=B(_F(_2C,_)),_2E=E(_2D);if(!_2E[0]){return new F(function(){return _2v(_);});}else{var _2F=E(_2E[2]);if(!_2F[0]){return new F(function(){return _2v(_);});}else{if(!E(_2F[2])[0]){var _2G=B(A(_2w,[_2z,_2E[1],_])),_2H=B(A(_2w,[_2A,_2F[1],_]));return [0,_2G,_2H];}else{return new F(function(){return _2v(_);});}}}},_2I=function(_2J,_2K,_2L,_){var _2M=__arr2lst(0,_2L),_2N=function(_2O,_){var _2P=E(_2O);if(!_2P[0]){return _2;}else{var _2Q=B(_2y(_2J,_2K,E(_2P[1]),_)),_2R=B(_2N(_2P[2],_));return [1,_2Q,_2R];}};return new F(function(){return _2N(_2M,_);});},_2S=function(_2T,_2U){while(1){var _2V=E(_2T);if(!_2V[0]){return (E(_2U)[0]==0)?true:false;}else{var _2W=E(_2U);if(!_2W[0]){return false;}else{if(E(_2V[1])!=E(_2W[1])){return false;}else{_2T=_2V[2];_2U=_2W[2];continue;}}}}},_2X=function(_2Y,_2Z){return (!B(_2S(_2Y,_2Z)))?true:false;},_30=[0,_2S,_2X],_31=function(_32,_33){while(1){var _34=E(_32);if(!_34[0]){return (E(_33)[0]==0)?1:0;}else{var _35=E(_33);if(!_35[0]){return 2;}else{var _36=E(_34[1]),_37=E(_35[1]);if(_36!=_37){return (_36>_37)?2:0;}else{_32=_34[2];_33=_35[2];continue;}}}}},_38=function(_39,_3a){return (B(_31(_39,_3a))==0)?true:false;},_3b=function(_3c,_3d){return (B(_31(_3c,_3d))==2)?false:true;},_3e=function(_3f,_3g){return (B(_31(_3f,_3g))==2)?true:false;},_3h=function(_3i,_3j){return (B(_31(_3i,_3j))==0)?false:true;},_3k=function(_3l,_3m){return (B(_31(_3l,_3m))==2)?E(_3l):E(_3m);},_3n=function(_3o,_3p){return (B(_31(_3o,_3p))==2)?E(_3p):E(_3o);},_3q=[0,_30,_31,_38,_3b,_3e,_3h,_3k,_3n],_3r=[1],_3s=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_3t=function(_3u){return new F(function(){return err(_3s);});},_3v=new T(function(){return B(_3t(_));}),_3w=function(_3x,_3y,_3z,_3A){var _3B=E(_3z);if(!_3B[0]){var _3C=_3B[1],_3D=E(_3A);if(!_3D[0]){var _3E=_3D[1],_3F=_3D[2],_3G=_3D[3];if(_3E<=(imul(3,_3C)|0)){return [0,(1+_3C|0)+_3E|0,E(_3x),_3y,E(_3B),E(_3D)];}else{var _3H=E(_3D[4]);if(!_3H[0]){var _3I=_3H[1],_3J=_3H[2],_3K=_3H[3],_3L=_3H[4],_3M=E(_3D[5]);if(!_3M[0]){var _3N=_3M[1];if(_3I>=(imul(2,_3N)|0)){var _3O=function(_3P){var _3Q=E(_3x),_3R=E(_3H[5]);return (_3R[0]==0)?[0,(1+_3C|0)+_3E|0,E(_3J),_3K,[0,(1+_3C|0)+_3P|0,E(_3Q),_3y,E(_3B),E(_3L)],[0,(1+_3N|0)+_3R[1]|0,E(_3F),_3G,E(_3R),E(_3M)]]:[0,(1+_3C|0)+_3E|0,E(_3J),_3K,[0,(1+_3C|0)+_3P|0,E(_3Q),_3y,E(_3B),E(_3L)],[0,1+_3N|0,E(_3F),_3G,E(_3r),E(_3M)]];},_3S=E(_3L);if(!_3S[0]){return new F(function(){return _3O(_3S[1]);});}else{return new F(function(){return _3O(0);});}}else{return [0,(1+_3C|0)+_3E|0,E(_3F),_3G,[0,(1+_3C|0)+_3I|0,E(_3x),_3y,E(_3B),E(_3H)],E(_3M)];}}else{return E(_3v);}}else{return E(_3v);}}}else{return [0,1+_3C|0,E(_3x),_3y,E(_3B),E(_3r)];}}else{var _3T=E(_3A);if(!_3T[0]){var _3U=_3T[1],_3V=_3T[2],_3W=_3T[3],_3X=_3T[5],_3Y=E(_3T[4]);if(!_3Y[0]){var _3Z=_3Y[1],_40=_3Y[2],_41=_3Y[3],_42=_3Y[4],_43=E(_3X);if(!_43[0]){var _44=_43[1];if(_3Z>=(imul(2,_44)|0)){var _45=function(_46){var _47=E(_3x),_48=E(_3Y[5]);return (_48[0]==0)?[0,1+_3U|0,E(_40),_41,[0,1+_46|0,E(_47),_3y,E(_3r),E(_42)],[0,(1+_44|0)+_48[1]|0,E(_3V),_3W,E(_48),E(_43)]]:[0,1+_3U|0,E(_40),_41,[0,1+_46|0,E(_47),_3y,E(_3r),E(_42)],[0,1+_44|0,E(_3V),_3W,E(_3r),E(_43)]];},_49=E(_42);if(!_49[0]){return new F(function(){return _45(_49[1]);});}else{return new F(function(){return _45(0);});}}else{return [0,1+_3U|0,E(_3V),_3W,[0,1+_3Z|0,E(_3x),_3y,E(_3r),E(_3Y)],E(_43)];}}else{return [0,3,E(_40),_41,[0,1,E(_3x),_3y,E(_3r),E(_3r)],[0,1,E(_3V),_3W,E(_3r),E(_3r)]];}}else{var _4a=E(_3X);return (_4a[0]==0)?[0,3,E(_3V),_3W,[0,1,E(_3x),_3y,E(_3r),E(_3r)],E(_4a)]:[0,2,E(_3x),_3y,E(_3r),E(_3T)];}}else{return [0,1,E(_3x),_3y,E(_3r),E(_3r)];}}},_4b=function(_4c,_4d){return [0,1,E(_4c),_4d,E(_3r),E(_3r)];},_4e=function(_4f,_4g,_4h){var _4i=E(_4h);if(!_4i[0]){return new F(function(){return _3w(_4i[2],_4i[3],_4i[4],B(_4e(_4f,_4g,_4i[5])));});}else{return new F(function(){return _4b(_4f,_4g);});}},_4j=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_4k=function(_4l){return new F(function(){return err(_4j);});},_4m=new T(function(){return B(_4k(_));}),_4n=function(_4o,_4p,_4q,_4r){var _4s=E(_4r);if(!_4s[0]){var _4t=_4s[1],_4u=E(_4q);if(!_4u[0]){var _4v=_4u[1],_4w=_4u[2],_4x=_4u[3];if(_4v<=(imul(3,_4t)|0)){return [0,(1+_4v|0)+_4t|0,E(_4o),_4p,E(_4u),E(_4s)];}else{var _4y=E(_4u[4]);if(!_4y[0]){var _4z=_4y[1],_4A=E(_4u[5]);if(!_4A[0]){var _4B=_4A[1],_4C=_4A[2],_4D=_4A[3],_4E=_4A[4];if(_4B>=(imul(2,_4z)|0)){var _4F=function(_4G){var _4H=E(_4A[5]);return (_4H[0]==0)?[0,(1+_4v|0)+_4t|0,E(_4C),_4D,[0,(1+_4z|0)+_4G|0,E(_4w),_4x,E(_4y),E(_4E)],[0,(1+_4t|0)+_4H[1]|0,E(_4o),_4p,E(_4H),E(_4s)]]:[0,(1+_4v|0)+_4t|0,E(_4C),_4D,[0,(1+_4z|0)+_4G|0,E(_4w),_4x,E(_4y),E(_4E)],[0,1+_4t|0,E(_4o),_4p,E(_3r),E(_4s)]];},_4I=E(_4E);if(!_4I[0]){return new F(function(){return _4F(_4I[1]);});}else{return new F(function(){return _4F(0);});}}else{return [0,(1+_4v|0)+_4t|0,E(_4w),_4x,E(_4y),[0,(1+_4t|0)+_4B|0,E(_4o),_4p,E(_4A),E(_4s)]];}}else{return E(_4m);}}else{return E(_4m);}}}else{return [0,1+_4t|0,E(_4o),_4p,E(_3r),E(_4s)];}}else{var _4J=E(_4q);if(!_4J[0]){var _4K=_4J[1],_4L=_4J[2],_4M=_4J[3],_4N=_4J[5],_4O=E(_4J[4]);if(!_4O[0]){var _4P=_4O[1],_4Q=E(_4N);if(!_4Q[0]){var _4R=_4Q[1],_4S=_4Q[2],_4T=_4Q[3],_4U=_4Q[4];if(_4R>=(imul(2,_4P)|0)){var _4V=function(_4W){var _4X=E(_4Q[5]);return (_4X[0]==0)?[0,1+_4K|0,E(_4S),_4T,[0,(1+_4P|0)+_4W|0,E(_4L),_4M,E(_4O),E(_4U)],[0,1+_4X[1]|0,E(_4o),_4p,E(_4X),E(_3r)]]:[0,1+_4K|0,E(_4S),_4T,[0,(1+_4P|0)+_4W|0,E(_4L),_4M,E(_4O),E(_4U)],[0,1,E(_4o),_4p,E(_3r),E(_3r)]];},_4Y=E(_4U);if(!_4Y[0]){return new F(function(){return _4V(_4Y[1]);});}else{return new F(function(){return _4V(0);});}}else{return [0,1+_4K|0,E(_4L),_4M,E(_4O),[0,1+_4R|0,E(_4o),_4p,E(_4Q),E(_3r)]];}}else{return [0,3,E(_4L),_4M,E(_4O),[0,1,E(_4o),_4p,E(_3r),E(_3r)]];}}else{var _4Z=E(_4N);return (_4Z[0]==0)?[0,3,E(_4Z[2]),_4Z[3],[0,1,E(_4L),_4M,E(_3r),E(_3r)],[0,1,E(_4o),_4p,E(_3r),E(_3r)]]:[0,2,E(_4o),_4p,E(_4J),E(_3r)];}}else{return [0,1,E(_4o),_4p,E(_3r),E(_3r)];}}},_50=function(_51,_52,_53){var _54=E(_53);if(!_54[0]){return new F(function(){return _4n(_54[2],_54[3],B(_50(_51,_52,_54[4])),_54[5]);});}else{return new F(function(){return _4b(_51,_52);});}},_55=function(_56,_57,_58,_59,_5a,_5b,_5c){return new F(function(){return _4n(_59,_5a,B(_50(_56,_57,_5b)),_5c);});},_5d=function(_5e,_5f,_5g,_5h,_5i,_5j,_5k,_5l){var _5m=E(_5g);if(!_5m[0]){var _5n=_5m[1],_5o=_5m[2],_5p=_5m[3],_5q=_5m[4],_5r=_5m[5];if((imul(3,_5n)|0)>=_5h){if((imul(3,_5h)|0)>=_5n){return [0,(_5n+_5h|0)+1|0,E(_5e),_5f,E(_5m),[0,_5h,E(_5i),_5j,E(_5k),E(_5l)]];}else{return new F(function(){return _3w(_5o,_5p,_5q,B(_5d(_5e,_5f,_5r,_5h,_5i,_5j,_5k,_5l)));});}}else{return new F(function(){return _4n(_5i,_5j,B(_5s(_5e,_5f,_5n,_5o,_5p,_5q,_5r,_5k)),_5l);});}}else{return new F(function(){return _55(_5e,_5f,_5h,_5i,_5j,_5k,_5l);});}},_5s=function(_5t,_5u,_5v,_5w,_5x,_5y,_5z,_5A){var _5B=E(_5A);if(!_5B[0]){var _5C=_5B[1],_5D=_5B[2],_5E=_5B[3],_5F=_5B[4],_5G=_5B[5];if((imul(3,_5v)|0)>=_5C){if((imul(3,_5C)|0)>=_5v){return [0,(_5v+_5C|0)+1|0,E(_5t),_5u,[0,_5v,E(_5w),_5x,E(_5y),E(_5z)],E(_5B)];}else{return new F(function(){return _3w(_5w,_5x,_5y,B(_5d(_5t,_5u,_5z,_5C,_5D,_5E,_5F,_5G)));});}}else{return new F(function(){return _4n(_5D,_5E,B(_5s(_5t,_5u,_5v,_5w,_5x,_5y,_5z,_5F)),_5G);});}}else{return new F(function(){return _4e(_5t,_5u,[0,_5v,E(_5w),_5x,E(_5y),E(_5z)]);});}},_5H=function(_5I,_5J,_5K,_5L){var _5M=E(_5K);if(!_5M[0]){var _5N=_5M[1],_5O=_5M[2],_5P=_5M[3],_5Q=_5M[4],_5R=_5M[5],_5S=E(_5L);if(!_5S[0]){var _5T=_5S[1],_5U=_5S[2],_5V=_5S[3],_5W=_5S[4],_5X=_5S[5];if((imul(3,_5N)|0)>=_5T){if((imul(3,_5T)|0)>=_5N){return [0,(_5N+_5T|0)+1|0,E(_5I),_5J,E(_5M),E(_5S)];}else{return new F(function(){return _3w(_5O,_5P,_5Q,B(_5d(_5I,_5J,_5R,_5T,_5U,_5V,_5W,_5X)));});}}else{return new F(function(){return _4n(_5U,_5V,B(_5s(_5I,_5J,_5N,_5O,_5P,_5Q,_5R,_5W)),_5X);});}}else{return new F(function(){return _4e(_5I,_5J,_5M);});}}else{return new F(function(){return _50(_5I,_5J,_5L);});}},_5Y=function(_5Z,_60,_61,_62){var _63=E(_5Z);if(_63==1){var _64=E(_62);return (_64[0]==0)?[0,new T(function(){return [0,1,E(_60),_61,E(_3r),E(_3r)];}),_2,_2]:(B(_31(_60,E(_64[1])[1]))==0)?[0,new T(function(){return [0,1,E(_60),_61,E(_3r),E(_3r)];}),_64,_2]:[0,new T(function(){return [0,1,E(_60),_61,E(_3r),E(_3r)];}),_2,_64];}else{var _65=B(_5Y(_63>>1,_60,_61,_62)),_66=_65[1],_67=_65[3],_68=E(_65[2]);if(!_68[0]){return [0,_66,_2,_67];}else{var _69=E(_68[1]),_6a=_69[1],_6b=_69[2],_6c=E(_68[2]);if(!_6c[0]){return [0,new T(function(){return B(_4e(_6a,_6b,_66));}),_2,_67];}else{var _6d=E(_6c[1]),_6e=_6d[1];if(!B(_31(_6a,_6e))){var _6f=B(_5Y(_63>>1,_6e,_6d[2],_6c[2]));return [0,new T(function(){return B(_5H(_6a,_6b,_66,_6f[1]));}),_6f[2],_6f[3]];}else{return [0,_66,_2,_68];}}}}},_6g=function(_6h,_6i,_6j){var _6k=E(_6h),_6l=E(_6j);if(!_6l[0]){var _6m=_6l[2],_6n=_6l[3],_6o=_6l[4],_6p=_6l[5];switch(B(_31(_6k,_6m))){case 0:return new F(function(){return _4n(_6m,_6n,B(_6g(_6k,_6i,_6o)),_6p);});break;case 1:return [0,_6l[1],E(_6k),_6i,E(_6o),E(_6p)];default:return new F(function(){return _3w(_6m,_6n,_6o,B(_6g(_6k,_6i,_6p)));});}}else{return [0,1,E(_6k),_6i,E(_3r),E(_3r)];}},_6q=function(_6r,_6s){while(1){var _6t=E(_6s);if(!_6t[0]){return E(_6r);}else{var _6u=E(_6t[1]),_6v=B(_6g(_6u[1],_6u[2],_6r));_6r=_6v;_6s=_6t[2];continue;}}},_6w=function(_6x,_6y,_6z,_6A){return new F(function(){return _6q(B(_6g(_6y,_6z,_6x)),_6A);});},_6B=function(_6C,_6D,_6E){var _6F=E(_6D);return new F(function(){return _6q(B(_6g(_6F[1],_6F[2],_6C)),_6E);});},_6G=function(_6H,_6I,_6J){while(1){var _6K=E(_6J);if(!_6K[0]){return E(_6I);}else{var _6L=E(_6K[1]),_6M=_6L[1],_6N=_6L[2],_6O=E(_6K[2]);if(!_6O[0]){return new F(function(){return _4e(_6M,_6N,_6I);});}else{var _6P=E(_6O[1]),_6Q=_6P[1];if(!B(_31(_6M,_6Q))){var _6R=B(_5Y(_6H,_6Q,_6P[2],_6O[2])),_6S=_6R[1],_6T=E(_6R[3]);if(!_6T[0]){var _6U=_6H<<1,_6V=B(_5H(_6M,_6N,_6I,_6S));_6H=_6U;_6I=_6V;_6J=_6R[2];continue;}else{return new F(function(){return _6B(B(_5H(_6M,_6N,_6I,_6S)),_6T[1],_6T[2]);});}}else{return new F(function(){return _6w(_6I,_6M,_6N,_6O);});}}}}},_6W=function(_6X,_6Y,_6Z,_70,_71){var _72=E(_71);if(!_72[0]){return new F(function(){return _4e(_6Z,_70,_6Y);});}else{var _73=E(_72[1]),_74=_73[1];if(!B(_31(_6Z,_74))){var _75=B(_5Y(_6X,_74,_73[2],_72[2])),_76=_75[1],_77=E(_75[3]);if(!_77[0]){return new F(function(){return _6G(_6X<<1,B(_5H(_6Z,_70,_6Y,_76)),_75[2]);});}else{return new F(function(){return _6B(B(_5H(_6Z,_70,_6Y,_76)),_77[1],_77[2]);});}}else{return new F(function(){return _6w(_6Y,_6Z,_70,_72);});}}},_78=function(_79){var _7a=E(_79);if(!_7a[0]){return [1];}else{var _7b=E(_7a[1]),_7c=_7b[1],_7d=_7b[2],_7e=E(_7a[2]);if(!_7e[0]){return [0,1,E(_7c),_7d,E(_3r),E(_3r)];}else{var _7f=_7e[2],_7g=E(_7e[1]),_7h=_7g[1],_7i=_7g[2];if(!B(_31(_7c,_7h))){return new F(function(){return _6W(1,[0,1,E(_7c),_7d,E(_3r),E(_3r)],_7h,_7i,_7f);});}else{return new F(function(){return _6w([0,1,E(_7c),_7d,E(_3r),E(_3r)],_7h,_7i,_7f);});}}}},_7j=function(_7k,_7l){return E(_7k)+E(_7l);},_7m=function(_7n,_7o,_7p){return new F(function(){return _7j(_7o,_7p);});},_7q=function(_7r,_7s){var _7t=E(_7s);return (_7t[0]==0)?[0,_7t[1],E(_7t[2]),B(A(_7r,[_7t[3]])),E(B(_7q(_7r,_7t[4]))),E(B(_7q(_7r,_7t[5])))]:[1];},_7u=function(_7v,_7w){return E(_7v)*E(_7w);},_7x=function(_7y){return E(E(_7y)[2]);},_7z=function(_7A,_7B,_7C){var _7D=_7B,_7E=_7C;while(1){var _7F=E(_7D),_7G=E(_7E);if(!_7G[0]){switch(B(A(_7x,[_7A,_7F,_7G[2]]))){case 0:_7D=_7F;_7E=_7G[4];continue;case 1:return [1,_7G[3]];default:_7D=_7F;_7E=_7G[5];continue;}}else{return [0];}}},_7H=function(_7I,_7J,_7K){while(1){var _7L=E(_7K);if(!_7L[0]){var _7M=_7L[5];switch(B(A(_7x,[_7I,_7J,_7L[2]]))){case 0:return [0,B(_7z(_7I,_7J,_7L[4])),_7L];case 1:return [0,[1,_7L[3]],_7M];default:_7K=_7M;continue;}}else{return [0,_2q,_3r];}}},_7N=function(_7O){return E(E(_7O)[3]);},_7P=function(_7Q){return E(E(_7Q)[6]);},_7R=function(_7S,_7T,_7U){while(1){var _7V=E(_7U);if(!_7V[0]){if(!B(A(_7P,[_7S,_7V[2],_7T]))){return E(_7V);}else{_7U=_7V[4];continue;}}else{return [1];}}},_7W=function(_7X,_7Y,_7Z,_80){while(1){var _81=E(_80);if(!_81[0]){var _82=_81[2],_83=_81[4],_84=_81[5];switch(B(A(_7x,[_7X,_7Y,_82]))){case 0:if(!B(A(_7N,[_7X,_82,_7Z]))){_80=_83;continue;}else{return [0,B(_7z(_7X,_7Y,_83)),_81];}break;case 1:return [0,[1,_81[3]],B(_7R(_7X,_7Z,_84))];default:_80=_84;continue;}}else{return [0,_2q,_3r];}}},_85=function(_86,_87,_88,_89){var _8a=E(_88);if(!_8a[0]){return new F(function(){return _7H(_86,_87,_89);});}else{return new F(function(){return _7W(_86,_87,_8a[1],_89);});}},_8b=[0],_8c=function(_8d,_8e,_8f){var _8g=E(_8e);if(!_8g[0]){return E(_8f);}else{var _8h=function(_8i,_8j){while(1){var _8k=E(_8j);if(!_8k[0]){var _8l=_8k[2],_8m=_8k[5];switch(B(A(_7x,[_8d,_8i,_8l]))){case 0:return new F(function(){return _5H(_8l,_8k[3],B(_8h(_8i,_8k[4])),_8m);});break;case 1:return E(_8m);default:_8j=_8m;continue;}}else{return [1];}}};return new F(function(){return _8h(_8g[1],_8f);});}},_8n=function(_8o,_8p,_8q){var _8r=E(_8p);if(!_8r[0]){return E(_8q);}else{var _8s=function(_8t,_8u){while(1){var _8v=E(_8u);if(!_8v[0]){var _8w=_8v[2],_8x=_8v[4];switch(B(A(_7x,[_8o,_8w,_8t]))){case 0:return new F(function(){return _5H(_8w,_8v[3],_8x,B(_8s(_8t,_8v[5])));});break;case 1:return E(_8x);default:_8u=_8x;continue;}}else{return [1];}}};return new F(function(){return _8s(_8r[1],_8q);});}},_8y=function(_8z){return E(E(_8z)[4]);},_8A=function(_8B,_8C,_8D,_8E){var _8F=E(_8C);if(!_8F[0]){var _8G=E(_8D);if(!_8G[0]){return E(_8E);}else{var _8H=_8G[1],_8I=_8E;while(1){var _8J=E(_8I);if(!_8J[0]){if(!B(A(_7P,[_8B,_8J[2],_8H]))){return E(_8J);}else{_8I=_8J[4];continue;}}else{return [1];}}}}else{var _8K=_8F[1],_8L=E(_8D);if(!_8L[0]){var _8M=_8K,_8N=_8E;while(1){var _8O=E(_8N);if(!_8O[0]){if(!B(A(_8y,[_8B,_8O[2],_8M]))){return E(_8O);}else{_8N=_8O[5];continue;}}else{return [1];}}}else{var _8P=_8K,_8Q=_8L[1],_8R=_8E;while(1){var _8S=E(_8R);if(!_8S[0]){var _8T=_8S[2];if(!B(A(_8y,[_8B,_8T,_8P]))){if(!B(A(_7P,[_8B,_8T,_8Q]))){return E(_8S);}else{_8R=_8S[4];continue;}}else{_8R=_8S[5];continue;}}else{return [1];}}}}},_8U=function(_8V,_8W,_8X,_8Y){var _8Z=E(_8X);if(!_8Z[0]){var _90=E(_8Y);if(!_90[0]){var _91=function(_92,_93,_94,_95){var _96=E(_95);if(!_96[0]){var _97=E(_94);if(!_97[0]){var _98=_97[2],_99=_97[3],_9a=_97[5],_9b=B(_85(_8V,_98,_93,_96)),_9c=_9b[2],_9d=[1,E(_98)],_9e=B(_91(_92,_9d,_97[4],B(_8A(_8V,_92,_9d,_96)))),_9f=E(_9b[1]);if(!_9f[0]){return new F(function(){return _5H(_98,E(_99),_9e,B(_91(_9d,_93,_9a,_9c)));});}else{return new F(function(){return _5H(_98,B(A(_8W,[_98,_99,_9f[1]])),_9e,B(_91(_9d,_93,_9a,_9c)));});}}else{return new F(function(){return _5H(_96[2],_96[3],B(_8c(_8V,_92,_96[4])),B(_8n(_8V,_93,_96[5])));});}}else{return E(_94);}};return new F(function(){return (function(_9g,_9h,_9i,_9j,_9k,_9l,_9m,_9n,_9o,_9p,_9q,_9r){var _9s=B(_85(_8V,_9j,_9h,[0,_9n,E(_9o),_9p,E(_9q),E(_9r)])),_9t=_9s[2],_9u=[1,E(_9j)],_9v=B(_91(_9g,_9u,_9l,B(_8A(_8V,_9g,_9u,[0,_9n,E(_9o),_9p,E(_9q),E(_9r)])))),_9w=E(_9s[1]);if(!_9w[0]){return new F(function(){return _5H(_9j,E(_9k),_9v,B(_91(_9u,_9h,_9m,_9t)));});}else{return new F(function(){return _5H(_9j,B(A(_8W,[_9j,_9k,_9w[1]])),_9v,B(_91(_9u,_9h,_9m,_9t)));});}})(_8b,_8b,_8Z[1],_8Z[2],_8Z[3],_8Z[4],_8Z[5],_90[1],_90[2],_90[3],_90[4],_90[5]);});}else{return E(_8Z);}}else{return E(_8Y);}},_9x=function(_9y,_9z,_9A,_9B){var _9C=new T(function(){return B(A(_9A,[_9B]));}),_9D=new T(function(){return E(E(_9C)[1]);}),_9E=new T(function(){return B(A(_9z,[_9B]));}),_9F=new T(function(){return E(E(_9E)[1]);}),_9G=new T(function(){return B(_8U(_9y,_7m,B(_7q(function(_9H){return new F(function(){return _7u(_9H,_9D);});},E(_9E)[2])),B(_7q(function(_9I){return new F(function(){return _7u(_9I,_9F);});},E(_9C)[2]))));});return [0,new T(function(){return B(_7u(_9F,_9D));}),_9G];},_9J=function(_9K){return  -E(_9K);},_9L=new T(function(){return B(_7q(_9J,_3r));}),_9M=new T(function(){return B(unCStr("x"));}),_9N=function(_9O,_9P,_9Q,_9R){var _9S=_9R;while(1){var _9T=E(_9S);if(!_9T[0]){switch(B(A(_7x,[_9O,E(_9Q),_9T[2]]))){case 0:_9S=_9T[4];continue;case 1:return E(_9T[3]);default:_9S=_9T[5];continue;}}else{return E(_9P);}}},_9U=1,_9V=new T(function(){return [0,1,E(_9M),_9U,E(_3r),E(_3r)];}),_9W=new T(function(){return 0/0;}),_9X=function(_9Y){return [0,new T(function(){return B(_9N(_3q,_9W,_9M,_9Y));}),_9V];},_9Z=3,_a0=[0,_9Z,_3r],_a1=function(_a2){return E(_a0);},_a3=new T(function(){return B(unCStr("y"));}),_a4=[0,1,E(_a3),_9U,E(_3r),E(_3r)],_a5=function(_a6){var _a7=new T(function(){var _a8=B(_9x(_3q,_a1,_9X,_a6));return [0,_a8[1],_a8[2]];});return [0,new T(function(){return E(E(_a7)[1])+B(_9N(_3q,_9W,_a3,_a6))+(-5);}),new T(function(){var _a9=E(_a3);return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_a7)[2],_a4)),_9L));})];},_aa=function(_ab){var _ac=B(_a5(_ab));return [0,_ac[1],_ac[2]];},_ad=2,_ae=[0,_ad,_3r],_af=function(_ag){return E(_ae);},_ah=new T(function(){return [0,1,E(_a3),_9U,E(_3r),E(_3r)];}),_ai=function(_aj){return [0,new T(function(){return B(_9N(_3q,_9W,_a3,_aj));}),_ah];},_ak=function(_al){var _am=new T(function(){var _an=B(_9x(_3q,_a1,_ai,_al));return [0,_an[1],_an[2]];}),_ao=new T(function(){var _ap=B(_9x(_3q,_af,_9X,_al));return [0,_ap[1],_ap[2]];});return [0,new T(function(){return E(E(_ao)[1])+E(E(_am)[1]);}),new T(function(){return B(_8U(_3q,_7m,E(_ao)[2],E(_am)[2]));})];},_aq=function(_ar){var _as=B(_ak(_ar));return [0,_as[1],_as[2]];},_at=new T(function(){return B(_7q(_9J,_a4));}),_au=[0,1,E(_9M),_9U,E(_3r),E(_3r)],_av=new T(function(){var _aw=E(_9M),_ax=E(_a3);return B(_8U(_3q,_7m,_au,_at));}),_ay=function(_az){return [0,new T(function(){return B(_9N(_3q,_9W,_9M,_az))+ -B(_9N(_3q,_9W,_a3,_az));}),_av];},_aA=function(_aB){var _aC=B(_ay(_aB));return [0,_aC[1],_aC[2]];},_aD=function(_aE){var _aF=new T(function(){var _aG=B(_9x(_3q,_aq,_aA,_aE));return [0,_aG[1],_aG[2]];});return [0,new T(function(){return E(E(_aF)[1])+(-2);}),new T(function(){return B(_8U(_3q,_7m,E(_aF)[2],_9L));})];},_aH=function(_aI){var _aJ=B(_aD(_aI));return [0,_aJ[1],_aJ[2]];},_aK=function(_aL,_aM){var _aN=new T(function(){return B(_78([1,[0,_9M,_aL],[1,[0,_a3,_aM],_2]]));});return E(B(_9x(_3q,_aH,_aH,_aN))[1])+E(B(_9x(_3q,_aa,_aa,_aN))[1]);},_aO=new T(function(){return B(unCStr("ay"));}),_aP=new T(function(){return [0,1,E(_aO),_9U,E(_3r),E(_3r)];}),_aQ=function(_aR){return [0,new T(function(){return B(_9N(_3q,_9W,_aO,_aR));}),_aP];},_aS=[1,_aQ,_2],_aT=new T(function(){return B(unCStr("ax"));}),_aU=new T(function(){return [0,1,E(_aT),_9U,E(_3r),E(_3r)];}),_aV=function(_aW){return [0,new T(function(){return B(_9N(_3q,_9W,_aT,_aW));}),_aU];},_aX=[1,_aV,_aS],_aY=new T(function(){return B(unCStr("bx"));}),_aZ=[0,1,E(_aY),_9U,E(_3r),E(_3r)],_b0=new T(function(){return B(_7q(_9J,_aZ));}),_b1=new T(function(){return B(unCStr("cx"));}),_b2=[0,1,E(_b1),_9U,E(_3r),E(_3r)],_b3=new T(function(){return B(_8U(_3q,_7m,_b2,_b0));}),_b4=new T(function(){var _b5=E(_b1),_b6=E(_aY);return E(_b3);}),_b7=function(_b8){return [0,new T(function(){return B(_9N(_3q,_9W,_b1,_b8))+ -B(_9N(_3q,_9W,_aY,_b8));}),_b4];},_b9=function(_ba){var _bb=B(_b7(_ba));return [0,_bb[1],_bb[2]];},_bc=new T(function(){return B(unCStr("by"));}),_bd=[0,1,E(_bc),_9U,E(_3r),E(_3r)],_be=new T(function(){return B(_7q(_9J,_bd));}),_bf=new T(function(){return B(unCStr("cy"));}),_bg=[0,1,E(_bf),_9U,E(_3r),E(_3r)],_bh=new T(function(){return B(_8U(_3q,_7m,_bg,_be));}),_bi=new T(function(){var _bj=E(_bf),_bk=E(_bc);return E(_bh);}),_bl=function(_bm){return [0,new T(function(){return B(_9N(_3q,_9W,_bf,_bm))+ -B(_9N(_3q,_9W,_bc,_bm));}),_bi];},_bn=function(_bo){var _bp=B(_bl(_bo));return [0,_bp[1],_bp[2]];},_bq=function(_br){var _bs=new T(function(){var _bt=B(_9x(_3q,_bn,_bn,_br));return [0,_bt[1],_bt[2]];}),_bu=new T(function(){var _bv=B(_9x(_3q,_b9,_b9,_br));return [0,_bv[1],_bv[2]];});return [0,new T(function(){return E(E(_bu)[1])+E(E(_bs)[1])+(-1);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_bu)[2],E(_bs)[2])),_9L));})];},_bw=function(_bx){var _by=B(_bq(_bx));return [0,_by[1],_by[2]];},_bz=[1,_bw,_aX],_bA=new T(function(){return [0,1,E(_aY),_9U,E(_3r),E(_3r)];}),_bB=function(_bC){return [0,new T(function(){return B(_9N(_3q,_9W,_aY,_bC));}),_bA];},_bD=new T(function(){return [0,1,E(_bc),_9U,E(_3r),E(_3r)];}),_bE=function(_bF){return [0,new T(function(){return B(_9N(_3q,_9W,_bc,_bF));}),_bD];},_bG=function(_bH){var _bI=new T(function(){var _bJ=B(_9x(_3q,_bE,_bE,_bH));return [0,_bJ[1],_bJ[2]];}),_bK=new T(function(){var _bL=B(_9x(_3q,_bB,_bB,_bH));return [0,_bL[1],_bL[2]];});return [0,new T(function(){return E(E(_bK)[1])+E(E(_bI)[1])+(-1);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_bK)[2],E(_bI)[2])),_9L));})];},_bM=function(_bN){var _bO=B(_bG(_bN));return [0,_bO[1],_bO[2]];},_bP=[1,_bM,_bz],_bQ=function(_bR){var _bS=new T(function(){var _bT=B(_9x(_3q,_aQ,_aQ,_bR));return [0,_bT[1],_bT[2]];}),_bU=new T(function(){var _bV=B(_9x(_3q,_aV,_aV,_bR));return [0,_bV[1],_bV[2]];});return [0,new T(function(){return E(E(_bU)[1])+E(E(_bS)[1])+(-1);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_bU)[2],E(_bS)[2])),_9L));})];},_bW=function(_bX){var _bY=B(_bQ(_bX));return [0,_bY[1],_bY[2]];},_bZ=[0,1,E(_aT),_9U,E(_3r),E(_3r)],_c0=new T(function(){var _c1=E(_aT),_c2=E(_aY);return B(_8U(_3q,_7m,_bZ,_b0));}),_c3=function(_c4){return [0,new T(function(){return B(_9N(_3q,_9W,_aT,_c4))+ -B(_9N(_3q,_9W,_aY,_c4));}),_c0];},_c5=function(_c6){var _c7=B(_c3(_c6));return [0,_c7[1],_c7[2]];},_c8=[0,1,E(_aO),_9U,E(_3r),E(_3r)],_c9=new T(function(){var _ca=E(_aO),_cb=E(_bc);return B(_8U(_3q,_7m,_c8,_be));}),_cc=function(_cd){return [0,new T(function(){return B(_9N(_3q,_9W,_aO,_cd))+ -B(_9N(_3q,_9W,_bc,_cd));}),_c9];},_ce=function(_cf){var _cg=B(_cc(_cf));return [0,_cg[1],_cg[2]];},_ch=function(_ci){var _cj=new T(function(){var _ck=B(_9x(_3q,_ce,_ce,_ci));return [0,_ck[1],_ck[2]];}),_cl=new T(function(){var _cm=B(_9x(_3q,_c5,_c5,_ci));return [0,_cm[1],_cm[2]];});return [0,new T(function(){return E(E(_cl)[1])+E(E(_cj)[1])+(-2);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_cl)[2],E(_cj)[2])),_9L));})];},_cn=function(_co){var _cp=B(_ch(_co));return [0,_cp[1],_cp[2]];},_cq=new T(function(){var _cr=E(_bf);return E(_bg);}),_cs=function(_ct){return [0,new T(function(){return B(_9N(_3q,_9W,_bf,_ct));}),_cq];},_cu=[1,_cs,_2],_cv=new T(function(){var _cw=E(_bc);return E(_bd);}),_cx=function(_cy){return [0,new T(function(){return B(_9N(_3q,_9W,_bc,_cy));}),_cv];},_cz=[1,_cx,_cu],_cA=new T(function(){var _cB=E(_b1),_cC=E(_aY);return B(_8U(_3q,_7m,_b3,_9L));}),_cD=function(_cE){return [0,new T(function(){return B(_9N(_3q,_9W,_b1,_cE))+ -B(_9N(_3q,_9W,_aY,_cE))+(-1);}),_cA];},_cF=function(_cG){var _cH=B(_cD(_cG));return [0,_cH[1],_cH[2]];},_cI=[1,_cF,_cz],_cJ=[1,_cn,_cI],_cK=[1,_bW,_cJ],_cL=(function(s,f){Haste[s] = f;}),_cM=(function(s,f){Haste[s] = f;}),_cN=(function(s,f){Haste[s] = f;}),_cO=(function(s,f){Haste[s] = f;}),_cP=(function(s,f){Haste[s] = f;}),_cQ=(function(s,f){Haste[s] = f;}),_cR=(function(s,f){Haste[s] = f;}),_cS=(function(s,f){Haste[s] = f;}),_cT=new T(function(){return B(unCStr("Map.!: given key is not an element in the map"));}),_cU=new T(function(){return B(err(_cT));}),_cV=function(_cW,_cX){while(1){var _cY=E(_cX);if(!_cY[0]){switch(B(_31(_cW,_cY[2]))){case 0:_cX=_cY[4];continue;case 1:return E(_cY[3]);default:_cX=_cY[5];continue;}}else{return E(_cU);}}},_cZ=function(_d0,_d1){var _d2=new T(function(){return B(_78([1,[0,_9M,_d0],[1,[0,_a3,_d1],_2]]));});return new F(function(){return _cV(_9M,B(_8U(_3q,_7m,B(_9x(_3q,_aH,_aH,_d2))[2],B(_9x(_3q,_aa,_aa,_d2))[2])));});},_d3=function(_d4,_d5){var _d6=new T(function(){return B(_78([1,[0,_9M,_d4],[1,[0,_a3,_d5],_2]]));});return new F(function(){return _cV(_a3,B(_8U(_3q,_7m,B(_9x(_3q,_aH,_aH,_d6))[2],B(_9x(_3q,_aa,_aa,_d6))[2])));});},_d7=function(_d8,_d9){var _da=E(_d9);return (_da[0]==0)?[0]:[1,new T(function(){return B(A(_d8,[_da[1]]));}),new T(function(){return B(_d7(_d8,_da[2]));})];},_db=function(_dc){var _dd=E(_dc);return new F(function(){return __lst2arr(B(_d7(_d,[1,_dd[1],[1,_dd[2],_2]])));});},_de=function(_df){var _dg=E(_df);return new F(function(){return __lst2arr(B(_d7(_d,[1,new T(function(){return toJSStr(E(_dg[1]));}),[1,_dg[2],_2]])));});},_dh=function(_di,_dj){return E(_di)/E(_dj);},_dk=function(_dl,_dm,_dn,_do){var _dp=new T(function(){return B(A(_dn,[_do]));}),_dq=new T(function(){return E(E(_dp)[1]);}),_dr=new T(function(){return B(A(_dm,[_do]));}),_ds=new T(function(){return E(E(_dr)[1]);}),_dt=new T(function(){var _du=new T(function(){return Math.pow(E(_dq),2);});return B(_7q(function(_dv){return new F(function(){return _dh(_dv,_du);});},B(_8U(_dl,_7m,B(_7q(function(_dw){return new F(function(){return _7u(_dw,_dq);});},E(_dr)[2])),B(_7q(function(_dx){return  -(E(_dx)*E(_ds));},E(_dp)[2]))))));});return [0,new T(function(){return B(_dh(_ds,_dq));}),_dt];},_dy=new T(function(){return B(_7q(_9J,_bg));}),_dz=new T(function(){var _dA=E(_aO),_dB=E(_bc);return B(_8U(_3q,_7m,_c8,_bd));}),_dC=function(_dD){return [0,new T(function(){return B(_9N(_3q,_9W,_aO,_dD))+B(_9N(_3q,_9W,_bc,_dD));}),_dz];},_dE=function(_dF){var _dG=B(_dC(_dF));return [0,_dG[1],_dG[2]];},_dH=function(_dI){var _dJ=new T(function(){var _dK=B(_dk(_3q,_dE,_af,_dI));return [0,_dK[1],_dK[2]];});return [0,new T(function(){return E(E(_dJ)[1])+ -B(_9N(_3q,_9W,_bf,_dI));}),new T(function(){var _dL=E(_bf);return B(_8U(_3q,_7m,E(_dJ)[2],_dy));})];},_dM=function(_dN){var _dO=B(_dH(_dN));return [0,_dO[1],_dO[2]];},_dP=new T(function(){return B(_7q(_9J,_b2));}),_dQ=new T(function(){return B(unCStr("dx"));}),_dR=[0,1,E(_dQ),_9U,E(_3r),E(_3r)],_dS=new T(function(){var _dT=E(_dQ),_dU=E(_b1);return B(_8U(_3q,_7m,_dR,_dP));}),_dV=function(_dW){return [0,new T(function(){return B(_9N(_3q,_9W,_dQ,_dW))+ -B(_9N(_3q,_9W,_b1,_dW));}),_dS];},_dX=function(_dY){var _dZ=B(_dV(_dY));return [0,_dZ[1],_dZ[2]];},_e0=new T(function(){return B(_7q(_9J,_bZ));}),_e1=new T(function(){var _e2=E(_aY),_e3=E(_aT);return B(_8U(_3q,_7m,_aZ,_e0));}),_e4=function(_e5){return [0,new T(function(){return B(_9N(_3q,_9W,_aY,_e5))+ -B(_9N(_3q,_9W,_aT,_e5));}),_e1];},_e6=function(_e7){var _e8=B(_e4(_e7));return [0,_e8[1],_e8[2]];},_e9=new T(function(){return B(unCStr("dy"));}),_ea=[0,1,E(_e9),_9U,E(_3r),E(_3r)],_eb=new T(function(){var _ec=E(_e9),_ed=E(_bf);return B(_8U(_3q,_7m,_ea,_dy));}),_ee=function(_ef){return [0,new T(function(){return B(_9N(_3q,_9W,_e9,_ef))+ -B(_9N(_3q,_9W,_bf,_ef));}),_eb];},_eg=function(_eh){var _ei=B(_ee(_eh));return [0,_ei[1],_ei[2]];},_ej=new T(function(){return B(_7q(_9J,_c8));}),_ek=new T(function(){var _el=E(_bc),_em=E(_aO);return B(_8U(_3q,_7m,_bd,_ej));}),_en=function(_eo){return [0,new T(function(){return B(_9N(_3q,_9W,_bc,_eo))+ -B(_9N(_3q,_9W,_aO,_eo));}),_ek];},_ep=function(_eq){var _er=B(_en(_eq));return [0,_er[1],_er[2]];},_es=function(_et){var _eu=new T(function(){var _ev=B(_9x(_3q,_ep,_eg,_et));return [0,_ev[1],_ev[2]];}),_ew=new T(function(){var _ex=B(_9x(_3q,_e6,_dX,_et));return [0,_ex[1],_ex[2]];});return [0,new T(function(){return E(E(_ew)[1])+E(E(_eu)[1]);}),new T(function(){return B(_8U(_3q,_7m,E(_ew)[2],E(_eu)[2]));})];},_ey=function(_ez){var _eA=B(_es(_ez));return [0,_eA[1],_eA[2]];},_eB=new T(function(){return B(_7q(_9J,_ea));}),_eC=new T(function(){var _eD=E(_bf),_eE=E(_e9);return B(_8U(_3q,_7m,_bg,_eB));}),_eF=function(_eG){return [0,new T(function(){return B(_9N(_3q,_9W,_bf,_eG))+ -B(_9N(_3q,_9W,_e9,_eG));}),_eC];},_eH=function(_eI){var _eJ=B(_eF(_eI));return [0,_eJ[1],_eJ[2]];},_eK=new T(function(){return B(_7q(_9J,_dR));}),_eL=new T(function(){var _eM=E(_b1),_eN=E(_dQ);return B(_8U(_3q,_7m,_b2,_eK));}),_eO=function(_eP){return [0,new T(function(){return B(_9N(_3q,_9W,_b1,_eP))+ -B(_9N(_3q,_9W,_dQ,_eP));}),_eL];},_eQ=function(_eR){var _eS=B(_eO(_eR));return [0,_eS[1],_eS[2]];},_eT=function(_eU){var _eV=new T(function(){var _eW=B(_9x(_3q,_eH,_eH,_eU));return [0,_eW[1],_eW[2]];}),_eX=new T(function(){var _eY=B(_9x(_3q,_eQ,_eQ,_eU));return [0,_eY[1],_eY[2]];});return [0,new T(function(){return E(E(_eX)[1])+E(E(_eV)[1])+(-1);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_eX)[2],E(_eV)[2])),_9L));})];},_eZ=function(_f0){var _f1=B(_eT(_f0));return [0,_f1[1],_f1[2]];},_f2=[1,_eZ,_2],_f3=[1,_ey,_f2],_f4=[1,_dM,_f3],_f5=new T(function(){var _f6=E(_aT),_f7=E(_aY);return B(_8U(_3q,_7m,_bZ,_aZ));}),_f8=function(_f9){return [0,new T(function(){return B(_9N(_3q,_9W,_aT,_f9))+B(_9N(_3q,_9W,_aY,_f9));}),_f5];},_fa=function(_fb){var _fc=B(_f8(_fb));return [0,_fc[1],_fc[2]];},_fd=function(_fe){var _ff=new T(function(){var _fg=B(_dk(_3q,_fa,_af,_fe));return [0,_fg[1],_fg[2]];});return [0,new T(function(){return E(E(_ff)[1])+ -B(_9N(_3q,_9W,_b1,_fe));}),new T(function(){var _fh=E(_b1);return B(_8U(_3q,_7m,E(_ff)[2],_dP));})];},_fi=function(_fj){var _fk=B(_fd(_fj));return [0,_fk[1],_fk[2]];},_fl=[1,_fi,_f4],_fm=[0,1],_fn=function(_fo){var _fp=I_decodeDouble(_fo);return [0,[1,_fp[2]],_fp[1]];},_fq=function(_fr){return [0,_fr];},_fs=function(_ft){var _fu=hs_intToInt64(2147483647),_fv=hs_leInt64(_ft,_fu);if(!_fv){return [1,I_fromInt64(_ft)];}else{var _fw=hs_intToInt64(-2147483648),_fx=hs_geInt64(_ft,_fw);if(!_fx){return [1,I_fromInt64(_ft)];}else{var _fy=hs_int64ToInt(_ft);return new F(function(){return _fq(_fy);});}}},_fz=new T(function(){var _fA=newByteArr(256),_fB=_fA,_=_fB["v"]["i8"][0]=8,_fC=function(_fD,_fE,_fF,_){while(1){if(_fF>=256){if(_fD>=256){return E(_);}else{var _fG=imul(2,_fD)|0,_fH=_fE+1|0,_fI=_fD;_fD=_fG;_fE=_fH;_fF=_fI;continue;}}else{var _=_fB["v"]["i8"][_fF]=_fE,_fI=_fF+_fD|0;_fF=_fI;continue;}}},_=B(_fC(2,0,1,_));return _fB;}),_fJ=function(_fK,_fL){while(1){var _fM=B((function(_fN,_fO){var _fP=hs_int64ToInt(_fN),_fQ=E(_fz)["v"]["i8"][(255&_fP>>>0)>>>0&4294967295];if(_fO>_fQ){if(_fQ>=8){var _fR=hs_uncheckedIShiftRA64(_fN,8),_fS=_fO-8|0;_fK=_fR;_fL=_fS;return null;}else{return [0,new T(function(){var _fT=hs_uncheckedIShiftRA64(_fN,_fQ);return B(_fs(_fT));}),_fO-_fQ|0];}}else{return [0,new T(function(){var _fU=hs_uncheckedIShiftRA64(_fN,_fO);return B(_fs(_fU));}),0];}})(_fK,_fL));if(_fM!=null){return _fM;}}},_fV=function(_fW){var _fX=hs_intToInt64(_fW);return E(_fX);},_fY=function(_fZ){var _g0=E(_fZ);if(!_g0[0]){return new F(function(){return _fV(_g0[1]);});}else{return new F(function(){return I_toInt64(_g0[1]);});}},_g1=function(_g2){return I_toInt(_g2)>>>0;},_g3=function(_g4){var _g5=E(_g4);if(!_g5[0]){return _g5[1]>>>0;}else{return new F(function(){return _g1(_g5[1]);});}},_g6=function(_g7,_g8){while(1){var _g9=E(_g7);if(!_g9[0]){_g7=[1,I_fromInt(_g9[1])];continue;}else{return [1,I_shiftLeft(_g9[1],_g8)];}}},_ga=function(_gb){var _gc=B(_fn(_gb)),_gd=_gc[1],_ge=_gc[2];if(_ge<0){var _gf=function(_gg){if(!_gg){return [0,E(_gd),B(_g6(_fm, -_ge))];}else{var _gh=B(_fJ(B(_fY(_gd)), -_ge));return [0,E(_gh[1]),B(_g6(_fm,_gh[2]))];}};if(!((B(_g3(_gd))&1)>>>0)){return new F(function(){return _gf(1);});}else{return new F(function(){return _gf(0);});}}else{return [0,B(_g6(_gd,_ge)),_fm];}},_gi=function(_gj,_gk){while(1){var _gl=E(_gj);if(!_gl[0]){return E(_gk);}else{var _gm=_gk+1|0;_gj=_gl[2];_gk=_gm;continue;}}},_gn=function(_go,_gp){var _gq=E(_gp);if(!_gq[0]){return [0,_2,_2];}else{var _gr=_gq[1],_gs=_gq[2],_gt=E(_go);if(_gt==1){return [0,[1,_gr,_2],_gs];}else{var _gu=new T(function(){var _gv=B(_gn(_gt-1|0,_gs));return [0,_gv[1],_gv[2]];});return [0,[1,_gr,new T(function(){return E(E(_gu)[1]);})],new T(function(){return E(E(_gu)[2]);})];}}},_gw=function(_gx,_gy){while(1){var _gz=E(_gy);if(!_gz[0]){return [0];}else{var _gA=_gz[2],_gB=E(_gx);if(_gB==1){return E(_gA);}else{_gx=_gB-1|0;_gy=_gA;continue;}}}},_gC=false,_gD=0,_gE=[0,_gD,_3r],_gF=function(_gG){return E(_gE);},_gH=function(_gI,_gJ){var _gK=function(_gL,_gM){while(1){var _gN=B((function(_gO,_gP){var _gQ=E(_gO);if(!_gQ[0]){return [0];}else{var _gR=_gQ[2];if(!B(A(_gI,[_gQ[1]]))){var _gS=_gP+1|0;_gL=_gR;_gM=_gS;return null;}else{return [1,_gP,new T(function(){return B(_gK(_gR,_gP+1|0));})];}}})(_gL,_gM));if(_gN!=null){return _gN;}}},_gT=B(_gK(_gJ,0));return (_gT[0]==0)?[0]:[1,_gT[1]];},_gU=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_gV=new T(function(){return B(err(_gU));}),_gW=function(_gX){while(1){var _gY=E(_gX);if(!_gY[0]){return true;}else{if(!E(_gY[1])){return false;}else{_gX=_gY[2];continue;}}}},_gZ=function(_h0){return (!E(_h0))?true:false;},_h1=new T(function(){return B(unCStr(": empty list"));}),_h2=new T(function(){return B(unCStr("Prelude."));}),_h3=function(_h4){return new F(function(){return err(B(_17(_h2,new T(function(){return B(_17(_h4,_h1));},1))));});},_h5=new T(function(){return B(unCStr("last"));}),_h6=new T(function(){return B(_h3(_h5));}),_h7=function(_h8,_h9){while(1){var _ha=E(_h9);if(!_ha[0]){var _hb=B(_h7(_h8,_ha[4]))+E(_ha[3]);_h8=_hb;_h9=_ha[5];continue;}else{return E(_h8);}}},_hc=function(_hd,_he){return E(_hd)-E(_he);},_hf=function(_hg,_hh,_hi){return new F(function(){return _hc(_hh,_hi);});},_hj=function(_hk){return new F(function(){return _7u(_hk,_hk);});},_hl=function(_hm){while(1){var _hn=E(_hm);if(!_hn[0]){var _ho=_hn[3],_hp=_hn[5];if(E(_hn[1])==1){var _hq=E(_ho);return (_hq==0)?true:(_hq<=0)? -_hq<1.0e-6:_hq<1.0e-6;}else{if(!B(_hl(_hn[4]))){return false;}else{var _hr=E(_ho);if(!_hr){_hm=_hp;continue;}else{if(_hr<=0){if( -_hr>=1.0e-6){return false;}else{_hm=_hp;continue;}}else{if(_hr>=1.0e-6){return false;}else{_hm=_hp;continue;}}}}}}else{return true;}}},_hs=function(_ht,_hu,_hv){var _hw=B(A(_hu,[_hv])),_hx=_hw[2],_hy=E(_hw[1]);if(_hy>=1.0e-6){if(!B(_hl(_hx))){var _hz=function(_hA){while(1){var _hB=B((function(_hC){var _hD=new T(function(){return B(_8U(_ht,_hf,_hv,B(_7q(function(_hE){return E(_hE)*_hC;},_hx))));}),_hF=E(B(A(_hu,[_hD]))[1]);if(_hy-_hF<_hC*0.5*B(_h7(0,B(_7q(_hj,_hx))))){var _hG=_hC*0.5;_hA=_hG;return null;}else{return [0,_hD,new T(function(){var _hH=_hy-_hF;if(!_hH){return true;}else{if(_hH<=0){return  -_hH<1.0e-6;}else{return _hH<1.0e-6;}}})];}})(_hA));if(_hB!=null){return _hB;}}},_hI=B(_hz(2));return (!E(_hI[2]))?[1,_hI[1]]:[0];}else{return [0];}}else{return [0];}},_hJ=function(_hK,_hL,_hM){var _hN=function(_hO,_hP){while(1){var _hQ=E(_hO);if(!_hQ[0]){return E(_hP);}else{_hO=B(_hs(_hK,_hL,_hQ[1]));_hP=_hQ;continue;}}},_hR=B((function(_hS,_hT){return new F(function(){return _hN(B(_hs(_hK,_hL,_hS)),[1,_hS]);});})(_hM,_h6));return (_hR[0]==0)?E(_gV):E(_hR[1]);},_hU=function(_hV,_hW,_hX){var _hY=new T(function(){var _hZ=new T(function(){var _i0=function(_i1,_i2){while(1){var _i3=B((function(_i4,_i5){var _i6=E(_i4);if(!_i6[0]){return E(_i5);}else{var _i7=_i6[1],_i8=function(_i9){var _ia=new T(function(){var _ib=B(_9x(_hV,_i7,_i7,_i9));return [0,_ib[1],_ib[2]];}),_ic=new T(function(){return B(A(_i5,[_i9]));});return [0,new T(function(){return E(E(_ic)[1])+E(E(_ia)[1]);}),new T(function(){return B(_8U(_hV,_7m,E(_ic)[2],E(_ia)[2]));})];};_i1=_i6[2];_i2=_i8;return null;}})(_i1,_i2));if(_i3!=null){return _i3;}}};return B(_i0(_hW,_gF));});return B(_hJ(_hV,_hZ,_hX));}),_id=B(_d7(function(_ie){var _if=E(B(A(_ie,[_hY]))[1]);return _if*_if<Math.sqrt(1.0e-6);},_hW));if(!B(_gW(_id))){var _ig=new T(function(){var _ih=B(_gH(_gZ,_id));if(!_ih[0]){return E(_gV);}else{return E(_ih[1]);}}),_ii=new T(function(){var _ij=E(_ig);if(_ij>0){var _ik=B(_gn(_ij,_hW)),_il=B(_hU(_hV,B(_17(_ik[1],new T(function(){return B(_gw(1,_ik[2]));},1))),_hY));return [0,_il[1],_il[2]];}else{var _im=B(_hU(_hV,B(_17(_2,new T(function(){return B(_gw(1,_hW));},1))),_hY));return [0,_im[1],_im[2]];}}),_in=new T(function(){var _io=E(_ig);if(_io>0){var _ip=B(_gn(_io,E(_ii)[1]));return B(_17(_ip[1],[1,_gC,_ip[2]]));}else{return B(_17(_2,[1,_gC,new T(function(){return E(E(_ii)[1]);})]));}});return [0,_in,new T(function(){return E(E(_ii)[2]);})];}else{return [0,_id,_hY];}},_iq=function(_ir,_is){var _it=E(_ir);if(!_it[0]){var _iu=_it[1],_iv=E(_is);if(!_iv[0]){var _iw=_iv[1];return (_iu!=_iw)?(_iu>_iw)?2:0:1;}else{var _ix=I_compareInt(_iv[1],_iu);return (_ix<=0)?(_ix>=0)?1:2:0;}}else{var _iy=_it[1],_iz=E(_is);if(!_iz[0]){var _iA=I_compareInt(_iy,_iz[1]);return (_iA>=0)?(_iA<=0)?1:2:0;}else{var _iB=I_compare(_iy,_iz[1]);return (_iB>=0)?(_iB<=0)?1:2:0;}}},_iC=new T(function(){return B(unCStr("GHC.Exception"));}),_iD=new T(function(){return B(unCStr("base"));}),_iE=new T(function(){return B(unCStr("ArithException"));}),_iF=new T(function(){var _iG=hs_wordToWord64(4194982440),_iH=hs_wordToWord64(3110813675);return [0,_iG,_iH,[0,_iG,_iH,_iD,_iC,_iE],_2,_2];}),_iI=function(_iJ){return E(_iF);},_iK=function(_iL){var _iM=E(_iL);return new F(function(){return _T(B(_R(_iM[1])),_iI,_iM[2]);});},_iN=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_iO=new T(function(){return B(unCStr("denormal"));}),_iP=new T(function(){return B(unCStr("divide by zero"));}),_iQ=new T(function(){return B(unCStr("loss of precision"));}),_iR=new T(function(){return B(unCStr("arithmetic underflow"));}),_iS=new T(function(){return B(unCStr("arithmetic overflow"));}),_iT=function(_iU,_iV){switch(E(_iU)){case 0:return new F(function(){return _17(_iS,_iV);});break;case 1:return new F(function(){return _17(_iR,_iV);});break;case 2:return new F(function(){return _17(_iQ,_iV);});break;case 3:return new F(function(){return _17(_iP,_iV);});break;case 4:return new F(function(){return _17(_iO,_iV);});break;default:return new F(function(){return _17(_iN,_iV);});}},_iW=function(_iX){return new F(function(){return _iT(_iX,_2);});},_iY=function(_iZ,_j0,_j1){return new F(function(){return _iT(_j0,_j1);});},_j2=function(_j3,_j4){return new F(function(){return _28(_iT,_j3,_j4);});},_j5=[0,_iY,_iW,_j2],_j6=new T(function(){return [0,_iI,_j5,_j7,_iK,_iW];}),_j7=function(_j8){return [0,_j6,_j8];},_j9=3,_ja=new T(function(){return B(_j7(_j9));}),_jb=new T(function(){return die(_ja);}),_jc=function(_jd,_je){var _jf=E(_jd);return (_jf[0]==0)?_jf[1]*Math.pow(2,_je):I_toNumber(_jf[1])*Math.pow(2,_je);},_jg=function(_jh,_ji){var _jj=E(_jh);if(!_jj[0]){var _jk=_jj[1],_jl=E(_ji);return (_jl[0]==0)?_jk==_jl[1]:(I_compareInt(_jl[1],_jk)==0)?true:false;}else{var _jm=_jj[1],_jn=E(_ji);return (_jn[0]==0)?(I_compareInt(_jm,_jn[1])==0)?true:false:(I_compare(_jm,_jn[1])==0)?true:false;}},_jo=function(_jp){var _jq=E(_jp);if(!_jq[0]){return E(_jq[1]);}else{return new F(function(){return I_toInt(_jq[1]);});}},_jr=function(_js,_jt){while(1){var _ju=E(_js);if(!_ju[0]){var _jv=_ju[1],_jw=E(_jt);if(!_jw[0]){var _jx=_jw[1],_jy=addC(_jv,_jx);if(!E(_jy[2])){return [0,_jy[1]];}else{_js=[1,I_fromInt(_jv)];_jt=[1,I_fromInt(_jx)];continue;}}else{_js=[1,I_fromInt(_jv)];_jt=_jw;continue;}}else{var _jz=E(_jt);if(!_jz[0]){_js=_ju;_jt=[1,I_fromInt(_jz[1])];continue;}else{return [1,I_add(_ju[1],_jz[1])];}}}},_jA=function(_jB,_jC){while(1){var _jD=E(_jB);if(!_jD[0]){var _jE=E(_jD[1]);if(_jE==(-2147483648)){_jB=[1,I_fromInt(-2147483648)];continue;}else{var _jF=E(_jC);if(!_jF[0]){var _jG=_jF[1];return [0,[0,quot(_jE,_jG)],[0,_jE%_jG]];}else{_jB=[1,I_fromInt(_jE)];_jC=_jF;continue;}}}else{var _jH=E(_jC);if(!_jH[0]){_jB=_jD;_jC=[1,I_fromInt(_jH[1])];continue;}else{var _jI=I_quotRem(_jD[1],_jH[1]);return [0,[1,_jI[1]],[1,_jI[2]]];}}}},_jJ=[0,0],_jK=function(_jL,_jM,_jN){if(!B(_jg(_jN,_jJ))){var _jO=B(_jA(_jM,_jN)),_jP=_jO[1];switch(B(_iq(B(_g6(_jO[2],1)),_jN))){case 0:return new F(function(){return _jc(_jP,_jL);});break;case 1:if(!(B(_jo(_jP))&1)){return new F(function(){return _jc(_jP,_jL);});}else{return new F(function(){return _jc(B(_jr(_jP,_fm)),_jL);});}break;default:return new F(function(){return _jc(B(_jr(_jP,_fm)),_jL);});}}else{return E(_jb);}},_jQ=function(_jR,_jS){var _jT=E(_jR);if(!_jT[0]){var _jU=_jT[1],_jV=E(_jS);return (_jV[0]==0)?_jU>_jV[1]:I_compareInt(_jV[1],_jU)<0;}else{var _jW=_jT[1],_jX=E(_jS);return (_jX[0]==0)?I_compareInt(_jW,_jX[1])>0:I_compare(_jW,_jX[1])>0;}},_jY=[0,1],_jZ=function(_k0,_k1){var _k2=E(_k0);if(!_k2[0]){var _k3=_k2[1],_k4=E(_k1);return (_k4[0]==0)?_k3<_k4[1]:I_compareInt(_k4[1],_k3)>0;}else{var _k5=_k2[1],_k6=E(_k1);return (_k6[0]==0)?I_compareInt(_k5,_k6[1])<0:I_compare(_k5,_k6[1])<0;}},_k7=new T(function(){return B(unCStr("Control.Exception.Base"));}),_k8=new T(function(){return B(unCStr("base"));}),_k9=new T(function(){return B(unCStr("PatternMatchFail"));}),_ka=new T(function(){var _kb=hs_wordToWord64(18445595),_kc=hs_wordToWord64(52003073);return [0,_kb,_kc,[0,_kb,_kc,_k8,_k7,_k9],_2,_2];}),_kd=function(_ke){return E(_ka);},_kf=function(_kg){var _kh=E(_kg);return new F(function(){return _T(B(_R(_kh[1])),_kd,_kh[2]);});},_ki=function(_kj){return E(E(_kj)[1]);},_kk=function(_kl){return [0,_km,_kl];},_kn=function(_ko,_kp){return new F(function(){return _17(E(_ko)[1],_kp);});},_kq=function(_kr,_ks){return new F(function(){return _28(_kn,_kr,_ks);});},_kt=function(_ku,_kv,_kw){return new F(function(){return _17(E(_kv)[1],_kw);});},_kx=[0,_kt,_ki,_kq],_km=new T(function(){return [0,_kd,_kx,_kk,_kf,_ki];}),_ky=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_kz=function(_kA){return E(E(_kA)[3]);},_kB=function(_kC,_kD){return new F(function(){return die(new T(function(){return B(A(_kz,[_kD,_kC]));}));});},_kE=function(_kF,_j8){return new F(function(){return _kB(_kF,_j8);});},_kG=function(_kH,_kI){var _kJ=E(_kI);if(!_kJ[0]){return [0,_2,_2];}else{var _kK=_kJ[1];if(!B(A(_kH,[_kK]))){return [0,_2,_kJ];}else{var _kL=new T(function(){var _kM=B(_kG(_kH,_kJ[2]));return [0,_kM[1],_kM[2]];});return [0,[1,_kK,new T(function(){return E(E(_kL)[1]);})],new T(function(){return E(E(_kL)[2]);})];}}},_kN=32,_kO=new T(function(){return B(unCStr("\n"));}),_kP=function(_kQ){return (E(_kQ)==124)?false:true;},_kR=function(_kS,_kT){var _kU=B(_kG(_kP,B(unCStr(_kS)))),_kV=_kU[1],_kW=function(_kX,_kY){var _kZ=new T(function(){var _l0=new T(function(){return B(_17(_kT,new T(function(){return B(_17(_kY,_kO));},1)));});return B(unAppCStr(": ",_l0));},1);return new F(function(){return _17(_kX,_kZ);});},_l1=E(_kU[2]);if(!_l1[0]){return new F(function(){return _kW(_kV,_2);});}else{if(E(_l1[1])==124){return new F(function(){return _kW(_kV,[1,_kN,_l1[2]]);});}else{return new F(function(){return _kW(_kV,_2);});}}},_l2=function(_l3){return new F(function(){return _kE([0,new T(function(){return B(_kR(_l3,_ky));})],_km);});},_l4=function(_l5){var _l6=_jY,_l7=0;while(1){if(!B(_jZ(_l6,_l5))){if(!B(_jQ(_l6,_l5))){if(!B(_jg(_l6,_l5))){return new F(function(){return _l2("GHC/Integer/Type.lhs:(555,5)-(557,32)|function l2");});}else{return E(_l7);}}else{return _l7-1|0;}}else{var _l8=B(_g6(_l6,1)),_l9=_l7+1|0;_l6=_l8;_l7=_l9;continue;}}},_la=function(_lb){var _lc=E(_lb);if(!_lc[0]){var _ld=_lc[1]>>>0;if(!_ld){return -1;}else{var _le=1,_lf=0;while(1){if(_le>=_ld){if(_le<=_ld){if(_le!=_ld){return new F(function(){return _l2("GHC/Integer/Type.lhs:(612,5)-(614,40)|function l2");});}else{return E(_lf);}}else{return _lf-1|0;}}else{var _lg=imul(_le,2)>>>0,_lh=_lf+1|0;_le=_lg;_lf=_lh;continue;}}}}else{return new F(function(){return _l4(_lc);});}},_li=function(_lj){var _lk=E(_lj);if(!_lk[0]){var _ll=_lk[1]>>>0;if(!_ll){return [0,-1,0];}else{var _lm=function(_ln,_lo){while(1){if(_ln>=_ll){if(_ln<=_ll){if(_ln!=_ll){return new F(function(){return _l2("GHC/Integer/Type.lhs:(612,5)-(614,40)|function l2");});}else{return E(_lo);}}else{return _lo-1|0;}}else{var _lp=imul(_ln,2)>>>0,_lq=_lo+1|0;_ln=_lp;_lo=_lq;continue;}}};return [0,B(_lm(1,0)),(_ll&_ll-1>>>0)>>>0&4294967295];}}else{var _lr=B(_la(_lk)),_ls=_lr>>>0;if(!_ls){var _lt=E(_lr);return (_lt==(-2))?[0,-2,0]:[0,_lt,1];}else{var _lu=function(_lv,_lw){while(1){if(_lv>=_ls){if(_lv<=_ls){if(_lv!=_ls){return new F(function(){return _l2("GHC/Integer/Type.lhs:(612,5)-(614,40)|function l2");});}else{return E(_lw);}}else{return _lw-1|0;}}else{var _lx=imul(_lv,2)>>>0,_ly=_lw+1|0;_lv=_lx;_lw=_ly;continue;}}},_lz=B(_lu(1,0));return ((_lz+_lz|0)!=_lr)?[0,_lr,1]:[0,_lr,0];}}},_lA=function(_lB,_lC){var _lD=E(_lB);if(!_lD[0]){var _lE=_lD[1],_lF=E(_lC);return (_lF[0]==0)?_lE<=_lF[1]:I_compareInt(_lF[1],_lE)>=0;}else{var _lG=_lD[1],_lH=E(_lC);return (_lH[0]==0)?I_compareInt(_lG,_lH[1])<=0:I_compare(_lG,_lH[1])<=0;}},_lI=function(_lJ,_lK){while(1){var _lL=E(_lJ);if(!_lL[0]){var _lM=_lL[1],_lN=E(_lK);if(!_lN[0]){return [0,(_lM>>>0&_lN[1]>>>0)>>>0&4294967295];}else{_lJ=[1,I_fromInt(_lM)];_lK=_lN;continue;}}else{var _lO=E(_lK);if(!_lO[0]){_lJ=_lL;_lK=[1,I_fromInt(_lO[1])];continue;}else{return [1,I_and(_lL[1],_lO[1])];}}}},_lP=function(_lQ,_lR){while(1){var _lS=E(_lQ);if(!_lS[0]){var _lT=_lS[1],_lU=E(_lR);if(!_lU[0]){var _lV=_lU[1],_lW=subC(_lT,_lV);if(!E(_lW[2])){return [0,_lW[1]];}else{_lQ=[1,I_fromInt(_lT)];_lR=[1,I_fromInt(_lV)];continue;}}else{_lQ=[1,I_fromInt(_lT)];_lR=_lU;continue;}}else{var _lX=E(_lR);if(!_lX[0]){_lQ=_lS;_lR=[1,I_fromInt(_lX[1])];continue;}else{return [1,I_sub(_lS[1],_lX[1])];}}}},_lY=[0,2],_lZ=function(_m0,_m1){var _m2=E(_m0);if(!_m2[0]){var _m3=(_m2[1]>>>0&(2<<_m1>>>0)-1>>>0)>>>0,_m4=1<<_m1>>>0;return (_m4<=_m3)?(_m4>=_m3)?1:2:0;}else{var _m5=B(_lI(_m2,B(_lP(B(_g6(_lY,_m1)),_jY)))),_m6=B(_g6(_jY,_m1));return (!B(_jQ(_m6,_m5)))?(!B(_jZ(_m6,_m5)))?1:2:0;}},_m7=function(_m8,_m9){while(1){var _ma=E(_m8);if(!_ma[0]){_m8=[1,I_fromInt(_ma[1])];continue;}else{return [1,I_shiftRight(_ma[1],_m9)];}}},_mb=function(_mc,_md,_me,_mf){var _mg=B(_li(_mf)),_mh=_mg[1];if(!E(_mg[2])){var _mi=B(_la(_me));if(_mi<((_mh+_mc|0)-1|0)){var _mj=_mh+(_mc-_md|0)|0;if(_mj>0){if(_mj>_mi){if(_mj<=(_mi+1|0)){if(!E(B(_li(_me))[2])){return 0;}else{return new F(function(){return _jc(_fm,_mc-_md|0);});}}else{return 0;}}else{var _mk=B(_m7(_me,_mj));switch(B(_lZ(_me,_mj-1|0))){case 0:return new F(function(){return _jc(_mk,_mc-_md|0);});break;case 1:if(!(B(_jo(_mk))&1)){return new F(function(){return _jc(_mk,_mc-_md|0);});}else{return new F(function(){return _jc(B(_jr(_mk,_fm)),_mc-_md|0);});}break;default:return new F(function(){return _jc(B(_jr(_mk,_fm)),_mc-_md|0);});}}}else{return new F(function(){return _jc(_me,(_mc-_md|0)-_mj|0);});}}else{if(_mi>=_md){var _ml=B(_m7(_me,(_mi+1|0)-_md|0));switch(B(_lZ(_me,_mi-_md|0))){case 0:return new F(function(){return _jc(_ml,((_mi-_mh|0)+1|0)-_md|0);});break;case 2:return new F(function(){return _jc(B(_jr(_ml,_fm)),((_mi-_mh|0)+1|0)-_md|0);});break;default:if(!(B(_jo(_ml))&1)){return new F(function(){return _jc(_ml,((_mi-_mh|0)+1|0)-_md|0);});}else{return new F(function(){return _jc(B(_jr(_ml,_fm)),((_mi-_mh|0)+1|0)-_md|0);});}}}else{return new F(function(){return _jc(_me, -_mh);});}}}else{var _mm=B(_la(_me))-_mh|0,_mn=function(_mo){var _mp=function(_mq,_mr){if(!B(_lA(B(_g6(_mr,_md)),_mq))){return new F(function(){return _jK(_mo-_md|0,_mq,_mr);});}else{return new F(function(){return _jK((_mo-_md|0)+1|0,_mq,B(_g6(_mr,1)));});}};if(_mo>=_md){if(_mo!=_md){return new F(function(){return _mp(_me,new T(function(){return B(_g6(_mf,_mo-_md|0));}));});}else{return new F(function(){return _mp(_me,_mf);});}}else{return new F(function(){return _mp(new T(function(){return B(_g6(_me,_md-_mo|0));}),_mf);});}};if(_mc>_mm){return new F(function(){return _mn(_mc);});}else{return new F(function(){return _mn(_mm);});}}},_ms=[0,2147483647],_mt=new T(function(){return B(_jr(_ms,_jY));}),_mu=function(_mv){var _mw=E(_mv);if(!_mw[0]){var _mx=E(_mw[1]);return (_mx==(-2147483648))?E(_mt):[0, -_mx];}else{return [1,I_negate(_mw[1])];}},_my=new T(function(){return 0/0;}),_mz=new T(function(){return -1/0;}),_mA=new T(function(){return 1/0;}),_mB=0,_mC=function(_mD,_mE){if(!B(_jg(_mE,_jJ))){if(!B(_jg(_mD,_jJ))){if(!B(_jZ(_mD,_jJ))){return new F(function(){return _mb(-1021,53,_mD,_mE);});}else{return  -B(_mb(-1021,53,B(_mu(_mD)),_mE));}}else{return E(_mB);}}else{return (!B(_jg(_mD,_jJ)))?(!B(_jZ(_mD,_jJ)))?E(_mA):E(_mz):E(_my);}},_mF=function(_mG,_mH){while(1){var _mI=B((function(_mJ,_mK){var _mL=E(_mK);if(!_mL[0]){_mG=[1,[0,_mL[2],_mL[3]],new T(function(){return B(_mF(_mJ,_mL[5]));})];_mH=_mL[4];return null;}else{return E(_mJ);}})(_mG,_mH));if(_mI!=null){return _mI;}}},_mM=function(_mN){return new F(function(){return _mF(_2,_mN);});},_mO=function(_mP,_mQ,_mR,_mS,_mT){if(!B(_gi(_mQ,0))){return new F(function(){return _mM(B(_hU(_3q,_mP,new T(function(){return B(_78(_mT));})))[2]);});}else{var _mU=new T(function(){return B(_17(_mQ,_a3));}),_mV=new T(function(){return B(_8U(_3q,_7m,[0,1,E(_mU),_9U,E(_3r),E(_3r)],_9L));}),_mW=new T(function(){var _mX=B(_ga(E(_mS)));return B(_mC(_mX[1],_mX[2]));}),_mY=function(_mZ){return [0,new T(function(){return B(_9N(_3q,_9W,_mU,_mZ))+ -E(_mW);}),_mV];},_n0=new T(function(){return B(_17(_mQ,_9M));}),_n1=new T(function(){return B(_8U(_3q,_7m,[0,1,E(_n0),_9U,E(_3r),E(_3r)],_9L));}),_n2=new T(function(){var _n3=B(_ga(E(_mR)));return B(_mC(_n3[1],_n3[2]));}),_n4=function(_n5){return [0,new T(function(){return B(_9N(_3q,_9W,_n0,_n5))+ -E(_n2);}),_n1];};return new F(function(){return _mM(B(_hU(_3q,[1,_n4,[1,_mY,_mP]],new T(function(){return B(_78(_mT));})))[2]);});}},_n6=new T(function(){var _n7=E(_aY);return E(_aZ);}),_n8=function(_n9){return [0,new T(function(){return B(_9N(_3q,_9W,_aY,_n9));}),_n6];},_na=0.8,_nb=[0,_na,_3r],_nc=function(_nd){return E(_nb);},_ne=function(_nf){var _ng=new T(function(){var _nh=B(_9x(_3q,_nc,_n8,_nf));return [0,_nh[1],_nh[2]];});return [0,new T(function(){return B(_9N(_3q,_9W,_bf,_nf))+ -B(_9N(_3q,_9W,_bc,_nf))+ -E(E(_ng)[1]);}),new T(function(){var _ni=E(_bf),_nj=E(_bc);return B(_8U(_3q,_7m,_bh,B(_7q(_9J,E(_ng)[2]))));})];},_nk=function(_nl){var _nm=B(_ne(_nl));return [0,_nm[1],_nm[2]];},_nn=[1,_nk,_2],_no=new T(function(){var _np=E(_bc);return B(_7q(_9J,_bd));}),_nq=function(_nr){return [0,new T(function(){return  -B(_9N(_3q,_9W,_bc,_nr));}),_no];},_ns=function(_nt){var _nu=new T(function(){var _nv=B(_9x(_3q,_nc,_nq,_nt));return [0,_nv[1],_nv[2]];});return [0,new T(function(){return B(_9N(_3q,_9W,_b1,_nt))+ -B(_9N(_3q,_9W,_aY,_nt))+ -E(E(_nu)[1]);}),new T(function(){var _nw=E(_b1),_nx=E(_aY);return B(_8U(_3q,_7m,_b3,B(_7q(_9J,E(_nu)[2]))));})];},_ny=function(_nz){var _nA=B(_ns(_nz));return [0,_nA[1],_nA[2]];},_nB=[1,_ny,_nn],_nC=function(_nD){var _nE=new T(function(){var _nF=B(_9x(_3q,_bE,_bE,_nD));return [0,_nF[1],_nF[2]];}),_nG=new T(function(){var _nH=B(_9x(_3q,_bB,_bB,_nD));return [0,_nH[1],_nH[2]];});return [0,new T(function(){return E(E(_nG)[1])+E(E(_nE)[1])+(-1);}),new T(function(){return B(_8U(_3q,_7m,B(_8U(_3q,_7m,E(_nG)[2],E(_nE)[2])),_9L));})];},_nI=function(_nJ){var _nK=B(_nC(_nJ));return [0,_nK[1],_nK[2]];},_nL=[1,_nI,_nB],_nM=function(_nN){var _nO=new T(function(){var _nP=B(_9x(_3q,_aa,_aa,_nN));return [0,_nP[1],_nP[2]];}),_nQ=new T(function(){var _nR=B(_9x(_3q,_aH,_aH,_nN));return [0,_nR[1],_nR[2]];});return [0,new T(function(){return E(E(_nQ)[1])+E(E(_nO)[1]);}),new T(function(){return B(_8U(_3q,_7m,E(_nQ)[2],E(_nO)[2]));})];},_nS=function(_nT){var _nU=B(_nM(_nT));return [0,_nU[1],_nU[2]];},_nV=function(_nW,_nX,_nY){var _nZ=function(_o0){var _o1=E(_o0);if(!_o1[0]){return [0];}else{var _o2=_o1[1];return [1,_o2,new T(function(){return B(_nZ(B(_hs(_nW,_nX,_o2))));})];}},_o3=_nY;return [1,_o3,new T(function(){return B(_nZ(B(_hs(_nW,_nX,_o3))));})];},_o4=function(_o5){return [0,new T(function(){return B(_cV(_9M,_o5));}),new T(function(){return B(_cV(_a3,_o5));})];},_o6=function(_o7,_o8){return new F(function(){return _d7(_o4,B(_nV(_3q,_nS,new T(function(){return B(_78([1,[0,_9M,_o7],[1,[0,_a3,_o8],_2]]));}))));});},_o9=function(_){var _oa=function(_ob){var _oc=new T(function(){var _od=B(A(_ob,[_]));return E(_od);}),_oe=function(_of){return new F(function(){return _cZ(_oc,new T(function(){var _og=B(A(_of,[_]));return E(_og);}));});};return E(_oe);},_oh=__createJSFunc(2,E(_oa)),_oi=E(_cN)("gradX",_oh),_oj=function(_ok){var _ol=new T(function(){var _om=B(A(_ok,[_]));return E(_om);}),_on=function(_oo){return new F(function(){return _d3(_ol,new T(function(){var _op=B(A(_oo,[_]));return E(_op);}));});};return E(_on);},_oq=__createJSFunc(2,E(_oj)),_or=E(_cM)("gradY",_oq),_os=function(_ot){var _ou=new T(function(){var _ov=B(A(_ot,[_]));return E(_ov);}),_ow=function(_ox){return new F(function(){return _aK(_ou,new T(function(){var _oy=B(A(_ox,[_]));return E(_oy);}));});};return E(_ow);},_oz=__createJSFunc(2,E(_os)),_oA=E(_cL)("val",_oz),_oB=function(_oC){var _oD=new T(function(){var _oE=B(A(_oC,[_]));return E(_oE);}),_oF=function(_oG){return new F(function(){return __lst2arr(B(_d7(_db,B(_o6(_oD,new T(function(){var _oH=B(A(_oG,[_]));return E(_oH);}))))));});};return E(_oF);},_oI=__createJSFunc(2,E(_oB)),_oJ=E(_cS)("trace",_oI),_oK=function(_oL){var _oM=new T(function(){var _oN=String(E(_oL));return fromJSStr(_oN);}),_oO=function(_oP){var _oQ=new T(function(){var _oR=B(A(_oP,[_]));return E(_oR);}),_oS=function(_oT){var _oU=new T(function(){var _oV=B(A(_oT,[_]));return E(_oV);}),_oW=function(_oX){return new F(function(){return __lst2arr(B(_d7(_de,B(_mO(_bP,_oM,_oQ,_oU,new T(function(){var _oY=B(_2I(_E,_f,E(_oX),_));return E(_oY);},1))))));});};return E(_oW);};return E(_oS);};return E(_oO);},_oZ=__createJSFunc(4,E(_oK)),_p0=E(_cR)("arm",_oZ),_p1=function(_p2){var _p3=new T(function(){var _p4=String(E(_p2));return fromJSStr(_p4);}),_p5=function(_p6){var _p7=new T(function(){var _p8=B(A(_p6,[_]));return E(_p8);}),_p9=function(_pa){var _pb=new T(function(){var _pc=B(A(_pa,[_]));return E(_pc);}),_pd=function(_pe){return new F(function(){return __lst2arr(B(_d7(_de,B(_mO(_nL,_p3,_p7,_pb,new T(function(){var _pf=B(_2I(_E,_f,E(_pe),_));return E(_pf);},1))))));});};return E(_pd);};return E(_p9);};return E(_p5);},_pg=__createJSFunc(4,E(_p1)),_ph=E(_cQ)("tangent",_pg),_pi=function(_pj){var _pk=new T(function(){var _pl=String(E(_pj));return fromJSStr(_pl);}),_pm=function(_pn){var _po=new T(function(){var _pp=B(A(_pn,[_]));return E(_pp);}),_pq=function(_pr){var _ps=new T(function(){var _pt=B(A(_pr,[_]));return E(_pt);}),_pu=function(_pv){return new F(function(){return __lst2arr(B(_d7(_de,B(_mO(_fl,_pk,_po,_ps,new T(function(){var _pw=B(_2I(_E,_f,E(_pv),_));return E(_pw);},1))))));});};return E(_pu);};return E(_pq);};return E(_pm);},_px=__createJSFunc(4,E(_pi)),_py=E(_cP)("perp",_px),_pz=function(_pA){var _pB=new T(function(){var _pC=String(E(_pA));return fromJSStr(_pC);}),_pD=function(_pE){var _pF=new T(function(){var _pG=B(A(_pE,[_]));return E(_pG);}),_pH=function(_pI){var _pJ=new T(function(){var _pK=B(A(_pI,[_]));return E(_pK);}),_pL=function(_pM){return new F(function(){return __lst2arr(B(_d7(_de,B(_mO(_cK,_pB,_pF,_pJ,new T(function(){var _pN=B(_2I(_E,_f,E(_pM),_));return E(_pN);},1))))));});};return E(_pL);};return E(_pH);};return E(_pD);},_pO=__createJSFunc(4,E(_pz)),_pP=E(_cO)("demo",_pO);return new F(function(){return _1(_);});},_pQ=function(_){return new F(function(){return _o9(_);});};
var hasteMain = function() {B(A(_pQ, [0]));};hasteMain(); init();