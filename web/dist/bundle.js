/*
 * ATTENTION: The "eval" devtool has been used (maybe by default in mode: "development").
 * This devtool is neither made for production nor for readable output files.
 * It uses "eval()" calls to create a separate source file in the browser devtools.
 * If you are trying to read the output file, select a different devtool (https://webpack.js.org/configuration/devtool/)
 * or disable the default devtool with "devtool: false".
 * If you are looking for production-ready output files, see mode: "production" (https://webpack.js.org/configuration/mode/).
 */
/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ "./pkg/compile_wasm.js":
/*!*****************************!*\
  !*** ./pkg/compile_wasm.js ***!
  \*****************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony export */ __webpack_require__.d(__webpack_exports__, {\n/* harmony export */   compile_to_wasm: () => (/* binding */ compile_to_wasm),\n/* harmony export */   compile_to_x86: () => (/* binding */ compile_to_x86),\n/* harmony export */   \"default\": () => (__WEBPACK_DEFAULT_EXPORT__),\n/* harmony export */   initSync: () => (/* binding */ initSync)\n/* harmony export */ });\nfunction _typeof(o) { \"@babel/helpers - typeof\"; return _typeof = \"function\" == typeof Symbol && \"symbol\" == typeof Symbol.iterator ? function (o) { return typeof o; } : function (o) { return o && \"function\" == typeof Symbol && o.constructor === Symbol && o !== Symbol.prototype ? \"symbol\" : typeof o; }, _typeof(o); }\nfunction _regeneratorRuntime() { \"use strict\"; /*! regenerator-runtime -- Copyright (c) 2014-present, Facebook, Inc. -- license (MIT): https://github.com/facebook/regenerator/blob/main/LICENSE */ _regeneratorRuntime = function _regeneratorRuntime() { return e; }; var t, e = {}, r = Object.prototype, n = r.hasOwnProperty, o = Object.defineProperty || function (t, e, r) { t[e] = r.value; }, i = \"function\" == typeof Symbol ? Symbol : {}, a = i.iterator || \"@@iterator\", c = i.asyncIterator || \"@@asyncIterator\", u = i.toStringTag || \"@@toStringTag\"; function define(t, e, r) { return Object.defineProperty(t, e, { value: r, enumerable: !0, configurable: !0, writable: !0 }), t[e]; } try { define({}, \"\"); } catch (t) { define = function define(t, e, r) { return t[e] = r; }; } function wrap(t, e, r, n) { var i = e && e.prototype instanceof Generator ? e : Generator, a = Object.create(i.prototype), c = new Context(n || []); return o(a, \"_invoke\", { value: makeInvokeMethod(t, r, c) }), a; } function tryCatch(t, e, r) { try { return { type: \"normal\", arg: t.call(e, r) }; } catch (t) { return { type: \"throw\", arg: t }; } } e.wrap = wrap; var h = \"suspendedStart\", l = \"suspendedYield\", f = \"executing\", s = \"completed\", y = {}; function Generator() {} function GeneratorFunction() {} function GeneratorFunctionPrototype() {} var p = {}; define(p, a, function () { return this; }); var d = Object.getPrototypeOf, v = d && d(d(values([]))); v && v !== r && n.call(v, a) && (p = v); var g = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(p); function defineIteratorMethods(t) { [\"next\", \"throw\", \"return\"].forEach(function (e) { define(t, e, function (t) { return this._invoke(e, t); }); }); } function AsyncIterator(t, e) { function invoke(r, o, i, a) { var c = tryCatch(t[r], t, o); if (\"throw\" !== c.type) { var u = c.arg, h = u.value; return h && \"object\" == _typeof(h) && n.call(h, \"__await\") ? e.resolve(h.__await).then(function (t) { invoke(\"next\", t, i, a); }, function (t) { invoke(\"throw\", t, i, a); }) : e.resolve(h).then(function (t) { u.value = t, i(u); }, function (t) { return invoke(\"throw\", t, i, a); }); } a(c.arg); } var r; o(this, \"_invoke\", { value: function value(t, n) { function callInvokeWithMethodAndArg() { return new e(function (e, r) { invoke(t, n, e, r); }); } return r = r ? r.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg(); } }); } function makeInvokeMethod(e, r, n) { var o = h; return function (i, a) { if (o === f) throw Error(\"Generator is already running\"); if (o === s) { if (\"throw\" === i) throw a; return { value: t, done: !0 }; } for (n.method = i, n.arg = a;;) { var c = n.delegate; if (c) { var u = maybeInvokeDelegate(c, n); if (u) { if (u === y) continue; return u; } } if (\"next\" === n.method) n.sent = n._sent = n.arg;else if (\"throw\" === n.method) { if (o === h) throw o = s, n.arg; n.dispatchException(n.arg); } else \"return\" === n.method && n.abrupt(\"return\", n.arg); o = f; var p = tryCatch(e, r, n); if (\"normal\" === p.type) { if (o = n.done ? s : l, p.arg === y) continue; return { value: p.arg, done: n.done }; } \"throw\" === p.type && (o = s, n.method = \"throw\", n.arg = p.arg); } }; } function maybeInvokeDelegate(e, r) { var n = r.method, o = e.iterator[n]; if (o === t) return r.delegate = null, \"throw\" === n && e.iterator[\"return\"] && (r.method = \"return\", r.arg = t, maybeInvokeDelegate(e, r), \"throw\" === r.method) || \"return\" !== n && (r.method = \"throw\", r.arg = new TypeError(\"The iterator does not provide a '\" + n + \"' method\")), y; var i = tryCatch(o, e.iterator, r.arg); if (\"throw\" === i.type) return r.method = \"throw\", r.arg = i.arg, r.delegate = null, y; var a = i.arg; return a ? a.done ? (r[e.resultName] = a.value, r.next = e.nextLoc, \"return\" !== r.method && (r.method = \"next\", r.arg = t), r.delegate = null, y) : a : (r.method = \"throw\", r.arg = new TypeError(\"iterator result is not an object\"), r.delegate = null, y); } function pushTryEntry(t) { var e = { tryLoc: t[0] }; 1 in t && (e.catchLoc = t[1]), 2 in t && (e.finallyLoc = t[2], e.afterLoc = t[3]), this.tryEntries.push(e); } function resetTryEntry(t) { var e = t.completion || {}; e.type = \"normal\", delete e.arg, t.completion = e; } function Context(t) { this.tryEntries = [{ tryLoc: \"root\" }], t.forEach(pushTryEntry, this), this.reset(!0); } function values(e) { if (e || \"\" === e) { var r = e[a]; if (r) return r.call(e); if (\"function\" == typeof e.next) return e; if (!isNaN(e.length)) { var o = -1, i = function next() { for (; ++o < e.length;) if (n.call(e, o)) return next.value = e[o], next.done = !1, next; return next.value = t, next.done = !0, next; }; return i.next = i; } } throw new TypeError(_typeof(e) + \" is not iterable\"); } return GeneratorFunction.prototype = GeneratorFunctionPrototype, o(g, \"constructor\", { value: GeneratorFunctionPrototype, configurable: !0 }), o(GeneratorFunctionPrototype, \"constructor\", { value: GeneratorFunction, configurable: !0 }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, u, \"GeneratorFunction\"), e.isGeneratorFunction = function (t) { var e = \"function\" == typeof t && t.constructor; return !!e && (e === GeneratorFunction || \"GeneratorFunction\" === (e.displayName || e.name)); }, e.mark = function (t) { return Object.setPrototypeOf ? Object.setPrototypeOf(t, GeneratorFunctionPrototype) : (t.__proto__ = GeneratorFunctionPrototype, define(t, u, \"GeneratorFunction\")), t.prototype = Object.create(g), t; }, e.awrap = function (t) { return { __await: t }; }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, c, function () { return this; }), e.AsyncIterator = AsyncIterator, e.async = function (t, r, n, o, i) { void 0 === i && (i = Promise); var a = new AsyncIterator(wrap(t, r, n, o), i); return e.isGeneratorFunction(r) ? a : a.next().then(function (t) { return t.done ? t.value : a.next(); }); }, defineIteratorMethods(g), define(g, u, \"Generator\"), define(g, a, function () { return this; }), define(g, \"toString\", function () { return \"[object Generator]\"; }), e.keys = function (t) { var e = Object(t), r = []; for (var n in e) r.push(n); return r.reverse(), function next() { for (; r.length;) { var t = r.pop(); if (t in e) return next.value = t, next.done = !1, next; } return next.done = !0, next; }; }, e.values = values, Context.prototype = { constructor: Context, reset: function reset(e) { if (this.prev = 0, this.next = 0, this.sent = this._sent = t, this.done = !1, this.delegate = null, this.method = \"next\", this.arg = t, this.tryEntries.forEach(resetTryEntry), !e) for (var r in this) \"t\" === r.charAt(0) && n.call(this, r) && !isNaN(+r.slice(1)) && (this[r] = t); }, stop: function stop() { this.done = !0; var t = this.tryEntries[0].completion; if (\"throw\" === t.type) throw t.arg; return this.rval; }, dispatchException: function dispatchException(e) { if (this.done) throw e; var r = this; function handle(n, o) { return a.type = \"throw\", a.arg = e, r.next = n, o && (r.method = \"next\", r.arg = t), !!o; } for (var o = this.tryEntries.length - 1; o >= 0; --o) { var i = this.tryEntries[o], a = i.completion; if (\"root\" === i.tryLoc) return handle(\"end\"); if (i.tryLoc <= this.prev) { var c = n.call(i, \"catchLoc\"), u = n.call(i, \"finallyLoc\"); if (c && u) { if (this.prev < i.catchLoc) return handle(i.catchLoc, !0); if (this.prev < i.finallyLoc) return handle(i.finallyLoc); } else if (c) { if (this.prev < i.catchLoc) return handle(i.catchLoc, !0); } else { if (!u) throw Error(\"try statement without catch or finally\"); if (this.prev < i.finallyLoc) return handle(i.finallyLoc); } } } }, abrupt: function abrupt(t, e) { for (var r = this.tryEntries.length - 1; r >= 0; --r) { var o = this.tryEntries[r]; if (o.tryLoc <= this.prev && n.call(o, \"finallyLoc\") && this.prev < o.finallyLoc) { var i = o; break; } } i && (\"break\" === t || \"continue\" === t) && i.tryLoc <= e && e <= i.finallyLoc && (i = null); var a = i ? i.completion : {}; return a.type = t, a.arg = e, i ? (this.method = \"next\", this.next = i.finallyLoc, y) : this.complete(a); }, complete: function complete(t, e) { if (\"throw\" === t.type) throw t.arg; return \"break\" === t.type || \"continue\" === t.type ? this.next = t.arg : \"return\" === t.type ? (this.rval = this.arg = t.arg, this.method = \"return\", this.next = \"end\") : \"normal\" === t.type && e && (this.next = e), y; }, finish: function finish(t) { for (var e = this.tryEntries.length - 1; e >= 0; --e) { var r = this.tryEntries[e]; if (r.finallyLoc === t) return this.complete(r.completion, r.afterLoc), resetTryEntry(r), y; } }, \"catch\": function _catch(t) { for (var e = this.tryEntries.length - 1; e >= 0; --e) { var r = this.tryEntries[e]; if (r.tryLoc === t) { var n = r.completion; if (\"throw\" === n.type) { var o = n.arg; resetTryEntry(r); } return o; } } throw Error(\"illegal catch attempt\"); }, delegateYield: function delegateYield(e, r, n) { return this.delegate = { iterator: values(e), resultName: r, nextLoc: n }, \"next\" === this.method && (this.arg = t), y; } }, e; }\nfunction asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { Promise.resolve(value).then(_next, _throw); } }\nfunction _asyncToGenerator(fn) { return function () { var self = this, args = arguments; return new Promise(function (resolve, reject) { var gen = fn.apply(self, args); function _next(value) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, \"next\", value); } function _throw(err) { asyncGeneratorStep(gen, resolve, reject, _next, _throw, \"throw\", err); } _next(undefined); }); }; }\nvar wasm;\nvar WASM_VECTOR_LEN = 0;\nvar cachedUint8Memory0 = null;\nfunction getUint8Memory0() {\n  if (cachedUint8Memory0 === null || cachedUint8Memory0.byteLength === 0) {\n    cachedUint8Memory0 = new Uint8Array(wasm.memory.buffer);\n  }\n  return cachedUint8Memory0;\n}\nvar cachedTextEncoder = typeof TextEncoder !== 'undefined' ? new TextEncoder('utf-8') : {\n  encode: function encode() {\n    throw Error('TextEncoder not available');\n  }\n};\nvar encodeString = typeof cachedTextEncoder.encodeInto === 'function' ? function (arg, view) {\n  return cachedTextEncoder.encodeInto(arg, view);\n} : function (arg, view) {\n  var buf = cachedTextEncoder.encode(arg);\n  view.set(buf);\n  return {\n    read: arg.length,\n    written: buf.length\n  };\n};\nfunction passStringToWasm0(arg, malloc, realloc) {\n  if (realloc === undefined) {\n    var buf = cachedTextEncoder.encode(arg);\n    var _ptr = malloc(buf.length, 1) >>> 0;\n    getUint8Memory0().subarray(_ptr, _ptr + buf.length).set(buf);\n    WASM_VECTOR_LEN = buf.length;\n    return _ptr;\n  }\n  var len = arg.length;\n  var ptr = malloc(len, 1) >>> 0;\n  var mem = getUint8Memory0();\n  var offset = 0;\n  for (; offset < len; offset++) {\n    var code = arg.charCodeAt(offset);\n    if (code > 0x7F) break;\n    mem[ptr + offset] = code;\n  }\n  if (offset !== len) {\n    if (offset !== 0) {\n      arg = arg.slice(offset);\n    }\n    ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;\n    var view = getUint8Memory0().subarray(ptr + offset, ptr + len);\n    var ret = encodeString(arg, view);\n    offset += ret.written;\n    ptr = realloc(ptr, len, offset, 1) >>> 0;\n  }\n  WASM_VECTOR_LEN = offset;\n  return ptr;\n}\nvar cachedInt32Memory0 = null;\nfunction getInt32Memory0() {\n  if (cachedInt32Memory0 === null || cachedInt32Memory0.byteLength === 0) {\n    cachedInt32Memory0 = new Int32Array(wasm.memory.buffer);\n  }\n  return cachedInt32Memory0;\n}\nvar cachedTextDecoder = typeof TextDecoder !== 'undefined' ? new TextDecoder('utf-8', {\n  ignoreBOM: true,\n  fatal: true\n}) : {\n  decode: function decode() {\n    throw Error('TextDecoder not available');\n  }\n};\nif (typeof TextDecoder !== 'undefined') {\n  cachedTextDecoder.decode();\n}\n;\nfunction getStringFromWasm0(ptr, len) {\n  ptr = ptr >>> 0;\n  return cachedTextDecoder.decode(getUint8Memory0().subarray(ptr, ptr + len));\n}\n/**\n* @param {string} header_file\n* @param {string} main_file\n* @returns {string}\n*/\nfunction compile_to_wasm(header_file, main_file) {\n  var deferred3_0;\n  var deferred3_1;\n  try {\n    var retptr = wasm.__wbindgen_add_to_stack_pointer(-16);\n    var ptr0 = passStringToWasm0(header_file, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);\n    var len0 = WASM_VECTOR_LEN;\n    var ptr1 = passStringToWasm0(main_file, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);\n    var len1 = WASM_VECTOR_LEN;\n    wasm.compile_to_wasm(retptr, ptr0, len0, ptr1, len1);\n    var r0 = getInt32Memory0()[retptr / 4 + 0];\n    var r1 = getInt32Memory0()[retptr / 4 + 1];\n    deferred3_0 = r0;\n    deferred3_1 = r1;\n    return getStringFromWasm0(r0, r1);\n  } finally {\n    wasm.__wbindgen_add_to_stack_pointer(16);\n    wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);\n  }\n}\n\n/**\n* @param {string} header_file\n* @param {string} main_file\n* @returns {string}\n*/\nfunction compile_to_x86(header_file, main_file) {\n  var deferred3_0;\n  var deferred3_1;\n  try {\n    var retptr = wasm.__wbindgen_add_to_stack_pointer(-16);\n    var ptr0 = passStringToWasm0(header_file, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);\n    var len0 = WASM_VECTOR_LEN;\n    var ptr1 = passStringToWasm0(main_file, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);\n    var len1 = WASM_VECTOR_LEN;\n    wasm.compile_to_x86(retptr, ptr0, len0, ptr1, len1);\n    var r0 = getInt32Memory0()[retptr / 4 + 0];\n    var r1 = getInt32Memory0()[retptr / 4 + 1];\n    deferred3_0 = r0;\n    deferred3_1 = r1;\n    return getStringFromWasm0(r0, r1);\n  } finally {\n    wasm.__wbindgen_add_to_stack_pointer(16);\n    wasm.__wbindgen_free(deferred3_0, deferred3_1, 1);\n  }\n}\nfunction __wbg_load(_x, _x2) {\n  return _wbg_load.apply(this, arguments);\n}\nfunction _wbg_load() {\n  _wbg_load = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee(module, imports) {\n    var bytes, instance;\n    return _regeneratorRuntime().wrap(function _callee$(_context) {\n      while (1) switch (_context.prev = _context.next) {\n        case 0:\n          if (!(typeof Response === 'function' && module instanceof Response)) {\n            _context.next = 23;\n            break;\n          }\n          if (!(typeof WebAssembly.instantiateStreaming === 'function')) {\n            _context.next = 15;\n            break;\n          }\n          _context.prev = 2;\n          _context.next = 5;\n          return WebAssembly.instantiateStreaming(module, imports);\n        case 5:\n          return _context.abrupt(\"return\", _context.sent);\n        case 8:\n          _context.prev = 8;\n          _context.t0 = _context[\"catch\"](2);\n          if (!(module.headers.get('Content-Type') != 'application/wasm')) {\n            _context.next = 14;\n            break;\n          }\n          console.warn(\"`WebAssembly.instantiateStreaming` failed because your server does not serve wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\\n\", _context.t0);\n          _context.next = 15;\n          break;\n        case 14:\n          throw _context.t0;\n        case 15:\n          _context.next = 17;\n          return module.arrayBuffer();\n        case 17:\n          bytes = _context.sent;\n          _context.next = 20;\n          return WebAssembly.instantiate(bytes, imports);\n        case 20:\n          return _context.abrupt(\"return\", _context.sent);\n        case 23:\n          _context.next = 25;\n          return WebAssembly.instantiate(module, imports);\n        case 25:\n          instance = _context.sent;\n          if (!(instance instanceof WebAssembly.Instance)) {\n            _context.next = 30;\n            break;\n          }\n          return _context.abrupt(\"return\", {\n            instance: instance,\n            module: module\n          });\n        case 30:\n          return _context.abrupt(\"return\", instance);\n        case 31:\n        case \"end\":\n          return _context.stop();\n      }\n    }, _callee, null, [[2, 8]]);\n  }));\n  return _wbg_load.apply(this, arguments);\n}\nfunction __wbg_get_imports() {\n  var imports = {};\n  imports.wbg = {};\n  return imports;\n}\nfunction __wbg_init_memory(imports, maybe_memory) {}\nfunction __wbg_finalize_init(instance, module) {\n  wasm = instance.exports;\n  __wbg_init.__wbindgen_wasm_module = module;\n  cachedInt32Memory0 = null;\n  cachedUint8Memory0 = null;\n  return wasm;\n}\nfunction initSync(module) {\n  if (wasm !== undefined) return wasm;\n  var imports = __wbg_get_imports();\n  __wbg_init_memory(imports);\n  if (!(module instanceof WebAssembly.Module)) {\n    module = new WebAssembly.Module(module);\n  }\n  var instance = new WebAssembly.Instance(module, imports);\n  return __wbg_finalize_init(instance, module);\n}\nfunction __wbg_init(_x3) {\n  return _wbg_init.apply(this, arguments);\n}\nfunction _wbg_init() {\n  _wbg_init = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee2(input) {\n    var imports, _yield$__wbg_load, instance, module;\n    return _regeneratorRuntime().wrap(function _callee2$(_context2) {\n      while (1) switch (_context2.prev = _context2.next) {\n        case 0:\n          if (!(wasm !== undefined)) {\n            _context2.next = 2;\n            break;\n          }\n          return _context2.abrupt(\"return\", wasm);\n        case 2:\n          if (typeof input === 'undefined') {\n            input = new URL(/* asset import */ __webpack_require__(/*! compile_wasm_bg.wasm */ \"./pkg/compile_wasm_bg.wasm\"), __webpack_require__.b);\n          }\n          imports = __wbg_get_imports();\n          if (typeof input === 'string' || typeof Request === 'function' && input instanceof Request || typeof URL === 'function' && input instanceof URL) {\n            input = fetch(input);\n          }\n          __wbg_init_memory(imports);\n          _context2.t0 = __wbg_load;\n          _context2.next = 9;\n          return input;\n        case 9:\n          _context2.t1 = _context2.sent;\n          _context2.t2 = imports;\n          _context2.next = 13;\n          return (0, _context2.t0)(_context2.t1, _context2.t2);\n        case 13:\n          _yield$__wbg_load = _context2.sent;\n          instance = _yield$__wbg_load.instance;\n          module = _yield$__wbg_load.module;\n          return _context2.abrupt(\"return\", __wbg_finalize_init(instance, module));\n        case 17:\n        case \"end\":\n          return _context2.stop();\n      }\n    }, _callee2);\n  }));\n  return _wbg_init.apply(this, arguments);\n}\n\n/* harmony default export */ const __WEBPACK_DEFAULT_EXPORT__ = (__wbg_init);\n\n//# sourceURL=webpack://web/./pkg/compile_wasm.js?");

/***/ }),

/***/ "./src/index.js":
/*!**********************!*\
  !*** ./src/index.js ***!
  \**********************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
eval("__webpack_require__.r(__webpack_exports__);\n/* harmony import */ var wabt__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! wabt */ \"./node_modules/wabt/index.js\");\n/* harmony import */ var wabt__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(wabt__WEBPACK_IMPORTED_MODULE_0__);\n/* harmony import */ var _pkg_compile_wasm_js__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ../pkg/compile_wasm.js */ \"./pkg/compile_wasm.js\");\n\n\n(0,_pkg_compile_wasm_js__WEBPACK_IMPORTED_MODULE_1__[\"default\"])();\nfunction loadAndInstantiateWAT() {\n  return fetch(\"./dist/alloc.wat\").then(function (response) {\n    if (!response.ok) {\n      throw new Error(\"Network response was not ok \" + response.statusText);\n    }\n    return response.text();\n  }).then(function (watText) {\n    return wabt__WEBPACK_IMPORTED_MODULE_0___default()().then(function (wabtInstance) {\n      var parsedModule = wabtInstance.parseWat(\"\", watText);\n      var binaryModule = parsedModule.toBinary({});\n      return WebAssembly.instantiate(binaryModule.buffer);\n    });\n  })[\"catch\"](function (error) {\n    console.error(\"Error in loading or instantiating the wat file:\", error);\n  });\n}\nfunction handleSubmit() {\n  var input = document.getElementById(\"edit-container\").value;\n  var output = (0,_pkg_compile_wasm_js__WEBPACK_IMPORTED_MODULE_1__.compile_to_wasm)(\"\", input);\n  var x64_output = (0,_pkg_compile_wasm_js__WEBPACK_IMPORTED_MODULE_1__.compile_to_x86)(\"\", input);\n  loadAndInstantiateWAT().then(function (allocModule) {\n    document.getElementById(\"wasm\").textContent = output;\n    wabt__WEBPACK_IMPORTED_MODULE_0___default()().then(function (wabt) {\n      var module = wabt.parseWat(\"\", output).toBinary({}).buffer;\n      var imports = {\n        alloc: {\n          allocate: allocModule.instance.exports.allocate,\n          memory: allocModule.instance.exports.memory\n        }\n      };\n      WebAssembly.instantiate(module, imports).then(function (result) {\n        var final_result = result.instance.exports.main();\n        document.getElementById(\"result\").textContent = final_result;\n      });\n    });\n  });\n  document.getElementById(\"x86\").textContent = x64_output;\n}\ndocument.addEventListener(\"DOMContentLoaded\", function () {\n  var submitButton = document.getElementById(\"compile-button\");\n  submitButton.addEventListener(\"click\", handleSubmit);\n});\n\n//# sourceURL=webpack://web/./src/index.js?");

/***/ }),

/***/ "./node_modules/wabt/index.js":
/*!************************************!*\
  !*** ./node_modules/wabt/index.js ***!
  \************************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {


/***/ }),

/***/ "./pkg/compile_wasm_bg.wasm":
/*!**********************************!*\
  !*** ./pkg/compile_wasm_bg.wasm ***!
  \**********************************/
/***/ ((module, __unused_webpack_exports, __webpack_require__) => {

"use strict";
eval("module.exports = __webpack_require__.p + \"c8bc94e5e093921035ef.wasm\";\n\n//# sourceURL=webpack://web/./pkg/compile_wasm_bg.wasm?");

/***/ }),

/***/ "?0142":
/*!********************!*\
  !*** fs (ignored) ***!
  \********************/
/***/ (() => {

eval("/* (ignored) */\n\n//# sourceURL=webpack://web/fs_(ignored)?");

/***/ }),

/***/ "?5316":
/*!**********************!*\
  !*** path (ignored) ***!
  \**********************/
/***/ (() => {

eval("/* (ignored) */\n\n//# sourceURL=webpack://web/path_(ignored)?");

/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = __webpack_modules__;
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/compat get default export */
/******/ 	(() => {
/******/ 		// getDefaultExport function for compatibility with non-harmony modules
/******/ 		__webpack_require__.n = (module) => {
/******/ 			var getter = module && module.__esModule ?
/******/ 				() => (module['default']) :
/******/ 				() => (module);
/******/ 			__webpack_require__.d(getter, { a: getter });
/******/ 			return getter;
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/global */
/******/ 	(() => {
/******/ 		__webpack_require__.g = (function() {
/******/ 			if (typeof globalThis === 'object') return globalThis;
/******/ 			try {
/******/ 				return this || new Function('return this')();
/******/ 			} catch (e) {
/******/ 				if (typeof window === 'object') return window;
/******/ 			}
/******/ 		})();
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/publicPath */
/******/ 	(() => {
/******/ 		var scriptUrl;
/******/ 		if (__webpack_require__.g.importScripts) scriptUrl = __webpack_require__.g.location + "";
/******/ 		var document = __webpack_require__.g.document;
/******/ 		if (!scriptUrl && document) {
/******/ 			if (document.currentScript)
/******/ 				scriptUrl = document.currentScript.src;
/******/ 			if (!scriptUrl) {
/******/ 				var scripts = document.getElementsByTagName("script");
/******/ 				if(scripts.length) {
/******/ 					var i = scripts.length - 1;
/******/ 					while (i > -1 && (!scriptUrl || !/^http(s?):/.test(scriptUrl))) scriptUrl = scripts[i--].src;
/******/ 				}
/******/ 			}
/******/ 		}
/******/ 		// When supporting browsers where an automatic publicPath is not supported you must specify an output.publicPath manually via configuration
/******/ 		// or pass an empty string ("") and set the __webpack_public_path__ variable from your code to use your own logic.
/******/ 		if (!scriptUrl) throw new Error("Automatic publicPath is not supported in this browser");
/******/ 		scriptUrl = scriptUrl.replace(/#.*$/, "").replace(/\?.*$/, "").replace(/\/[^\/]+$/, "/");
/******/ 		__webpack_require__.p = scriptUrl;
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/jsonp chunk loading */
/******/ 	(() => {
/******/ 		__webpack_require__.b = document.baseURI || self.location.href;
/******/ 		
/******/ 		// object to store loaded and loading chunks
/******/ 		// undefined = chunk not loaded, null = chunk preloaded/prefetched
/******/ 		// [resolve, reject, Promise] = chunk loading, 0 = chunk loaded
/******/ 		var installedChunks = {
/******/ 			"main": 0
/******/ 		};
/******/ 		
/******/ 		// no chunk on demand loading
/******/ 		
/******/ 		// no prefetching
/******/ 		
/******/ 		// no preloaded
/******/ 		
/******/ 		// no HMR
/******/ 		
/******/ 		// no HMR manifest
/******/ 		
/******/ 		// no on chunks loaded
/******/ 		
/******/ 		// no jsonp function
/******/ 	})();
/******/ 	
/************************************************************************/
/******/ 	
/******/ 	// startup
/******/ 	// Load entry module and return exports
/******/ 	// This entry module can't be inlined because the eval devtool is used.
/******/ 	var __webpack_exports__ = __webpack_require__("./src/index.js");
/******/ 	
/******/ })()
;