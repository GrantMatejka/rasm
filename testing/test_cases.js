const test_cases = [
  {
    basename: "return_func",
    description: "Testing calling a function returned from wasm",
    callback: (obj) => obj.rasm.toJS(obj.rasm.jsTojs.func()(3)),
    expected: 4,
  },
  {
    basename: "func_as_arg4",
    description:
      "Testing passing a returned function as a parameter to another function",
    callback: (obj) =>
      obj.rasm.toJS(
        obj.rasm.wasmTowasm["apply-adder"](
          obj.rasm.wasmTowasm["make-adder"](obj.rasm.toWasm(12)),
          obj.rasm.toWasm(3)
        )
      ),
    expected: 15,
  },
  {
    basename: "func_as_arg4",
    description:
      "Testing passing a returned function as a parameter to another function with shorthand functions",
    callback: (obj) =>
      obj.rasm.toJS(
        obj.rasm.wasmTowasm["apply-adder"](
          obj.rasm.jsTowasm["make-adder"](12),
          obj.rasm.toWasm(3)
        )
      ),
    expected: 15,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test Basic Arithmetic",
    export: "add1",
    params: [45742],
    expected: 45743,
  },
  {
    basename: "arithmetic_basic",
    description: "Test Basic Arithmetic",
    export: "add1",
    params: [],
    expected: 45742,
  },
  {
    basename: "arithmetic_basic",
    description: "Test Basic Arithmetic",
    export: "add1",
    expected: 45742,
  },
  {
    basename: "case_lambda",
    description: "Test Case Lambda with no args",
    export: "cl1",
    expected: 0,
  },
  {
    basename: "case_lambda",
    description: "Test Case Lambda with 1 arg",
    export: "cl2",
    params: [45],
    expected: 46,
  },
  {
    basename: "case_lambda",
    description: "Test Case Lambda with 2 args",
    export: "cl3",
    params: [12, 45],
    expected: 57,
  },
  {
    basename: "case_lambda",
    description: "Test Case Lambda with 3 args",
    export: "cl4",
    params: [1, 234, 3],
    expected: 238,
  },
  {
    basename: "y_combinator",
    description: "Test Y Combinator",
    export: "ycomb",
    params: [],
    expected: 20,
  },
  {
    basename: "y_combinator5",
    description: "Test Y Combinator",
    export: "fact",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "y_combinator4",
    description: "Test Y Combinator",
    export: "fact",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "y_combinator3",
    description: "Test Y Combinator",
    export: "fact",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "y_combinator2",
    description: "Test Y Combinator",
    export: "fact",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "y_combinator2",
    description: "Test Y Combinator",
    export: "fact2",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "func_as_arg3",
    description: "Test passing function as argument",
    export: "func",
    params: [10],
    expected: 11,
  },
  {
    basename: "func_as_arg2",
    description: "Test passing function as argument",
    export: "func",
    params: [10],
    expected: 11,
  },
  {
    basename: "func_as_arg",
    description: "Test passing function as argument",
    export: "func",
    params: [10],
    expected: 11,
  },
  {
    basename: "recursion",
    description: "Test some recursion method",
    export: "add-fact",
    params: [5, 10],
    expected: 3628920,
  },
  {
    basename: "unique",
    description: "Test some scoping and variable uniquifying",
    export: "test",
    expected: 33,
  },
  {
    basename: "test_03",
    description: "General Test",
    export: "my-fac",
    params: [13],
    expected: 6227020800,
  },
  {
    basename: "test_02",
    description: "General Test",
    export: "my-fac",
    expected: 3628800,
  },
  {
    basename: "test_01",
    description: "General Test",
    export: "f",
    params: [5],
    expected: 25,
  },
  {
    basename: "loop",
    description: "Test loop form",
    export: "my-loop",
    params: [0],
    expected: 10,
  },
  {
    basename: "let",
    description: "Test let form",
    export: "test",
    expected: 39,
  },
  {
    basename: "lam",
    description: "Test lambda form",
    export: "triple",
    params: [33],
    expected: 99,
  },
  {
    basename: "lam_env3",
    description: "Test capturing environment",
    export: "x",
    expected: 7,
  },
  {
    basename: "lam_env2",
    description: "Test capturing environment",
    export: "f",
    expected: 8,
  },
  {
    basename: "lam_env2",
    description: "Test capturing environment",
    export: "x",
    params: [1],
    expected: 9,
  },
  {
    basename: "lam_env",
    description: "Test capturing environment",
    export: "f",
    expected: 8,
  },
  {
    basename: "lam_env",
    description: "Test capturing environment",
    export: "x",
    expected: 8,
  },
  {
    basename: "if",
    description: "Test if form",
    export: "five?",
    params: [5],
    expected: 1,
  },
  {
    basename: "if",
    description: "Test if form",
    export: "five?",
    params: [6],
    expected: 0,
  },
  {
    basename: "if",
    description: "Test if form",
    export: "one?",
    params: [5],
    expected: 0,
  },
  {
    basename: "if",
    description: "Test if form",
    export: "one?",
    params: [1],
    expected: 1,
  },
  {
    basename: "empty",
    description: "Test empty module",
  },
  {
    basename: "cond",
    description: "Test cond form",
    export: "mycond",
    params: [-1],
    expected: 0,
  },
  {
    basename: "cond",
    description: "Test cond form",
    export: "mycond",
    params: [3],
    expected: 1,
  },
  {
    basename: "cond",
    description: "Test cond form",
    export: "mycond",
    params: [6],
    expected: 2,
  },
  {
    basename: "cond",
    description: "Test cond form",
    export: "mycond",
    params: [15],
    expected: 3,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "add1",
    expected: 45742,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "add2",
    expected: 14.7,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "add3",
    expected: 8.5,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "sub1",
    expected: 123400,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "sub2",
    expected: 1.3,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "sub3",
    expected: 1.5,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "mul1",
    expected: 8000,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "mul2",
    expected: 26.25,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "mul3",
    expected: 25.0,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div1",
    expected: 2250,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div2",
    expected: 47.421052631578945,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div3",
    expected: 1.25,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div4",
    expected: 5.0,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div5",
    expected: 5.0,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div6",
    expected: 2.5,
  },
  {
    basename: "arithmetic_basic",
    description: "Test basic arithmetic",
    export: "div7",
    expected: 2.0,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "add1",
    expected: 50752,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "add2",
    expected: 99.7,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "add3",
    expected: 65.2,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "sub1",
    expected: 123410,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "sub2",
    expected: 0.7,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "sub3",
    expected: 46.05,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "mul1",
    expected: 360000,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "mul2",
    expected: 17.0625,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "mul3",
    expected: 1415.925,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "div1",
    expected: 450,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "div2",
    expected: 52.69,
  },
  {
    basename: "arithmetic_nested",
    description: "Test basic nested arithmetic",
    export: "div3",
    expected: 2.0,
  },
  {
    basename: "adder",
    description: "Test closures",
    export: "add",
    expected: 22,
  },
  {
    basename: "add1_A",
    description: "Test module creation",
    export: "add1",
    params: [21],
    expected: 22,
  },
  {
    basename: "add1_B",
    description: "Test module creation",
    export: "add1",
    params: [21],
    expected: 22,
  },
  {
    basename: "add1_C",
    description: "Test module creation",
    export: "add1",
    params: [21],
    expected: 22,
  },
  {
    basename: "arithmetic_complex",
    description: "Test complex arithmetic",
    export: "a1",
    expected: 232.22,
  },
  {
    basename: "arithmetic_complex",
    description: "Test complex arithmetic",
    export: "a2",
    expected: 225,
  },
  {
    basename: "arithmetic_complex",
    description: "Test complex arithmetic",
    export: "a3",
    params: [3],
    expected: 90,
  },
  {
    basename: "arithmetic_complex",
    description: "Test complex arithmetic",
    export: "a4",
    params: [10, 8],
    expected: 0,
  },
  {
    basename: "arithmetic_complex",
    description: "Test complex arithmetic",
    export: "a5",
    params: [1, 2, 3],
    expected: -26,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "tmp",
    expected: 3,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "seven",
    expected: 7,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "add1",
    params: [33],
    expected: 34,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "sub1",
    params: [33],
    expected: 32,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "half",
    params: [32],
    expected: 16,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "double",
    params: [33],
    expected: 66,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "square",
    params: [3],
    expected: 9,
  },
  {
    basename: "arithmetic_funcs",
    description: "Test arithmetic functions",
    export: "add",
    params: [33, 47],
    expected: 80,
  },
];

module.exports = { test_cases };
