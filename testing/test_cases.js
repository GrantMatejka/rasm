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
];

module.exports = { test_cases };
