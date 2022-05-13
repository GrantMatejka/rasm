const fs = require("fs");
const exec = require("child_process").exec;
const assert = require("assert");

const my_exec = function (cmd, callback) {
  exec(cmd, (error, stdout, stderr) => {
    if (error) {
      console.error(`error: ${error.message}`);
      return;
    }
    if (stderr) {
      console.error(`stderr: ${stderr}`);
      return;
    }
    if (stdout.trim()) {
      console.log(`stdout: ${stdout}`);
    }
    callback();
  });
};

const my_assert = (want, got, msg) => {
  const epsilon = 0.005;

  assert.ok(want - epsilon < got && want + epsilon > got, msg);
};

// These files contain programs that don't export simple functions
// some handcoded test cases required

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
];

const runTests = (test_cases) => {
  test_cases.forEach((test) => {
    const basename = test.basename;
    const rkt_path = `../examples/${basename}.rkt`;
    const out_path = `out/${basename}.wat`;

    const copy_rasm = `cp ../rasm.js .`;
    const copy_index = `cp ../example_index.js .`;
    const compile_file = `racket ../compiler.rkt ${rkt_path}`;
    const generate_wasm = `wat2wasm ${out_path} -o out/a.wasm`;

    my_exec(
      `${copy_rasm} && ${copy_index} && ${compile_file} && ${generate_wasm}`,
      () => {
        const wasm_helper = require("./rasm");
        const bytes = fs.readFileSync("out/a.wasm");

        console.log(`Testing Test Case for: ${basename}`);

        wasm_helper.instantiate(bytes).then((obj) => {
          const val = test.callback(obj);
          my_assert(test.expected, val, test.description);
        });
      }
    );
  });
};

runTests(test_cases);
