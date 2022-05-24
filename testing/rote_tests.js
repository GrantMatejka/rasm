const fs = require("fs");
const exec = require("child_process").exec;
const assert = require("assert");
const { test_cases } = require("./test_cases");

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

const runTests = (test_cases) => {
  const copy_rasm = `cp ../rasm.js .`;
  const copy_index = `cp ../example_index.js .`;
  const make_compiler = `raco make ../compiler.rkt`;
  my_exec(`${copy_rasm} && ${copy_index} && ${make_compiler}`, () =>
    test_cases.forEach((test) => {
      const basename = test.basename;
      const rkt_path = `../examples/${basename}.rkt`;
      const out_path = `out/${basename}.wat`;

      const compile_file = `racket ../compiler.rkt ${rkt_path}`;
      const generate_wasm = `wat2wasm ${out_path} -o out/a.wasm`;

      // Sleep to make sure final file is done being written to
      my_exec(`${compile_file} && sleep 1`, () =>
        my_exec(`${generate_wasm}`, () => {
          const wasm_helper = require("./rasm");
          const bytes = fs.readFileSync("out/a.wasm");

          console.log(`Test Case: ${basename}`);

          wasm_helper.instantiate(bytes).then((obj) => {
            if (test.callback) {
              const val = test.callback(obj);
              my_assert(test.expected, val, test.description);
            } else {
              const params = test.params ? test.params : [];
              const val = obj.rasm.jsTojs[test.export](...params);
              my_assert(test.expected, val, test.description);
            }
          });
        })
      );
    })
  );
};

runTests(test_cases);
