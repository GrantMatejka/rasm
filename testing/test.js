const fs = require("fs");
const exec = require("child_process").exec;
const assert = require("assert");
const { test_cases } = require("./test_cases");
var wabt = require("wabt")();
const rasm = require("./rasm");

const my_exec = (cmd, callback) => {
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

const copy_rasm = `cp ../rasm.js .`;
const copy_index = `cp ../example_index.js .`;
const make_compiler = `raco make ../compiler.rkt`;

my_exec(`${copy_rasm} && ${copy_index} && ${make_compiler}`, () =>
  test_cases.forEach((test) => {
    const basename = test.basename;
    const rkt_path = `../examples/${basename}.rkt`;

    const compile_file = `racket ../compiler.rkt ${rkt_path}`;

    my_exec(`${compile_file}`, () => {
      console.log(`Test Case: ${basename}`);

      const inputWat = `out/${basename}.wat`;
      wabt.then((wabt) => {
        var wasmModule = wabt.parseWat(
          inputWat,
          fs.readFileSync(inputWat, "utf8")
        );

        rasm.instantiate(wasmModule.toBinary({}).buffer).then((obj) => {
          if (test.callback) {
            const val = test.callback(obj);
            my_assert(test.expected, val, test.description);
          } else if (test.export) {
            const params = test.params ? test.params : [];
            const val = obj.rasm.jsTojs[test.export](...params);
            my_assert(
              test.expected,
              val,
              `${test.export}: ${test.description}`
            );
          }
        });
      });
    });
  })
);
