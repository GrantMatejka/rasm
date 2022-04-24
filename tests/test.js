const fs = require("fs");
const exec = require("child_process").exec;
const assert = require("assert");
const wasm_helper = require("./rasm");

assert.equal(process.argv.length, 3, "Expected use: node test.js <filename>");

const expected = JSON.parse(fs.readFileSync("./expected.json"));

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

const testFiles = (files, expected) => {
  files.forEach((basename) => {
    const rkt_path = `../examples/${basename}.rkt`;
    const out_path = `out/${basename}.wat`;

    const copy_rasm = `cp ../rasm.js .`;
    const compile_file = `racket ../compiler.rkt ${rkt_path}`;
    const generate_wasm = `wat2wasm ${out_path} -o out/a.wasm`;

    my_exec(`${copy_rasm} && ${compile_file} && ${generate_wasm}`, () => {
      const expected_exports = expected[basename]["exports"];
      const bytes = fs.readFileSync("out/a.wasm");

      wasm_helper.instantiate(bytes).then((obj) => {
        console.log(JSON.stringify(wasm_helper.instantiate));
        console.log(JSON.stringify(obj.funcs));

        for (const funcname in obj.funcs) {
          const expectations = expected_exports[funcname];
          const wasm_export = obj.funcs[funcname];
          let result = expectations.hasOwnProperty("params")
            ? wasm_export(...expectations["params"])
            : wasm_export();

          const expected_val = expectations["val"];
          assert.equal(
            expected_val,
            result,
            `FAIL: ${funcname} -> expected ${expected_val} got ${result}`
          );
        }

        for (const valname in obj.vals) {
          const expectations = expected_exports[valname];
          const expected_val = expectations["val"];
          const wasm_export = obj.vals[valname];
          assert.equal(
            expected_val,
            wasm_export,
            `FAIL: ${valname} -> expected ${expected_val} got ${wasm_export}`
          );
        }
      });
    });
  });
};

let files = [];
if (process.argv[2] === "all") {
  files = expected.keys();
  my_exec(`raco make ../compiler.rkt`, () => testFiles(files, expected));
} else {
  files.push(process.argv[2]);
  testFiles(files, expected);
}
