// JUST an example of how to work with a compiled rasm module
const exec = require("child_process").exec;
const rasm = require("./rasm");

// A simple wrapper of exec to keep track of some IO
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

const process_wasm = (bytes) =>
  // To use a returned closure: obj.rasm.toJS(obj.rasm.toJS(CLOSURE)(ARGUMENTS))

  // obj is an instatiated WebAssembly module
  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Module
  rasm.instantiate(bytes).then((obj) => {
    /**
     * Every function in `obj.funcs` is a special wrapper of the webassembly function
     * The wasm function itself is still accessible in `obj.instance.exports`
     *
     * When working with rasm, you should only call functions from this wrapped form
     */
    for (const export_name in obj.rasm.jsTojs) {
      const exp = obj.rasm.jsTojs[export_name];

      // We are only calling exported functions where we know how many arguments they take
      if (exp.numargs) {
        const params = Array(exp.numargs)
          .fill(0)
          .map((_, __) => Math.floor(Math.random() * 10));

        console.log(
          `-----------------\n${export_name} called with [ ${params} ] returned: ${exp(
            ...params
          )}`
        );
      }
    }
  });

if (process.argv.length !== 3) {
  console.error("Expected use: node index.js <filename>");
}

const filename = process.argv[2].endsWith(".wat")
  ? process.argv[2]
  : process.argv[2].concat(".wat");

my_exec(`wat2wasm ${filename} -o a.wasm`, () => {
  const fs = require("fs");
  const bytes = fs.readFileSync("./a.wasm");

  process_wasm(bytes);
});
