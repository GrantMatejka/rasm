// How to work with a compiled rasm module

const fs = require("fs");
// We expect an `a.wasm` file
const bytes = fs.readFileSync("./a.wasm");
const rasm = require("./rasm");


// rasm.instantiate(bytes).then((obj) => {
//    HERE: You can process the webassembly module however you want
//    obj is an instantiated webassembly module
//    - obj.funcs contains wrapped webassembly functions
//    -- You should only call functions from this wrapped form
//    - obj.vals contains any exported global variables/constants
// });

rasm.instantiate(bytes).then((obj) => {
  for (const funcname in obj.funcs) {
    const func = obj.funcs[funcname];

    const params = Array(func.numargs)
      .fill(0)
      .map((_, __) => Math.floor(Math.random() * 10));

    const return_val = func(...params);

    console.log(
      `-----------------\n${funcname} called with [ ${params} ] returned: ${return_val}`
    );
  }

  for (const valname in obj.vals) {
    const val = obj.funcs[valname];

    console.log(`-----------------\nExported ${valname} with Value ${val}`);
  }
});
