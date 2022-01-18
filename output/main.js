const fs = require("fs");
const bytes = fs.readFileSync("./output.wasm");

WebAssembly.instantiate(new Uint8Array(bytes)).then((obj) => {
  console.log(obj.instance.exports.add());
  console.log(obj.instance.exports.sub());
});
