const rasm = require('./rasm');
const fs = require('fs');

rasm.instantiate(fs.readFileSync('./a.wasm')).then((obj) => {
  // benchmark method here

});
