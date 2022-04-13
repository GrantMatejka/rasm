const fs = require("fs");
// We expect an `a.wasm` file
const bytes = fs.readFileSync("./a.wasm");

const memory = new WebAssembly.Memory({ initial: 256 });

// TODO: What will we need here???
const importObject = {
  env: {
    mem: memory,
  },
};

WebAssembly.instantiate(new Uint8Array(bytes), importObject).then((obj) => {
  // TODO: Let's just call and print every exported function
  for (const eprop in obj.instance.exports) {
    const exp = obj.instance.exports[eprop];

    if (typeof exp === "function") {
      const params = Array(exp.length)
        .fill(0)
        .map((_, __) => Math.floor(Math.random() * 10));
      console.log(
        `-----------------\n${eprop} called with [ ${params} ] returned: ${exp(
          ...params
        )}`
      );
    } else {
      console.log(`-----------------\nExported ${eprop} with Value ${exp.value}`);
    }
  }
});
