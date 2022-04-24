const getVal = (bytes) => {
  const hexStr = "0x" + Buffer.from(bytes.reverse()).toString("hex");

  return BigInt(hexStr);
};

const getResult = (ptr, mem) => {
  switch (mem[ptr]) {
    case 0: // int
      return getVal(mem.slice(ptr + 1, ptr + 5));
    case 1: // float
      return getVal(mem.slice(ptr + 1, ptr + 5));
    default:
      return -1;
  }
};

const instantiate = (bytes) => {
  const importObject = {
    env: {
      log: (i) => console.log(i),
    },
  };

  return WebAssembly.instantiate(new Uint8Array(bytes), importObject).then(
    (obj) => {
      obj.funcs = [];
      obj.vals = [];

      for (const wasm_export_name in obj.instance.exports) {
        const wasm_export = obj.instance.exports[wasm_export_name];

        if (typeof wasm_export === "function") {
          obj.funcs[wasm_export_name] = (...params) =>
            getResult(
              wasm_export(...params),
              new Uint8Array(obj.instance.exports.memory.buffer)
            );
        } else {
          if (wasm_export.value) {
            obj.vals[wasm_export_name] = wasm_export.value;
          }
        }
      }

      return obj;
    }
  );
};

module.exports = { instantiate, getResult, getVal };
