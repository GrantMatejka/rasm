const getResult = (ptr, mem) => {
  let val = 0;
  switch (mem[ptr]) {
    case 0: // int
      val = Buffer.from(mem.slice(ptr + 1, ptr + 9)).readBigInt64LE();
      return val.toString();
    case 1: // float
      val = Buffer.from(mem.slice(ptr + 1, ptr + 9)).readDoubleLE();
      return val.toString();
    default:
      return -1;
  }
};

const instantiate = (bytes) => {
  const importObject = {
    env: {
      log_f64: (i) => console.log(i),
      log_i32: (i) => console.log(i),
      log_i64: (i) => console.log(i),
    },
  };

  return WebAssembly.instantiate(new Uint8Array(bytes), importObject).then(
    (obj) => {
      obj.funcs = [];
      obj.vals = [];

      for (const wasm_export_name in obj.instance.exports) {
        const wasm_export = obj.instance.exports[wasm_export_name];

        if (
          typeof wasm_export === "function" &&
          !wasm_export_name.startsWith("__")
        ) {
          const wrapped = function (...params) {
            const arg_ptrs = params.map((p) => {
              // All numbers in javascript are 64-bit floats
              if (typeof p === "number") {
                return obj.instance.exports.__allocate_float(p);
              }
            });

            return getResult(
              wasm_export(...arg_ptrs),
              new Uint8Array(obj.instance.exports.memory.buffer)
            );
          };
          wrapped.numargs = wasm_export.length;
          wrapped.length = wasm_export.length;
          obj.funcs[wasm_export_name] = wrapped;
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

module.exports = { instantiate, getResult };
