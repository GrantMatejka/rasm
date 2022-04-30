const getResult = (ptr, mem) => {
  let val = 0;
  switch (mem[ptr]) {
    case 0: // int
      val = Buffer.from(mem.slice(ptr + 1, ptr + 9)).readBigInt64LE();
      return val.toString();
    case 1: // float
      val = Buffer.from(mem.slice(ptr + 1, ptr + 9)).readDoubleLE();
      return val.toString();
    case 3:
      return "[function]";
    default:
      return -1;
  }
};

const wrapFunction = (wasm_export, wasm_export_name, obj_exports) => {
  const wrapped = function (...params) {
    const arg_ptrs = params.map((p) => {
      // All numbers in javascript are 64-bit floats
      if (typeof p === "number") {
        return obj_exports.__allocate_float(p);
      }
    });

    const param_list_ptr = obj_exports.__alloc(4 * arg_ptrs.length);

    arg_ptrs.map((ptr, idx) => {
      obj_exports.__store_i32(4 * idx + param_list_ptr, ptr);
    });

    return getResult(
      wasm_export(param_list_ptr),
      new Uint8Array(obj_exports.memory.buffer)
    );
  };

  const param_length_name = "__" + wasm_export_name + "_length";
  if (obj_exports[param_length_name]) {
    wrapped.numargs = obj_exports[param_length_name].value;
    wrapped.length = obj_exports[param_length_name].value;
  } else {
    wrapped.numargs = wasm_export.length;
    wrapped.length = wasm_export.length;
  }

  return wrapped;
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
      obj.internals = [];

      for (const wasm_export_name in obj.instance.exports) {
        const wasm_export = obj.instance.exports[wasm_export_name];

        if (wasm_export_name.startsWith("__")) {
          if (typeof wasm_export === "function") {
            const wrapped = wrapFunction(
              wasm_export,
              wasm_export_name,
              obj.instance.exports
            );

            obj.internals[wasm_export_name] = wrapped;
          } else if (wasm_export.value) {
            obj.internals[wasm_export_name] = getResult(
              wasm_export.value,
              new Uint8Array(obj.instance.exports.memory.buffer)
            );
          }
        } else if (typeof wasm_export === "function") {
          const wrapped = wrapFunction(
            wasm_export,
            wasm_export_name,
            obj.instance.exports
          );

          obj.funcs[wasm_export_name] = wrapped;
        } else {
          if (wasm_export.value) {
            obj.vals[wasm_export_name] = getResult(
              wasm_export.value,
              new Uint8Array(obj.instance.exports.memory.buffer)
            );
          }
        }
      }

      return obj;
    }
  );
};

module.exports = { instantiate, getResult };
