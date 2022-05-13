const allocateParamList = (obj_exports, arg_ptrs) => {
  const param_list_ptr = obj_exports.__alloc(4 * arg_ptrs.length);

  arg_ptrs.map((ptr, idx) => {
    obj_exports.__store_i32(4 * idx + param_list_ptr, ptr);
  });

  return param_list_ptr;
};

const wrapFunctionExport = (wasm_export, wasm_export_name, obj_exports) => {
  const wrapped = (...params) => {
    const arg_ptrs = params.map((p) => {
      // All numbers in javascript are 64-bit floats
      if (typeof p === "number") {
        return obj_exports.__allocate_float(p);
      }
    });

    return wasm_export(allocateParamList(obj_exports, arg_ptrs));
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
      obj.rasm = {};

      obj.rasm.jsTojs = {};
      obj.rasm.jsTowasm = {};
      obj.rasm.wasmTojs = {};
      obj.rasm.wasmTowasm = {};

      // .internals holds all the secret stuff
      obj.rasm.internals = {};

      const mem = new Uint8Array(obj.instance.exports.memory.buffer);
      // toJS converts a WebAssembly memory pointer to its respective JS value
      obj.rasm.toJS = (ptr) => {
        let val = 0;
        switch (mem[ptr]) {
          case 0: // int
            val = new DataView(mem.slice(ptr + 1, ptr + 9).buffer).getBigUint64(
              0,
              true
            );
            return val.toString();
          case 1: // float
            val = new DataView(mem.slice(ptr + 1, ptr + 9).buffer).getFloat64(
              0,
              true
            );
            return val.toString();
          case 3: // closure
            val = function (...params) {
              // TODO: It is not allowed to pass JavaScript functions to WebAssembly
              const arg_ptrs = params.map((p) => {
                // All numbers in javascript are 64-bit floats
                if (typeof p === "number") {
                  return obj.instance.exports.__allocate_float(p);
                }
              });

              const param_list_ptr = allocateParamList(
                obj.instance.exports,
                arg_ptrs
              );
              
              return obj.instance.exports.__app(ptr, param_list_ptr);
            };
            return val;
          default:
            return -1;
        }
      };

      // Converts a js value to a WebAssembly pointer
      obj.rasm.toWasm = (val) => {
        if (typeof val === "number") {
          return obj.instance.exports.__allocate_float(val);
        } else if (typeof val === "bigint") {
          return obj.instance.exports.__allocate_int(val);
        } else if (typeof val === "boolean") {
          return val
            ? obj.instance.exports.__allocate_int(1)
            : obj.instance.exports.__allocate_int(0);
        } else {
          return -1;
        }
      };

      for (const wasm_export_name in obj.instance.exports) {
        const wasm_export = obj.instance.exports[wasm_export_name];

        if (wasm_export_name.startsWith("__")) {
          if (typeof wasm_export === "function") {
            const wrapped = wrapFunctionExport(
              wasm_export,
              wasm_export_name,
              obj.instance.exports
            );

            obj.rasm.internals[wasm_export_name] = wrapped;
          } else if (wasm_export.value) {
            obj.rasm.internals[wasm_export_name] = wasm_export.value;
          }
        } else if (typeof wasm_export === "function") {
          // wasm ptrs => wasm ptrs
          obj.rasm.wasmTowasm[wasm_export_name] = (...param_ptrs) =>
            wasm_export(allocateParamList(obj.instance.exports, param_ptrs));
          // wasm ptrs => js values
          obj.rasm.wasmTojs[wasm_export_name] = (...param_ptrs) =>
            obj.rasm.toJS(
              wasm_export(allocateParamList(obj.instance.exports, param_ptrs))
            );

          const wrapped = wrapFunctionExport(
            wasm_export,
            wasm_export_name,
            obj.instance.exports
          );

          const wrappedToJS = (...params) => obj.rasm.toJS(wrapped(...params));
          wrappedToJS.numargs = wrapped.numargs;
          wrappedToJS.length = wrapped.length;

          // js vals => wasm ptrs
          obj.rasm.jsTowasm[wasm_export_name] = wrapped;
          // js vals => js vals
          obj.rasm.jsTojs[wasm_export_name] = wrappedToJS;
        } else {
          if (wasm_export.value) {
            obj.rasm.wasmTowasm = (...params) => {
              const val = wasm_export.value;

              switch (mem[val]) {
                case 3:
                  return obj.instance.exports.__app(
                    wasm_export.value,
                    allocateParamList(obj.instance.exports, params)
                  );
                default:
                  return val;
              }
            };

            obj.rasm.wasmTojs = (...params) => {
              const val = wasm_export.value;

              switch (mem[val]) {
                case 3:
                  return obj.rasm.toJS(
                    obj.instance.exports.__app(
                      wasm_export.value,
                      allocateParamList(obj.instance.exports, params)
                    )
                  );
                default:
                  return val;
              }
            };

            obj.rasm.jsTowasm[wasm_export_name] = (...params) => {
              const js_val = obj.rasm.toJS(wasm_export.value);

              if (typeof js_val == "function") {
                return js_val(...params);
              } else {
                return js_val;
              }
            };

            obj.rasm.jsTojs[wasm_export_name] = (...params) => {
              const js_val = obj.rasm.toJS(wasm_export.value);

              if (typeof js_val == "function") {
                return obj.rasm.toJS(js_val(...params));
              } else {
                return js_val;
              }
            };
          }
        }
      }

      return obj;
    }
  );
};

module.exports = { instantiate };
