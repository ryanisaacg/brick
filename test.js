var url = "out.wasm";
WebAssembly.instantiateStreaming(fetch(url)).then((result) => {
  window.brick = result.instance;
  const store_in_mem = result.instance.exports.store_in_mem;
  const memory = result.instance.exports.memory;

  // Do some analysis here
  const values = new Uint32Array(result.instance.exports.memory.buffer);
  window.brick_mem = values;
});
