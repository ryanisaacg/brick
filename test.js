var url = "out.wasm";
WebAssembly.instantiateStreaming(fetch(url)).then((result) => {
  window.brick = result.instance;
  const store_in_mem = result.instance.exports.store_in_mem;
  const memory = result.instance.exports.memory;

  // Do some analysis here
  window.brick_mem = () =>
    new Uint32Array(result.instance.exports.memory.buffer);
});
