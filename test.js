var url = "out.wasm";
WebAssembly.instantiateStreaming(fetch(url)).then((result) => {
  const store_in_mem = result.instance.exports.store_in_mem;
  const memory = result.instance.exports.memory;
  console.log(result.instance.exports.f());

  // Do some analysis here
});
