use wasmtime::{AsContextMut, Caller, Extern, Linker};

pub fn add_runtime_functions(linker: &mut Linker<()>) -> anyhow::Result<()> {
    linker.func_wrap(
        "brick-runtime",
        "brick_runtime_init",
        |mut caller: Caller<'_, ()>, heap_start: i32, size: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                let allocator =
                    brick_runtime::brick_runtime_init(mem.add(heap_start as usize), size as usize);
                (allocator as *mut u8).offset_from(mem) as i32
            }
        },
    )?;
    linker.func_wrap(
        "brick-runtime",
        "brick_runtime_alloc",
        |mut caller: Caller<'_, ()>, allocator: i32, size: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                let allocated_block =
                    brick_runtime::brick_runtime_alloc(mem.add(allocator as usize), size as usize);
                allocated_block.offset_from(mem) as i32
            }
        },
    )?;
    linker.func_wrap(
        "brick-runtime",
        "brick_runtime_realloc",
        |mut caller: Caller<'_, ()>, allocator: i32, ptr: i32, size: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                let allocated_block = brick_runtime::brick_runtime_realloc(
                    mem.add(allocator as usize),
                    mem.add(ptr as usize),
                    size as usize,
                );
                allocated_block.offset_from(mem) as i32
            }
        },
    )?;
    linker.func_wrap(
        "brick-runtime",
        "brick_runtime_dealloc",
        |mut caller: Caller<'_, ()>, allocator: i32, region: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                brick_runtime::brick_runtime_dealloc(
                    mem.add(allocator as usize),
                    mem.add(region as usize),
                );
            }
        },
    )?;
    linker.func_wrap(
        "brick-runtime",
        "brick_string_concat",
        |mut caller: Caller<'_, ()>,
         allocator: i32,
         a_ptr: i32,
         a_len: i32,
         b_ptr: i32,
         b_len: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                let new_str = brick_runtime::brick_string_concat(
                    mem.add(allocator as usize),
                    mem.add(a_ptr as usize),
                    a_len as usize,
                    mem.add(b_ptr as usize),
                    b_len as usize,
                );
                (new_str.offset_from(mem) as i32, a_len + b_len)
            }
        },
    )?;
    linker.func_wrap(
        "brick-runtime",
        "brick_memcpy",
        |mut caller: Caller<'_, ()>, dest: i32, source: i32, len: i32| {
            let mem = mem_ptr(&mut caller);
            unsafe {
                let dest = mem.add(dest as usize);
                let source = mem.add(source as usize);
                brick_runtime::brick_memcpy(dest, source, len as usize);
            }
        },
    )?;

    Ok(())
}

fn mem_ptr(caller: &mut Caller<'_, ()>) -> *mut u8 {
    let Extern::Memory(mem) = caller.get_export("memory").unwrap() else {
        unreachable!();
    };
    let store = caller.as_context_mut();
    mem.data_ptr(store)
}
