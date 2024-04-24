#![allow(clippy::missing_safety_doc)]

#[no_mangle]
pub unsafe extern "C" fn brick_runtime_alloc(
    region: *mut u8,
    allocator: *mut usize,
    alloc_size: usize,
) -> *mut u8 {
    let block_start = *allocator;
    *allocator += alloc_size;
    region.add(block_start)
}

pub unsafe extern "C" fn brick_string_concat(
    region: *mut u8,
    allocator: *mut usize,
    a_ptr: *const u8,
    a_len: usize,
    b_ptr: *const u8,
    b_len: usize,
    len_output: *mut usize,
) -> *mut u8 {
    let a_slice = std::slice::from_raw_parts(a_ptr, a_len);
    let b_slice = std::slice::from_raw_parts(b_ptr, b_len);

    let a_str = std::str::from_utf8(a_slice).unwrap();
    let b_str = std::str::from_utf8(b_slice).unwrap();

    // TODO: allocating this then copying it into interpreter memory is obviously stupid
    // but I don't have a strong idea of how allocation should be handled yet
    let mut result_string = String::new();
    result_string.push_str(a_str);
    result_string.push_str(b_str);
    unsafe {
        *len_output = result_string.len();
    }
    let memory_region = brick_runtime_alloc(region, allocator, result_string.len());
    let target = std::slice::from_raw_parts_mut(memory_region, result_string.len());
    target.copy_from_slice(result_string.as_bytes());

    memory_region
}
