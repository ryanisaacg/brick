#![allow(clippy::missing_safety_doc)]
#![no_std]

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
) -> *mut u8 {
    let a_slice = core::slice::from_raw_parts(a_ptr, a_len);
    let b_slice = core::slice::from_raw_parts(b_ptr, b_len);

    let memory_region = brick_runtime_alloc(region, allocator, a_len + b_len);
    let target = core::slice::from_raw_parts_mut(memory_region, a_len);
    target.copy_from_slice(a_slice);
    let target = core::slice::from_raw_parts_mut(memory_region.add(a_len), b_len);
    target.copy_from_slice(b_slice);

    memory_region
}

pub unsafe extern "C" fn brick_memcpy(dest: *mut u8, src: *const u8, len: usize) {
    let dest_slice = core::slice::from_raw_parts_mut(dest, len);
    let src_slice = core::slice::from_raw_parts(src, len);
    dest_slice.copy_from_slice(src_slice);
}
