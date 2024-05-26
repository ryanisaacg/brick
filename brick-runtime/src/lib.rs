#![allow(clippy::missing_safety_doc)]
#![no_std]

use core::alloc::{GlobalAlloc, Layout};

use linked_list_allocator::LockedHeap as Heap;

#[no_mangle]
pub unsafe fn brick_runtime_init(heap_start: *mut u8, size: usize) -> *mut Heap {
    let heap = Heap::empty();
    heap.lock().init(heap_start, size);

    let memory_region = heap.alloc(
        Layout::from_size_align(core::mem::size_of::<Heap>(), core::mem::align_of::<Heap>())
            .unwrap(),
    ) as *mut Heap;
    *memory_region = heap;
    memory_region
}

const LAYOUT_SIZE: usize = core::mem::size_of::<Layout>();

// TODO: take alignment as a parameter?
#[no_mangle]
pub unsafe extern "C" fn brick_runtime_alloc(allocator: *mut u8, alloc_size: usize) -> *mut u8 {
    let heap = (allocator as *mut Heap).as_ref().unwrap();
    let alloc_size = alloc_size + LAYOUT_SIZE;
    let layout = Layout::from_size_align(alloc_size, 1).unwrap();
    let allocation = heap.alloc(layout);

    if allocation as usize == 0 {
        panic!("out of memory");
    }

    save_layout_to_region(allocation, layout);

    allocation.add(LAYOUT_SIZE)
}

#[no_mangle]
pub unsafe extern "C" fn brick_runtime_realloc(
    allocator: *mut u8,
    region: *mut u8,
    new_size: usize,
) -> *mut u8 {
    let heap = (allocator as *mut Heap).as_ref().unwrap();
    let region = region.offset(-(LAYOUT_SIZE as isize));
    let layout_ptr = region as *mut Layout;
    let layout = *layout_ptr.as_ref().unwrap();
    let new_size = new_size + LAYOUT_SIZE;

    let allocation = heap.realloc(region, layout, new_size);
    if allocation as usize == 0 {
        panic!("out of memory");
    }

    save_layout_to_region(
        allocation,
        Layout::from_size_align(new_size, layout.align()).unwrap(),
    );

    allocation.add(LAYOUT_SIZE)
}

unsafe fn save_layout_to_region(region: *mut u8, layout: Layout) {
    let layout_ptr = region as *mut Layout;
    *layout_ptr.as_mut().unwrap() = layout;
}

#[no_mangle]
pub unsafe extern "C" fn brick_runtime_dealloc(allocator: *mut u8, region: *mut u8) {
    let heap = (allocator as *mut Heap).as_ref().unwrap();
    let region = region.offset(-(LAYOUT_SIZE as isize));
    let layout_ptr = region as *mut Layout;
    let layout = *layout_ptr.as_ref().unwrap();
    heap.dealloc(region, layout)
}

pub unsafe extern "C" fn brick_string_concat(
    allocator: *mut u8,
    a_ptr: *const u8,
    a_len: usize,
    b_ptr: *const u8,
    b_len: usize,
) -> *mut u8 {
    let a_slice = core::slice::from_raw_parts(a_ptr, a_len);
    let b_slice = core::slice::from_raw_parts(b_ptr, b_len);

    let memory_region = brick_runtime_alloc(allocator, a_len + b_len);
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
