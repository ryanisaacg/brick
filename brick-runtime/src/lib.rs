#![allow(clippy::missing_safety_doc)]
#![cfg_attr(not(feature = "std"), no_std)]

use core::alloc::{GlobalAlloc, Layout};

use linked_list_allocator::LockedHeap as Heap;

#[cfg(all(target_arch = "wasm32", not(feature = "std")))]
#[cfg_attr(all(target_arch = "wasm32", not(feature = "std")), panic_handler)]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[no_mangle]
pub unsafe extern "C" fn brick_runtime_init(heap_start: *mut u8, size: usize) -> *mut Heap {
    let heap = Heap::empty();
    heap.lock().init(heap_start, size);

    let memory_region = heap.alloc(Layout::new::<Heap>()) as *mut Heap;
    *memory_region = heap;

    memory_region
}

const LAYOUT_SIZE: usize = core::mem::size_of::<Layout>();
#[cfg(target_arch = "wasm32")]
const WASM_PAGE_SIZE: usize = 65536;

#[no_mangle]
pub unsafe extern "C" fn brick_runtime_alloc(
    allocator: *mut u8,
    alloc_size: usize,
    alloc_align: usize,
) -> *mut u8 {
    let heap = (allocator as *mut Heap).as_ref().unwrap();
    let locked_heap = heap.lock();
    let free_space = locked_heap.free();
    let used_space = locked_heap.used();
    drop(locked_heap);

    if free_space <= alloc_size {
        #[cfg(target_arch = "wasm32")]
        {
            let missing_space = alloc_size - free_space;
            let amount_to_alloc = (missing_space * 2).max(used_space);
            let alloc_pages = (amount_to_alloc / WASM_PAGE_SIZE) + 1;
            core::arch::wasm32::memory_grow(0, alloc_pages);
            heap.lock().extend(alloc_pages * WASM_PAGE_SIZE);
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            unimplemented!("can't request more memory outside of WASM, tried to allocate {alloc_size} with {free_space} free and {used_space} used");
        }
    }
    let alloc_align = core::mem::align_of::<Layout>().max(alloc_align);
    let alloc_size = alloc_size + LAYOUT_SIZE;
    let layout = Layout::from_size_align(alloc_size, alloc_align).unwrap();
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

#[no_mangle]
pub unsafe extern "C" fn brick_string_concat(
    allocator: *mut u8,
    a_ptr: *const u8,
    a_len: usize,
    b_ptr: *const u8,
    b_len: usize,
    c_ptr_ptr: *mut *const u8,
    c_len_ptr: *mut usize,
) {
    let a_slice = core::slice::from_raw_parts(a_ptr, a_len);
    let b_slice = core::slice::from_raw_parts(b_ptr, b_len);

    let memory_region = brick_runtime_alloc(allocator, a_len + b_len, 8);
    let target = core::slice::from_raw_parts_mut(memory_region, a_len);
    target.copy_from_slice(a_slice);
    let target = core::slice::from_raw_parts_mut(memory_region.add(a_len), b_len);
    target.copy_from_slice(b_slice);

    *c_ptr_ptr = memory_region;
    *c_len_ptr = a_len + b_len;
}

#[no_mangle]
pub unsafe extern "C" fn brick_memcpy(dest: *mut u8, src: *const u8, len: usize) {
    let dest_slice = core::slice::from_raw_parts_mut(dest, len);
    let src_slice = core::slice::from_raw_parts(src, len);
    dest_slice.copy_from_slice(src_slice);
}
