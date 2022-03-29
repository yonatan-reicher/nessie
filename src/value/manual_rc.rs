//! Provides the `ManualRc` type, a manual reference-counting pointer type.

use std::alloc::{alloc, dealloc, Layout};
use std::ptr::{self, NonNull as P};


/// A heap-allocated value of type `T` which is immutable and must be counted
/// by hand.
pub struct ManualRc<T>
where T: ?Sized + ManualRcBoxable {
    ptr: P<ManualRcBox<T>>,
}

pub type RefCount = usize;

/// A value that can be put inside a `ManualRc`.
pub trait ManualRcBoxable {
    type Header;

    fn header(&self) -> Self::Header;

    fn layout(&self) -> Layout;

    fn layout_from_header(header: &Self::Header) -> Layout;

    unsafe fn mem_to_rcbox(&self, mem: P<u8>) -> P<ManualRcBox<Self>>;

    unsafe fn copy_to(&self, ptr: P<ManualRcBox<Self>>);
}

#[repr(C)]
struct ManualRcBox<T>
where T: ?Sized + ManualRcBoxable {
    ref_count: RefCount,
    header: T::Header,
    value: T,
}

impl<T> Clone for ManualRc<T>
where T: ?Sized + ManualRcBoxable {
    fn clone(&self) -> Self {
        ManualRc { ptr: self.ptr }
    }
}

impl<T> Copy for ManualRc<T>
where T: ?Sized + ManualRcBoxable {}


impl<T> ManualRc<T>
where T: ?Sized + ManualRcBoxable {
    /// Creates a new `ManualRc` containing the given value.
    pub unsafe fn new(value: &T) -> Self {
        println!("Newing a value");
        let layout = value.layout();
        let ptr = P::new(alloc(layout)).unwrap();
        let mut ptr = value.mem_to_rcbox(ptr);
        ptr.as_mut().ref_count = 1;
        (&mut ptr.as_mut().header as *mut T::Header).write(value.header());
        value.copy_to(ptr);
        ManualRc { ptr }
    }

    /// Returns a mutable reference to the contained value.
    pub unsafe fn get_mut(&mut self) -> &mut T {
        &mut self.ptr.as_mut().value
    }

    /// Returns a reference to the contained value.
    pub unsafe fn get(&self) -> &T {
        &self.ptr.as_ref().value
    }

    fn ref_count_mut(&mut self) -> &mut RefCount {
        unsafe {
            &mut self.ptr.as_mut().ref_count
        }
    }

    pub fn ref_count(&self) -> RefCount {
        unsafe {
            self.ptr.as_ref().ref_count
        }
    }

    /// Increases the reference count by one.
    pub fn inc_ref(&mut self) {
        *self.ref_count_mut() += 1;
    }

    /// Decreases the reference count by one.
    /// # Safety
    /// The caller must ensure that the reference count is greater than zero.
    pub unsafe fn dec_ref(&mut self) {
        *self.ref_count_mut() -= 1;
        if self.ref_count() == 0 {
            // Drop the value and free the memory
            println!("Dropping value");
            ptr::drop_in_place(self.ptr.as_ptr());
            dealloc(
                self.ptr.as_ptr().cast(),
                T::layout_from_header(&self.ptr.as_ref().header)
            );
        }
    }
}

impl<T> ManualRcBoxable for T
where T: Clone {
    type Header = ();

    fn header(&self) -> Self::Header { () }

    fn layout(&self) -> Layout {
        Layout::new::<ManualRcBox<T>>()
    }

    fn layout_from_header(_: &Self::Header) -> Layout {
        Layout::new::<ManualRcBox<T>>()
    }

    unsafe fn mem_to_rcbox(&self, mem: P<u8>) -> P<ManualRcBox<T>> {
        mem.cast()
    }

    unsafe fn copy_to(&self, mut ptr: P<ManualRcBox<T>>) {
        ptr::write(&mut ptr.as_mut().value, self.clone());
    }
}

impl<T> ManualRcBoxable for [T]
where T: Copy {
    type Header = usize;

    fn header(&self) -> Self::Header {
        self.len()
    }

    fn layout(&self) -> Layout {
        Layout::array::<T>(self.len()).unwrap()
    }

    fn layout_from_header(header: &Self::Header) -> Layout {
        Layout::array::<T>(*header).unwrap()
    }

    unsafe fn mem_to_rcbox(&self, mem: P<u8>) -> P<ManualRcBox<[T]>> {
        let mem = mem.as_ptr();
        let ptr = ptr::slice_from_raw_parts(mem as *mut T, self.len());
        P::new(ptr as _).unwrap()
    }

    unsafe fn copy_to(&self, mut ptr: P<ManualRcBox<[T]>>) {
        ptr::copy_nonoverlapping(
            self.as_ptr(),
            &mut ptr.as_mut().value as *mut [T] as *mut T,
            self.len()
        );
    }
}

impl ManualRcBoxable for str {
    type Header = usize;

    fn header(&self) -> Self::Header {
        self.len()
    }

    fn layout(&self) -> Layout {
        Layout::for_value(self)
    }

    fn layout_from_header(header: &Self::Header) -> Layout {
        Layout::for_value(header)
    }

    unsafe fn mem_to_rcbox(&self, mem: P<u8>) -> P<ManualRcBox<str>> {
        let mem = mem.as_ptr();
        let ptr = ptr::slice_from_raw_parts(mem as *mut u8, self.len());
        P::new(ptr as _).unwrap()
    }

    unsafe fn copy_to(&self, mut ptr: P<ManualRcBox<str>>) {
        ptr::copy_nonoverlapping(
            self.as_ptr(),
            &mut ptr.as_mut().value as *mut str as *mut u8,
            self.len()
        );
    }
}

