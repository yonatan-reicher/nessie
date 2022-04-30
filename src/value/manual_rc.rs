//! Provides the `ManualRc` type, a manual reference-counting pointer type.

use std::alloc::{alloc, dealloc, Layout};
use std::marker::PhantomData;
use std::ptr::{self, NonNull as P};

/// A heap-allocated value of type `T` which is immutable and must be counted
/// by hand.
pub struct ManualRc<T>
where
    T: ?Sized + ManualRcBoxable,
{
    ptr: P<u8>,
    phantom: PhantomData<T>,
}

pub type RefCount = usize;

/// A value that can be put inside a `ManualRc`.
pub trait ManualRcBoxable {
    type Header;

    fn box_header(&self) -> Self::Header;

    fn layout(header: &Self::Header) -> Layout;

    unsafe fn from_ptr(header: &Self::Header, mem: P<u8>) -> P<ManualRcBox<Self>>;
}

struct ManualRcBoxHead<T>
where
    T: ?Sized + ManualRcBoxable,
{
    ref_count: RefCount,
    header: T::Header,
}

#[repr(C)]
pub struct ManualRcBox<T>
where
    T: ?Sized + ManualRcBoxable,
{
    head: ManualRcBoxHead<T>,
    value: T,
}

impl<T> Clone for ManualRc<T>
where
    T: ?Sized + ManualRcBoxable,
{
    fn clone(&self) -> Self {
        ManualRc {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> Copy for ManualRc<T> where T: ?Sized + ManualRcBoxable {}

impl<T> ManualRc<T>
where
    T: ?Sized + ManualRcBoxable,
{
    pub(super) unsafe fn new_uninit(header: T::Header) -> Self {
        #[cfg(debug_assertions)]
        println!("Allocating");

        let layout = {
            Layout::new::<ManualRcBoxHead<T>>()
                .extend(T::layout(&header))
                .unwrap()
                .0
                .pad_to_align()
        };
        let ptr = P::new(alloc(layout)).unwrap();
        {
            let mut ptr = T::from_ptr(&header, ptr);
            ptr.as_mut().head.ref_count = 1;
            (&mut ptr.as_mut().head.header as *mut T::Header).write(header);
        }
        ManualRc {
            ptr,
            phantom: PhantomData,
        }
    }

    /// Returns a pointer to the memory managed by this `ManualRc`.
    pub fn ptr(&self) -> P<u8> {
        self.ptr
    }

    unsafe fn head(&self) -> P<ManualRcBoxHead<T>> {
        self.ptr.cast()
    }

    pub unsafe fn header(&self) -> &T::Header {
        &self.head().as_ref().header
    }

    pub unsafe fn ref_count(&self) -> RefCount {
        self.head().as_ref().ref_count
    }

    pub unsafe fn ref_count_mut(&mut self) -> &mut RefCount {
        &mut self.head().as_mut().ref_count
    }

    unsafe fn fat_ptr(&self) -> P<ManualRcBox<T>> {
        T::from_ptr(self.header(), self.ptr)
    }

    /// Returns a mutable reference to the contained value.
    pub unsafe fn get_mut(&mut self) -> &mut T {
        &mut self.fat_ptr().as_mut().value
    }

    /// Returns a reference to the contained value.
    pub unsafe fn get(&self) -> &T {
        &self.fat_ptr().as_ref().value
    }

    /// Increases the reference count by one.
    pub unsafe fn inc_ref(&mut self) {
        *self.ref_count_mut() += 1;
    }

    /// Decreases the reference count by one.
    /// # Safety
    /// The caller must ensure that the reference count is greater than zero.
    pub unsafe fn dec_ref(&mut self) {
        *self.ref_count_mut() -= 1;
        if self.ref_count() == 0 {
            // Drop the value and free the memory

            #[cfg(debug_assertions)]
            println!("Deallocating");

            ptr::drop_in_place(self.get_mut());
            dealloc(self.ptr.as_ptr(), T::layout(self.header()));
        }
    }
}

impl<T> ManualRcBoxable for T {
    type Header = ();

    fn box_header(&self) -> Self::Header {
        ()
    }

    fn layout(_: &Self::Header) -> Layout {
        Layout::new::<T>()
    }

    unsafe fn from_ptr(_: &Self::Header, mem: P<u8>) -> P<ManualRcBox<T>> {
        mem.cast()
    }
}

impl<T> ManualRc<T>
where
    T: ManualRcBoxable,
{
    pub unsafe fn new(value: T) -> Self {
        let mut rc = Self::new_uninit(value.box_header());
        (rc.get_mut() as *mut T).write(value);
        rc
    }
}

impl<T> ManualRcBoxable for [T]
where
    T: Copy,
{
    type Header = usize;

    fn box_header(&self) -> Self::Header {
        self.len()
    }

    fn layout(header: &Self::Header) -> Layout {
        Layout::array::<T>(*header).unwrap()
    }

    unsafe fn from_ptr(header: &Self::Header, mem: P<u8>) -> P<ManualRcBox<[T]>> {
        let mem = mem.as_ptr();
        let ptr = ptr::slice_from_raw_parts(mem as *mut T, *header);
        P::new(ptr as _).unwrap()
    }
}

impl<T> ManualRc<[T]>
where
    T: Copy,
{
    pub unsafe fn from_slice(arr: &[T]) -> Self {
        let mut rc = Self::new_uninit(arr.box_header());
        ptr::copy_nonoverlapping(
            arr.as_ptr(),
            rc.get_mut().as_mut_ptr(),
            arr.len(),
        );
        rc
    }
}

impl ManualRcBoxable for str {
    type Header = usize;

    fn box_header(&self) -> Self::Header {
        self.len()
    }

    fn layout(header: &Self::Header) -> Layout {
        Layout::array::<u8>(*header).unwrap()
    }

    unsafe fn from_ptr(header: &Self::Header, mem: P<u8>) -> P<ManualRcBox<str>> {
        let mem = mem.as_ptr();
        let ptr = ptr::slice_from_raw_parts(mem as *mut u8, *header);
        P::new(ptr as _).unwrap()
    }
}

impl ManualRc<str> {
    pub unsafe fn from_str(s: &str) -> Self {
        let mut rc = Self::new_uninit(s.box_header());
        ptr::copy_nonoverlapping(
            s.as_ptr(),
            rc.get_mut().as_mut_ptr(),
            s.len(),
        );
        rc
    }
}
