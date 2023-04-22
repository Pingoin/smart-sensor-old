use cortex_m::interrupt::{self, Mutex};
use core::cell::RefCell;
use core::ops::DerefMut;

pub struct MutexBox<T> { 
    pub name: &'static str,
    pub mutex: Mutex<RefCell<Option<T>>>,
}

impl<T> MutexBox<T> {
    pub const fn new(name: &'static str) -> Self {
        let mutex= Mutex::new(RefCell::new(None));
        MutexBox {
            name: name,
            mutex: mutex,
        }
    }

    pub fn open_locked<FunctionLocked>(
        &self,
        found: FunctionLocked,
    )
    where
        FunctionLocked: FnOnce(&mut T),
    {
        interrupt::free(|cs| {
            if let Some(ref mut data) = self.mutex.borrow(cs).borrow_mut().deref_mut() {
                found(data);
            }
        });
    }

    pub fn init(&self, data: T) {
        interrupt::free(|cs| {
            self.mutex.borrow(cs).replace(Some(data));
        });
    }
}
