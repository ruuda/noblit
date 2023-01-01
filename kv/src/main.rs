
use std::future::{Future};
use std::pin::{Pin};
use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};

struct Runtime;

#[derive(Clone)]
struct WakerImpl;

impl WakerImpl {
    fn wake(&self) {}
    fn wake_by_ref(&self) {}
    fn drop(&self) {}

    fn as_raw(&self) -> RawWaker {
        const VTABLE: RawWakerVTable = RawWakerVTable::new(
            // `clone`
            |ptr| {
                let w: &WakerImpl = unsafe { &*(ptr as *const WakerImpl) }; 
                println!("waker::clone");
                w.clone().as_raw()
            },
            // `wake`
            |ptr| {
                let w: &WakerImpl = unsafe { &*(ptr as *const WakerImpl) }; 
                println!("waker::wake");
                w.wake()
            },
            // `wake_by_ref`
            |ptr| {
                let w: &WakerImpl = unsafe { &*(ptr as *const WakerImpl) }; 
                println!("waker::wake_by_ref");
                w.wake_by_ref()
            },
            // `drop`
            |ptr| { 
                let w: &WakerImpl = unsafe { &*(ptr as *const WakerImpl) }; 
                println!("waker::drop");
                w.drop()
            },
        );
        let self_ptr = self as *const WakerImpl;
        RawWaker::new(self_ptr.cast(), &VTABLE)
    }

    fn as_waker(&self) -> Waker {
        let raw = self.as_raw();
        unsafe { Waker::from_raw(raw) }
    }
}

impl Runtime {
    pub fn new() -> Runtime {
        Runtime
    }

    pub fn execute<T, F>(&mut self, mut future: F) -> T 
        where F: Future<Output = T> + Unpin
    {
        let waker_impl = WakerImpl;
        let waker = waker_impl.as_waker();
        let mut context = Context::from_waker(&waker);
        loop {
            // let p: Pin<&mut F> = Pin::new(&mut future);
            match Pin::new(&mut future).poll(&mut context) {
                Poll::Pending => {
                    // TODO: Execute new work that may have been enqueued.
                }
                Poll::Ready(v) => return v,
            }
        }
    }
}

fn main() {
    let mut runtime = Runtime::new();
    let f_result = std::future::ready("Hello");
    let result = runtime.execute(f_result);
    println!("Result: {}", result);
}
