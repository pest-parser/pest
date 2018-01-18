use std::hash::Hasher;

// Hash a fat pointer.
// From https://github.com/mbrubeck/rust/blob/c2c1910d69086827629d37deb5ce6a2febdb36fd/src/libcore/hash/mod.rs#L674-L679.
// Should be available natively in 1.23 (just use `(my_str as *const str).hash(state)`).
pub fn hash_str<H: Hasher>(p: *const str, state: &mut H) {
    let (a, b) = unsafe {
        *(p as *const (usize, usize))
    };
    state.write_usize(a);
    state.write_usize(b);
}
