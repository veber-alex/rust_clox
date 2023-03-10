use crate::{
    object::{ObjKind, ObjPtr},
    vm::VM,
};

pub fn free_objects(vm: &mut VM) {
    let mut obj = vm.objects;
    while !obj.as_ptr().is_null() {
        let next = obj.next();
        free_object(obj);
        obj = next;
    }
}

fn free_object(obj: ObjPtr) {
    match obj.kind() {
        ObjKind::OBJ_STRING => {
            // Safety: obj is a valid ObjString due to kind check
            unsafe {
                let string = obj.as_string();
                let string_str = (*string).ptr;
                free(string_str);
                free(string);
            }
        }
    }
}

// Safety: T was allocated on the heap with Box
unsafe fn free<T: ?Sized>(ptr: *mut T) {
    // Safety: T was allocated on the heap with Box
    unsafe { Box::from_raw(ptr) };
}
