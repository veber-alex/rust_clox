use std::{ptr, slice};

use crate::{
    memory::{allocate_memory, free_array_memory, grow_capacity},
    object::ObjString,
    value::Value,
};

const TABLE_MAX_LOAD: f32 = 0.75;

// TODO: Is this really better then std HashMap with a decent algo?
pub struct Table {
    count: usize,
    capacity: usize,
    entries: *mut Entry,
}

// TODO: create enum with 3 states, occupied, empty, tombstone
#[derive(Clone, Copy)]
pub struct Entry {
    key: *mut ObjString,
    value: Value,
}

impl Table {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            entries: ptr::null_mut(),
        }
    }

    fn with_capacity(capacity: usize) -> Self {
        let new_table = Table {
            count: 0,
            capacity,
            entries: allocate_memory(capacity),
        };

        // Zero out new table
        // TODO: This can be just brute force zeroed ? memeset ? cmalloc?
        for i in 0..capacity {
            // Safety: We allocated enough memory above
            unsafe {
                let entry = new_table.entries.add(i);
                (*entry).key = ptr::null_mut();
                (*entry).value = Value::Nil;
            }
        }

        new_table
    }
}

pub fn table_set(table: *mut Table, key: *mut ObjString, value: Value) -> bool {
    // Safety: Table is valid
    unsafe {
        if (*table).count + 1 > ((*table).capacity as f32 * TABLE_MAX_LOAD) as usize {
            let capacity = grow_capacity((*table).capacity);
            adjust_table(table, capacity);
        }

        let entry = find_entry(table, key);
        let is_new_key = (*entry).key.is_null();

        if is_new_key && (*entry).value == Value::Nil {
            (*table).count += 1;
        }

        (*entry).key = key;
        (*entry).value = value;

        is_new_key
    }
}

pub fn table_get(table: *mut Table, key: *mut ObjString, value: *mut Value) -> bool {
    unsafe {
        if (*table).count == 0 {
            return false;
        }

        let entry = find_entry(table, key);
        if (*entry).key.is_null() {
            return false;
        }

        *value = (*entry).value;
        true
    }
}

pub fn table_delete(table: *mut Table, key: *mut ObjString) -> bool {
    unsafe {
        if (*table).count == 0 {
            return false;
        }

        let entry = find_entry(table, key);
        if (*entry).key.is_null() {
            return false;
        }

        (*entry).key = ptr::null_mut();
        (*entry).value = Value::Boolean(true);

        true
    }
}

// TODO: Try to use refernces here
pub fn find_entry(table: *mut Table, key: *mut ObjString) -> *mut Entry {
    unsafe {
        let mut index = (*key).hash as usize % (*table).capacity;
        let mut tombstone: *mut Entry = ptr::null_mut();
        loop {
            let entry = (*table).entries.add(index);
            if (*entry).key.is_null() {
                if (*entry).value == Value::Nil {
                    // Empty entry
                    return if !tombstone.is_null() {
                        tombstone
                    } else {
                        entry
                    };
                } else {
                    // Found tombstone
                    if tombstone.is_null() {
                        tombstone = entry;
                    }
                }
            } else if (*entry).key == key {
                // Found the key
                return entry;
            }

            index = (index + 1) % (*table).capacity;
        }
    }
}

fn adjust_table(table: *mut Table, new_capacity: usize) {
    let mut new_table = Table::with_capacity(new_capacity);

    // Safety: Table entires are valid
    unsafe {
        // Copy over existing entires
        for i in 0..(*table).capacity {
            let entry = (*table).entries.add(i);

            if (*entry).key.is_null() {
                continue;
            }

            let dest = find_entry(&mut new_table, (*entry).key);
            (*dest).key = (*entry).key;
            (*dest).value = (*entry).value;
            new_table.count += 1;
        }

        // Free existing table and replace it with new one
        free_table(table);
        *table = new_table;
    }
}

pub fn table_add_all(from: *const Table, to: *mut Table) {
    // Safety: both tables are valid
    unsafe {
        for i in 0..(*from).capacity {
            let entry = (*from).entries.add(i);
            if !(*entry).key.is_null() {
                table_set(to, (*entry).key, (*entry).value);
            }
        }
    }
}

pub fn free_table(table: *mut Table) {
    unsafe {
        free_array_memory((*table).entries, (*table).capacity);
    }
}

pub fn table_find_string(
    table: *const Table,
    ptr: *const u8,
    len: usize,
    hash: u32,
) -> *mut ObjString {
    unsafe {
        if (*table).count == 0 {
            return ptr::null_mut();
        }

        let mut index = hash as usize % (*table).capacity;
        loop {
            let entry = (*table).entries.add(index);
            let key = (*entry).key;
            if key.is_null() {
                // Stop if we find an empty non-tombstone entry.
                if (*entry).value == Value::Nil {
                    return ptr::null_mut();
                }
            } else if (*key).len == len
                && (*key).hash == hash
                && slice::from_raw_parts(ptr, len) == slice::from_raw_parts((*key).ptr, len)
            {
                return (*entry).key;
            }
            index = (index + 1) % (*table).capacity;
        }
    }
}
