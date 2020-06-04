pub enum Value {
    StringValue(String),
    NumberValue(f32),
    ArrayValue(Vec<Value>)
}



fn id_to_char(id: usize) -> char {
    assert!(id < 26, format!("Index {} out of range 0..26", id));
    ((id as u8) + b'A') as char
}

pub fn id_to_array_name(id: usize) -> String {
    format!("{}", id_to_char(id))
}

pub fn array_name_to_id(name: &str) -> usize {
    let bytes = name.as_bytes();

    assert_eq!(bytes.len(), 1);

    let id = (bytes[0] - b'A') as usize;
    debug_assert!(id < 26, "Failed to get valid index for array variable {}", id);

    id
}

pub fn def_name_to_id(name: &str) -> usize {
    let bytes = name.as_bytes();

    assert_eq!(bytes.len(), 3);

    let id = (bytes[2] - b'A') as usize;
    debug_assert!(id < 26, "Failed to get valid index for array variable {}", id);

    id
}

pub fn id_to_def_name(id: usize) -> String {
    format!("FN{}", id_to_char(id))
}

pub fn id_to_string_name(id: usize) -> String {
    format!("{}$", id_to_char(id))
}

pub fn string_name_to_id(name: &str) -> usize {
    let bytes = name.as_bytes();

    assert_eq!(bytes.len(), 2);
    assert_eq!(bytes[1], b'$');

    let id = (bytes[0] - b'A') as usize;
    debug_assert!(id < 26, "Failed to get valid index for string variable {}", id);

    id
}

pub fn id_to_num_name(id: usize) -> String {
    assert!(id < (26 * 11), "Number id out of range {}", 26 * 11);
    match id < 26 {
        true => format!("{}", id_to_char(id)), // A..Z
        _ => format!("{}{}", id_to_char((id - 26) / 10), ((id - 26) % 10)) // A0.. Z9
    }
}

pub fn num_name_to_id(name: &str) -> usize {
    let bytes = name.as_bytes();

    assert!(bytes.len() >= 1, "Number name {} must be at least one byte long", name);

    let mut id = (bytes[0] - b'A') as usize;

    if bytes.len() > 1 {
        id = 26 + (id * 10) + ((bytes[1] - b'0') as usize);
    }

    debug_assert!(id < 26 * 11, "Failed to get valid index for numeric variable {}", id);

    id
}
