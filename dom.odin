
package gon

import "core:fmt"
import "core:strconv"

// do the basic in-situ parsing thing
DOM_File :: struct {
    fields : [dynamic]DOM_Field,
}

DOM_Field :: struct {
    parent   : int,
    name     : string, 
    type     : Field_Type,
    value    : string,
    children : [dynamic]int,
}

// TOOD: probably implement a parse context for the DOM version as well so that we can track some params such as whether to copy strings 

DOM_parse_file :: proc(str: string) -> (DOM_File, bool) {
    str := str
    using gon_file : DOM_File
    root : DOM_Field = { 
        parent = 0, 
        name   = "root",
        type   = .OBJECT,
    }
    append(&fields, root)
    if !DOM_parse_object(&gon_file, 0, &str) {
        DOM_file_destroy(&gon_file)
        return {}, false
    }
    return gon_file, true
}

DOM_parse_object :: proc(gon_file: ^DOM_File, parent: int, str: ^string) -> bool {
    using gon_file

    next_token_type : Token_Type
    next_token      : string

    for {
        field : DOM_Field
        field.parent = parent
        field_index := len(fields)

        // read field name
        if fields[parent].type != .ARRAY {
            next_token_type, next_token = get_next_token(str)
            #partial switch next_token_type {
                case .EOF:
                return true
                case .STRING:
                field.name = next_token
                case .OBJECT_END:
                if fields[parent].type != .OBJECT {
                    fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token)
                    return false
                }
                return true
                case:
                fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token)
                return false
            }
        }

        // read field value and append
        next_token_type, next_token = get_next_token(str)
        #partial switch next_token_type {
            case .STRING:
                field.type = .FIELD
                field.value = next_token
            case .OBJECT_BEGIN:
                field.type = .OBJECT
            case .ARRAY_BEGIN:
                field.type = .ARRAY
            case .ARRAY_END:
                if fields[parent].type != .ARRAY {
                fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token);
                return false
                }
                return true
            case:
                fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token);
                return false
        }

        append(&fields, field)
        append(&fields[parent].children, field_index)
        if field.type == .OBJECT || field.type == .ARRAY {
            DOM_parse_object(gon_file, field_index, str) or_return
        }
    }
}

DOM_file_destroy :: proc(using file: ^DOM_File) {
    for &f in fields {
        DOM_field_destroy(&f)
    }
    delete(fields)
}

DOM_field_destroy :: proc(using field: ^DOM_Field) {
    delete(children)
}

get_child_by_name :: proc(using gon_file: ^DOM_File, parent: int, name: string) -> (int, bool) {
    if gon_file == nil || parent < 0 || parent >= len(fields) || fields[parent].type == .FIELD {
        return 0, false
    }
    for i in fields[parent].children {
        if fields[i].name == name {
        return i, true
        }
    }
    return 0, false
}

// searches the file an arbitrary depth and pulls out an index to the desired field
get_field_by_address :: proc(using gon_file: ^DOM_File, address: []string) -> (int, bool) {
    if gon_file == nil do return 0, false
    addr_idx : int
    parent   : int
    child    : int
    found    : bool
    for {
        fmt.println(".")
        fmt.println("parent is", parent, fields[parent])
        fmt.println("searching for", address[addr_idx])
        #partial switch fields[parent].type {
            case .FIELD:
                fmt.println("Parent type cannot be a field.")
                return 0, false
            case .OBJECT:
                fmt.println("o", addr_idx)
                child, found = get_child_by_name(gon_file, parent, address[addr_idx])
                if !found {
                    fmt.println("No field found at address ", address[:addr_idx])
                    return 0, false
                }
                fmt.println("found", address[addr_idx], "at", address[:addr_idx])
            case .ARRAY:
                fmt.println("a", addr_idx)
                index := strconv.atoi(address[addr_idx])
                if index < 0 || index >= len(fields[parent].children) {
                    fmt.println("Invalid index", index, "into", address[:addr_idx])
                    return 0, false
                }
                fmt.println("found", address[addr_idx], "at", address[:addr_idx])
                child = fields[parent].children[index]
        }
        addr_idx += 1
        if addr_idx == len(address) {
            return child, true
        }
        parent = child
    }
    return 0, false
}

get_value_or_default :: proc(using gon_file: ^DOM_File, parent: int, name: string, default: string) -> string {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD do return default
    return fields[child].value
}

get_int_or_default :: proc(using gon_file: ^DOM_File, parent: int, name: string, default: int) -> int {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD do return default
    return strconv.atoi(fields[child].value)
}

get_float_or_default :: proc(using gon_file: ^DOM_File, parent: int, name: string, default: f64) -> f64 {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD do return default
    return strconv.atof(fields[child].value)
}

try_get_value :: proc(using gon_file: ^DOM_File, parent: int, name: string) -> (string, bool) {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD do return {}, false
    return fields[child].value, true
}

try_get_int :: proc(using gon_file: ^DOM_File, parent: int, name: string) -> (int, bool) {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD do return 0, false
    return strconv.atoi(fields[child].value), true
}

try_get_float :: proc(using gon_file: ^DOM_File, parent: int, name: string) -> (f64, bool) {
    child, ok := get_child_by_name(gon_file, parent, name)
    if !ok || fields[child].type != .FIELD  do return 0, false
    return strconv.atof(fields[child].value), true
}

