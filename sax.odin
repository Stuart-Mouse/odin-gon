
package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:unicode/utf8"

SAX_Field :: struct {
    name         : string,
    value        : string,
    type         : Field_Type,
    data_binding : any,
    parent       : ^SAX_Field,
    index        : int,
    // flags        : SAX_Field_Flags,
}

Data_Binding :: struct {
    binding     : any,
    field_path  : string,

    _field_path : []string,
    _path_depth : int,
}

SAX_Parse_Context :: struct {
    file          : string,
    data_bindings : []Data_Binding,
    event_handler : SAX_Event_Handler,

    _field_depth  : int,
}

SAX_Return_Code :: enum {
    ERROR = 0,
    OK    = 1, // TODO: rename to OK

    SKIP_BINDING,
}

SAX_Event_Handler_Proc :: proc(^SAX_Parse_Context, ^SAX_Field) -> SAX_Return_Code

SAX_Event_Handler :: struct {
    object_begin,
    object_end,
    field_read,
    field_data_bind,
    parent_data_bind : SAX_Event_Handler_Proc
}

prep_data_bindings :: proc(data_bindings: []Data_Binding) {
    for &b in data_bindings {
        b._field_path = strings.split(b.field_path, "/", allocator = context.temp_allocator)
    }
}

SAX_parse_file :: proc(using parse_context: ^SAX_Parse_Context) -> bool {
    root := SAX_Field {
        name   = "root",
        type   = .OBJECT,
        parent = nil,
    }

    // TODO: we should probably verify that the path strings actually conform to the standard for gon strings

    // split the paths for all data bindings before parsing
    for &b in data_bindings {
        if b.field_path == "" {
            // an empty path means we are binding to the root of the file
            // we can only have one binding to the root of the file!
            if root.data_binding == nil {
                root.data_binding = b
            } else {
                fmt.println("Unable to bind multiple values to the root object!")
                return false
            }
        } else {
            // standard binding, split path 
            b._field_path = strings.split(b.field_path, "/", allocator = context.temp_allocator)
        }
    }

    return SAX_parse_object(parse_context, &root)
}

SAX_parse_object :: proc(using parse_context: ^SAX_Parse_Context, parent: ^SAX_Field) -> bool {
    next_token_type : Token_Type
    next_token      : string

    // refers to the index of the field within the scope of the current parent object
    field_index := 0

    // process a single field per iteration
    for ;; field_index += 1 {
        field: SAX_Field = {
            parent = parent,
            index  = field_index,
        }
        
        // read field name
        if parent == nil || parent.type != .ARRAY {
            next_token_type, next_token = get_next_token(&file)
            #partial switch next_token_type {
                case .EOF:
                    return true
                case .STRING:
                    field.name = next_token
                case .OBJECT_END:
                    if parent.type != .OBJECT {
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
        next_token_type, next_token = get_next_token(&file)
        #partial switch next_token_type {
            case .STRING:
                field.type = .FIELD
                field.value = next_token
            case .OBJECT_BEGIN:
                field.type = .OBJECT
            case .ARRAY_BEGIN:
                field.type = .ARRAY
            case .ARRAY_END:
                if parent.type != .ARRAY {
                    fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token);
                    return false
                }
                return true
            case:
                fmt.printf("GON parse error: Unexpected %v token \"%v\".\n", next_token_type, next_token);
                return false
        }

        L_Direct_Binding: {
            event_result: SAX_Return_Code = .OK;
            if event_handler.field_read != nil {
                event_result = event_handler.field_read(parse_context, &field)
                if event_result == .ERROR do return false;
            }
            if event_result == .SKIP_BINDING {
                break L_Direct_Binding
            }

            for &b in data_bindings {
                // check that field address matched up to this point
                // also skip completed matches
                if b._path_depth < _field_depth || 
                len(b._field_path) <= _field_depth {
                    continue
                }

                // check if _field_path[_field_depth] is a match
                if field.name != b._field_path[_field_depth] {
                    continue
                }
                b._path_depth += 1;

                // check if we've matched the entire field address
                if len(b._field_path) == b._path_depth {
                    b._path_depth = -1; // deactivate the binding so that it will be skipped in future checks

                    if !set_field_data_binding(parse_context, &field, b.binding) {
                        return false
                    }
                }
            }
        }
        
        L_Indirect_Binding: if parent != nil && parent.data_binding != nil {
            event_result: SAX_Return_Code = .OK;
            if event_handler.parent_data_bind != nil {
                event_result = event_handler.parent_data_bind(parse_context, &field)
                if event_result == .ERROR do return false
            }
            if event_result == .SKIP_BINDING {
                break L_Indirect_Binding
            }

            parent_binding_ti := runtime.type_info_base(type_info_of(parent.data_binding.id))
            #partial switch &parent_tiv in parent_binding_ti.variant {
                case runtime.Type_Info_Map:
                    assert(parent.type == .OBJECT)
                    raw_map := cast(^runtime.Raw_Map) parent.data_binding.data
                    
                    // This is a leak, need to figure out how to give the user some idea that he needs to clone these strings and manage them himself.
                    name_copy := strings.clone(field.name)
                    key       := cast(rawptr) &name_copy
                    
                    runtime.__dynamic_map_check_grow(raw_map, parent_tiv.map_info)
                    
                    // allocate empty space that can be safely memcopied from
                    // this has to be done because apparently there's no way to insert a hash dynamically without passing a value
                    empty_value := cast(rawptr) raw_data(make([]u8, parent_tiv.value.size, context.temp_allocator))

                    value := runtime.__dynamic_map_set_without_hash(
                        raw_map, parent_tiv.map_info, key, empty_value,
                    )
                    
                    if !set_field_data_binding(parse_context, &field, any{rawptr(value), parent_tiv.value.id}) {
                        return false
                    }
            
                case runtime.Type_Info_Bit_Set:
                    assert(parent.type == .ARRAY)
                    if !set_field_data_binding(parse_context, &field, parent.data_binding) {
                        return false
                    }

                case runtime.Type_Info_Dynamic_Array:
                    assert(parent.type == .ARRAY) // TODO
                    field_data_binding := array_add_any(parent.data_binding) or_return
                    if !set_field_data_binding(parse_context, &field, field_data_binding) {
                        return false
                    }

                case runtime.Type_Info_Array:
                    assert(parent.type == .ARRAY) // TODO
                    raw_slice := cast(^runtime.Raw_Slice) parent.data_binding.data
                    if field_index >= parent_tiv.count {
                        fmt.println("Unable to add to array, ran out of space.")
                        return false
                    } else {
                        elem_ti := runtime.type_info_base(parent_tiv.elem)
                        field_data_binding := any {
                            data = mem.ptr_offset(cast(^u8)parent.data_binding.data, elem_ti.size * field_index),
                            id   = elem_ti.id,
                        }
                        if !set_field_data_binding(parse_context, &field, field_data_binding) {
                            return false
                        }
                    }

                case runtime.Type_Info_Slice:
                    assert(parent.type == .ARRAY) // TODO
                    raw_slice := cast(^runtime.Raw_Slice) parent.data_binding.data
                    if field_index >= raw_slice.len {
                        fmt.println("Unable to add to slice, ran out of space.")
                        return false
                    } else {
                        elem_ti := runtime.type_info_base(parent_tiv.elem)
                        field_data_binding := any {
                            data = mem.ptr_offset(cast(^u8)raw_slice.data, elem_ti.size * field_index),
                            id   = elem_ti.id,
                        }
                        if !set_field_data_binding(parse_context, &field, field_data_binding) {
                            return false
                        }
                    }

                case runtime.Type_Info_Struct:
                    member : reflect.Struct_Field
                    #partial switch parent.type {
                        case .ARRAY  : member = reflect.struct_field_at(parent_binding_ti.id, field_index)
                        case .OBJECT : member = reflect.struct_field_by_name(parent_binding_ti.id, field.name) 
                    }
                    if member != {} {
                        data_binding: any = {
                            data = mem.ptr_offset(cast(^u8)parent.data_binding.data, member.offset),
                            id   = member.type.id,
                        }
                        if !set_field_data_binding(parse_context, &field, data_binding) {
                            return false
                        } 
                    }
            }
        }

        // recurse for object / array
        if field.type == .OBJECT || field.type == .ARRAY {
            _field_depth += 1

            event_result: SAX_Return_Code = .OK;
            if event_handler.object_begin != nil {
                event_result = event_handler.object_begin(parse_context, &field)
                if event_result == .ERROR do return false
            }

            SAX_parse_object(parse_context, &field) or_return

            if event_handler.object_end != nil {
                event_result = event_handler.object_end(parse_context, &field)
                if event_result == .ERROR do return false
            }

            _field_depth -= 1

            for &b in data_bindings {
                if b._path_depth > _field_depth {
                    b._path_depth -= 1
                }
            }
        }
    }
}

set_field_data_binding :: proc(using parse_context: ^SAX_Parse_Context, field: ^SAX_Field, data_binding: any) -> bool {
    // Set the data binding right away. 
    // This data binding can be removed/canceled by the event handler or type-specific handling.
    field.data_binding = data_binding

    // handle field_data_bind event
    event_result: SAX_Return_Code = .OK;
    if event_handler.field_data_bind != nil {
        event_result = event_handler.field_data_bind(parse_context, field);
        if event_result == .ERROR {
            return false
        }
        if event_result == .SKIP_BINDING {
            field.data_binding = {}
            return true
        }
    }
    
    // type-specific handling
    type_io_data, found := IO_Data_Lookup[ield.data_binding.id]
    if found {
        
    }    

    binding_ti := runtime.type_info_base(type_info_of(data_binding.id))
    if field.type == .FIELD {
        // restrict types to which we can bind
        #partial switch tiv in binding_ti.variant {
            case runtime.Type_Info_Integer:
            case runtime.Type_Info_Float:
            case runtime.Type_Info_Enum:
            case runtime.Type_Info_String:
            case runtime.Type_Info_Bit_Set:
            case runtime.Type_Info_Boolean:
            case runtime.Type_Info_Array:
                if tiv.elem.size != 1 {
                    fmt.println("Unable to bind field to data of type:", data_binding.id)
                    return false
                }
            case runtime.Type_Info_Dynamic_Array:
                if tiv.elem.size != 1 {
                    fmt.println("Unable to bind field to data of type:", data_binding.id)
                    return false
                }
            case runtime.Type_Info_Slice:
                if tiv.elem.size != 1 {
                    fmt.println("Unable to bind field to data of type:", data_binding.id)
                    return false
                }
            case: 
                fmt.println("Unable to bind field to data of type:", data_binding.id)
                return false
        }
        if !set_value_from_string(data_binding, field.value) {
            return false
        }
    }
    else { // field type is either object or array
        #partial switch tiv in binding_ti.variant {
            case runtime.Type_Info_Array:
            case runtime.Type_Info_Dynamic_Array:
            case runtime.Type_Info_Slice:
            case runtime.Type_Info_Bit_Set:
            case runtime.Type_Info_Map:
                // no op
            case runtime.Type_Info_Struct:
                // mem.set(data_binding.data, 0, binding_ti.size)
            case:
                fmt.println("Unable to bind object or array to data of type:", data_binding.id)
                return false
        }
    }
    return true // ?
}


set_value_from_string :: proc(value: any, text: string) -> bool {
    using runtime
    if text == "" {
        return true
    }

    ti := type_info_of(value.id)
    if _, ok := ti.variant.(Type_Info_Named); ok {
        ti = type_info_base(ti)
    }

    #partial switch &tiv in ti.variant {
        case Type_Info_Integer:
            if !dynamic_int_cast(value, strconv.atoi(text)) {
                return false
            }
            return true

        case Type_Info_Float:
            if !dynamic_float_cast(value, strconv.atof(text)) {
                return false
            }
            return true

        case Type_Info_Enum:
            for name, index in tiv.names {
                if name == text {
                    switch ti.size {
                        case 1: (cast(^u8 )value.data)^ = auto_cast tiv.values[index]
                        case 2: (cast(^u16)value.data)^ = auto_cast tiv.values[index]
                        case 4: (cast(^u32)value.data)^ = auto_cast tiv.values[index]
                        case 8: (cast(^u64)value.data)^ = auto_cast tiv.values[index]      
                    }
                    return true
                }
            }
            return true

        case Type_Info_Bit_Set:
            i64_value: u64
            dynamic_int_cast(i64_value, value)
            bytes := transmute(^[8]byte) &i64_value

            elem_ti := type_info_base(tiv.elem)
            #partial switch elem_tiv in elem_ti.variant {
                case Type_Info_Integer:
                    bit := cast(i64) strconv.atoi(text)
                    if bit >= tiv.lower && bit <= tiv.upper {
                        bit -= tiv.lower
                        bytes[bit / 8] |= u8(1 << u64(bit % 8))
                    }
                case Type_Info_Rune:
                    rune_value, _ := utf8.decode_rune_in_string(text)
                    bit := cast(i64) rune_value
                    if bit >= tiv.lower && bit <= tiv.upper {
                        bit -= tiv.lower
                        bytes[bit / 8] |= u8(1 << u64(bit % 8))
                    }
                case Type_Info_Enum:
                    for name, index in elem_tiv.names {
                        if index >= int(tiv.lower) && index <= int(tiv.upper) && name == text  {
                            bit := int(elem_tiv.values[index]) - int(tiv.lower)
                            bytes[bit / 8] |= u8(1 << u64(bit % 8))
                        }
                    }
            }

            dynamic_int_cast(value, i64_value)
            return true

        case Type_Info_String:
            string_value := strings.clone(text)
            if tiv.is_cstring {
                (cast(^cstring)value.data)^ = cstring(raw_data(string_value))
            } else {
                (cast(^string)value.data)^ = string_value
            }
            return true

        case Type_Info_Boolean:
            if text[0] == 't' || text[0] == 'T' {
                switch ti.size {
                    case 1: (cast(^b8 )value.data)^ = true
                    case 2: (cast(^b16)value.data)^ = true
                    case 4: (cast(^b32)value.data)^ = true
                    case 8: (cast(^b64)value.data)^ = true
                }
            }
            return true

        case Type_Info_Array:
            if tiv.elem.size != 1 {
                fmt.println("Unsupported type in set_value_from_string():", value.id)
                return true
            }
            if len(text) >= tiv.count { // leave one byte pad on the end so we can null terminate
                fmt.printf("Unable to copy string of len %v to [%v]u8\n", len(text), tiv.count)
                return true
            }
            mem.copy(value.data, raw_data(text), len(text))
            (transmute([^]u8)value.data)[len(text)] = 0 // null terminate
            return true

        case Type_Info_Slice:
            slice      := cast(^runtime.Raw_Slice) value.data
            data       := slice.data
            elem_count := slice.len
            if tiv.elem.size != 1 {
                fmt.println("Unsupported type in set_value_from_string():", value.id)
                return false
            }
            (cast(^string)value.data)^ = strings.clone(text)
            return true
    
        case Type_Info_Dynamic_Array:
            array      := cast(^runtime.Raw_Dynamic_Array) value.data
            elem_count := array.len
            elem_ti    := runtime.type_info_base(tiv.elem)
            if elem_ti.size != 1 {
                fmt.println("Unsupported type in set_value_from_string():", value.id)
                return true
            }
            arr_u8 := transmute(^[dynamic]u8) array
            clear(arr_u8)
            append_elem_string(arr_u8, text)
            return true

        case:
            fmt.println("Unsupported type in set_value_from_string().", value.id)
            return true
    }
    
    return true
}

array_add_any :: proc(array: any) -> (any, bool) {
    ti := type_info_of(array.id)
    if ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array); ok {
        return array_add_any_nocheck(auto_cast array.data, ti_array.elem), true
    }
    return {}, false
}

array_add_any_nocheck :: proc(array: ^runtime.Raw_Dynamic_Array, elem_ti: ^runtime.Type_Info) -> any {
    if array.len >= array.cap {
        reserve   := max(2 * array.cap, 8)
        old_size  := elem_ti.size *  array.cap
        new_size  := elem_ti.size * (array.cap + reserve) 
        allocator := array.allocator != {} ? array.allocator : context.allocator
        array.data, _ = mem.resize(array.data, old_size, new_size, elem_ti.align, allocator)
        array.cap = new_size
    }
    array.len += 1
    return {
        data = mem.ptr_offset(cast(^u8) array.data, array.len * elem_ti.size),
        id   = elem_ti.id, 
    }
}

