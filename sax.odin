
package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:unicode/utf8"
import "core:math"


SAX_Field :: struct {
    name         : string,
    value        : string,
    type         : Field_Type,
    data_binding : any,
    parent       : ^SAX_Field,
    index        : int,
    io_data      : IO_Data, // not by ref so that it can be modified in callbacks
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
    log           : Log_Proc,

    _field_depth  : int,
}

SAX_Return_Code :: enum {
    ERROR = 0,
    OK    = 1,

    SKIP_BINDING,
}

SAX_Event_Handler_Proc :: proc(^SAX_Parse_Context, ^SAX_Field) -> SAX_Return_Code

SAX_Event_Handler :: struct {
    object_begin,
    object_end,
    field_read,
    data_binding,
    indirect_data_binding : SAX_Event_Handler_Proc
}

print_field_address :: proc(field: ^SAX_Field) {
    f := field
    for f != nil {
        fmt.printf("%v/", f.name)
        f = f.parent
    }
    fmt.println()
}

SAX_parse_file :: proc(using ctxt: ^SAX_Parse_Context) -> bool {
    root := SAX_Field {
        name   = "root",
        type   = .OBJECT,
        parent = nil,
    }

    // ensure that parse context is properly init'd
    if log == nil {
        log = default_log_proc
        if log == nil {
            log = log_stub
        }
    }
    
    // TODO: we should probably verify that the path strings actually conform to the standard for gon strings

    // split the paths for all data bindings before parsing
    for &b in data_bindings {
        if b.field_path == "" {
            // an empty path means we are binding to the root of the file
            // we can only have one binding to the root of the file!
            if root.data_binding == nil {
                root.data_binding = b.binding
            } else {
                log("Unable to bind multiple values to the root object!")
                return false
            }
        } else {
            // standard binding, split path 
            b._field_path = strings.split(b.field_path, "/", allocator = context.temp_allocator)
        }
    }

    return SAX_parse_object(ctxt, &root)
}

SAX_parse_object :: proc(using ctxt: ^SAX_Parse_Context, parent: ^SAX_Field) -> (success: bool) {
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
                        log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                        return false
                    }
                    return true
                case:
                    log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                    return false
            }
        } else {
            field.name = fmt.tprintf("%v[%v]", field.parent.name, field.index)
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
                    log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token);
                    return false
                }
                return true
            case:
                log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token);
                return false
        }

        L_Direct_Binding: {
            event_result: SAX_Return_Code = .OK;
            if event_handler.field_read != nil {
                event_result = event_handler.field_read(ctxt, &field)
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
                b._path_depth += 1

                // check if we've matched the entire field address
                if len(b._field_path) == b._path_depth {
                    b._path_depth = -1                  // deactivate the binding so that it will be skipped in future checks
                    field.data_binding = b.binding      // set the data binding
                }
            }
        }
        
        // Processing for indirect data bindings is still very language-specific.
        // If we create version of this parser for other languages, this should probably be factored into a separate procedure so that it is more apparent that this is not language agnostic like the rest of the procedure.
        // check_for_indirect_bindings() could basically be a SAX_Event_Handler_Proc
        L_Indirect_Binding: if parent != nil && parent.data_binding != nil {
            event_result: SAX_Return_Code = .OK;
            if event_handler.indirect_data_binding != nil {
                event_result = event_handler.indirect_data_binding(ctxt, &field)
                if event_result == .ERROR do return false
            }
            if event_result == .SKIP_BINDING {
                break L_Indirect_Binding
            }

            parent_ti := runtime.type_info_base(type_info_of(parent.data_binding.id))
            #partial switch &parent_tiv in parent_ti.variant {
                case runtime.Type_Info_Map:
                    assert(parent.type == .OBJECT)
                    raw_map := cast(^runtime.Raw_Map) parent.data_binding.data
                    
                    // This is a leak, need to figure out how to give the user some idea 
                    //   that he needs to clone these strings and manage them himself.
                    name_copy := strings.clone(field.name)
                    key       := cast(rawptr) &name_copy
                    
                    runtime.__dynamic_map_check_grow(raw_map, parent_tiv.map_info)
                    
                    // allocate empty space that can be safely memcopied from
                    // this has to be done because apparently there's no way to insert a hash 
                    //   dynamically without passing a value
                    empty_value := cast(rawptr) raw_data(make([]u8, parent_tiv.value.size, context.temp_allocator))

                    value := runtime.__dynamic_map_set_without_hash(
                        raw_map, parent_tiv.map_info, key, empty_value,
                    )
                    
                    field.data_binding = any { rawptr(value), parent_tiv.value.id }
            
                case runtime.Type_Info_Bit_Set:
                    assert(parent.type == .ARRAY)
                    field.data_binding = parent.data_binding

                case runtime.Type_Info_Dynamic_Array:
                    if .PARSE_ARRAY_INDEXED in parent.io_data.parse.flags {
                        assert(parent.type == .OBJECT) // TODO
                        field.index = strconv.atoi(field.name)
                        field.data_binding = array_add_any_at_index(parent.data_binding, field.index)
                    } else {
                        assert(parent.type == .ARRAY) // TODO
                        field.data_binding = array_add_any(parent.data_binding)
                    }
                    
                    if field.data_binding == nil {
                        return false
                    }

                case runtime.Type_Info_Array:
                    if .PARSE_ARRAY_INDEXED in parent.io_data.parse.flags {
                        field.index = strconv.atoi(field.name)
                    }
                    
                    if field.index >= parent_tiv.count {
                        log("Unable to add to array, ran out of space.")
                        return false
                    } else {
                        elem_ti := runtime.type_info_base(parent_tiv.elem)
                        field.data_binding = any {
                            data = mem.ptr_offset(cast(^u8)parent.data_binding.data, elem_ti.size * field.index),
                            id   = parent_tiv.elem.id,
                        }
                    }

                case runtime.Type_Info_Slice:
                    raw_slice := cast(^runtime.Raw_Slice) parent.data_binding.data
                    
                    if .PARSE_ARRAY_INDEXED in parent.io_data.parse.flags {
                        field.index = strconv.atoi(field.name)
                    }
                    
                    if field.index >= raw_slice.len {
                        log("Unable to add to slice, ran out of space.")
                        return false
                    } else {
                        elem_ti := runtime.type_info_base(parent_tiv.elem)
                        field.data_binding = any {
                            data = mem.ptr_offset(cast(^u8)raw_slice.data, elem_ti.size * field.index),
                            id   = parent_tiv.elem.id,
                        }
                    }

                case runtime.Type_Info_Struct:
                    member: reflect.Struct_Field
                    #partial switch parent.type {
                        case .ARRAY : member = reflect.struct_field_at     (parent_ti.id, field.index)
                        case .OBJECT: member = reflect.struct_field_by_name(parent_ti.id, field.name ) 
                    }
                    // NOTE: should we check that member is not name member? 
                    // (No, because name member is only used when struct is within an array.)
                    if member != {} {
                        // TODO: probably need to implement merge proc for io_data struct. 
                        // We may want to automatically merge the type io data with the member io data, 
                        // but how to do this well is unclear. Will just leave it up to the user for now.
                        found: bool
                        field.io_data, found = parent.io_data.member_data[member.name]
                        
                        field.data_binding = any {
                            data = mem.ptr_offset(cast(^u8)parent.data_binding.data, member.offset),
                            id   = member.type.id,
                        }
                    }
            }
        }
        
        // NOTE: Factoring this out here may turn out to improve performance if it gets inlined, 
        //       so perhaps that is another reason to keep it this way going forward.
        if field.data_binding != nil {
            type_io_data, found := IO_Data_Lookup[field.data_binding.id]
            if found {
                field.io_data = type_io_data
            }
            
            if !process_data_binding(ctxt, &field) {
                return false
            }
        }
        
        // recurse for object / array
        if field.type == .OBJECT || field.type == .ARRAY {
            _field_depth += 1

            event_result: SAX_Return_Code = .OK;
            if event_handler.object_begin != nil {
                event_result = event_handler.object_begin(ctxt, &field)
                if event_result == .ERROR do return false
            }

            SAX_parse_object(ctxt, &field) or_return

            if event_handler.object_end != nil {
                event_result = event_handler.object_end(ctxt, &field)
                if event_result == .ERROR do return false
            }

            _field_depth -= 1

            for &b in data_bindings {
                if b._path_depth > _field_depth {
                    b._path_depth -= 1
                }
            }
        }
        
        // TODO: else field type was invalid and we need to error out
        // will also need to add continues to field and object/array cases
    }
}

/*
    For fields, we will call set_value_from_string().
    For objects and arrays, the action taken will depend on the type of the data binding.
    Because of the need for type-checking, the implementation is language-specific.
    In general, fields can only bind to primitive data types while objects and arrays can only bind to more complex data types such as structs, arrays, etc.
*/
process_data_binding :: proc(using ctxt: ^SAX_Parse_Context, field: ^SAX_Field) -> bool {
    // handle data_binding event
    event_result: SAX_Return_Code = .OK;
    if event_handler.data_binding != nil {
        event_result = event_handler.data_binding(ctxt, field);
        if event_result == .ERROR {
            return false
        }
        if event_result == .SKIP_BINDING {
            field.data_binding = {}
            return true
        }
    }
    
    // not sure if this is actually the optimal place for this
    // depends on how much power we want to give to io_data parse procs at this location
    // but it seems appropriate that these parse procs cut in at the same point as the data bind event handlers
    // Should this be moved into the .FIELD case? prevent user from needing to check the field type, but also prevents custom processing for gon objects/arrays
    if field.io_data.parse.parse_proc != nil {
        return field.io_data.parse.parse_proc(ctxt, field) == .OK
    }
    
    // NOTE: should we move this to before handling data binding event since the binding will not actually occur?
    if .SKIP in field.io_data.parse.flags {
        field.data_binding = {}
        return true
    }
    
    // TODO: convert to a switch on field type, handle invalid cases
    binding_ti := runtime.type_info_base(type_info_of(field.data_binding.id))
    if field.type == .FIELD {
        // restrict types to which we can bind a field
        #partial switch tiv in binding_ti.variant {
            case runtime.Type_Info_Integer:
            case runtime.Type_Info_Float:
            case runtime.Type_Info_Enum:
            case runtime.Type_Info_String:
            case runtime.Type_Info_Bit_Set:
            case runtime.Type_Info_Boolean:
            
            // arrays of bytes/u8 are permitted as single-valued fields so that we can parse them as strings
            case runtime.Type_Info_Array:
                if tiv.elem.size != 1 {
                    log("Unable to bind field \"%v\" to data of type: %v", field.name, field.data_binding.id)
                    print_field_address(field)
                    return false
                }
            case runtime.Type_Info_Dynamic_Array:
                if tiv.elem.size != 1 {
                    log("Unable to bind field \"%v\" to data of type: %v", field.name, field.data_binding.id)
                    print_field_address(field)
                    return false
                }
            case runtime.Type_Info_Slice:
                if tiv.elem.size != 1 {
                    log("Unable to bind field \"%v\" to data of type: %v", field.name, field.data_binding.id)
                    print_field_address(field)
                    return false
                }
                
            case: 
                log("Unable to bind field \"%v\" to data of type: %v", field.name, field.data_binding.id)
                print_field_address(field)
                return false
        }
        if !set_value_from_string(ctxt, field.data_binding, field.value) {
            return false
        }
    }
    else { // field type is either object or array
        #partial switch tiv in binding_ti.variant {
            case runtime.Type_Info_Array:
            case runtime.Type_Info_Dynamic_Array:
            case runtime.Type_Info_Slice:
                // 
            
            case runtime.Type_Info_Bit_Set:
                // assert(field.type) == .ARRAY
            case runtime.Type_Info_Map:
                // assert(field.type) == .OBJECT
            
            case runtime.Type_Info_Struct:
                if .INIT in field.io_data.parse.flags {
                    mem.set(field.data_binding.data, 0, binding_ti.size)
                }
                // if a struct is inside an object, then assign the gon object name to the struct name member
                L_Get_Name_Member: {
                    if field.parent.type != .OBJECT {
                        break L_Get_Name_Member
                    }
                    
                    // TODO: this check should go elsewhere, probably up to the array cases above.
                    // if .PARSE_AS_OBJECT not_in field.parent.io_data.parse.flags {
                    //     break L_Get_Name_Member
                    // }
                    
                    // Currently, field.io_data only gets set right before calling into this procedure, 
                    // which means that this will necessarily be the same data as the io_data for the type specified in IO_Data_Lookup,
                    // UNLESS the user changed the io data in the data bind callback.
                    // This is probably something that we want to allow though, since if the user messes things up on their own, that's on them and I don't care so much.
                    
                    // type_io_data, found := IO_Data_Lookup[field.data_binding.id]
                    // if !found {
                    //     log("Unable to parse named struct array, element type is '%v', but this struct type does not specify any IO data.", field.data_binding.id)
                    //     return false
                    // }
                    
                    if field.io_data.name_member == "" {
                        // Maybe we should have some kind of error here if parent is internally an array or map type?
                        // Doesn't really matter for an array, though it would be weird to have named objects in an array only for those names to be discarded.
                        // Especially for map, since we presumably need someone to take ownership of the string used for the key?
                        // log("Unable to parse named struct, element type is '%v', but this struct type does not specify a name member in its IO data.", field.data_binding.id)
                        break L_Get_Name_Member
                    }
                    
                    member := reflect.struct_field_by_name(field.data_binding.id, field.io_data.name_member)
                    if member == {} {
                        log("Unable to parse named struct, the type '%v' specifies an invalid name member '%v' in its IO data.", field.data_binding.id, field.io_data.name_member)
                        return false
                    }
                    
                    member_any := any {
                        data = mem.ptr_offset(cast(^u8)field.data_binding.data, member.offset),
                        id   = member.type.id,
                    }
                    if !set_value_from_string(ctxt, member_any, field.name) {
                        return false
                    }
                }
                
            case:
                log("Unable to bind object or array to data of type: %v", field.data_binding.id)
                return false
        }
    }
    return true // ?
}


/* 
    This single procedure is essentially our data interface layer.
    Implementation is language-specific.
    
    TODO: this doesn't need to be passed the parse context, and it probably shouldn't be.
*/
set_value_from_string :: proc(using ctxt: ^SAX_Parse_Context, value: any, text: string) -> bool {
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
                log("Unsupported type in set_value_from_string(): %v", value.id)
                return true
            }
            if len(text) >= tiv.count { // leave one byte pad on the end so we can null terminate
                log("Unable to copy string of len %v to [%v]u8", len(text), tiv.count)
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
                log("Unsupported type in set_value_from_string(): %v", value.id)
                return false
            }
            (cast(^string)value.data)^ = strings.clone(text)
            return true
    
        case Type_Info_Dynamic_Array:
            array      := cast(^runtime.Raw_Dynamic_Array) value.data
            elem_count := array.len
            elem_ti    := runtime.type_info_base(tiv.elem)
            if elem_ti.size != 1 {
                log("Unsupported type in set_value_from_string(): %v", value.id)
                return true
            }
            arr_u8 := transmute(^[dynamic]u8) array
            clear(arr_u8)
            append_elem_string(arr_u8, text)
            return true

        case:
            log("Unsupported type in set_value_from_string(): %v", value.id)
            return true
    }
    
    return true
}

array_add_any :: proc(array: any) -> any {
    if array.data == nil {
		return {}
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
    a := cast(^runtime.Raw_Dynamic_Array) array.data
    
    a.len += 1
    
    if a.len >= a.cap {
        new_cap := max(8, a.cap * 2)
        if !reserve_any_dynamic_array(array, new_cap) {
            return {}
        }
    }
    
    ret := any {
        data = mem.ptr_offset(cast(^u8) a.data, (a.len - 1) * ti_array.elem.size),
        id   = ti_array.elem.id, 
    }
    
    return ret
}

array_add_any_at_index :: proc(array: any, index: int) -> any {
    new_cap := math.next_power_of_two(index)
    
    if array.data == nil {
		return false
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
    if !reserve_any_dynamic_array(array, new_cap) {
        return {}
    }
    
    a := cast(^runtime.Raw_Dynamic_Array) array.data
    
    ret := any {
        data = mem.ptr_offset(cast(^u8) a.data, index * ti_array.elem.size),
        id   = ti_array.elem.id, 
    }
    return ret
}

reserve_any_dynamic_array :: proc(array: any, capacity: int) -> bool {
    if array.data == nil {
		return false
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
	a := cast(^runtime.Raw_Dynamic_Array) array.data 

	if capacity <= a.cap {
		return true
	}

	if a.allocator.procedure == nil {
		a.allocator = context.allocator
	}
	assert(a.allocator.procedure != nil)

	old_size  := a.cap    * ti_array.elem.size
	new_size  := capacity * ti_array.elem.size
	allocator := a.allocator

	new_data, err := mem.resize(a.data, old_size, new_size, ti_array.elem.align, allocator)
	if err != nil {
		return false
	}
	if new_data == nil && new_size > 0 {
		return false
	}

	a.data = new_data
	a.cap  = capacity
	return true
}


// get_size_with_align :: proc(size, align: int) -> int {
//     if align == 0 do return size
    
//     whole     := size / align
//     remainder := size / align
    
//     if remainder != 0 do whole += 1
    
//     return whole * align
// }
