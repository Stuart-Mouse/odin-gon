
package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:unicode/utf8"
import "core:math"

import "core:encoding/json"


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
    _path_depth : int,          // rename _field_path_index or something. maybe also rename others
}

File_Format :: enum {
    GON,
    JSON,
    // XML,
}

Token :: struct {
    type: Token_Type,
    text: string,
}

// refactor so that it is easier to add user implementations
SAX_Tokenizer :: struct {
    type: File_Format,
    using variant: struct #raw_union {
        json : json.Tokenizer,
        gon  : GON_Tokenizer,
    },
}

Parser :: struct {    
    tokenizer     : SAX_Tokenizer,
    
    data_bindings : [dynamic] Data_Binding,
    event_handler : SAX_Event_Handler,
    log           : Log_Proc,

    _field_depth  : int,
}

Parser_Init_Flags :: bit_set [Parser_Init_Flag]
Parser_Init_Flag  :: enum {
    DO_NOT_INCLUDE_STANDARD_EVENT_HANDLER,
}

init_parse_context :: proc(ctxt: ^Parser, flags: Parser_Init_Flags) {
    if .DO_NOT_INCLUDE_STANDARD_EVENT_HANDLER not_in flags {
        append_elems(&ctxt.event_handler.field_read           , ..standard_event_handler.field_read[:])
        append_elems(&ctxt.event_handler.data_binding         , ..standard_event_handler.data_binding[:])
        append_elems(&ctxt.event_handler.indirect_data_binding, ..standard_event_handler.indirect_data_binding[:])
        append_elems(&ctxt.event_handler.object_begin         , ..standard_event_handler.object_begin[:])
        append_elems(&ctxt.event_handler.object_end           , ..standard_event_handler.object_end[:])
    }
}

set_file_to_parse :: proc(ctxt: ^Parser, file: string, file_format: File_Format = .GON) {
    ctxt.tokenizer.type = file_format
    switch file_format {
        case .GON:
            ctxt.tokenizer.gon.file = file
        case .JSON:
            ctxt.tokenizer.json = json.make_tokenizer(file)
    }
}

add_data_binding :: proc(ctxt: ^Parser, binding: any, path: string) {
    append(&ctxt.data_bindings, Data_Binding { 
        binding    = binding, 
        field_path = path,
    })
}

add_event_handler :: proc(event_handler: ^SAX_Event_Handler, event_type: SAX_Event_Type, handler_proc: SAX_Event_Handler_Proc) {
    dst: ^[dynamic]SAX_Event_Handler_Proc

    switch event_type {
        case .OBJECT_BEGIN:
            dst = &event_handler.object_begin
        case .OBJECT_END:
            dst = &event_handler.object_end
        case .FIELD_READ:
            dst = &event_handler.field_read
        case .DATA_BINDING:
            dst = &event_handler.data_binding
        case .INDIRECT_DATA_BINDING:
            dst = &event_handler.indirect_data_binding
        case: 
            fmt.println("Error: Tried to add an event handler for an invalid event type!")
            assert(false)
    }

    append(dst, handler_proc)
}

/*
    Could change this to return flags instead.
    This would allow for more fine grain control over what actions to take when returning from a callback.
    
    flags:
        ERROR
        SKIP_DIRECT_BINDING
        SKIP_INDIRECT_BINDING
        SKIP_FIELD
    
*/
SAX_Return_Code :: enum {
    ERROR = 0,
    OK    = 1,

    SKIP_BINDING,
}

SAX_Event_Handler_Proc :: proc(^Parser, ^SAX_Field) -> SAX_Return_Code

SAX_Event_Handler :: struct {
    object_begin,
    object_end,
    field_read,
    data_binding,
    indirect_data_binding : [dynamic] SAX_Event_Handler_Proc
}

SAX_Event_Type :: enum {
    OBJECT_BEGIN,
    OBJECT_END,
    FIELD_READ,
    DATA_BINDING,
    INDIRECT_DATA_BINDING,
}

standard_event_handler: SAX_Event_Handler

print_field_address :: proc(field: ^SAX_Field) {
    f := field
    for f != nil {
        fmt.printf("%v/", f.name)
        f = f.parent
    }
    fmt.println()
}

format_field_address :: proc(sb: ^strings.Builder, field: ^SAX_Field) {
    if field.parent != nil {
        format_field_address(sb, field.parent)
        strings.write_byte(sb, '/')
    }
    strings.write_string(sb, field.name)
}


SAX_parse_file :: proc(using ctxt: ^Parser) -> bool {
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
            // TODO: we actually need to process this data binding like any other
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

SAX_parse_object :: proc(using ctxt: ^Parser, parent: ^SAX_Field) -> (success: bool) {
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
            next_token_type, next_token = get_next_token(ctxt)
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
        next_token_type, next_token = get_next_token(ctxt)
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
                    log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                    return false
                }
                return true
            case:
                log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                return false
        }
        
        assert(field.type != .INVALID)
        
        event_result: SAX_Return_Code = .OK
        for e in event_handler.field_read {
            if e != nil {
                event_result = e(ctxt, &field)
                if event_result == .ERROR do return false
                if event_result != .OK    do break
            }
        }
        
        // If .SKIP_BINDING is returned from the field_read event, then the field will not receive any automatic data bindings whatsoever.
        // However, if the user sets the data binding manually in the callback, that data binding will still be processed.
        if event_result != .SKIP_BINDING {
            // set direct data bindings
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
            
            // will mutate field to add data binding
            if !check_for_indirect_data_binding(ctxt, &field, parent) {
                return false
            }
        }
        
        if field.data_binding != nil {
            if !process_data_binding(ctxt, &field) {
                return false
            }
        }

        // recurse for object / array
        if field.type == .OBJECT || field.type == .ARRAY {
            _field_depth += 1

            event_result: SAX_Return_Code = .OK;
            for e in event_handler.object_begin {
                if e != nil {
                    event_result = e(ctxt, &field)
                    if event_result == .ERROR do return false
                    if event_result != .OK    do break
                }
            }
            
            SAX_parse_object(ctxt, &field) or_return

            for e in event_handler.object_end {
                if e != nil {
                    event_result = e(ctxt, &field)
                    if event_result == .ERROR do return false
                    if event_result != .OK    do break
                }
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
process_data_binding :: proc(using ctxt: ^Parser, field: ^SAX_Field) -> bool {
    type_io_data, found := IO_Data_Lookup[field.data_binding.id]
    if found {
        // may implement more complex merge thing here?
        field.io_data = type_io_data
    }

    // handle data_binding event
    event_result: SAX_Return_Code = .OK;
    for e in event_handler.data_binding {
        if e != nil {
            event_result = e(ctxt, field);
            if event_result == .ERROR {
                return false
            }
            if event_result == .SKIP_BINDING {
                field.data_binding = {}
                return true
            }
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
    
    // TODO: figure out if this is where this needs to be. I have a lot of these dumb todos just saying to check the order of operations now...
    //       presumably, we want derefing any pointers to be the last thing we do before actaully setting a value through a data binding,
    //       that way the user can see that it is in fact a pointer in the callback and do something about that if they want to.
    field.data_binding, _ = deref_any_pointer(field.data_binding)
    
    // TODO: convert to a switch on field type, handle invalid cases
    binding_ti := runtime.type_info_base(type_info_of(field.data_binding.id))
    #partial switch field.type {
        case .FIELD:
            // restrict types to which we can bind a field
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Integer:
                case runtime.Type_Info_Float:
                case runtime.Type_Info_Enum:
                case runtime.Type_Info_String:
                case runtime.Type_Info_Boolean:
                
                case runtime.Type_Info_Bit_Set: 
                    // check if parent data binding is the same.
                    if field.parent.data_binding.data != field.data_binding.data {
                        sb := strings.builder_make(); defer strings.builder_destroy(&sb)
                        strings.write_string(&sb, "Error on field '")
                        format_field_address(&sb, field)
                        strings.write_string(&sb, "': Bit sets must be expressed as GON arrays, not as single-valued fields.")
                        log(strings.to_string(sb))
                        return false
                    }
                
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
            if !set_value_from_string(field.data_binding, field.value) {
                return false
            }
            
        case .ARRAY:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Array,
                     runtime.Type_Info_Dynamic_Array,
                     runtime.Type_Info_Slice:
                    // maybe add some check to see if these are supposed to be parsed as indexed or something
                    // In general, parsing is designed to be a bit more lax about accepting input, so long as it is valid GON
                
                case runtime.Type_Info_Bit_Set:
                    if field.parent.data_binding.data == field.data_binding.data {
                        sb := strings.builder_make(); defer strings.builder_destroy(&sb)
                        strings.write_string(&sb, "Error on field '")
                        format_field_address(&sb, field)
                        strings.write_string(&sb, "': Bit sets cannot contain nested GON arrays, only bit values.")
                        log(strings.to_string(sb))
                        return false
                    }
                
                case runtime.Type_Info_Struct:
                    if .INIT in field.io_data.parse.flags {
                        mem.set(field.data_binding.data, 0, binding_ti.size)
                    }
                
                case:
                    log("Unable to bind internal type '%v' to GON array.", field.data_binding.id)
                    return false
            }
            
        case .OBJECT:
            #partial switch tiv in binding_ti.variant {
                case runtime.Type_Info_Array:
                case runtime.Type_Info_Dynamic_Array:
                case runtime.Type_Info_Slice:
                    // maybe add some check to see if these are supposed to be parsed as indexed or something
            
                case runtime.Type_Info_Map:
                    // no op
            
                case runtime.Type_Info_Struct:
                    // This may not be necessary at all in Odin, since the only way this does anything is if the user passes in an object with some values already set. Because any memory allocated when expanding a dynamic array of strcuts will zero the memory (Unless we add the option of setting the allocator manually in the io data or something).
                    // The other realistic use case is that the user actually has some custom init proc for this type, in which case we should just check the IO data for that.
                    // But just zeroing the memory is probably doing nothing of value here since it is almost certainly already zeroed.
                    if .INIT in field.io_data.parse.flags {
                        mem.set(field.data_binding.data, 0, binding_ti.size)
                    }
                    
                    // Currently, field.io_data only gets set right before calling into this procedure, 
                    // which means that this will necessarily be the same data as the io_data for the type specified in IO_Data_Lookup,
                    // UNLESS the user changed the io data in the data bind callback.
                    // This is probably something that we want to allow though, since if the user messes things up on their own, that's on them and I don't care so much.
                    
                    if field.io_data.name_member != {} {
                        // Maybe we should have some kind of error here if parent is internally an array or map type?
                        // Doesn't really matter for an array, though it would be weird to have named objects in an array only for those names to be discarded.
                        // Especially for map, since we presumably need someone to take ownership of the string used for the key?
                        member_any := any {
                            data = mem.ptr_offset(cast(^u8)field.data_binding.data, field.io_data.name_member.offset),
                            id   = field.io_data.name_member.type.id,
                        }
                        if !set_value_from_string(member_any, field.name) {
                            return false
                        }
                    }
                
                case:
                    log("Unable to bind internal type '%v' to GON object.", field.data_binding.id)
                    return false
            }
            
        case .INVALID: fallthrough
        case:
            log("Invalid field passed to process_data_binding.")
            return false
    }
    return true // ?
}


// current solution is to just mutate the field and parent values passed, may change later if need be
check_for_indirect_data_binding :: proc(using ctxt: ^Parser, field, parent: ^SAX_Field) -> bool {
    if parent == nil || parent.data_binding == nil do return true
    
    event_result: SAX_Return_Code = .OK;
    for e in event_handler.indirect_data_binding {
        if e != nil {
            event_result = e(ctxt, field)
            if event_result == .ERROR do return false
            if event_result != .OK    do break
        }
    }
    
    if event_result == .SKIP_BINDING {
        return true
    }

    parent_ti := runtime.type_info_base(type_info_of(parent.data_binding.id))
    #partial switch &parent_tiv in parent_ti.variant {
        // perhaps it would be better to just support maps through a callback proc
        // then we can be more explicit about how we handle copyin key values
        // I don't like the extra complexity of Odin's type system as compared to Jai...
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
            if .ARRAY_INDEXED in parent.io_data.parse.flags {
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
            if .ARRAY_INDEXED in parent.io_data.parse.flags {
                assert(parent.type == .OBJECT) // TODO
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
            
            if .ARRAY_INDEXED in parent.io_data.parse.flags {
                assert(parent.type == .OBJECT) // TODO
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
    
    return true
}
