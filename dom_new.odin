
package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"

/*
    After getting some of the basic stuff working for the DOM thing, I may be able to use it to generate parsing procedures for binary data as well.
    and perhaps this could as do things like manage versioning of structs, etc.
*/

INDENTATION_STRING := "    "

// this struct is kinda big
// maybe we optimize this later, but for now just making it work
DOM_Node :: struct {
    parent       : ^DOM_Node, 
    next         : ^DOM_Node, 
    prev         : ^DOM_Node, 

    name         : string,
    type         : Field_Type,
    data_binding : any,
    flags        : enum { SAME_LINE },
    
    // value is text for .FIELD, value is children for .OBJECT and .ARRAY
    using value: struct #raw_union {
        text : string,
        using children: struct { 
            first : ^DOM_Node,
            last  : ^DOM_Node,
            count : int,
        },
    },
}

// walk_nodes_depth_first :: proc(node: ^DOM_Node, walk_proc: proc(^DOM_Node, rawptr) -> bool, data: rawptr) {
//     if walk_proc == nil do return false
    
//     walk_proc(node, data) or_return
    
//     child := node.first
//     for child != nil {
//         walk_nodes_depth_first(child, walk_proc, data)
//         child = child.next
//     }
// }

debug_print_all_nodes :: proc(node: ^DOM_Node, indent: int = 0) {
    for i in 0..<indent do fmt.print(INDENTATION_STRING)
    fmt.println(node.name)

    child := node.first
    for child != nil {
        debug_print_all_nodes(child, indent + 1)
        child = child.next
    }
}

// does not delete the passed node or its neighbors, only children
delete_child_nodes_recursive :: proc(node: ^DOM_Node) {
    if node.type == .OBJECT || node.type == .ARRAY {
        child := node.first
        for child != nil {
            next := child.next
            delete_child_nodes_recursive(child)
            free(child)
            child = next
        }
    }
}

find_node_by_path :: proc(node: ^DOM_Node, path: string) -> ^DOM_Node {
    path := path
    node := node
    for path != "" {
        next, remaining, ok := get_next_ident_from_path_string(path)
        if !ok do return nil
        path = remaining
        node = find_child_node_by_name(node, next)
    }
    return node
}

// will return nil if not found
find_child_node_by_name :: proc(parent: ^DOM_Node, name: string) -> ^DOM_Node {
    node := parent.first
    for node != nil {
        if node.name == name do break
        node = node.next
    }
    return node
}

append_nodes_for_indirect_bindings :: proc(node: ^DOM_Node, allocator := context.allocator) {
    if node == nil || node.data_binding.data == nil do return
    using runtime

    ti := type_info_base(type_info_of(node.data_binding.id))
    #partial switch tiv in ti.variant {
        case Type_Info_Struct: 
            member_count := len(tiv.names)
            for i in 0..<member_count {
                member_type   := tiv.types  [i]
                member_name   := tiv.names  [i]
                member_offset := tiv.offsets[i]
                
                member_any := any {
                    data = mem.ptr_offset(cast(^byte)node.data_binding.data, member_offset),
                    id   = member_type.id,
                }
                
                // figure out whether to prepend elems (will do for things that need to be attrs)
                append_data_node(node, member_name, member_any, allocator = allocator)
            }
            
            return
            
        case Type_Info_Array, Type_Info_Slice, Type_Info_Dynamic_Array: 
            data       : rawptr
            elem_count : int
            elem_ti    : ^Type_Info
            
            // disambiguate array/slice/dynamic
            #partial switch tiv in tiv {
                case Type_Info_Array:
                    data       = node.data_binding.data
                    elem_count = tiv.count
                    elem_ti    = tiv.elem
        
                case Type_Info_Slice:
                    raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                    data       = raw_slice.data
                    elem_count = raw_slice.len
                    elem_ti    = tiv.elem
      
                case Type_Info_Dynamic_Array:
                    raw_dynamic_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
                    data       = raw_dynamic_array.data
                    elem_count = raw_dynamic_array.len
                    elem_ti    = tiv.elem
                    if elem_count == 0 do return // skip serializing empty dynamic arrays
            }
            
            for i in 0..<elem_count {
                elem_any := any {
                    id   = elem_ti.id,
                    data = mem.ptr_offset(cast(^byte)data, elem_ti.size * i),
                }
                
                // TODO: as indexed, as object
                
                elem_name: string = fmt.tprint(i)
                append_data_node(node, elem_name, elem_any, allocator = allocator)
            }
            
            return
            
        case Type_Info_Map:
            raw_map := transmute(^Raw_Map) node.data_binding.data
            #partial switch ti_key in runtime.type_info_base(tiv.key).variant {
                case Type_Info_String:
                    m := (^mem.Raw_Map)(node.data_binding.data)
                    
                    if m != nil {
                        if tiv.map_info == nil {
                            return
                        }
                        map_cap := uintptr(runtime.map_cap(m^))
                        ks, vs, hs, _, _ := runtime.map_kvh_data_dynamic(m^, tiv.map_info)
                        j := 0
                        for bucket_index in 0..<map_cap {
                            runtime.map_hash_is_valid(hs[bucket_index]) or_continue         
                            key   := runtime.map_cell_index_dynamic(ks, tiv.map_info.ks, bucket_index)
                            value := runtime.map_cell_index_dynamic(vs, tiv.map_info.vs, bucket_index)
                  
                            append_data_node(node, (cast(^string)key)^, any { rawptr(value), tiv.value.id })
                        }
                    }
                    
                case: 
                    fmt.printf("Unable to serialize type: %v\nCurrently, only maps with string keys are supported.", ti)
            }
                    
            return
    }
}

append_data_node :: proc(parent: ^DOM_Node, path: string, data_binding: any, prepend := false, allocator := context.allocator) ->  ^DOM_Node {    
    node := append_node_with_path(parent, path, prepend, allocator)
    if node == nil do return node
    
    node.data_binding = data_binding
    node.type = determine_node_type_for_serialization(node) // TODO: combine into one proc with the below indirect data bindings one below. maybe we just inline those here for now
    
    // make indirect bindings
    if node.type == .OBJECT || node.type == .ARRAY {
        append_nodes_for_indirect_bindings(node, allocator)
    }
    
    return node
}

// does the bare minimum to append a node, not even giving it a name
// after the node is appended, caller should initialize it
append_child_node :: proc(parent: ^DOM_Node, prepend := false, allocator := context.allocator) ->  ^DOM_Node {
    node, err := new(DOM_Node, allocator)
    if err != nil do return nil // don't want to pass down the allocator error atm, maybe do this later
    
    node.parent = parent
    parent.count += 1
    
    if prepend {
        if parent.first != nil {
            parent.first.prev = node
            node.next = parent.first
        }
        parent.first = node
        
        if parent.last == nil {
            parent.last = node
        }
    } else {
        if parent.last != nil {
            parent.last.next = node
            node.prev = parent.last
        }
        parent.last = node
        
        if parent.first == nil {
            parent.first = node
        }
    }
    
    return node
}

append_node_with_path :: proc(parent: ^DOM_Node, path: string = "", prepend := false, allocator := context.allocator) -> ^DOM_Node {
    path := path
    node := parent
    
    // empty path is not valid, reject it
    if path == "" do return nil
    
    for {
        next, remaining, ok := get_next_ident_from_path_string(path)
        if !ok do return nil
        path = remaining
        
        if path != "" { 
            // non-terminal node
            // find if exists
            child := find_child_node_by_name(node, next)
            if child != nil {
                if child.type != .OBJECT {
                    return nil // error, we can't create a named subnode on an array or field type node
                }
                node = child
                continue
            }
            // create if does not exist
            node = append_child_node(node, prepend, allocator)
            node.name = next
            node.type = .OBJECT
        } 
        else { 
            // terminal node
            node = append_child_node(node, prepend, allocator)
            node.name = next
            break
        }
    }
    
    return node
}

// if next returns empty, then there was an error and remaining is also not a valid value
get_next_ident_from_path_string :: proc(path: string) -> (next, remaining: string, ok: bool) {
    if path == "" do return
    remaining = path

    defer if !ok {
        next      = ""
        remaining = path
    }
    
    for is_whitespace(remaining[0]) {
        if !advance(&remaining) do return
    }
    
    if remaining[0] == '\"' || remaining[0] == '\'' {
        quote_char := remaining[0]
        
        if !advance(&remaining) do return
        next = remaining[0:]
        string_len := 0
        
        for remaining[0] != quote_char {
            adv := 1 + int(remaining[0] == '\\') // TODO: handle escape sequences more properly
            string_len += adv
            if !advance(&remaining, adv) do return
        }
        next = next[:string_len]
        if !advance(&remaining) do return
    } else {
        next = remaining[0:]
        string_len := 0
        for is_character_permitted_in_unquoted_string(remaining[0]) {
            string_len += 1
            if !advance(&remaining) do break // it is ok if we can't advance here, may have just hit end of path
        }
        next = next[:string_len]
    }
    
    if next == "" do return
    
    if remaining != "" {
        for is_whitespace(remaining[0]) {
            if !advance(&remaining) {
                ok = true
                return
            }
        }
        if remaining[0] == '/' {
            if !advance(&remaining) do return
        }
    }
    
    ok = true
    return
}

is_character_permitted_in_unquoted_string :: proc(char: u8) -> bool {
    return (char >= 'a' && char <= 'z') || 
           (char >= 'A' && char <= 'Z') || 
           (char >= '0' && char <= '9') || 
            char == '_' || 
            char == '-' || 
            char == '.'
}

// returns the unescaped character and the length of the escape sequence in characters
// parse_escape_sequence :: proc(str: string) -> (u8, int) {
    
// }

// determination is currently made based only on node's data binding and parent's data binding, but we will probably consider some other flags on the node later
// the type set here is authoritative, so when we go to actually serialize a node later, it must be serializable as this type
determine_node_type_for_serialization :: proc(node: ^DOM_Node) -> Field_Type {
    if node.data_binding.data == nil do return .OBJECT

    io_data, io_data_found := IO_Data_Lookup[node.data_binding.id]
    ti := runtime.type_info_base(type_info_of(node.data_binding.id))
    
    #partial switch tiv in ti.variant {
        case runtime.Type_Info_Integer,
             runtime.Type_Info_Float,
             runtime.Type_Info_Enum,
             runtime.Type_Info_String,
             runtime.Type_Info_Boolean:
            return .FIELD
        
        case runtime.Type_Info_Bit_Set: 
            // check if parent data binding is the same.
            if node.parent.data_binding.data == node.data_binding.data {
                return .FIELD
            }
            return .ARRAY
        
        // arrays of bytes/u8 are serialized as string
        // we will probably distinguish this later on u8 vs byte, where byte is serialized using some binary data blob
        case runtime.Type_Info_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Dynamic_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Slice:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT               in io_data.serialize.flags ||
                   .SERIALIZE_ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Struct:
            if io_data_found {
                if .AS_ARRAY in io_data.serialize.flags {
                    return .ARRAY
                }
            }
            return .OBJECT
        
        case runtime.Type_Info_Map:
            return .OBJECT
            
        case:
            return .INVALID
    }
    
    return .INVALID
}

/*
    It seems like we really may not *need* to create nodes for all indirect bindings when serializing.
    The only reason we need to create the nodes is so that we can reorder elements, and we may attach some formatting flags to the nodes but those flags are presumably available also by checking the io data for the data binding. 
    
    not sure if I like the separationg of determining the field type and creating indirect data bindings
    the reason they are separate now is because of how I am the nodes with the type before creating indirect bindings
    
    I think though, that we can append the node, then resolve the type and create indirect bindings in a single procedure. 
        And that seems like it may be a better idea since both of those operations require similar information.
        Plus, we could more succinctly rebind data as necessary, like in treating []u8 types as strings.
    
    
    SIDE NOTE:
    
    we will have to insert a special condition when serializing a node to handle the custom formatting that's required for a bit set
    likewise for parsing a bit set from a dom also.
    
    
    Need two methods of inserting nodes into a DOM
    1. insert a node from a field, with or without a data binding set (for parsing)
    2. insert a node from a data_binding + name string (for serialization)
    
*/

// used to build a DOM from a text file
DOM_Parser :: struct {
    file           : string,
    dom_root       : ^DOM_Node,
    data_bindings  : [dynamic] Data_Binding,
    log            : Log_Proc,
    allocator      : runtime.Allocator,
}

init_dom_parser :: proc(using parser: ^DOM_Parser, _file: string, _allocator := context.allocator) {
    file      = _file
    allocator = _allocator
}

add_data_binding_to_dom_parser :: proc(using parser: ^DOM_Parser, binding: any, path: string) {
    append(&data_bindings, Data_Binding { 
        binding    = binding, 
        field_path = path,
    })
    // should we precheck that field path is valid? will still have to verify that there are no conflicts later on
    // we will detect conflicts when actually creating the bindings to the DOM, since we can't just textually compare field paths trivially, and I don't want to do it that way anyhow
}

/*
    Steps in parsing:
    
    + read tokens and append all nodes
    insert data bindings into dom nodes
        check data type compatibility
        run callbacks for walking dom similar to what we have in sax mode
            problem: we don't actually walk the dom when inserting nodes
            maybe we can walk it when we got to actually set all values for data bindings?
            this should be totally fine so long as long as we dont have very sparse bindings in a file
                we don't actually store the ^node back in the data binding, plus also indirect bindings, so really our only choice is to walk the dom for the final evaluation
        if value uses field reference, save this and resolve later
    resolve field references / data dependencies
        it's possible there's a circular dependency in which case we should error
        better to do this before setting any values, the idea is that every thing is correct before we start allocating
    set data from text values of fields
*/

construct_dom_from_gon_file :: proc(file: string) -> (root: ^DOM_Node) {
    file := file

    next_token_type : Token_Type
    next_token      : string
    
    root = new(DOM_Node)
    root.name = "root"
    root.type = .OBJECT
    
    success = false
    defer if !success {
        delete_child_nodes_recursive(root)
        free(root)
    }
    
    parent := root
    L_Loop: for parent != nil {
        name, text: string
        type: Field_Type
        
        // read field name
        if parent.type != .ARRAY {
            next_token_type, next_token = get_next_token_gon(&file)
            #partial switch next_token_type {
                case .STRING: 
                    name = next_token
                case .EOF:
                    if parent != root {
                        return nil
                    }
                    break L_Loop
                case .OBJECT_END:
                    if parent.type != .OBJECT {
                        // log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                        return nil
                    }
                    parent = parent.parent
                    continue
                case:
                    // log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                    return nil
            }
        }

        // read field value
        next_token_type, next_token = get_next_token_gon(&file)
        #partial switch next_token_type {
            case .STRING: 
                type = .FIELD
                text = next_token
            case .OBJECT_BEGIN: 
                type = .OBJECT
            case .ARRAY_BEGIN: 
                type = .ARRAY
            case .ARRAY_END:
                if type != .ARRAY {
                    // log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                    return nil
                }
                parent = parent.parent
                continue
            case:
                // log("GON parse error: Unexpected %v token \"%v\".", next_token_type, next_token)
                return nil
        }
        
        assert(type != .INVALID)
        
        node := append_child_node(parent)
        node.name = name
        node.type = type
        if node.type == .OBJECT || node.type == .ARRAY {
            parent = node
        } else {
            node.text = text
        }
    }
    
    success = true
    return root
}

add_data_bindings_to_nodes :: proc(using parser: ^DOM_Parser) -> bool {
    for b in data_bindings {
        node := find_node_by_path(b.field_path)
        if node == nil do return false
        
        // check that node type is compatible with binding type
        // for fields, do everything required to process a binding in sax, except dont actually set the value
        // for array/oibject, do indirect bindings
        // eventually, mark fields that use field references to be resolved later
        
        switch node.type {
            case .FIELD:
                
                
            case .OBJECT:
                
                
            case .ARRAY:
                
                
            case:
                // invalid node type error?
        }
    }
}
