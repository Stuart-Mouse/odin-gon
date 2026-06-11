#+feature using-stmt
package gon

import "base:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:math"
import "core:log"
import "core:encoding/json"


Node_Flags :: bit_set[Node_Flag; u8]
Node_Flag  :: enum u8 {
    // parsing flags
    REFERENCES_RESOLVED,
    BINDING_RESOLVED,
    BINDING_ON_PATH,
    
    ARRAY_AS_OBJECT,
    ARRAY_INDEXED,
    ARRAY_ENUMERATED,
    
    // used to indicate that a field assumes the parent object's binding, used for special field value ref syntax
    BIND_PARENT,
    
    // formatting flags
    SAME_LINE,
}

Reference_Type :: enum u8 { VALUE, POINTER, INDEX }

Node_Type :: enum u8 { 
    INVALID = 0, 
    FIELD   = 1,
    OBJECT  = 2, 
    ARRAY   = 3,
    REF     = 4,
}

// this struct is kinda big
// maybe we optimize this later, but for now just making it work
Node :: struct {
    parent, next, prev: ^Node,
    
    name:           string,
    type:           Node_Type,
    flags:          Node_Flags,
    data_binding:   any,
    location:       Source_Location, 
    
    using content: struct #raw_union {
        ref: struct {
            text:   string, 
            node:   ^Node,
            type:   Reference_Type,
        },
        
        value: string,
        
        children: struct { 
            first:  ^Node,
            last:   ^Node,
            count:  int,
        },
    },
}

Node_Insertion_Behaviour :: enum {
    DEFAULT,        // just insert nodes with no extra checks
    OVERWRITE,      // overwrite existing nodes with the same name
    UNDERWRITE,     // don't insert node if one with the same name already exists
}


// DOM OPERATIONS

get_node_index :: proc(node: ^Node) -> int {
    index := 0
    for n := node.prev; n != nil; n = n.prev {
        index += 1
    }
    return index
}

debug_print_all_nodes :: proc(node: ^Node, indent: int = 0) {
    for i in 0..<indent do fmt.print(INDENTATION_STRING)
    fmt.println(node.name)

    child := node.children.first
    for child != nil {
        debug_print_all_nodes(child, indent + 1)
        child = child.next
    }
}

// does not delete the passed node or its neighbors, only children
delete_child_nodes_recursive :: proc(node: ^Node, allocator := context.allocator) {
    if node == nil do return
    if node.type == .OBJECT || node.type == .ARRAY {
        child := node.children.first
        for child != nil {
            next := child.next
            delete_child_nodes_recursive(child, allocator)
            free(child, allocator)
            child = next
        }
    }
}

find_node_by_path :: proc(node: ^Node, path: string) -> (^Node, int) {
    if path == "" do return node, get_node_index(node)
    node  := node
    path  := path
    index := 0
    next: string
    for node != nil {
        next, path, _ = path_next(path)
        if next == ".." {
            node  = node.parent
            index = get_node_index(node)
        } else {
            node, index = find_child_node_by_name(node, next)
        }
        if path == "" do break
    }
    return node, index
}

// will return nil if not found
find_child_node_by_name :: proc(parent: ^Node, name: string) -> (^Node, int) {
    node := parent.children.first
    index := 0
    for node != nil {
        if node.name == name do break
        node = node.next
        index += 1
    }
    return node, index
}

append_data_node :: proc(parent: ^Node, path: string, data_binding: any, prepend := false, allocator := context.allocator) ->  ^Node {    
    node := append_node_with_path(parent, path, prepend = prepend, allocator = allocator)
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
append_child_node :: proc(parent: ^Node, prepend := false, allocator := context.allocator) ->  ^Node {
    node, err := new(Node, allocator)
    if err != nil do return nil // don't want to pass down the allocator error atm, maybe do this later
    
    node.parent = parent
    parent.children.count += 1
    
    if prepend {
        if parent.children.first != nil {
            parent.children.first.prev = node
            node.next = parent.children.first
        }
        parent.children.first = node
        
        if parent.children.last == nil {
            parent.children.last = node
        }
    } else {
        if parent.children.last != nil {
            parent.children.last.next = node
            node.prev = parent.children.last
        }
        parent.children.last = node
        
        if parent.children.first == nil {
            parent.children.first = node
        }
    }
    
    return node
}

// may be a little bit odd, but if you want tell if a node was overwritten or not, check if type == .INVALID. if so, then the node was either created or overwritten
get_or_add_child_node :: proc(parent: ^Node, name: string, behavior: Node_Insertion_Behaviour = .DEFAULT, prepend := false, allocator := context.allocator) ->  ^Node {
    node: ^Node
    
    if behavior != .DEFAULT {
        node, _ = find_child_node_by_name(parent, name)
    }
    
    if node == nil {
        node = append_child_node(parent, prepend, allocator)
    } 
    else if behavior == .UNDERWRITE {
        return node
    }
    
    node^ = { name = name }
    return node
}

append_node_with_path :: proc(parent: ^Node, path: string, behavior: Node_Insertion_Behaviour = .DEFAULT, prepend := false, allocator := context.allocator) -> ^Node {
    node := parent
    path := path
    next: string
    for {
        next, path, _ := path_next(path)
        if path == "" {
            return get_or_add_child_node(node, next, behavior, prepend, allocator)
        }
        
        child, _ := find_child_node_by_name(node, next)
        if child != nil {
            if child.type != .OBJECT {
                return nil // error, we can't create a named subnode on an array or field type node
            }
            node = child
            continue
        }
        
        node      = append_child_node(node, prepend, allocator)
        node.name = next
        node.type = .OBJECT
    }
    
    assert(false, "unreachable")
    return nil
}

remove_node :: proc(node: ^Node, allocator := context.allocator) {
    node.parent.children.count -= 1
    if node.next != nil do node.next.prev = node.prev
    if node.prev != nil do node.prev.next = node.next
    if node.parent.children.first == node do node.parent.children.first = node.next
    if node.parent.children.last  == node do node.parent.children.last  = node.prev
    free(node, allocator)
}

clone_child_nodes_recursive :: proc(dst: ^Node, src: ^Node, allocator := context.allocator) -> bool {
    for child := src.children.first; child != nil; child = child.next {
        node := append_child_node(dst, allocator = allocator)
        if !clone_node_recursive(node, child) do return false
    }
    return true
}

clone_node_recursive :: proc(dst: ^Node, src: ^Node, allocator := context.allocator) -> bool {    
    if dst.type == .OBJECT {
        delete_child_nodes_recursive(dst)
    }

    dst.name         = src.name
    dst.type         = src.type
    dst.flags        = src.flags
    dst.data_binding = src.data_binding
    dst.content      = {}

    switch src.type {
      case .OBJECT, .ARRAY:
        return clone_child_nodes_recursive(dst, src, allocator)
      case .REF:
        dst.ref = src.ref
        if dst.ref.type == .VALUE {
            fmt.printfln("ERROR: cloned a value ref node @ %v", format_node_path(dst))
        }
      case .FIELD:
        dst.value = src.value
      case .INVALID:
        return false
    }
    
    return true
}

format_node_path :: proc(node: ^Node) -> string {
    recurse :: proc(builder: ^strings.Builder, node: ^Node) {
        if node.parent != nil {
            recurse(builder, node.parent)
            strings.write_byte(builder, '/')
        }
        fmt.sbprintf(builder, "%v", node.name)
    }
    
    builder := strings.builder_make(allocator = context.temp_allocator)
    defer strings.builder_destroy(&builder)
    recurse(&builder, node)
    return strings.to_string(builder)
}






// DATA BINDINGS

add_data_binding_to_dom :: proc(using parser: ^Parser, binding: any, path: string) -> (ok: bool) {
    node, _ := find_node_by_path(parser.dom_root, path)
    if node == nil {
        // log.logf(.Error, "Error: unable to create data binding for path '%v'. Path not found.", path)
        return false
    }
    if !add_data_binding_to_node(node, binding) {
        log.logf(.Error, "Error: unable to create data binding for path '%v'", path)
        return false
    }
    return true
}

add_data_bindings_to_dom :: proc(using parser: ^Parser, bindings: [] struct { binding: any, path: string }) -> bool {
    for b in bindings {
        // if !add_data_binding_to_dom(parser, b.binding, b.path) do return false
        add_data_binding_to_dom(parser, b.binding, b.path) 
    }
    return true
}

validate_node_references :: proc(using parser: ^Parser) -> bool {
    Result :: bit_set[ enum{ ERROR, COMPLETE, PROGRESS, REMOVE_NODE } ]

    recurse :: proc(using parser: ^Parser, node: ^Node) -> Result {
        if .REFERENCES_RESOLVED in node.flags do return { .COMPLETE }
        
        switch node.type {
          case .FIELD:
            node.flags |= { .REFERENCES_RESOLVED }
            return { .PROGRESS, .COMPLETE }
    
          case .OBJECT, .ARRAY:
                result: Result = { .COMPLETE }
                child := node.children.first; 
                for child != nil {
                    next_child   := child.next
                    child_result := recurse(parser, child)
                    if .ERROR       in child_result do return { .ERROR }
                    if .REMOVE_NODE in child_result do remove_node(child, node_allocator)
                    result |=  child_result & { .PROGRESS }
                    result &= (child_result & { .COMPLETE }) | ~{ .COMPLETE }
                    child = next_child
                }
                if .COMPLETE in result {
                    node.flags |= { .REFERENCES_RESOLVED }
                }
                return result
            
          case .REF:
            path := node.ref.text
            if path == "" {
                log.logf(.Error, "Empty reference on node '%v'.", format_node_path(node))
                return { .ERROR }
            }
            
            search_from_node := node.parent
            if path[0] == '/' {
                path = path[1:]
                search_from_node = parser.dom_root
            }
            ref_node, _ := find_node_by_path(search_from_node, path)
            
            if ref_node == nil {
                // TODO: find a way to print failed node path only when full pass makes no progress
                // maybe we collect warnings in some array and only print on error
                // would want to enumerate error types and store node, then format and print later
                log.logf(.Error, "WARNING: Cannot resolve ref from %v to %v", format_node_path(node), node.ref.text)
                return { }
            }
            node.ref.node = ref_node
            
            // pointer and index refs can be passed along and handled later
            if node.ref.type != .VALUE { 
                node.flags |= { .REFERENCES_RESOLVED }
                return { .PROGRESS, .COMPLETE }
            }
            
            // value ref to value ref cannot be resolved yet
            if node.ref.node.type == .REF && node.ref.node.ref.type == .VALUE {
                log.logf(.Error, "WARNING: Cannot resolve value ref to value ref: %v", format_node_path(node))
                return { }
            }
            
            result := Result { .PROGRESS, .COMPLETE }
            
            if .BIND_PARENT in node.flags {
                assert(ref_node.type == .OBJECT, "ref node with bind parent flag was not pointing to an object.")
            
                // we can't copy these nodes until they are all resolved, otherwise we get issues
                if .REFERENCES_RESOLVED not_in ref_node.flags {
                    return { }
                }
            
                parent := node.parent
                for child := ref_node.children.first; child != nil; child = child.next {
                    dst, _ := find_child_node_by_name(parent, child.name)
                    if dst != nil do continue
                    dst = append_child_node(parent, allocator = node_allocator)
                    if !clone_node_recursive(dst, child) do return { .ERROR }
                }
                result |= { .REMOVE_NODE }
            }
            else {
                name := node.name
                if !clone_node_recursive(node, ref_node) do return { .ERROR }
                node.name = name
            }
            
            node.flags |= { .REFERENCES_RESOLVED }
            return result
            
          case .INVALID:
            log.logf(.Error, "Invalid node type in validate_node_references().")
            return { .ERROR }
        }
        
        return { .ERROR }
    }
    
    iterations := 0
    for {
        iterations += 1
        result: Result = { .COMPLETE }
        child := parser.dom_root.children.first;
        for child != nil {
            next_child   := child.next
            child_result := recurse(parser, child)
            if .ERROR       in child_result {
                log.logf(.Error, "Error while trying to resolve node references.")
                return false
            }
            if .REMOVE_NODE in child_result {
                remove_node(child, node_allocator)
            }
            result |=  child_result & { .PROGRESS }
            result &= (child_result & { .COMPLETE }) | ~{ .COMPLETE }
            child = next_child
        }
        if .COMPLETE     in result do break
        if .PROGRESS not_in result {
            log.logf(.Error, "Unable to resolve node references.")
            return false
        }
    }
    
    // log.logf(.Error, "Resolved node references in %v iterations.", iterations)
    return true
}

process_data_bindings :: proc(using parser: ^Parser) -> bool {
    return process_node_binding(parser, dom_root)
}

process_node_binding :: proc(using parser: ^Parser, node: ^Node) -> bool {
    if .BINDING_RESOLVED in node.flags do return true
        
    callback_results: Callback_Results
    for callback in callbacks {
        if callback != nil {
            callback_results |= callback(node)
            if .ERROR in callback_results {
                return false
            }
        }
    }
    if .SKIP_BINDING in callback_results {
        return true
    }
    
    #partial switch node.type {
      case .OBJECT, .ARRAY:
        for child := node.children.first; child != nil; child = child.next {
            if !process_node_binding(parser, child) {
                return false
            }
        }
        return true
        
      case .FIELD: 
        if node.data_binding == nil do return true
        if binding_io_data, found := &IO_Data_Lookup[node.data_binding.id]; found {
            using binding_io_data.parse
            if parse_proc != nil {
                if !parse_proc(node.data_binding, node.value) {
                    fmt.println("Error, parse_proc() failed.")
                    return false
                }
                return true
            }
        }
        return set_value_from_string(node.data_binding, node.value)
        
      case .REF:
        assert(node.ref.node != nil, "ref node was nil")
        #partial switch node.ref.type {
          case .INDEX:
            return node.data_binding == nil || dynamic_int_cast(node.data_binding, get_node_index(node.ref.node))
            
          case .POINTER:
            if node.data_binding == nil do return true
            if node.ref.node.data_binding == nil {
                // TODO: have some option for this to be an error
                log.logf(.Error, "Warning: no data binding on pointer ref node '%v'.", format_node_path(node))
                return true
            }
            
            ref_ti_base     := reflect.type_info_base(type_info_of(node.ref.node.data_binding.id))
            binding_ti_base := reflect.type_info_base(type_info_of(node.data_binding.id).variant.(runtime.Type_Info_Pointer).elem)
            if ref_ti_base != binding_ti_base {
                fmt.printfln("pointer type mismatch %v vs %v", ref_ti_base.id, binding_ti_base.id)
                return false
            }
            (cast(^rawptr) node.data_binding.data)^ = node.ref.node.data_binding.data
            return true
            
          case:
            fmt.printfln("ERROR: got a ref of type %v in process_node_binding.", node.ref.type)
            return false
        }
            
    }
    
    return true
}

add_data_binding_to_node :: proc(node: ^Node, binding: any) -> bool  {
    if node == nil || binding.data == nil do return true
    
    if node.data_binding.data != nil {
        fmt.println("Error, node already has a data binding set...")
        return false
    }
    
    // check if we need to modify binding based on io data
    // if binding_io_data, found := *IO_Data_Lookup[node.data_binding.id]; found {
    //     using binding_io_data.parse
    //     if bind_proc != nil {
    //         binding = bind_proc(binding)
    //         if binding.data == nil {
    //             fmt.println("Error, bind_proc() returned nil.")
    //             return false
    //         }
    //     }
    // }
    
    node.data_binding = binding

    // binding, _ = deref_any_pointer(binding)
    binding_ti := runtime.type_info_base(type_info_of(binding.id))
    
    // make indirect bindings onto child nodes
    #partial switch node.type {
      case .OBJECT:
        for child := node.children.first; child != nil; child = child.next {
            if .BIND_PARENT in child.flags { // necessarily a value ref
                child.data_binding = node.data_binding
            }
        }
        
        #partial switch tiv in binding_ti.variant {
          case runtime.Type_Info_Struct:
            io_data, io_data_found := &IO_Data_Lookup[node.data_binding.id]
            
            name_member_name: string
            if io_data_found {
                name_member_name = io_data.name_member.name
            }
            is_name_set := false
            
            for child := node.children.first; child != nil; child = child.next {
                member := reflect.struct_field_by_name(node.data_binding.id, child.name) 
                if member == {} do continue
                if member.name == name_member_name do is_name_set = true
                
                member_any := any {
                    data = mem.ptr_offset(cast(^u8)node.data_binding.data, member.offset),
                    id   = member.type.id,
                }
                add_data_binding_to_node(child, member_any)
            }
            
            L_Assign_Name: if io_data_found && !is_name_set {
                // check parent attributes and maybe skip assigning name member
                if node.parent != nil {
                    if .ARRAY_INDEXED in node.parent.flags do break L_Assign_Name
                
                    if node.parent.data_binding.data != nil {
                        parent_ti := reflect.type_info_base(type_info_of(node.parent.data_binding.id))
                        _, parent_is_map := parent_ti.variant.(runtime.Type_Info_Map)
                        if parent_is_map do break L_Assign_Name
                    }
                }
                
                // TODO: maybe we want error handling when name member is missing, especially if parent type is array-object 
                if io_data.name_member != {} {
                    member_any := any {
                        data = mem.ptr_offset(cast(^u8)node.data_binding.data, io_data.name_member.offset),
                        id   = io_data.name_member.type.id,
                    }
                    if !set_value_from_string(member_any, node.name) {
                        return false
                    }
                }
            }
            
          case runtime.Type_Info_Map:
            // I suppose map key bindings are a special exception to the rule that we don't assign any values at this point in parsing
            // this should be fine because we can't use a field ref for the name or anything funky like that, so this will not possibly have any data dependencies
            key_member: reflect.Struct_Field
            value_ti := runtime.type_info_base(tiv.value)
            _, is_struct := value_ti.variant.(runtime.Type_Info_Struct) 
            if is_struct {
                type_io_data, found := IO_Data_Lookup[tiv.value.id]
                if found {
                    key_member = type_io_data.map_key_member
                    if runtime.type_info_base(key_member.type) != runtime.type_info_base(tiv.key) {
                        fmt.println("Error: key member type does not match map key type.")
                        return false
                    }
                }
            }
            
            // allocate temp space for key value
            // if key value type is a string, then set_value_from_string will also perform its own allocation in order to copy the source string
            // it is up to the user to free this string later. 
            // if key_member is set, the key value will be memcopied there. for a string, it will not clone the underlying data
            key_any := dynamic_new(tiv.key.id, context.temp_allocator)
            
            // allocate empty space that can be safely memcopied from
            // this has to be done because apparently there's no way to insert a hash dynamically without passing a value
            empty_value := cast(rawptr) raw_data(make([]u8, tiv.value.size, context.temp_allocator))
            
            raw_map := cast(^runtime.Raw_Map) node.data_binding.data
            for child := node.children.first; child != nil; child = child.next {
                runtime.__dynamic_map_check_grow(raw_map, tiv.map_info)
                if !set_value_from_string(key_any, child.name) {
                    return false
                }
                
                value := runtime.__dynamic_map_set_without_hash(
                    raw_map, tiv.map_info, key_any.data, empty_value,
                )
                add_data_binding_to_node(child, any { rawptr(value), tiv.value.id })
                
                if key_member != {} {
                    key_member_ptr := mem.ptr_offset(cast(^u8) child.data_binding.data, key_member.offset)
                    mem.copy(key_member_ptr, key_any.data, tiv.key.size)
                }
            }
            
          case runtime.Type_Info_Dynamic_Array:
            raw_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
          
            io_data, io_data_found := &IO_Data_Lookup[node.data_binding.id]
            if io_data_found && .ARRAY_INDEXED in io_data.parse.flags {
                node.flags |= { .ARRAY_INDEXED }
            } else {
                elem_ti := runtime.type_info_base(tiv.elem)
                _, is_struct := elem_ti.variant.(runtime.Type_Info_Struct) 
                if is_struct {
                    node.flags |= { .ARRAY_AS_OBJECT }
                }
                // TODO: maybe error when elem type here is not a struct
                if !reserve_any_dynamic_array(node.data_binding, node.children.count) { 
                    return false
                }
                raw_array.len = node.children.count
            }
            
            if .ARRAY_INDEXED in node.flags {
                for child := node.children.first; child != nil; child = child.next {
                    elem_index: int
                    if io_data.enum_index_type != {} {
                        elem_index = auto_cast reflect.enum_from_name_any(io_data.enum_index_type, child.name) or_return
                    } else {
                        elem_index = strconv.atoi(child.name)
                    }
                    elem_any := array_add_any_at_index(node.data_binding, elem_index)
                    add_data_binding_to_node(child, elem_any)
                }
            } else {
                index := 0
                for child := node.children.first; child != nil; child = child.next {
                    elem_any := any {
                        data = mem.ptr_offset(cast(^u8)raw_array.data, tiv.elem.size * index),
                        id   = tiv.elem.id,
                    }
                    add_data_binding_to_node(child, elem_any)
                    index += 1
                }
            }
            
            
          case runtime.Type_Info_Array, runtime.Type_Info_Slice, runtime.Type_Info_Enumerated_Array:
            io_data, found := &IO_Data_Lookup[node.data_binding.id]

            data:       rawptr
            elem_count: int
            min_value:  int
            max_value:  int
            elem_ti:    ^runtime.Type_Info
            
            // disambiguate array/slice
            #partial switch tiv in tiv {
              case runtime.Type_Info_Array:
                data       = node.data_binding.data
                elem_count = tiv.count
                elem_ti    = tiv.elem
                max_value  = elem_count-1
                
              case runtime.Type_Info_Slice:
                raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                data       = raw_slice.data
                elem_count = raw_slice.len
                elem_ti    = tiv.elem
                max_value  = elem_count-1
                  
              case runtime.Type_Info_Enumerated_Array:
                data       = node.data_binding.data
                elem_count = tiv.count
                elem_ti    = tiv.elem
                min_value  = auto_cast tiv.min_value
                max_value  = auto_cast tiv.max_value
            }
            
            if found && .ARRAY_INDEXED in io_data.parse.flags {
                node.flags |= { .ARRAY_INDEXED }
                for child := node.children.first; child != nil; child = child.next {
                    elem_index: int
                    if io_data.enum_index_type != {} {
                        elem_index = auto_cast reflect.enum_from_name_any(io_data.enum_index_type, child.name) or_return
                    } else {
                        elem_index = strconv.atoi(child.name)
                    }
                    if elem_index < min_value || elem_index > max_value {
                        fmt.println("Error: array index is out of bounds.")
                        return false
                    }
                    elem_any := any {
                        data = mem.ptr_offset(cast(^u8)data, elem_ti.size * elem_index),
                        id   = elem_ti.id,
                    }
                    add_data_binding_to_node(child, elem_any)
                }
            } else {
                if node.children.count > elem_count {
                    fmt.println("Error: too many elements in array.")
                    return false
                }
                
                elem_ti_base := runtime.type_info_base(elem_ti)
                _, is_struct := elem_ti_base.variant.(runtime.Type_Info_Struct) 
                
                if is_struct {
                    node.flags |= { .ARRAY_AS_OBJECT }
                    // TODO: also check the io data to see if name member is defined
                } else {
                    fmt.println("Data binding error: object-type array must contain a struct with a defined name member.")
                    return false
                }
                
                index := 0
                for child := node.children.first; child != nil; child = child.next {
                    elem_index := index - min_value;
                    elem_any := any {
                        data = mem.ptr_offset(cast(^u8)data, elem_ti.size * elem_index),
                        id   = elem_ti.id,
                    }
                    add_data_binding_to_node(child, elem_any)
                    index += 1
                }
            }
            
          case:
            fmt.println("Invalid data binding, mismatched gon/internal type: %v vs %v", node.type, binding.id)
            return false
        }
      
      case .ARRAY:
        #partial switch tiv in binding_ti.variant {
          case runtime.Type_Info_Bit_Set:
            if node.parent.data_binding.data == binding.data {
                return false
            }
            for child := node.children.first; child != nil; child = child.next {
                add_data_binding_to_node(child, node.data_binding)
            }
            
          case runtime.Type_Info_Struct:
            if node.children.count > cast(int) tiv.field_count {
                fmt.println("Data binding error: array-type struct contains too many elements.")
                return false
            }
            index := 0
            for child := node.children.first; child != nil; child = child.next {
                member_any := any {
                    data = mem.ptr_offset(cast(^u8)node.data_binding.data, tiv.offsets[index]),
                    id   = tiv.types[index].id,
                }
                add_data_binding_to_node(child, member_any)
                index += 1
            }
            
          case runtime.Type_Info_Dynamic_Array:
            if !reserve_any_dynamic_array(node.data_binding, node.children.count) { 
                fmt.println("Data binding error: failed to reserve space in dynamic array.")
                return false
            }
            
            raw_array := cast(^runtime.Raw_Dynamic_Array) node.data_binding.data
            raw_array.len = node.children.count
            
            index := 0
            for child := node.children.first; child != nil; child = child.next {
                elem_any := any {
                    data = mem.ptr_offset(cast(^u8)raw_array.data, tiv.elem.size * index),
                    id   = tiv.elem.id,
                }
                add_data_binding_to_node(child, elem_any)
                index += 1
            }
      
          case runtime.Type_Info_Array, runtime.Type_Info_Slice:
            io_data, found := &IO_Data_Lookup[node.data_binding.id]

            data       : rawptr
            elem_count : int
            elem_ti    : ^runtime.Type_Info
            
            // disambiguate array/slice
            #partial switch tiv in tiv {
                case runtime.Type_Info_Array:
                    data       = node.data_binding.data
                    elem_count = tiv.count
                    elem_ti    = tiv.elem
          
                case runtime.Type_Info_Slice:
                    if !alloc_any_slice(node.data_binding, node.children.count) {
                        fmt.println("Data binding error: failed to allocate data for slice.")
                        return false
                    }
                    
                    raw_slice := cast(^runtime.Raw_Slice) node.data_binding.data
                    data       = raw_slice.data
                    elem_count = raw_slice.len
                    elem_ti    = tiv.elem
            }
            
            if node.children.count > elem_count {
                fmt.println("Data binding error: bounds check failed on array or slice.")
                return false
            }
            
            index := 0
            for child := node.children.first; child != nil; child = child.next {
                elem_any := any {
                    data = mem.ptr_offset(cast(^u8)data,  elem_ti.size * index),
                    id   = elem_ti.id,
                }
                add_data_binding_to_node(child, elem_any)
                index += 1
            }
            
          case:
            fmt.printfln("Data binding error: mismatched gon/internal type: %v vs %v, at node %v", node.type, binding.id, format_node_path(node))
            return false
        }

      case .FIELD:
        #partial switch tiv in binding_ti.variant {
          case runtime.Type_Info_Integer,
               runtime.Type_Info_Float,
               runtime.Type_Info_Enum,
               runtime.Type_Info_String,
               runtime.Type_Info_Boolean:   // no op
              
          case runtime.Type_Info_Struct: 
            // TODO: for now, permitting all structs here
            // in future may want to precheck that there is some custom parse proc for this type
          
          case runtime.Type_Info_Bit_Set: 
            // For bit sets, both the enclosing array and the individual elements have the same binding
            // For fields, we must verify that the parent binding is the same as the field binding
            if node.parent.data_binding.data != binding.data {
                return false
            }
          
          // Arrays of bytes/u8 are permitted as single-valued fields so that we can parse them as strings
          case runtime.Type_Info_Array         : if tiv.elem.size != 1 do return false
          case runtime.Type_Info_Dynamic_Array : if tiv.elem.size != 1 do return false
          case runtime.Type_Info_Slice         : if tiv.elem.size != 1 do return false
          
          case:
            fmt.println("Invalid data binding, mismatched gon/internal type: %v vs %v", node.type, binding.id)
            return false
        }

      case .REF:
        switch node.ref.type {
          case .VALUE: 
            fmt.printfln("Trying to bind to a value ref node @ %v!", format_node_path(node))
            return false
            
          case .INDEX:
            #partial switch tiv in binding_ti.variant {
              case runtime.Type_Info_Integer,
                   runtime.Type_Info_Float,
                   runtime.Type_Info_Enum:    // no op
              case:
                return false
            }
            
          case .POINTER:
            // all we can do here is check that the data binding is actually a pointer type
            tip, ok := binding_ti.variant.(runtime.Type_Info_Pointer)
            if !ok do return false
        }
        
      case:
        // invalid node type error?
    }
    
    return true
}


@private
path_next :: proc(path: string) -> (next: string, remaining: string, ok: bool) {
    index := strings.index_any(path, "/\\"); 
    if index < 0 || index+1 == len(path) {
        return path, "", true;
    }
    return path[:index], path[index+1:], true;
}