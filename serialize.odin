package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:unicode/utf8"
import "core:math"
import "core:os"

import "core:encoding/json"


// DATA SEGMENT

Serializer :: struct {
    builder       : strings.Builder,
    format        : File_Format,
    indent        : int,
    
    dom_root      : ^DOM_Node,
    do_free_nodes : bool,
    
    // event_handler : SAX_Event_Handler,
    log           : Log_Proc,
    
    allocator     : runtime.Allocator, // only used for allocating dom nodes, not used for string builder atm. may store a separate allocator for that
}


// INTERFACE PROCEDURES

init_serializer :: proc(using serializer: ^Serializer, root: ^DOM_Node = nil, _allocator := context.allocator) {
    allocator = _allocator
    builder   = strings.builder_make()
    
    if root != nil {
        dom_root = root
    } else {
        dom_root  = new(DOM_Node, allocator)
        dom_root^ = { 
            name = "root",
            type = .OBJECT,
        }
        do_free_nodes = true
    }
}

deinit_serializer :: proc(using serializer: ^Serializer) {
    strings.builder_destroy(&builder)
    if do_free_nodes {
        delete_child_nodes_recursive(dom_root)
        free(dom_root, allocator)
    }
}

// serializes to the serializer's internal string builder
// user will still need to write the constructed string out to a file
serialize :: proc(using serializer: ^Serializer) -> bool {
    if serializer == nil do return false
    if dom_root   == nil do return false
    
    serialize_proc: proc(using serializer: ^Serializer, node: ^DOM_Node) -> bool 
    switch serializer.format {
        case .GON  : serialize_proc = serialize_dom_nodes_to_gon
        case .JSON : serialize_proc = serialize_dom_nodes_to_json
    }
    
    // manually iterate over root node's children
    // we don't want to call _serialize_dom_nodes_to_gon() on the root node because we don't actually want to print that node explicitly
    child := dom_root.children.first
    for child != nil {
        if !serialize_proc(serializer, child) {
            return false
        }
        child = child.next
    }
    
    return true
}

serialize_to_string :: proc(using serializer: ^Serializer) -> (str: string, ok: bool) {
    serialize(serializer) or_return
    return strings.to_string(serializer.builder), true
}

serialize_to_file :: proc(using serializer: ^Serializer, file_path: string) -> bool {
    serialize(serializer) or_return
    os.write_entire_file(file_path, serializer.builder.buf[:])
    return true
}

serializer_insert_data_binding :: proc(serializer: ^Serializer, path: string, binding: any, prepend := false) {
    append_data_node(serializer.dom_root, path, binding, prepend)
}


// INTERNAL PROCEDURES

// We could convert this to an iterative process rather than a recursive one relatively easily,
//      because we don't currently have any need to use the stack to hold state.
// May be worth considering later once functionality is more complete.
serialize_dom_nodes_to_gon :: proc(using serializer: ^Serializer, node: ^DOM_Node) -> bool {
    if serializer == nil || node == nil do return false
    
    in_array       := false
    is_first_child := false
    same_line      := false

    if node.parent != nil {
        in_array       = node.parent.type == .ARRAY
        is_first_child = node == node.parent.first
        same_line      = (.SAME_LINE in node.parent.flags)      // check for sameline on parent, not self (we do that later)
    }
    
    if same_line || in_array {
        if !is_first_child {
            strings.write_string(&builder, ",")
        }
    }
    
    if same_line {
        strings.write_string(&builder, " ")
    } else {
        strings.write_string(&builder, "\n")
        for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING)
    }
    
    if !in_array {
        strings.write_string(&builder, 
            to_conformant_string(node.name, allocator = context.temp_allocator),
        )
        strings.write_string(&builder, " ");
    }
    
    #partial switch node.type {
        case .OBJECT, .ARRAY: 
            // ensure that end of object/array gets printed on same line if sameline flag is set on self
            same_line ||= (.SAME_LINE in node.flags)
        
            is_array := (node.type == .ARRAY)
            if is_array {
                elem_tid := node.first.data_binding.id
                if do_sameline_for_type(elem_tid) {
                    node.flags |= { .SAME_LINE }
                    same_line = true
                }
            }
            
            strings.write_string(&builder, is_array ? "[" : "{")
            
            // recurse for children
            indent += 1
            child := node.children.first
            for child != nil {
                if !serialize_dom_nodes_to_gon(serializer, child) {
                    return false
                }
                child = child.next
            }
            indent -= 1
            
            if same_line {
                strings.write_string(&builder, " ")
            } else {
                strings.write_string(&builder, "\n")
                for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING)
            }
            
            strings.write_string(&builder, is_array ? "]" : "}")
            
        case .FIELD:
            if node.value.text == "" {
                if node.data_binding.data == nil {
                    fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                    return false
                }
                node.value.text = fmt.tprintf("%v", node.data_binding)
            }
            strings.write_string(&builder, 
                to_conformant_string(node.value.text, allocator = context.temp_allocator),
            )
            
        case .REF:
            switch node.ref.type {
                case .INDEX   : fmt.sbprintf(&builder, "%v", get_node_index(node.ref.node))
                case .POINTER : fmt.sbprintf(&builder, "*\"%v\"", node.ref.text)
                case .VALUE   : assert(false, "Cannot print a value node.") // should not occur
            }
            
    }
    
    return true
}

serialize_dom_nodes_to_json :: proc(using serializer: ^Serializer, node: ^DOM_Node) -> bool {
    if serializer == nil || node == nil do return false
    
    in_array := node.parent != nil && node.parent.type == .ARRAY
    
    is_first_child := node.parent != nil && node == node.parent.first
    
    if !is_first_child {
        strings.write_string(&builder, ",")
    }
    strings.write_string(&builder, "\n")
    for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING);
    
    if !in_array {
        strings.write_string(&builder, 
            to_conformant_string(node.name, allocator = context.temp_allocator, force_quotes = true),
        )
        strings.write_string(&builder, ": ")
    }
    
    #partial switch node.type {
        case .OBJECT, .ARRAY: 
            is_array := node.type == .ARRAY
            
            strings.write_string(&builder, is_array ? "[" : "{")
            
            // recurse for children
            indent += 1
            child := node.children.first
            for child != nil {
                if !serialize_dom_nodes_to_json(serializer, child) {
                    return false
                }
                child = child.next
            }
            indent -= 1
            
            strings.write_string(&builder, "\n")
            for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING)
            
            strings.write_string(&builder, is_array ? "]" : "}")
            
        case .FIELD:
            if node.value.text == "" {
                if node.data_binding.data == nil {
                    fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                    return false
                }
                node.value.text = fmt.tprintf("%v", node.data_binding)
            }
            strings.write_string(&builder, 
                to_conformant_string(node.value.text, allocator = context.temp_allocator),
            )
    }
    
    return true
}


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
            node.flags |= { .SAME_LINE }
            return .ARRAY
        
        // arrays of bytes/u8 are serialized as string
        // we will probably distinguish this later on u8 vs byte, where byte is serialized using some binary data blob
        case runtime.Type_Info_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT     in io_data.serialize.flags ||
                   .ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Dynamic_Array:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT     in io_data.serialize.flags ||
                   .ARRAY_INDEXED in io_data.serialize.flags {
                    return .OBJECT
                }
            }
            return .ARRAY
            
        case runtime.Type_Info_Slice:
            if tiv.elem.size == 1 {
                return .FIELD
            }
            if io_data_found {
                if .AS_OBJECT     in io_data.serialize.flags ||
                   .ARRAY_INDEXED in io_data.serialize.flags {
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

do_sameline_for_type :: proc(type: typeid) -> bool {
    ti := reflect.type_info_base(type_info_of(type))
    
    _, type_is_int   := ti.variant.(runtime.Type_Info_Integer)
    _, type_is_float := ti.variant.(runtime.Type_Info_Float)
    _, type_is_enum  := ti.variant.(runtime.Type_Info_Enum)
    _, type_is_rune  := ti.variant.(runtime.Type_Info_Rune)
    
    return type_is_int || type_is_float || type_is_enum || type_is_rune
}