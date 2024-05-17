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

destroy_serializer :: proc(using serializer: ^Serializer) {
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
    
    in_array := node.parent != nil && node.parent.type == .ARRAY
    
    is_first_child := node.parent != nil && node == node.parent.first
    
    if in_array {
        if !is_first_child {
            strings.write_string(&builder, ",")
        }
        strings.write_string(&builder, " ")
    } else {
        strings.write_string(&builder, "\n")
        for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING);
        strings.write_string(&builder, 
            to_conformant_string(node.name, allocator = context.temp_allocator),
        )
        strings.write_string(&builder, " ");
    }
    
    #partial switch node.type {
        case .OBJECT, .ARRAY: 
            is_array := node.type == .ARRAY
            
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
            
            if is_array {
                strings.write_string(&builder, " ")
            } else {
                strings.write_string(&builder, "\n")
                for i in 0..<indent do strings.write_string(&builder, INDENTATION_STRING)
            }
            
            strings.write_string(&builder, is_array ? "]" : "}")
            
        case .FIELD:
            if node.text == "" {
                if node.data_binding.data == nil {
                    fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                    return false
                }
                node.text = fmt.tprintf("%v", node.data_binding)
            }
            strings.write_string(&builder, 
                to_conformant_string(node.text, allocator = context.temp_allocator),
            )
            
        case .REF:
            fmt.sbprintf(&builder, "%v", get_node_index(node.ref.node))
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
            if node.text == "" {
                if node.data_binding.data == nil {
                    fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                    return false
                }
                node.text = fmt.tprintf("%v", node.data_binding)
            }
            strings.write_string(&builder, 
                to_conformant_string(node.text, allocator = context.temp_allocator),
            )
    }
    
    return true
}
