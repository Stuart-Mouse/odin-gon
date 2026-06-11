#+feature using-stmt

package gon

import "base:runtime"
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

File_Format :: enum { GON, JSON }

Serializer :: struct {
    builder:        strings.Builder,
    format:         File_Format,
    indent:         int,
    
    dom_root:       ^Node,
    do_free_nodes:  bool,
    
    allocator:      runtime.Allocator, // only used for allocating dom nodes, not used for string builder atm. may store a separate allocator for that
}

INDENTATION_STRING :: "    "


// INTERFACE PROCEDURES

init_serializer :: proc(using serializer: ^Serializer, root: ^Node = nil, _allocator := context.allocator) {
    allocator = _allocator
    builder   = strings.builder_make()
    
    if root != nil {
        dom_root = root
    } else {
        dom_root  = new(Node, allocator)
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
    
    serialize_proc: proc(using serializer: ^Serializer, node: ^Node) -> bool 
    switch serializer.format {
      case .GON:  serialize_proc = serialize_dom_nodes_to_gon
      case .JSON: serialize_proc = serialize_dom_nodes_to_json
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
    _ = os.write_entire_file(file_path, serializer.builder.buf[:])
    return true
}

serializer_insert_data_binding :: proc(serializer: ^Serializer, path: string, binding: any, prepend := false) {
    append_data_node(serializer.dom_root, path, binding, prepend)
}


// INTERNAL PROCEDURES

// We could convert this to an iterative process rather than a recursive one relatively easily,
//      because we don't currently have any need to use the stack to hold state.
// May be worth considering later once functionality is more complete.
serialize_dom_nodes_to_gon :: proc(using serializer: ^Serializer, node: ^Node) -> bool {
    if serializer == nil || node == nil do return false
    
    in_array       := false
    is_first_child := false
    same_line      := false

    if node.parent != nil {
        in_array       = node.parent.type == .ARRAY
        is_first_child = node == node.parent.children.first
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
            elem_tid := node.children.first.data_binding.id
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
        if node.value == "" {
            if node.data_binding.data == nil {
                fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                return false
            }
            node.value = fmt.tprintf("%v", node.data_binding)
        }
        strings.write_string(&builder, 
            to_conformant_string(node.value, allocator = context.temp_allocator),
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

serialize_dom_nodes_to_json :: proc(using serializer: ^Serializer, node: ^Node) -> bool {
    if serializer == nil || node == nil do return false
    
    in_array := node.parent != nil && node.parent.type == .ARRAY
    
    is_first_child := node.parent != nil && node == node.parent.children.first
    
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
        if node.value == "" {
            if node.data_binding.data == nil {
                fmt.printf("ERROR: no value defined for node '%v'\n", node.name) // TODO: proc to get full path to node
                return false
            }
            node.value = fmt.tprintf("%v", node.data_binding)
        }
        strings.write_string(&builder, 
            to_conformant_string(node.value, allocator = context.temp_allocator),
        )
    }
    
    return true
}


// determination is currently made based only on node's data binding and parent's data binding, but we will probably consider some other flags on the node later
// the type set here is authoritative, so when we go to actually serialize a node later, it must be serializable as this type
determine_node_type_for_serialization :: proc(node: ^Node) -> Node_Type {
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
        
      case runtime.Type_Info_Array:
        // arrays of bytes/u8 are serialized as string
        // we will probably distinguish this later on u8 vs byte, where byte is serialized using some binary data blob
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

append_nodes_for_indirect_bindings :: proc(node: ^Node, allocator := context.allocator) {
    if node == nil || node.data_binding.data == nil do return
    using runtime

    ti := type_info_base(type_info_of(node.data_binding.id))
    #partial switch tiv in ti.variant {
      case Type_Info_Struct: 
        for i in 0..<tiv.field_count {
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



// IMMEDIATE-MODE SERIALIZATION

open_object :: #force_inline proc(sb: ^strings.Builder, name: string) {
    strings.write_string(sb, to_conformant_string(name, allocator = context.temp_allocator),)
    strings.write_string(sb, " { ")
    strings.write_byte(sb, '\n')
}

close_object :: #force_inline proc(sb: ^strings.Builder) {
    strings.write_string(sb, "} ")
    strings.write_byte(sb, '\n')
}

open_array :: #force_inline proc(sb: ^strings.Builder, name: string) {
    strings.write_string(sb, to_conformant_string(name, allocator = context.temp_allocator),)
    strings.write_string(sb, " [ ")
    strings.write_byte(sb, '\n')
}

close_array :: #force_inline proc(sb: ^strings.Builder) {
    strings.write_string(sb, "] ")
    strings.write_byte(sb, '\n')
}

write_field :: #force_inline proc(sb: ^strings.Builder, name: string, value: string, raw_value := false) {
    if value == "" do return
    
    strings.write_string(sb, to_conformant_string(name, allocator = context.temp_allocator))
    strings.write_byte(sb, ' ')
    
    value := value
    if !raw_value do value = to_conformant_string(value, allocator = context.temp_allocator)
    strings.write_string(sb, value)
    strings.write_byte(sb, ' ')
    strings.write_byte(sb, '\n')
}





is_reserved_char :: proc(char: u8) -> bool {
    return char == '#' || char == '{' || char == '}' || char == '[' || char == ']'
}

// only " and \ need to be escaped
is_escaped_char :: proc(char: u8) -> bool {
    return char == '\\' || char == '\"'
}

to_conformant_string :: proc(s: string, force_quotes := false, allocator := context.allocator) -> string {
    if s == "" do return strings.clone("\"\"", allocator)

    sb := strings.builder_make(allocator)
    defer strings.builder_destroy(&sb)
    
    write_quotes := force_quotes || (len(s) == 0)
    if !write_quotes {
        for c in transmute([]u8)s {
            if !is_char_permitted_in_unquoted_string(c) {
                write_quotes = true
            }
        }
    }
    
    if write_quotes do strings.write_byte(&sb, '\"')
  
    for c in (transmute([]u8)s) {
        if c == 0 do break
        if is_escaped_char(c) {
            strings.write_byte(&sb, '\\')
        }
        strings.write_byte(&sb, c)
    }
  
    if write_quotes do strings.write_byte(&sb, '\"')
  
    return strings.to_string(sb)
}

type_has_custom_serialization_proc :: proc(type: typeid) -> bool {
    type_io_data, found := IO_Data_Lookup[type]
    if !found do return false
    return type_io_data.serialize.to_string_proc != nil
}

/*
    Trying to just get a quick and dirty solution done, so there are some things done very inefficiently.
    For example, the print_to_builder proc was just inteded to allow me to more easily port my Jai code even though the temp allocations are kinda dumb.
*/
serialize_any :: proc(
    sb:       ^strings.Builder, 
    name:     string, 
    value:    any, 
    indent:   int    = 0, 
    delim:    string = "",
    flags:    Serialization_Flags = {},
) {
    using runtime
    
    if value.data == nil do return

    ti := type_info_base(type_info_of(value.id))
    
    flags := flags
    type_io_data, _ := IO_Data_Lookup[value.id] // don't currently need to check if actually found. This is likely to change
    
    flags |= type_io_data.serialize.flags
    
    if .SKIP_IF_EMPTY in flags && all_bytes_are_zero(value) do return // skip serializing zero'd data
    
    if type_io_data.serialize.to_string_proc != nil {
        for i in 0..<indent do strings.write_string(sb, " ")
    
        // TODO: name this variables better
        // Custom serialization proc can return both name and value strings
        x_name, x_value, ok := type_io_data.serialize.to_string_proc(value)
        if !ok do return

        if name != "" {
            if x_name == "" do x_name = name // use default name if none provided by the custom serialization proc
            strings.write_string(sb, 
                to_conformant_string(x_name, allocator = context.temp_allocator),
            )
            strings.write_string(sb, " ");
        }
        
        fmt.sbprintf(sb, "%v", to_conformant_string(x_value, force_quotes = true))
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim)
        
        return
    }

    #partial switch tiv in ti.variant {
      case Type_Info_Struct: 
        for i in 0..<indent do strings.write_string(sb, " ");
        if name != "" {
            strings.write_string(sb, 
                to_conformant_string(name, allocator = context.temp_allocator),
            )
            strings.write_string(sb, " ");
        }
        
        as_array    := .AS_ARRAY    in flags
        on_one_line := .ON_ONE_LINE in flags
        
        strings.write_byte(sb, as_array    ? '[' : '{' )
        strings.write_byte(sb, on_one_line ? ' ' : '\n')
        
        member_indent := on_one_line ? 0 : indent + 2
        
        for i in 0..<tiv.field_count {
            type   := tiv.types  [i]
            name   := tiv.names  [i]
            offset := tiv.offsets[i]
            
            member_any := any {
                data = mem.ptr_offset(cast(^byte)value.data, offset),
                id   = type.id,
            }
            
            // We have to figure out the delim on every iteration so that we don't write
            //   a comma after the last element when fields are all on one line.
            // member_delim := type_io_data.serialize.member_delim
            // if member_delim == "" {
                // I apologize for the nested ternary
                member_delim := on_one_line ? ((i == tiv.field_count-1) ? " " : ", ") : "\n"
            // }
            
            member_flags: Serialization_Flags
            if .SKIP_ELEMS_IF_EMPTY in flags {
                member_flags |= { .SKIP_IF_EMPTY }
            }
            
            member_name := as_array ? "" : name
            
            serialize_any(sb, 
                name   = member_name, 
                value  = member_any, 
                indent = member_indent, 
                delim  = member_delim, 
                flags  = member_flags,
            )
        }
        
        if !on_one_line do for i in 0..<indent do strings.write_string(sb, " ");
        
        strings.write_byte(sb, as_array ? ']' : '}' )
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim);
        
        return

      case Type_Info_Array, Type_Info_Slice, Type_Info_Dynamic_Array: 
        data:       rawptr
        elem_count: int
        elem_ti:    ^Type_Info
        
        // disambiguate array/slice/dynamic
        #partial switch tiv in tiv {
          case Type_Info_Array:
            data       = value.data
            elem_count = tiv.count
            elem_ti    = tiv.elem

          case Type_Info_Slice:
            raw_slice := cast(^runtime.Raw_Slice) value.data
            data       = raw_slice.data
            elem_count = raw_slice.len
            elem_ti    = tiv.elem

          case Type_Info_Dynamic_Array:
            raw_dynamic_array := cast(^runtime.Raw_Dynamic_Array) value.data
            data       = raw_dynamic_array.data
            elem_count = raw_dynamic_array.len
            elem_ti    = tiv.elem
            if elem_count == 0 do return // skip serializing empty dynamic arrays
        }

        // skip serializing if all bytes of array data are 0
        if .SKIP_IF_EMPTY in flags && 
           all_bytes_are_zero(data, elem_count * elem_ti.size) {
            return
        }

        for i in 0..<indent do strings.write_string(sb, " ");
        if name != "" {
            strings.write_string(sb, 
                to_conformant_string(name, allocator = context.temp_allocator),
            )
            strings.write_string(sb, " ");
        }

        // serialize as a string if the element type is u8
        if elem_ti.size == 1 {
            str := transmute(string) runtime.Raw_String {
                data = auto_cast data,
                len  = elem_count,
            }
            strings.write_string(sb, 
                to_conformant_string(str, force_quotes = true, allocator = context.temp_allocator),
            )
            strings.write_byte(sb, '\n');
            return 
        }
        
        as_indexed  := .ARRAY_INDEXED in flags 
        as_object   := .AS_OBJECT     in flags 
        
        // by default, print structs and arrays on individual lines, all else print on one line
        // perhaps we should also consider the number of elements?
        // maybe strings should print on individual lines?
        on_one_line := false
        #partial switch elem_tiv in runtime.type_info_base(elem_ti).variant {
          case Type_Info_Array, Type_Info_Slice, Type_Info_Dynamic_Array, Type_Info_Struct:
            break
            
          case: // everything else
            on_one_line = true
        }
        on_one_line |= .ON_ONE_LINE in flags
        
        elem_indent := on_one_line ? 0 : indent + 2
        elem_delim  : string // declared outside the loop so that we can use it afterwards
        
        strings.write_byte(sb, 
            as_indexed || as_object ? '{' : '['
        )
        strings.write_byte(sb, on_one_line ? ' ' : '\n')
        
        for i in 0..<elem_count {
            elem_any := any {
                id   = elem_ti.id,
                data = mem.ptr_offset(cast(^byte)data, elem_ti.size * i),
            }
            
            // We have to figure out the delim on every frame so that we don't write
            //   a comma after the last element when fields are all on one line.
            // elem_delim = type_io_data.serialize.member_delim
            // if elem_delim == "" {
                // I apologize for the nested ternary
                elem_delim = on_one_line ? ((i == elem_count-1) ? " " : ", ") : "\n"
            // }
            
            elem_name: string
            if as_indexed do elem_name = fmt.tprint(i)
            if as_object {
                // TODO: implement normal case to get struct name member
                if type_has_custom_serialization_proc(elem_any.id) {
                    elem_name = " "
                }
            }
            
            elem_flags: Serialization_Flags
            if .SKIP_ELEMS_IF_EMPTY in flags {
                elem_flags |= { .SKIP_IF_EMPTY }
            }
            
            serialize_any(sb, 
                name   = elem_name, 
                value  = elem_any, 
                indent = elem_indent, 
                delim  = elem_delim, 
                flags  = elem_flags, 
            )
        }
        
        if !on_one_line do for i in 0..<indent do strings.write_string(sb, " ");
        
        strings.write_byte(sb, 
            as_indexed || as_object ? '}' : ']'
        )
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim);
            
        return
    
      case Type_Info_String: 
        str: string
        if tiv.is_cstring {
            str = string((cast(^cstring)value.data)^)
        } else {
            str = (cast(^string)value.data)^
        }
        
        for i in 0..<indent do strings.write_string(sb, " ")
        if name != "" {
            strings.write_string(sb, 
                to_conformant_string(name, allocator = context.temp_allocator),
            )
            strings.write_byte(sb, ' ')
        }
        
        strings.write_string(sb, 
            to_conformant_string(str, force_quotes = true, allocator = context.temp_allocator),
        )
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim);
        
        return

      case Type_Info_Bit_Set: 
        for i in 0..<indent do strings.write_string(sb, " ");
        if name != "" {
            strings.write_string(sb, 
                to_conformant_string(name, allocator = context.temp_allocator),
            )
            strings.write_string(sb, " ");
        }

        u64_value: u64
        dynamic_int_cast(u64_value, value)

        bytes := transmute([8]byte) u64_value
        strings.write_string(sb, "[ ")
        #partial switch elem_ti in type_info_base(type_info_of(tiv.elem.id)).variant {
          case Type_Info_Enum:
            for value, i in elem_ti.values {
                if i64(value) >= tiv.lower && i64(value) <= tiv.upper {
                    bit := value - auto_cast tiv.lower
                    if bool(bytes[bit / 8] & u8(1 << u64(bit % 8))) {
                        strings.write_string(sb, elem_ti.names[i])
                        strings.write_string(sb, " ")
                    }
                }
            }
          case Type_Info_Integer:
            for i in tiv.lower..=tiv.upper {
                bit := i - tiv.lower
                if bool(bytes[bit / 8] & u8(1 << u64(bit % 8))) {
                    strings.write_int(sb, int(i))
                    strings.write_string(sb, " ")
                }
            }
          case Type_Info_Rune:
            for i in tiv.lower..=tiv.upper {
                bit := i - tiv.lower
                if bool(bytes[bit / 8] & u8(1 << u64(bit % 8))) {
                    strings.write_rune(sb, rune(i))
                    strings.write_string(sb, " ")
                }
            }
          case:
            fmt.println("Unsupported bit set element type", elem_ti)
            return
        }
        strings.write_string(sb, "]")
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim);
        
        return

      case Type_Info_Map:
        raw_map := transmute(^Raw_Map) value.data
        #partial switch ti_key in runtime.type_info_base(tiv.key).variant {
          case Type_Info_String:
            for i in 0..<indent do strings.write_string(sb, " ");
            if name != "" {
                strings.write_string(sb, 
                    to_conformant_string(name, allocator = context.temp_allocator),
                )
                strings.write_string(sb, " ");
            }
            
            strings.write_string(sb, "{\n")
            m := (^mem.Raw_Map)(value.data)
            
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
          
                    elem_flags := flags
                    serialize_any(sb, (cast(^string)key)^, any{rawptr(value), tiv.value.id}, indent = indent + 2, flags = elem_flags)
                }
            }
                        
            for i in 0..<indent do strings.write_string(sb, " ")
            strings.write_string(sb, "}")
            
            delim := delim != "" ? delim : "\n" 
            strings.write_string(sb, delim);
            
            return
                  
          case: 
            fmt.printf("Unable to serialize type: %v\nCurrently, only maps with string keys are supported.", ti)
            return
        }
        
      case Type_Info_Integer, Type_Info_Float, Type_Info_Enum, Type_Info_Boolean: 
        for i in 0..<indent do strings.write_string(sb, " ");
        
        if name != "" {
            strings.write_string(sb, 
                to_conformant_string(name, allocator = context.temp_allocator),
            )
            strings.write_string(sb, " ");
        }
        
        fmt.sbprintf(sb, "%v", value);
        
        delim := delim != "" ? delim : "\n" 
        strings.write_string(sb, delim);
        
        return
    }    
    
    fmt.println("Unable to serialize type", ti)
    return
}