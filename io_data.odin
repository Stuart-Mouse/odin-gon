package gon

import "core:fmt"
import "core:strconv"
import "core:strings"
import "core:os"
import "core:runtime"
import "core:reflect"
import "core:mem"


Serialization_Flags :: bit_set[Serialization_Flag]
Serialization_Flag :: enum {
    // the struct member or data type will be skipped during serialization if 0-valued
    // arrays will also be skipped if all elements are 0-valued
    // elements within indexed arrays will also be skipped if 0-valued
    SKIP_IF_EMPTY,
    
    // will skip array elems or struct members if they are empty
    // will not skip the array/object itself due to being empty, you would just get "array []" or "object {}"
    SKIP_ELEMS_IF_EMPTY,
    
    // serializes a struct as though it were an array, binding to fields by index rather than by name
    // this should only be used if the structure is stable, as changing the order of fields would cause parsing issues across program versions  
    AS_ARRAY,
    
    // serializes an array of structs as a GON object, using the @gon_name struct member as the name for each object
    // this is primarily used just to make some files more human readable/editable
    AS_OBJECT,
    
    // serialize an object or array on a single line
    // uses ", " as field delimiter by default, so you don't need to manually set a custom delim
    ON_ONE_LINE,
    
    // serializes an array as a GON object, using the index of each element as the name for the object
    ARRAY_INDEXED,
}

Serialization_Settings :: struct {
    flags : Serialization_Flags,
    // TODO: replace with an interface for user to insert nodes manually
    to_string_proc : proc(any) -> (string, string, bool),
}

Parse_Flags :: bit_set[Parse_Flag]
Parse_Flag :: enum {
    // only applies to arrays
    ARRAY_INDEXED,
    ARRAY_ENUMERATED,
    
    INIT, // will initialize via an initialization proc if one is provided, or else memset to 0
    SKIP, // will prevent any data bindings from occurring
}

Parse_Settings :: struct {
    flags      : Parse_Flags,
    parse_proc : proc(^Parser, ^SAX_Field) -> SAX_Return_Code
}


/*
    Add parsing/serialization settings data for all of your data types here at startup.
*/
IO_Data_Lookup : map[typeid]IO_Data

IO_Data :: struct {
    parse     : Parse_Settings,
    serialize : Serialization_Settings,

    // for structs only
    name_member    : reflect.Struct_Field,
    map_key_member : reflect.Struct_Field,
    member_data    : map[string]IO_Data,
    
    // for arrays only
    // can be used to index any array using an enum name
    enum_index_type : typeid,
}

// Do we really even want or need these wrapper functions? 
// Currently, they are very much superfluous.

// get io_data by pointer, which may not be desirable in all cases
get_io_data :: proc(type: typeid) -> (^IO_Data, bool) {
    io_data, found := &IO_Data_Lookup[type]
    return io_data, found
}

register_io_data :: proc(type: typeid, io_data: IO_Data) {
    IO_Data_Lookup[type] = io_data
}

// io_data_set_name_member :: proc(type: typeid, member_name: string) -> bool {
//     io_data, found := &IO_Data_Lookup[type]
//     if !found {
//         IO_Data_Lookup[type] = {}
//         io_data, found = &IO_Data_Lookup[type]
//         if !found do return false
//     }
//     io_data.name_member = reflect.struct_field_by_name(type, member_name)
//     return true
// }
