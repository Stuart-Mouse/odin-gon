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
    SERIALIZE_ARRAY_INDEXED,
}

Serialization_Settings :: struct {
    flags : Serialization_Flags,
    
    // TODO: make decision whether to properly remove
    // Not really necessary. Realistically, there's no reason to need to set the delim manually now that we have the on_one_line flag
    // member_delim   : string, 
  
    // Having completely customized serialization procedures that get all of the same parameters as serialzie_any
    //   seems like it would be too much complication to ask the user to implement for what should be a simple 
    //   interface for extending serialization.
    // So instead, the intended usage is to offer amore simple callback in which the user is asked only to provided the stringified value.
    // This will allow the usual code to handle all of the usual indentation, flags, etc.
    // Technically, this is less powerful, but it is also so much simpler and I doubt that most people would have need for the options
    //   offered by the more complex implementation.
    // And if they do, they may as well write it into the library themself.
    to_string_proc : proc(any) -> (string, string, bool),
}

Parse_Flags :: bit_set[Parse_Flag]
Parse_Flag :: enum {
    // only applies to arrays
    PARSE_ARRAY_INDEXED,
    
    INIT, // will initialize via an initialization proc if one is provided, or else memset to 0
    SKIP, // will prevent any data bindings from occurring
}

Parse_Settings :: struct {
    flags      : Parse_Flags,

    parse_proc : proc(^Parser, ^SAX_Field) -> SAX_Return_Code
    // init_proc  : proc(rawptr) -> bool // TODO
}


/*
    Add parsing/serialization settings data for all of your data types here at startup.
*/
IO_Data_Lookup : map[typeid]IO_Data

IO_Data :: struct {
    parse     : Parse_Settings,
    serialize : Serialization_Settings,

    // for structs only
    name_member    : string,
    map_key_member : string,
    member_data    : map[string]IO_Data,
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
