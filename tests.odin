
package gon

import "core:fmt"
import "core:mem"

/*
    [?]u8 can be parsed from strings in GON
    This facilitates storing fixed-length strings in a more compact, 
        readable format than storing the integer value for each byte.
*/
_test_array_strings :: proc() {
    dst: [32]u8
    
    mem.set(&dst[0], 'a', 32)
    mem.set(&dst[31], 0, 1)

    fmt.println()
    fmt.println(cstring(raw_data(&dst)))

    file := ` string "this is a string" `
    
    ctxt := SAX_Parse_Context {
        file = file,
        data_bindings = {
            { binding = dst,  field_path = "string" },
        },
    }
    
    if !SAX_parse_file(&ctxt) {
        fmt.println("Failed to parse file.")
    }
    
    fmt.println()
    fmt.println(cstring(raw_data(&dst)))
} 

_test_dynamic_arrays :: proc() {
    dst: [dynamic] int
    
    file := `
array [ 
    00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 
    30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 
    60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 
    90, 91, 92, 93, 94, 95, 96, 97, 98, 99
]
`
    ctxt := SAX_Parse_Context {
        file = file,
        data_bindings = {
            { binding = dst,  field_path = "array" },
        },
    }
    
    if !SAX_parse_file(&ctxt) {
        fmt.println("Failed to parse file.")
    }
    
    fmt.println()
    fmt.println(dst)
}









