
package gon

import "core:fmt"
import "core:mem"
import "core:os"

// Test_Enum :: enum {

// }

// Test_Struct :: struct {
//     t_u8           : u8,
//     t_i8           : i8,
//     t_u16          : u16,
//     t_i16          : i16,
//     t_u32          : u32,
//     t_i32          : i32,
//     t_u64          : u64,
//     t_i64          : i64,
//     t_f16          : f16,
//     t_f32          : f32,
//     t_f64          : f64,
//     t_enum         : enum { thing1, thing2, apple, red, west },
//     t_i32_2        : [2] i32,
//     t_i32_3        : [3] i32,
//     t_i32_4        : [4] i32,
//     t_f32_2        : [2] f32,
//     t_f32_3        : [3] f32,
//     t_f32_4        : [4] f32,
//     t_bool         : bool,
//     t_b8           : b8,
//     t_b16          : b16,
//     t_b32          : b32,
//     t_b64          : b64,
//     t_u8_16        : [16] u8,
//     t_u8_slice     : [] u8,
//     t_u8_dynamic   : [dynamic] u8,
//     t_bit_set_int  : bit_set[5..=10],
//     t_bit_set_rune : bit_set['a'..='z'],
//     t_bit_set_enum : bit_set[enum{ thing1, thing2, apple, red, west }],
//     t_string       : string,
//     t_cstring      : cstring,
// }

/*
    u8,    0, 255
    i8, -128, 127
    
    u16,       0, 65_535
    i16, -32_768, 32_767
    
    u32,              0, 4_294_967_295
    i32, -2_147_483_648, 2_147_483_647
    
    u64,                          0, 18_446_744_073_709_551_615
    i64, -9_223_372_036_854_775_808,  9_223_372_036_854_775_807 
*/

_test_set_value_from_string :: proc() -> bool {
    success := true

    test_integers :: proc() -> bool {
        t_u8  : u8
        t_i8  : i8
        t_u16 : u16
        t_i16 : i16
        t_u32 : u32
        t_i32 : i32
        t_u64 : u64
        t_i64 : i64
        
        success := true
        
        set_value_from_string(t_u8, "0")
        if t_u8 != u8(0) {
            fmt.println("Failed test for type 'u8', value '255'")
            success = false
        }
        set_value_from_string(t_u8, "-1")
        if t_u8 != u8(255) {
            fmt.println("Failed test for type 'u8', value '-1'")
            success = false
        }
        set_value_from_string(t_u8, "255")
        if t_u8 != u8(255) {
            fmt.println("Failed test for type 'u8', value '255'")
            success = false
        }
        set_value_from_string(t_u8, "256")
        if t_u8 != u8(0) {
            fmt.println("Failed test for type 'u8', value '256'")
            success = false
        }
        set_value_from_string(t_i8, "-128")
        if t_i8 != i8(-128) {
            fmt.println("Failed test for type 'i8', value '127'")
            success = false
        }
        set_value_from_string(t_i8, "-129")
        if t_i8 != i8(127) {
            fmt.println("Failed test for type 'i8', value '-129'")
            success = false
        }
        set_value_from_string(t_i8, "127")
        if t_i8 != i8(127) {
            fmt.println("Failed test for type 'i8', value '127'")
            success = false
        }
        set_value_from_string(t_i8, "128")
        if t_i8 != i8(-128) {
            fmt.println("Failed test for type 'i8', value '128'")
            success = false
        }
        set_value_from_string(t_u16, "0")
        if t_u16 != u16(0) {
            fmt.println("Failed test for type 'u16', value '65535'")
            success = false
        }
        set_value_from_string(t_u16, "-1")
        if t_u16 != u16(65535) {
            fmt.println("Failed test for type 'u16', value '-1'")
            success = false
        }
        set_value_from_string(t_u16, "65535")
        if t_u16 != u16(65535) {
            fmt.println("Failed test for type 'u16', value '65535'")
            success = false
        }
        set_value_from_string(t_u16, "65536")
        if t_u16 != u16(0) {
            fmt.println("Failed test for type 'u16', value '65536'")
            success = false
        }
        set_value_from_string(t_i16, "-32768")
        if t_i16 != i16(-32768) {
            fmt.println("Failed test for type 'i16', value '32767'")
            success = false
        }
        set_value_from_string(t_i16, "-32769")
        if t_i16 != i16(32767) {
            fmt.println("Failed test for type 'i16', value '-32769'")
            success = false
        }
        set_value_from_string(t_i16, "32767")
        if t_i16 != i16(32767) {
            fmt.println("Failed test for type 'i16', value '32767'")
            success = false
        }
        set_value_from_string(t_i16, "32768")
        if t_i16 != i16(-32768) {
            fmt.println("Failed test for type 'i16', value '32768'")
            success = false
        }
        set_value_from_string(t_u32, "0")
        if t_u32 != u32(0) {
            fmt.println("Failed test for type 'u32', value '4294967295'")
            success = false
        }
        set_value_from_string(t_u32, "-1")
        if t_u32 != u32(4294967295) {
            fmt.println("Failed test for type 'u32', value '-1'")
            success = false
        }
        set_value_from_string(t_u32, "4294967295")
        if t_u32 != u32(4294967295) {
            fmt.println("Failed test for type 'u32', value '4294967295'")
            success = false
        }
        set_value_from_string(t_u32, "4294967296")
        if t_u32 != u32(0) {
            fmt.println("Failed test for type 'u32', value '4294967296'")
            success = false
        }
        set_value_from_string(t_i32, "-2147483648")
        if t_i32 != i32(-2147483648) {
            fmt.println("Failed test for type 'i32', value '2147483647'")
            success = false
        }
        set_value_from_string(t_i32, "-2147483649")
        if t_i32 != i32(2147483647) {
            fmt.println("Failed test for type 'i32', value '-2147483649'")
            success = false
        }
        set_value_from_string(t_i32, "2147483647")
        if t_i32 != i32(2147483647) {
            fmt.println("Failed test for type 'i32', value '2147483647'")
            success = false
        }
        set_value_from_string(t_i32, "2147483648")
        if t_i32 != i32(-2147483648) {
            fmt.println("Failed test for type 'i32', value '2147483648'")
            success = false
        }
        set_value_from_string(t_u64, "0")
        if t_u64 != u64(0) {
            fmt.println("Failed test for type 'u64', value '18446744073709551615'")
            success = false
        }
        set_value_from_string(t_u64, "-1")
        if t_u64 != u64(18446744073709551615) {
            fmt.println("Failed test for type 'u64', value '-1'")
            success = false
        }
        set_value_from_string(t_u64, "18446744073709551615")
        if t_u64 != u64(18446744073709551615) {
            fmt.println("Failed test for type 'u64', value '18446744073709551615'")
            success = false
        }
        set_value_from_string(t_u64, "18446744073709551616")
        if t_u64 != u64(0) {
            fmt.println("Failed test for type 'u64', value '18446744073709551616'")
            success = false
        }
        set_value_from_string(t_i64, "-9223372036854775808")
        if t_i64 != i64(-9223372036854775808) {
            fmt.println("Failed test for type 'i64', value '9223372036854775807'")
            success = false
        }
        set_value_from_string(t_i64, "-9223372036854775809")
        if t_i64 != i64(9223372036854775807) {
            fmt.println("Failed test for type 'i64', value '-9223372036854775809'")
            success = false
        }
        set_value_from_string(t_i64, "9223372036854775807")
        if t_i64 != i64(9223372036854775807) {
            fmt.println("Failed test for type 'i64', value '9223372036854775807'")
            success = false
        }
        set_value_from_string(t_i64, "9223372036854775808")
        if t_i64 != i64(-9223372036854775808) {
            fmt.println("Failed test for type 'i64', value '9223372036854775808'")
            success = false
        }
        
        return success
    }
    
    if !test_integers() {
        success = false
    } else {
        fmt.println("Passed all integer tests!")
    }
    
    
    // test float types
    
    // test enums
    // as integers
    // named
    
    // test bit sets
    // set multiple times
    
    // test strings
    // test cstrings
    
    // test booleans
    
    // test array strings
    
    return success
}


_test_basics :: proc() {
    // file := `
    //     test {
    //         t_u8           1
    //         t_i8           2
    //         t_u16          3
    //         t_i16          4
    //         t_u32          5
    //         t_i32          6
    //         t_u64          7
    //         t_i64          8
    //         t_f16          9
    //         t_f32          10
    //         t_f64          11
    //         t_enum         apple
    //         t_i32_2        [ 1, 2 ]
    //         t_i32_3        [ 1, 2, 3 ]
    //         t_i32_4        [ 1, 2, 3, 4 ]
    //         t_f32_2        [ 1.1, 2.2 ]
    //         t_f32_3        [ 1.1, 2.2, 3.3 ]
    //         t_f32_4        [ 1.1, 2.2, 3.3, 4.4 ]
    //         t_bool         true
    //         t_b8           
    //         t_b16          
    //         t_b32          
    //         t_b64          
    //         t_u8_16        
    //         t_u8_slice     
    //         t_u8_dynamic   
    //         t_bit_set_int  
    //         t_bit_set_rune 
    //         t_bit_set_enum [ apple, red ]
    //         t_string       
    //         t_cstring      
    //     }
    // `

    // test_struct: Test_Struct

    // // parsing the test_struct
    // ctxt := SAX_Parse_Context {
    //     file = file,
        
    // }

    // // serializing the test struct
    // serialize_any("test", test_struct)
    
    
} 



/*
    [?]u8 can be parsed from strings in GON
    This facilitates storing fixed-length strings in a more compact, 
        readable format than storing the integer value for each byte.
*/
_test_array_strings :: proc() {
    dst: [32]u8
    
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

_test_indexed_arrays :: proc() {

}




