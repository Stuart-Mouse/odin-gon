package gon

import "core:runtime"
import "core:reflect"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:mem"
import "core:unicode/utf8"
import "core:math"


/* 
    This single procedure is essentially our data interface layer.
    Implementation is language-specific.
    
    ## TODO
        Consider factoring out []u8-ish cases and passing this proc a string instead.
            Then we can also have options there for whether or not to null-terminate.
            Presumably, if we are writing a string into a buffer, we always want to null-terminate though...
*/
set_value_from_string :: proc(value: any, text: string, no_copy := false, loc := #caller_location) -> bool {
    using runtime
    if text == "" do return true
    ti := type_info_base(type_info_of(value.id))

    #partial switch &tiv in ti.variant {
        case Type_Info_Integer:
            if !dynamic_int_cast(value, strconv.atoi(text)) {
                return false
            }
            return true

        case Type_Info_Float:
            if !dynamic_float_cast(value, strconv.atof(text)) {
                return false
            }
            return true

        case Type_Info_Enum:
            for name, index in tiv.names {
                if name == text {
                    switch ti.size {
                        case 1: (cast(^u8 )value.data)^ = auto_cast tiv.values[index]
                        case 2: (cast(^u16)value.data)^ = auto_cast tiv.values[index]
                        case 4: (cast(^u32)value.data)^ = auto_cast tiv.values[index]
                        case 8: (cast(^u64)value.data)^ = auto_cast tiv.values[index]
                    }
                    return true
                }
            }
            if dynamic_int_cast(value, strconv.atoi(text)) {
                return true
            }
            return false

        case Type_Info_Bit_Set:
            i64_value: u64
            dynamic_int_cast(i64_value, value)
            bytes := transmute(^[8]byte) &i64_value

            elem_ti := type_info_base(tiv.elem)
            #partial switch elem_tiv in elem_ti.variant {
                case Type_Info_Integer:
                    bit := cast(i64) strconv.atoi(text)
                    if bit >= tiv.lower && bit <= tiv.upper {
                        bit -= tiv.lower
                        bytes[bit / 8] |= u8(1 << u64(bit % 8))
                    }
                case Type_Info_Rune:
                    rune_value, _ := utf8.decode_rune_in_string(text)
                    bit := cast(i64) rune_value
                    if bit >= tiv.lower && bit <= tiv.upper {
                        bit -= tiv.lower
                        bytes[bit / 8] |= u8(1 << u64(bit % 8))
                    }
                case Type_Info_Enum:
                    for name, index in elem_tiv.names {
                        if index >= int(tiv.lower) && index <= int(tiv.upper) && name == text {
                            bit := int(elem_tiv.values[index]) - int(tiv.lower)
                            bytes[bit / 8] |= u8(1 << u64(bit % 8))
                        }
                    }
            }

            dynamic_int_cast(value, i64_value)
            return true

        case Type_Info_String:
            string_value := text
            if !no_copy {
                string_value = strings.clone(string_value, loc = loc)
            }
            if tiv.is_cstring {
                (cast(^cstring)value.data)^ = cstring(raw_data(string_value))
            } else {
                (cast(^string)value.data)^ = string_value
            }
            return true

        case Type_Info_Boolean:
            if text[0] == 't' || text[0] == 'T' {
                switch ti.size {
                    case 1: (cast(^b8 )value.data)^ = true
                    case 2: (cast(^b16)value.data)^ = true
                    case 4: (cast(^b32)value.data)^ = true
                    case 8: (cast(^b64)value.data)^ = true
                }
            }
            return true

        case Type_Info_Array:
            if tiv.elem.size != 1 {
                return false
            }
            if len(text) >= tiv.count { // leave one byte pad on the end so we can null terminate
                return false
            }
            mem.copy(value.data, raw_data(text), len(text))
            (transmute([^]u8)value.data)[len(text)] = 0 // null terminate
            return true

        case Type_Info_Slice:
            slice      := cast(^runtime.Raw_Slice) value.data
            data       := slice.data
            elem_count := slice.len
            if tiv.elem.size != 1 {
                return false
            }
            (cast(^string)value.data)^ = strings.clone(text)
            return true
    
        case Type_Info_Dynamic_Array:
            array      := cast(^runtime.Raw_Dynamic_Array) value.data
            elem_count := array.len
            elem_ti    := runtime.type_info_base(tiv.elem)
            if elem_ti.size != 1 {
                return false
            }
            arr_u8 := transmute(^[dynamic]u8) array
            clear(arr_u8)
            append_elem_string(arr_u8, text)
            return true

        case:
            // log("Unsupported type in set_value_from_string(): %v", value.id)
            return true
    }
    
    return true
}

array_add_any :: proc(array: any) -> any {
    if array.data == nil {
		return {}
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
    a := cast(^runtime.Raw_Dynamic_Array) array.data
    
    a.len += 1
    
    if a.len >= a.cap {
        new_cap := max(8, a.cap * 2)
        if !reserve_any_dynamic_array(array, new_cap) {
            return {}
        }
    }
    
    ret := any {
        data = mem.ptr_offset(cast(^u8) a.data, (a.len - 1) * ti_array.elem.size),
        id   = ti_array.elem.id, 
    }
    
    return ret
}

array_add_any_at_index :: proc(array: any, index: int) -> any {
    new_cap := math.next_power_of_two(index+1)
    
    if array.data == nil {
		return false
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
    if !reserve_any_dynamic_array(array, new_cap) {
        return {}
    }
    
    a := cast(^runtime.Raw_Dynamic_Array) array.data
    
    a.len = max(a.len, index+1)
    
    ret := any {
        data = mem.ptr_offset(cast(^u8) a.data, index * ti_array.elem.size),
        id   = ti_array.elem.id, 
    }
    return ret
}

reserve_any_dynamic_array :: proc(array: any, capacity: int) -> bool {
    if array.data == nil {
		return false
	}
    
    ti := type_info_of(array.id)
    ti_array, ok := ti.variant.(runtime.Type_Info_Dynamic_Array)
    if !ok {
        return false
    }
    
	a := cast(^runtime.Raw_Dynamic_Array) array.data 

	if capacity <= a.cap {
		return true
	}

	if a.allocator.procedure == nil {
		a.allocator = context.allocator
	}
	assert(a.allocator.procedure != nil)

	old_size  := a.cap    * ti_array.elem.size
	new_size  := capacity * ti_array.elem.size
	allocator := a.allocator

	new_data, err := mem.resize(a.data, old_size, new_size, ti_array.elem.align, allocator)
	if err != nil {
		return false
	}
	if new_data == nil && new_size > 0 {
		return false
	}

	a.data = new_data
	a.cap  = capacity
	return true
}

// I don't actually know what the rules are for alignment of elements within an array.
// TODO: run some test with aligned structs to figure out what is needed here
// get_size_with_align :: proc(size, align: int) -> int {
//     if align == 0 do return size
    
//     whole     := size / align
//     remainder := size / align
    
//     if remainder != 0 do whole += 1
    
//     return whole * align
// }

// second return value indicates that the any value was actually a pointer
deref_any_pointer :: proc(value: any) -> (any, bool) {
    ti := runtime.type_info_base(type_info_of(value.id))
    ti_pointer, ok := ti.variant.(runtime.Type_Info_Pointer)
    if ok {
        ret := any {
            id   = ti_pointer.elem.id,
            data = (cast(^rawptr)value.data)^,
        }
        return ret, true
    }
    
    // if the type is not a pointer, just return the original value
    return value, false
}

// also works for enum and boolean types, for the sake of convenience
dynamic_int_cast :: proc(dst, src: any, enforce_size := false) -> bool {
    using runtime
  
    ti_src := type_info_base(type_info_of(src.id))
    ti_dst := type_info_base(type_info_of(dst.id))
  
    if enforce_size && ti_src.size > ti_dst.size {
        return false
    }
  
    // This is kind of an ugly solution
    // But basically, just filter out all types which are not int, enum, or bool types
    #partial switch tiv in ti_src.variant {
        case Type_Info_Integer,
             Type_Info_Enum,
             Type_Info_Boolean,
             Type_Info_Bit_Set:
             
        case: return false
    }
    #partial switch tiv in ti_dst.variant {
        case Type_Info_Integer,
             Type_Info_Enum,
             Type_Info_Boolean,
             Type_Info_Bit_Set:
        
        case: return false
    }
  
    i64_value: i64
  
    switch ti_src.size {
        case  1: i64_value = auto_cast (cast(^i8  )src.data)^
        case  2: i64_value = auto_cast (cast(^i16 )src.data)^
        case  4: i64_value = auto_cast (cast(^i32 )src.data)^
        case  8: i64_value = auto_cast (cast(^i64 )src.data)^
        case 16: i64_value = auto_cast (cast(^i128)src.data)^
    }
  
    switch ti_dst.size {
        case  1: (cast(^i8  )dst.data)^ = auto_cast i64_value
        case  2: (cast(^i16 )dst.data)^ = auto_cast i64_value
        case  4: (cast(^i32 )dst.data)^ = auto_cast i64_value
        case  8: (cast(^i64 )dst.data)^ = auto_cast i64_value
        case 16: (cast(^i128)dst.data)^ = auto_cast i64_value
    }
  
    return true
}

dynamic_float_cast :: proc(dst, src: any) -> bool {
    using runtime
    
    ti_src := type_info_base(type_info_of(src.id))
    ti_dst := type_info_base(type_info_of(dst.id))
    
    _, allow_dst := ti_dst.variant.(Type_Info_Float)
    
    if !allow_dst {
        return false
    }
    
    f64_value: f64
    
    #partial switch tiv in ti_src.variant {
        case Type_Info_Float:
            switch ti_src.size {
                case 2: f64_value = auto_cast (cast(^f16)src.data)^
                case 4: f64_value = auto_cast (cast(^f32)src.data)^
                case 8: f64_value = auto_cast (cast(^f64)src.data)^
            }
        case Type_Info_Integer:
            switch ti_src.size {
                case  1: f64_value = auto_cast (cast(^i8  )src.data)^
                case  2: f64_value = auto_cast (cast(^i16 )src.data)^
                case  4: f64_value = auto_cast (cast(^i32 )src.data)^
                case  8: f64_value = auto_cast (cast(^i64 )src.data)^
                case 16: f64_value = auto_cast (cast(^i128)src.data)^
            }
        case:
            return false
    }
    
    switch ti_dst.size {
        case 2: (cast(^f16)dst.data)^ = auto_cast f64_value
        case 4: (cast(^f32)dst.data)^ = auto_cast f64_value
        case 8: (cast(^f64)dst.data)^ = auto_cast f64_value
    }
    
    return true
}

all_bytes_are_zero :: proc {
    all_bytes_are_zero_any,
    all_bytes_are_zero_data,
}

all_bytes_are_zero_any :: proc(value: any) -> bool {
    ti := runtime.type_info_base(type_info_of(value.id))
    bytes := transmute([]u8) runtime.Raw_Slice {
        data = value.data,
        len  = ti.size,
    }
    for b in bytes {
        if b != 0 do return false
    }
    return true
}

all_bytes_are_zero_data :: proc(data: rawptr, len: int) -> bool {
    bytes := transmute([]u8) runtime.Raw_Slice {
        data = data,
        len  = len,
    }
    for b in bytes {
        if b != 0 do return false
    }
    return true
}

dynamic_new :: proc(type: typeid, allocator := context.allocator) -> any {
    ti  := type_info_of(type)
    buf := make([]u8, ti.size, allocator)
    return any {
        data = raw_data(buf),
        id   = type,
    }
}