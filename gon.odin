package gon

import "core:fmt"
import "core:strconv"
import "core:strings"
import "core:os"
import "core:runtime"
import "core:reflect"
import "core:mem"

whitespace_chars :: " ,\t\r\n"
reserved_chars   :: "#{}[]\""
whitespace_and_reserved_chars :: " ,\t\r\n#{}[]\""

Token_Type :: enum {
  INVALID,
  STRING,
  OBJECT_BEGIN,
  OBJECT_END,
  ARRAY_BEGIN,
  ARRAY_END,
  EOF,
}

Field_Type :: enum { 
  INVALID = 0, 
  FIELD   = 1,
  OBJECT  = 2, 
  ARRAY   = 3,
}

print_all_tokens :: proc(file: string) {
  file := file
  next_token_type : Token_Type
  next_token      : string
  fmt.println("Tokens in file:")
  for {
    next_token_type, next_token = get_next_token(&file)
    #partial switch next_token_type {
      case .INVALID:      fmt.println("INV"); return
      case .EOF:          fmt.println("EOF"); return
      case .OBJECT_BEGIN: fmt.println("{")
      case .OBJECT_END:   fmt.println("}")
      case .ARRAY_BEGIN:  fmt.println("[")
      case .ARRAY_END:    fmt.println("]")
      case: fmt.println(next_token)
    }
  }
  fmt.println()
}

// mutates the passed string, advancing it to the position after the returned token
get_next_token :: proc(file: ^string) -> (Token_Type, string) {
  if len(file^) <= 0 do return .EOF, ""
  if !skip_whitespace_and_comments(file) do return .EOF, ""
  if len(file^) <= 0 do return .EOF, ""

  switch file^[0] {
    case '{':
      advance(file)
      return .OBJECT_BEGIN, ""
    case '}':
      advance(file)
      return .OBJECT_END, ""
    case '[':
      advance(file)
      return .ARRAY_BEGIN, ""
    case ']':
      advance(file)
      return .ARRAY_END, ""
  }

  // next token is a string token
  string_value := file^

  // scan for end of string in quotation marks
  if file^[0] == '\"' {
    if !advance(file) do return .INVALID, ""
    string_value = string_value[1:]
    string_len := 0

    for file^[0] != '\"' {
      adv : int = 1
      if file^[0] == '\\' do adv = 2
      if !advance(file, adv) do return .INVALID, ""
      string_len += adv
    }

    if !advance(file) do return .INVALID, ""

    return .STRING, string_value[:string_len]
  }

  // scan for end of bare string
  if !is_reserved_char(file^[0]) {
    string_len := 0
    for !is_reserved_char(file^[0]) && !is_whitespace(file^[0]) {
      if !advance(file) {
        return .EOF, ""
      }
      string_len += 1
    }
    return .STRING, string_value[:string_len]
  }

  // there's probably some funky character in the file...?
  fmt.println("Something funky happened.\n")
  return .INVALID, ""
}

// bascially wraps our slice operation so that we can handle an error in the case that we run out of characters
advance :: proc(file: ^string, amount := 1) -> bool {
  amount := min(amount, len(file))
  file^ = file^[amount:]
  return len(file) != 0 
}

is_whitespace :: proc(char: u8) -> bool {
  return char == ' ' || char == ',' || char == '\t' || char == '\r' || char == '\n'
}

is_reserved_char :: proc(char: u8) -> bool {
  return char == '#' || char == '{' || char == '}' || char == '[' || char == ']'
}

skip_whitespace_and_comments :: proc(file: ^string) -> bool {
  for {
    for is_whitespace(file^[0]) {
      advance(file) or_return
    }
    if file^[0] == '#' {
      for file^[0] != '\n' {
        advance(file) or_return
      }
      continue
    }
    return true
  }
}


/*
  Trying to just get a quick and dirty solution done, so there are some things done very inefficiently.
    For example, the print_to_builder proc was just inteded to allow me to more easily port my Jai code even though the temp allocations are kinda dumb.
*/
serialize_any :: proc(
  sb         : ^strings.Builder, 
  name       : string, 
  value      : any, 
  indent     : int = 0, 
) {
  print_maybe_in_quotes :: proc(sb: ^strings.Builder, name: string) {
    write_quotes := strings.contains_any(name, whitespace_and_reserved_chars)
    if write_quotes do strings.write_string(sb, "\"")
    strings.write_string(sb, name);
    if write_quotes do strings.write_string(sb, "\"")
  }
  print_to_builder :: proc(sb: ^strings.Builder, format: string, args: ..any) {
    strings.write_string(sb, fmt.tprintf(format, ..args))
  }

  using runtime

  if value.data == nil do return
  // if all_bytes_are_zero(value) do return // skip serializing zero'd data

  ti := type_info_base(type_info_of(value.id))
  // if ti_named, ok := ti.variant.(Type_Info_Named); ok {
  //     ti = ti_named.base
  // }

  #partial switch tiv in ti.variant {
    case Type_Info_Struct: 
      for i in 0..<indent do strings.write_string(sb, " ");
      if name != "" {
        print_maybe_in_quotes(sb, name);
        strings.write_string(sb, " ");
      }
      strings.write_string(sb, "{\n");
      member_count := len(tiv.names)
      for i in 0..<member_count {
        type   := tiv.types  [i]
        name   := tiv.names  [i]
        offset := tiv.offsets[i]
        member_any := any {
          data = mem.ptr_offset(cast(^byte)value.data, offset),
          id   = type.id,
        }
        serialize_any(sb, name, member_any, indent + 2);
      }
      for i in 0..<indent do strings.write_string(sb, " ");
      strings.write_string(sb, "}\n");
      return

    case Type_Info_Array, Type_Info_Slice, Type_Info_Dynamic_Array: 
      data       : rawptr
      elem_count : int
      elem_ti    : ^Type_Info
      
      // disambiguate array/slice/dynamic
      #partial switch tiv in tiv {
        case Type_Info_Array:
          data       = value.data
          elem_count = tiv.count
          elem_ti    = type_info_base(tiv.elem)

        case Type_Info_Slice:
          raw_slice := cast(^runtime.Raw_Slice) value.data
          data       = raw_slice.data
          elem_count = raw_slice.len
          elem_ti    = type_info_base(tiv.elem)

        case Type_Info_Dynamic_Array:
          raw_dynamic_array := cast(^runtime.Raw_Dynamic_Array) value.data
          data       = raw_dynamic_array.data
          elem_count = raw_dynamic_array.len
          elem_ti    = type_info_base(tiv.elem)
          if elem_count == 0 do return // skip serializing empty dynamic arrays
      }

      // skip serializing if all bytes of array data are 0
      // if all_bytes_are_zero(data, elem_count * elem_ti.size) {
      //   return
      // }

      for i in 0..<indent do strings.write_string(sb, " ");
      if name != "" {
        print_maybe_in_quotes(sb, name);
        strings.write_string(sb, " ");
      }
      strings.write_string(sb, "[")

      // use a different spacing delimiter between fields vs objects/arrays  
      // TODO: probably pass this is a param to sub-field instead of determining delim based on field type alone
      delim: string = " "
      #partial switch elem_tiv in elem_ti.variant {
        case Type_Info_Array, 
             Type_Info_Slice, 
             Type_Info_Dynamic_Array, 
             Type_Info_Bit_Set, 
             Type_Info_Struct:
          delim = "\n"
      }
      strings.write_string(sb, delim)

      for i in 0..<elem_count {
        item := any {
          id   = elem_ti.id,
          data = mem.ptr_offset(cast(^byte)data, elem_ti.size * i),
        }
        serialize_any(sb, "", item, indent + 2)
      }
      if delim == "\n" {
        for i in 0..<indent do strings.write_string(sb, " ")
      }
      strings.write_string(sb, "]\n")
      return

    case Type_Info_Integer: 
    case Type_Info_Float: 
    case Type_Info_Enum: 
    case Type_Info_Boolean: 
    case Type_Info_String: 
      str: string
      if tiv.is_cstring {
        str = string((cast(^cstring)value.data)^)
      } else {
        str = (cast(^string)value.data)^
      }
      if name != "" {
        for i in 0..<indent do strings.write_string(sb, " ");
        print_maybe_in_quotes(sb, name);
        strings.write_string(sb, " ");
        print_to_builder(sb, "\"%v\"\n", str);
      } else {
        print_to_builder(sb, "\"%v\" ", str);
      }
      return

    case Type_Info_Bit_Set: 
      fmt.println(value)
      for i in 0..<indent do strings.write_string(sb, " ");
      if name != "" {
        print_maybe_in_quotes(sb, name);
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
      strings.write_string(sb, "]\n")
      return

    case:
      fmt.println("Unable to serialize type", ti)
      return
  }    

  if name != "" {
    for i in 0..<indent do strings.write_string(sb, " ");
    print_maybe_in_quotes(sb, name);
    strings.write_string(sb, " ");
    print_to_builder(sb, "%v\n", value);
  }
  else {
    print_to_builder(sb, "%v ", value);
  }
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
    case Type_Info_Integer:
    case Type_Info_Enum:
    case Type_Info_Boolean:
    case Type_Info_Bit_Set:
    case: return false
  }
  #partial switch tiv in ti_dst.variant {
    case Type_Info_Integer:
    case Type_Info_Enum:
    case Type_Info_Boolean:
    case Type_Info_Bit_Set:
    case: return false
  }

  i64_value: i64

  switch ti_src.size {
    case 1 : i64_value = auto_cast (cast(^i8  )src.data)^
    case 2 : i64_value = auto_cast (cast(^i16 )src.data)^
    case 4 : i64_value = auto_cast (cast(^i32 )src.data)^
    case 8 : i64_value = auto_cast (cast(^i64 )src.data)^
    case 16: i64_value = auto_cast (cast(^i128)src.data)^
  }

  switch ti_dst.size {
    case 1 : (cast(^i8  )dst.data)^ = auto_cast i64_value
    case 2 : (cast(^i16 )dst.data)^ = auto_cast i64_value
    case 4 : (cast(^i32 )dst.data)^ = auto_cast i64_value
    case 8 : (cast(^i64 )dst.data)^ = auto_cast i64_value
    case 16: (cast(^i128)dst.data)^ = auto_cast i64_value
  }

  return true
}


dynamic_float_cast :: proc(dst, src: any, enforce_size := false) -> bool {
  using runtime

  ti_src := type_info_base(type_info_of(src.id))
  ti_dst := type_info_base(type_info_of(dst.id))

  if enforce_size && ti_src.size > ti_dst.size {
    return false
  }

  _, allow_src := ti_src.variant.(Type_Info_Float)
  _, allow_dst := ti_dst.variant.(Type_Info_Float)
  if !allow_src || !allow_dst {
    return false
  }

  f64_value: f64

  switch ti_src.size {
    case 2: f64_value = auto_cast (cast(^f16)src.data)^
    case 4: f64_value = auto_cast (cast(^f32)src.data)^
    case 8: f64_value = auto_cast (cast(^f64)src.data)^
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
  using runtime

  ti := type_info_base(type_info_of(value.id))
  bytes := transmute([]u8) Raw_Slice {
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

// TODO: needs further implementation
Serialization_Flags :: bit_set[Serialization_Flag]
Serialization_Flag :: enum {
  // when applied to a struct member, that member will never be serialized 
  SKIP_ALWAYS,

  // the struct member or data type will be skipped during serialization if 0-valued
  // arrays will also be skipped if all elements are 0-valued
  SKIP_IF_EMPTY,

  // when applied to a struct member, that member will always be serialized, even if 0-valued 
  SKIP_NEVER,

  // serializes a struct as though it were an array, binding to fields by index rather than by name
  // this should only be used if the structure is stable, as changing the order of fields would cause parsing issues across program versions  
  SERIALIZE_AS_ARRAY,

  // serializes an array of structs as a GON object, using the @gon_name struct member as the name for each object
  // this is primarily used just to make some files more human readable/editable
  SERIALIZE_AS_OBJECT,

  // to be used when some data type or struct member represents sensitive data
  // will only be serialized when the corresponding flag is present in the serialization settings
  SENSITIVE,
}

Serialization_Settings_Struct :: struct {
  one_line         : bool,
  member_delimiter : []string, // can have a unique delimiter between each field
}