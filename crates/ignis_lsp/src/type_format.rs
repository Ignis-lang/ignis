use std::collections::HashMap;

use ignis_type::definition::DefinitionStore;
use ignis_type::symbol::SymbolId;
use ignis_type::types::{Type, TypeId, TypeStore};

pub fn format_type(
  types: &TypeStore,
  defs: &DefinitionStore,
  symbol_names: &HashMap<SymbolId, String>,
  type_id: &TypeId,
) -> String {
  let ty = types.get(type_id);
  match ty {
    Type::I8 => "i8".to_string(),
    Type::I16 => "i16".to_string(),
    Type::I32 => "i32".to_string(),
    Type::I64 => "i64".to_string(),
    Type::U8 => "u8".to_string(),
    Type::U16 => "u16".to_string(),
    Type::U32 => "u32".to_string(),
    Type::U64 => "u64".to_string(),
    Type::F32 => "f32".to_string(),
    Type::F64 => "f64".to_string(),
    Type::Boolean => "boolean".to_string(),
    Type::Char => "char".to_string(),
    Type::String => "string".to_string(),
    Type::Atom => "atom".to_string(),
    Type::Void => "void".to_string(),
    Type::Never => "never".to_string(),
    Type::Infer => "infer".to_string(),
    Type::NullPtr => "null".to_string(),
    Type::Error => "error".to_string(),
    Type::Pointer { inner, mutable } => {
      if *mutable {
        format!("*mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("*{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Reference { inner, mutable } => {
      if *mutable {
        format!("&mut {}", format_type(types, defs, symbol_names, inner))
      } else {
        format!("&{}", format_type(types, defs, symbol_names, inner))
      }
    },
    Type::Vector { element, size } => {
      format!("{}[{}]", format_type(types, defs, symbol_names, element), size)
    },
    Type::Tuple(elements) => {
      let elem_strs: Vec<_> = elements
        .iter()
        .map(|e| format_type(types, defs, symbol_names, e))
        .collect();
      format!("({})", elem_strs.join(", "))
    },
    Type::Function {
      params,
      ret,
      is_variadic,
    } => {
      let param_strs: Vec<_> = params
        .iter()
        .map(|p| format_type(types, defs, symbol_names, p))
        .collect();
      let variadic = if *is_variadic { ", ..." } else { "" };
      format!(
        "({}{}) -> {}",
        param_strs.join(", "),
        variadic,
        format_type(types, defs, symbol_names, ret)
      )
    },
    Type::Record(def_id) => symbol_names
      .get(&defs.get(def_id).name)
      .cloned()
      .unwrap_or_else(|| "?".to_string()),
    Type::Enum(def_id) => symbol_names
      .get(&defs.get(def_id).name)
      .cloned()
      .unwrap_or_else(|| "?".to_string()),
    Type::Param { index, .. } => {
      format!("T{}", index)
    },
    Type::Instance { generic, args } => {
      let base_name = symbol_names
        .get(&defs.get(generic).name)
        .cloned()
        .unwrap_or_else(|| "?".to_string());
      let arg_strs: Vec<_> = args.iter().map(|a| format_type(types, defs, symbol_names, a)).collect();
      format!("{}<{}>", base_name, arg_strs.join(", "))
    },
  }
}
