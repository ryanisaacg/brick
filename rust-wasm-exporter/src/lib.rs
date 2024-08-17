use std::collections::{HashMap, HashSet};

use thiserror::Error;
use wasm_encoder::{
    reencode::{Reencode, RoundtripReencoder},
    Section,
};
use wasmparser::{Chunk, KnownCustom, Name, Parser};

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    WasmParseError(#[from] wasmparser::BinaryReaderError),
    #[error("{0}")]
    WasmConvertError(#[from] wasm_encoder::reencode::Error),
}

pub fn export_dynamic_lib_globals(mut buffer: &[u8]) -> Result<Vec<u8>, Error> {
    let mut current_parser = Parser::new(0);
    let mut stack = Vec::new();

    let mut reencode = RoundtripReencoder;
    let mut sections: Vec<Box<dyn Section>> = Vec::new();

    let mut code_section = wasm_encoder::CodeSection::new();
    let mut code_section_countdown = 0;

    let mut global_section = None;
    let mut export_section = None;

    let mut globals_to_export = HashSet::new();
    let mut global_names = HashMap::new();

    loop {
        if buffer.is_empty() {
            break;
        }

        let (payload, consumed) = match current_parser.parse(buffer, false)? {
            Chunk::NeedMoreData(_hint) => unreachable!(),
            Chunk::Parsed { consumed, payload } => (payload, consumed),
        };

        use wasmparser::Payload::*;
        match payload {
            // Re-encode these parts of the input without modification
            Version { .. } => { /* ... */ }
            TypeSection(parse_section) => {
                let mut encode_section = wasm_encoder::TypeSection::new();
                reencode.parse_type_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            ImportSection(parse_section) => {
                let mut encode_section = wasm_encoder::ImportSection::new();
                reencode.parse_import_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            FunctionSection(parse_section) => {
                let mut encode_section = wasm_encoder::FunctionSection::new();
                reencode.parse_function_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            TableSection(parse_section) => {
                let mut encode_section = wasm_encoder::TableSection::new();
                reencode.parse_table_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            MemorySection(parse_section) => {
                let mut encode_section = wasm_encoder::MemorySection::new();
                reencode.parse_memory_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            TagSection(parse_section) => {
                let mut encode_section = wasm_encoder::TagSection::new();
                reencode.parse_tag_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            ExportSection(parse_section) => {
                let mut index = sections.len();
                if global_section.is_some() {
                    index += 1;
                }
                export_section = Some((index, parse_section));
            }
            StartSection { func, .. } => {
                sections.push(Box::new(wasm_encoder::StartSection {
                    function_index: func,
                }));
            }
            ElementSection(parse_section) => {
                let mut encode_section = wasm_encoder::ElementSection::new();
                reencode.parse_element_section(&mut encode_section, parse_section)?;
                sections.push(Box::new(encode_section));
            }
            DataCountSection { .. } => { /* ... */ }
            DataSection(_) => { /* ... */ }
            CodeSectionStart { count, .. } => {
                code_section = wasm_encoder::CodeSection::new();
                code_section_countdown = count;
            }
            CodeSectionEntry(function) => {
                reencode.parse_function_body(&mut code_section, function)?;
                code_section_countdown -= 1;
                if code_section_countdown == 0 {
                    sections.push(Box::new(code_section));
                    code_section = wasm_encoder::CodeSection::new();
                }
            }
            GlobalSection(globals) => {
                global_section = Some((sections.len(), globals));
            }

            // Sections for WebAssembly components - skipped for now
            InstanceSection(_) => { /* ... */ }
            CoreTypeSection(_) => { /* ... */ }
            ComponentInstanceSection(_) => { /* ... */ }
            ComponentAliasSection(_) => { /* ... */ }
            ComponentTypeSection(_) => { /* ... */ }
            ComponentCanonicalSection(_) => { /* ... */ }
            ComponentStartSection { .. } => { /* ... */ }
            ComponentImportSection(_) => { /* ... */ }
            ComponentExportSection(_) => { /* ... */ }

            ModuleSection { parser, .. } | ComponentSection { parser, .. } => {
                stack.push(current_parser.clone());
                current_parser = parser.clone();
            }

            CustomSection(section) => {
                if let KnownCustom::Name(names) = section.as_known() {
                    for name in names {
                        let Name::Global(global_name) = name? else {
                            continue;
                        };
                        for name in global_name {
                            let name = name?;
                            if name.name == "__stack_pointer" {
                                globals_to_export.insert(name.index);
                                global_names.insert(name.index, "__stack_pointer");
                            }
                        }
                    }
                }
                let encode_section = reencode.custom_section(section);
                sections.push(Box::new(encode_section));
            }

            // most likely you'd return an error here
            UnknownSection { id, .. } => unimplemented!("unknown section: {id}"),

            // Once we've reached the end of a parser we either resume
            // at the parent parser or we break out of the loop because
            // we're done.
            End(_) => {
                if let Some(parent_parser) = stack.pop() {
                    current_parser = parent_parser;
                } else {
                    break;
                }
            }
        }

        // once we're done processing the payload we can forget the
        // original.
        buffer = &buffer[consumed..];
    }

    if let Some((index, globals)) = global_section {
        let mut encode_section = wasm_encoder::GlobalSection::new();
        for (idx, global) in globals.into_iter().enumerate() {
            let wasmparser::Global { mut ty, init_expr } = global?;
            if globals_to_export.contains(&(idx as u32)) {
                ty.mutable = true;
                //ty.shared = true;
            }
            encode_section.global(reencode.global_type(ty)?, &reencode.const_expr(init_expr)?);
        }
        sections.insert(index, Box::new(encode_section));
    }
    if let Some((index, exports)) = export_section {
        let mut encode_section = wasm_encoder::ExportSection::new();
        for export in exports {
            let export = export?;
            if export.kind == wasmparser::ExternalKind::Global {
                globals_to_export.remove(&export.index);
            }
            encode_section.export(export.name, reencode.export_kind(export.kind), export.index);
        }
        for export_index in globals_to_export {
            encode_section.export(
                global_names[&export_index],
                wasm_encoder::ExportKind::Global,
                export_index,
            );
        }
        sections.insert(index, Box::new(encode_section));
    }

    let mut module = Vec::new();
    module.extend(wasm_encoder::Module::HEADER);
    for section in sections {
        module.push(section.id());
        section.encode(&mut module);
    }

    Ok(module)
}

#[cfg(test)]
mod tests {
    use wabt::{wasm2wat, wat2wasm};

    use crate::export_dynamic_lib_globals;

    #[test]
    fn empty_module() {
        let module = wat2wasm("(module)").unwrap();
        export_dynamic_lib_globals(&module[..]).unwrap();
    }

    #[test]
    fn is_prime() {
        // from https://github.com/eliben/wasm-wat-samples/blob/main/prime-test/isprime.wat
        let source = r#";; Basic primality testing function.
;;
;; Eli Bendersky [https://eli.thegreenplace.net]
;; This code is in the public domain.
(module
    ;; is_prime(n) takes a (positive) number and returns 1 if this number is
    ;; prime, 0 if it's composite.
    (func $is_prime (export "is_prime") (param $n i32) (result i32)
        (local $i i32)
        
        ;; n < 2 are not prime
        (i32.lt_u (local.get $n) (i32.const 2))
        if
            i32.const 0
            return
        end

        ;; n == 2 is prime
        (i32.eq (local.get $n) (i32.const 2))
        if
            i32.const 1
            return
        end

        ;; Other even numbers are not prime
        (i32.eq (i32.rem_u (local.get $n) (i32.const 2)) (i32.const 0))
        if
            i32.const 0
            return
        end

        ;; Here we know that n > 2 and that n is odd. Run a loop trying to
        ;; divide it by all odd numbers smaller than it.
        ;;
        ;; for i = 3; i < n; i += 2
        (local.set $i (i32.const 3))
        (loop $testprime (block $breaktestprime
            (i32.ge_u (local.get $i) (local.get $n))
            br_if $breaktestprime

            ;; divisor found; return false
            (i32.eq (i32.rem_u (local.get $n) (local.get $i)) (i32.const 0))
            if
                i32.const 0
                return
            end

            (local.set $i (i32.add (local.get $i) (i32.const 2)))
            br $testprime
        ))

        ;; if we're here, the loop didn't find a divisor
        i32.const 1
    )
)"#;
        let module1 = wat2wasm(source).unwrap();
        let module2 = export_dynamic_lib_globals(&module1[..]).unwrap();
        let wat1 = wasm2wat(module1);
        let wat2 = wasm2wat(module2);
        assert_eq!(wat1, wat2);
    }

    #[test]
    fn export_globals() {
        let source = include_bytes!("./rust-wasm-lib-test.wasm");
        let module = export_dynamic_lib_globals(&source[..]).unwrap();
        let expected = r#"(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param f32)))
  (import "env" "print" (func (;0;) (type 0)))
  (func (;1;) (type 1) (param f32)
    (local i32 f32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 1
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      local.get 0
      f32.const 0x0p+0 (;=0;)
      f32.gt
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 1
        i32.const 12
        i32.add
        call 0
        local.get 1
        local.get 1
        f32.load offset=12
        f32.const 0x1.99999ap-4 (;=0.1;)
        f32.add
        local.tee 2
        f32.store offset=12
        local.get 2
        local.get 0
        f32.lt
        br_if 0 (;@2;)
      end
    end
    local.get 1
    i32.const 16
    i32.add
    global.set 0)
  (memory (;0;) 16)
  (global (;0;) (mut i32) (i32.const 1048576))
  (global (;1;) i32 (i32.const 1048576))
  (global (;2;) i32 (i32.const 1048576))
  (export "memory" (memory 0))
  (export "beesbees" (func 1))
  (export "__data_end" (global 1))
  (export "__heap_base" (global 2))
  (export "__stack_pointer" (global 0)))
"#;
        let actual = wasm2wat(module).unwrap();
        assert_eq!(expected, actual);
    }
}
