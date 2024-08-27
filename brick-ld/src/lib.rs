use std::collections::HashMap;

use thiserror::Error;

use wasm_encoder::reencode::{Reencode, RoundtripReencoder};
use wasmparser::{ExternalKind, Operator, TypeRef};

#[derive(Default)]
pub struct InputModule<'a> {
    pub name: &'a str,
    pub definition: &'a [u8],
    pub public_exports: bool,
    pub is_start: bool,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("{0}")]
    WasmParseError(#[from] wasmparser::BinaryReaderError),
    #[error("{0}")]
    WasmConvertError(#[from] wasm_encoder::reencode::Error),
    #[error("may not have more than one start section, both {0} and {1} do")]
    MoreThanOneStartSection(String, String),
    #[error("wasm64 memories not supported in module {0}")]
    Wasm64Memory(String),
    #[error("multi-memory not supported in module {0}")]
    MoreThanOneMemory(String),
    #[error("mismatched type between {}'s import of {} from {}: expected {:?}, received {:?}", .0.import_module, .0.name, .0.export_module, .0.import_ty, .0.export_ty)]
    MismatchedType(Box<MismatchedTypeInfo>),
}

#[derive(Debug)]
pub struct MismatchedTypeInfo {
    import_module: String,
    export_module: String,
    name: String,
    import_ty: TypeRef,
    export_ty: TypeRef,
}

pub fn link(modules: &[InputModule]) -> Result<Vec<u8>, Error> {
    let mut parsed = modules
        .iter()
        .map(ParsedModule::parse)
        .collect::<Result<Vec<_>, _>>()?;
    let name_to_idx: HashMap<_, _> = parsed
        .iter()
        .enumerate()
        .map(|(idx, module)| (module.name, idx))
        .collect();
    let mut module_exports = HashMap::new();
    for module in parsed.iter() {
        let exports: HashMap<_, _> = module
            .exports
            .iter()
            .map(|export| (export.name, (export.kind, export.index)))
            .collect();
        module_exports.insert(module.name, exports);
    }
    satisfy_imports(&mut parsed, &module_exports, &name_to_idx)?;
    let offsets = calculate_offsets(&parsed)
        .into_iter()
        .zip(parsed.iter().map(|module| module.name))
        .map(|(offset, name)| (name, offset))
        .collect();
    for module in parsed.iter_mut() {
        module.apply_offset(&offsets);
    }

    encode(parsed)
}

#[derive(Default)]
struct ParsedModule<'a> {
    name: &'a str,
    types: Vec<wasmparser::RecGroup>,

    imports: Vec<Import<'a>>,
    imported_functions: u32,
    imported_tables: u32,
    imported_globals: u32,

    functions: Vec<u32>,
    tables: Vec<wasmparser::Table<'a>>,
    memory: Option<wasmparser::MemoryType>,
    globals: Vec<wasmparser::Global<'a>>,
    exports: Vec<wasmparser::Export<'a>>,
    start: Option<(u32, &'a str)>,
    elements: Vec<Element<'a>>,
    data: Vec<wasmparser::Data<'a>>,
    function_bodies: Vec<FunctionBody<'a>>,
    public_exports: bool,
}

struct FunctionBody<'a> {
    locals: Vec<(u32, wasmparser::ValType)>,
    operators: Vec<Operator<'a>>,
}

struct Element<'a> {
    kind: wasmparser::ElementKind<'a>,
    items: ElementItems<'a>,
}

#[derive(Debug)]
enum ElementItems<'a> {
    Functions(Vec<u32>),
    Expressions(wasmparser::RefType, Vec<wasmparser::ConstExpr<'a>>),
}

impl ParsedModule<'_> {
    fn parse<'a>(module: &InputModule<'a>) -> Result<ParsedModule<'a>, Error> {
        let mut buffer = module.definition;
        let mut parsed_module = ParsedModule {
            name: module.name,
            public_exports: module.public_exports,
            ..ParsedModule::default()
        };

        let mut current_parser = wasmparser::Parser::new(0);
        let mut stack = Vec::new();

        loop {
            if buffer.is_empty() {
                break;
            }
            let (payload, consumed) = match current_parser.parse(buffer, false)? {
                wasmparser::Chunk::NeedMoreData(_hint) => unreachable!(),
                wasmparser::Chunk::Parsed { consumed, payload } => (payload, consumed),
            };
            buffer = &buffer[consumed..];

            use wasmparser::Payload::*;
            match payload {
                Version { .. } => {}
                TypeSection(ty_section) => {
                    for ty in ty_section {
                        parsed_module.types.push(ty?);
                    }
                }
                ImportSection(import_section) => {
                    for import in import_section {
                        let import = import?;
                        match &import.ty {
                            TypeRef::Func(_) => {
                                parsed_module.imported_functions += 1;
                            }
                            TypeRef::Table(_) => {
                                parsed_module.imported_tables += 1;
                            }
                            TypeRef::Global(_) => {
                                parsed_module.imported_globals += 1;
                            }
                            TypeRef::Memory(_) | TypeRef::Tag(_) => {}
                        }
                        parsed_module.imports.push(Import::Unresolved(import));
                    }
                }
                FunctionSection(function_section) => {
                    for function in function_section {
                        parsed_module.functions.push(function?);
                    }
                }
                // TODO: table
                TableSection(table_section) => {
                    for table in table_section {
                        parsed_module.tables.push(table?);
                    }
                }
                MemorySection(memory_section) => {
                    let mut mem_iter = memory_section.into_iter();
                    let memory = mem_iter.next();
                    let Some(memory) = memory else {
                        continue;
                    };
                    if mem_iter.next().is_some() {
                        return Err(Error::MoreThanOneMemory(module.name.to_string()));
                    }
                    let memory = memory?;
                    if memory.memory64 {
                        return Err(Error::Wasm64Memory(module.name.to_string()));
                    }
                    parsed_module.memory = Some(memory);
                }
                TagSection(_) => {}
                GlobalSection(global_section) => {
                    for global in global_section {
                        parsed_module.globals.push(global?);
                    }
                }
                ExportSection(export_section) => {
                    for export in export_section {
                        parsed_module.exports.push(export?);
                    }
                }
                StartSection { func, .. } => {
                    if module.is_start {
                        parsed_module.start = Some((func, module.name));
                    }
                }
                // TODO: data / elements
                ElementSection(element_section) => {
                    for element in element_section {
                        let element = element?;
                        parsed_module.elements.push(Element {
                            kind: if let wasmparser::ElementKind::Active {
                                table_index,
                                offset_expr,
                            } = element.kind
                            {
                                wasmparser::ElementKind::Active {
                                    table_index: table_index.or(Some(0)),
                                    offset_expr,
                                }
                            } else {
                                element.kind
                            },
                            items: match element.items {
                                wasmparser::ElementItems::Functions(functions) => {
                                    ElementItems::Functions(
                                        functions.into_iter().collect::<Result<Vec<_>, _>>()?,
                                    )
                                }
                                wasmparser::ElementItems::Expressions(ty, expr) => {
                                    ElementItems::Expressions(
                                        ty,
                                        expr.into_iter().collect::<Result<Vec<_>, _>>()?,
                                    )
                                }
                            },
                        });
                    }
                }
                DataCountSection { .. } => {}
                DataSection(data_section) => {
                    for data in data_section {
                        parsed_module.data.push(data?);
                    }
                }
                CodeSectionStart { .. } => {}
                CodeSectionEntry(function) => {
                    let locals = function
                        .get_locals_reader()?
                        .into_iter()
                        .collect::<Result<Vec<_>, _>>()?;
                    let operators = function
                        .get_operators_reader()?
                        .into_iter()
                        .collect::<Result<Vec<_>, _>>()?;
                    parsed_module
                        .function_bodies
                        .push(FunctionBody { locals, operators });
                }

                ModuleSection { parser, .. } => {
                    stack.push(current_parser.clone());
                    current_parser = parser.clone();
                }

                InstanceSection(_)
                | CoreTypeSection(_)
                | ComponentSection { .. }
                | ComponentInstanceSection(_)
                | ComponentAliasSection(_)
                | ComponentTypeSection(_)
                | ComponentCanonicalSection(_)
                | ComponentStartSection { .. }
                | ComponentImportSection(_)
                | ComponentExportSection(_) => unimplemented!("WASM components not yet supported"),

                // TODO: custom sections
                CustomSection(_) => {}
                UnknownSection { .. } => panic!("unknown section encountered"),

                End(_) => {
                    if let Some(parent_parser) = stack.pop() {
                        current_parser = parent_parser;
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(parsed_module)
    }

    fn apply_offset(&mut self, offsets: &HashMap<&str, ModuleOffsets>) {
        let self_offset = &offsets[self.name];
        for func_header in self.functions.iter_mut() {
            *func_header += self_offset.types;
        }
        let mut function_remap = HashMap::new();
        let mut table_remap = HashMap::new();
        let mut global_remap = HashMap::new();

        let mut import_func_source_idx = 0;
        let mut import_func_dest_idx = self_offset.import_functions;

        let mut import_table_source_idx = 0;
        let mut import_table_dest_idx = self_offset.import_tables;

        let mut import_global_source_idx = 0;
        let mut import_global_dest_idx = self_offset.import_globals;

        for import in self.imports.iter() {
            match import {
                Import::Unresolved(import) => match import.ty {
                    TypeRef::Func(_) => {
                        function_remap.insert(import_func_source_idx, import_func_dest_idx);
                        import_func_dest_idx += 1;
                        import_func_source_idx += 1;
                    }
                    TypeRef::Table(_) => {
                        table_remap.insert(import_table_source_idx, import_table_dest_idx);
                        import_table_source_idx += 1;
                        import_table_dest_idx += 1;
                    }
                    TypeRef::Global(_) => {
                        global_remap.insert(import_global_source_idx, import_global_dest_idx);
                        import_global_source_idx += 1;
                        import_global_dest_idx += 1;
                    }
                    TypeRef::Memory(_) | TypeRef::Tag(_) => {}
                },
                Import::Function { module, index } => {
                    function_remap
                        .insert(import_func_source_idx, offsets[module].functions + index);
                    import_func_source_idx += 1;
                }
                Import::Global { module, index } => {
                    global_remap.insert(import_global_source_idx, offsets[module].globals + index);
                    import_global_source_idx += 1;
                }
                Import::Memory => {}
            }
        }
        for idx in 0..(self.functions.len() as u32) {
            function_remap.insert(self.imported_functions + idx, self_offset.functions + idx);
        }
        for idx in 0..(self.tables.len() as u32) {
            table_remap.insert(self.imported_tables + idx, self_offset.tables + idx);
        }
        for idx in 0..(self.globals.len() as u32) {
            global_remap.insert(self.imported_globals + idx, self_offset.globals + idx);
        }

        for body in self.function_bodies.iter_mut() {
            for instr in body.operators.iter_mut() {
                match instr {
                    Operator::GlobalGet { global_index } | Operator::GlobalSet { global_index } => {
                        *global_index = global_remap[global_index as &u32];
                    }
                    Operator::Call { function_index } => {
                        *function_index = function_remap[function_index as &u32];
                    }
                    Operator::CallIndirect {
                        type_index,
                        table_index,
                    } => {
                        *type_index += self_offset.types;
                        *table_index = table_remap[table_index as &u32];
                    }
                    _ => {}
                }
            }
        }

        for element in self.elements.iter_mut() {
            // TODO: process the constexpr when dealing with globals
            if let wasmparser::ElementKind::Active { table_index, .. } = &mut element.kind {
                if let Some(table_index) = table_index.as_mut() {
                    *table_index = table_remap[table_index as &u32];
                }
            }
            match &mut element.items {
                ElementItems::Functions(functions) => {
                    for function_index in functions.iter_mut() {
                        *function_index = function_remap[function_index as &u32];
                    }
                }
                // TODO: process the constexpr
                ElementItems::Expressions(_, _) => {}
            }
        }

        for export in self.exports.iter_mut() {
            if export.kind == ExternalKind::Func {
                export.index = function_remap[&export.index];
            }
        }

        for _global in self.globals.iter_mut() {
            // TODO: process the constexpr
        }

        if let Some((start_idx, _)) = self.start.as_mut() {
            *start_idx = function_remap[start_idx as &u32];
        }
    }
}

enum Import<'a> {
    Unresolved(wasmparser::Import<'a>),
    Memory,
    Function { module: &'a str, index: u32 },
    Global { module: &'a str, index: u32 },
}

#[derive(Clone, Debug, Default)]
struct ModuleOffsets {
    import_functions: u32,
    import_tables: u32,
    import_globals: u32,
    functions: u32,
    types: u32,
    tables: u32,
    globals: u32,
}

fn satisfy_imports(
    parsed_modules: &mut [ParsedModule],
    exports: &HashMap<&str, HashMap<&str, (ExternalKind, u32)>>,
    name_to_idx: &HashMap<&str, usize>,
) -> Result<(), Error> {
    let mut resolved_imports = Vec::new();

    // Ensure all imports match the export they're being matched with
    // and calculate what their export kind / index is
    for module in parsed_modules.iter() {
        let mut imports_for_module = HashMap::new();
        for (import_idx, import) in module.imports.iter().enumerate() {
            let Import::Unresolved(import) = import else {
                unreachable!();
            };
            let Some((kind, idx)) = exports
                .get(import.module)
                .and_then(|import_module| import_module.get(import.name))
            else {
                continue;
            };
            let export_module_idx = name_to_idx[import.module];
            let export_module = &parsed_modules[export_module_idx];
            let export_ty = match kind {
                ExternalKind::Func => TypeRef::Func(*idx),
                ExternalKind::Table => todo!(),
                ExternalKind::Memory => TypeRef::Memory(export_module.memory.unwrap()),
                ExternalKind::Global => TypeRef::Global(export_module.globals[*idx as usize].ty),
                ExternalKind::Tag => unimplemented!("tags not yet supported"),
            };
            let do_types_match = match (import.ty, export_ty) {
                (TypeRef::Func(import_idx), TypeRef::Func(export_idx)) => {
                    let fn_ty = export_module.functions
                        [(export_idx - export_module.imported_functions) as usize];
                    module.types[import_idx as usize] == export_module.types[fn_ty as usize]
                }
                (TypeRef::Table(_), TypeRef::Table(_)) => todo!(),
                (TypeRef::Memory(_), TypeRef::Memory(_)) => true,
                (TypeRef::Global(import_global), TypeRef::Global(export_global)) => {
                    import_global == export_global
                }
                (TypeRef::Tag(_), TypeRef::Tag(_)) => unimplemented!(),
                _ => false,
            };
            if !do_types_match {
                return Err(Error::MismatchedType(Box::new(MismatchedTypeInfo {
                    import_module: module.name.to_string(),
                    export_module: import.module.to_string(),
                    name: import.name.to_string(),
                    import_ty: import.ty,
                    export_ty,
                })));
            }
            imports_for_module.insert(import_idx, (import.module, kind, idx));
        }
        resolved_imports.push(imports_for_module);
    }

    for (module, mut resolved_imports) in parsed_modules.iter_mut().zip(resolved_imports.drain(..))
    {
        for (idx, import) in module.imports.iter_mut().enumerate() {
            let Some((export_module, export_kind, export_idx)) = resolved_imports.remove(&idx)
            else {
                continue;
            };

            match export_kind {
                ExternalKind::Func => {
                    *import = Import::Function {
                        module: export_module,
                        index: *export_idx,
                    };
                }
                ExternalKind::Table => todo!(),
                ExternalKind::Memory => {
                    *import = Import::Memory;
                }
                ExternalKind::Global => {
                    *import = Import::Global {
                        module: export_module,
                        index: *export_idx,
                    };
                }
                ExternalKind::Tag => unimplemented!(),
            }
        }
    }

    Ok(())
}

fn calculate_offsets(modules: &[ParsedModule]) -> Vec<ModuleOffsets> {
    let mut offsets = vec![ModuleOffsets::default()];
    for module in modules.iter().take(modules.len() - 1) {
        let mut offset = offsets.last().unwrap().clone();
        for import in module.imports.iter() {
            let Import::Unresolved(import) = import else {
                continue;
            };
            match import.ty {
                TypeRef::Func(_) => offset.import_functions += 1,
                TypeRef::Table(_) => offset.import_tables += 1,
                TypeRef::Memory(_) => {}
                TypeRef::Global(_) => offset.import_globals += 1,
                TypeRef::Tag(_) => todo!(),
            }
        }
        offsets.push(offset);
    }
    let last_offset = offsets.last().unwrap().clone();
    offsets[0].functions = last_offset.import_functions;
    offsets[0].tables = last_offset.import_tables;
    offsets[0].globals = last_offset.import_globals;

    for idx in 1..modules.len() {
        offsets[idx].functions =
            offsets[idx - 1].functions + modules[idx - 1].functions.len() as u32;
        offsets[idx].types = offsets[idx - 1].types + modules[idx - 1].types.len() as u32;
        offsets[idx].tables += offsets[idx - 1].tables + modules[idx - 1].tables.len() as u32;
        offsets[idx].globals += offsets[idx - 1].globals + modules[idx - 1].globals.len() as u32;
    }

    offsets
}

fn encode(modules: Vec<ParsedModule>) -> Result<Vec<u8>, Error> {
    let mut ty_section = wasm_encoder::TypeSection::new();
    let mut import_section = wasm_encoder::ImportSection::new();
    let mut function_section = wasm_encoder::FunctionSection::new();
    let mut table_section = wasm_encoder::TableSection::new();
    let mut memory_section = wasm_encoder::MemorySection::new();
    let mut global_section = wasm_encoder::GlobalSection::new();
    let mut export_section = wasm_encoder::ExportSection::new();
    //let mut start_section = None;
    let mut element_section = wasm_encoder::ElementSection::new();
    let mut code_section = wasm_encoder::CodeSection::new();
    let mut data_section = wasm_encoder::DataSection::new();

    let mut reencode = RoundtripReencoder;

    let mut accumulated_memory: Option<wasmparser::MemoryType> = None;
    for module in modules.iter() {
        for memory in module.memory.iter() {
            if memory.memory64 {
                return Err(Error::Wasm64Memory(module.name.to_string()));
            }
            if let Some(accumulated_memory) = accumulated_memory.as_mut() {
                accumulated_memory.initial = memory.initial.max(accumulated_memory.initial);
                accumulated_memory.maximum = memory.maximum.max(accumulated_memory.maximum);
                accumulated_memory.shared = memory.shared.max(accumulated_memory.shared);
            } else {
                accumulated_memory = Some(*memory);
            }
        }
    }
    if let Some(memory) = accumulated_memory {
        memory_section.memory(reencode.memory_type(memory));
    }

    for module in modules {
        for ty in module.types {
            reencode.parse_recursive_type_group(&mut ty_section, ty)?;
        }
        for import in module.imports {
            let Import::Unresolved(import) = import else {
                continue;
            };
            reencode.parse_import(&mut import_section, import)?;
        }
        for func in module.functions {
            function_section.function(func);
        }
        for table in module.tables {
            reencode.parse_table(&mut table_section, table)?;
        }
        for global in module.globals {
            reencode.parse_global(&mut global_section, global)?;
        }
        for element in module.elements {
            // borrowck hack - temporary doesn't live long enough
            let offset = if let wasmparser::ElementKind::Active { offset_expr, .. } = &element.kind
            {
                Some(reencode.const_expr(offset_expr.clone())?)
            } else {
                None
            };
            element_section.segment(wasm_encoder::ElementSegment {
                mode: match element.kind {
                    wasmparser::ElementKind::Passive => wasm_encoder::ElementMode::Passive,
                    wasmparser::ElementKind::Active {
                        table_index,
                        offset_expr: _,
                    } => wasm_encoder::ElementMode::Active {
                        table: table_index,
                        offset: offset.as_ref().unwrap(),
                    },
                    wasmparser::ElementKind::Declared => wasm_encoder::ElementMode::Declared,
                },
                elements: match &element.items {
                    ElementItems::Functions(items) => wasm_encoder::Elements::Functions(&items[..]),
                    ElementItems::Expressions(ty, _exprs) => {
                        // TODO: exprs support
                        wasm_encoder::Elements::Expressions(reencode.ref_type(*ty)?, &[])
                    }
                },
            });
        }
        for export in module.exports {
            if module.public_exports {
                reencode.parse_export(&mut export_section, export);
            }
        }
        for body in module.function_bodies {
            let mut function =
                wasm_encoder::Function::new(body.locals.into_iter().map(|(count, val)| {
                    (count, reencode.val_type(val).expect("val type translation"))
                }));
            for operator in body.operators {
                function.instruction(&reencode.instruction(operator)?);
            }
            code_section.function(&function);
        }
        for data in module.data {
            reencode.parse_data(&mut data_section, data)?;
        }
    }

    let mut module = wasm_encoder::Module::new();
    module.section(&ty_section);
    module.section(&import_section);
    module.section(&function_section);
    module.section(&table_section);
    module.section(&memory_section);
    module.section(&global_section);
    module.section(&export_section);
    module.section(&element_section);
    module.section(&code_section);
    module.section(&data_section);

    Ok(module.finish())
}
