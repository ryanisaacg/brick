use std::ffi::CString;

use gimli::{
    constants as gc,
    write::{
        Address, AttributeValue, DwarfUnit, LineProgram, LineString, StringTable, Unit, UnitEntryId,
    },
    DwAte, LineEncoding,
};

use crate::analyzer::FunDecl;

pub struct DebugInfo {
    pub dwarf: DwarfUnit,
    pub bool_id: UnitEntryId,
    pub i32_id: UnitEntryId,
    pub i64_id: UnitEntryId,
    pub f32_id: UnitEntryId,
    pub f64_id: UnitEntryId,
}

impl DebugInfo {
    pub fn new() -> DebugInfo {
        let encoding = gimli::Encoding {
            format: gimli::Format::Dwarf32,
            version: 5,
            address_size: 8,
        };
        let mut dwarf = DwarfUnit::new(encoding);
        // TODO: for now, we're going to pretend all compilation stems from a single file called
        // "brick" with the directory of "."
        dwarf.unit.line_program = LineProgram::new(
            encoding,
            LineEncoding::default(),
            line_string("."),
            line_string("brick"),
            None,
        );
        let root = dwarf.unit.get_mut(dwarf.unit.root());
        // TODO: move string attributes to being in the strings table
        root.set(gc::DW_AT_comp_dir, string_attr(&mut dwarf.strings, "."));
        root.set(gc::DW_AT_name, string_attr(&mut dwarf.strings, "brick"));

        let unit = &mut dwarf.unit;
        let f32_id = base_type(&mut dwarf.strings, unit, "f32", gc::DW_ATE_float, 32);
        let f64_id = base_type(&mut dwarf.strings, unit, "f64", gc::DW_ATE_float, 64);
        let i32_id = base_type(&mut dwarf.strings, unit, "i32", gc::DW_ATE_signed, 32);
        let i64_id = base_type(&mut dwarf.strings, unit, "i64", gc::DW_ATE_signed, 64);
        // TODO: will the boolean be 32 bits on all backends?
        let bool_id = base_type(&mut dwarf.strings, unit, "bool", gc::DW_ATE_boolean, 32);

        DebugInfo {
            dwarf,
            bool_id,
            i32_id,
            i64_id,
            f32_id,
            f64_id,
        }
    }

    pub fn pointer_type(&mut self, inner_type_id: UnitEntryId) -> UnitEntryId {
        let unit = &mut self.dwarf.unit;
        let root = unit.root();
        let id = unit.add(root, gc::DW_TAG_pointer_type);
        let die = unit.get_mut(id);
        die.set(gc::DW_AT_type, AttributeValue::UnitRef(inner_type_id));

        id
    }

    pub fn array_type(&mut self, inner_type_id: UnitEntryId) -> UnitEntryId {
        let unit = &mut self.dwarf.unit;
        let root = unit.root();
        let id = unit.add(root, gc::DW_TAG_array_type);
        let die = unit.get_mut(id);
        die.set(gc::DW_AT_type, AttributeValue::UnitRef(inner_type_id));

        id
    }

    pub fn function_declaration(
        &mut self,
        function_declaration: &FunDecl,
        debug_reprs: &[Option<UnitEntryId>],
        source_range: Option<(usize, usize)>,
    ) -> UnitEntryId {
        let unit = &mut self.dwarf.unit;
        let id = unit.add(unit.root(), gc::DW_TAG_subprogram);
        let die = unit.get_mut(id);
        die.set(
            gc::DW_AT_name,
            string_attr(&mut self.dwarf.strings, &function_declaration.name),
        );
        // TODO: are functions exported?
        die.set(gc::DW_AT_external, AttributeValue::Flag(true));
        // TODO: DW_AT_main_subprogram?

        if let Some(unit_ref) = debug_reprs[function_declaration.returns] {
            die.set(gc::DW_AT_type, AttributeValue::UnitRef(unit_ref));
        }
        if let Some((start, end)) = source_range {
            die.set(
                gc::DW_AT_low_pc,
                AttributeValue::Address(Address::Constant(start as u64)),
            );
            die.set(
                gc::DW_AT_high_pc,
                AttributeValue::Address(Address::Constant(end as u64)),
            );
        }

        id
    }

    pub fn set_unit_range(&mut self, start: usize, end: usize) {
        let unit = &mut self.dwarf.unit;
        let root = unit.root();
        let root = unit.get_mut(root);
        root.set(
            gc::DW_AT_low_pc,
            AttributeValue::Address(Address::Constant(start as u64)),
        );
        root.set(
            gc::DW_AT_high_pc,
            AttributeValue::Address(Address::Constant(end as u64)),
        );
        let file_id = unit.line_program.add_file(
            line_string("brick"),
            unit.line_program.default_directory(),
            None,
        );
        unit.line_program
            .begin_sequence(Some(Address::Constant(start as u64)));
        unit.line_program.row().file = file_id;
        unit.line_program.generate_row();
        unit.line_program.end_sequence((end - start) as u64);
    }

    // TODO: struct types
    // TODO: function types
    // TODO: handle binary operations
    // TODO: handle mapping from WASM locations to DWARF locations

    // REMINDER: use DW_AT_artificial for auto-derefs and things.
    // This also implies that the IR should probably grow a capacity for
    // rememebering what IR has been auto-generated by the compiler and what
    // maps to source code
}

fn base_type(
    strings: &mut StringTable,
    unit: &mut Unit,
    name: &str,
    encoding: DwAte,
    size_in_bits: u8,
) -> UnitEntryId {
    let root = unit.root();
    let base = unit.add(root, gc::DW_TAG_base_type);
    let die = unit.get_mut(base);
    die.set(gc::DW_AT_encoding, AttributeValue::Encoding(encoding));
    die.set(gc::DW_AT_name, string_attr(strings, name));
    die.set(gc::DW_AT_bit_size, AttributeValue::Data1(size_in_bits));

    base
}

fn string_attr(strings: &mut StringTable, string: &str) -> AttributeValue {
    AttributeValue::StringRef(strings.add(string))
}

fn line_string(string: &str) -> LineString {
    LineString::String(
        CString::new(string)
            .expect("null byte in identifier")
            .into_bytes(),
    )
}
